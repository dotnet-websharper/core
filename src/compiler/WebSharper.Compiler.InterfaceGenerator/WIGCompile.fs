// $begin{copyright}
//
// This file is part of WebSharper
//
// Copyright (c) 2008-2026 IntelliFactory
//
// Licensed under the Apache License, Version 2.0 (the "License"); you
// may not use this file except in compliance with the License.  You may
// obtain a copy of the License at
//
//     http://www.apache.org/licenses/LICENSE-2.0
//
// Unless required by applicable law or agreed to in writing, software
// distributed under the License is distributed on an "AS IS" BASIS,
// WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or
// implied.  See the License for the specific language governing
// permissions and limitations under the License.
//
// $end{copyright}

/// Compiles `Library` values to assemblies.
namespace WebSharper.InterfaceGenerator

open System
open System.Collections.Generic
open System.IO
open System.Reflection
open System.Reflection.PortableExecutable
open System.Text.RegularExpressions
open System.Xml
open System.Xml.Linq
open WebSharper.Core

module Code = WebSharper.InterfaceGenerator.CodeModel
module CT = WebSharper.Core.ContentTypes
module R = WebSharper.Core.Reflection
module Ty = WebSharper.InterfaceGenerator.Type

type T = Ty.Type
type internal Comments = Dictionary<obj,string>
type internal Types = Dictionary<Ty.Id,TypeDef>
type GenericTypes = Dictionary<Ty.Id, int>

[<Sealed>]
type InlineGenerator() =

    static let validJsIdentRE =
        Regex("^[a-zA-Z_$][0-9a-zA-Z_$]*$")

    static let invalidCsIdentCharRE =
        Regex(@"[^\p{Ll}\p{Lu}\p{Lt}\p{Lo}\p{Nd}\p{Nl}\p{Mn}\p{Mc}\p{Cf}\p{Pc}\p{Lm}]")

    member g.GetMethodBaseInline(td: Code.TypeDeclaration, t: T, m: Code.MethodBase) =
        let withOutTransform retT mInl =
            match retT with  
            | Type.InteropType (_, tr) -> tr.Out mInl
            | _ -> mInl
        let t, interop =
            match t with
            | Type.NoInteropType t -> t, false
            | t -> t, true
        match m.Inline with
        | Some (Code.BasicInline inl) -> inl
        | Some (Code.TransformedInline createInline) ->
            match t with 
            | Type.FunctionType f ->
                let argMap = Dictionary()
                for i in 0 .. f.Parameters.Length - 1 do
                    let index = string ((if m.IsStatic then 0 else 1) + i)
                    let inl = "$" + index
                    let name, trInl =
                        match interop, f.Parameters.[i] with
                        | true, (n, Type.InteropType (_, tr)) -> n, tr.In inl
                        | _, (n, _) -> n, inl
                    argMap.Add(index, trInl)        
                    argMap.Add(name, trInl)        
                createInline (fun argName -> 
                    match argMap.TryGetValue argName with
                    | true, inl -> inl
                    | _ -> failwithf "Unrecognized parameter name in transformed inline: %s in member of %s" argName td.Name)
                |> withOutTransform f.ReturnType
            | _ -> failwith "GetMethodBaseInline error"
        | _ ->
            match t with
            | Type.FunctionType f ->
                let args =
                    seq {
                        for i in 0 .. f.Parameters.Length - 1 ->
                            let inl = "$" + string ((if m.IsStatic then 0 else 1) + i)
                            match interop, f.Parameters.[i] with
                            | true, (_, Type.InteropType (_, tr)) -> tr.In inl
                            | _ -> inl
                    }
                match m with
                | :? Code.Constructor as c when c.IsObject ->
                    let ss =
                        (f.Parameters, args)
                        ||> Seq.map2 (fun (n, _) a -> Util.Quote n + ":" + a)
                    "{" + String.concat "," ss + "}"
                | _ ->
                let args = args |> String.concat ","
                let mInl =
                    let typeName = if Option.isSome td.Import then "$import" else td.Name
                    match f.ParamArray with
                    | Some v ->
                        let name =
                            match m.Name with
                            | _ when Option.isSome m.Import -> "$import"
                            | "" -> typeName + ".prototype.constructor"
                            | name when m.IsStatic -> typeName + "." + name
                            | name -> name
                        match m.IsStatic, f.Parameters.Length with
                        | true,  0     -> sprintf "$wsruntime.Apply(%s, %s, $0)" name typeName
                        | false, 0     -> sprintf "$wsruntime.Apply($this.%s, $this, $1)" name
                        | true,  arity -> sprintf "$wsruntime.Apply(%s, %s,[%s].concat($%d))" name typeName args arity
                        | false, arity -> sprintf "$wsruntime.Apply($this.%s, $this,[%s].concat($%d))" name args (arity + 1)
                    | None ->
                        let name =
                            match m.Name with
                            | _ when Option.isSome m.Import -> "$import"
                            | "" -> "new " + typeName
                            | name when m.IsStatic -> typeName + "." + name
                            | name -> name
                        if m.IsStatic
                        then sprintf "%s(%s)" name args
                        else sprintf "$this.%s(%s)" name args
                mInl |> withOutTransform f.ReturnType
            | _ -> failwith "GetMethodBaseInline error"

    member g.GetPropertyGetterInline(td: Code.TypeDeclaration, t: T, p: Code.Property) =
        let withOutTransform inl = 
            match t with  
            | Type.InteropType (_, tr) -> tr.Out inl
            | _ -> inl
        let index() =
            match p.IndexerType with
            | Some (Type.InteropType (_, tr)) -> tr.In "$index"
            | Some _ -> "$index"
            | _ -> failwithf "No index type is defined but used in interop inline: %s in member of %s" p.Name td.Name
        match p.GetterInline with
        | Some (Code.BasicInline inl) -> inl
        | Some (Code.TransformedInline createInline) ->
            createInline (fun i ->
                if i = "index" then index()
                else failwithf "In a property with WithInteropGetterInline, call the provided function only with \"index\"."
            )
            |> withOutTransform
        | _ ->
            let typeName = if Option.isSome td.Import then "$import" else td.Name
            let inl = 
                let pfx = if p.IsStatic then typeName else "$this"
                let noIndex =
                    if Option.isSome p.Import then "$import" else
                    let name = p.Name
                    if name = "" then pfx
                    elif validJsIdentRE.IsMatch name
                    then sprintf "%s.%s" pfx name
                    else sprintf "%s['%s']" pfx name
                if p.IndexerType.IsSome then sprintf "%s[%s]" noIndex (index()) else noIndex
            withOutTransform inl

    member g.GetPropertySetterInline(td: Code.TypeDeclaration, t: T, p: Code.Property) =
        let getTOptAndValue =     
            let t, opt =
                match t with
                | Type.OptionType t -> t, true
                | t -> t, false 
            let value = 
                match t with
                | Type.InteropType (_, tr) -> 
                    if opt 
                    then "$value?{$: 1, $0: " + tr.In "$value.$0" + "}:null"
                    else tr.In "$value"
                | _ -> "$value"
            t, opt, value
        let index() =
            match p.IndexerType with
            | Some (Type.InteropType (_, tr)) -> tr.In "$index"
            | Some _ -> "$index"
            | _ -> failwithf "No index type is defined but used in interop inline: %s in member of %s" p.Name td.Name
        match p.SetterInline with
        | Some (Code.BasicInline inl) -> inl
        | Some (Code.TransformedInline createInline) ->
            let (_, _, value) = getTOptAndValue
            createInline (fun x -> 
                if x = "value" then value
                elif x = "index" then index()
                else failwithf "In a property with WithInteropSetterInline, call the provided function only with \"value\" or \"index\"."
            )
        | _ ->
            let t, opt =
                match t with
                | Type.OptionType t -> t, true
                | t -> t, false 
            let name = p.Name
            let typeName = if Option.isSome td.Import then "$import" else td.Name
            let pfx = if p.IsStatic then typeName else "$this"
            let value = 
                match t with
                | Type.InteropType (_, tr) -> 
                    if opt 
                    then "$value?{$: 1, $0: " + tr.In "$value.$0" + "}:null"
                    else tr.In "$value"
                | _ -> "$value"
            let prop() =
                if Option.isSome p.Import then "$import" else
                if validJsIdentRE.IsMatch name then sprintf "%s.%s" pfx name
                else sprintf "%s['%s']" pfx name
            if opt then
                if name = "" then 
                    if p.IndexerType.IsSome 
                    then sprintf "$wsruntime.SetOrDelete(%s, %s, %s)" pfx (index()) value
                    else failwith "Optional property with empty name not allowed."
                else
                    if p.IndexerType.IsSome then
                        sprintf "$wsruntime.SetOrDelete(%s, %s, %s)" (prop()) (index()) value 
                    else 
                        sprintf "$wsruntime.SetOrDelete(%s, '%s', %s)" pfx name value    
            else
                let ind = if p.IndexerType.IsSome then "[" + index() + "]" else ""
                if name = "" then sprintf "void (%s%s = %s)" pfx ind value
                else sprintf "void (%s%s = %s)" (prop()) ind value

    member g.GetSourceName(entity: Code.Entity) =
        let mangle name =
            invalidCsIdentCharRE.Replace(name, "_")
        if entity.SourceName.IsSome then entity.SourceName.Value else
            let name = entity.Name
            let name =
                if entity :? Code.NamespaceEntity then
                    let name =
                        if name.StartsWith "new " then
                            name.Substring 4
                        else
                            name
                    match name.LastIndexOf '.' with
                    | -1 -> name
                    | n -> name.Substring(n + 1)
                else
                    name.Replace('.', '_')
            name.Substring(0, 1).ToUpper() + name.Substring 1
        |> mangle

    member g.GetPropertySourceName(p: Code.Property) =
        if p.Name = "" && Option.isSome p.IndexerType then "Item"
        else g.GetSourceName(p)

[<Sealed>]
type internal TypeBuilder(resolver: ReferenceResolver) =
    let resolvedTypes = Dictionary()

    let resolveTypeName (asmName: string) (typeName: string) =
        match resolvedTypes.TryGetValue(typeName) with
        | true, x -> x
        | false, _ ->
            let ty = resolver.Type(asmName, typeName)
            resolvedTypes.[typeName] <- ty
            ty

    let resolveType (ty: System.Type) =
        resolveTypeName ty.Assembly.FullName ty.FullName

    let genericInstance (def: TypeRef) (args: seq<TypeRef>) =
        if Seq.isEmpty args then
            def
        else
            match def with
            | Instance (d, _) -> Instance (d, List.ofSeq args)
            | d -> Instance (d, List.ofSeq args)

    let wsCore = typeof<WebSharper.InlineAttribute>.Assembly.FullName

    let commonType origAsmName baseName ts =
        let name =
            match Seq.length ts with
            | 0 -> baseName
            | k -> baseName + "`" + string k
        let tDef = resolveTypeName origAsmName name
        genericInstance tDef ts

    let coreLibType baseName ts = commonType typeof<obj>.Assembly.FullName baseName ts
    let wsCoreType baseName ts = commonType wsCore baseName ts

    // FSharp.Core is resolved without explicit version,
    // so that the resolver can pick up the version from the passed references.
    let funcType = resolveTypeName "FSharp.Core" "Microsoft.FSharp.Core.FSharpFunc`2"
    let attributeType = resolveType typeof<System.Attribute>
    let converterType = resolveType typedefof<System.Converter<_, _>>
    let objectType = resolveType typeof<obj>
    let stringType = resolveType typeof<string>
    let systemType = resolveType typeof<System.Type>
    let voidType = resolveType typeof<System.Void>
    let baseResourceType = resolveType typeof<WebSharper.Core.Resources.BaseResource>
    let paramArray = resolveType typeof<System.ParamArrayAttribute>
    let notImpl = resolveType typeof<System.NotImplementedException>
    let obsolete = resolveType typeof<System.ObsoleteAttribute>
    let nameAttr = resolveType typeof<WebSharper.NameAttribute>
    let inlineAttr = resolveType typeof<WebSharper.InlineAttribute>
    let macroAttr = resolveType typeof<WebSharper.MacroAttribute>
    let requireAttr = resolveType typeof<WebSharper.RequireAttribute>
    let pureAttr = resolveType typeof<WebSharper.PureAttribute>
    let warnAttr = resolveType typeof<WebSharper.WarnAttribute>
    let typeAttr = resolveType typeof<WebSharper.TypeAttribute>
    let importAttr = resolveType typeof<WebSharper.ImportAttribute>
    let funcWithArgs = resolveType typedefof<WebSharper.JavaScript.FuncWithArgs<_,_>>
    let funcWithThis = resolveType typedefof<WebSharper.JavaScript.FuncWithThis<_,_>> 
    let funcWithOnlyThis = resolveType typedefof<WebSharper.JavaScript.FuncWithOnlyThis<_,_>>
    let funcWithArgsRest = resolveType typedefof<WebSharper.JavaScript.FuncWithArgsRest<_,_,_>>
    let optionType = resolveType typedefof<WebSharper.JavaScript.Optional<_>>
    let itemOrArrayType = resolveType typedefof<WebSharper.JavaScript.ItemOrArray<_>>

    member b.Action ts =
        coreLibType "System.Action" ts

    member b.Converter d r =
        genericInstance converterType [d; r]

    member b.Function d r =
        genericInstance funcType [d; r]

    member c.GenericInstanceType(def: TypeRef, args: seq<TypeRef>) =
        genericInstance def args

    member b.Tuple(ts: seq<TypeRef>) =
        let rec createTuple (ta: _[]) =
            if ta.Length < 8 then
                coreLibType "System.Tuple" ta
            else
                coreLibType "System.Tuple" (Seq.append (ta.[.. 6]) [ createTuple ta.[7 ..] ])    
        createTuple (Array.ofSeq ts)

    member b.Choice(ts: seq<TypeRef>) =
        wsCoreType "WebSharper.JavaScript.Union" ts

    member b.ItemOrArray t =
        genericInstance itemOrArrayType [t]    

    member b.Option t =
        genericInstance optionType [t]    

    member b.Type(assemblyName: string, fullName: string) =
        resolveTypeName assemblyName fullName

    member b.Type(t: Type) =
        b.Type(t.Assembly.FullName, t.FullName)

    member b.Type<'T>() =
        b.Type typeof<'T>

    member b.FuncWithArgs args ret =
        genericInstance funcWithArgs [args; ret]   
                 
    member b.FuncWithThis this func =
        genericInstance funcWithThis [this; func]        

    member b.FuncWithOnlyThis this ret =
        genericInstance funcWithThis [this; ret]        

    member b.FuncWithRest args rest result =
        wsCoreType "WebSharper.JavaScript.FuncWithRest" (args @ [ rest; result ])

    member b.FuncWithArgsRest args rest result =
        genericInstance funcWithArgsRest [args; rest; result]        

    member b.Delegate args res =
        let tn = if Option.isSome res then "System.Func" else "System.Action"
        coreLibType tn (args @ Option.toList res)

    member b.InteropDelegate this args pars res =
        let tn =
            "WebSharper.JavaScript."
            + if Option.isSome this then "This" else ""
            + if Option.isSome pars then "Params" else ""
            + if Option.isSome res then "Func" else "Action"
        if List.length args <= 6 then
            wsCoreType tn (Option.toList this @ args @ Option.toList pars @ Option.toList res)
        else
            b.Type(typeof<WebSharper.JavaScript.Function>)

    member b.Attribute = attributeType
    member b.BaseResource = baseResourceType
    member b.Name = nameAttr
    member b.Inline = inlineAttr
    member b.Macro = macroAttr
    member b.NotImplemented = notImpl
    member b.Object = objectType
    member b.ParamArray = paramArray
    member b.Require = requireAttr
    member b.Obsolete = obsolete
    member b.Pure = pureAttr
    member b.Warn = warnAttr
    member b.TypeAttr = typeAttr
    member b.Import = importAttr
    member b.String = stringType
    member b.SystemType = systemType
    member b.Void = voidType

[<Sealed>]
type internal TypeConverter private (tB: TypeBuilder, types: Types, genTypes: GenericTypes, 
                                genericsByPosition: GenericParam [], genericsById: Type.Id -> GenericParam) =

    let byId id =
        match types.TryGetValue id with
        | true, x -> Local x
        | _ ->
            if id.Name = "" then tB.Object else
            failwithf "Type definition not included in assembly definition: %s" id.Name

    static let noGenerics _ = failwith "Generic parameter not found."

    new (tB, types, genTypes) =
        TypeConverter(tB, types, genTypes, Array.empty, noGenerics)

    member c.TypeReference(d: R.TypeDefinition) =
        tB.Type(d.AssemblyName.FullName, d.FullName)

    member c.TypeReference(t: R.Type) =
        match t with
        | R.Type.Array (t, rank) ->
            ArrayOf(c.TypeReference t, rank)
        | R.Type.Concrete (def, args) ->
            if not args.IsEmpty then
                Instance(c.TypeReference def, args |> List.map c.TypeReference)
            else
                c.TypeReference def
        | R.Type.Generic pos ->
            Param genericsByPosition.[pos]

    member private c.TypeReference(t: T, defT: Code.TypeDeclaration, allowGeneric: bool, isCSharp: bool) =
        let tRef x = c.TypeReference(x, defT, false, isCSharp)
        let tres =
            match t with
            | Type.ArrayType (rank, t) ->
                ArrayOf(tRef t, rank)
            | Type.DeclaredType id ->
                byId id
            | Type.FunctionType f ->
                let args = f.Parameters |> List.map (snd >> tRef)
                if isCSharp then
                    let ret = 
                        match f.ReturnType with
                        | Type.Unit -> None
                        | r -> Some (tRef r)
                    match f.This, f.ParamArray with
                    | None, None -> tB.Delegate args ret
                    | _ -> tB.InteropDelegate (Option.map tRef f.This) args (Option.map tRef f.ParamArray) ret
                else
                    let ret = tRef f.ReturnType
                    let func =
                        match f.ParamArray with
                        | None ->
                            match args with
                            | [] -> tB.Function (tB.Type<unit>()) ret
                            | [a] -> tB.Function a ret
                            | _ -> tB.FuncWithArgs (tB.Tuple args) ret                
                        | Some p ->
                            let pa = tRef p
                            if args.Length <= 6 then
                                tB.FuncWithRest args pa ret
                            else
                                tB.FuncWithArgsRest (tB.Tuple args) pa ret        
                    match f.This with
                    | None -> func
                    | Some this -> 
                        match f.ParamArray, args with
                        | None, [] ->
                            tB.FuncWithOnlyThis (tRef this) ret
                        | _ -> 
                            tB.FuncWithThis (tRef this) func
            | Type.DelegateType(a, r) ->
                tB.Delegate (List.map tRef a) (Option.map tRef r)
            | Type.FSFunctionType (a, r) ->
                tB.Function (tRef a) (tRef r)
            | Type.GenericType i ->
                Param (genericsById i)
            | Type.SpecializedType (Type.DeclaredType id, xs) 
            | Type.SpecializedType (Type.InteropType(Type.DeclaredType id, _), xs)
            | Type.SpecializedType (Type.NoInteropType(Type.DeclaredType id), xs) ->
                let t = byId id
                let gen =
                    match genTypes.TryGetValue id with
                    | true, g -> g
                    | _ -> 0
                if gen <> xs.Length then
                    failwithf "Wrong number of generic parameters applied on %s in member of %s: %d instead of %d"
                        t.FullName defT.Name xs.Length gen
                let args = xs |> Seq.map tRef
                tB.GenericInstanceType(t, args)
            | Type.SpecializedType (x, xs) ->
                let t = c.TypeReference(x, defT, true, isCSharp)
                let gen = t.GenericParameterCount
                if gen <> xs.Length then
                    failwithf "Wrong number of generic parameters applied on %s in member of %s: %d instead of %d"
                        t.FullName defT.Name xs.Length gen
                let args = xs |> Seq.map tRef
                tB.GenericInstanceType(t, args)
            | Type.SystemType t ->
                c.TypeReference t
            | Type.TupleType xs ->
                tB.Tuple [ for t in List.rev xs -> tRef t ]
            | Type.InteropType (t, _)
            | Type.NoInteropType t ->
                tRef t
            | Type.UnionType _ -> tB.Object
            | Type.ChoiceType ts ->
                tB.Choice (ts |> Seq.map tRef)
            | Type.OptionType t ->
                tB.Option (tRef t) 
            | Type.ItemOrArrayType t ->
                tB.ItemOrArray (tRef t) 
            | Type.DefiningType ->
                byId defT.Id
        // check missing generics
        if not allowGeneric then
            match t with
            | Type.DeclaredType id ->
                if genTypes.ContainsKey id then
                    failwithf "Generic parameters not applied on %s in member of %s" tres.FullName defT.Name       
            | _ ->
                if tres.HasGenericParameters then
                    failwithf "Generic parameters not applied on %s in member of %s" tres.FullName defT.Name       
        tres

    member c.TypeReference(t: T, defT: Code.TypeDeclaration) =
        c.TypeReference(t, defT, false, true)

    member c.TypeReference(t: T, defT: Code.TypeDeclaration, isCSharp) =
        c.TypeReference(t, defT, false, isCSharp)

    member c.WithGenerics(gs: seq<GenericParam>, byId: Type.Id -> GenericParam option) =
        let gbi i =
            match byId i with
            | Some p -> p
            | _ -> genericsById i
        TypeConverter(tB, types, genTypes, Array.append genericsByPosition (Seq.toArray gs), gbi)

[<Sealed>]
type internal MemberBuilder(tB: TypeBuilder) =

    let ctor t pars =
        { DeclaringType = t; Name = ".ctor"; HasThis = true; ReturnType = tB.Void; Parameters = pars }

    let paramArrayConstructor = ctor tB.ParamArray []
    let notImplementedConstructor = ctor tB.NotImplemented []
    let baseResourceCtor1 = ctor tB.BaseResource [tB.String]
    let baseResourceCtorN = ctor tB.BaseResource [tB.String; tB.String; ArrayOf(tB.String, 1)]
    let nameAttributeConstructor = ctor tB.Name [tB.String]
    let inlineAttributeConstructor = ctor tB.Inline [tB.String]
    let macroAttributeConstructor = ctor tB.Macro [tB.SystemType]
    let requireAttributeConstructor = ctor tB.Require [tB.SystemType]
    let obsoleteAttributeConstructor = ctor tB.Obsolete []
    let obsoleteAttributeWithMsgConstructor = ctor tB.Obsolete [tB.String]
    let pureAttributeConstructor = ctor tB.Pure []
    let warnAttributeConstructor = ctor tB.Warn [tB.String]
    let typeAttributeConstructor = ctor tB.TypeAttr [tB.String]
    let importAttributeConstructorDefault = ctor tB.Import [tB.String]
    let importAttributeConstructor = ctor tB.Import [tB.String; tB.String]

    member c.Constructor(t: TypeRef, pars: TypeRef list) = ctor t pars

    member c.AddBody(m: MethodDef) =
        m.Body <- Some (ThrowNew notImplementedConstructor)

    member c.BuildConstructor(?attrs) =
        let attrs =
            defaultArg attrs MethodAttributes.Public
            ||| MethodAttributes.SpecialName
            ||| MethodAttributes.RTSpecialName
        let r = MethodDef(".ctor", attrs, tB.Void)
        c.AddBody(r)
        r

    member c.BuildParamArrayParameter(t: TypeRef) =
        let p = ParamDef("ps", ArrayOf(t, 1))
        p.CustomAttributes.Add { Constructor = paramArrayConstructor; Arguments = [] }
        p

    member c.BaseResourceConstructor1 = baseResourceCtor1
    member c.BaseResourceConstructorN = baseResourceCtorN
    member c.NameAttributeConstructor = nameAttributeConstructor
    member c.InlineAttributeConstructor = inlineAttributeConstructor
    member c.MacroAttributeConstructor = macroAttributeConstructor
    member c.RequireAttributeConstructor = requireAttributeConstructor
    member c.ObsoleteAttributeConstructor = obsoleteAttributeConstructor
    member c.ObsoleteAttributeWithMsgConstructor = obsoleteAttributeWithMsgConstructor
    member c.PureAttributeConstructor = pureAttributeConstructor
    member c.WarnAttributeConstructor = warnAttributeConstructor
    member c.TypeAttributeConstructor = typeAttributeConstructor
    member c.ImportAttributeConstructorDefault = importAttributeConstructorDefault
    member c.ImportAttributeConstructor = importAttributeConstructor


type CompilerOptions =
    {
        AssemblyName : string
        AssemblyResolver : option<WebSharper.Compiler.AssemblyResolver>
        AssemblyVersion : Version
        DocPath : option<string>
        EmbeddedResources : seq<string>
        OutputPath : option<string>
        ReferencePaths : seq<string>
        StrongNameKeyPath : option<string>
    }

    static member Default(assemblyName) =
        {
            AssemblyName = assemblyName
            AssemblyResolver = None
            AssemblyVersion = Version(0, 0)
            DocPath = None
            EmbeddedResources = Seq.empty
            OutputPath = None
            ReferencePaths = Seq.empty
            StrongNameKeyPath = None
        }

    static member Parse args =
        let (|S|_|) (prefix: string) (x: string) =
            if x.StartsWith(prefix) then Some (x.Substring(prefix.Length)) else None
        (CompilerOptions.Default("Assembly"), args)
        ||> Seq.fold (fun opts arg ->
            match arg with
            | S "-n:" name ->
                { opts with AssemblyName = name }
            | S "-v:" ver ->
                { opts with AssemblyVersion = Version.Parse ver }
            | S "-snk:" path ->
                { opts with StrongNameKeyPath = Some path }
            | S "-embed:" path ->
                { opts with EmbeddedResources = Seq.append opts.EmbeddedResources [path] }
            | S "-o:" out ->
                { opts with OutputPath = Some out }
            | S "-doc:" doc ->
                { opts with DocPath = Some doc }
            | S "-r:" ref ->
                let rps = Seq.toArray (Seq.append opts.ReferencePaths [ref])
                { opts with ReferencePaths = rps :> seq<_> }
            | _ ->
                opts)

[<Sealed>]
type internal MemberConverter
        (
            tB: TypeBuilder,
            mB: MemberBuilder,
            tC: TypeConverter,
            types: Types,
            iG: InlineGenerator,
            asmAttrs: List<CustomAttr>,
            comments: Comments,
            compilerOptions: CompilerOptions,
            genParamNames: string list
        ) =

    let attr ctor args = { Constructor = ctor; Arguments = args }

    let nameAttribute (code: string) = attr mB.NameAttributeConstructor [AString code]

    let inlineAttribute (code: string) = attr mB.InlineAttributeConstructor [AString code]

    let macroAttribute (macroType: TypeRef) = attr mB.MacroAttributeConstructor [AType macroType]

    let requireAttribute (resourceType: TypeRef) = attr mB.RequireAttributeConstructor [AType resourceType]

    let obsoleteAttribute = attr mB.ObsoleteAttributeConstructor []
    let obseleteAttributeWithMsg (msg: string) = attr mB.ObsoleteAttributeWithMsgConstructor [AString msg]
    let setObsoleteAttribute (x: CodeModel.Entity) (attrs: List<CustomAttr>) =
        match x.ObsoleteStatus with
        | CodeModel.NotObsolete -> ()
        | CodeModel.Obsolete None -> attrs.Add obsoleteAttribute
        | CodeModel.Obsolete (Some msg) -> attrs.Add (obseleteAttributeWithMsg msg)

    let typeAttribute (t: string) = attr mB.TypeAttributeConstructor [AString t]
    let setTSAttribute (x: CodeModel.TypeDeclaration) (attrs: List<CustomAttr>) =
        match x.TSType with
        | None -> ()
        | Some t -> attrs.Add (typeAttribute t)

    let importAttribute (n: option<string>) (f: string) =
        match n with
        | None -> attr mB.ImportAttributeConstructorDefault [AString f]
        | Some n -> attr mB.ImportAttributeConstructor [AString n; AString f]
    let setImportAttribute (x: CodeModel.Entity) (attrs: List<CustomAttr>) =
        match x.Import with
        | None -> ()
        | Some (n, f) -> attrs.Add (importAttribute n f)

    let pureAttribute = attr mB.PureAttributeConstructor []

    let setPureAttribute (x: CodeModel.MethodBase) (attrs: List<CustomAttr>) =
        if x.IsPure then attrs.Add pureAttribute

    let warnAttribute warning = attr mB.WarnAttributeConstructor [AString warning]

    let setWarnAttribute (x: CodeModel.Entity) (attrs: List<CustomAttr>) =
        match x.Warning with
        | Some warning -> attrs.Add (warnAttribute warning)
        | _ -> ()

    let makeParameters (f: Type.Function, defT, isCSharp) =
        Seq.ofArray [|
            for (n, t) in f.Parameters do
                yield ParamDef(n, tC.TypeReference (t, defT, isCSharp))
            match f.ParamArray with
            | None -> ()
            | Some pa ->
                yield mB.BuildParamArrayParameter(tC.TypeReference (pa, defT, isCSharp))
        |]

    let staticMethodAttributes = MethodAttributes.Static ||| MethodAttributes.Public
    let instanceMethodAttributes = MethodAttributes.Public
    let ctorMethodAttributes = MethodAttributes.Public
    let interfaceMethodAttributes = MethodAttributes.Public ||| MethodAttributes.Abstract ||| MethodAttributes.Virtual ||| MethodAttributes.HideBySig ||| MethodAttributes.NewSlot
    let implementMethodAttributes = MethodAttributes.Public ||| MethodAttributes.Final ||| MethodAttributes.Virtual ||| MethodAttributes.HideBySig ||| MethodAttributes.NewSlot
    let overrideMethodAttributes = MethodAttributes.Public ||| MethodAttributes.Virtual ||| MethodAttributes.HideBySig
    let abstractMethodAttributes = MethodAttributes.Public ||| MethodAttributes.Abstract ||| MethodAttributes.Virtual ||| MethodAttributes.HideBySig ||| MethodAttributes.NewSlot
    let virtualMethodAttributes = MethodAttributes.Public ||| MethodAttributes.Virtual ||| MethodAttributes.HideBySig ||| MethodAttributes.NewSlot

    let methodAttributes (dt: TypeDef) (x: Code.Entity) =
        if dt.IsInterface then 
            interfaceMethodAttributes 
        else
            match x with
            | :? Code.Constructor -> ctorMethodAttributes
            | :? Code.Member as m when m.IsStatic -> staticMethodAttributes
            | _ ->
            match x with // workaround for https://github.com/dotnet/fsharp/issues/13175
            | :? Code.Method as m ->
                match m.Kind with
                | Code.Implementation -> implementMethodAttributes
                | Code.NonVirtual -> instanceMethodAttributes
                | Code.Virtual -> virtualMethodAttributes
                | Code.Override -> overrideMethodAttributes
                | Code.Abstract -> abstractMethodAttributes
            | :? Code.Property as p when p.IsOverride -> overrideMethodAttributes
            | _ -> instanceMethodAttributes

    let addDependencies (ent: Code.IResourceDependable) (attrs: List<CustomAttr>) =
        for d in ent.GetRequires() do
            match d with
            | Code.LocalDependency d ->
                match types.TryGetValue(d) with
                | true, t -> attrs.Add(requireAttribute (Local t))
                | _ -> ()
            | Code.ExternalDependency ty ->
                let t = tC.TypeReference (ty, Unchecked.defaultof<_>)
                attrs.Add(requireAttribute t)

    let addConstructor (dT: TypeDef) (td: Code.TypeDeclaration) (x: Code.Constructor) =
        let overloads =
            x.Type
            |> Type.GetOverloads
        let overloads = 
            match x.Inline with
            | Some (Code.BasicInline _) -> overloads |> List.collect Type.WithFSharpOverloads
            | _ -> overloads |> List.collect Type.TransformArgs
        for t, isCSharp in overloads do
            match t with
            | Type.NoInteropType (Type.FunctionType f)
            | Type.FunctionType f ->
                let attrs = methodAttributes dT x
                let cD = mB.BuildConstructor(methodAttributes dT x)
                match x.Macro with
                | Some macro ->
                    tC.TypeReference (macro, td)
                    |> macroAttribute
                    |> cD.CustomAttributes.Add
                | _ -> 
                    iG.GetMethodBaseInline(td, t, x)
                    |> inlineAttribute
                    |> cD.CustomAttributes.Add
                for p in makeParameters (f, td, isCSharp) do
                    cD.Parameters.Add p
                setObsoleteAttribute x cD.CustomAttributes
                setImportAttribute x cD.CustomAttributes 
                setPureAttribute x cD.CustomAttributes
                setWarnAttribute x cD.CustomAttributes
                addDependencies x cD.CustomAttributes
                dT.Methods.Add(cD)
                do
                    match x.Comment with
                    | None -> ()
                    | Some c -> comments.[cD] <- c
            | _ -> ()

    let addProperty (dT: TypeDef) (td: Code.TypeDeclaration) name (p: Code.Property) =
        let t =
            match p.GetterInline, p.SetterInline with
            | Some (Code.BasicInline _), _
            | _, Some (Code.BasicInline _) -> Type.Normalize p.Type
            | _ -> Type.TransformOption true (Type.Normalize p.Type)
        let ty = tC.TypeReference (t, td)
        //let name = iG.GetPropertySourceName p
        let pD = PropertyDef(name, ty)
        do
            match p.Comment with
            | None -> ()
            | Some c -> comments.[pD] <- c
        let isInterface = dT.IsInterface
        let isMixin = isInterface && not (p.GetterInline.IsSome || p.SetterInline.IsSome)
        if p.HasGetter then
            let mD = MethodDef("get_" + name, methodAttributes dT p, ty)
            if not isMixin then
                if not isInterface then
                    mB.AddBody mD
                iG.GetPropertyGetterInline(td, t, p)
                |> inlineAttribute
                |> mD.CustomAttributes.Add
            match p.IndexerType with
            | None -> ()
            | Some it ->
                mD.Parameters.Add(ParamDef("index", tC.TypeReference (it, td)))         
            dT.Methods.Add mD
            pD.GetMethod <- mD
        if p.HasSetter then
            let mD = MethodDef("set_" + name, methodAttributes dT p, tB.Void)
            if not isMixin then
                if not isInterface then
                    mB.AddBody mD
                iG.GetPropertySetterInline(td, t, p)
                |> inlineAttribute
                |> mD.CustomAttributes.Add
            match p.IndexerType with
            | None -> ()
            | Some it ->
                mD.Parameters.Add(ParamDef("index", tC.TypeReference (it, td)))         
            mD.Parameters.Add(ParamDef("value", ty))
            dT.Methods.Add mD
            pD.SetMethod <- mD
        if isMixin then
            p.Name
            |> nameAttribute
            |> pD.CustomAttributes.Add
        setObsoleteAttribute p pD.CustomAttributes
        setImportAttribute p pD.CustomAttributes 
        addDependencies p pD.CustomAttributes
        setWarnAttribute p pD.CustomAttributes
        dT.Properties.Add pD

    let withGenerics (generics: Code.TypeParameter list, td: Code.TypeDeclaration, addGenericParameter: string -> GenericParam) =
        let namesTaken = HashSet(genParamNames) 
        let fmt (x: string) (n: int) =
            if n = 0 then x else
                System.String.Format("{0}{1:x}", x, n)
        let rec pick name k =
            let res = fmt name k
            if namesTaken.Contains res then
                pick name (k + 1)
            else res
        let gs =
            [
                for g in generics ->
                    let n = pick g.Name 0
                    namesTaken.Add n |> ignore
                    let gP = addGenericParameter n
                    for c in g.Constraints do
                        gP.Constraints.Add(tC.TypeReference (c, td))
                    g.Id, gP
            ]        
        let byId i =
            gs |> List.tryPick (fun (id, p) -> if i = id then Some p else None)    
        MemberConverter(tB, mB, tC.WithGenerics (gs |> Seq.map snd, byId), types, iG, asmAttrs, comments, 
            compilerOptions, List.ofSeq namesTaken)

    let genericType (x: Code.TypeDeclaration) k =
        match types.TryGetValue(x.Id) with
        | true, tD ->
            if x.Generics.Length > 0 then
                tD.Name <- tD.Name + "`" + string x.Generics.Length
            k (withGenerics (x.Generics, x, tD.AddGenericParameter)) tD
            setWarnAttribute x tD.CustomAttributes
        | _ -> ()

    member private c.AddMethod(dT: TypeDef, td: Code.TypeDeclaration, x: Code.Method) =
        let overloads =
            x.Type
            |> Type.GetOverloads
        let overloads = 
            match x.Kind, x.Inline with
            | Code.NonVirtual, Some (Code.BasicInline _) -> overloads |> List.collect Type.WithFSharpOverloads
            | _ -> overloads |> List.collect Type.TransformArgs
        for t, isCSharp in overloads do
            match t with
            | Type.NoInteropType (Type.FunctionType f)
            | Type.FunctionType f ->
                let name = iG.GetSourceName x
                let attrs = methodAttributes dT x
                let mD = MethodDef(name, attrs, tB.Object)
                do
                    match x.Comment with
                    | None -> ()
                    | Some c -> comments.[mD] <- c
                let c = withGenerics (x.Generics, td, mD.AddGenericParameter)
                c.AddMethod(dT, td, x, mD, t, f, isCSharp)
            | _ -> ()

    member private c.AddMethod(dT: TypeDef, td: Code.TypeDeclaration, x: Code.Method, mD: MethodDef, t: Type.Type, f: Type.Function, isCSharp: bool) =
        mD.ReturnType <-
            match f.ReturnType with
            | Type.Unit -> tB.Void
            | Type.NonUnit -> tC.TypeReference (f.ReturnType, td, isCSharp)
        for p in makeParameters (f, td, isCSharp) do
            mD.Parameters.Add p
        let isInterface = dT.IsInterface
        let isAbstract =
            if isInterface then
                not (x.Inline.IsSome || x.Macro.IsSome)
            else
                match x.Kind with
                | CodeModel.NonVirtual -> false
                | CodeModel.Implementation -> x.Inline.IsNone
                | CodeModel.Override | CodeModel.Abstract | CodeModel.Virtual ->
                    if x.Inline.IsSome then
                        failwithf "Cannot create abstract or virtual method with inline: %s on %s" x.Name dT.Name
                    true
        if not isInterface then
            mB.AddBody mD
        if isAbstract then
            x.Name
            |> nameAttribute
            |> mD.CustomAttributes.Add
        else
            match x.Macro with
            | Some macro ->
                tC.TypeReference (macro, td)
                |> macroAttribute
                |> mD.CustomAttributes.Add
            | _ ->
                iG.GetMethodBaseInline(td, t, x)
                |> inlineAttribute
                |> mD.CustomAttributes.Add
            setPureAttribute x mD.CustomAttributes
            setWarnAttribute x mD.CustomAttributes
        setObsoleteAttribute x mD.CustomAttributes
        setImportAttribute x mD.CustomAttributes 
        addDependencies x mD.CustomAttributes
        dT.Methods.Add mD

    member private c.AddTypeMembers<'T when 'T :> Code.TypeDeclaration and 'T :> Code.IResourceDependable<'T>>
            (x: 'T, tD: TypeDef) =
        for m in x.Methods do
            c.AddMethod(tD, x, m)
        let propNames = HashSet()
        for p in x.Properties do
            let name = iG.GetPropertySourceName p
            if not (propNames.Add name) then failwithf "Duplicate property definition: %s on %s" name x.Name
            addProperty tD x name p
        addDependencies x tD.CustomAttributes
        setWarnAttribute x tD.CustomAttributes
    
    member d.AddDependencies(ent: Code.IResourceDependable, attrs: List<CustomAttr>) =
        addDependencies ent attrs

    member c.Class(x: Code.Class) =
        genericType x (fun c tD -> c.Class(x, tD))

    member private c.Class(x: Code.Class, tD: TypeDef) =
        if x.IsAbstract then
            tD.Attributes <- tD.Attributes ||| TypeAttributes.Abstract
        do
            match x.BaseClass with
            | None -> tD.BaseType <- Some tB.Object
            | Some t -> tD.BaseType <- Some (tC.TypeReference (t, x))
        do
            match x.Comment with
            | None -> ()
            | Some c -> comments.[tD] <- c
        for i in x.ImplementedInterfaces do
            let tr = tC.TypeReference (i, x)
            if tr.IsInterface then
                tD.Interfaces.Add tr
            else
                failwithf "Class %s is trying to implement a non-interface type: %s" x.Name tr.FullName
        for ctor in x.Constructors do
            addConstructor tD x ctor
        setObsoleteAttribute x tD.CustomAttributes
        setImportAttribute x tD.CustomAttributes 
        setWarnAttribute x tD.CustomAttributes
        setTSAttribute x tD.CustomAttributes
        c.AddTypeMembers(x, tD)

    member c.Interface(x: Code.Interface) =
        genericType x (fun c tD -> c.Interface(x, tD))

    member private c.Interface(x: Code.Interface, tD: TypeDef) =
        for i in x.BaseInterfaces do
            let tr = tC.TypeReference (i, x)
            if tr.IsInterface then
                tD.Interfaces.Add tr
            else
                failwithf "Interface %s is trying to inherit a non-interface type: %s" x.Name tr.FullName
        setObsoleteAttribute x tD.CustomAttributes
        setImportAttribute x tD.CustomAttributes 
        setTSAttribute x tD.CustomAttributes
        c.AddTypeMembers(x, tD)
        do
            match x.Comment with
            | None -> ()
            | Some c -> comments.[tD] <- c

    member c.Resource(r: Code.Resource) =
        match types.TryGetValue(r.Id) with
        | true, tD ->
            addDependencies r tD.CustomAttributes
            if r.IsAssemblyWide then
                asmAttrs.Add(requireAttribute (Local tD))
            match r.Paths with
            | [] -> ()
            | [p] ->
                tD.BaseType <- Some tB.BaseResource
                let ctor = mB.BuildConstructor()
                ctor.Body <- Some (CallBaseConstructor(mB.BaseResourceConstructor1, [p], None))
                tD.Methods.Add ctor
            | a :: b :: rest ->
                tD.BaseType <- Some tB.BaseResource
                let ctor = mB.BuildConstructor()
                ctor.Body <- Some (CallBaseConstructor(mB.BaseResourceConstructorN, [a; b], Some (tB.String, rest)))
                tD.Methods.Add ctor
        | _ -> ()

[<Sealed>]
type internal XmlDocGenerator(assemblyName: string, types: seq<TypeDef>, comments: Comments) =

    let getComment (m: obj) =
        match comments.TryGetValue m with
        | true, c -> Some c
        | _ -> None

    let e name (attrs: seq<string * string>) (contents: seq<XNode>) =
        let r = XElement(XName.Get name)
        for (k, v) in attrs do
            let a = XAttribute(XName.Get k, v)
            r.Add a
        for c in contents do
            r.Add c
        r

    let t (text: string) =
        XText(text)

    let rec typeRefId (t: TypeRef) : string =
        let fullName (t: TypeRef) =
            t.FullName.Replace("+", ".")
        match t with
        | ArrayOf (el, rank) ->
            match rank with
            | 0 | 1 -> typeRefId el + "[]"
            | k -> sprintf "%s, [%s]" (typeRefId el) (String.replicate (k - 1) ",")
        | Instance (d, args) ->
            let name = fullName d
            let untaggedName = name.[.. name.IndexOf '`' - 1]
            let pars =
                args
                |> Seq.map typeRefId
                |> String.concat ","
            sprintf "%s{%s}" untaggedName pars
        | Param g ->
            if g.IsMethodParameter then "``" + string g.Position
            else "`" + string g.Position
        | _ ->
            fullName t

    let propertyId (dT: TypeDef) (p: PropertyDef) =
        sprintf "P:%s.%s" (typeRefId (Local dT)) p.Name

    let methodId (dT: TypeDef) (m: MethodDef) =
        let name =
            match m.Name with
            | ".ctor" -> "#ctor"
            | ".cctor" -> "#cctor"
            | n -> n
        let gen = if m.GenericParameters.Count > 0 then "``" + string m.GenericParameters.Count else ""
        sprintf "M:%s.%s%s%s" (typeRefId (Local dT)) name gen
            (if m.Parameters.Count = 0 then "" else
                seq {
                    for p in m.Parameters ->
                        typeRefId p.ParameterType
                }
                |> String.concat ","
                |> sprintf "(%s)"
            )

    let visitMember name comment =
        seq {
            match comment with
            | Some c -> yield e "member" ["name", name] [e "summary" [] [t c]]
            | None -> ()
        }

    let rec visitType (dT: TypeDef) =
        seq {
            yield! visitMember ("T:" + typeRefId (Local dT)) (getComment dT)
            for m in dT.Methods do
                yield! visitMember (methodId dT m) (getComment m)
            for p in dT.Properties do
                yield! visitMember (propertyId dT p) (getComment p)
        }

    let generate (fileName: string) =
        let doc = XDocument()
        e "doc" [] [
            yield e "assembly" [] [
                e "name" [] [t assemblyName]
            ] :> XNode
            for dT in types do
                for m in visitType dT do
                    yield m :> XNode
        ]
        |> doc.Add
        doc.Save fileName

    member g.Generate(fileName: string) =
        generate fileName

[<Sealed>]
type CompiledAssembly internal (def: AssemblyDef, doc: XmlDocGenerator, tB: TypeBuilder,
                                prototypes: IDictionary<string, string>, mB: MemberBuilder, logger: WebSharper.Compiler.LoggerBase) =

    let webResources = List<string * string * byte[]>()

    member a.GetBytes() =
        AssemblyWriter.Write def

    /// WebSharper metadata of the assembly image: read from the image, with the hashes
    /// of its web resources.
    member a.GetWebSharperMetadata(image: WebSharper.Compiler.PEMetadata.PEAssembly) =
        let meta = WebSharper.Compiler.Reflector.TransformAssembly prototypes image
        let bytes = WebSharper.Compiler.FrontEnd.ModifyWIGAssembly meta image (webResources.ToArray())
        logger.TimedStage "Creating WebSharper metadata"
        bytes

    /// Adds web resources, written to the sidecar file. The first one with a name is kept.
    member a.AddWebResources(resources: seq<string * string * byte[]>) =
        for (file, _, _) as r in resources do
            if not (webResources.Exists(fun (n, _, _) -> n = file)) then
                webResources.Add r

    /// Writes the assembly and its sidecar file with the WebSharper metadata and web resources.
    member a.Save(path: string) =
        let d = Path.GetDirectoryName path
        let dir = DirectoryInfo d
        if not dir.Exists then
            dir.Create()
        let bytes = a.GetBytes()
        logger.TimedStage "Writing assembly"
        File.WriteAllBytes(path, bytes)
        let image = WebSharper.Compiler.PEMetadata.PEAssembly.Read bytes
        let meta = a.GetWebSharperMetadata image
        WebSharper.Compiler.FrontEnd.WriteSidecar path image.ModuleId [
            yield WebSharper.Constants.EmbeddedResources.EMBEDDED_METADATA, meta
            yield! WebSharper.Compiler.FrontEnd.WebResourceSidecarEntries (webResources.ToArray())
        ]

    member a.Write(out: Stream) =
        let bytes = a.GetBytes()
        out.Write(bytes, 0, bytes.Length)

    member a.FileName =
        def.Name + ".dll"

    /// Copies the assembly attributes describing the assembly (title, versions, metadata...)
    /// with string arguments from the interface generator assembly.
    member internal a.SetAssemblyAttributes(orig: Assembly) =
        let attrTypes =
            HashSet [|
                typeof<System.Reflection.AssemblyCompanyAttribute>
                typeof<System.Reflection.AssemblyCopyrightAttribute>
                typeof<System.Reflection.AssemblyProductAttribute>
                typeof<System.Reflection.AssemblyTitleAttribute>
                typeof<System.Reflection.AssemblyFileVersionAttribute>
                typeof<System.Reflection.AssemblyInformationalVersionAttribute>
                typeof<System.Reflection.AssemblyMetadataAttribute>
                typeof<System.Runtime.Versioning.TargetFrameworkAttribute>
            |]
        for data in orig.GetCustomAttributesData() do
            let attrType = data.Constructor.DeclaringType
            if attrTypes.Contains attrType then
                let args = Seq.toList data.ConstructorArguments
                let strings =
                    args |> List.choose (fun x ->
                        match x.Value with
                        | :? string as s when x.ArgumentType = typeof<string> -> Some s
                        | _ -> None)
                if not args.IsEmpty && strings.Length = args.Length then
                    let ctor = mB.Constructor(tB.Type attrType, strings |> List.map (fun _ -> tB.String))
                    def.CustomAttributes.Add { Constructor = ctor; Arguments = strings |> List.map AString }

[<Sealed>]
type Compiler(logger: WebSharper.Compiler.LoggerBase) =

    let iG = InlineGenerator()

    let createAssemblyResolver (opts: CompilerOptions) =
        let aR =
            match opts.AssemblyResolver with
            | Some aR -> aR
            | None -> WebSharper.Compiler.AssemblyResolver.Create()
        aR.SearchPaths(opts.ReferencePaths)

    let getId (d: Code.NamespaceEntity) =
        d.Id

    let visit visitClass visitInterface visitResource visitNestedClass visitNestedInterface (assembly: Code.Assembly) =
        let names = HashSet()
        let genCl (c: CodeModel.Class) =
            if c.Generics.Length > 0 then "`" + string c.Generics.Length else ""
        let genInt (i: CodeModel.Interface) =
            if i.Generics.Length > 0 then "`" + string i.Generics.Length else ""
        let rec onClass (ctx: Choice<string,Code.Class>) fullName sourceName (c: Code.Class) =
            match ctx with
            | Choice1Of2 ns -> visitClass ns sourceName c
            | Choice2Of2 parent -> visitNestedClass parent sourceName c
            for nC in c.NestedClasses do
                let nestedSourceName = iG.GetSourceName nC
                let nestedFullName = fullName + "." + nestedSourceName + genCl nC
                if not (names.Add nestedFullName) then failwithf "Duplicate type definition: %s" nestedFullName
                onClass (Choice2Of2 c) nestedFullName nestedSourceName nC
            for nI in c.NestedInterfaces do
                let nestedSourceName = iG.GetSourceName nI
                let nestedFullName = fullName + "." + nestedSourceName + genInt nI
                if not (names.Add nestedFullName) then failwithf "Duplicate type definition: %s" nestedFullName
                visitNestedInterface c nestedSourceName nI
        for ns in assembly.Namespaces do
            for c in ns.Classes do
                let sourceName = iG.GetSourceName c
                let fullName = ns.Name + "." + sourceName + genCl c
                if not (names.Add fullName) then failwithf "Duplicate type definition: %s" fullName
                onClass (Choice1Of2 ns.Name) fullName sourceName c
            for i in ns.Interfaces do
                let sourceName = iG.GetSourceName i
                let fullName = ns.Name + "." + sourceName + genInt i
                if not (names.Add fullName) then failwithf "Duplicate type definition: %s" fullName
                visitInterface ns.Name sourceName i
            for r in ns.Resources do
                let sourceName = iG.GetSourceName r
                let fullName = ns.Name + "." + sourceName
                if not (names.Add fullName) then failwithf "Duplicate type definition: %s" fullName
                visitResource ns.Name sourceName r

    let buildInitialTypes assembly (def: AssemblyDef) =
        let types : Types = Dictionary()
        let genTypes : GenericTypes = Dictionary()
        let build attrs ns sourceName (x: Code.NamespaceEntity) =
            let attrs = attrs ||| TypeAttributes.Public
            let tD = TypeDef(ns, sourceName, attrs)
            types.[getId x] <- tD
            def.Types.Add(tD)
        let buildType attrs ns sourceName (x: Code.TypeDeclaration) =
            let attrs = if x.IsAbstract then attrs ||| TypeAttributes.Abstract else attrs
            build attrs ns sourceName x
            match x.Generics.Length with
            | 0 -> ()
            | gs -> genTypes.[getId x] <- gs
        let buildNested attrs (parent: Code.NamespaceEntity) sourceName (x: Code.TypeDeclaration) =
            match types.TryGetValue parent.Id with
            | true, parent ->
                let attrs = attrs ||| TypeAttributes.NestedPublic
                let attrs = if x.IsAbstract then attrs ||| TypeAttributes.Abstract else attrs
                let tD = TypeDef("", sourceName, attrs)
                types.[getId x] <- tD
                parent.AddNestedType tD
                match x.Generics.Length with
                | 0 -> ()
                | gs -> genTypes.[getId x] <- gs
            | _ -> ()
        assembly
        |> visit
            (buildType TypeAttributes.Class)
            (buildType TypeAttributes.Interface)
            (build TypeAttributes.Class)
            (buildNested TypeAttributes.Class)
            (buildNested TypeAttributes.Interface)
        types, genTypes

    /// The assembly to generate. The identity, Win32 resources (file properties) and PE
    /// header settings come from the interface generator assembly, if there is one.
    let createAssembly options (filePath: string) (hasOriginal: bool) =
        let def = AssemblyDef(options.AssemblyName, options.AssemblyVersion)
        let keyPair =
            options.StrongNameKeyPath |> Option.bind (fun p -> WebSharper.Compiler.PEWriter.ReadKeyPair (File.ReadAllBytes p))
        def.KeyPair <- keyPair
        keyPair |> Option.iter (fun k -> def.PublicKey <- k.PublicKey)
        if hasOriginal then
            use pe = new PEReader(File.ReadAllBytes filePath |> System.Collections.Immutable.ImmutableArray.Create<byte>)
            let md = System.Reflection.Metadata.PEReaderExtensions.GetMetadataReader pe
            let a = md.GetAssemblyDefinition()
            def.Culture <- md.GetString a.Culture
            def.Flags <- a.Flags
            if keyPair.IsNone then
                def.PublicKey <- md.GetBlobBytes a.PublicKey
            def.Win32Resources <-
                match WebSharper.Compiler.PEWriter.CopiedWin32Resources.TryRead pe with
                | Some r -> r :> ResourceSectionBuilder
                | None -> null
            let h = pe.PEHeaders
            def.Header <-
                PEHeaderBuilder(
                    machine = h.CoffHeader.Machine,
                    imageCharacteristics = h.CoffHeader.Characteristics,
                    subsystem = h.PEHeader.Subsystem,
                    dllCharacteristics = h.PEHeader.DllCharacteristics)
        def

    let buildAssembly resolver options (assembly: Code.Assembly) (hasOriginal: bool) (filePath: string) =
        if box assembly = null then
            failwithf "buildAssembly: assembly cannot be null"
        let comments : Comments = Dictionary()
        let def = createAssembly options filePath hasOriginal
        let types, genTypes = buildInitialTypes assembly def
        let tB = TypeBuilder(resolver)
        let tC = TypeConverter(tB, types, genTypes)
        let mB = MemberBuilder(tB)
        let mC = MemberConverter(tB, mB, tC, types, iG, def.CustomAttributes, comments, options, [])
        assembly
        |> visit
            (fun _ _ c -> mC.Class c)
            (fun _ _ i -> mC.Interface i)
            (fun _ _ r -> mC.Resource r)
            (fun _ _ c -> mC.Class c)
            (fun _ _ i -> mC.Interface i)
        mC.AddDependencies(assembly, def.CustomAttributes)
        (def, comments, mB, tB)

    member c.Compile(aR: WebSharper.Compiler.AssemblyResolver, options, assembly, filePath: string, ?originalAssembly: Assembly) =
        let resolver = ReferenceResolver(aR, options.ReferencePaths, options.AssemblyName)
        let (def, comments, mB, tB) = buildAssembly resolver options assembly (originalAssembly.IsSome && filePath <> "") filePath
        logger.TimedStage "Built assembly"
        // embedded JavaScript and CSS files are web resources in the sidecar file, others
        // are embedded in the assembly
        let webResources = List()
        for f in options.EmbeddedResources do
            let name = Path.GetFileName(f)
            let contents =
                try File.ReadAllBytes(f)
                with _ -> failwithf "Failed to add resource file: %s" f
            match CT.TryGuessByFileName name with
            | Some (CT.JavaScript as res)
            | Some (CT.Css as res) ->
                webResources.Add((name, res.Text, contents))
            | _ ->
                def.Resources.Add((name, contents))

        let assemblyPrototypes = Dictionary()
        for ns in assembly.Namespaces do
            let rec addClass parentFullName (c: CodeModel.Class) =
                let sn =
                    parentFullName +
                    (iG.GetSourceName c) +
                    (match c.Generics with [] -> "" | g -> "`" + string (List.length g))
                assemblyPrototypes.Add(sn, c.Name)
                for nc in c.NestedClasses do addClass (sn + "+") nc
            for c in ns.Classes do addClass (ns.Name + ".") c

        let doc = XmlDocGenerator(options.AssemblyName, def.AllTypes, comments)
        // WebSharper metadata is created from the written assembly
        let r = CompiledAssembly(def, doc, tB, assemblyPrototypes, mB, logger)
        r.AddWebResources webResources
        match originalAssembly with
        | None -> ()
        | Some assem -> r.SetAssemblyAttributes(assem)
        match options.OutputPath with
        | None -> ()
        | Some out -> r.Save out
        match options.DocPath with
        | None -> ()
        | Some docPath -> doc.Generate docPath
        r

    member c.Compile(options, assembly, filePath, ?original) =
        let aR = createAssemblyResolver options
        c.Compile(aR, options, assembly, filePath, ?originalAssembly = original)

    member c.StartProgram(args, assembly, ?resolver: WebSharper.Compiler.AssemblyResolver, ?originalAssembly: Assembly) =
        let opts =
            let opts = CompilerOptions.Parse args
            match resolver with
            | None -> opts
            | Some r -> { opts with AssemblyResolver = Some r }
        let aR = createAssemblyResolver opts
        aR.Wrap <| fun () ->
            c.Compile(aR, opts, assembly, "", ?originalAssembly = originalAssembly)
            |> ignore
        0

    member c.Start(args, assembly, ?resolver) =
        c.StartProgram(args, assembly, ?resolver = resolver)

    member c.Start(args, assembly, original, ?resolver) =
        c.StartProgram(args, assembly, ?resolver = resolver, originalAssembly = original)

    static member Create(logger) =
        Compiler(logger)

module ExtensionAssembly =

    let Load (file: string) (logger: WebSharper.Compiler.LoggerBase) =
        let asm = R.LoadAssembly(file)
        let genFile = Path.ChangeExtension(file, ".Generator.dll")
        if File.Exists genFile then File.Delete genFile
        // Instead of removing the original dll, we are going to keep the original F# dll as is,
        // so we can properly carry over the win32 resources, so that the file properties are populated
        File.Copy(file, genFile)
        let name = asm.GetName()
        let typedArg =
            asm.CustomAttributes |> Seq.tryPick (fun a ->
                if a.AttributeType.FullName = "WebSharper.InterfaceGenerator.Pervasives+ExtensionAttribute" then
                    Some a.ConstructorArguments.[0]
                else
                    None
            )
        match typedArg with 
        | Some a ->
            let typeName = (a.Value :?> System.Type).FullName
            let t = asm.GetType(typeName)
            let e = Activator.CreateInstance(t)
            logger.TimedStage "Loading IExtension implementation"
            let assemblyGetter =
                let intfMap = t.GetInterfaceMap(typeof<Pervasives.IExtension>)
                intfMap.InterfaceMethods.[0]
            let asmDefObj = assemblyGetter.Invoke(e, [||])
            let asmDef = asmDefObj :?> Code.Assembly
            logger.TimedStage "Getting Assembly definition"
            name, asmDef, asm
        | None ->
            failwith "No ExtensionAttribute set on the input assembly"
