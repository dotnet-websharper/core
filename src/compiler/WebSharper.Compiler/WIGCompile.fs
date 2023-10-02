// $begin{copyright}
//
// This file is part of WebSharper
//
// Copyright (c) 2008-2018 IntelliFactory
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

/// Compiles `Library` values to `Mono.Cecil` assemblies.
namespace WebSharper.InterfaceGenerator

open System
open System.Collections.Generic
open System.IO
open System.Reflection
open System.Text.RegularExpressions
open System.Xml
open System.Xml.Linq
open Mono.Cecil
open Mono.Cecil.Cil
open WebSharper.Core

module Code = WebSharper.InterfaceGenerator.CodeModel
module CT = WebSharper.Core.ContentTypes
module R = WebSharper.Core.Reflection
module Ty = WebSharper.InterfaceGenerator.Type

type T = Ty.Type
type Comments = Dictionary<MemberReference,string>
type Types = Dictionary<Ty.Id,TypeDefinition>
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
type TypeBuilder(aR: WebSharper.Compiler.LoaderUtility.Resolver, out: AssemblyDefinition, referencePaths: string seq) =
    let assemblies = Dictionary()
    let main = out.MainModule
    let resolvedTypes = Dictionary()
    
    let netstandard =
        let netstandardPath = 
            referencePaths
            |> Seq.tryPick (fun p ->
                if Path.GetFileName(p).ToLower() = "netstandard.dll" then Some p else None
            )
        match netstandardPath with
        | Some p -> 
            AssemblyDefinition.ReadAssembly p
        | _ ->
            failwith "WIG project requires netstandard.dll reference"

    let corelib =
        let corePath = Assembly.GetAssembly(typeof<obj>).Location
        AssemblyDefinition.ReadAssembly corePath
    
    do
        assemblies.["netstandard"] <- netstandard
        assemblies.["System.Private.CoreLib"] <- corelib

    let rec correctType (t: TypeReference) =
        if AssemblyConventions.IsNetStandardType t.FullName then
            t.Scope <- netstandard.Name
        match t with
        | :? GenericInstanceType as g ->
            for a in g.GenericArguments do
                correctType a
        | :? ArrayType as a ->
            correctType a.ElementType
        | _ -> ()
        ()

    let rec correctMethod (m: MethodReference) =
        correctType m.ReturnType
        correctType m.DeclaringType
        for p in m.Parameters do
            correctParameter p
        match m with 
        | :? GenericInstanceMethod as g ->
            for a in g.GenericArguments do
                correctType a
        | _ -> ()

    and correctParameter (p: ParameterDefinition) =
        correctType p.ParameterType
        for a in p.CustomAttributes do
            correctAttribute a

    and correctAttribute (a: CustomAttribute) =
        correctMethod a.Constructor
        for a in a.ConstructorArguments do
            correctAttributeArg a
        for a in a.Properties do
            correctAttributeNamedArg a
        for a in a.Fields do
            correctAttributeNamedArg a

    and correctAttributeArg (a: CustomAttributeArgument) =
        correctType a.Type

    and correctAttributeNamedArg (a: CustomAttributeNamedArgument) =
        correctAttributeArg a.Argument

    let resolveAsm (asmName: string) =
        match assemblies.TryGetValue(AssemblyName(asmName).Name) with
        | true, x -> x
        | false, _ ->
            let asm = aR.Resolve(asmName)
            assemblies.[asmName] <- asm
            asm

    let doResolveTypeName (asm: AssemblyDefinition) (origAsmName: string) (typeName: string) =
        let ty = 
            match asm.MainModule.GetType(typeName.Replace('+', '/')) with
            | null -> 
                let asmName = string asm.Name
                let asmNameWithOrig =
                    if asmName = origAsmName then
                        asmName
                    else
                        origAsmName + " loaded as " + asmName
                failwithf "Could not resolve type %s in %A" typeName asmNameWithOrig
            | ty when asm.Name = out.Name ->
                correctType ty
                ty :> TypeReference
            | ty -> 
                correctType ty
                main.ImportReference ty
        resolvedTypes.[typeName] <- ty
        ty

    let resolveTypeName asm origAsmName typeName =
        match resolvedTypes.TryGetValue(typeName) with
        | true, x -> x
        | false, _ -> doResolveTypeName asm origAsmName typeName

    let resolveType (ty: System.Type) =
        match resolvedTypes.TryGetValue(ty.FullName) with
        | true, x -> x
        | false, _ ->
            let origAsmName = ty.Assembly.FullName
            let asm = resolveAsm origAsmName
            resolveTypeName asm origAsmName ty.FullName

    let genericInstance (def: TypeReference) (args: seq<TypeReference>) =
        if Seq.isEmpty args then
            def
        else
            let r = 
                match def with
                | :? GenericInstanceType as r ->
                    r.GenericArguments.Clear()
                    r
                | _ -> GenericInstanceType(def)
            for x in args do
                r.GenericArguments.Add(x)
            r :> TypeReference

    let wsCore =
        let t = typeof<WebSharper.InlineAttribute>
        resolveAsm t.Assembly.FullName

    let commonType (assembly: AssemblyDefinition) origAsmName baseName ts =
        let name =
            match Seq.length ts with
            | 0 -> baseName
            | k -> baseName + "`" + string k
        let tDef = resolveTypeName assembly origAsmName name
        genericInstance tDef ts

    let coreLibType baseName ts = commonType corelib "System.Private.CoreLib" baseName ts
    let wsCoreType baseName ts = commonType wsCore "WebSharper.Core" baseName ts

    // First resolve FSharp.Core without explicit version,
    // so that the resolver can pick up the version from the passed references.
    let fsharpCore = resolveAsm "FSharp.Core"
    let funcType = resolveTypeName fsharpCore "FSharp.Core" "Microsoft.FSharp.Core.FSharpFunc`2"
    let attributeType = resolveType typeof<System.Attribute>
    let converterType = resolveType typedefof<System.Converter<_, _>>
    let objectType = resolveType typeof<obj>
    let stringType = resolveType typeof<string>
    let systemType = resolveType typeof<System.Type>
    let voidType = resolveType typeof<System.Void>
    let webResource = resolveType typeof<WebSharper.WebResourceAttribute>
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

    member b.CorrectType t = correctType t; t

    member b.CorrectMethod m = correctMethod m; m

    member b.CoreLib = corelib

    member b.Action ts =
        coreLibType "System.Action" ts

    member b.Converter d r =
        genericInstance converterType [d; r]

    member b.Function d r =
        genericInstance funcType [d; r]

    member c.GenericInstanceType(def: TypeReference, args: seq<TypeReference>) =
        genericInstance def args

    member b.Tuple(ts: seq<TypeReference>) =
        let rec createTuple (ta: _[]) =
            if ta.Length < 8 then
                coreLibType "System.Tuple" ta
            else
                coreLibType "System.Tuple" (Seq.append (ta.[.. 6]) [ createTuple ta.[7 ..] ])    
        createTuple (Array.ofSeq ts)

    member b.Choice(ts: seq<TypeReference>) =
        wsCoreType "WebSharper.JavaScript.Union" ts

    member b.Option t =
        genericInstance optionType [t]    

    member b.Type(assemblyName: string, fullName: string) =
        match resolveAsm assemblyName with
        | null -> failwithf "Could not resolve assembly: %s" assemblyName
        | asm -> resolveTypeName asm assemblyName fullName

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
    member b.WebResource = webResource

[<Sealed>]
type TypeConverter private (tB: TypeBuilder, types: Types, genTypes: GenericTypes, 
                                genericsByPosition: GenericParameter [], genericsById: Type.Id -> GenericParameter) =

    let byId id =
        match types.TryGetValue id with
        | true, x -> x :> TypeReference
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
            ArrayType(c.TypeReference t, rank) :> TypeReference
        | R.Type.Concrete (def, args) ->
            if not args.IsEmpty then
                let r = GenericInstanceType(c.TypeReference def)
                for a in args do
                    r.GenericArguments.Add(c.TypeReference a)
                r :> TypeReference
            else
                c.TypeReference def
        | R.Type.Generic pos ->
            genericsByPosition.[pos] :> _

    member private c.TypeReference(t: T, defT: Code.TypeDeclaration, allowGeneric: bool, isCSharp: bool) =
        let tRef x = c.TypeReference(x, defT, false, isCSharp)
        let tres =
            match t with
            | Type.ArrayType (rank, t) ->
                ArrayType(tRef t, rank) :> TypeReference
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
                genericsById i :> _
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
                let gen = 
                    match t with
                    | :? GenericInstanceType as t -> t.GenericArguments.Count
                    | _ -> t.GenericParameters.Count
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
            | Type.DefiningType ->
                byId defT.Id
        // check missing generics
        if not allowGeneric then
            match t with
            | Type.DeclaredType id ->
                if genTypes.ContainsKey id then
                    failwithf "Generic parameters not applied on %s in member of %s" tres.FullName defT.Name       
            | _ ->
                if tres.HasGenericParameters && not (tres :? GenericInstanceType) then
                    failwithf "Generic parameters not applied on %s in member of %s" tres.FullName defT.Name       
        tres

    member c.TypeReference(t: T, defT: Code.TypeDeclaration) =
        c.TypeReference(t, defT, false, true)

    member c.TypeReference(t: T, defT: Code.TypeDeclaration, isCSharp) =
        c.TypeReference(t, defT, false, isCSharp)

    member c.WithGenerics(gs: seq<GenericParameter>, byId: Type.Id -> GenericParameter option) =
        let gbi i =
            match byId i with
            | Some p -> p
            | _ -> genericsById i
        TypeConverter(tB, types, genTypes, Array.append genericsByPosition (Seq.toArray gs), gbi)

[<Sealed>]
type MemberBuilder(tB: TypeBuilder, def: AssemblyDefinition) =

    let findConstructor (t: TypeReference) (isMatch: MethodDefinition -> bool) =
        t.Resolve().Methods
        |> Seq.tryFind (fun m -> m.IsConstructor && isMatch m)
        |> function
            | Some x ->
                tB.CorrectMethod x
                |> def.MainModule.ImportReference
            | None -> failwithf "Could not find a constructor in %s" t.FullName

    let findConstructorByArity t n =
        findConstructor t (fun m -> m.Parameters.Count = n)

    let findDefaultConstructor t =
        findConstructorByArity t 0

    let findTypedConstructor t names =
        findConstructor t (fun m ->
            m.Parameters.Count = List.length names
            && (m.Parameters, names) ||> Seq.forall2 (fun p n -> p.ParameterType.Name = n))

    let webResourceConstructor = findTypedConstructor tB.WebResource [tB.String.Name; tB.String.Name]

    let paramArrayConstructor = findDefaultConstructor tB.ParamArray
    let notImplementedConstructor = findDefaultConstructor tB.NotImplemented
    let baseResourceCtor1 = findTypedConstructor tB.BaseResource [tB.String.Name]
    let baseResourceCtorN = findConstructorByArity tB.BaseResource 3
    let nameAttributeConstructor = findTypedConstructor tB.Name [tB.String.Name]
    let inlineAttributeConstructor = findTypedConstructor tB.Inline [tB.String.Name]
    let macroAttributeConstructor = findTypedConstructor tB.Macro [tB.SystemType.Name]
    let requireAttributeConstructor = findTypedConstructor tB.Require [tB.SystemType.Name]
    let obsoleteAttributeConstructor = findDefaultConstructor tB.Obsolete 
    let obsoleteAttributeWithMsgConstructor = findTypedConstructor tB.Obsolete [tB.String.Name]
    let pureAttributeConstructor = findDefaultConstructor tB.Pure 
    let warnAttributeConstructor = findTypedConstructor tB.Warn [tB.String.Name]
    let typeAttributeConstructor = findTypedConstructor tB.TypeAttr [tB.String.Name]
    let importAttributeConstructorDefault = findTypedConstructor tB.Import [tB.String.Name]
    let importAttributeConstructor = findTypedConstructor tB.Import [tB.String.Name; tB.String.Name]

    member c.AddBody(m: MethodDefinition) =
        let body = MethodBody(m)
        Instruction.Create(OpCodes.Newobj, notImplementedConstructor)
        |> body.Instructions.Add
        Instruction.Create(OpCodes.Throw)
        |> body.Instructions.Add
        m.Body <- body

    member c.BuildConstructor(?attrs) =
        let attrs =
            defaultArg attrs MethodAttributes.Public
            ||| MethodAttributes.SpecialName
            ||| MethodAttributes.RTSpecialName
        let r = MethodDefinition(".ctor", attrs, tB.Void)
        c.AddBody(r)
        r

    member c.BuildParamArrayParameter(t: TypeReference) =
        let t = ArrayType(t)
        let p = ParameterDefinition("ps", ParameterAttributes.None, t)
        p.CustomAttributes.Add(CustomAttribute paramArrayConstructor)
        p

    member c.BuildWebResourceAttribute(a: string, b: string) =
        let attr = CustomAttribute(webResourceConstructor)
        attr.ConstructorArguments.Add(CustomAttributeArgument(tB.String, a))
        attr.ConstructorArguments.Add(CustomAttributeArgument(tB.String, b))
        attr

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


type CompilationKind =
    | LibraryKind
    | ConsoleKind
    | WindowsKind

    static member Library = LibraryKind
    static member Console = ConsoleKind
    static member Windows = WindowsKind

type CompilerOptions =
    {
        AssemblyName : string
        AssemblyResolver : option<WebSharper.Compiler.AssemblyResolver>
        AssemblyVersion : Version
        DocPath : option<string>
        EmbeddedResources : seq<string>
        Kind : CompilationKind
        OutputPath : option<string>
        ProjectDir : string
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
            Kind = LibraryKind
            OutputPath = None
            ProjectDir = "."
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
            | "-console" ->
                { opts with Kind = CompilationKind.Console }
            | "-library" ->
                { opts with Kind = CompilationKind.Library }
            | "-windows" ->
                { opts with Kind = CompilationKind.Windows }
            | _ ->
                opts)

[<Sealed>]
type MemberConverter
        (
            tB: TypeBuilder,
            mB: MemberBuilder,
            tC: TypeConverter,
            types: Types,
            iG: InlineGenerator,
            def: AssemblyDefinition,
            comments: Comments,
            compilerOptions: CompilerOptions,
            genParamNames: string list
        ) =

    let nameAttribute (code: string) =
        let attr = CustomAttribute(mB.NameAttributeConstructor)
        attr.ConstructorArguments.Add(CustomAttributeArgument(tB.String, code))
        attr

    let inlineAttribute (code: string) =
        let attr = CustomAttribute(mB.InlineAttributeConstructor)
        attr.ConstructorArguments.Add(CustomAttributeArgument(tB.String, code))
        attr

    let macroAttribute (macroType: TypeReference) =
        let attr = CustomAttribute(mB.MacroAttributeConstructor)
        attr.ConstructorArguments.Add(CustomAttributeArgument(tB.SystemType, macroType))
        attr

    let requireAttribute (resourceType: TypeReference) =
        let attr = CustomAttribute(mB.RequireAttributeConstructor)
        attr.ConstructorArguments.Add(CustomAttributeArgument(tB.SystemType, resourceType))
        attr

    let obsoleteAttribute = CustomAttribute(mB.ObsoleteAttributeConstructor)
    let obseleteAttributeWithMsg (msg: string) =
        let ca = CustomAttribute(mB.ObsoleteAttributeWithMsgConstructor)
        ca.ConstructorArguments.Add(CustomAttributeArgument(tB.String, box msg))
        ca
    let setObsoleteAttribute (x: CodeModel.Entity) (attrs: Mono.Collections.Generic.Collection<CustomAttribute>) =
        match x.ObsoleteStatus with
        | CodeModel.NotObsolete -> ()
        | CodeModel.Obsolete None -> attrs.Add obsoleteAttribute
        | CodeModel.Obsolete (Some msg) -> attrs.Add (obseleteAttributeWithMsg msg)

    let typeAttribute (t: string) =
        let ca = CustomAttribute(mB.TypeAttributeConstructor)
        ca.ConstructorArguments.Add(CustomAttributeArgument(tB.String, box t))
        ca
    let setTSAttribute (x: CodeModel.TypeDeclaration) (attrs: Mono.Collections.Generic.Collection<CustomAttribute>) =
        match x.TSType with
        | None -> ()
        | Some t -> attrs.Add (typeAttribute t)

    let importAttribute (n: option<string>) (f: string) =
        match n with
        | None ->
            let attr = CustomAttribute(mB.ImportAttributeConstructorDefault)
            attr.ConstructorArguments.Add(CustomAttributeArgument(tB.String, box f))
            attr
        | Some n ->
            let attr = CustomAttribute(mB.ImportAttributeConstructor)
            attr.ConstructorArguments.Add(CustomAttributeArgument(tB.String, box n))
            attr.ConstructorArguments.Add(CustomAttributeArgument(tB.String, box f))
            attr
    let setImportAttribute (x: CodeModel.Entity) (attrs: Mono.Collections.Generic.Collection<CustomAttribute>) =
        match x.Import with
        | None -> ()
        | Some (n, f) -> attrs.Add (importAttribute n f)

    let pureAttribute = CustomAttribute(mB.PureAttributeConstructor)

    let setPureAttribute (x: CodeModel.MethodBase) (attrs: Mono.Collections.Generic.Collection<CustomAttribute>) =
        if x.IsPure then attrs.Add pureAttribute

    let warnAttribute warning = 
        let attr = CustomAttribute(mB.WarnAttributeConstructor)
        attr.ConstructorArguments.Add(CustomAttributeArgument(tB.String, warning))
        attr 

    let setWarnAttribute (x: CodeModel.Entity) (attrs: Mono.Collections.Generic.Collection<CustomAttribute>) =
        match x.Warning with
        | Some warning -> attrs.Add (warnAttribute warning)
        | _ -> ()

    let makeParameters (f: Type.Function, defT, isCSharp) =
        Seq.ofArray [|
            for (n, t) in f.Parameters do
                yield ParameterDefinition(n, ParameterAttributes.None, tC.TypeReference (t, defT, isCSharp))
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

    let methodAttributes (dt: TypeDefinition) (x: Code.Entity) =
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

    let addDependencies (ent: Code.IResourceDependable) (attrs: Mono.Collections.Generic.Collection<CustomAttribute>) =
        for d in ent.GetRequires() do
            match d with
            | Code.LocalDependency d ->
                match types.TryGetValue(d) with
                | true, t -> attrs.Add(requireAttribute t)
                | _ -> ()
            | Code.ExternalDependency ty ->
                let t = tC.TypeReference (ty, Unchecked.defaultof<_>)
                attrs.Add(requireAttribute t)

    let addConstructor (dT: TypeDefinition) (td: Code.TypeDeclaration) (x: Code.Constructor) =
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

    let addProperty (dT: TypeDefinition) (td: Code.TypeDeclaration) name (p: Code.Property) =
        let t =
            match p.GetterInline, p.SetterInline with
            | Some (Code.BasicInline _), _
            | _, Some (Code.BasicInline _) -> Type.Normalize p.Type
            | _ -> Type.TransformOption true (Type.Normalize p.Type)
        let ty = tC.TypeReference (t, td)
        //let name = iG.GetPropertySourceName p
        let attrs = PropertyAttributes.None
        let pD = PropertyDefinition(name, attrs, ty)
        do
            match p.Comment with
            | None -> ()
            | Some c -> comments.[pD] <- c
        let isInterface = dT.IsInterface
        let isMixin = isInterface && not (p.GetterInline.IsSome || p.HasSetter || p.IndexerType.IsSome)
        if p.HasGetter then
            let mD = MethodDefinition("get_" + name, methodAttributes dT p, ty)
            mD.IsGetter <- true
            if isMixin then
                p.Name 
                |> nameAttribute
                |> mD.CustomAttributes.Add
            else
                if not isInterface then
                    mB.AddBody mD
                iG.GetPropertyGetterInline(td, t, p)
                |> inlineAttribute
                |> mD.CustomAttributes.Add
            match p.IndexerType with
            | None -> ()
            | Some it ->
                mD.Parameters.Add(ParameterDefinition("index", ParameterAttributes.None, tC.TypeReference (it, td)))         
            dT.Methods.Add mD
            pD.GetMethod <- mD
        if p.HasSetter then
            let mD = MethodDefinition("set_" + name, methodAttributes dT p, tB.Void)
            mD.IsSetter <- true
            if isMixin then
                p.Name
                |> nameAttribute
                |> mD.CustomAttributes.Add
            else
                if not isInterface then
                    mB.AddBody mD
                iG.GetPropertySetterInline(td, t, p)
                |> inlineAttribute
                |> mD.CustomAttributes.Add
            match p.IndexerType with
            | None -> ()
            | Some it ->
                mD.Parameters.Add(ParameterDefinition("index", ParameterAttributes.None, tC.TypeReference (it, td)))         
            mD.Parameters.Add(ParameterDefinition("value", ParameterAttributes.None, ty))
            dT.Methods.Add mD
            pD.SetMethod <- mD
        setObsoleteAttribute p pD.CustomAttributes
        setImportAttribute p pD.CustomAttributes 
        addDependencies p pD.CustomAttributes
        setWarnAttribute p pD.CustomAttributes
        dT.Properties.Add pD

    let withGenerics (generics: Code.TypeParameter list, td: Code.TypeDeclaration, owner) =
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
                    let gP = GenericParameter(n, owner)
                    for c in g.Constraints do
                        gP.Constraints.Add(GenericParameterConstraint(tC.TypeReference (c, td)))
                    owner.GenericParameters.Add(gP)
                    g.Id, gP
            ]        
        let byId i =
            gs |> List.tryPick (fun (id, p) -> if i = id then Some p else None)    
        MemberConverter(tB, mB, tC.WithGenerics (gs |> Seq.map snd, byId), types, iG, def, comments, 
            compilerOptions, List.ofSeq namesTaken)

    let genericType (x: Code.TypeDeclaration) k =
        match types.TryGetValue(x.Id) with
        | true, tD ->
            if x.Generics.Length > 0 then
                tD.Name <- tD.Name + "`" + string x.Generics.Length
            k (withGenerics (x.Generics, x, tD)) tD
            setWarnAttribute x tD.CustomAttributes
        | _ -> ()

    member private c.AddMethod(dT: TypeDefinition, td: Code.TypeDeclaration, x: Code.Method) =
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
                let mD = MethodDefinition(name, attrs, tB.Object)
                do
                    match x.Comment with
                    | None -> ()
                    | Some c -> comments.[mD] <- c
                let c = withGenerics (x.Generics, td, mD)
                c.AddMethod(dT, td, x, mD, t, f, isCSharp)
            | _ -> ()

    member private c.AddMethod(dT: TypeDefinition, td: Code.TypeDeclaration, x: Code.Method, mD: MethodDefinition, t: Type.Type, f: Type.Function, isCSharp: bool) =
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
            (x: 'T, tD: TypeDefinition) =
        for m in x.Methods do
            c.AddMethod(tD, x, m)
        let propNames = HashSet()
        for p in x.Properties do
            let name = iG.GetPropertySourceName p
            if not (propNames.Add name) then failwithf "Duplicate property definition: %s on %s" name x.Name
            addProperty tD x name p
        addDependencies x tD.CustomAttributes
        setWarnAttribute x tD.CustomAttributes
    
    member d.AddDependencies(ent: Code.IResourceDependable, prov: ICustomAttributeProvider) =
        addDependencies ent prov.CustomAttributes

    member c.Class(x: Code.Class) =
        genericType x (fun c tD -> c.Class(x, tD))

    member private c.Class(x: Code.Class, tD: TypeDefinition) =
        if x.IsAbstract then
            tD.IsAbstract <- true
        do
            match x.BaseClass with
            | None -> tD.BaseType <- tB.Object
            | Some t -> tD.BaseType <- tC.TypeReference (t, x)
        do
            match x.Comment with
            | None -> ()
            | Some c -> comments.[tD] <- c
        for i in x.ImplementedInterfaces do
            let tr = tC.TypeReference (i, x)
            if tr.Resolve().IsInterface then
                tD.Interfaces.Add(InterfaceImplementation(tr))
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

    member private c.Interface(x: Code.Interface, tD: TypeDefinition) =
        for i in x.BaseInterfaces do
            let tr = tC.TypeReference (i, x)
            if tr.Resolve().IsInterface then
                tD.Interfaces.Add(InterfaceImplementation(tr))
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
            /// recognize embedded resources here.
            do
                match r.Paths with
                | [p] when
                    compilerOptions.EmbeddedResources
                    |> Seq.exists ((=) p) ->
                        let f = Path.Combine(compilerOptions.ProjectDir, r.Name)
                        if File.Exists(f) then
                            EmbeddedResource(Path.GetFileName(f), ManifestResourceAttributes.Public, File.ReadAllBytes(f))
                            |> def.MainModule.Resources.Add
                | _ -> ()
            addDependencies r tD.CustomAttributes
            if r.IsAssemblyWide then
                def.CustomAttributes.Add(requireAttribute tD)
            match r.Paths with
            | [] -> ()
            | [p] ->
                tD.BaseType <- tB.BaseResource
                let ctor = mB.BuildConstructor()
                let body = MethodBody(ctor)
                let add (i: Instruction) = body.Instructions.Add i
                Instruction.Create(OpCodes.Ldarg_0) |> add
                Instruction.Create(OpCodes.Ldstr, p) |> add
                Instruction.Create(OpCodes.Callvirt, mB.BaseResourceConstructor1) |> add
                Instruction.Create(OpCodes.Ret) |> add
                ctor.Body <- body
                tD.Methods.Add ctor
            | a :: b :: rest ->
                tD.BaseType <- tB.BaseResource
                let ctor = mB.BuildConstructor()
                let body = MethodBody(ctor)
                let add (i: Instruction) = body.Instructions.Add i
                Instruction.Create(OpCodes.Ldarg_0) |> add
                Instruction.Create(OpCodes.Ldstr, a) |> add
                Instruction.Create(OpCodes.Ldstr, b) |> add
                Instruction.Create(OpCodes.Ldc_I4, List.length rest) |> add
                Instruction.Create(OpCodes.Newarr, tB.String) |> add
                rest
                |> List.iteri (fun i arg ->
                    Instruction.Create(OpCodes.Ldc_I4, i) |> add
                    Instruction.Create(OpCodes.Ldstr, arg) |> add
                    Instruction.Create(OpCodes.Stelem_Ref, tB.String) |> add)
                Instruction.Create(OpCodes.Callvirt, mB.BaseResourceConstructorN) |> add
                Instruction.Create(OpCodes.Ret) |> add
                ctor.Body <- body
                tD.Methods.Add ctor
        | _ -> ()

[<Sealed>]
type XmlDocGenerator(assembly: AssemblyDefinition, comments: Comments) =

    let getComment (m: MemberReference) =
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

    let rec typeRefId (t: TypeReference) : string =
        let fullName (t: TypeReference) =
            t.FullName.Replace("/", ".")
        if t.IsArray then
            let t = t :?> ArrayType
            match t.Rank with
            | 0 | 1 -> typeRefId t.ElementType + "[]"
            | k -> sprintf "%s, [%s]" (typeRefId t.ElementType) (String.replicate (k - 1) ",")
        elif t.IsGenericInstance then
            let t = t :?> GenericInstanceType
            let untaggedName = t.FullName.[.. (fullName t).IndexOf '`' - 1]
            let pars =
                t.GenericArguments
                |> Seq.map typeRefId
                |> String.concat ","
            sprintf "%s{%s}" untaggedName pars
        elif t.IsGenericParameter then
            let t = t :?> GenericParameter
            match t.DeclaringMethod with
            | null -> "`" + string t.Position
            | _ -> "``" + string t.Position
        else
            fullName t

    let propertyId (p: PropertyDefinition) =
        sprintf "P:%s.%s" (typeRefId p.DeclaringType) p.Name

    let methodId (m: MethodDefinition) =
        let name =
            match m.Name with
            | ".ctor" -> "#ctor"
            | ".cctor" -> "#cctor"
            | n -> n
        let gen = if m.HasGenericParameters then "``" + string m.GenericParameters.Count else ""
        sprintf "M:%s.%s%s%s" (typeRefId m.DeclaringType) name gen
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

    let visitMethod m =
        visitMember (methodId m) (getComment m)

    let visitProperty p =
        visitMember (propertyId p) (getComment p)

    let rec visitType (t: TypeDefinition) =
        seq {
            yield! visitMember ("T:" + typeRefId t) (getComment t)
            for m in t.Methods do
                yield! visitMethod m
            for p in t.Properties do
                yield! visitProperty p
        }

    let visitMembers () =
        seq {
            for t in assembly.MainModule.GetTypes() do
                yield! visitType t
        }

    let generate (fileName: string) =
        let doc = XDocument()
        e "doc" [] [
            yield e "assembly" [] [
                e "name" [] [t assembly.Name.Name]
            ] :> XNode
            for m in visitMembers () do
                yield m :> XNode
        ]
        |> doc.Add
        doc.Save fileName

    member g.Generate(fileName: string) =
        generate fileName

[<Sealed>]
type CompiledAssembly(def: AssemblyDefinition, doc: XmlDocGenerator, options: CompilerOptions, tB: TypeBuilder) =

    let writerParams =
        match options.StrongNameKeyPath with
        | None -> WriterParameters()
        | Some p ->
            WriterParameters(StrongNameKeyBlob = File.ReadAllBytes p)

    member a.GetBytes() =
        use out = new MemoryStream()
        a.Write(out)
        out.ToArray()

    member a.Save(path: string) =
        let mm = def.MainModule
        let d = Path.GetDirectoryName path
        let dir = DirectoryInfo d
        if not dir.Exists then
            dir.Create()
        use out = File.Open(path, FileMode.Create)
        a.Write(out)

    member a.Write(out: Stream) =
        def.Write(out, writerParams)

    member a.FileName =
        def.Name.Name + ".dll"

    member a.SetAssemblyAttributes(orig: Assembly) =
        let attrTypes =
            HashSet [|
                typeof<System.Reflection.AssemblyCompanyAttribute>
                typeof<System.Reflection.AssemblyCopyrightAttribute>
                typeof<System.Reflection.AssemblyProductAttribute>
                typeof<System.Reflection.AssemblyTitleAttribute>
                typeof<System.Reflection.AssemblyFileVersionAttribute>
                typeof<System.Reflection.AssemblyInformationalVersionAttribute>
                typeof<System.Runtime.Versioning.TargetFrameworkAttribute>
            |]
        let getSystemTypeDef (t: Type) =
            tB.CoreLib.MainModule.GetType(t.FullName)
            |> tB.CorrectType
            |> def.MainModule.ImportReference
        let stringTypeDef =
            getSystemTypeDef typeof<string>
        let findStringCtor (ty: TypeReference) =
            ty.Resolve().Methods
            |> Seq.find (fun m ->
                m.IsConstructor
                && m.HasParameters
                && m.Parameters.Count = 1
                && m.Parameters.[0].ParameterType.FullName = stringTypeDef.FullName)
            |> tB.CorrectMethod
            |> def.MainModule.ImportReference
        let setAttr (t: Type) (v: string) =
            let ty = getSystemTypeDef t
            let ctor = findStringCtor ty
            let attr = CustomAttribute(ctor)
            attr.ConstructorArguments.Add(CustomAttributeArgument(stringTypeDef, v))
            def.CustomAttributes.Add(attr)
        for data in orig.GetCustomAttributesData() do
            let attrType = data.Constructor.DeclaringType
            if attrTypes.Contains attrType then
                match Seq.toList data.ConstructorArguments with
                | [x] when x.ArgumentType = typeof<string> ->
                    match x.Value with
                    | :? string as s ->
                        setAttr attrType s
                    | _ -> ()
                | _ -> ()

[<Sealed>]
type Compiler(logger: WebSharper.Compiler.LoggerBase) =

    let iG = InlineGenerator()

    let createAssemblyResolvers (opts: CompilerOptions) =
        let aR =
            match opts.AssemblyResolver with
            | Some aR -> aR
            | None -> WebSharper.Compiler.AssemblyResolver.Create()
        let aR = aR.SearchPaths(opts.ReferencePaths)
        (aR, new WebSharper.Compiler.LoaderUtility.Resolver(aR))

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

    let buildInitialTypes assembly (def: AssemblyDefinition) =
        let types : Types = Dictionary()
        let genTypes : GenericTypes = Dictionary()
        let build attrs ns sourceName (x: Code.NamespaceEntity) =
            let attrs = attrs ||| TypeAttributes.Public
            let tD = TypeDefinition(ns, sourceName, attrs)
            types.[getId x] <- tD
            def.MainModule.Types.Add(tD)
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
                let tD = TypeDefinition(null, sourceName, attrs)
                tD.DeclaringType <- parent
                types.[getId x] <- tD
                parent.NestedTypes.Add tD
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

    let buildAssembly resolver options (assembly: Code.Assembly) (originalAssembly: Assembly option) (filePath: string) =
        if box assembly = null then
            failwithf "buildAssembly: assembly cannot be null"
        let aND = AssemblyNameDefinition(options.AssemblyName, options.AssemblyVersion)
        let mp = ModuleParameters()
        mp.Kind <-
            match options.Kind with
            | ConsoleKind -> ModuleKind.Console
            | LibraryKind -> ModuleKind.Dll
            | WindowsKind -> ModuleKind.Windows
        mp.AssemblyResolver <- resolver
        mp.Runtime <- TargetRuntime.Net_4_0
        let comments : Comments = Dictionary()
        let def =
            // If the original assembly is provided, we should read the Assembly from the fileSystem through Mono.Cecil,
            // instead of creating a new one.
            // Note: the ReaderParameters settings are important, as we want to avoid locking the dll we are handling, otherwise when we want to save the dll,
            // we can run into access denied errors.
            match originalAssembly with
            | Some _ ->
                let rp = ReaderParameters(ReadWrite = true, InMemory = true, ReadingMode = ReadingMode.Immediate)
                AssemblyDefinition.ReadAssembly(filePath, rp)
            | _ ->
                AssemblyDefinition.CreateAssembly(aND, options.AssemblyName, mp)
        def.Modules.Clear()
        def.CustomAttributes.Clear()
        def.MainModule.Resources.Clear()
        let types, genTypes = buildInitialTypes assembly def
        let tB = TypeBuilder(resolver, def, options.ReferencePaths)
        let tC = TypeConverter(tB, types, genTypes)
        let mB = MemberBuilder(tB, def)
        let mC = MemberConverter(tB, mB, tC, types, iG, def, comments, options, [])
        assembly
        |> visit
            (fun _ _ c -> mC.Class c)
            (fun _ _ i -> mC.Interface i)
            (fun _ _ r -> mC.Resource r)
            (fun _ _ c -> mC.Class c)
            (fun _ _ i -> mC.Interface i)
        mC.AddDependencies(assembly, def)
        (def, comments, mB, tB)

    let addResourceExports (mB: MemberBuilder) (def: AssemblyDefinition) =
        let addResource name mime =
            def.CustomAttributes.Add(mB.BuildWebResourceAttribute(name, mime))
        for r in def.MainModule.Resources do
            if r.IsPublic then
                match CT.TryGuessByFileName r.Name with
                | Some (CT.JavaScript as res) ->
                    addResource r.Name res.Text
                | Some (CT.Css as res) ->
                    addResource r.Name res.Text
                | _ -> () // TODO: correct here?

    member c.Compile(resolver, options, assembly, filePath: string, ?originalAssembly: Assembly) =
        let (def, comments, mB, tB) = buildAssembly resolver options assembly originalAssembly filePath
        logger.TimedStage "Built assembly"
        for f in options.EmbeddedResources do
            try
                EmbeddedResource(Path.GetFileName(f), ManifestResourceAttributes.Public, File.ReadAllBytes(f))
                |> def.MainModule.Resources.Add
            with _ ->
                failwithf "Failed to add resource file: %s" f
        addResourceExports mB def
        
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

        // Add WebSharper metadata
        let meta = WebSharper.Compiler.Reflector.TransformAssembly assemblyPrototypes def
        WebSharper.Compiler.FrontEnd.ModifyWIGAssembly meta def |> ignore
        logger.TimedStage "Creating WebSharper metadata"

        let doc = XmlDocGenerator(def, comments)
        let r = CompiledAssembly(def, doc, options, tB)
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
        let (aR, resolver) = createAssemblyResolvers options
        c.Compile(resolver, options, assembly, filePath, ?originalAssembly = original)

    member c.StartProgram(args, assembly, ?resolver: WebSharper.Compiler.AssemblyResolver, ?originalAssembly: Assembly) =
        let opts =
            let opts = CompilerOptions.Parse args
            match resolver with
            | None -> opts
            | Some r -> { opts with AssemblyResolver = Some r }
        let (aR, resolver) = createAssemblyResolvers opts
        aR.Wrap <| fun () ->
            c.Compile(resolver, opts, assembly, "", ?originalAssembly = originalAssembly)
            |> ignore
        0

    member c.Start(args, assembly, ?resolver) =
        c.StartProgram(args, assembly, ?resolver = resolver)

    member c.Start(args, assembly, original, ?resolver) =
        c.StartProgram(args, assembly, ?resolver = resolver, originalAssembly = original)

    static member Create(logger) =
        Compiler(logger)
