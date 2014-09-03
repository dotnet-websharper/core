// $begin{copyright}
//
// This file is part of WebSharper
//
// Copyright (c) 2008-2013 IntelliFactory
//
// GNU Affero General Public License Usage
// WebSharper is free software: you can redistribute it and/or modify it under
// the terms of the GNU Affero General Public License, version 3, as published
// by the Free Software Foundation.
//
// WebSharper is distributed in the hope that it will be useful, but WITHOUT
// ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
// FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License
// for more details at <http://www.gnu.org/licenses/>.
//
// If you are unsure which license is appropriate for your use, please contact
// IntelliFactory at http://intellifactory.com/contact.
//
// $end{copyright}

/// Compiles `Library` values to `Mono.Cecil` assemblies.
namespace IntelliFactory.WebSharper.InterfaceGenerator

open System
open System.Collections.Generic
open System.IO
open System.Reflection
open System.Text.RegularExpressions
open System.Xml
open System.Xml.Linq
open Mono.Cecil
open Mono.Cecil.Cil
open IntelliFactory.Core
open IntelliFactory.WebSharper.Core

module Code = IntelliFactory.WebSharper.InterfaceGenerator.CodeModel
module CT = IntelliFactory.WebSharper.Core.ContentTypes
module R = IntelliFactory.WebSharper.Core.Reflection
module Ty = IntelliFactory.WebSharper.InterfaceGenerator.Type

type T = Ty.Type
type Comments = Dictionary<MemberReference,string>
type Types = Dictionary<Ty.Id,TypeDefinition>

[<Sealed>]
type InlineGenerator() =

    static let validJsIdentRE =
        Regex("^[a-zA-Z_$][0-9a-zA-Z_$]*$")

    static let invalidCsIdentCharRE =
        Regex(@"[^\p{Ll}\p{Lu}\p{Lt}\p{Lo}\p{Nd}\p{Nl}\p{Mn}\p{Mc}\p{Cf}\p{Pc}\p{Lm}]")

    member g.GetMethodBaseInline(td: Code.TypeDeclaration, t: T, m: Code.MethodBase) =
        if m.Inline.IsSome then m.Inline.Value else
            match t with
            | Type.FunctionType f ->
                let args =
                    seq {
                        for i in 0 .. f.Parameters.Length - 1 ->
                            "$" + string ((if m.IsStatic then 0 else 1) + i)
                    }
                    |> String.concat ","
                let arity = f.Parameters.Length
                match f.ParamArray with
                | Some v ->
                    let name =
                        match m.Name with
                        | "" -> td.Name + ".prototype.constructor"
                        | name when m.IsStatic -> td.Name + "." + name
                        | name -> name
                    if m.IsStatic then
                        let tpl = "{0}.apply(undefined,[{1}].concat(${2}))"
                        String.Format(tpl, name, args, arity)
                    else
                        let tpl = "$this.{0}.apply($this,[{1}].concat(${2}))"
                        String.Format(tpl, name, args, arity + 1)
                | None ->
                    let name =
                        match m.Name with
                        | "" -> "new " + td.Name
                        | name when m.IsStatic -> td.Name + "." + name
                        | name -> name
                    let tpl =
                        if m.IsStatic
                        then "{0}({1})"
                        else "$this.{0}({1})"
                    String.Format(tpl, name, args)
            | _ ->
                if m.IsStatic then m.Name + "()" else "$this." + m.Name + "()"

    member g.GetPropertyGetterInline(td: Code.TypeDeclaration, p: Code.Property) =
        if p.GetterInline.IsSome then p.GetterInline.Value else
            let pfx = if p.IsStatic then td.Name else "$this"
            let ind = if p.IndexerType.IsSome then "[$index]" else ""
            let format =
                if validJsIdentRE.IsMatch p.Name
                then 
                    if p.Name = "item" 
                    then "{0}{2}" 
                    else "{0}.{1}{2}"
                else "{0}['{1}']{2}"
            String.Format(format, pfx, p.Name, ind)

    member g.GetPropertySetterInline(td: Code.TypeDeclaration, p: Code.Property) =
        if p.SetterInline.IsSome then p.SetterInline.Value else
            let pfx = if p.IsStatic then td.Name else "$this"
            let ind = if p.IndexerType.IsSome then "[$index]" else ""
            let format =
                if validJsIdentRE.IsMatch p.Name
                then
                    if p.Name = "item"
                    then "void ({0}{2} = $value)"
                    else "void ({0}.{1}{2} = $value)"
                else "void ({0}['{1}']{2} = $value)"
            String.Format(format, pfx, p.Name, ind)

    member g.GetSourceName(entity: Code.Entity) =
        let mangle name =
            invalidCsIdentCharRE.Replace(name, "_")
        if entity.SourceName.IsSome then entity.SourceName.Value else
            let name = entity.Name
            let name =
                match name.LastIndexOf '.' with
                | -1 -> name
                | n -> name.Substring(n + 1)
            let name =
                if name.StartsWith "new " then
                    name.Substring 4
                else
                    name
            name.Substring(0, 1).ToUpper() + name.Substring 1
        |> mangle

[<Sealed>]
type TypeBuilder(aR: IAssemblyResolver, out: AssemblyDefinition, fsCoreFullName: string) =
    let mscorlib = aR.Resolve(typeof<int>.Assembly.FullName)
    let fscore = aR.Resolve(fsCoreFullName)
    let wsCore = aR.Resolve(typeof<IntelliFactory.WebSharper.Core.Attributes.InlineAttribute>.Assembly.FullName)
    let sysWeb = aR.Resolve(typeof<System.Web.UI.WebResourceAttribute>.Assembly.FullName)
    let main = out.MainModule

    let func =
        fscore.MainModule.GetType(typedefof<_->_>.FullName)
        |> main.Import

    let functions =
        wsCore.MainModule.GetType("IntelliFactory.WebSharper.Core", "Functions")

    let fromSystem (name: string) =
        mscorlib.MainModule.GetType("System", name)
        |> main.Import

    let attributeType = fromSystem "Attribute"
    let converterType = fromSystem "Converter`2"
    let objectType = fromSystem "Object"
    let stringType = fromSystem "String"
    let systemType = fromSystem "Type"
    let voidType = fromSystem "Void"

    let webResource =
        sysWeb.MainModule.GetType("System.Web.UI", "WebResourceAttribute")
        |> main.Import

    let baseResourceType =
        let coreResources =
            wsCore.MainModule.GetType("IntelliFactory.WebSharper.Core", "Resources")
        coreResources.NestedTypes
        |> Seq.find (fun t -> t.Name = "BaseResource")
        |> main.Import

    let genericInstance (def: TypeReference) (args: seq<TypeReference>) =
        if Seq.isEmpty args then
            def
        else
            let r = GenericInstanceType(def)
            for x in args do
                r.GenericArguments.Add(x)
            r :> TypeReference

    let commonType (assembly: AssemblyDefinition) ns baseName ts =
        let n = Seq.length ts
        let name =
            match n with
            | 0 -> baseName
            | k -> baseName + "`" + string k
        let tDef =
            mscorlib.MainModule.GetType(ns, name)
            |> main.Import
        genericInstance tDef ts

    let paramArray = fromSystem "ParamArrayAttribute"
    let notImpl = fromSystem "NotImplementedException"
    let obsolete = fromSystem "ObsoleteAttribute"

    let attributes =
        wsCore.MainModule.GetType("IntelliFactory.WebSharper.Core", "Attributes")

    let findWsAttr (name: string) =
        attributes.NestedTypes
        |> Seq.find (fun t -> t.Name = name)
        |> main.Import

    let inlineAttr = findWsAttr "InlineAttribute"
    let requireAttr = findWsAttr "RequireAttribute"

    member b.Action ts =
        commonType mscorlib "System" "Action" ts

    member b.Converter d r =
        genericInstance converterType [d; r]

    member b.Function d r =
        genericInstance func [d; r]

    member c.GenericInstanceType(def: TypeReference, args: seq<TypeReference>) =
        if Seq.isEmpty args then
            def
        else
            let r = GenericInstanceType(def)
            for x in args do
                r.GenericArguments.Add(x)
            r :> TypeReference

    member b.Tuple(ts: seq<TypeReference>) =
        commonType mscorlib "System" "Tuple" ts

    member b.Type(assemblyName: string, fullName: string) =
        let a = if AssemblyName(assemblyName).Name = "FSharp.Core" then fscore else aR.Resolve assemblyName
        let imp (x: TypeReference) =
            if a.Name = out.Name then x else main.Import x
        match a with
        | null -> failwithf "Could not resolve assembly: %s" assemblyName
        | a ->
            match a.MainModule.GetType(fullName.Replace('+', '/')) with
            | null -> failwithf "Could not resolve type %s in %s" fullName assemblyName
            | tR -> imp tR

    member b.Type(t: Type) =
        b.Type(t.Assembly.FullName, t.FullName)

    member b.Type<'T>() =
        b.Type typeof<'T>

    member b.WebSharperFunc ts ret =
        let n = Seq.length ts + 1
        let t =
            functions.NestedTypes
            |> Seq.find (fun t -> t.Name.StartsWith("Func") && t.GenericParameters.Count = n)
            |> main.Import
        b.GenericInstanceType(t, Seq.append ts [ret])

    member b.Attribute = attributeType
    member b.BaseResource = baseResourceType
    member b.Inline = inlineAttr
    member b.NotImplemented = notImpl
    member b.Object = objectType
    member b.ParamArray = paramArray
    member b.Require = requireAttr
    member b.Obsolete = obsolete
    member b.String = stringType
    member b.SystemType = systemType
    member b.Void = voidType
    member b.WebResource = webResource

[<Sealed>]
type TypeConverter private (tB: TypeBuilder, types: Types, genericsByPosition: GenericParameter [], genericsByName: Map<string,GenericParameter>) =

    let byId id =
        match types.TryGetValue id with
        | true, x -> x :> TypeReference
        | _ -> tB.Object

    new (tB, types) =
        TypeConverter(tB, types, Array.empty, Map.empty)

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

    member c.TypeReference(t: T) =
        match t with
        | Type.ArrayType (rank, t) ->
            ArrayType(c.TypeReference t, rank) :> TypeReference
        | Type.DeclaredType id ->
            byId id
        | Type.FunctionType f ->
            match f.ParamArray, f.This with
            | None, None ->
                let domain =
                    match f.Parameters with
                    | [] -> tB.Type<unit>()
                    | [(_, t)] -> c.TypeReference t
                    | ts -> tB.Tuple [for (_, t) in ts -> c.TypeReference t]
                tB.Function domain (c.TypeReference f.ReturnType)
            | None, Some this when f.Parameters.IsEmpty ->
                match f.ReturnType with
                | Type.Unit ->
                    tB.Action [c.TypeReference this]
                | Type.NonUnit ->
                    tB.Converter (c.TypeReference this) (c.TypeReference f.ReturnType)
            | None, Some this ->
                let types =
                    match f.ReturnType with
                    | Type.Unit ->
                        Type.Unit :: this :: List.map snd f.Parameters
                    | Type.NonUnit ->
                        f.ReturnType :: this :: List.map snd f.Parameters
                    |> List.map c.TypeReference
                tB.WebSharperFunc types.Tail types.Head
            | _ ->
                // ParamArray not supported yet:
                tB.Object
        | Type.GenericType (name, _) ->
            genericsByName.[name] :> _
        | Type.SpecializedType (x, xs) ->
            let args = xs |> Seq.map c.TypeReference
            tB.GenericInstanceType(c.TypeReference x, args)
        | Type.SystemType t ->
            c.TypeReference t
        | Type.TupleType xs ->
            tB.Tuple [ for t in List.rev xs -> c.TypeReference t ]
        | Type.UnionType _ -> tB.Object

    member c.WithGenerics(gs: seq<GenericParameter>) =
        let gs = Seq.toArray gs
        let gbn =
            (genericsByName, gs)
            ||> Array.fold (fun m g -> m.Add(g.Name, g))
        TypeConverter(tB, types, Array.append genericsByPosition gs, gbn)

[<Sealed>]
type MemberBuilder(tB: TypeBuilder, def: AssemblyDefinition) =

    let findConstructor (t: TypeReference) (isMatch: MethodDefinition -> bool) =
        t.Resolve().Methods
        |> Seq.tryFind (fun m -> m.IsConstructor && isMatch m)
        |> function
            | Some x -> def.MainModule.Import x
            | None -> failwithf "Could not find a constructor in %O" t.FullName

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
    let inlineAttributeConstructor = findTypedConstructor tB.Inline [tB.String.Name]
    let requireAttributeConstructor = findTypedConstructor tB.Require [tB.SystemType.Name]
    let obsoleteAttributeConstructor = findDefaultConstructor tB.Obsolete 

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
    member c.InlineAttributeConstructor = inlineAttributeConstructor
    member c.RequireAttributeConstructor = requireAttributeConstructor
    member c.ObsoleteAttributeConstructor = obsoleteAttributeConstructor

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
        AssemblyResolver : option<AssemblyResolver>
        AssemblyVersion : Version
        DocPath : option<string>
        EmbeddedResources : seq<string>
        Kind : CompilationKind
        OutputPath : option<string>
        ProjectDir : string
        ReferencePaths : seq<string>
        StrongNameKeyPair : option<StrongNameKeyPair>
    }

    static member Default(name) =
        {
            AssemblyName = name
            AssemblyResolver = None
            AssemblyVersion = Version(0, 0)
            DocPath = None
            EmbeddedResources = Seq.empty
            Kind = LibraryKind
            OutputPath = None
            ProjectDir = "."
            ReferencePaths = Seq.empty
            StrongNameKeyPair = None
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
                let snk = StrongNameKeyPair(File.ReadAllBytes path)
                { opts with StrongNameKeyPair = Some snk }
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
            compilerOptions: CompilerOptions
        ) =

    let inlineAttribute (code: string) =
        let attr = CustomAttribute(mB.InlineAttributeConstructor)
        attr.ConstructorArguments.Add(CustomAttributeArgument(tB.String, code))
        attr

    let requireAttribute (resourceType: TypeReference) =
        let attr = CustomAttribute(mB.RequireAttributeConstructor)
        attr.ConstructorArguments.Add(CustomAttributeArgument(tB.SystemType, resourceType))
        attr

    let obsoleteAttribute = CustomAttribute(mB.ObsoleteAttributeConstructor)

    let withGenerics gs =
        MemberConverter(tB, mB, tC.WithGenerics gs, types, iG, def, comments, compilerOptions)

    let makeParameters (f: Type.Function) =
        Seq.ofArray [|
            for (n, t) in f.Parameters do
                yield ParameterDefinition(n, ParameterAttributes.None, tC.TypeReference t)
            match f.ParamArray with
            | None -> ()
            | Some pa ->
                yield mB.BuildParamArrayParameter(tC.TypeReference pa)
        |]

    let methodAttributes (dt: TypeDefinition) (x: Code.Entity) =
        let accessAttrs =
            match x.AccessModifier with
            | Code.AccessModifier.Private ->
                MethodAttributes.Private
            | Code.AccessModifier.Protected ->
                MethodAttributes.Family
            | Code.AccessModifier.Internal ->
                MethodAttributes.Assembly
            | Code.AccessModifier.Public ->
                MethodAttributes.Public
            | _ ->
                MethodAttributes.Public
        match x with
        | :? Code.Constructor -> accessAttrs
        | :? Code.Member as m when m.IsStatic -> MethodAttributes.Static ||| accessAttrs
        | _ when dt.IsInterface -> accessAttrs ||| MethodAttributes.Abstract ||| MethodAttributes.Virtual
        | _ -> accessAttrs

    let addConstructor (dT: TypeDefinition) (td: Code.TypeDeclaration) (x: Code.Constructor) =
        let overloads =
            x.Type
            |> Type.Normalize
            |> Type.DistinctOverloads
        for t in overloads do
            match t with
            | Type.FunctionType f ->
                let attrs = methodAttributes dT x
                let cD = mB.BuildConstructor(methodAttributes dT x)
                iG.GetMethodBaseInline(td, t, x)
                |> inlineAttribute
                |> cD.CustomAttributes.Add
                for p in makeParameters f do
                    cD.Parameters.Add p
                if x.IsObsolete then cD.CustomAttributes.Add obsoleteAttribute
                dT.Methods.Add(cD)
                do
                    match x.Comment with
                    | None -> ()
                    | Some c -> comments.[cD] <- c
            | _ -> ()

    let addProperty (dT: TypeDefinition) (td: Code.TypeDeclaration) (p: Code.Property) =
        let ty =
            match Type.Normalize p.Type with
            | [t] -> tC.TypeReference t
            | _  -> tB.Object
        let name = iG.GetSourceName p
        let attrs = PropertyAttributes.None
        let pD = PropertyDefinition(name, attrs, ty)
        do
            match p.Comment with
            | None -> ()
            | Some c -> comments.[pD] <- c
        if p.HasGetter then
            let mD = MethodDefinition("get_" + name, methodAttributes dT p, ty)
            if not dT.IsInterface then
                mB.AddBody mD
                iG.GetPropertyGetterInline(td, p)
                |> inlineAttribute
                |> mD.CustomAttributes.Add
            match p.IndexerType with
            | None -> ()
            | Some it ->
                mD.Parameters.Add(ParameterDefinition("index", ParameterAttributes.None, tC.TypeReference it))         
            dT.Methods.Add mD
            pD.GetMethod <- mD
        if p.HasSetter then
            let mD = MethodDefinition("set_" + name, methodAttributes dT p, tB.Void)
            if not dT.IsInterface then
                mB.AddBody mD
                iG.GetPropertySetterInline(td, p)
                |> inlineAttribute
                |> mD.CustomAttributes.Add
            match p.IndexerType with
            | None -> ()
            | Some it ->
                mD.Parameters.Add(ParameterDefinition("index", ParameterAttributes.None, tC.TypeReference it))         
            mD.Parameters.Add(ParameterDefinition("value", ParameterAttributes.None, ty))
            if not dT.IsInterface then
                mB.AddBody mD
            dT.Methods.Add mD
            pD.SetMethod <- mD
        if p.IsObsolete then pD.CustomAttributes.Add obsoleteAttribute
        dT.Properties.Add pD

    let genericType (x: Code.TypeDeclaration) k =
        match types.TryGetValue(x.Id) with
        | true, tD ->
            let gs =
                [
                    for g in x.Generics ->
                        let gP = GenericParameter(g.Name, tD)
                        for c in g.Constraints do
                            gP.Constraints.Add(tC.TypeReference c)
                        tD.GenericParameters.Add(gP)
                        gP
                ]
            if x.Generics.Length > 0 then
                tD.Name <- tD.Name + "`" + string x.Generics.Length
            k (withGenerics gs) tD
        | _ -> ()

    member private c.AddMethod(dT: TypeDefinition, td: Code.TypeDeclaration, x: Code.Method) =
        let overloads =
            x.Type
            |> Type.Normalize
            |> Type.DistinctOverloads
        for t in overloads do
            match t with
            | Type.FunctionType f ->
                let name = iG.GetSourceName x
                let attrs = methodAttributes dT x
                let mD = MethodDefinition(name, attrs, tB.Object)
                do
                    match x.Comment with
                    | None -> ()
                    | Some c -> comments.[mD] <- c
                let gs = 
                    [| 
                        for g in x.Generics -> 
                            let gP = GenericParameter(g.Name, mD) 
                            for c in g.Constraints do
                                gP.Constraints.Add(tC.TypeReference c)
                            mD.GenericParameters.Add(gP)
                            gP
                    |]
                let c = withGenerics gs
                c.AddMethod(dT, td, x, mD, f)
            | _ -> ()

    member private c.AddMethod(dT: TypeDefinition, td: Code.TypeDeclaration, x: Code.Method, mD: MethodDefinition, f: Type.Function) =
        mD.ReturnType <-
            match f.ReturnType with
            | Type.Unit -> tB.Void
            | Type.NonUnit -> tC.TypeReference f.ReturnType
        for p in makeParameters f do
            mD.Parameters.Add p
        if not dT.IsInterface then
            mB.AddBody mD
            iG.GetMethodBaseInline(td, Type.FunctionType f, x)
            |> inlineAttribute
            |> mD.CustomAttributes.Add
        if x.IsObsolete then mD.CustomAttributes.Add obsoleteAttribute
        dT.Methods.Add mD

    member private c.AddTypeMembers<'T when 'T :> Code.TypeDeclaration and 'T :> Code.IResourceDependable<'T>>
            (x: 'T, tD: TypeDefinition) =
        for m in x.Methods do
            c.AddMethod(tD, x, m)
        for p in x.Properties do
            addProperty tD x p
        c.AddDependencies(x, tD)

    member d.AddDependencies(ent: Code.IResourceDependable, prov: ICustomAttributeProvider) =
        for d in ent.GetRequires() do
            match d with
            | Code.LocalDependency d ->
                match types.TryGetValue(d) with
                | true, t -> prov.CustomAttributes.Add(requireAttribute t)
                | _ -> ()
            | Code.ExternalDependency ty ->
                let t = tC.TypeReference ty
                prov.CustomAttributes.Add(requireAttribute t)

    member c.Class(x: Code.Class) =
        genericType x (fun c tD -> c.Class(x, tD))

    member private c.Class(x: Code.Class, tD: TypeDefinition) =
        do
            match x.BaseClass with
            | None -> tD.BaseType <- tB.Object
            | Some t -> tD.BaseType <- tC.TypeReference t
        do
            match x.Comment with
            | None -> ()
            | Some c -> comments.[tD] <- c
        for i in x.ImplementedInterfaces do
            tD.Interfaces.Add(tC.TypeReference i)
        for ctor in x.Constructors do
            addConstructor tD x ctor
        if x.IsObsolete then tD.CustomAttributes.Add obsoleteAttribute
        c.AddTypeMembers(x, tD)

    member c.Interface(x: Code.Interface) =
        genericType x (fun c tD -> c.Interface(x, tD))

    member private c.Interface(x: Code.Interface, tD: TypeDefinition) =
        for i in x.BaseInterfaces do
            tD.Interfaces.Add(tC.TypeReference i)
        if x.IsObsolete then tD.CustomAttributes.Add obsoleteAttribute
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
            c.AddDependencies(r, tD)
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
        if t.IsArray then
            let t = t :?> ArrayType
            match t.Rank with
            | 0 | 1 -> String.Format("{0}[]", typeRefId t.ElementType)
            | k -> String.Format("{0}, [{1}]", typeRefId t.ElementType, String.replicate (k - 1) ",")
        else
            // TODO: generic types.
            t.FullName

    let propertyId (p: PropertyDefinition) =
        String.Format
            (
                "P:{0}.{1}",
                typeRefId p.DeclaringType,
                p.Name
            )

    let methodId (m: MethodDefinition) =
        String.Format
            (
                "M:{0}.{1}({2})", 
                typeRefId m.DeclaringType,
                m.Name,
                seq {
                    for p in m.Parameters ->
                        typeRefId p.ParameterType
                }
                |> String.concat ","
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
            yield! visitMember (String.Format("T:{0}", typeRefId t)) (getComment t)
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
type CompiledAssembly(def: AssemblyDefinition, doc: XmlDocGenerator, options: CompilerOptions) =

    let writerParams =
        match options.StrongNameKeyPair with
        | None -> WriterParameters()
        | Some p -> WriterParameters(StrongNameKeyPair = p)

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
            |]
        let systemAssembly =
            def.MainModule.AssemblyResolver.Resolve(typeof<string>.Assembly.FullName)
        let getSystemTypeDef (t: Type) =
            systemAssembly.MainModule.GetType(t.FullName)
            |> def.MainModule.Import
        let stringTypeDef =
            getSystemTypeDef typeof<string>
        let findStringCtor (ty: TypeReference) =
            ty.Resolve().Methods
            |> Seq.find (fun m ->
                m.IsConstructor
                && m.HasParameters
                && m.Parameters.Count = 1
                && m.Parameters.[0].ParameterType.FullName = stringTypeDef.FullName)
            |> def.MainModule.Import
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
type Resolver(aR: AssemblyResolver) =
    let def = DefaultAssemblyResolver()

    let resolve (ref: string) (par: option<ReaderParameters>) =
        let n = AssemblyName(ref)
        match aR.ResolvePath n with
        | Some x ->
            try
                if x = null || not (FileInfo(x).Exists) then
                    failwithf "Invalid file resolution: %s" (string x)
            with :? ArgumentException ->
                failwithf "Invalid file resolution: [%s]" (string x)
            match par with
            | None -> AssemblyDefinition.ReadAssembly(x)
            | Some par -> AssemblyDefinition.ReadAssembly(x, par)
        | None -> def.Resolve(ref)

    interface IAssemblyResolver with

        member x.Resolve(name) =
            resolve name None

        member x.Resolve(name: string, par) =
            resolve name (Some par)

        member x.Resolve(ref: AssemblyNameReference, par: ReaderParameters) =
            let ref = ref.FullName
            resolve ref (Some par)

        member x.Resolve(ref: AssemblyNameReference) =
            let ref = ref.FullName
            resolve ref None

[<Sealed>]
type Compiler() =

    let iG = InlineGenerator()

    let createAssemblyResolvers (opts: CompilerOptions) =
        let aR =
            match opts.AssemblyResolver with
            | Some aR -> aR
            | None -> AssemblyResolver.Create()
        let aR = aR.SearchPaths(opts.ReferencePaths)
        (aR, Resolver(aR) :> IAssemblyResolver)

    let getAccessAttributes (nested: bool) (t: Code.NamespaceEntity) =
        match t.AccessModifier, nested with
        | Code.AccessModifier.Private, true ->
            TypeAttributes.NestedPrivate
        | Code.AccessModifier.Protected, true ->
            TypeAttributes.NestedFamily
        | Code.AccessModifier.Internal, true ->
            TypeAttributes.NestedFamORAssem
        | Code.AccessModifier.Public, true ->
            TypeAttributes.NestedPublic
        | Code.AccessModifier.Public, false ->
            TypeAttributes.Public
        | _ , _ ->
            TypeAttributes.NotPublic

    let getId (d: Code.NamespaceEntity) =
        d.Id

    let visit visitClass visitInterface visitResource visitNestedClass visitNestedInterface (assembly: Code.Assembly) =
        let rec onClass (ctx: Choice<string,Code.Class>) (c: Code.Class) =
            match ctx with
            | Choice1Of2 ns -> visitClass ns c
            | Choice2Of2 parent -> visitNestedClass parent c
            for nC in c.NestedClasses do
                onClass (Choice2Of2 c) nC
            for nI in c.NestedInterfaces do
                visitNestedInterface c nI
        for ns in assembly.Namespaces do
            for c in ns.Classes do
                onClass (Choice1Of2 ns.Name) c
            for i in ns.Interfaces do
                visitInterface ns.Name i
            for r in ns.Resources do
                visitResource ns.Name r

    let buildInitialTypes assembly (def: AssemblyDefinition) =
        let types : Types = Dictionary()
        let build attrs ns (x: Code.NamespaceEntity) =
            let nested = false
            let attrs = attrs ||| getAccessAttributes nested x
            let tD = TypeDefinition(ns, iG.GetSourceName x, attrs)
            types.[getId x] <- tD
            def.MainModule.Types.Add(tD)
        let buildNested attrs (parent: Code.NamespaceEntity) (x: Code.NamespaceEntity) =
            match types.TryGetValue parent.Id with
            | true, parent ->
                let nested = true
                let attrs = attrs ||| getAccessAttributes nested x
                let tD = TypeDefinition(null, iG.GetSourceName x, attrs)
                tD.DeclaringType <- parent
                types.[getId x] <- tD
                parent.NestedTypes.Add tD
            | _ -> ()
        let interf =
            TypeAttributes.Interface
            ||| TypeAttributes.Abstract
        assembly
        |> visit
            (build TypeAttributes.Class)
            (build interf)
            (build TypeAttributes.Class)
            (buildNested TypeAttributes.Class)
            (buildNested interf)
        types

    let findFSharpCoreFullName options =
        let fsCorePath =
            options.ReferencePaths
            |> Seq.tryFind (fun p -> p.ToLower().EndsWith("fsharp.core.dll"))
        match fsCorePath with
        | None -> typedefof<list<_>>.Assembly.FullName
        | Some p ->
            AssemblyName.GetAssemblyName(p).FullName

    let buildAssembly resolver options (assembly: Code.Assembly) =
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
        mp.Runtime <- TargetRuntime.Net_4_0 // TODO: make a parameter
        let comments : Comments = Dictionary()
        let def = AssemblyDefinition.CreateAssembly(aND, options.AssemblyName, mp)
        let types = buildInitialTypes assembly def
        let tB = TypeBuilder(resolver, def, findFSharpCoreFullName options)
        let tC = TypeConverter(tB, types)
        let mB = MemberBuilder(tB, def)
        let mC = MemberConverter(tB, mB, tC, types, iG, def, comments, options)
        assembly
        |> visit
            (fun _ c -> mC.Class c)
            (fun _ i -> mC.Interface i)
            (fun _ r -> mC.Resource r)
            (fun _ c -> mC.Class c)
            (fun _ i -> mC.Interface i)
        mC.AddDependencies(assembly, def)
        (def, comments, mB)

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

    member c.Compile(resolver, options, assembly, ?originalAssembly: Assembly) =
        let (def, comments, mB) = buildAssembly resolver options assembly
        for f in options.EmbeddedResources do
            EmbeddedResource(Path.GetFileName(f), ManifestResourceAttributes.Public, File.ReadAllBytes(f))
            |> def.MainModule.Resources.Add
        addResourceExports mB def
        let doc = XmlDocGenerator(def, comments)
        let r = CompiledAssembly(def, doc, options)
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

    member c.Compile(options, assembly) =
        let (aR, resolver) = createAssemblyResolvers options
        c.Compile(resolver, options, assembly)

    member c.StartProgram(args, assembly, ?resolver: AssemblyResolver, ?originalAssembly: Assembly) =
        let opts =
            let opts = CompilerOptions.Parse args
            match resolver with
            | None -> opts
            | Some r -> { opts with AssemblyResolver = Some r }
        let (aR, resolver) = createAssemblyResolvers opts
        aR.Wrap <| fun () ->
            c.Compile(resolver, opts, assembly, ?originalAssembly = originalAssembly)
            |> ignore
        0

    member c.Start(args, assembly, ?resolver) =
        c.StartProgram(args, assembly, ?resolver = resolver)

    member c.Start(args, assembly, original, ?resolver) =
        c.StartProgram(args, assembly, ?resolver = resolver, originalAssembly = original)

    static member Create() =
        Compiler()
