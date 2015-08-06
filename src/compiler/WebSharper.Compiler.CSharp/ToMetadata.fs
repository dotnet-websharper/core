module RoslynTest.ToMetadata

open System.Collections.Generic

open CommonAST
open CommonAST.AST
open CommonAST.Metadata

open Microsoft.CodeAnalysis
open Microsoft.CodeAnalysis.CSharp
open Microsoft.CodeAnalysis.CSharp.Syntax

type Attribute =
    | Proxy of TypeDefinition
    | Inline of option<string>
    | Name of string
    | JavaScript

let getAttributes (symbol: ISymbol) =
    symbol.GetAttributes() |> Seq.choose (fun a ->
        let ty = a.AttributeClass
        //TODO: check ty.ContainingAssembly.Name
        let tyArg() =
            match a.ConstructorArguments.[0].Value with
            | :? INamedTypeSymbol as t -> ToCSharpAST.getNamedTypeDefinition t //ToFSharpAST.getTypeDefinition t.TypeDefinition
            | :? string as s ->
                let ss = s.Split([|','|])
                Hashed {
                    FullName = ss.[0].Trim()
                    Assembly = ss.[1].Trim()
                }
        match ty.Name with
        | "ProxyAttribute" ->
            Some (Proxy (tyArg()))
        | "InlineAttribute" ->
            let args = a.ConstructorArguments
            if args.Length = 0 then None
            else Some (a.ConstructorArguments.[0].Value :?> string)                
            |> Inline |> Some
        | "JavaScriptAttribute" ->
            Some JavaScript
        | "NameAttribute" ->
            a.ConstructorArguments.[0].Value :?> string
            |> Name |> Some
        | _ -> None        
    )
    |> List.ofSeq
    
let rec getAllTypeMembers (n: INamespaceSymbol) =
    let rec getWithNested (t: INamedTypeSymbol) =
        Seq.append
            (Seq.singleton t)
            (t.GetTypeMembers() |> Seq.collect getWithNested)     
    if n.Name = "System" then Seq.empty else
    Seq.append 
        (n.GetTypeMembers() |> Seq.collect getWithNested)
        (n.GetNamespaceMembers() |> Seq.collect getAllTypeMembers)
  
let emptySet = System.Collections.Immutable.ImmutableHashSet.Create()  
let emptyDict<'a, 'b> = System.Collections.Immutable.ImmutableDictionary.Create<'a, 'b>()
         
let transformAssembly (assembly : IAssemblySymbol) =
    let allTypes = assembly.GlobalNamespace |> getAllTypeMembers |> Array.ofSeq

    let translated = Dictionary() :> IDictionary<_,_>         

    let root = Dictionary()

    let transformInterface (intf: INamedTypeSymbol) =
        if intf.TypeKind <> TypeKind.Interface then None else
        let methodNames = Dictionary()
        let def =
            getAttributes intf |> List.tryPick (function Proxy p -> Some p | _ -> None)
            |> Option.fallback (fun () -> ToCSharpAST.getNamedTypeDefinition intf)
        let intfName = intf.Name.Replace('.', '$') + "$"
        for m in intf.GetMembers() do
            let n = 
                m |> getAttributes 
                |> Seq.tryPick (function Name n -> Some n | _ -> None) 
                |> Option.fallback (fun () -> intfName + m.Name)
            let md = 
                match ToCSharpAST.getMember (m :?> IMethodSymbol) with
                | CommonAST.AST.Method md -> md
                | _ -> failwith "invalid interface member"
            methodNames.Add(md, n)
        Some (def, 
            {
                Extends = intf.Interfaces |> Seq.map (fun i -> ToCSharpAST.getNamedTypeDefinition i) |> List.ofSeq
                MethodNames = methodNames 
            }
        )

    let transformClass (class_: INamedTypeSymbol) =
        if class_.TypeKind <> TypeKind.Class then None else
        let attrs = getAttributes class_
    
        let rec getNamespaceOrTypeAddress acc (symbol: INamespaceOrTypeSymbol) =
            match symbol.ContainingNamespace with
            | null -> acc
            | ns -> getNamespaceOrTypeAddress (symbol.Name :: acc) ns   
    
        let rec getTypeAddress acc (symbol: INamedTypeSymbol) =
            match symbol.ContainingType with
            | null -> getNamespaceOrTypeAddress acc symbol
            | t -> getTypeAddress (symbol.Name :: acc) t           

        let addressAndMembersDict =
            lazy
                match attrs |> List.tryPick (function Name n -> Some n | _ -> None) with
                | Some n -> n.Split('.') |> List.ofArray
                | _ -> getTypeAddress [] class_    
                |> Resolve.classAddress root

        let def =
            attrs |> List.tryPick (function Proxy p -> Some p | _ -> None)
            |> Option.fallback (fun () -> ToCSharpAST.getNamedTypeDefinition class_)

        let prototype =
            lazy
                let a, memD = addressAndMembersDict.Value
                let p = Dictionary() :> IDictionary<_,_>
                let name = Resolve.addChild "$proto" None memD
                let reqs = HashSet()
                let deps = HashSet() 
                let paddr = Hashed(name :: a.Value)
                translated.Add(Hashed(name :: a.Value), Prototype (def, p, deps, reqs)) 
                name, (p, deps, reqs, paddr)

        let isJS = attrs |> List.contains JavaScript

        let methods = Dictionary()
        let constructors = Dictionary()
        let mutable cctor = None

        let transformMethod meth =
            let attrs = getAttributes meth
            let isJS =
                isJS || attrs |> List.exists (function JavaScript | Inline _ -> true | _ -> false)
            if isJS then
            let def = ToCSharpAST.getMember meth
            do
                let reqs = HashSet()
                let inl = attrs |> List.tryPick (function Inline i -> i | _ -> None)
                let staticAddress methodName =
                    let a, memD = addressAndMembersDict.Value
                    let name = Resolve.addChild methodName None memD   
                    Hashed(name :: a.Value)
                match def with
                | CommonAST.AST.Method mdef ->
                    match inl with
                    | Some i ->
                        methods.Add (mdef, InlineMethod (ref (NotParsed (i, meth, reqs)), emptySet, reqs))
                    | _ ->
                    if meth.IsStatic then
                        let deps = HashSet()
                        let address = staticAddress mdef.Value.MethodName
                        let content = ref (NotCompiled (meth, deps, reqs))
                        translated.Add(address, Content (content, deps, reqs))
                        methods.Add(mdef, CompiledStaticMethod (address, content))
                    else
                        let _, (p, deps, reqs, _) = prototype.Value
                        let content = ref (NotCompiled (meth, deps, reqs))
                        let name = Resolve.addChild mdef.Value.MethodName content p
                        methods.Add(mdef, CompiledInstanceMethod (name, content))
                | CommonAST.AST.Constructor cdef ->
                        let _, (_, _, _, paddr) = prototype.Value 
                        let deps = HashSet()
                        let address = staticAddress "ctor"
                        let content = ref (NotCompiledConstructor (meth, paddr, deps, reqs))
                        translated.Add(address, Content (content, deps, reqs))
                        constructors.Add(cdef, CompiledConstructor (address, content))
                | CommonAST.AST.StaticConstructor ->
                        let deps = HashSet()
                        let address = staticAddress "cctor"
                        let content = ref (NotCompiled (meth, deps, reqs))
                        cctor <- Some (CompiledStaticConstructor (address, content))

        let transformField (field_: IFieldSymbol) =
            let isStatic = field_.IsStatic 
            let f = ToCSharpAST.getField field_
            if isStatic then
                let a, memD = addressAndMembersDict.Value    
                let name = Resolve.addChild f.Value.FieldName None memD
                let address = Hashed(name :: a.Value)
                translated.Add(address, Content (ref (CompiledExpr Undefined), null, null))
                f, StaticField (address, false) // TODO : optional fields 
            else
                let _, (p, _, _, _) = prototype.Value
                let name = Resolve.addChild f.Value.FieldName (ref (CompiledExpr Undefined)) p
                f, InstanceField (name, false) // TODO : optional fields 

        let members = class_.GetMembers()
        members.OfType<IMethodSymbol>() |> Seq.iter transformMethod

        let fields = members.OfType<IFieldSymbol>() |> Seq.map transformField |> dict

        Some (
            def,
            {
//                CurrentAssembly = true
                Address = if addressAndMembersDict.IsValueCreated then Some (fst addressAndMembersDict.Value) else None
                BaseClass = class_.BaseType |> Option.ofObj |> Option.map ToCSharpAST.getNamedTypeDefinition
                FieldNames = Set.empty // TODO
                Fields = fields
                Prototype = if prototype.IsValueCreated then Some (fst prototype.Value) else None 
                StaticConstructor = cctor         
                Methods = methods 
                Constructors = constructors
                Interfaces = class_.Interfaces |> Seq.map (fun i -> ToCSharpAST.getNamedTypeDefinition i) |> List.ofSeq
            }
        )

    {
        Interfaces = allTypes |> Seq.choose transformInterface |> dict
        Classes = allTypes |> Seq.choose transformClass |> dict
        Proxies = emptyDict // TODO
        RemoteMethods = emptyDict // TODO
        Translated = translated
    }      

let stringInlines (compilation : Compilation) =
    let allTypes = compilation.GlobalNamespace |> getAllTypeMembers |> Array.ofSeq
    
    let transformClass (class_: INamedTypeSymbol) =
        let attrs = getAttributes class_

        let methods = Dictionary()
        let constructors = Dictionary()

        let transformMethod meth =
 
            let attrs = getAttributes meth
            let isInline = attrs |> List.exists (function Inline _ -> true | _ -> false)
            if isInline then
                let def = ToCSharpAST.getMember meth
                let reqs = HashSet()
                match attrs |> List.tryPick (function Inline i -> i | _ -> None) with
                | Some i ->
                    match def with
                    | CommonAST.AST.Method mdef ->
                        methods.Add(mdef, InlineMethod (ref (NotParsed (i, meth, reqs)), emptySet, reqs))
                    | CommonAST.AST.Constructor cdef ->
                        constructors.Add(cdef, InlineConstructor (ref (NotParsed (i, meth, reqs)), emptySet, reqs))
                | None -> failwith ""

        let members = class_.GetMembers()
        members.OfType<IMethodSymbol>() |> Seq.iter transformMethod

        if methods.Count = 0 && constructors.Count = 0 then None else

        let def =
            attrs |> List.tryPick (function Proxy p -> Some p | _ -> None)
            |> Option.fallback (fun () -> ToCSharpAST.getNamedTypeDefinition class_)

        Some (
            def,
            {
//                CurrentAssembly = false
                Address = None
                BaseClass = None
                FieldNames = Set.empty // TODO
                Fields = emptyDict
                Prototype = None
                Constructors = constructors
                StaticConstructor = None     
                Methods = methods 
                Interfaces = []
            }
        )

    {
        Interfaces = dict []
        Classes = allTypes |> Seq.choose transformClass |> dict
        Proxies = emptyDict 
        RemoteMethods = emptyDict // TODO
        Translated = emptyDict
    }      
