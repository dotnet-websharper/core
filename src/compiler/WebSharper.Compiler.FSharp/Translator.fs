module WebSharper.Compiler.FSharp.Translator

open Microsoft.FSharp.Compiler.SourceCodeServices
open System.Collections.Generic

open WebSharper.Core.AST
open WebSharper.Core.Metadata

type FSIFD = FSharpImplementationFileDeclaration
type FSMFV = FSharpMemberOrFunctionOrValue

module A = WebSharper.Compiler.Common.AttributeReader

type FSharpAttributeReader() =
    inherit A.AttributeReader<FSharpAttribute>()
    override this.GetAssemblyName attr = attr.AttributeType.Assembly.SimpleName
    override this.GetName attr = attr.AttributeType.LogicalName
    override this.GetCtorArgs attr = attr.ConstructorArguments |> Seq.map snd |> Array.ofSeq          
    override this.GetTypeDef o = ToFSharpAST.getTypeDefinition (o :?> FSharpType).TypeDefinition

let attrReader = FSharpAttributeReader()

let transformInterface (intf: FSharpEntity) =
    if not intf.IsInterface then None else
    let methodNames = Dictionary()
    let annot =
        attrReader.GetTypeAnnot(None, intf.Attributes) // TODO inherited attributes
    let def =
        match annot.Kind with
        | Some (A.TypeKind.Proxy d) -> d 
        | _ -> ToFSharpAST.getTypeDefinition intf
    let intfName = intf.FullName + "." //.Replace('.', '$') + "$"
    for m in intf.MembersFunctionsAndValues do
        if not m.IsProperty then
            let annot = attrReader.GetMemberAnnot(annot.Kind, m.Attributes)
            let n = 
                match annot.Name with
                | Some n -> n
                | _ -> intfName + m.CompiledName 
            let md = 
                match ToFSharpAST.getMember m with
                | WebSharper.Core.AST.Method md -> md
                | _ -> failwith "invalid interface member"
            methodNames.Add(md, n)
    Some (def, 
        {
            Extends  = intf.DeclaredInterfaces |> Seq.map (fun i -> ToFSharpAST.getTypeDefinition i.TypeDefinition) |> List.ofSeq
            MethodNames = methodNames 
        }
    )

let emptySet<'a> = System.Collections.Immutable.ImmutableHashSet.Create<'a>()  
let emptyDict<'a, 'b> = System.Collections.Immutable.ImmutableDictionary.Create<'a, 'b>()

let transformClass root (translated: IDictionary<_,_>) (proxies: IDictionary<_,_>) (cls: FSharpEntity) members =
    if cls.CompiledName = "OperatorsProxy" then 
        ()
    if not (cls.IsClass || cls.IsFSharpExceptionDeclaration || cls.IsFSharpModule || cls.IsFSharpRecord || cls.IsFSharpUnion) then None else

    let annot = attrReader.GetTypeAnnot(None, cls.Attributes)
//    let attrs = cls.Attributes |> getAttributes

    let addressAndMembersDict =
        lazy
            match annot.Name with
            | Some n -> n.Split('.') |> List.ofArray
            | _ -> cls.FullName.Split('.') |> List.ofArray
            |> Resolve.classAddress root

    let def =
        let thisDef = ToFSharpAST.getTypeDefinition cls
        
        match annot.Kind with
        | Some (A.TypeKind.Proxy p) -> 
            proxies.Add(thisDef, p)
            p
        | _ -> thisDef

    let prototype =
        lazy
            let a, memD = addressAndMembersDict.Value
            let p = Dictionary() :> IDictionary<_,_>
            let name = Resolve.addChild "$proto" None memD
            let reqs = HashSet()
            let deps = HashSet() 
            let paddr = Hashed(name :: a.Value)
            let node =
                {
                    Info = Prototype def
                    Body = Undefined
                    Dependencies = deps
                    Requires = reqs
                }
            translated.Add(Hashed(name :: a.Value), node) 
            name, (p, deps, reqs, paddr)

//    let isJS = annot.Kind |> List.contains JavaScript // TODO : nested in JS annotated

    let methods = Dictionary()
    let constructors = Dictionary()

    let mutable cctor = None

    for meth in cls.MembersFunctionsAndValues do
        if meth.IsProperty then () else
        let mAnnot = attrReader.GetMemberAnnot(annot.Kind, meth.Attributes)
        let node info =
            {
                Info = info
                Body = Undefined
                Dependencies = HashSet()
                Requires = HashSet()
            } 
        if mAnnot.Kind = Some A.MemberKind.Stub then
            match ToFSharpAST.getMember meth with
            | WebSharper.Core.AST.Method mdef ->
                let name = 
                    match mAnnot.Name with
                    | Some n -> n
                    | _ -> mdef.Value.MethodName
                if meth.IsInstanceMember then 
                    methods.Add(mdef, node (Instance name))
                else
                    let a, _ = addressAndMembersDict.Value
                    let addr = Hashed (name :: a.Value)
                    methods.Add(mdef, node (Static addr))
            | WebSharper.Core.AST.Constructor cdef -> // Todo stub constructors
                let a, _ = addressAndMembersDict.Value
                constructors.Add(cdef, node (Constructor a))
            | _ -> failwith "Static method can't have Stub attribute"

    //let transformMember ((meth: FSMFV, _, _) as info) =
    for (meth: FSMFV, args: list<list<FSMFV>>, expr: FSharpExpr) in members do
        if meth.IsProperty then () else
        let mAnnot = attrReader.GetMemberAnnot(annot.Kind, meth.Attributes)
        
        
//    | Inline of string
//    | Direct of string
//    | InlineJavaScript
//    | JavaScript
//    | Constant of Expression
//    | Macro of TypeDefinition * option<MemberKind>
//    | Generated of TypeDefinition
//    | Remote
//    | Require
//    | Stub
//    | OptionalField

        //let macro = attrs |> List.tryPick (function Macro m -> Some m | _ -> None) 
        match mAnnot.Kind with
        | Some kind when kind <> A.MemberKind.Remote ->
            let def = ToFSharpAST.getMember meth
            let staticAddress methodName =
                let a, memD = addressAndMembersDict.Value
                let name = Resolve.addChild methodName None memD   
                Hashed(name :: a.Value)
            let node info body = 
                {
                    Info = info
                    Body = body
                    Dependencies = HashSet()
                    Requires = HashSet()
                }
            let getBody() = 
                let args = args |> Seq.concat |> List.ofSeq 
                let vars = args |> Seq.map (fun p -> Id p.DisplayName) |> List.ofSeq
                let env = ToFSharpAST.Environment.New (List.zip args vars, meth.GenericParameters |> Seq.map (fun p -> p.Name) |> List.ofSeq)  
                Function(vars, Return (ToFSharpAST.transformExpression env expr))
            match def with
            | WebSharper.Core.AST.Method mdef ->
                match kind with
                | A.MemberKind.Macro (m, p, f) -> // TODO macro fallback
                    methods.Add(mdef, node (Macro (m, p, None)) Undefined)    
                | A.MemberKind.Inline inl -> 
                    let vars = args |> Seq.concat |> Seq.map (fun p -> p.CompiledName) |> List.ofSeq
                    let parsed = WebSharper.Compiler.Common.Recognize.createInline vars inl
                    methods.Add(mdef, node Inline parsed)
                | _ ->
                if meth.IsInstanceMember then
                    let _, (p, deps, reqs, paddr) = prototype.Value
                    let name = 
                        match annot.Name with
                        | Some n -> n
                        | _ -> mdef.Value.MethodName
                    let name = Resolve.addChild mdef.Value.MethodName () p
                    let node = node (Instance name) (getBody())    
                    translated.Add (Hashed (name :: paddr.Value), node) 
                    methods.Add(mdef, node)
                else
                    let address = staticAddress mdef.Value.MethodName
                    let node = node (Static address) (getBody())
                    translated.Add (address, node) 
                    methods.Add(mdef, node)
            | WebSharper.Core.AST.Constructor cdef ->
                match kind with
                | A.MemberKind.Macro (m, p, f) ->
                    constructors.Add(cdef, node (Macro (m, p, None)) Undefined)
                | A.MemberKind.Inline inl ->
                    let vars = args |> Seq.concat |> Seq.map (fun p -> p.CompiledName) |> List.ofSeq
                    let parsed = WebSharper.Compiler.Common.Recognize.createInline vars inl
                    constructors.Add(cdef, node Inline parsed)
                | _ ->
                let address = staticAddress "ctor"
                let node = node (Static address) (getBody())
                translated.Add (address, node) 
                constructors.Add(cdef, node)
            | WebSharper.Core.AST.StaticConstructor ->
                let address = staticAddress "cctor"
                let node = node (Static address) (getBody())
                translated.Add (address, node) 
                cctor <- Some node
        | _ -> ()

    Some (
        def,
        {
//                CurrentAssembly = true
            Address = if addressAndMembersDict.IsValueCreated then Some (fst addressAndMembersDict.Value) else None
            BaseClass = None // TODO cls.BaseType |> Option.map (fun t -> t.TypeDefinition |> ToFSharpAST.getTypeDefinition)
            FieldNames = Set.empty // TODO
            Fields = emptyDict // TODO
            Prototype = if prototype.IsValueCreated then Some (fst prototype.Value) else None 
            StaticConstructor = cctor         
            Methods = methods 
            Constructors = constructors
            Interfaces = cls.DeclaredInterfaces |> Seq.map (fun i -> ToFSharpAST.getTypeDefinition i.TypeDefinition) |> List.ofSeq
        }
    )

do 3

let transformAssembly (declarations : seq<FSIFD>) =    

    let typesWithMembers = Dictionary<FSharpEntity, ResizeArray<_>>()
    
    let rec getTypesWithMembers d =
        match d with
        | FSIFD.Entity (a, b) ->
            if not a.IsFSharpAbbreviation then
                typesWithMembers.Add (a, ResizeArray())
                b |> List.iter getTypesWithMembers
        | FSIFD.MemberOrFunctionOrValue (a, b, c) -> 
                typesWithMembers.[a.EnclosingEntity].Add(a, b, c)
        | FSIFD.InitAction a ->
            ()

    declarations |> Seq.iter getTypesWithMembers 
    
    let translated = Dictionary() :> IDictionary<_,_>         
    let proxies = Dictionary() :> IDictionary<_,_>        

    let root = Dictionary()

    {
        Interfaces = typesWithMembers |> Seq.choose (fun (KeyValue(c, _)) -> transformInterface c) |> dict
        Classes = typesWithMembers |> Seq.choose (fun (KeyValue(c, m)) -> transformClass root translated proxies c m) |> dict
        Proxies = proxies
        RemoteMethods = emptyDict // TODO
        Translated = translated
    }      
