module WebSharper.Core.Metadata

open System.Collections.Generic

open WebSharper.Core.AST

type MemberScope =
    | InstanceMember
    | StaticMember

type RemotingKind =
    | RemoteAsync
    | RemoteSend
    | RemoteSync

type MethodHandle =
    {
        Assembly : string
        Code : int
    }
    member this.Pack() =
        string this.Assembly + ":" + string this.Code

    static member Unpack(s: string) =
        try
            let i = s.LastIndexOf ':'
            let code = int (s.Substring(i + 1))
            let assembly = s.Substring(0, i)
            { Assembly = assembly; Code = code }
        with _ ->
            failwith "Failed to deserialize method handle"
            //raise (MethodHandleSerializationException e)

[<RequireQualifiedAccess>]
type ParameterObject =
    | Null
    | Bool   of bool
    | SByte  of sbyte
    | Int16  of int16
    | Int32  of int32
    | Int64  of int64
    | Byte   of byte
    | UInt16 of uint16
    | UInt32 of uint32
    | UInt64 of uint64
    | Single of single
    | Double of double
    | Char   of char
    | String of string
    | Type   of Type 
    | Array  of ParameterObject[]

    static member OfObj(o: obj) =
        match o with
        | null -> Null
        | :? bool   as v -> Bool   v
        | :? sbyte  as v -> SByte  v
        | :? int16  as v -> Int16  v
        | :? int32  as v -> Int32  v
        | :? int64  as v -> Int64  v
        | :? byte   as v -> Byte   v
        | :? uint16 as v -> UInt16 v
        | :? uint32 as v -> UInt32 v
        | :? uint64 as v -> UInt64 v
        | :? single as v -> Single v
        | :? double as v -> Double v
        | :? char   as v -> Char   v
        | :? string as v -> String v
        | :? Type   as v -> Type   v
        | :? (obj[]) as a -> a |> Array.map ParameterObject.OfObj |> Array
        | _ -> failwith "Invalid type for macro/generator parameter object"

    static member ToObj(o: ParameterObject) =
        match o with
        | Null   -> null
        | Bool   x -> box x 
        | SByte  x -> box x 
        | Int16  x -> box x 
        | Int32  x -> box x 
        | Int64  x -> box x 
        | Byte   x -> box x 
        | UInt16 x -> box x 
        | UInt32 x -> box x 
        | UInt64 x -> box x 
        | Single x -> box x 
        | Double x -> box x 
        | Char   x -> box x 
        | String x -> box x 
        | Type   x -> box x // TODO: load System.Type
        | Array  a -> box (a |> Array.map ParameterObject.ToObj)

type CompiledMember =
    | Instance of string
    | Abstract of string
    | Static of Address
    | Constructor of Address
    | Inline
    | Macro of TypeDefinition * option<ParameterObject> * option<CompiledMember> 
    | Remote of MemberScope * RemotingKind * MethodHandle

let ignoreMacro m =
    match m with
    | Macro (_, _, Some f) -> f
    | _ -> m

type CompilingMember =
    | NotCompiled of CompiledMember
    | NotGenerated of TypeDefinition * option<obj> * CompiledMember

[<RequireQualifiedAccess>]
type NotResolvedMemberKind = 
    | Instance
//    | Abstract
    | Static
    | Constructor
    | Override of TypeDefinition
    | Implementation of TypeDefinition
    | Remote of MemberScope * RemotingKind * MethodHandle
    | Inline
    | NoFallback

type private N = NotResolvedMemberKind

type Node =
    | MethodNode of TypeDefinition * Method
    | ConstructorNode of TypeDefinition * Constructor
    | ImplementationNode of TypeDefinition * TypeDefinition * Method
    | AbstractMethodNode of TypeDefinition * Method
    | TypeNode of TypeDefinition
    | ResourceNode of TypeDefinition
    | AssemblyNode of string 
//    | External of Node

let getDefiningTypeOfNode node =
    match node with
    | MethodNode (td, _) -> Some td
    | ConstructorNode (td, _) -> Some td
    | ImplementationNode (td, _, _) -> Some td
    | AbstractMethodNode (td, _) -> Some td
    | TypeNode td -> Some td
    | ResourceNode td -> Some td
    | AssemblyNode _ -> None

type NotResolvedMethod =
    {
        Kind : NotResolvedMemberKind
        StrongName : option<string>
        Macro : option<TypeDefinition * option<obj>>
        Generator : option<TypeDefinition * option<obj>>
        Compiled : bool
        Body : Expression
        Requires : list<TypeDefinition>
    }

type CompiledField =
    | InstanceField of string
    | OptionalField of string
    | StaticField of Address
//    | StaticGetSet of Address

type ClassInfo =
    {
        Address : option<Address>
        BaseClass : option<TypeDefinition>
        Constructors : IDictionary<Constructor, CompiledMember * Expression>
        Fields : IDictionary<string, CompiledField>
        StaticConstructor : option<Address * Expression>
        Methods : IDictionary<Method, CompiledMember * Expression>
        Implementations : IDictionary<TypeDefinition * Method, CompiledMember * Expression>
        IsModule : bool
    }

type NotResolvedField =
    {
        StrongName : option<string>
        IsStatic : bool
        IsOptional : bool   
    }

[<RequireQualifiedAccess>]
type NotResolvedMember =
    | Constructor of Constructor * NotResolvedMethod
    | Field of string * NotResolvedField
    | StaticConstructor of Expression
    | Method of Method * NotResolvedMethod

type private M = NotResolvedMember

type NotResolvedClass =
    {
        StrongName : option<string>
        BaseClass : option<TypeDefinition>
        Requires : list<TypeDefinition> 
        Members : list<NotResolvedMember>
        IsModule : bool
    }

type InterfaceInfo =
    {
        Extends : list<TypeDefinition>
        Methods : IDictionary<Method, string>
    }

type NotResolvedInterface =
    {
        StrongName : option<string>
        Extends : list<TypeDefinition>
        NotResolvedMethods : list<Method * option<string>>
    }

type GraphData =
    {
        Nodes : Node[]
        Edges : int[][]
        Overrides : (int * (int * int)[])[]
    }
 
module R = Resources 
            
[<Sealed>]
type AssemblyResource(name) =
    interface R.IResource with
        member this.Render ctx writer =
            let r = ctx.GetAssemblyRendering name
            r.Emit(writer R.Scripts, R.Js)

let activate resource =
    match resource with
    | AssemblyNode name ->
        AssemblyResource name :> R.IResource
    | ResourceNode t ->
        try
            Reflection.loadTypeDefinition t
            |> System.Activator.CreateInstance
            |> unbox
        with e ->
            {
                new R.IResource with
                    member this.Render ctx writer =
                        let writer = writer R.Scripts
                        writer.Write("<-- ")
                        writer.Write("Failed to load: {0}; because of: {1}", t, e.Message)
                        writer.WriteLine(" -->")
                        ()
            }
    | _ -> failwith "not a resource node"

type Graph =
    {
        Nodes : ResizeArray<Node>
        Edges : ResizeArray<SortedSet<int>>
        Overrides : IDictionary<int, IDictionary<int, int>> // TypeNode -> AbstractMethodNode -> ImplementationNode
        Lookup : IDictionary<Node, int>
        Resources : IDictionary<int, Resources.IResource>
        NewNodes : ResizeArray<int>
    }

    static member FromData (data: GraphData) =
        let lookup = Dictionary()
        data.Nodes |> Seq.iteri (fun i n -> lookup.Add(n, i))
        let overrides = Dictionary()
        for typ, ms in data.Overrides do
            let o = Dictionary()
            for td, d in ms do o.Add(td, d)
            overrides.Add(typ, o :> IDictionary<_,_>)
        {
            Nodes = ResizeArray(data.Nodes)
            Edges = ResizeArray(data.Edges |> Array.map (fun e -> SortedSet(e)))
            Overrides = overrides
            Lookup = lookup
            Resources = Dictionary()
            NewNodes = ResizeArray()
        }

    static member FromData (data: seq<GraphData>) =
        let graphs = Array.ofSeq data
            
        let nodes = ResizeArray()
        let lookup = Dictionary()
        let edges = ResizeArray() 
        let overrides = Dictionary()
        for i = 0 to graphs.Length - 1 do
            let g = graphs.[i]
            let r = Dictionary() // redirects
            for j = 0 to g.Nodes.Length - 1 do
                let jn = g.Nodes.[j]
                let jr =
                    match lookup.TryFind jn with 
                    | Some jr -> jr
                    | _ ->
                        let jr = nodes.Count
                        nodes.Add jn 
                        edges.Add(SortedSet())
                        lookup.Add(jn, jr)
                        jr
                r.Add(j, jr)
            
            for j = 0 to g.Edges.Length - 1 do
                let ejr = edges.[r.[j]]
                for d in g.Edges.[j] do
                    ejr.Add r.[d] |> ignore
            
            for typ, ors in g.Overrides do
                let o = Dictionary() :> IDictionary<_,_>
                overrides.Add(typ, o)
                for td, n in ors do
                    o.Add(td, n)                 
        {
            Nodes = nodes
            Edges = edges
            Overrides = overrides
            Lookup = lookup
            Resources = Dictionary()
            NewNodes = ResizeArray()
        }

    member this.GetCurrentData() =
        let allNodes = SortedSet(this.NewNodes)
        // adding all assembly nodes cause them to be correctly ordered on unioning 
        this.Nodes |> Seq.iteri (fun i n ->
            match n with
            | AssemblyNode _ -> allNodes.Add i |> ignore
            | _ -> ()        
        )
        for n in this.NewNodes do
            for d in this.Edges.[n] do 
                allNodes.Add d |> ignore
            match this.Overrides.TryFind n with
            | Some td ->
                for KeyValue(a, m) in td do
                    allNodes.Add a |> ignore
                    allNodes.Add m |> ignore                   
            | _ -> ()
        let r = Dictionary() // redirects
        let nodes = ResizeArray()
        for i in allNodes do
            r.Add(i, nodes.Count)
            nodes.Add(this.Nodes.[i])            
        {
            GraphData.Nodes = nodes.ToArray()
            Edges =
                allNodes |> Seq.map (fun i ->
                    if this.NewNodes.Contains i then
                        this.Edges.[i] |> Seq.map (fun d -> r.[d]) |> Array.ofSeq  
                    else [||] 
                ) |> Array.ofSeq
            Overrides =
                this.NewNodes |> Seq.choose (fun tn ->
                    match this.Overrides.TryFind tn with
                    | Some td ->
                        Some (
                            r.[tn],
                            td |> Seq.map (fun (KeyValue (td, m)) -> r.[td], r.[m]) |> Array.ofSeq
                        )
                    | _ -> None
                ) |> Array.ofSeq
        }

    member this.GetData() =
        {
            GraphData.Nodes = this.Nodes.ToArray()
            Edges = this.Edges.ToArray() |> Array.map Array.ofSeq
            Overrides = 
                this.Overrides 
                |> Seq.map (fun (KeyValue (typ, ms)) -> 
                    typ,
                    ms |> Seq.map (fun (KeyValue(td, d)) -> td, d) |> Array.ofSeq
                ) 
                |> Array.ofSeq
        } 
    
    member this.GetDependencies (nodes : seq<Node>) =
        let allNodes = SortedSet()
        let newNodes = SortedSet()
        let newTypes = HashSet()
        let newAbstractMembers = HashSet()

        let graphAllNodeStrings =
            this.Nodes |> Seq.map (fun n ->
                match n with
                | MethodNode (td, m) -> sprintf "MethodNode (%s, %s)" td.Value.FullName m.Value.MethodName
                | ConstructorNode (td, _) -> sprintf "ConstructorNode %s" td.Value.FullName 
                | ImplementationNode (td, i, m) -> sprintf "ImplementationNode (%s, %s, %s)" td.Value.FullName i.Value.FullName m.Value.MethodName
                | AbstractMethodNode (td, m) -> sprintf "AbstractMethodNode (%s, %s)" td.Value.FullName m.Value.MethodName
                | TypeNode td -> sprintf "TypeNode %s" td.Value.FullName 
                | ResourceNode td -> sprintf "ResourceNode %s" td.Value.FullName 
                | AssemblyNode a -> sprintf "AssemblyNode %s" a
            ) |> Array.ofSeq

        let addNode n i =
            if allNodes.Add i then
                newNodes.Add i |> ignore
                match n with
                | AbstractMethodNode (td, m) -> this.Lookup.TryFind (AbstractMethodNode (td, m)) |> Option.iter (newAbstractMembers.Add >> ignore) 
                | TypeNode td -> this.Lookup.TryFind (TypeNode td) |> Option.iter (newTypes.Add >> ignore)
                | _ -> ()
        
        nodes |> Seq.iter (fun n -> this.Lookup.TryFind n |> Option.iter (addNode n))

        while newNodes.Count > 0 do
            let currentNodes = Array.ofSeq newNodes
            newNodes.Clear()
            
            for n in currentNodes do
                this.Edges.[n] |> Seq.iter (fun i -> addNode this.Nodes.[i] i)
            
            let currentTypes = Array.ofSeq newTypes
            newTypes.Clear()
            let currentAbstractMembers = Array.ofSeq newAbstractMembers
            newAbstractMembers.Clear()
            
            for typ in currentTypes do
                match this.Overrides.TryFind typ with
                | Some ors ->
                    for mem in currentAbstractMembers do
                        ors.TryFind mem |> Option.iter (fun i -> addNode this.Nodes.[i] i)
                | _ -> ()
        allNodes |> Seq.map (fun n -> this.Nodes.[n]) |> List.ofSeq

    member this.GetResources (nodes : seq<Node>) =
        let nodes = Array.ofSeq nodes        
        let deps = this.GetDependencies(nodes)
        
        let res = deps |> List.choose (fun n ->
            match n with
            | AssemblyNode _
            | ResourceNode _ ->
                let i = this.Lookup.[n]
                match this.Resources.TryFind i with
                | Some _ as found -> found
                | _ ->
                    let res = activate n
                    this.Resources.Add(i, res)
                    Some res
            | _ -> None
        )

        let jq =
            Reflection.loadTypeDefinition (Hashed { Assembly = "WebSharper.JQuery"; FullName = "WebSharper.JQuery.Resources.JQuery" })
            |> System.Activator.CreateInstance
            |> unbox

        Resources.Runtime.Instance :: jq :: res

//        this.GetDependencies(nodes) |> List.choose (fun n ->
//            match n with
//            | AssemblyNode _
//            | ResourceNode _ ->
//                let i = this.Lookup.[n]
//                match this.Resources.TryFind i with
//                | Some _ as found -> found
//                | _ ->
//                    let res = activate n
//                    this.Resources.Add(i, res)
//                    Some res
//            | _ -> None
//        )

    member this.AddOrLookupNode n =
        match this.Lookup.TryFind n with
        | Some i -> i
        | _ ->
            let i = this.Nodes.Count
            this.Nodes.Add n
            this.Edges.Add(SortedSet())
            this.Lookup.Add(n, i)
            this.NewNodes.Add i
            i  

    member this.GetNodeDeps n =
        this.Edges.[this.AddOrLookupNode n]    

    member this.AddEdge (n, d) = this.Edges.[n].Add(d) |> ignore
    member this.AddEdge (n, d) = this.Edges.[n].Add(this.AddOrLookupNode d) |> ignore
    member this.AddEdge (n, d) = this.Edges.[this.AddOrLookupNode n].Add(d) |> ignore
    member this.AddEdge (n, d) = this.Edges.[this.AddOrLookupNode n].Add(this.AddOrLookupNode d) |> ignore
    
//    
//    member this.GetOverrideNode(typ, td, m) =
//        match this.Lookup.TryFind (TypeNode typ) with
//        | Some tn ->
//            let t =
//                match this.Overrides.TryFind tn with
//                | Some t -> t
//                | None ->
//                    let t = Dictionary()
//                    this.Overrides.Add(tn, t)
//                    t :> _
//            match this.Lookup.TryFind (AbstractMethodNode (td, m)) with
//            | Some an ->
//                match t.TryFind an with
//                | Some d -> d
//                | None ->
//                    t.Add((td, m), an)
//                    d 
//            | _ -> []
//        | _ -> []

    static member Empty : Graph =
        {
            Nodes = ResizeArray()
            Edges = ResizeArray()
            Overrides = Dictionary()
            Lookup = Dictionary()
            Resources = Dictionary()
            NewNodes = ResizeArray()
        }

type Metadata =
    {
        SiteletDefinition: option<TypeDefinition>
        Dependencies : GraphData
        Interfaces : IDictionary<TypeDefinition, InterfaceInfo>
        Classes : IDictionary<TypeDefinition, ClassInfo>
    }

let empty =
    {
        SiteletDefinition = None
        Dependencies = Graph.Empty.GetData()
        Interfaces = Map.empty
        Classes = Map.empty
    }

let union (metas: seq<Metadata>) =
    let metas = Array.ofSeq metas
    {
        SiteletDefinition = metas |> Seq.tryPick (fun m -> m.SiteletDefinition) // TODO 
        Dependencies = Graph.FromData(metas |> Seq.map (fun m -> m.Dependencies)).GetData()
        Interfaces = Dict.union (metas |> Seq.map (fun m -> m.Interfaces))
        Classes = Dict.union (metas |> Seq.map (fun m -> m.Classes))
    }

let getAllAddresses (meta: Metadata) =
    let r = Resolve.Resolver()
    for KeyValue(typ, cls) in meta.Classes do
        let hasPrototype = Option.isSome cls.Address
        let clAddr = cls.Address |> Option.map (fun a -> r.ExactClassAddress(a.Value, hasPrototype))
        let pr = if hasPrototype then Some (r.LookupPrototype typ) else None 
        let rec addMember (m: CompiledMember) =
            match m with
            | Instance n -> pr.Value.Add n |> ignore
            | Static a 
            | Constructor a -> r.ExactStaticAddress a.Value |> ignore
            | Macro (_, _, Some m) -> addMember m
            | _ -> ()
        for m, _ in cls.Constructors.Values do addMember m
        for f in cls.Fields.Values do
            match f with
            | InstanceField n 
            | OptionalField n -> pr.Value.Add n |> ignore
            | StaticField a -> r.ExactStaticAddress a.Value |> ignore
        for m, _ in cls.Implementations.Values do addMember m
        for m, _ in cls.Methods.Values do addMember m
        match cls.StaticConstructor with
        | Some (a, _) -> r.ExactStaticAddress a.Value |> ignore  
        | _ -> ()
    // inheritance 
    let inheriting = HashSet()
    let rec inheritMembers typ (cls: ClassInfo) =
        if inheriting.Add typ then
            match cls.BaseClass with
            | None -> ()
            | Some b ->
                // assembly containing base class may not be referenced
                match meta.Classes.TryFind b with
                | None -> ()
                | Some bCls ->
                    inheritMembers b bCls              
                    (r.LookupPrototype typ).UnionWith(r.LookupPrototype b) 
    for KeyValue(typ, cls) in meta.Classes do
        inheritMembers typ cls        
    r

module B = WebSharper.Core.Binary

//exception CompilationError of message: string * position: SourcePos
//    with override this.Message = this.message    

type CompilationError =
    | SourceError of string
    | NameConflict of string * string
    | TypeNotFound of TypeDefinition
//    | MethodNotFound of TypeDefinition * Method
    | MethodNotFound of TypeDefinition * Method * list<Method>
    | MethodNameNotFound of TypeDefinition * Method * list<string>
    | ConstructorNotFound of TypeDefinition * Constructor
    | FieldNotFound of TypeDefinition * string 
  
    override this.ToString() =
        match this with
        | SourceError msg -> msg
        | NameConflict (msg, a) -> sprintf "%s at JavaScript address: '%s'" msg a
        | TypeNotFound typ -> sprintf "Type not found in compilation: %s" typ.Value.FullName
//        | MethodNotFound (typ, meth) -> sprintf "Method not found in compilation: %s.%s" typ.Value.FullName meth.Value.MethodName
        | MethodNotFound (typ, meth, candidates) ->
            sprintf "Method not found in compilation: %s, Candidates: %s" 
                (Reflection.printMethod meth.Value) 
                (candidates |> Seq.map (fun m -> Reflection.printMethod m.Value) |> String.concat ", ")
        | MethodNameNotFound (typ, meth, candidates) ->
            sprintf "Method name not found in compilation: %s, Members: %s" 
                (Reflection.printMethod meth.Value) 
                (candidates |> String.concat ", ")
        | ConstructorNotFound (typ, _) -> sprintf "Constructor not found in compilation for: %s" typ.Value.FullName
        | FieldNotFound (typ, field) -> sprintf "Field not found in compilation: %s.%s" typ.Value.FullName field

type LookupMemberResult =
    | Compiled of CompiledMember * Expression
    | Compiling of CompilingMember * Expression
    | LookupMemberError of CompilationError 

type LookupFieldResult =
    | CompiledField of CompiledField
    | LookupFieldError of CompilationError 

type CompilationWarning =
    | PublicProxy of TypeDefinition
             
type Compilation(meta: Metadata) =    
    let notResolvedInterfaces = Dictionary<TypeDefinition, NotResolvedInterface>()
    let notResolvedClasses = Dictionary<TypeDefinition, NotResolvedClass>()
    let proxies = Dictionary<TypeDefinition, TypeDefinition>()

    let classes = MergedDictionary meta.Classes
    let interfaces = MergedDictionary meta.Interfaces

    let graph = Graph.FromData(meta.Dependencies)

    let compilingMethods = Dictionary<TypeDefinition * Method, CompilingMember * Expression>()
    let compilingImplementations = Dictionary<TypeDefinition * TypeDefinition * Method, CompilingMember * Expression>()
    let compilingConstructors = Dictionary<TypeDefinition * Constructor, CompilingMember * Expression>()
    let compilingStaticConstructors = Dictionary<TypeDefinition, Address * Expression>()

    let findProxied typ = 
        match proxies.TryFind typ with
        | Some p -> p 
        | _ -> typ

    let errors = ResizeArray()
    let warnings = ResizeArray() 

    let macros = System.Collections.Generic.Dictionary<TypeDefinition, Macro option>()
//    let generators = System.Collections.Generic.Dictionary<TypeDefinition, Generator>()

    member val SiteletDefinition: option<TypeDefinition> = None with get, set
    member val AssemblyName = "EntryPoint" with get, set
    member val AssemblyRequires = [] : list<TypeDefinition> with get, set
    
    member this.AddError (pos : SourcePos option, error : CompilationError) =
        errors.Add (pos, error)

    member this.Errors = List.ofSeq errors

    member this.AddWarning (pos : SourcePos option, warning : CompilationWarning) =
        warnings.Add (pos, warning)

    member this.Warnings = List.ofSeq warnings

    member this.GetOrInitMacro(macro) =
        match macros.TryFind macro with
        | Some res -> res
        | _ ->
            let res =
                try 
                    let mt = System.Type.GetType(macro.Value.AssemblyQualifiedName)
                    match mt.GetConstructor([|typeof<Compilation>|]) with
                    | null -> mt.GetConstructor([||]).Invoke([||])
                    | mctor -> mctor.Invoke([|this|])
                    :?> WebSharper.Core.Macro
                    |> Some
                with _ ->                                                                      
                    this.AddError(None, SourceError(sprintf "Failed to load macro type: '%s'" macro.Value.AssemblyQualifiedName))
                    None 
            macros.Add(macro, res)
            res

    member this.DependencyMetadata = meta

    member this.Graph = graph

    member this.ToCurrentMetadata() =
        {
            SiteletDefinition = this.SiteletDefinition 
            Dependencies = graph.GetCurrentData() // TODO only current
            Interfaces = interfaces.Current
            Classes = classes.Current
        }    

    member this.AddProxy(tProxy, tTarget) =
        proxies.Add(tProxy, tTarget)  

    member this.AddClass(typ, cls) =
        notResolvedClasses.Add(typ, cls)

    member this.AddInterface(typ, intf) =
        notResolvedInterfaces.Add(typ, intf)
    
    member this.TryLookupClassInfo typ =   
        classes.TryFind(findProxied typ)
    
    member this.LookupMethodInfo(typ, meth) = 
        let typ = findProxied typ
        match interfaces.TryFind typ with
        | Some intf -> Compiled (Instance intf.Methods.[meth], Undefined) 
        | _ -> 
        match classes.TryFind typ with
        | Some cls ->
            match cls.Methods.TryFind meth with
            | Some m -> Compiled m
            | _ -> 
                match compilingMethods.TryFind (typ, meth) with
                | Some m -> Compiling m
                | _ -> 
                    let mName = meth.Value.MethodName
                    let candidates = 
                        [
                            for m in cls.Methods.Keys do
                                if m.Value.MethodName = mName then
                                    yield m
                            for t, m in compilingMethods.Keys do
                                if typ = t && m.Value.MethodName = mName then
                                    yield m
                        ]
                    if List.isEmpty candidates then
                        let names =
                            seq {
                                for m in cls.Methods.Keys do
                                    yield m.Value.MethodName
                                for t, m in compilingMethods.Keys do
                                    if typ = t then
                                        yield m.Value.MethodName
                            }
                            |> Seq.distinct |> List.ofSeq
                        LookupMemberError (MethodNameNotFound (typ, meth, names))
                    else
                        LookupMemberError (MethodNotFound (typ, meth, candidates))
        | _ ->
            LookupMemberError (TypeNotFound typ)

    member this.LookupFieldInfo(typ, field) =
        let typ = findProxied typ
        match classes.TryFind typ with
        | Some cls ->
            match cls.Fields.TryFind field with
            | Some f -> CompiledField f
            | _ -> LookupFieldError (FieldNotFound (typ, field))
        | _ ->
            LookupFieldError (TypeNotFound typ)

    member this.LookupConstructorInfo(typ, ctor) =
        let typ = findProxied typ
        match classes.TryFind typ with
        | Some cls ->
            match cls.Constructors.TryFind ctor with
            | Some c -> Compiled c
            | _ -> 
                match compilingConstructors.TryFind (typ, ctor) with
                | Some c -> Compiling c
                | _ -> LookupMemberError (ConstructorNotFound (typ, ctor))
        | _ ->
            LookupMemberError (TypeNotFound typ)
    
    member this.LookupStaticConstructorAddress(typ) =
        let typ = findProxied typ
        let cls = classes.[typ]
        let res = fst cls.StaticConstructor.Value
        if res.Value.IsEmpty then
            fst compilingStaticConstructors.[typ]        
        else 
            res

    member this.CompilingMethods = compilingMethods  
    member this.CompilingConstructors = compilingConstructors

    member this.AddCompiledMethod(typ, meth, info, comp) =
        let typ = findProxied typ 
        compilingMethods.Remove(typ, meth) |> ignore
        let cls = classes.[typ]
        cls.Methods.Add(meth, (info, comp))

    member this.AddCompiledConstructor(typ, ctor, info, comp) = 
        let typ = findProxied typ 
        compilingConstructors.Remove(typ, ctor) |> ignore
        let cls = classes.[typ]
        cls.Constructors.Add(ctor, (info, comp))

    member this.GetCompilingStaticConstructors() =
        compilingStaticConstructors |> Seq.map (fun (KeyValue(t, (a, c))) -> t, a, c) |> Array.ofSeq

    member this.AddCompiledStaticConstructor(typ, addr, cctor) =
        let typ = findProxied typ 
        compilingStaticConstructors.Remove typ |> ignore
        let cls = classes.[typ]
        classes.[typ] <- { cls with StaticConstructor = Some (addr, cctor) }

    member this.GetCompilingImplementations() =
        compilingImplementations |> Seq.map (fun (KeyValue((t, i, m), (n, e))) -> t, i, m, n, e) |> Array.ofSeq

    member this.AddCompiledImplementation(typ, intf, meth, info, comp) =
        let typ = findProxied typ 
        compilingImplementations.Remove(typ, intf, meth) |> ignore
        let cls = classes.[typ]
        cls.Implementations.Add((intf, meth), (info, comp))

    member this.Resolve () =
        let rec resolveInterface typ (nr: NotResolvedInterface) =
            let allMembers = HashSet()
            
            let rec addInherited (n: InterfaceInfo) =
                for i in n.Extends do
                    addInherited interfaces.[i]
                for m in n.Methods.Values do
                    if not (allMembers.Add m) then
                        failwith "Interface method name collision."
            
            for i in nr.Extends do
                notResolvedInterfaces.TryFind i |> Option.iter (resolveInterface i)       
                addInherited interfaces.[i]
//                match interfaces.TryFind i with
//                | Some n -> addInherited n
//                | _ -> failwith "Interface declaration or proxy not found."
            
            let resMethods = Dictionary()
                            
            for m, n in nr.NotResolvedMethods do
                match n with
                | Some n -> 
                    if not (allMembers.Add n) then
                        failwith "Explicitly declared interface method name collision."
                    resMethods.Add(m, n)
                | _ -> ()

            let intfName = typ.Value.FullName.Replace('.', '-').Replace('+', '-') + "-"

            for m, n in nr.NotResolvedMethods do
                match n with
                | None ->
                    let n = Resolve.getRenamed (intfName + m.Value.MethodName) allMembers
                    resMethods.Add(m, n)
                | _ -> ()

            let resNode =
                {
                    Extends = nr.Extends
                    Methods = resMethods
                }
            interfaces.Add(typ, resNode)
            notResolvedInterfaces.Remove typ |> ignore
        
        while notResolvedInterfaces.Count > 0 do
            let (KeyValue(typ, nr)) = Seq.head notResolvedInterfaces  
            resolveInterface typ nr

        let r = getAllAddresses meta

        let someEmptyAddress = Some (Address [])
        let unresolvedCctor = Some (Address [], Undefined)

        let isInstanceKind k = match k with N.Instance | N.Override _ | N.Implementation _ -> true | _ -> false

        let isInstanceMember m =
            match m with
            | M.Constructor (_, { Kind = k })
            | M.Method (_, { Kind = k }) -> isInstanceKind k 
            | _ -> false

        let asmNodeIndex = graph.AddOrLookupNode(AssemblyNode this.AssemblyName)
        for req in this.AssemblyRequires do
            graph.AddEdge(asmNodeIndex, ResourceNode req)

        // initialize all class entries
        for KeyValue(typ, cls) in notResolvedClasses do
            let hasPrototype = cls.Members |> Seq.exists isInstanceMember
            let cctor = cls.Members |> Seq.tryPick (function M.StaticConstructor e -> Some e | _ -> None)
            let baseCls =
                cls.BaseClass |> Option.bind (fun b ->
                    let b = findProxied b
                    if classes.ContainsKey b || notResolvedClasses.ContainsKey b then Some b else None
                )
            classes.Add (typ,
                {
                    Address = if hasPrototype then someEmptyAddress else None
                    BaseClass = baseCls
                    Constructors = Dictionary() 
                    Fields = Dictionary() 
                    StaticConstructor = if Option.isSome cctor then unresolvedCctor else None 
                    Methods = Dictionary()
                    Implementations = Dictionary()
                    IsModule = cls.IsModule
                }
            ) 
            // set up dependencies
            let clsNodeIndex = graph.AddOrLookupNode(TypeNode typ)
            graph.AddEdge(clsNodeIndex, asmNodeIndex)
            for req in cls.Requires do
                graph.AddEdge(clsNodeIndex, ResourceNode req)
            cls.BaseClass |> Option.iter (fun b -> graph.AddEdge(clsNodeIndex, TypeNode b))
            for m in cls.Members do
                match m with
                | M.Constructor (ctor, { Kind = k; Requires = reqs }) -> 
                    match k with
                    | N.NoFallback -> ()
                    | _ ->
                        let cNode = graph.AddOrLookupNode(ConstructorNode(typ, ctor))
                        graph.AddEdge(cNode, clsNodeIndex)
                        for req in reqs do
                            graph.AddEdge(cNode, ResourceNode req)
                | M.Method (meth, { Kind = k; Requires = reqs }) -> 
                    match k with
                    | N.Override btyp ->
                        let mNode = graph.AddOrLookupNode(MethodNode(typ, meth))
                        graph.AddEdge(mNode, AbstractMethodNode(btyp, meth))
                        graph.AddEdge(mNode, clsNodeIndex)
                        for req in reqs do
                            graph.AddEdge(mNode, ResourceNode req)
                    | N.Implementation intf ->
                        let mNode = graph.AddOrLookupNode(ImplementationNode(typ, intf, meth))
                        graph.AddEdge(mNode, clsNodeIndex)
                        for req in reqs do
                            graph.AddEdge(mNode, ResourceNode req)
                    | N.NoFallback -> ()
                    | _ -> 
                        let mNode = graph.AddOrLookupNode(MethodNode(typ, meth))
                        graph.AddEdge(mNode, clsNodeIndex)
                        for req in reqs do
                            graph.AddEdge(mNode, ResourceNode req)
                | _ -> ()

        let withMacro nr woMacro =
            match nr.Macro with
            | Some (p, o) ->
                match nr.Kind with
                | N.NoFallback -> Macro (p, o |> Option.map ParameterObject.OfObj, None)
                | _ -> Macro (p, o |> Option.map ParameterObject.OfObj, Some woMacro)
            | _ -> woMacro

        let compiledStaticMember (address: Address) (nr : NotResolvedMethod) =
            match nr.Kind with
            | N.Static -> Static address
            | N.Constructor -> Constructor address
            | _ -> failwith "Invalid static member kind"
            |> withMacro nr        

        let compiledNoAddressMember (nr : NotResolvedMethod) =
            match nr.Kind with
            | N.Inline -> Inline
            | N.Remote (s, k, h) -> Remote (s, k, h)
            | N.NoFallback -> Inline // will be erased
            | _ -> failwith "Invalid not compiled member kind"
            |> withMacro nr

        let compiledInstanceMember (name: string) (nr: NotResolvedMethod) =
            match nr.Kind with
            | N.Instance  
            | N.Override _  
            | N.Implementation _ -> Instance name
            | _ -> failwith "Invalid instance member kind"
            |> withMacro nr

        let toCompilingMember (nr : NotResolvedMethod) (comp: CompiledMember) =
            match nr.Generator with
            | Some (g, p) -> NotGenerated(g, p, comp)
            | _ -> NotCompiled comp
            
        let setClassAddress typ clAddr =
            let res = classes.[typ]
            classes.[typ] <- { res with Address = Some clAddr }

        // split to resolve steps
        let stronglyNamedClasses = ResizeArray()
        let fullyNamedStaticMembers = ResizeArray()
        let remainingClasses = ResizeArray()
        let remainingNamedStaticMembers = Dictionary()
        let namedInstanceMembers = Dictionary() // includes implementations
//        let abstractInstanceMembers = Dictionary()
        let remainingStaticMembers = Dictionary()
        let remainingInstanceMembers = Dictionary() // includes overrides

        let addCctorCall typ (ci: ClassInfo) expr =
            if Option.isSome ci.StaticConstructor then
                match expr with
                | Function (args, body) ->
                    Function(args, Statements [ ExprStatement (Cctor typ); body ])
                | _ -> failwith "Member body must be a function"
            else expr

        for KeyValue(typ, cls) in notResolvedClasses do
            let namedCls =
                match cls.StrongName with
                | Some sn ->
                    stronglyNamedClasses.Add (typ, sn)
                    true
                | _ -> 
                    remainingClasses.Add typ
                    false

            let getStrongNameAndScope m =
                let getScope kind =
                    match kind with
                    | N.Override _
                    | N.Implementation _
                    | N.Instance -> Some InstanceMember
                    | N.Static
                    | N.Constructor -> Some StaticMember
                    | N.Remote _
                    | N.Inline
                    | N.NoFallback -> None
 
                match m with
                | M.Constructor (_, { StrongName = sn; Kind = k }) 
                | M.Method (_, { StrongName = sn; Kind = k }) -> sn, getScope k
                | M.Field (_, { StrongName = sn; IsStatic = s }) -> 
                    sn, Some (if s then StaticMember else InstanceMember) 
                | M.StaticConstructor _ -> None, Some StaticMember
            
            let cc = classes.[typ]

            // TODO: fail on named overrides and implementations
            for m in cls.Members do
                match getStrongNameAndScope m with
                | Some sn, Some StaticMember ->
                    if namedCls || sn.Contains "." then
                        fullyNamedStaticMembers.Add (typ, m, sn) 
                    else 
                        Dict.addToMulti remainingNamedStaticMembers typ (m, sn)
                | Some sn, Some InstanceMember ->
                    Dict.addToMulti namedInstanceMembers typ (m, sn)
                | _, None ->
                    match m with 
                    | M.Constructor (cDef, nr) ->
                        let comp = compiledNoAddressMember nr
                        if nr.Compiled then
                            cc.Constructors.Add (cDef, (comp, addCctorCall typ cc nr.Body))
                        else 
                            compilingConstructors.Add((typ, cDef), (toCompilingMember nr comp, addCctorCall typ cc nr.Body))      
                    | M.Method (mDef, nr) -> 
                        let comp = compiledNoAddressMember nr
                        if nr.Compiled then
                            cc.Methods.Add (mDef, (comp, addCctorCall typ cc nr.Body))
                        else 
                            compilingMethods.Add((typ, mDef), (toCompilingMember nr comp, addCctorCall typ cc nr.Body)) 
                    | _ -> failwith "Fields ands static constructors are always named"     
                | None, Some StaticMember ->
                    Dict.addToMulti remainingStaticMembers typ m
                | None, Some InstanceMember ->
                    match m with
                    | M.Method (mDef, { Kind = N.Override td }) ->
                        if td = sysObjDef then //|| td.Value.FullName = "WebSharper.Web.Control" then
                            Dict.addToMulti namedInstanceMembers typ (m, mDef.Value.MethodName)
                        else
                            Dict.addToMulti remainingInstanceMembers typ m
//                        let n = 
//                            match classes.[td].Methods.[mDef] with
//                            | Instance n , _ -> n
//                            | _ -> failwith "abstract methods must be Instance"
//                        Dict.addToMulti namedInstanceMembers typ (m, n)
                    | M.Method (mDef, { Kind = N.Implementation td }) ->
                        if td.Value.FullName = "System.Collections.Generic.IEnumerable`1" then
                            Dict.addToMulti namedInstanceMembers typ (m, "GetEnumerator")
                        else 
                        let n = interfaces.[td].Methods.[mDef]
                        Dict.addToMulti namedInstanceMembers typ (m, n)
                    | _ -> 
                        Dict.addToMulti remainingInstanceMembers typ m                   

        for typ, sn in stronglyNamedClasses do
            let addr = 
                match sn.Split('.') with
                | [||] -> 
                    this.AddError(None, SourceError (sprintf "Invalid Name attribute argument on type '%s'" typ.Value.FullName))
                    ["$$ERROR$$"]
                    //failwith "Invalid Name attribute argument"
//                | [| n |] ->
//                    // TODO maybe other logic for nested
//                    let orig = typ.Value.FullName.Split('.')
//                    [ 
//                        yield! orig |> Seq.take (orig.Length - 2)
//                        yield n
//                    ]
                | a -> List.ofArray a
                |> List.rev
            if not (r.ExactClassAddress(addr, Option.isSome classes.[typ].Address)) then
                this.AddError(None, NameConflict ("Class name conflict", sn))
            setClassAddress typ (Address addr)

        let nameStaticMember typ addr m = 
            let res = classes.[typ]
            match m with
            | M.Constructor (cDef, nr) ->
                let comp = compiledStaticMember addr nr
                if nr.Compiled then
                    res.Constructors.Add(cDef, (comp, addCctorCall typ res nr.Body))
                else
                    compilingConstructors.Add((typ, cDef), (toCompilingMember nr comp, addCctorCall typ res nr.Body))
            | M.Field (fName, _) ->
                res.Fields.Add(fName, StaticField addr)
            | M.Method (mDef, nr) ->
                let comp = compiledStaticMember addr nr
                if nr.Compiled then 
                    res.Methods.Add(mDef, (comp, addCctorCall typ res nr.Body))
                else
                    compilingMethods.Add((typ, mDef), (toCompilingMember nr comp, addCctorCall typ res nr.Body))
            | M.StaticConstructor expr ->                
                compilingStaticConstructors.Add(typ, (addr, expr))
        
        let nameInstanceMember typ name m =
            let res = classes.[typ]
            match m with
            | M.Field (fName, f) ->
                res.Fields.Add(fName, if f.IsOptional then OptionalField name else InstanceField name)
            | M.Method (mDef, nr) ->
                let comp = compiledInstanceMember name nr
                match nr.Kind with
                | N.Implementation intf ->
                    if nr.Compiled then 
                        res.Implementations.Add((intf, mDef), (comp, addCctorCall typ res nr.Body))
                    else
                        compilingImplementations.Add((typ, intf, mDef), (toCompilingMember nr comp, addCctorCall typ res nr.Body))
                | _ ->
                    if nr.Compiled then 
                        res.Methods.Add(mDef, (comp, addCctorCall typ res nr.Body))
                    else
                        compilingMethods.Add((typ, mDef), (toCompilingMember nr comp, addCctorCall typ res nr.Body))
            | _ -> failwith "Invalid instance member kind"   

        for typ, m, sn in fullyNamedStaticMembers do
            let res = classes.[typ]
            let addr =
                match sn.Split('.') with
                | [||] ->
                    this.AddError(None, SourceError (sprintf "Invalid Name attribute argument on type '%s'" typ.Value.FullName))
                    ["$$ERROR$$"]
                | [| n |] -> 
                    n :: res.Address.Value.Value
                | a -> List.ofArray a
            if not (r.ExactStaticAddress addr) then
                this.AddError(None, NameConflict ("Static member name conflict", sn)) 
            nameStaticMember typ (Address addr) m
              
        for typ in remainingClasses do
            let addr = typ.Value.FullName.Split('.') |> List.ofArray |> List.rev 
            r.ClassAddress(addr, Option.isSome classes.[typ].Address)
            |> setClassAddress typ
        
        let extraClassAddresses = Dictionary()

        let getClassAddress typ =
            match classes.[typ].Address with
            | Some a -> a.Value
            | _ -> 
            match extraClassAddresses.TryFind typ with
            | Some a -> a
            | _ ->
                let a = r.ClassAddress(typ.Value.FullName.Split('.') |> List.ofArray |> List.rev, false).Value    
                extraClassAddresses.Add(typ, a)
                a
                                     
        for KeyValue(typ, ms) in remainingNamedStaticMembers do
            let clAddr = getClassAddress typ
            for m, n in ms do
                let addr = n :: clAddr
                if not (r.ExactStaticAddress addr) then
                    this.AddError(None, NameConflict ("Static member name conflict", addr |> String.concat "."))
                nameStaticMember typ (Address addr) m
           
        // TODO: check nothing is hiding (exclude overrides and implementations)
        for KeyValue(typ, ms) in namedInstanceMembers do
            let pr = r.LookupPrototype typ
            for m, n in ms do
                // TODO: fail on clashes, not on Stubs?
                pr.Add n |> ignore
//                if not (pr.Add n) then
//                    failwith "instance member name collision"
                nameInstanceMember typ n m      

        for KeyValue(typ, ms) in remainingStaticMembers do
            let clAddr = getClassAddress typ
            for m in ms do
                let n = 
                    match m with
                    | M.Constructor _ -> "New"
                    | M.Field (fName, _) -> fName
                    | M.Method (meth, _) -> meth.Value.MethodName
                    | M.StaticConstructor _ -> "$cctor"
                let addr = r.StaticAddress (n :: clAddr)
                nameStaticMember typ addr m

        let resolved = HashSet()
        
        // TODO: add abstract/interface methods even if there are no implementations
        let rec resolveRemainingInstanceMembers typ (cls: ClassInfo) ms =
            if resolved.Add typ then
                let pr = r.LookupPrototype typ
                // inherit members
                match cls.BaseClass with
                | None -> ()
                | Some bTyp ->
                    match classes.Current.TryFind bTyp with
                    | Some bCls ->
                        let bMs =
                            match remainingInstanceMembers.TryFind bTyp with
                            | Some bMs -> bMs 
                            | _ -> []  
                        resolveRemainingInstanceMembers bTyp bCls bMs
                    | _ -> ()
                    pr.UnionWith(r.LookupPrototype bTyp) 
                
                let ms =
                    let nonOverrides, overrides = 
                        ms |> List.partition (fun m ->
                            match m with
                            | M.Method (_, { Kind = N.Override _ }) -> false
                            | _ -> true 
                        )
                    Seq.append nonOverrides overrides

                for m in ms do
                    let name = 
                        match m with
                        | M.Field (fName, _) -> Resolve.getRenamed fName pr
                        | M.Method (mDef, { Kind = N.Instance }) -> 
                            Resolve.getRenamed mDef.Value.MethodName pr
                        | M.Method (mDef, { Kind = N.Override td }) ->
                            let tCls = classes.[td]
                            let smi = 
                                match tCls.Methods.TryFind mDef with
                                | Some (smi, _) -> 
                                    // remove abstract slot
                                    if td = typ then tCls.Methods.Remove mDef |> ignore
                                    smi
                                | _ ->
                                match compilingMethods.[td, mDef] with
                                | NotCompiled smi, _
                                | NotGenerated (_, _, smi), _ -> smi
                            match smi with
                            | Instance n -> n
                            | _ -> failwith "Abstract methods must be Instance"
                        | _ -> failwith "Invalid instance member kind"
                    //let name = Resolve.getRenamed n pr
                    nameInstanceMember typ name m

//            if typ.Value.FullName = "WebSharper.Web.Control" then 
//                ()

        for KeyValue(typ, ms) in remainingInstanceMembers do
            resolveRemainingInstanceMembers typ classes.[typ] ms    

type RemoteMethods = IDictionary<MethodHandle, TypeDefinition * Method>

let getRemoteMethods meta =
    let remotes = Dictionary()
    for KeyValue(cDef, c) in meta.Classes do
        for KeyValue(mDef, (m, _)) in c.Methods do
            match ignoreMacro m with
            | Remote (scope, kind, handle) ->
                remotes.Add(handle, (cDef, mDef))
            | _ -> ()
    remotes :> RemoteMethods

//let getRequires (comp: Metadata) (nodes: seq<Node>) =
//    [] : Resources.IResource list
//    nodes |> Seq.collect (fun n -> n.Requires) |> Seq.distinct
//    |> Seq.map (fun td -> System.Type.GetType(td.Value.AssemblyQualifiedName).GetConstructor([||]).Invoke([||]) :?> Resources.IResource)
//    |> List.ofSeq

let lookupClassM meta typ =                                 
    try meta.Classes.[typ]
    with _ -> 
        let classNames = meta.Classes.Keys |> Seq.map (fun c -> c.Value.FullName + ", " + c.Value.Assembly) |> Array.ofSeq |> Array.sort
        failwithf "Failed to find translation or proxy for class: %s" typ.Value.AssemblyQualifiedName          

let lookupFieldM meta typ field =
    let t = lookupClassM meta typ
    try t.Fields.[field]                                          
    with _ -> failwithf "Failed to find translation or proxy of field: %s.%s" typ.Value.FullName field

let tryLookupClass meta typ = meta.Classes.TryFind(typ)

module IO =
    open WebSharper.Core
    module B = WebSharper.Core.Binary

    let MetadataEncoding =
        try
            let eP = B.EncodingProvider.Create()
            eP.DeriveEncoding typeof<Metadata>
        with B.NoEncodingException t ->
            failwith "Failed to create binary encoder for type %s" t.FullName

    let Decode stream = MetadataEncoding.Decode stream :?> Metadata   
    let Encode stream (comp: Compilation) = MetadataEncoding.Encode stream comp

    let LoadReflected(a: System.Reflection.Assembly) =
        if a.FullName.StartsWith "System" then None else
            let n = "WebSharper.meta"
            if Array.exists ((=) n) (a.GetManifestResourceNames()) then
                use s = a.GetManifestResourceStream n
                try
                    Some (Decode s)
                with _ ->
                    failwithf "Failed to load metadata for: %s" a.FullName
            else
                None
