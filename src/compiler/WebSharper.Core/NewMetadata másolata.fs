module WebSharper.Core.NewMetadata

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

type CompiledNodeInfo =
    | Instance of string
    | Static of Address
    | Constructor of Address
    | Inline
    | Macro of TypeDefinition * option<ParameterObject> * option<CompiledNodeInfo> 
    | Remote of MemberScope * RemotingKind * MethodHandle

type CompilingNodeInfo =
    | Compiled of CompiledNodeInfo    
    | NotCompiled of CompiledNodeInfo
    | NotGenerated of TypeDefinition * option<ParameterObject> * CompiledNodeInfo

[<RequireQualifiedAccess>]
type NotResolvedNodeKind = 
    | Instance of bool * string
    | Static of bool * string    
    | Constructor of bool * string
    | Override of TypeDefinition
    | Implementation of TypeDefinition
    | Remote of MemberScope * RemotingKind * MethodHandle

type NodeKind =
    | MethodNode of TypeDefinition * Method
    | ConstructorNode of TypeDefinition * Constructor
    | AbstractMethodNode of Method
    | TypeNode of TypeDefinition
    | ResourceNode of TypeDefinition
    | AssemblyNode of string 
    | External of NodeKind

//type CompiledNode =
//    {
//        Info : CompiledNodeInfo
//        Body : Expression
//    }

type CompilingNode =
    {
        mutable Info : CompilingNodeInfo
        mutable Body : Expression 
    }

type NotResolvedNode =
    {
        Kind : NotResolvedNodeKind
        Macro : option<TypeDefinition * option<ParameterObject>>
        Generator : option<TypeDefinition * option<ParameterObject>>
        Compiled : bool
        Body : Expression
    }

type CompilingClassAddress =
    | ClassAddress of option<Address>
    | NotResolvedClassAddress of bool * string

type ClassNode =
    {
        Address : option<Address>
        BaseClass : option<TypeDefinition>
//        FieldNames : ISet<string>
        HasPrototype : bool
        Constructors : IDictionary<Constructor, CompiledNodeInfo * Expression>
        Fields : IDictionary<string, string * bool> // isOptional
        StaticConstructor : option<CompiledNodeInfo * Expression>
        Methods : IDictionary<Method, CompiledNodeInfo * Expression>
        Implementations : IDictionary<Method * TypeDefinition, CompiledNodeInfo * Expression>
    }

[<RequireQualifiedAccess>]
type CompilingClassNode =
    {
        mutable CompilingAddress : CompilingClassAddress
        BaseClass : option<TypeDefinition>
//        mutable FieldNames : ISet<string>
//        FieldNames : Set<string>
        HasPrototype : bool
        Constructors : IDictionary<Constructor, CompilingNode>
        Fields : IDictionary<string, option<string> * bool>
        StaticConstructor : option<CompilingNode>
        Methods : IDictionary<Method, CompilingNode>
        Implementations : IDictionary<TypeDefinition * Method, CompilingNode>
    }

[<RequireQualifiedAccess>]
type NotResolvedClassNode =
    {
        mutable CompilingAddress : CompilingClassAddress
        BaseClass : option<TypeDefinition>
//        mutable FieldNames : ISet<string>
//        FieldNames : Set<string>
        HasPrototype : bool
        Constructors : list<Constructor * NotResolvedNode>
        Fields : list<string * option<string> * bool>
        StaticConstructor : option<NotResolvedNode>
        Methods : list<Method * NotResolvedNode>
//        Implementations : IDictionary<TypeDefinition * Method, NotResolvedNode>
    }


type InterfaceNode =
    {
        Extends : list<TypeDefinition>
        MethodNames : IDictionary<Method, string>
    }

//type NotResolvedInterfaceMethodName =
//    | InterfaceMethodName of string
//    | NotResolvedInterfaceMethodName of bool * string //* SourcePos 

type NotResolvedInterfaceNode =
    {
        Extends : list<TypeDefinition>
        mutable CompilingMethodNames : list<Method * bool * string>
    }

type GraphData<'T> =
    {
        Nodes : 'T[]
        Edges : 'T[][]
    }
            
type Graph<'T> =
    {
        Data : GraphData<'T>
        Lookup : IDictionary<'T, int>
    }

    static member FromData data =
        {
            Data = data
            Lookup = dict (data.Nodes |> Seq.mapi (fun i n -> n, i))
        }

type MetadataInfo =
    {
        SiteletDefinition: option<TypeDefinition>
        Dependencies : GraphData<NodeKind>
        Interfaces : IDictionary<TypeDefinition, InterfaceNode>
        Classes : IDictionary<TypeDefinition, ClassNode>
        Proxies : IDictionary<TypeDefinition, TypeDefinition>
    }

module B = WebSharper.Core.Binary

let MetadataEncoding =
    try
        let eP = B.EncodingProvider.Create()
        eP.DeriveEncoding typeof<MetadataInfo>
    with B.NoEncodingException t ->
        failwith "Failed to create binary encoder for type %s" t.FullName

type Metadata internal (info: MetadataInfo) =
    member internal this.Info = info
    
    member this.Encode (w: System.IO.BinaryWriter) = MetadataEncoding.Encode (downcast w) info
    static member Decode (r : System.IO.BinaryReader) = Metadata(downcast MetadataEncoding.Decode (downcast r))

exception CompilationError of message: string * position: SourcePos
    with override this.Message = this.message    

type Compilation(meta: Metadata, interfaces, classes, proxies) =
    let metaInfo = meta.Info

    let proxied typ = Dict.tryFindSameIn2 metaInfo.Proxies proxies typ 
    
    member val SiteletDefinition: option<TypeDefinition> = None with get, set
    member val AssemblyName = "EntryPoint" with get, set

//    member val Interfaces = MergedDictionary(metaInfo.Interfaces) with get
//    member val Classes : MergedDictionary<TypeDefinition, CompilingClassAddress * > with get, set
//    member val Proxies : MergedDictionary<TypeDefinition, TypeDefinition>        with get, set
    
//    member this.GetInterfaceMethodName (typ, meth) =

type ResolveError =
    | NameConflict of string * SourcePos
                
type CompilationStart(meta: Metadata) =    
    let metaInfo = meta.Info

    let nrInterfaces = Dictionary()
    let nrClasses = Dictionary()

    let interfaces = MergedDictionary(metaInfo.Interfaces)
    let classes = MergedDictionary(metaInfo.Classes)
    let proxies = MergedDictionary(metaInfo.Proxies)

    member this.AddProxy(tProxy, tTarget) =
        proxies.Add(tProxy, tTarget)  

    member this.AddClass(typ, cls) =
        nrClasses.Add(typ, cls)

    member this.AddInterface(typ, intf) =
        nrInterfaces.Add(typ, intf)

    member this.Resolve() =
        let rec resolveInterface typ (nr: NotResolvedInterfaceNode) =
            let allMembers = HashSet()
            
            let rec addInherited (n: InterfaceNode) =
                for i in n.Extends do
                    addInherited interfaces.[i]
                for m in n.MethodNames.Values do
                    if not (allMembers.Add m) then
                        failwith "Interface method name collision."
            
            for i in nr.Extends do
                nrInterfaces.TryFind i |> Option.iter (resolveInterface i)       
                addInherited interfaces.[i]
//                match interfaces.TryFind i with
//                | Some n -> addInherited n
//                | _ -> failwith "Interface declaration or proxy not found."
            
            let res = Dictionary()
            
            let strongNamed, others = 
                nr.CompilingMethodNames |> List.partition (fun (_, n, _) -> n)
                
            for m, _, n in strongNamed do
                if not (allMembers.Add n) then
                    failwith "Explicitly declared interface method name collision."
                res.Add(m, n)

            for m, _, n in others do
                let n = Resolve.addRenamed n allMembers
                res.Add(m, n)

            let resNode =
                {
                    Extends = nr.Extends
                    MethodNames = res
                }
            interfaces.Add(typ, resNode)
            nrInterfaces.Remove typ |> ignore
                    
        while nrInterfaces.Count > 0 do
            let (KeyValue(typ, nr)) = Seq.head nrInterfaces  
            resolveInterface typ nr

        let globalStatics
        
        let rec resolveClass typ (nr: NotResolvedClassNode) =
            let allInstanceMembers = HashSet()
            let add n = allInstanceMembers.Add n |> ignore
            let addChecked n =
                if not (allInstanceMembers.Add n) then
                    failwith "Class member name collision."

            let allStaticMembers = HashSet()
            let addS n = allStaticMembers.Add n |> ignore
            let addSChecked n =
                if not (allStaticMembers.Add n) then
                    failwith "Class member name collision."

            let rec addInherited (n: ClassNode) =
                n.BaseClass |> Option.iter (fun b -> addInherited classes.[b])
                for f, _ in n.Fields.Values do add f
                for m, _ in Seq.append n.Methods.Values n.Implementations.Values do
                    match m with
                    | Instance f -> add f
                    | _ -> ()

            nr.BaseClass |> Option.iter (fun b ->
                nrClasses.TryFind b |> Option.iter (resolveClass b)  
                addInherited classes.[b]
            )

            let resFields = Dictionary()
            let resMethods = Dictionary()
            let resConstructors = Dictionary()
            let resImplementations = Dictionary()

            let node b c i = 
                { 
                    Info = if c then Compiled i else NotCompiled i
                    Body = b     
                }

            if nr.HasPrototype then
                addS "prototype"

            // resolving overload and implee
//            for k, m in nr.Constructors do
//                match m.Kind with
            
            for k, m in nr.Methods do
                match m.Kind with        
                | NotResolvedNodeKind.Override ot ->
                    match classes.[ot].Methods.[k] with
                    | Instance mn, _ -> 
                        add mn
                        resMethods.Add(k, node m.Body m.Compiled (Instance mn))
                    | _ -> failwith "Override found for not an instance member"
                | NotResolvedNodeKind.Implementation it ->
                    let mn = interfaces.[it].MethodNames.[k]
                    add mn 
                    resMethods.Add(k, node m.Body m.Compiled (Instance mn))
                | NotResolvedNodeKind.Remote (scope, kind, handle) ->
                    resMethods.Add(k, node m.Body m.Compiled (Remote(scope, kind, handle)))
                | _ -> ()

            for k, m in nr.Constructors do
                match m.Kind with        
                | NotResolvedNodeKind.Constructor (true, mn) ->
                    addSChecked mn
                    resConstructors.Add(k, node m.Body m.Compiled (Constructor mn))
                | _ -> ()

            let hasCCtor = Option.isSome nr.StaticConstructor

            for k, m in nr.Methods do
                match m.Kind with
                | NotResolvedNodeKind.Instance (true, mn) ->
                    addChecked mn
                    resMethods.Add(k, node m.Body m.Compiled (Instance mn))
                | NotResolvedNodeKind.Static (true, mn) ->
                    
                    addSChecked mn
                    //if hasCCtor then
                        resMethods.Add(k, node m.Body m.Compiled (Static mn))
                | _ -> ()

            for k, m in nr.Methods do
                match m.Kind with
                | NotResolvedNodeKind.Instance (false, mn) ->
                    let mn = Resolve.addRenamed mn allInstanceMembers
                    resMethods.Add(k, node m.Body m.Compiled (Instance mn))
                | NotResolvedNodeKind.Static (false, mn) ->
                    let mn = Resolve.addRenamed mn allStaticMembers
                    resMethods.Add(k, node m.Body m.Compiled (Static mn))


            


//            let res = Diction


            ()

        while nrClasses.Count > 0 do 
            let (KeyValue(typ, nr)) = Seq.head nrClasses  
            resolveClass typ nr
                
            
        
type ICompilation