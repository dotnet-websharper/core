module WebSharper.Core.Metadata

open System.Collections.Generic

open WebSharper.Core.AST

type Dependencies = ISet<Address>
type Requires = ISet<TypeDefinition>                 

type FieldInfo =
    | InstanceField of string
    | OptionalInstanceField of string
    | StaticField of Address
    | OptionalStaticField of Address 

type RemotingKind =
    | RemoteAsync
    | RemoteSend
    | RemoteSync

type MethodHandle =
    {
        Assembly : string
        Code : int
    }

type NodeInfo =
    | Instance of string
    | Static of Address
    | Constructor of Address
//    | FinalInline of Expression
    | Inline
    | Macro of TypeDefinition * option<obj> * option<NodeInfo> 
    | NotCompiled of NodeInfo 
//    | NotParsed of string * list<string> //* obj
//    | NotCompiledCtor of Address
    | NotGenerated of TypeDefinition * option<obj>
    | Prototype of TypeDefinition
    | RemoteInstance of RemotingKind * MethodHandle
    | RemoteStatic of RemotingKind * MethodHandle

type Node =
    {
        mutable Info: NodeInfo
        mutable Body : Expression
        Dependencies: ISet<Address>
        Requires: ISet<TypeDefinition> 
    }

//type Body =
//    | NotCompiled of obj * Dependencies * Requires
//    | NotParsed of string * obj * Requires
//    | NotCompiledConstructor of obj * Address * Dependencies * Requires
//    | CompiledExpr of Expression
//    | CompiledStatement of Statement

//type CompiledConstructor =
//    | CompiledInstance of string * Dependencies * Requires
//    | CompiledStatic of Address * Dependencies * Requires
//    | InlineConstructor of Expression * Dependencies * Requires
//    | MacroConstructor of 
//
//type Constructor =
//    | CompiledConstructor of Address * Body
//    | InlineConstructor of Body * Dependencies * Requires
//    | MacroConstructor of TypeDefinition * option<Constructor>
//    | StubConstructor
//
//type StaticConstructor =
//    | CompiledStaticConstructor of Address * Body
//
//type Method =
//    | CompiledInstanceMethod of string * Body
//    | CompiledStaticMethod of Address * Body
//    | InlineMethod of Body * Dependencies * Requires
//    | MacroMethod of TypeDefinition * option<Method>
//    | StubMethod of string
//
//type Member =
//    | Method of Method
//    | Constructor of Constructor
//    | StaticConstructor of StaticConstructor
     
//type Property =
//    | BasicProperty of option<Method> * option<Method>
//    | InstanceOptProperty of string
//    | StaticOptProperty of list<string>
//    | FieldProperty of int
//    | InstanceStubProperty of string
//    | InterfaceProperty of string
//    | StaticStubProperty of list<string>

type Class =
    {
//        CurrentAssembly : bool
        Address : option<Address>
        BaseClass : option<TypeDefinition>
        FieldNames : Set<string>
        Prototype : option<string>
        Constructors : IDictionary<Constructor, Node>
        Fields : IDictionary<string, FieldInfo>
        StaticConstructor : option<Node>
        Methods : IDictionary<Method, Node>
        Interfaces : list<TypeDefinition> 
//        Properties : IDictionary<DotNet.Property, option<Method> * option<Method>>
    }

let rewrite (dict: IDictionary<_,_>) key value =
    dict.[key] <- value

type Interface =
    {
        Extends : list<TypeDefinition>
        MethodNames : IDictionary<Method, string>
    }
//
//type UnionCaseKind =
//    | BasicUnionCase of int
//    | CompiledUnionCase of list<string> * int
//    | ConstantUnionCase of Literal

//type Translated =
////    | Resource of DotNet.Type
//    | Prototype of TypeDefinition * IDictionary<string, ref<Body>> * Dependencies * Requires
//    | Content of ref<Body> * Dependencies * Requires

//type Module =
//    {
//        Classes : list<Class>
//        Functions : list<Method>
//    }


type Compilation =
    {
        Interfaces : IDictionary<TypeDefinition, Interface>
        Classes : IDictionary<TypeDefinition, Class>
        Proxies : IDictionary<TypeDefinition, TypeDefinition>
        RemoteMethods : IDictionary<MethodHandle, TypeDefinition * Method>
        Translated : IDictionary<Address, Node>
    }

let findProxied assembly typ =
    match assembly.Proxies.TryGetValue typ with
    | true, t -> t
    | _ -> typ   

let tryLookupClass assembly typ =                                 
    assembly.Classes.TryFind(findProxied assembly typ)

let lookupClass assembly typ =                                 
    try assembly.Classes.[findProxied assembly typ]
    with _ -> 
//        let classNames = assembly.Classes.Keys |> Seq.map (fun c -> c.Value.FullName + ", " + c.Value.Assembly) |> Array.ofSeq |> Array.sort
        failwithf "Failed to find translation or proxy for class: %s" typ.Value.FullName          

let lookupInterface assembly typ =
    try assembly.Interfaces.[typ]
    with _ -> 
        failwithf "Failed to find translation or proxy for interface: %s" typ.Value.FullName          

let lookupConstructor assembly typ constructor_ =
    let t = lookupClass assembly typ
    try t.Constructors.[constructor_]                                          
    with _ -> failwithf "Failed to find translation or proxy of constructor for type: %s" typ.Value.FullName

let lookupStaticConstructor assembly typ =
    let t = lookupClass assembly typ
    try t.StaticConstructor.Value                              
    with _ -> failwithf "Failed to find translation or proxy of constructor for type: %s" typ.Value.FullName

let printSignature (meth: Method) =
    match meth.Value.Parameters with
    | [] -> "unit"
    | ps -> ps |> Seq.map string |> String.concat " * "
    + " -> " + string meth.Value.ReturnType
    
let lookupClassMethod assembly typ meth =
    let t = lookupClass assembly typ
    try t.Methods.[meth]                                          
    with _ ->
        failwithf "Failed to find translation or proxy of method: %s.%s with signature %s"
            typ.Value.FullName meth.Value.MethodName (printSignature meth)

let lookupInterfaceMethodName assembly typ meth =
    let t = lookupInterface assembly typ
    try t.MethodNames.[meth]
    with _ ->
        failwithf "Failed to find translation or proxy of interface method: %s.%s with signature %s"
            typ.Value.FullName meth.Value.MethodName (printSignature meth)

let lookupField assembly typ field =
    let t = lookupClass assembly typ
    try t.Fields.[field]                                          
    with _ -> failwithf "Failed to find translation or proxy of field: %s.%s" typ.Value.FullName field

let union dep main =
    let dictUnion (a: IDictionary<_,_>) (b: IDictionary<_,_>) =
        if a.Count = 0 then b
        elif b.Count = 0 then a
        else
            dict (Seq.append a b |> Seq.distinctBy (fun x -> x.Key) |> Seq.map (fun (KeyValue(k, v)) -> k, v))        
    {
        Interfaces = dictUnion main.Interfaces dep.Interfaces 
        Classes = dictUnion main.Classes dep.Classes
        Proxies = dictUnion main.Proxies dep.Proxies
        RemoteMethods = dict []
        Translated = dictUnion main.Translated dep.Translated 
    }      
