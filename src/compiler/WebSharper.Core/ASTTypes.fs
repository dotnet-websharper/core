namespace WebSharper.Core.AST

open WebSharper.Core

[<CustomEquality; CustomComparison; Struct>]
[<System.Diagnostics.DebuggerDisplay("{Value}")>]
type Hashed<'T when 'T : equality and 'T : comparison> =
    val Value : 'T
    val Hash : int 

    new v = { Value = v; Hash = hash v }

    override this.GetHashCode() =
        this.Hash

    override this.ToString() = this.Value.ToString()
    
    override this.Equals(other: obj) : bool =
        match other with
        | :? Hashed<'T> as o ->
            obj.ReferenceEquals(this, o) || (
                this.Hash = o.Hash && (
                    let v1 = this.Value
                    let v2 = o.Value
                    obj.ReferenceEquals(v1, v2) || v1 = v2
                )
            )
        | _ -> failwith "invalid equality check"

    interface System.IComparable with
        member this.CompareTo (other: obj) =
            match other with
        | :? Hashed<'T> as o ->
            compare this.Value o.Value
        | _ -> failwith "invalid comparison"

type Id(?name: string) =
    static let mutable lastId = -1L

#if DEBUG
    do
        match name with
        | Some n when System.String.IsNullOrEmpty n ->
            failwith "Identifier name cannot be empty."
        | _ -> ()
#endif

    let id = System.Threading.Interlocked.Increment(&lastId)

    member this.Name = name
    member this.Id = id

    override this.GetHashCode() = int id
    
    override this.Equals other =
        match other with
        | :? Id as o -> id = o.Id
        | _ -> false

    interface System.IComparable with
        member this.CompareTo other =
            match other with
            | :? Id as o -> compare id o.Id
            | _ -> invalidArg "other" "Invalid comparison."

    override this.ToString() =
        Option.toObj name + "$" + string id

type SourcePos =
    {
        FileName : string
        Start : int * int
        End : int * int
    }

type MutatingBinaryOperator =
    | ``=``    = 0
    | ``+=``   = 1
    | ``-=``   = 2
    | ``*=``   = 3
    | ``/=``   = 4
    | ``%=``   = 5
    | ``&=``   = 6
    | ``^=``   = 7
    | ``|=``   = 8
    | ``<<=``  = 9
    | ``>>=``  = 10
    | ``>>>=`` = 11 //unsigned right shift - JS only

type MutatingUnaryOperator =
    | ``++()`` = 0
    | ``()++`` = 1
    | ``--()`` = 2
    | ``()--`` = 3
    | delete = 4

type BinaryOperator =
    | ``!==``        = 0
    | ``!=``         = 1
    | ``%``          = 2
    | ``&&``         = 3
    | ``&``          = 4
    | ``*``          = 5
    | ``+``          = 6
    | ``,``          = 7
    | ``-``          = 8
    | ``.``          = 9
    | ``/``          = 10
    | ``<<``         = 11
    | ``<=``         = 12
    | ``<``          = 13
    | ``===``        = 14
    | ``==``         = 15
    | ``=``          = 16
    | ``>=``         = 17
    | ``>>>``        = 18
    | ``>>``         = 19
    | ``>``          = 20
    | ``^``          = 21
    | ``in``         = 22
    | instanceof = 23
    | ``|``          = 24
    | ``||``         = 25

type UnaryOperator =
    | ``!`` = 0
    | ``void`` = 1
    | ``+`` = 2
    | ``-`` = 3
    | ``~`` = 4
    | typeof = 5

[<System.Diagnostics.DebuggerDisplay("{Assembly}.{FullName}")>]
type TypeDefinitionInfo =
    {
        Assembly : string
        FullName : string
    }
    member this.AssemblyQualifiedName =
        this.FullName + ", " + this.Assembly
        
    override this.ToString() = this.FullName            

type TypeDefinition = Hashed<TypeDefinitionInfo>

type Concrete<'T> =
    {
        Generics : list<Type>
        Entity : 'T
    }

and Type =
    | ConcreteType of Concrete<TypeDefinition>
    | GenericType of int
    | ArrayType of Type * int
    | TupleType of list<Type>
    | FSharpFuncType of Type * Type
//    | ReferenceType of Type

    override this.ToString() =
        match this with
        | ConcreteType t -> 
            string t.Entity +
                match t.Generics with
                | [] -> ""
                | gs -> "<" + (gs |> Seq.map string |> String.concat ", ") + ">"
        | GenericType i -> "'T" + string i
        | ArrayType (t, a) -> string t + "[" + String.replicate (a - 1) "," + "]"
        | TupleType ts -> "(" + (ts |> Seq.map string |> String.concat " * ") + ")"
        | FSharpFuncType (a, r) -> string a + " -> " + string r

    member this.AssemblyQualifiedName =
        match this with
        | ConcreteType t ->
            t.Entity.Value.AssemblyQualifiedName +
                match t.Generics with
                | [] -> ""
                | gs -> "[[" + String.concat "],[" (gs |> Seq.map (fun g -> g.AssemblyQualifiedName)) + "]]"
        | GenericType _ ->
            failwith "Cannot get AssemblyQualifiedName of a type parameter"
        | ArrayType (t, i) ->
            t.AssemblyQualifiedName + "[" + String.replicate (i - 1) "," + "]"  
        | TupleType ts ->
            let rec getName l (ts: List<Type>) =
                if l <= 7 then
                    "System.Tuple`" + (string l) + " [[" + String.concat "],[" (ts |> Seq.map (fun g -> g.AssemblyQualifiedName)) + "]], mscorlib"
                else
                    "System.Tuple`8[[" + 
                        String.concat "],[" (ts |> Seq.take 7 |> Seq.map (fun g -> g.AssemblyQualifiedName)) + 
                        getName (l - 7) (ts |> Seq.skip 7 |> List.ofSeq) + "]], mscorlib"
            getName (List.length ts) ts
        | FSharpFuncType (a, r) ->
            "Microsoft.FSharp.Core.FSharpFunc`2[[" + a.AssemblyQualifiedName + "],[" + r.AssemblyQualifiedName + "]], FSharp.Core"
        
//type Special =
//    | SeqAppendWithDelay of 
//    | SeqBreak of Id
//    | SeqContinue of Id

[<System.Diagnostics.DebuggerDisplay("{MethodName}")>]
type MethodInfo =
    {
        MethodName : string
        Parameters : list<Type>
        ReturnType : Type
        Generics : int       
    }

type Method = Hashed<MethodInfo>

type ConstructorInfo =
    {
        CtorParameters : list<Type>    
    }

type Constructor = Hashed<ConstructorInfo>

type Member =
    | Method of Method
    | Constructor of Constructor
    | StaticConstructor

//[<System.Diagnostics.DebuggerDisplay("{FieldName}")>]
//type FieldInfo =
//    {
//        FieldName : string
//        Type : Type
//    }
//
//type Field = Hashed<FieldInfo>

//type PropertyInfo =
//    {
//        PropertyName : string
//        Type : Type
//        Parameters : list<Type> 
//    }
//
//type Property = Hashed<PropertyInfo>

type UnionCaseInfo =
    {
        UnionCaseName : string 
    }

type UnionCase = Hashed<UnionCaseInfo>

type Address = Hashed<list<string>>

module Reflection = 
    type private FST = FSharp.Reflection.FSharpType

    let getTypeDefinition (t: System.Type) =
        if t.IsArray || FST.IsFunction t || FST.IsTuple t then
            failwithf "Not a simple type: %A" t
        else
            Hashed {
                Assembly = t.Assembly.FullName
                FullName = t.FullName
            } 

    let rec getType (t: System.Type) =
        if t.IsArray then
            ArrayType (getType(t.GetElementType()), t.GetArrayRank())
        elif FST.IsFunction t then
            let a, r = FST.GetFunctionElements t
            FSharpFuncType(getType a, getType r)        
        elif FST.IsTuple t then
            TupleType(FST.GetTupleElements t |> Seq.map getType |> List.ofSeq) 
        elif t.IsGenericParameter then
            GenericType t.GenericParameterPosition
        else
            ConcreteType {
                Generics = 
                    if t.IsGenericType then 
                        t.GetGenericArguments() |> Seq.map getType |> List.ofSeq 
                    else [] 
                Entity = Hashed {
                    Assembly = t.Assembly.FullName
                    FullName = t.FullName
                } 
            }

    let getMethod (i : System.Reflection.MethodInfo) =
        {
            MethodName = i.Name
            Parameters = i.GetParameters() |> Seq.map (fun p -> getType p.ParameterType) |> List.ofSeq
            ReturnType = getType i.ReturnType 
            Generics   = if i.IsGenericMethod then i.GetGenericArguments().Length else 0
        }

    let loadType (t: Type) =
        System.Type.GetType(t.AssemblyQualifiedName, true)   

    let loadTypeNonGeneric t = 
        let rec checkNonGeneric t =
            match t with
            | ArrayType (t, _) -> checkNonGeneric t
            | ConcreteType c ->
                for t in c.Generics do checkNonGeneric t
            | GenericType _ ->
                failwith "Cannot load generic type"    
        checkNonGeneric t
        loadType t 

    let loadTypeDefinition (td: TypeDefinition) =
        System.Type.GetType(td.Value.AssemblyQualifiedName, true)   

    let loadMethod td (m: Method) =
        let m = m.Value
        (loadTypeDefinition td).GetMethods()
        |> Seq.find (fun i -> i.Name = m.MethodName && getMethod i = m)
        