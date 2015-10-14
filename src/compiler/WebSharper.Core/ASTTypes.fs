namespace WebSharper.Core.AST

open WebSharper.Core

module Ids =
    let lastId = ref -1L
    
[<CustomComparison; CustomEquality>]
type Id =
    {
        Name : string option
        Id: int64
    }

    static member New(?name) =
        {
            Name = name
            Id = 
                Ids.lastId := !Ids.lastId + 1L
                !Ids.lastId
        }

    override this.GetHashCode() = int this.Id
    
    override this.Equals other =
        match other with
        | :? Id as o -> this.Id = o.Id
        | _ -> false

    interface System.IComparable with
        member this.CompareTo other =
            match other with
            | :? Id as o -> compare this.Id o.Id
            | _ -> invalidArg "other" "Invalid comparison."

    override this.ToString() =
        Option.toObj this.Name + "$" + string this.Id

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
    | instanceof     = 23
    | ``|``          = 24
    | ``||``         = 25

type UnaryOperator =
    | ``!`` = 0
    | ``void`` = 1
    | ``+`` = 2
    | ``-`` = 3
    | ``~`` = 4
    | typeof = 5

//[<System.Diagnostics.DebuggerDisplay("{Assembly}.{FullName}")>]
type TypeDefinitionInfo =
    {
        Assembly : string
        FullName : string
//        IsInterface : bool
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
    | VoidType
//    | ReferenceType of Type

    override this.ToString() =
        match this with
        | ConcreteType t -> 
            string t.Entity.Value +
                match t.Generics with
                | [] -> ""
                | gs -> "<" + (gs |> Seq.map string |> String.concat ", ") + ">"
        | GenericType i -> "'T" + string i
        | ArrayType (t, a) -> string t + "[" + String.replicate (a - 1) "," + "]"
        | TupleType ts -> "(" + (ts |> Seq.map string |> String.concat " * ") + ")"
        | FSharpFuncType (a, r) -> string a + " -> " + string r
        | VoidType -> "unit"

    member this.AssemblyQualifiedName =
        let combine (n, a) = n + ", " + a
        let rec getNameAndAsm ty =
            match ty with
            | ConcreteType t ->
                let en, ea = 
                    let e = t.Entity.Value
                    e.FullName, e.Assembly
                en +
                    match t.Generics with
                    | [] -> ""
                    | gs -> "[[" + String.concat "],[" (gs |> Seq.map (fun g -> g.AssemblyQualifiedName)) + "]]"
                , ea
            | GenericType _ ->
                failwith "Cannot get AssemblyQualifiedName of a type parameter"
            | ArrayType (t, i) ->
                let tn, ta = getNameAndAsm t
                tn + "[" + String.replicate (i - 1) "," + "]", ta
            | TupleType ts ->
                let rec getName l (ts: List<Type>) =
                    if l <= 7 then
                        "System.Tuple`" + (string l) + "[[" + String.concat "],[" (ts |> Seq.map (fun g -> g.AssemblyQualifiedName)) + "]]"
                    else
                        "System.Tuple`8[[" + 
                            String.concat "],[" (ts |> Seq.take 7 |> Seq.map (fun g -> g.AssemblyQualifiedName)) + 
                            getName (l - 7) (ts |> Seq.skip 7 |> List.ofSeq) + "]]"
                getName (List.length ts) ts, "mscorlib"
            | FSharpFuncType (a, r) ->
                "Microsoft.FSharp.Core.FSharpFunc`2[[" + a.AssemblyQualifiedName + "],[" + r.AssemblyQualifiedName + "]]", "FSharp.Core"
            | VoidType -> "System.Void", "mscorlib"
        getNameAndAsm this |> combine
        
//module SpecialTypes =  
//    let Unit = 
//        ConcreteType {
//            Generics = []
//            Entity = Hashed { Assembly = "FSharp.Core"; FullName = "Microsoft.FSharp.Core.Unit" }
//        }

//type Special =
//    | SeqAppendWithDelay of 
//    | SeqBreak of Id
//    | SeqContinue of Id

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

[<RequireQualifiedAccess>]
type Member =
    | Method of isInstance:bool * Method
    | Implementation of TypeDefinition * Method
    | Override of TypeDefinition * Method
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
    type private FST = Microsoft.FSharp.Reflection.FSharpType

    let private getTypeDefinitionUnchecked fullAsmName (t: System.Type) =
        let rec getName (t: System.Type) =
            if t.IsNested then
                getName t.DeclaringType + "+" + t.Name 
            else t.Namespace + "." + t.Name         
        Hashed {
            Assembly = if fullAsmName then t.Assembly.FullName else t.Assembly.FullName.Split(',').[0]
            FullName = getName t
        } 

    let getTypeDefinition (t: System.Type) =
        if t.IsArray || FST.IsFunction t || FST.IsTuple t then
            failwithf "Not a simple type: %A" t
        else
            getTypeDefinitionUnchecked false t

    let unitTy = typeof<unit>
    let voidTy = typeof<System.Void>

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
        elif t = voidTy || t = unitTy then
            VoidType
        else
            ConcreteType {
                Generics = 
                    if t.IsGenericType then 
                        t.GetGenericArguments() |> Seq.map getType |> List.ofSeq 
                    else [] 
                Entity = getTypeDefinitionUnchecked false t
            }

    // used by WIG
    let rec getTypeWithFullAsmNames (t: System.Type) =
        if t.IsArray then
            ArrayType (getTypeWithFullAsmNames(t.GetElementType()), t.GetArrayRank())
        elif FST.IsFunction t then
            let a, r = FST.GetFunctionElements t
            FSharpFuncType(getTypeWithFullAsmNames a, getTypeWithFullAsmNames r)        
        elif FST.IsTuple t then
            TupleType(FST.GetTupleElements t |> Seq.map getTypeWithFullAsmNames |> List.ofSeq) 
        elif t.IsGenericParameter then
            GenericType t.GenericParameterPosition
        else
            ConcreteType {
                Generics = 
                    if t.IsGenericType then 
                        t.GetGenericArguments() |> Seq.map getTypeWithFullAsmNames |> List.ofSeq 
                    else [] 
                Entity = getTypeDefinitionUnchecked true t
            }

    let getMethod (i : System.Reflection.MethodInfo) =
        {
            MethodName = i.Name
            Parameters = i.GetParameters() |> Seq.map (fun p -> getType p.ParameterType) |> List.ofSeq
            ReturnType = getType i.ReturnType 
            Generics   = if i.IsGenericMethod then i.GetGenericArguments().Length else 0
        }

    let loadType (t: Type) =
        try System.Type.GetType(t.AssemblyQualifiedName, true)  
        with _ -> failwithf "Failed to load type '%s'" t.AssemblyQualifiedName

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

    let printMethod (m: MethodInfo) =
        sprintf "(%s%s : %s -> %O)"
            m.MethodName 
            (if m.Generics > 0 then "<" + (Seq.init m.Generics (fun _ -> "_") |> String.concat ",") + ">" else "")
            (m.Parameters |> Seq.map string |> String.concat " * ") 
            m.ReturnType

    let loadMethod td (m: Method) =
        let m = m.Value
        let methodInfos = (loadTypeDefinition td).GetMethods()
        try
            methodInfos
            |> Seq.find (fun i -> i.Name = m.MethodName && getMethod i = m)
        with _ ->
            failwithf "Could not load method %s candidates: %A" (printMethod m) (methodInfos |> Seq.choose (fun c -> 
                let mc = getMethod c
                if mc.MethodName = m.MethodName then Some (printMethod mc) else None
            ) |> Array.ofSeq)
        