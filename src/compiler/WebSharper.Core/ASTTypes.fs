// $begin{copyright}
//
// This file is part of WebSharper
//
// Copyright (c) 2008-2016 IntelliFactory
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

namespace WebSharper.Core.AST

open WebSharper.Core

type private Ids() =
    static let mutable lastId = -1L
    static member New() =
        System.Threading.Interlocked.Increment(&lastId)

[<CustomComparison; CustomEquality>]
/// An identifier for a variable or label.
type Id =
    private {
        mutable IdName : string option
        Id: int64
        Mutable : bool
    }

    member this.Name 
        with get () = this.IdName
        and set n = this.IdName <- n

    member this.IsMutable = this.Mutable
    
    static member New(?name, ?mut) =
        {
            IdName = name
            Id = Ids.New()
            Mutable = defaultArg mut true
        }

    static member Global() =
        {
            IdName = Some "window"
            Id = -1L
            Mutable = false
        }

    member this.Clone() =
        {
            IdName = this.IdName
            Id = Ids.New()
            Mutable = this.Mutable
        }

    member this.ToMutable() =
        {
            IdName = this.IdName
            Id = this.Id
            Mutable = true
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
        (match this.Name with Some n -> n | _ -> "") + "$" + string this.Id + (if this.Mutable then "M" else "")

/// Specifies a curried or tupled F# function argument that is translated to a flat function
type FuncArgOptimization =
    | NotOptimizedFuncArg
    | CurriedFuncArg of int    
    | TupledFuncArg of int    

/// A range in original source code
type SourcePos =
    {
        FileName : string
        Start : int * int
        End : int * int
    }

    override this.ToString() =
        sprintf "%s %A-%A" this.FileName this.Start this.End

type MutatingBinaryOperator =
    | Assign                   = 0
    | AddAssign                = 1
    | SubstractAssign          = 2
    | MultiplyAssign           = 3
    | DivideAssign             = 4
    | ModuloAssign             = 5
    | BitwiseAndAssign         = 6
    | BitwiseXorAssign         = 7
    | BitwiseOrAssign          = 8
    | LeftShiftAssign          = 9
    | RightShiftAssign         = 10
    | UnsignedRightShiftAssign = 11

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]  
module MutatingBinaryOperator =
    let [<Literal>] ``=``    = MutatingBinaryOperator.Assign                  
    let [<Literal>] ``+=``   = MutatingBinaryOperator.AddAssign               
    let [<Literal>] ``-=``   = MutatingBinaryOperator.SubstractAssign         
    let [<Literal>] ``*=``   = MutatingBinaryOperator.MultiplyAssign          
    let [<Literal>] ``/=``   = MutatingBinaryOperator.DivideAssign            
    let [<Literal>] ``%=``   = MutatingBinaryOperator.ModuloAssign            
    let [<Literal>] ``&=``   = MutatingBinaryOperator.BitwiseAndAssign        
    let [<Literal>] ``^=``   = MutatingBinaryOperator.BitwiseXorAssign        
    let [<Literal>] ``|=``   = MutatingBinaryOperator.BitwiseOrAssign         
    let [<Literal>] ``<<=``  = MutatingBinaryOperator.LeftShiftAssign         
    let [<Literal>] ``>>=``  = MutatingBinaryOperator.RightShiftAssign        
    let [<Literal>] ``>>>=`` = MutatingBinaryOperator.UnsignedRightShiftAssign

type MutatingUnaryOperator =
    | PreIncrement  = 0
    | PostIncrement = 1
    | PreDecrement  = 2
    | PostDecrement = 3
    | Delete        = 4
  
[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]  
module MutatingUnaryOperator =
    let [<Literal>] ``++()`` = MutatingUnaryOperator.PreIncrement 
    let [<Literal>] ``()++`` = MutatingUnaryOperator.PostIncrement
    let [<Literal>] ``--()`` = MutatingUnaryOperator.PreDecrement 
    let [<Literal>] ``()--`` = MutatingUnaryOperator.PostDecrement
    let [<Literal>] delete   = MutatingUnaryOperator.Delete
                                         
type BinaryOperator =
    | NotReferenceEquals = 0
    | NotEquals          = 1
    | Modulo             = 2
    | And                = 3
    | BitwiseAnd         = 4
    | Multiply           = 5
    | Add                = 6
    | Substract          = 7
    | Divide             = 8
    | LeftShift          = 9
    | LessOrEquals       = 10
    | Less               = 11
    | ReferenceEqualsOp  = 12
    | EqualsOp           = 13
    | GreaterOrEquals    = 14
    | UnsignedRightShift = 15
    | RightShift         = 16
    | Greater            = 17
    | BitwiseXor         = 18
    | In                 = 29
    | InstanceOf         = 20
    | BitwiseOr          = 21
    | Or                 = 22

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]  
module BinaryOperator =
    let [<Literal>] ``!==``    = BinaryOperator.NotReferenceEquals
    let [<Literal>] ``!=``     = BinaryOperator.NotEquals         
    let [<Literal>] ``%``      = BinaryOperator.Modulo            
    let [<Literal>] ``&&``     = BinaryOperator.And               
    let [<Literal>] ``&``      = BinaryOperator.BitwiseAnd        
    let [<Literal>] ``*``      = BinaryOperator.Multiply          
    let [<Literal>] ``+``      = BinaryOperator.Add               
    let [<Literal>] ``-``      = BinaryOperator.Substract         
    let [<Literal>] ``/``      = BinaryOperator.Divide            
    let [<Literal>] ``<<``     = BinaryOperator.LeftShift         
    let [<Literal>] ``<=``     = BinaryOperator.LessOrEquals      
    let [<Literal>] ``<``      = BinaryOperator.Less              
    let [<Literal>] ``===``    = BinaryOperator.ReferenceEqualsOp 
    let [<Literal>] ``==``     = BinaryOperator.EqualsOp          
    let [<Literal>] ``>=``     = BinaryOperator.GreaterOrEquals   
    let [<Literal>] ``>>>``    = BinaryOperator.UnsignedRightShift
    let [<Literal>] ``>>``     = BinaryOperator.RightShift        
    let [<Literal>] ``>``      = BinaryOperator.Greater           
    let [<Literal>] ``^``      = BinaryOperator.BitwiseXor        
    let [<Literal>] ``in``     = BinaryOperator.In                
    let [<Literal>] instanceof = BinaryOperator.InstanceOf            
    let [<Literal>] ``|``      = BinaryOperator.BitwiseOr         
    let [<Literal>] ``||``     = BinaryOperator.Or  

type UnaryOperator =
    | Not        = 0
    | Void       = 1
    | Promotion  = 2
    | Inversion  = 3
    | Complement = 4
    | TypeOf     = 5

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]  
module UnaryOperator =
    let [<Literal>] ``!``    = UnaryOperator.Not       
    let [<Literal>] ``void`` = UnaryOperator.Void      
    let [<Literal>] ``+``    = UnaryOperator.Promotion 
    let [<Literal>] ``-``    = UnaryOperator.Inversion 
    let [<Literal>] ``~``    = UnaryOperator.Complement
    let [<Literal>] typeof   = UnaryOperator.TypeOf    

/// Identifies a type definition by AssemblyName and FullName
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

/// Stores a definition and type parameter information
type Concrete<'T> =
    {
        Generics : list<Type>
        Entity : 'T
    }

/// Identifies a type by shape
and Type =
    /// A specific type not covered by other cases
    | ConcreteType of Concrete<TypeDefinition>
    /// A class and method type parameters specified by index in the combined list
    | TypeParameter of int
    /// An array with the specified number of dimensions
    | ArrayType of Type * int
    /// A Sytem.Tuple type, type parameters are in a straight list
    | TupleType of list<Type> * bool
    /// Identifies the FSharp.Core.FSharpFunc type
    | FSharpFuncType of Type * Type
    /// The type of a ref or out parameter
    | ByRefType of Type
    /// Unified case for FSharp.Core.Unit and System.Void
    | VoidType 
    /// used for F# statically resolved type parameters
    | StaticTypeParameter of int
    /// used for F# inner generics
    | LocalTypeParameter

    override this.ToString() =
        match this with
        | ConcreteType t -> 
            string t.Entity.Value +
                match t.Generics with
                | [] -> ""
                | gs -> "<" + (gs |> Seq.map string |> String.concat ", ") + ">"
        | TypeParameter i -> "'T" + string i
        | ArrayType (t, a) -> string t + "[" + String.replicate (a - 1) "," + "]"
        | TupleType (ts, v) -> (if v then "struct " else "") + "(" + (ts |> Seq.map string |> String.concat " * ") + ")"
        | FSharpFuncType (a, r) -> "(" + string a + " -> " + string r + ")"
        | ByRefType t -> "byref<" + string t + ">"
        | VoidType -> "unit"
        | StaticTypeParameter i -> "^T" + string i
        | LocalTypeParameter -> "'?"

    member this.IsParameter =
        match this with
        | TypeParameter _
        | StaticTypeParameter _
        | LocalTypeParameter -> true
        | _ -> false

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
            | StaticTypeParameter i
            | TypeParameter i ->
                "$" + string i, ""
            | ArrayType (t, i) ->
                let tn, ta = getNameAndAsm t
                tn + "[" + String.replicate (i - 1) "," + "]", ta
            | TupleType (ts, v) ->
                let name = if v then "System.ValueTuple`" else "System.Tuple`"
                let rec getName l (ts: List<Type>) =
                    if l <= 7 then
                        name  + (string l) + "[[" + String.concat "],[" (ts |> Seq.map (fun g -> g.AssemblyQualifiedName)) + "]]"
                    else
                        name + "8[[" + 
                            String.concat "],[" (ts |> Seq.take 7 |> Seq.map (fun g -> g.AssemblyQualifiedName)) + 
                            getName (l - 7) (ts |> Seq.skip 7 |> List.ofSeq) + "]]"
                getName (List.length ts) ts, "mscorlib"
            | FSharpFuncType (a, r) ->
                "Microsoft.FSharp.Core.FSharpFunc`2[[" + a.AssemblyQualifiedName + "],[" + r.AssemblyQualifiedName + "]]", "FSharp.Core"
            | ByRefType t -> getNameAndAsm t
            | VoidType -> "Microsoft.FSharp.Core.Unit", "FSharp.Core"
            | LocalTypeParameter -> "$?", ""
        getNameAndAsm this |> combine

    member this.TypeDefinition =
        match this with
        | ConcreteType t -> t.Entity 
        | StaticTypeParameter _
        | LocalTypeParameter 
        | TypeParameter _ -> invalidOp "Generic parameter has no TypeDefinition"
        | ArrayType _ -> invalidOp "Array type has no TypeDefinition"
        | TupleType _ -> invalidOp "Tuple type has no TypeDefinition"
        | FSharpFuncType _ -> invalidOp "FSharpFunc type has no TypeDefinition"
        | ByRefType t -> t.TypeDefinition
        | VoidType -> invalidOp "Void type has no TypeDefinition"

    member this.SubstituteGenerics (gs : Type[]) =
        match this with 
        | ConcreteType t -> ConcreteType { t with Generics = t.Generics |> List.map (fun p -> p.SubstituteGenerics gs) }
        | TypeParameter i -> gs.[i]
        | ArrayType (t, i) -> ArrayType (t.SubstituteGenerics gs, i)
        | TupleType (ts, v) -> TupleType (ts |> List.map (fun p -> p.SubstituteGenerics gs), v) 
        | FSharpFuncType (a, r) -> FSharpFuncType (a.SubstituteGenerics gs, r.SubstituteGenerics gs)
        | ByRefType t -> ByRefType (t.SubstituteGenerics gs)
        | VoidType -> VoidType
        | StaticTypeParameter i -> StaticTypeParameter i
        | LocalTypeParameter -> LocalTypeParameter

    member this.SubstituteGenericsToSame(o : Type) =
        match this with 
        | ConcreteType t -> ConcreteType { t with Generics = t.Generics |> List.map (fun p -> p.SubstituteGenericsToSame(o)) }
        | TypeParameter _ -> o
        | ArrayType (t, i) -> ArrayType (t.SubstituteGenericsToSame(o), i)
        | TupleType (ts, v) -> TupleType (ts |> List.map (fun p -> p.SubstituteGenericsToSame(o)), v) 
        | FSharpFuncType (a, r) -> FSharpFuncType (a.SubstituteGenericsToSame(o), r.SubstituteGenericsToSame(o))
        | ByRefType t -> ByRefType (t.SubstituteGenericsToSame(o))
        | VoidType -> VoidType
        | StaticTypeParameter i -> StaticTypeParameter i
        | LocalTypeParameter -> LocalTypeParameter

type MethodInfo =
    {
        MethodName : string
        Parameters : list<Type>
        ReturnType : Type
        Generics : int       
    }
    
    override m.ToString() =
        sprintf "(%s%s : %s -> %O)"
            m.MethodName 
            (if m.Generics > 0 then "<" + (Seq.init m.Generics (fun _ -> "_") |> String.concat ",") + ">" else "")
            (m.Parameters |> Seq.map string |> String.concat " * ") 
            m.ReturnType

type Method = Hashed<MethodInfo>

type ConstructorInfo =
    {
        CtorParameters : list<Type>    
    }
    override c.ToString() =
        sprintf "%s"
            (c.CtorParameters |> Seq.map string |> String.concat " * ") 

type Constructor = Hashed<ConstructorInfo>

[<RequireQualifiedAccess>]
type Member =
    | Method of isInstance:bool * Method
    | Implementation of TypeDefinition * Method
    | Override of TypeDefinition * Method
    | Constructor of Constructor
    | StaticConstructor

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

    let ReadTypeDefinition (t: System.Type) =
        if t.IsArray then
            Hashed {
                Assembly = "mscorlib"
                FullName = "[]"
            }    
        elif FST.IsFunction t then
            Hashed {
                Assembly = "FSharp.Core"
                FullName = "Microsoft.FSharp.Core.FSharpFunc`2"
            }
        elif FST.IsTuple t then
            let name = if t.IsValueType then "System.ValueTuple`" else "System.Tuple`"
            let g = t.GetGenericArguments().Length
            Hashed {
                Assembly = "mscorlib"
                FullName = name + string (max g 8)
            }
        else
            getTypeDefinitionUnchecked false t

    let private unitTy = typeof<unit>
    let private voidTy = typeof<System.Void>

    let rec ReadType (t: System.Type) =        
        let gen () =
            ConcreteType {
                Generics = 
                    if t.IsGenericType then 
                        t.GetGenericArguments() |> Seq.map ReadType |> List.ofSeq 
                    else [] 
                Entity = getTypeDefinitionUnchecked false t
            }
        if t.IsArray then
            ArrayType (ReadType(t.GetElementType()), t.GetArrayRank())
        elif t.IsByRef then
            ByRefType (ReadType(t.GetElementType()))
        elif FST.IsFunction t then
            let a, r = FST.GetFunctionElements t
            FSharpFuncType(ReadType a, ReadType r)        
        elif FST.IsTuple t then
            // if a tuple type is generic on the rest parameter, we don't have a definite length tuple and GetTupleElements fails
            try TupleType(FST.GetTupleElements t |> Seq.map ReadType |> List.ofSeq, t.IsValueType) 
            with _ -> gen()
        elif t.IsGenericParameter then  
            if t.DeclaringMethod <> null then
                let dT = t.DeclaringType
                let k =
                    if not dT.IsGenericType then 0 else
                        dT.GetGenericArguments().Length
                TypeParameter (k + t.GenericParameterPosition)
            else
                TypeParameter t.GenericParameterPosition
        elif t = voidTy || t = unitTy then
            VoidType
        else
            gen()

    let private readMethodInfo (m : System.Reflection.MethodInfo) =
        let i = m.Module.ResolveMethod m.MetadataToken :?> System.Reflection.MethodInfo
        {
            MethodName = i.Name
            Parameters = i.GetParameters() |> Seq.map (fun p -> ReadType p.ParameterType) |> List.ofSeq
            ReturnType = ReadType i.ReturnType 
            Generics   = if i.IsGenericMethod then i.GetGenericArguments().Length else 0
        }

    let ReadMethod m =
        Method (readMethodInfo m)

    let private readConstructorInfo (i : System.Reflection.ConstructorInfo) =
        {
            CtorParameters = i.GetParameters() |> Seq.map (fun p -> ReadType p.ParameterType) |> List.ofSeq
        }

    let ReadConstructor c =
        Constructor (readConstructorInfo c)    

    let ReadMember (m: System.Reflection.MemberInfo) =
        match m with
        | :? System.Reflection.ConstructorInfo as c ->
            if c.IsStatic then Member.StaticConstructor
            else Member.Constructor (ReadConstructor c)    
            |> Some
        | :? System.Reflection.MethodInfo as m ->
            if m.IsVirtual then
                let b = m.GetBaseDefinition()
                let typ = b.DeclaringType 
                let info = ReadTypeDefinition typ, ReadMethod b 
                if typ.IsInterface then Member.Implementation info
                else Member.Override info
            else Member.Method (not m.IsStatic, ReadMethod m)    
            |> Some
        | _ -> None

    let LoadType (t: Type) =
        try System.Type.GetType(t.AssemblyQualifiedName, true)  
        with _ -> failwithf "Failed to load type %s" t.AssemblyQualifiedName

    let LoadTypeDefinition (td: TypeDefinition) =
        try System.Type.GetType(td.Value.AssemblyQualifiedName, true)   
        with _ -> failwithf "Failed to load type %s from assembly %s" td.Value.FullName td.Value.Assembly

    let [<Literal>] AllMethodsFlags = 
        System.Reflection.BindingFlags.Instance
        ||| System.Reflection.BindingFlags.Static
        ||| System.Reflection.BindingFlags.Public
        ||| System.Reflection.BindingFlags.NonPublic

    let [<Literal>] AllPublicMethodsFlags = 
        System.Reflection.BindingFlags.Instance
        ||| System.Reflection.BindingFlags.Static
        ||| System.Reflection.BindingFlags.Public

    let LoadMethod td (m: Method) =
        let m = m.Value
        let methodInfos = (LoadTypeDefinition td).GetMethods(AllMethodsFlags)
        try
            methodInfos
            |> Seq.find (fun i -> i.Name = m.MethodName && readMethodInfo i = m)
        with _ ->
            failwithf "Could not load method %O candidates: %A" m (methodInfos |> Seq.choose (fun c -> 
                let mc = readMethodInfo c
                if mc.MethodName = m.MethodName then Some (string mc) else None
            ) |> Array.ofSeq)

    let LoadConstructor td (c: Constructor) =
        let c = c.Value
        let ctorInfos = (LoadTypeDefinition td).GetConstructors(AllMethodsFlags)
        try
            ctorInfos
            |> Seq.find (fun i -> readConstructorInfo i = c)
        with _ ->
            failwithf "Could not load constructor for type %s" td.Value.AssemblyQualifiedName

