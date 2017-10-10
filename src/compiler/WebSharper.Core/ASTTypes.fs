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

#nowarn "86" // redefining operators

namespace WebSharper.Core.AST

open WebSharper.Core

module S = WebSharper.Core.JavaScript.Syntax

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
        StrongName : bool
        Optional : bool
    }

    member this.Name 
        with get () = this.IdName
        and set n = this.IdName <- n

    member this.IsMutable = this.Mutable
    member this.HasStrongName = this.StrongName
    member this.IsOptional = this.Optional
    
    static member New(?name, ?mut, ?str, ?opt) =
        {
            IdName = name
            Id = Ids.New()
            Mutable = defaultArg mut true
            StrongName = defaultArg str false
            Optional = defaultArg opt false
        }

    member this.Clone() =
        { this with
            Id = if this.Id < 0L then this.Id else Ids.New()
        }

    member this.ToMutable() =
        { this with
            Mutable = true
        }

    member this.ToNonOptional() =
        if this.Optional then
            { this with
                Optional = false
            }
        else this

    member this.IsGlobal() = this.Id = -1L

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

type Purity =
    | NonPure
    | NoSideEffect
    | Pure

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

module StableHash =
 
    let string (s: string)= 
        let mutable hash1 = (5381 <<< 16) + 5381
        let mutable hash2 = hash1

        for i in 0 .. 2 .. s.Length do
            hash1 <- ((hash1 <<< 5) + hash1) ^^^ int s.[i]
            hash2 <- ((hash2 <<< 5) + hash2) ^^^ int s.[i+1];

        hash1 + (hash2*1566083941)

    let tuple (a, b) =
        a * 33 + b

    let list l =
        let mutable h = 1
        let mutable l = l
        while not (List.isEmpty l) do 
            h <- -1640531527 + l.Head + (h <<< 6) + (h >>> 2)
            l <- l.Tail
        h

    let data (d: byte[]) =
        let mutable h = 1
        for i = 0 to (d.Length / 4) - 1 do
            let x = System.BitConverter.ToInt32(d, 4 * i)
            h <- 1448225822 + x + (h <<< 6) + (h >>> 2)
        for i = (d.Length / 4) * 4 to d.Length - 1 do
            let x = int d.[i]
            h <- 1448225822 + x + (h <<< 6) + (h >>> 2)
        h

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

module Definitions =
    open System.Reflection

    let FSharpFunc =
        TypeDefinition {
            Assembly = "FSharp.Core"
            FullName = "Microsoft.FSharp.Core.FSharpFunc`2"
        }

    let Tuple isStruct (arity: int) =
        TypeDefinition {
            Assembly = if isStruct then "System.ValueTuple" else "mscorlib"
            FullName = 
                let name = if isStruct then "System.ValueTuple" else "System.Tuple"
                if arity = 0 then name else name + "`" + string (min arity 8)
        }

    let FSharpList =
        TypeDefinition {
            Assembly = "FSharp.Core"
            FullName = "Microsoft.FSharp.Collections.FSharpList`1"
        }

    let Array =
        TypeDefinition {
            Assembly = "mscorlib"
            FullName = "[]"
        }    

    let Array2 =
        TypeDefinition {
            Assembly = "mscorlib"
            FullName = "[,]"
        }

    let ResizeArray =
        TypeDefinition {
            Assembly = "mscorlib"
            FullName = "System.Collections.Generic.List`1"
        }

    let Unit =
        TypeDefinition {
            Assembly = "FSharp.Core"
            FullName = "Microsoft.FSharp.Core.Unit"
        }

    let Void =
        TypeDefinition {
            Assembly = "mscorlib"
            FullName = "System.Void"
        }

    let Object =
        TypeDefinition {
            Assembly = "mscorlib"
            FullName = "System.Object"
        }

    let Bool =
        TypeDefinition {
            Assembly = "mscorlib"
            FullName = "System.Boolean"
        }

    let UInt8 =
        TypeDefinition {
            Assembly = "mscorlib"
            FullName = "System.Byte"
        }
    let Byte = UInt8

    let Int8 =
        TypeDefinition {
            Assembly = "mscorlib"
            FullName = "System.SByte"
        }
    let SByte = Int8

    let UInt16 =
        TypeDefinition {
            Assembly = "mscorlib"
            FullName = "System.UInt16"
        }

    let Int16 =
        TypeDefinition {
            Assembly = "mscorlib"
            FullName = "System.Int16"
        }

    let UInt32 =
        TypeDefinition {
            Assembly = "mscorlib"
            FullName = "System.UInt32"
        }

    let Int32 =
        TypeDefinition {
            Assembly = "mscorlib"
            FullName = "System.Int32"
        }
    let Int = Int32

    let UInt64 =
        TypeDefinition {
            Assembly = "mscorlib"
            FullName = "System.UInt64"
        }

    let Int64 =
        TypeDefinition {
            Assembly = "mscorlib"
            FullName = "System.Int64"
        }

    let String =
        TypeDefinition {
            Assembly = "mscorlib"
            FullName = "System.String"
        }

    let Float32 =
        TypeDefinition {
            Assembly = "mscorlib"
            FullName = "System.Single"
        }

    let Float =
        TypeDefinition {
            Assembly = "mscorlib"
            FullName = "System.Double"
        }

    let FSharpAsync =
        TypeDefinition {
            Assembly = "FSharp.Core"
            FullName = "Microsoft.FSharp.Control.FSharpAsync`1"
        }

    let FSharpChoice (arity: int) =
        TypeDefinition {
            Assembly = "FSharp.Core"
            FullName = "Microsoft.FSharp.Core.FSharpChoice`" + string arity
        }

/// Stores a definition and type parameter information
type Concrete<'T> =
    {
        Generics : list<Type>
        Entity : 'T
    }

/// Identifies a type by shape
and Type =
    /// A specific type not covered by other cases
    | ConcreteType of concreteType: Concrete<TypeDefinition>
    /// A class and method type parameters specified by index in the combined list
    | TypeParameter of ordinal: int
    /// An array with the specified number of dimensions
    | ArrayType of elemType: Type * rank: int
    /// A Sytem.Tuple type, type parameters are in a straight list
    | TupleType of elemTypes: list<Type> * isStruct: bool
    /// Identifies the FSharp.Core.FSharpFunc type
    | FSharpFuncType of argumentType: Type * returnType: Type
    /// The type of a ref or out parameter
    | ByRefType of undelyingType: Type
    /// Unified case for FSharp.Core.Unit and System.Void
    | VoidType 
    /// used for F# statically resolved type parameters
    | StaticTypeParameter of ordinal: int
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
                getName (List.length ts) ts, if v then "System.ValueTuple" else "mscorlib"
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
        | ArrayType _ -> Definitions.Array
        | TupleType (ts, isStruct)  -> Definitions.Tuple isStruct (List.length ts)
        | FSharpFuncType _ -> Definitions.FSharpFunc
        | ByRefType t -> t.TypeDefinition
        | VoidType -> Definitions.Unit

    member this.SubstituteGenerics (gs : Type[]) =
        match this with 
        | ConcreteType t -> ConcreteType { t with Generics = t.Generics |> List.map (fun p -> p.SubstituteGenerics gs) }
        | TypeParameter i -> gs.[i]
        | ArrayType (t, i) -> ArrayType (t.SubstituteGenerics gs, i)
        | TupleType (ts, v) -> TupleType (ts |> List.map (fun p -> p.SubstituteGenerics gs), v) 
        | FSharpFuncType (a, r) -> FSharpFuncType (a.SubstituteGenerics gs, r.SubstituteGenerics gs)
        | ByRefType t -> ByRefType (t.SubstituteGenerics gs)
        | VoidType 
        | StaticTypeParameter _ 
        | LocalTypeParameter -> this

    member this.SubstituteGenericsToSame(o : Type) =
        match this with 
        | ConcreteType t -> ConcreteType { t with Generics = t.Generics |> List.map (fun p -> p.SubstituteGenericsToSame(o)) }
        | TypeParameter _ -> o
        | ArrayType (t, i) -> ArrayType (t.SubstituteGenericsToSame(o), i)
        | TupleType (ts, v) -> TupleType (ts |> List.map (fun p -> p.SubstituteGenericsToSame(o)), v) 
        | FSharpFuncType (a, r) -> FSharpFuncType (a.SubstituteGenericsToSame(o), r.SubstituteGenericsToSame(o))
        | ByRefType t -> ByRefType (t.SubstituteGenericsToSame(o))
        | VoidType 
        | StaticTypeParameter _ 
        | LocalTypeParameter -> this

    member this.GetStableHash()  =
        let inline (++) a b = StableHash.tuple (a, b)
        let inline (!^) a = StableHash.string a
        let inline (!!) a = StableHash.list a

        let hashTd (td: TypeDefinition) =
            StableHash.string td.Value.Assembly ++ StableHash.string td.Value.FullName 

        let hashTypeList ts =
            ts |> List.map (fun (t: Type) -> t.GetStableHash()) |> StableHash.list 

        match this with 
        | ConcreteType t -> 0 ++ hashTd t.Entity ++ hashTypeList t.Generics 
        | TypeParameter i -> 1 ++ i
        | ArrayType (t, i) -> 2 ++ t.GetStableHash() ++ i
        | TupleType (ts, v) -> 3 ++ hashTypeList ts + (if v then 1 else 0)
        | FSharpFuncType (a, r) -> 4 ++ a.GetStableHash() ++ r.GetStableHash()
        | ByRefType t -> 5 ++ t.GetStableHash()
        | VoidType -> 6
        | StaticTypeParameter i -> 7 ++ i
        | LocalTypeParameter -> 8

    member this.Normalize() =
        match this with
        | ConcreteType t -> 
            let td = t.Entity
            if td = Definitions.Void || td = Definitions.Unit then
                VoidType
            elif td = Definitions.Array then
                ArrayType (t.Generics.[0].Normalize(), 1)
            elif td = Definitions.Array2 then
                ArrayType (t.Generics.[0].Normalize(), 2)
            elif td = Definitions.FSharpFunc then
                FSharpFuncType (t.Generics.[0].Normalize(), t.Generics.[1].Normalize())
            elif td.Value.FullName.StartsWith "System.Tuple`" then // TODO: longer tuples 
                TupleType(t.Generics |> List.map (fun p -> p.Normalize()), false)
            elif td.Value.FullName.StartsWith "System.ValueTuple`" then
                TupleType(t.Generics |> List.map (fun p -> p.Normalize()), true)
            else
                ConcreteType { t with Generics = t.Generics |> List.map (fun p -> p.Normalize()) }
        | ArrayType (t, i) -> ArrayType (t.Normalize(), i)
        | TupleType (ts, v) -> TupleType (ts |> List.map (fun p -> p.Normalize()), v) 
        | FSharpFuncType (a, r) -> FSharpFuncType (a.Normalize(), r.Normalize())
        | ByRefType t -> ByRefType (t.Normalize())
        | TypeParameter _
        | VoidType 
        | StaticTypeParameter _ 
        | LocalTypeParameter -> this

[<RequireQualifiedAccess>]
type TSType =
    | Any
    | Basic of string
    | Generic of TSType * list<TSType>
    | Imported of Id * string
    | Lambda of list<TSType> * TSType
    | New of list<TSType>
    | Tuple of list<TSType>
    | Union of list<TSType>
    | Param of int
    | Constraint of TSType * list<TSType>

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
            (if m.Parameters.Length > 0 then
                m.Parameters |> Seq.map string |> String.concat " * "
            else
                "unit") 
            m.ReturnType

type Method = Hashed<MethodInfo>

type ConstructorInfo =
    {
        CtorParameters : list<Type>    
    }
    override c.ToString() =
        sprintf "%s"
            (if c.CtorParameters.Length > 0 then
                c.CtorParameters |> Seq.map string |> String.concat " * "
            else
                "unit") 

type Constructor = Hashed<ConstructorInfo>

[<RequireQualifiedAccess>]
type Member =
    | Method of isInstance:bool * Method
    | Implementation of TypeDefinition * Method
    | Override of TypeDefinition * Method
    | Constructor of Constructor
    | StaticConstructor

type Module =
    | StandardLibrary
    | JavaScriptFile of string
    | WebSharperModule of string
    | CurrentModule

type Address =
    {
        Module : Module
        Address : Hashed<list<string>>
    }

    member this.JSAddress =
        match this.Module with
        | StandardLibrary
        | JavaScriptFile _ ->
            Some this.Address
        | _ -> None

module private Instances =
    let uniqueId name i = 
        {
            IdName = Some name
            Id = i
            Mutable = false
            StrongName = true
            Optional = false
        }

    let GlobalId = uniqueId "window" -1L

    let DefaultCtor =
        Constructor { CtorParameters = [] }

    let RuntimeModule = JavaScriptFile "Runtime"

    let GlobalAddress = { Module = StandardLibrary; Address = Hashed [] }
    let EmptyAddress = { Module = CurrentModule; Address = Hashed [] }

type Id with
    static member Global() = Instances.GlobalId

type ConstructorInfo with
    static member Default() = Instances.DefaultCtor

type Address with
    static member Runtime() = { Module = Instances.RuntimeModule; Address = Hashed ["WSRuntime"] }
    static member Runtime f = { Module = Instances.RuntimeModule; Address = Hashed [f; "WSRuntime"] }
    static member Lib a = { Module = StandardLibrary; Address = Hashed [ a ] }
    static member Global() = Instances.GlobalAddress
    static member Empty() = Instances.EmptyAddress

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
            if t.GetArrayRank() = 1 then
                Definitions.Array
            else 
                Definitions.Array2
        elif FST.IsFunction t then 
            Definitions.FSharpFunc
        elif FST.IsTuple t then 
            Definitions.Tuple t.IsValueType (t.GetGenericArguments().Length)
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
            match t.DeclaringMethod with
            | null ->
                TypeParameter t.GenericParameterPosition
            | _ ->
                let dT = t.DeclaringType
                let k =
                    if not dT.IsGenericType then 0 else
                        dT.GetGenericArguments().Length
                TypeParameter (k + t.GenericParameterPosition)
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

