// $begin{copyright}
//
// This file is part of WebSharper
//
// Copyright (c) 2008-2018 IntelliFactory
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

namespace rec WebSharper.Core.AST

open WebSharper.Core

module S = WebSharper.Core.JavaScript.Syntax

type private Ids() =
    static let mutable lastId = -1L
    static member New() =
        System.Threading.Interlocked.Increment(&lastId)

type Modifiers = S.Modifiers

[<CustomComparison; CustomEquality>]
/// An identifier for a variable or label.
type Id =
    private {
        mutable IdName : string option
        Id: int64
        Mutable : bool
        Rest: bool
        StrongName : bool
        Optional : bool
        Type : Type option
    }

    member this.Name 
        with get () = this.IdName
        and set n = this.IdName <- n

    member this.IsMutable = this.Mutable
    member this.HasStrongName = this.StrongName
    member this.IsOptional = this.Optional
    member this.IsRest = this.Rest
    
    static member New(?name, ?mut, ?str, ?opt, ?rest, ?typ) =
        {
            IdName = name
            Id = Ids.New()
            Mutable = defaultArg mut true
            StrongName = defaultArg str false
            Optional = defaultArg opt false
            Rest = defaultArg rest false
            Type = typ
        }

    static member NewThis() = Id.New("_this", mut = false)

    member this.Clone() =
        { this with
            Id = if this.Id < 0L then this.Id else Ids.New()
        }

    member this.ToMutable() =
        { this with
            Mutable = true
        }

    member this.ToRest() =
        { this with
            Rest = true
        }

    member this.ToNonOptional() =
        if this.Optional then
            { this with
                Optional = false
            }
        else this

    member this.VarType = this.Type

    member this.IsTuple =
        match this.Type with
        | Some (TupleType _)
        | Some (TSType (TSType.Tuple _)) -> true
        | _ -> false

    member this.ToTSType(toTSType) =
        match this.Type with
        | None -> this
        | Some t -> { this with Type = Some (TSType (toTSType t)) }

    member this.TSType =
        match this.Type with
        | Some (TSType t) -> Some t
        | _ -> None

    member this.SubstituteGenerics(gs : Type[]) =
        match this.Type with
        | Some t -> { this with Type = Some (t.SubstituteGenerics gs) }
        | _ -> this

    member this.HasUnresolvedGenerics =
        match this.Type with
        | Some t -> t.HasUnresolvedGenerics
        | _ -> false

    member this.IsGlobal() = this.Id = -1L

    member this.WithType(t) = { this with Type = t }

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
        (match this.Name with Some n -> n | _ -> "") + "#" + string this.Id + (if this.Mutable then "M" else "") +
        (match this.Type with Some t -> ":" + string t | _ -> ":NONTYPED")

    member this.ToString(m: Modifiers) =
        String.concat "" [
            if m.HasFlag Modifiers.Private then yield "private "
            if m.HasFlag Modifiers.Public then yield "public "
            if m.HasFlag Modifiers.ReadOnly then yield "readonly"
        ] + string this

/// Specifies a curried or tupled F# function argument that is translated to a flat function
type FuncArgOptimization =
    | NotOptimizedFuncArg
    | CurriedFuncArg of int    
    | TupledFuncArg of int    
    | OutRefArg
    | InRefArg

type Purity =
    /// Marks that a function call/property get has a side effect or depends on execution order.
    /// Cannot be dropped or execution order swapped with other non-pure expressions.
    | NonPure
    /// Marks that a function call/property get has no side effect but depends on execution order.
    /// Can be dropped if result is unused. Cannot have execution order swapped with other non-pure expressions.
    | NoSideEffect
    /// Marks that a function call/property get has no side effect and only depends on its arguments.
    /// Can be dropped if result is unused. Execution order can swapped.
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
    | CoalesceAssign           = 12

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
    let [<Literal>] ``??=``  = MutatingBinaryOperator.CoalesceAssign

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
    | Exponentiation     = 23
    | Coalesce           = 24

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]  
module BinaryOperator =
    let [<Literal>] ``!==``    = BinaryOperator.NotReferenceEquals
    let [<Literal>] ``!=``     = BinaryOperator.NotEquals         
    let [<Literal>] ``%``      = BinaryOperator.Modulo            
    let [<Literal>] ``&&``     = BinaryOperator.And               
    let [<Literal>] ``&``      = BinaryOperator.BitwiseAnd        
    let [<Literal>] ``*``      = BinaryOperator.Multiply          
    let [<Literal>] ``**``     = BinaryOperator.Exponentiation          
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
    let [<Literal>] ``??``     = BinaryOperator.Coalesce

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

    member this.GenericLength =
        try
            this.FullName.Split('.', '+') |> Array.sumBy (fun n ->
                match n.IndexOf '`' with
                | -1 -> 0
                | i -> int (n.Substring(i + 1))
            )
        with _ ->
            failwithf "failed to get generics count of type %s" this.FullName

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
            Assembly = "netstandard"
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
            Assembly = "netstandard"
            FullName = "[]"
        }    

    let Array2 =
        TypeDefinition {
            Assembly = "netstandard"
            FullName = "[,]"
        }

    let ResizeArray =
        TypeDefinition {
            Assembly = "netstandard"
            FullName = "System.Collections.Generic.List`1"
        }

    let Unit =
        TypeDefinition {
            Assembly = "FSharp.Core"
            FullName = "Microsoft.FSharp.Core.Unit"
        }

    let Void =
        TypeDefinition {
            Assembly = "netstandard"
            FullName = "System.Void"
        }

    let Obj =
        TypeDefinition {
            Assembly = "netstandard"
            FullName = "System.Object"
        }
    let Object = Obj

    let Bool =
        TypeDefinition {
            Assembly = "netstandard"
            FullName = "System.Boolean"
        }

    let Char =
        TypeDefinition {
            Assembly = "netstandard"
            FullName = "System.Char"
        }

    let UInt8 =
        TypeDefinition {
            Assembly = "netstandard"
            FullName = "System.Byte"
        }
    let Byte = UInt8

    let Int8 =
        TypeDefinition {
            Assembly = "netstandard"
            FullName = "System.SByte"
        }
    let SByte = Int8

    let UInt16 =
        TypeDefinition {
            Assembly = "netstandard"
            FullName = "System.UInt16"
        }

    let Int16 =
        TypeDefinition {
            Assembly = "netstandard"
            FullName = "System.Int16"
        }

    let UInt32 =
        TypeDefinition {
            Assembly = "netstandard"
            FullName = "System.UInt32"
        }

    let Int32 =
        TypeDefinition {
            Assembly = "netstandard"
            FullName = "System.Int32"
        }
    let Int = Int32

    let UInt64 =
        TypeDefinition {
            Assembly = "netstandard"
            FullName = "System.UInt64"
        }

    let Int64 =
        TypeDefinition {
            Assembly = "netstandard"
            FullName = "System.Int64"
        }

    let String =
        TypeDefinition {
            Assembly = "netstandard"
            FullName = "System.String"
        }

    let Float32 =
        TypeDefinition {
            Assembly = "netstandard"
            FullName = "System.Single"
        }

    let Float =
        TypeDefinition {
            Assembly = "netstandard"
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
    
    let Decimal =
        TypeDefinition {
            Assembly = "netstandard"
            FullName = "System.Decimal"
        }

    let WSDecimal =
        TypeDefinition {
            Assembly = "WebSharper.MathJS.Extensions"
            FullName = "WebSharper.Decimal"
        }

    let FSharpOption =
        TypeDefinition {
            Assembly = "FSharp.Core"
            FullName = "Microsoft.FSharp.Core.FSharpOption`1"
        }
    
    let FSharpValueOption =
        TypeDefinition {
            Assembly = "FSharp.Core"
            FullName = "Microsoft.FSharp.Core.FSharpValueOption`1"
        }

    let OptimizedClosuresFSharpFunc3 =
        TypeDefinition {
            Assembly = "FSharp.Core"
            FullName = "Microsoft.FSharp.Core.OptimizedClosures+FSharpFunc`3"
        }
    
    let OptimizedClosuresFSharpFunc4 =
        TypeDefinition {
            Assembly = "FSharp.Core"
            FullName = "Microsoft.FSharp.Core.OptimizedClosures+FSharpFunc`4"
        }
    
    let OptimizedClosuresFSharpFunc5 =
        TypeDefinition {
            Assembly = "FSharp.Core"
            FullName = "Microsoft.FSharp.Core.OptimizedClosures+FSharpFunc`5"
        }
    
    let OptimizedClosuresFSharpFunc6 =
        TypeDefinition {
            Assembly = "FSharp.Core"
            FullName = "Microsoft.FSharp.Core.OptimizedClosures+FSharpFunc`6"
        }

    let ValueType =
        TypeDefinition {
            Assembly = "netstandard"
            FullName = "System.ValueType"    
        }

    let Dynamic =
        TypeDefinition {
            Assembly = ""
            FullName = "dynamic"
        }

    let IResource =
        TypeDefinition {
            Assembly = "WebSharper.Core"
            FullName = "WebSharper.Core.Resources+IResource"    
        }

    let Async =
        TypeDefinition {
            Assembly = "FSharp.Core"
            FullName = "Microsoft.FSharp.Control.FSharpAsync`1"
        }
        
    let Task =
        TypeDefinition {
            Assembly = "netstandard"
            FullName = "System.Threading.Tasks.Task"
        }

    let Task1 =
        TypeDefinition {
            Assembly = "netstandard"
            FullName = "System.Threading.Tasks.Task`1"
        }

    let IRemotingProvider =
        TypeDefinition {
            Assembly = "WebSharper.Main"
            FullName = "WebSharper.Remoting+IRemotingProvider"
        } 
    
/// Stores a definition and type parameter information
type Concrete<'T> =
    {
        Generics : list<Type>
        Entity : 'T
    }

/// Identifies a type by shape
type Type =
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
    /// Translated type
    | TSType of tsType: TSType

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
        | TSType ts -> "TS:" + string ts

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
                getName (List.length ts) ts, "netstandard"
            | FSharpFuncType (a, r) ->
                "Microsoft.FSharp.Core.FSharpFunc`2[[" + a.AssemblyQualifiedName + "],[" + r.AssemblyQualifiedName + "]]", "FSharp.Core"
            | ByRefType t -> getNameAndAsm t
            | VoidType -> "Microsoft.FSharp.Core.Unit", "FSharp.Core"
            | LocalTypeParameter -> "$?", ""
            | TSType _ -> invalidOp "TypeScript type has no AssemblyQualifiedName"
        getNameAndAsm this |> combine

    member this.DisplayName =
        let rec getName ty =
            match ty with
            | ConcreteType t -> (t.Entity.Value.FullName.Split([| '.'; '+' |]) |> Array.last).Replace("`", "_")
            | StaticTypeParameter _
            | LocalTypeParameter 
            | TypeParameter _ -> invalidOp "Generic parameter has no TypeDefinition"
            | ArrayType (t, i) -> "Array" + (if i = 0 then "" else string i + "D") + "_" + getName t
            | TupleType (ts, _)  -> "Tuple_" + (ts |> Seq.map getName |> String.concat "_")
            | FSharpFuncType _ -> "Func"
            | ByRefType t -> "ByRef_" + getName t
            | VoidType -> "Void"
            | TSType _ -> invalidOp "TypeScript type has no DisplayName"
        getName this

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
        | TSType _ -> invalidOp "TypeScript type has no TypeDefinition"

    member this.SubstituteGenerics (gs : Type[], ?staticOnly) =
        match this with 
        | ConcreteType t -> ConcreteType { t with Generics = t.Generics |> List.map (fun p -> p.SubstituteGenerics(gs, ?staticOnly = staticOnly)) }
        | TypeParameter i ->
            if staticOnly.IsSome && staticOnly.Value then
                this
            elif gs.Length > i then 
                gs.[i] 
            else 
                failwithf "Error during generic substitution, index %d, types: %A" i (gs |> Seq.map string |> String.concat ";")
        | StaticTypeParameter i ->
            if gs.Length > i then 
                gs.[i] 
            else 
                failwithf "Error during generic substitution, index %d, types: %A" i (gs |> Seq.map string |> String.concat ";")
        | ArrayType (t, i) -> ArrayType (t.SubstituteGenerics(gs, ?staticOnly = staticOnly), i)
        | TupleType (ts, v) -> TupleType (ts |> List.map (fun p -> p.SubstituteGenerics(gs, ?staticOnly = staticOnly)), v) 
        | FSharpFuncType (a, r) -> FSharpFuncType (a.SubstituteGenerics(gs, ?staticOnly = staticOnly), r.SubstituteGenerics(gs, ?staticOnly = staticOnly))
        | ByRefType t -> ByRefType (t.SubstituteGenerics(gs, ?staticOnly = staticOnly))
        | VoidType -> this
        | LocalTypeParameter -> ConcreteType { Entity = Definitions.Object; Generics = [] }
        | TSType _ -> invalidOp "TypeScript type does not support SubstituteGenerics"

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
        | TSType _ -> invalidOp "TypeScript type does not support SubstituteGenericsToSame"

    member this.HasUnresolvedGenerics =
        match this with 
        | ConcreteType t -> t.Generics |> List.exists (fun p -> p.HasUnresolvedGenerics)
        | StaticTypeParameter _ 
        | TypeParameter _ -> true
        | VoidType 
        | LocalTypeParameter -> false
        | ArrayType (t, _) -> t.HasUnresolvedGenerics
        | TupleType (ts, v) -> ts |> List.exists (fun p -> p.HasUnresolvedGenerics)
        | FSharpFuncType (a, r) -> a.HasUnresolvedGenerics || r.HasUnresolvedGenerics
        | ByRefType t -> t.HasUnresolvedGenerics
        | TSType _ -> invalidOp "TypeScript type does not support HasUnresolvedGenerics"

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
        | TSType _ -> invalidOp "TypeScript type does not support GetStableHash"

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
        | TSType _ -> invalidOp "TypeScript type does not support Normalize"

    member this.MapTypeDefinitions(mapping) =
        match this with 
        | ConcreteType t -> 
            ConcreteType { 
                Generics = t.Generics |> List.map (fun p -> p.MapTypeDefinitions mapping)
                Entity = mapping t.Entity 
            }
        | TypeParameter i
        | StaticTypeParameter i -> this
        | ArrayType (t, i) -> ArrayType (t.MapTypeDefinitions mapping, i)
        | TupleType (ts, v) -> TupleType (ts |> List.map (fun p -> p.MapTypeDefinitions mapping), v) 
        | FSharpFuncType (a, r) -> FSharpFuncType (a.MapTypeDefinitions mapping, r.MapTypeDefinitions mapping)
        | ByRefType t -> ByRefType (t.MapTypeDefinitions mapping)
        | VoidType -> this
        | LocalTypeParameter -> ConcreteType { Entity = Definitions.Object; Generics = [] }
        | TSType _ -> invalidOp "TypeScript type does not support MapTypeDefinitions"

    static member IsGenericCompatible(targetSig, usageSig) =
        let d = System.Collections.Generic.Dictionary() 
        let rec isCompat t1 t2 =
            match t1, t2 with
            | (StaticTypeParameter i | TypeParameter i), t ->
                match d.TryGetValue(i) with
                | true, ts -> ts = t
                | _ ->
                    d.Add(i, t)
                    true
            | ConcreteType t1, ConcreteType t2 -> t1.Entity = t2.Entity && t1.Generics.Length = t2.Generics.Length && List.forall2 isCompat t1.Generics t2.Generics
            | ArrayType (t1, r1), ArrayType (t2, r2) -> r1 = r2 && isCompat t1 t2
            | TupleType (t1, s1), TupleType (t2, s2) -> s1 = s2 && t1.Length = t2.Length && List.forall2 isCompat t1 t2 
            | FSharpFuncType (a1, r1), FSharpFuncType (a2, r2) -> isCompat a1 a2 && isCompat r1 r2
            | ByRefType t1, ByRefType t2 -> isCompat t1 t2
            | VoidType, VoidType -> true
            | _ -> false
        isCompat targetSig usageSig

    member this.IsOptional =
        match this with
        | ConcreteType t ->
            t.Entity = Definitions.FSharpOption 
            || t.Entity = Definitions.FSharpValueOption 
        | _ -> false

module TypeHelpers =
    let (|OptimizedClosures3|_|) t =
        match t with
        | ConcreteType { Entity = e; Generics = [t1; t2; t3] } when e = Definitions.OptimizedClosuresFSharpFunc3 ->
            Some (t1, t2, t3)
        | _ -> None

    let (|OptimizedClosures4|_|) t =
        match t with
        | ConcreteType { Entity = e; Generics = [t1; t2; t3; t4] } when e = Definitions.OptimizedClosuresFSharpFunc4 ->
            Some (t1, t2, t3, t4)
        | _ -> None

    let (|OptimizedClosures5|_|) t =
        match t with
        | ConcreteType { Entity = e; Generics = [t1; t2; t3; t4; t5] } when e = Definitions.OptimizedClosuresFSharpFunc5 ->
            Some (t1, t2, t3, t4, t5)
        | _ -> None

    let (|OptimizedClosures6|_|) t =
        match t with
        | ConcreteType { Entity = e; Generics = [t1; t2; t3; t4; t5; t6] } when e = Definitions.OptimizedClosuresFSharpFunc6 ->
            Some (t1, t2, t3, t4, t5, t6)
        | _ -> None

type [<RequireQualifiedAccess>] MemberKind =
    | Simple
    | Getter
    | Setter

type CodeResource =
    {
        Assembly : string
        Name : string
    }
    override this.ToString() = 
        sprintf "%s/%s" this.Assembly this.Name

type [<RequireQualifiedAccess>] TSType =
    | Any
    | Named of list<string>
    | Generic of TSType * list<TSType>
    | Imported of Id * list<string>
    | Importing of Address
    | Function of option<TSType> * list<TSType * bool> * option<TSType> * TSType
    | New of list<TSType> * TSType
    | Tuple of list<TSType>
    | Union of list<TSType>
    | Intersection of list<TSType>
    | Param of int
    | Constraint of TSType * list<TSType>
    | TypeGuard of Id * TSType
    | ObjectOf of TSType
    | TypeLiteral of list<string * MemberKind * TSType> 

    member this.SubstituteGenerics (gs : TSType[]) =
        let inline tr (p:TSType) = p.SubstituteGenerics gs
        match this with 
        | Any
        | Named _
        | Imported _
        | Importing _
            -> this
        | Param i -> gs.[i]
        | Generic (e, g) -> Generic (tr e, List.map tr g)
        | Function (t, a, e, r) -> Function (Option.map tr t, List.map (fun (a, o) -> tr a, o) a, Option.map tr e, tr r)
        | New (a, r) -> New (List.map tr a, tr r)
        | Tuple ts -> Tuple (List.map tr ts)
        | Union ts -> Union (List.map tr ts)
        | Intersection ts -> Intersection (List.map tr ts)
        | Constraint (t, c) -> Constraint (tr t, List.map tr c)
        | TypeGuard (a, t) -> TypeGuard(a, tr t)
        | ObjectOf a -> ObjectOf(tr a)
        | TypeLiteral m -> TypeLiteral(m |> List.map (fun (n, k, t) -> n, k, tr t))

    member this.ResolveAddress (resolve: Address -> TSType) =
        let inline tr (p:TSType) = p.ResolveAddress resolve
        match this with 
        | Any
        | Named _
        | Imported _
        | Param _
            -> this
        | Importing m  -> resolve m
        | Generic (e, g) -> Generic (tr e, List.map tr g)
        | Function (t, a, e, r) -> Function (Option.map tr t, List.map (fun (a, o) -> tr a, o) a, Option.map tr e, tr r)
        | New (a, r) -> New (List.map tr a, tr r)
        | Tuple ts -> Tuple (List.map tr ts)
        | Union ts -> Union (List.map tr ts)
        | Intersection ts -> Intersection (List.map tr ts)
        | Constraint (t, c) -> Constraint (tr t, List.map tr c)
        | TypeGuard (a, t) -> TypeGuard(a, tr t)
        | ObjectOf a -> ObjectOf(tr a)
        | TypeLiteral m -> TypeLiteral(m |> List.map (fun (n, k, t) -> n, k, tr t))

    member this.ResolveModule (getModule: Module -> Id option) =
        this.ResolveAddress (fun m ->
            match getModule m.Module with
            | Some v -> Imported(v, m.Address)
            | _ -> Named m.Address
        )

    override this.ToString() =
        match this with
        | Any -> "any"
        | Named t -> t |> String.concat "."
        | Generic (t, g) -> string t + "<" + (g |> List.map string |> String.concat ",") + ">"
        | Imported (i, t) -> string i + "::" + (t |> String.concat ".")
        | Importing a -> string a.Module + "?:" + (a.Address |> List.rev |> String.concat ".")
        | Function (t, a, s, r) -> "((" + (a |> List.map string |> String.concat ",") + ")=>" + string r + ")" 
        | New (a, r) -> "(new(" + (a |> List.map string |> String.concat ",") + ")=>" + string r + ")" 
        | Tuple ts -> "[" + (ts |> Seq.map string |> String.concat ",") + "]"
        | Union ts -> "(" + (ts |> Seq.map string |> String.concat "|") + ")"
        | Intersection ts -> "(" + (ts |> Seq.map string |> String.concat "&") + ")"
        | Param n -> "T" + string n
        | Constraint (t, ts) -> string t + " extends " + (ts |> Seq.map string |> String.concat ",")
        | TypeGuard (i, t) -> string i + " is " + string t
        | ObjectOf t -> "{[a:string]:" + string t + "}"
        | TypeLiteral m ->
            "{" + (
                m |> List.map (fun (n, k, t) -> 
                    match k with
                    | MemberKind.Simple -> n + ":" + t.ToString()
                    | MemberKind.Getter -> "get" + n + ":" + t.ToString()
                    | MemberKind.Setter -> "set " + n + ":" + t.ToString()
                )
                |> String.concat ","
            ) + "}"

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

    member this.SubstituteResolvedGenerics (gs : Type[]) =
        { this with
            Parameters = this.Parameters |> List.map (fun t -> t.SubstituteGenerics(gs))
            ReturnType = this.ReturnType.SubstituteGenerics(gs)
        }

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
    | JavaScriptFile of CodeResource
    | JavaScriptModule of CodeResource
    | DotNetType of CodeResource
    | NpmPackage of string
    | ImportedModule of Id

    override this.ToString() =
        match this with
        | StandardLibrary -> ""
        | JavaScriptFile r
        | JavaScriptModule r -> string r
        | DotNetType r -> string r
        | NpmPackage p -> string p
        | ImportedModule i -> string i

type PlainAddress = Hashed<list<string>>

type Address =
    {
        Module : Module
        Address : list<string>
    }

    override this.ToString() =
        match this.Module with
        | StandardLibrary
        | JavaScriptFile _ -> "globalThis."
        | JavaScriptModule c 
        | DotNetType c -> string c + "::"
        | NpmPackage p -> p
        | ImportedModule i -> string i + "::"
        + (this.Address |> List.rev |> String.concat ".")

    member this.JSAddress =
        match this.Module with
        | StandardLibrary
        | JavaScriptFile _ ->
            Some this.Address
        | _ -> None

    //member this.MapName f =
    //    match this.Address.Value with
    //    | n :: r ->
    //        { this with Address = Hashed (f n :: r) }
    //    | _ ->
    //        failwith "MapName on empty address"

    member this.Sub n =
        { this with Address = n :: this.Address }

    member this.Func n =
        { this with Address = [ n ] }

    member this.Static n =
        { this with Address = [ n; "default" ] }

module internal Instances =
    let uniqueId name i = 
        {
            IdName = Some name
            Id = i
            Mutable = false
            StrongName = true
            Optional = false
            Rest = false
            Type = None
        }

    let GlobalId = uniqueId "globalThis" -1L

    let ImportId = uniqueId "import" -2L

    let SourceTypeId = uniqueId "type" -3L

    let DefaultCtor =
        Constructor { CtorParameters = [] }

    let RuntimeModule = JavaScriptModule { Assembly = "WebSharper.Core.JavaScript"; Name = "Runtime" }

    let DefaultAddress = [ "default" ]

    let GlobalAddress = { Module = StandardLibrary; Address = [] }
    let ErrorAddress = { Module = StandardLibrary; Address = [ "$$ERROR$$" ] }

type Id with
    static member Global() = Instances.GlobalId
    static member Import() = Instances.ImportId
    static member SourceType() = Instances.SourceTypeId

type ConstructorInfo with
    static member Default() = Instances.DefaultCtor

module Address =
    let Runtime a = { Module = Instances.RuntimeModule; Address = [ a ] }
    let RuntimeAddr a = { Module = Instances.RuntimeModule; Address = List.rev a }
    let Lib a = { Module = StandardLibrary; Address = [ a ] }
    let LibAddr a = { Module = StandardLibrary; Address = List.rev a }
    let Global() = Instances.GlobalAddress
    let Error() = Instances.ErrorAddress
    let TypeModuleRoot t = { Module = DotNetType t; Address = [] }
    let TypeDefaultExport t = { Module = DotNetType t; Address = Instances.DefaultAddress }
    let TypeNamedExport t n = { Module = DotNetType t; Address = [ n ] }
    let Import asmName (export: string option, from: string) = 
        let from = if from.EndsWith ".js" then from.[.. from.Length - 4] else from
        let m =
            if from.StartsWith("./") then
                JavaScriptModule { Assembly = asmName; Name = from[2 ..] }
            elif from.StartsWith("../") then    
                let from = from[3 ..]
                match from.IndexOf('/') with
                | -1 -> JavaScriptModule { Assembly = asmName; Name = from }
                | i -> JavaScriptModule { Assembly = from[ .. i - 1]; Name = from[i + 1 ..] }
            else    
                NpmPackage from
        match export with
        | None -> { Module = m; Address = Instances.DefaultAddress }
        | Some x -> { Module = m; Address = [ x ] }

    /// Recognizes a WebSharper Runtime address
    let (|Runtime|_|) (a: Address) =
        match a with
        | { Module = m; Address = [ a ] } when m = Instances.RuntimeModule -> Some a
        | _ -> None

    let (|Global|_|) (a: Address) =
        match a with
        | { Module = StandardLibrary; Address = [ a ] } -> Some a
        | _ -> None
