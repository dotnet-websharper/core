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

namespace WebSharper.InterfaceGenerator

/// Defines meta-objects for describing JavaScript types,
/// in particular object contracts and member signatures.
/// The meta-objects are defined with overloaded operators and
/// helper types to support an F#-embedded DSL.
module Type =
    module R = WebSharper.Core.Reflection

    /// Represents type identifiers that are used to match types to classes.
    [<Sealed>]
    type Id (?name) =
        static let mutable last = -1
        let id = System.Threading.Interlocked.Increment(&last)
        
        member val Name = defaultArg name "" with get, set

        override this.GetHashCode() = id

        member private this.Id = id

        override this.Equals(other) =
            match other with
            | :? Id as o -> id = o.Id
            | _ -> false

    /// Represents types used in bindings.
    type Type =
        internal
        | ArrayType of int * Type
        | DeclaredType of Id
        | FunctionType of Function
        | GenericType of Id
        | SpecializedType of Type * list<Type>
        | SystemType of R.Type
        | TupleType of list<Type>
        | UnionType of Type * Type
        | InteropType of Type * InlineTransforms
        | NoInteropType of Type
        | FSFunctionType of Type * Type 
        | DelegateType of list<Type> * option<Type>
        | ChoiceType of Type list
        | OptionType of Type
        | ItemOrArrayType of Type
        | DefiningType

        member this.Item
            with get ([<System.ParamArray>] x : IType []) =
                match this with
                | SpecializedType(g, _) -> SpecializedType (g, [for t in x -> t.Type])
                | _ -> SpecializedType (this, [for t in x -> t.Type])

        /// `T?x` constructs a `Parameter` named "x" of type `T`.
        static member op_Dynamic(this: Type, name: string) =
            {  this.Parameter with Name = Some name }

        /// Constructs tuple types.
        static member ( * ) (a: Type, b: IType) =
            match a with
            | TupleType xs -> TupleType (b.Type :: xs)
            | _ -> TupleType [b.Type; a]

        /// Lifts the type to a parameter and joins it.
        static member ( * ) (this: Type, other: Parameter) =
            this.Parameter * other

        /// Constructs type unions.
        static member ( + ) (this: Type, other: IType) =
            UnionType (this, other.Type)

        /// Constructs an option type.
        static member ( !? ) (this: Type) =
            OptionType this

        /// Converts the type to a parameter.
        member this.Parameter = 
            match this with
            | OptionType t ->
                {
                    Name = None
                    Type = t
                    Optional = true
                }
            | _ ->
                {
                    Name = None
                    Type = this
                    Optional = false
                }

        /// Substitutes DefiningType values to `toType`.
        member this.SubsDefining(toType: Type) =
            let sub (t: Type) = t.SubsDefining toType
            match this with
            | ArrayType (i, t) -> ArrayType (i, sub t) 
            | FunctionType f ->
                FunctionType {
                    ReturnType = sub f.ReturnType
                    Parameters = f.Parameters |> List.map (fun (n, t) -> n, sub t)
                    ParamArray = f.ParamArray |> Option.map sub
                    This       = f.This |> Option.map sub
                }
            | SpecializedType (t, ts) -> SpecializedType (sub t, ts |> List.map sub)
            | TupleType ts -> TupleType (ts |> List.map sub)
            | UnionType (t1, t2) -> UnionType (sub t1, sub t2)
            | InteropType (t, tr) -> InteropType (sub t, tr)
            | NoInteropType t -> NoInteropType (sub t)
            | FSFunctionType (a, r) -> FSFunctionType (sub a, sub r)
            | DelegateType (a, r) -> DelegateType (a |> List.map sub, r)
            | ChoiceType ts -> ChoiceType (ts |> List.map sub)
            | OptionType t -> OptionType (sub t)
            | ItemOrArrayType t -> ItemOrArrayType (sub t)
            | DefiningType -> toType 
            | _ -> this

        interface IParameter with
            member this.Parameter = this.Parameter
  
        interface IParameters with
            member this.Parameters =
                {
                    This = None
                    Variable = None
                    Arguments =
                        match this with
                        | SystemType t when t.FullName = "Microsoft.FSharp.Core.Unit" ->
                            []
                        | TupleType ts ->
                            [ for t in List.rev ts ->
                                (t :> IParameter).Parameter
                            ]
                        | t ->
                            [(t :> IParameter).Parameter]
                }

        interface IType with
            member this.Type = this
      
    and [<ReferenceEquality>] InlineTransforms =
        {
            In : string -> string
            Out : string -> string
        }
        static member (*) (outer: InlineTransforms, inner: InlineTransforms) =
            {
                In = outer.In >> inner.In
                Out = inner.Out >> outer.Out 
            }

    /// Represents a JavaScript function type.
    and Function =        
        {
            ReturnType : Type
            Parameters : list<string * Type>
            ParamArray : option<Type>
            This       : option<Type>
        }

    /// A helper interface implemented by objects that
    /// are equivalent to `Type`.
    and IType =
        inherit IParameter

        /// Converts this object to a `Type`.
        abstract member Type : Type

    /// Represents function parameters.
    and Parameter =
        {
            Name     : option<string>
            Optional : bool
            Type     : Type
        }

        /// Joins parameters.
        static member ( * ) (a: Parameter, b: IParameter) : Parameters =
            {
                This = None
                Arguments = [a; b.Parameter]
                Variable = None
            }

        static member ( !? ) (this: Parameter) =
            { this with Optional = true }

        /// Sets the name.
        static member ( |=> ) (this: Parameter, name: string) =
            { this with Name = Some name }

        interface IParameter with
            member this.Parameter = this

        interface IParameters with
            member this.Parameters =
                {
                    This = None
                    Arguments = [this]
                    Variable = None
                }

    /// A helper interface implemented by objects that are
    /// equivalent to `Parameter`.
    and IParameter =
        inherit IParameters

        /// Converts this object to a `Parameter`.
        abstract member Parameter : Parameter

    /// Represents parameter specifications.
    and Parameters =
        {
            This      : option<Type>
            Arguments : list<Parameter>
            Variable  : option<Type>
        }

        /// Constructs an empty `Parameters` object.
        static member Empty =
            {
                This = None
                Arguments = []
                Variable = None
            }

        /// Appends types or parameters.
        static member ( * ) (a: Parameters, b: IParameter) =
            { a with Arguments = a.Arguments @ [b.Parameter] }

        interface IParameters with
            member this.Parameters = this

    /// A helper interface implemented by objects that are
    /// equivalent to `Parameters`.
    and IParameters =

        /// Converts this object to `Parameters`.
        abstract member Parameters : Parameters

    /// Constructs a new `ClassType`.
    [<System.Obsolete "Use Class or Interface for the initial definitions">]
    let New () = DeclaredType (Id ())

    /// Constructs a new `TupleType`.
    let Tuple (ts: list<#IType>) =
        match [for x in List.rev ts -> x.Type] with
        | []  -> SystemType (R.Type.FromType typeof<unit>)
        | [t] -> t
        | ts  -> TupleType ts

    /// Constructs a `FunctionType`.
    let internal Lambda (parameters: Parameters) (returnType: Type) =
        let args = parameters.Arguments
        let revArgs = List.rev args
        let optArgs =
            revArgs
            |> Seq.takeWhile (fun p -> p.Optional)
            |> Seq.toList
            |> List.rev
        let reqArgs =
            revArgs
            |> Seq.skipWhile (fun p -> p.Optional)
            |> Seq.toList
            |> List.rev
        let rec heads a = 
            match a with
            | [] -> [[]]
            | x :: xs -> [] :: List.map (fun a -> x :: a) (heads xs)
        let rec variants (a: list<Parameter>) =
            match a with
            | [] -> [[]]
            | x :: xs ->
                let sub = variants xs
                let def = List.map (fun a -> x :: a) sub
                if x.Optional then sub @ def else def
        let combinations =
            [ for req in variants reqArgs do
                for opt in heads optArgs do
                    yield req @ opt
            ]
        let signature (ps: list<Parameter>) =
            let args =
                ps
                |> List.mapi (fun i p ->
                    let n = string (char (int 'a' + i))
                    (defaultArg p.Name n, p.Type))
            let var =
                match parameters.Variable with
                | Some t -> Some t
                | None   -> None
            {
                This = parameters.This
                Parameters = args
                ParamArray = var
                ReturnType = returnType
            }            
        seq { for ps in combinations -> FunctionType (signature ps) }
        |> Seq.reduce ( + )

    /// Tests if a type is a function type.
    let IsFunction t =
        match t with
        | FunctionType _ -> true
        | _ -> false

    /// Constructs an array type from the element type.
    let ArrayOf (x: IType) = ArrayType (1, x.Type)

    /// Constructs a multi-dimensional array of.
    let ArrayXD (dim: int) (x: IType) = ArrayType (dim, x.Type)

    /// Detects the `unit` type.
    let (|Unit|NonUnit|) (t: Type) =
        match t with
        | (InteropType (SystemType t, _) | NoInteropType (SystemType t) | SystemType t)
            when t.FullName = "Microsoft.FSharp.Core.Unit" -> Unit
        | _ -> NonUnit

    let Unit = SystemType (R.Type.FromType typeof<unit>)

    /// Recognize arrays, tuples, functions and convert them to WIG Type representation.
    let rec Normalize (t: Type) : Type =
        let rec normSys (t: R.Type) : Type =
            match t with
            | R.Type.Array (eT, rank) ->
                ArrayType (rank, normSys eT)
            | R.Type.Concrete (tD, ts) when tD.FullName.StartsWith "System.Tuple" ->
                TupleType (List.rev (List.map normSys ts))
            | R.Type.Concrete (tD, [d; r]) when tD.FullName.StartsWith "Microsoft.FSharp.Core.FSharpFunc" ->
                let ps =
                    if d.FullName = "Microsoft.FSharp.Core.Unit" then [] else
                        ["x", normSys d]
                FunctionType {
                    ParamArray = None
                    This       = None
                    ReturnType = normSys r
                    Parameters = ps
                }
            | R.Type.Concrete (tD, ((x :: _) as ts)) ->
                let def = normSys (R.Type.Concrete (tD, []))
                SpecializedType (def, List.map normSys ts)
            | _ ->
                SystemType t
        match t with
        | ArrayType (rank, t) ->
            ArrayType (rank, Normalize t)
        | FunctionType x ->
            FunctionType {
                This = x.This |> Option.map Normalize
                ReturnType = x.ReturnType |> Normalize
                ParamArray = x.ParamArray |> Option.map Normalize
                Parameters = x.Parameters |> List.map (fun (n, t) -> n, Normalize t)         
            }
        | SpecializedType (x, y) ->
            SpecializedType (Normalize x, y |> List.map Normalize)
        | SystemType t ->
            normSys t
        | TupleType xs ->
            TupleType (xs |> List.map Normalize)
        | UnionType (x, y) ->
            UnionType (Normalize x, Normalize y)
        | InteropType (t, tr) ->
            InteropType (Normalize t, tr)
        | NoInteropType (InteropType (t, _))
        | NoInteropType t ->
            NoInteropType (Normalize t)
        | _ -> t        

    type private Overload =
        | BasicOverload of Type
        | FunctionOverload of list<Type>

    let private (|SysObj|_|) (t: R.Type) =
        if t.FullName = "System.Object" then Some () else None   

    /// Computes the distinct overloads of a function type, eliminating the Union case.
    /// The returned list is always non-empty.
    let rec GetOverloads (t: Type) : list<Type> =
        let normArgs args =
            match args with
            | [(_, Unit)] -> []
            | _ -> args
        let rec norm t =
            match t with
            | ArrayType (rank, t) ->
                List.map (fun x -> ArrayType (rank, x)) (norm t)
            | FunctionType x ->
                [ for this in normo x.This do
                    for returnType in norm x.ReturnType do
                        // if there is a generic or obj paramarray but no 
                        // other parameters, create a single parameter overload
                        match x.Parameters, x.ParamArray with
                        | [], Some (GenericType _ | SystemType SysObj as pa) ->
                            let normpa = norm pa
                            for p in normpa do
                                yield {
                                    This = this
                                    ReturnType = returnType
                                    ParamArray = None
                                    Parameters = [ "a", p ]
                                }
                                |> FunctionType
                                yield {
                                    This = this
                                    ReturnType = returnType
                                    ParamArray = Some p
                                    Parameters = []
                                }
                                |> FunctionType
                            if List.length normpa > 1 then
                                yield {
                                    This = this
                                    ReturnType = returnType
                                    ParamArray = None
                                    Parameters = []
                                }
                                |> FunctionType
                        | _ -> 
                            let normopa = normo x.ParamArray
                            for paramArray in normopa do
                                let parameters =
                                    norms (List.map snd x.Parameters)
                                for p in parameters do
                                    yield {
                                        This = this
                                        ReturnType = returnType
                                        ParamArray = paramArray
                                        Parameters =
                                            (x.Parameters, p)
                                            ||> List.map2 (fun (x, _) y -> (x, y))
                                            |> normArgs
                                    }
                                    |> FunctionType
                            if List.length normopa > 1 then
                                yield {
                                    This = this
                                    ReturnType = returnType
                                    ParamArray = None
                                    Parameters = []
                                }
                                |> FunctionType
                ]
            | SpecializedType (x, y) ->
                [ for x in norm x do
                    for y in norms y do
                        yield SpecializedType (x, y)
                ]
            | SystemType _ ->
                [Normalize t]
            | TupleType xs ->
                [for n in norms xs -> TupleType xs]
            | UnionType (x, y) ->
                norm x @ norm y
            | InteropType (t, tr) ->
                norm t |> List.map (fun t -> InteropType (t, tr))
            | NoInteropType t ->
                norm t |> List.map NoInteropType
            | t -> [t]
        and normo t =
            match t with
            | None -> [None]
            | Some t -> List.map Some (norm t)
        and norms ts =
            match ts with
            | [] -> [[]]
            | x :: xs ->
                [ for a in norm x do
                    for b in norms xs do
                        yield a :: b ]
        let rec withoutInterop t =
            match t with
            | ArrayType (i, t) -> 
                ArrayType (i, withoutInterop t)
            | FunctionType f ->
                FunctionType {
                    ReturnType = f.ReturnType |> withoutInterop
                    Parameters = f.Parameters |> List.map (fun (n, p) -> n, withoutInterop p)
                    ParamArray = f.ParamArray |> Option.map withoutInterop
                    This       = f.This       |> Option.map withoutInterop
                }
            | SpecializedType (t, ts) ->
                SpecializedType (withoutInterop t, List.map withoutInterop ts)    
            | TupleType ts ->
                TupleType (List.map withoutInterop ts)
            | UnionType (a, b) ->
                UnionType (withoutInterop a, withoutInterop b) 
            | InteropType (t, _)
            | NoInteropType t -> t
            | FSFunctionType (a, r) ->
                FSFunctionType (withoutInterop a, withoutInterop r)   
            | DelegateType (a, r) ->
                DelegateType (List.map withoutInterop a, Option.map withoutInterop r)   
            | ChoiceType ts ->
                ChoiceType (List.map withoutInterop ts)
            | OptionType t ->
                OptionType (withoutInterop t)
            | ItemOrArrayType t ->
                ItemOrArrayType (withoutInterop t)
            | _ -> t
        let key t = 
            match withoutInterop t with
            | FunctionType f ->
                FunctionOverload [
                    for _, p in f.Parameters -> p
                    match f.ParamArray with
                    | Some pa ->
                        yield ArrayType (1, pa)
                    | None -> ()
                ]
            | t -> BasicOverload t
        norm t
        |> Seq.groupBy key
        |> Seq.map (fun (k, gr) ->
            match k with      
            | BasicOverload _ -> Seq.head gr
            | FunctionOverload _ ->
                if Seq.length gr = 1 then
                    Seq.head gr
                else
                    let getReturnType t =
                        match t with 
                        | FunctionType f -> f.ReturnType
                        | NoInteropType (FunctionType f) ->
                            match f.ReturnType with
                            | NoInteropType _ as r -> r
                            | InteropType(r, _)
                            | r -> t
                        | InteropType _ -> failwith "The function type defining a method signature cannot use WithInterop."
                        | _ -> failwith "The type defining a method signature must be a function."
                    let groupReturnTypes =
                        gr |> Seq.map getReturnType |> Seq.distinct 
                        |> Seq.reduce (fun a b -> UnionType(a, b))
                    match Seq.head gr with
                    | NoInteropType (FunctionType g) ->
                        NoInteropType(FunctionType { g with ReturnType = groupReturnTypes })                        
                    | FunctionType g ->
                        FunctionType { g with ReturnType = groupReturnTypes }
                    | _ -> failwith "unreachable"
        )
        |> Seq.toList

    let private onlyThisTransform =
        {
            In = fun x -> "$wsruntime.CreateFuncWithOnlyThis(" + x + ")"
            Out = fun x -> "function(obj) { return (" + x + ").call(obj); }"
        }

    let private thisTransform =
        {
            In = fun x -> "$wsruntime.CreateFuncWithThis(" + x + ")"
            Out = fun x -> "function(obj) { return $wsruntime.Bind(" + x + ", obj); }"
        }

    let private thisArgsTransform =
        {
            In = fun x -> "$wsruntime.CreateFuncWithThisArgs(" + x + ")"
            Out = fun x -> "function(obj) { return function(args) { return $wsruntime.Apply(" + x + ", obj, args); }; }"
        }

    let private argsTransform = 
        {
            In = fun x -> "$wsruntime.CreateFuncWithArgs(" + x + ")"
            Out = fun x -> "function(args) { return $wsruntime.Apply(" + x + ", this, args); }"
        }

    let private restTransform (i: int) =
        {
            In =
                match i with
                | 0 -> 
                    fun x -> "$wsruntime.CreateFuncWithArgs(" + x + ")"
                | _ -> 
                    fun x -> "$wsruntime.CreateFuncWithRest(" + string i + ", " + x + ")"
            Out = 
                match i with
                | 0 ->
                    fun x -> "function(rest) { return $wsruntime.Apply(" + x + ", null, rest); }" 
                | 1 ->
                    fun x -> "function(args) { return $wsruntime.Apply(" + x + ", null, [args[0]].concat(args[1])); }"                  
                | _ ->
                    fun x -> "function(args) { return $wsruntime.Apply(" + x + ", null, args.slice(0, " + string i + ").concat(args[ " + string i + "])); }" 
        }
        
    let (|UnionOf|_|) t =
        let rec getTypes t =
            match t with
            | UnionType (t1, t2) -> Seq.append (getTypes t1) (getTypes t2)
            | _ -> Seq.singleton t
        match getTypes t |> Seq.distinct |> List.ofSeq with
        | [_] -> None
        | ts -> Some ts

    let private NumberTypes = 
        Set [
            "Byte"
            "SByte"
            "Int16"
            "Int32"
            "UInt16"
            "UInt32"
            "Int64"
            "UInt64" 
            "Char"
            "Double"
            "Single"
            "TimeSpan"
            "DateTime"
        ]

    let rec GetJSType t = 
        let (|StartsWith|_|) (start: string) (s: string) =
            if s.StartsWith start then Some (s.Substring(start.Length)) else None
        match t with
        | ArrayType _ -> Some "0"
        | TupleType ts -> Some (string ts.Length)
        | FunctionType _
        | FSFunctionType _
        | DelegateType _ -> Some "'function'"
        | SpecializedType (t, _)
        | InteropType (t, _)
        | NoInteropType t -> GetJSType t
        | SystemType t ->
            match t.FullName with
            | StartsWith "System." n ->
                match n with
                | "Guid"
                | "String" -> Some "'string'"
                | "Boolean" -> Some "'boolean'"
                | "Object" -> None
                | n when NumberTypes.Contains n -> Some "'number'"
                | _ -> Some "'object'"
            | StartsWith "System.Collections.Generic." n ->
                match n with
                | "List`1"
                | "Queue`1"
                | "Stack`1" -> Some "0"
                | _ -> Some "'object'"
            | StartsWith "WebSharper.JavaScript." n ->
                match n with
                | "Array" -> Some "0"
                | "Boolean" -> Some "'boolean'"
                | "Number" -> Some "'number'"
                | "String" -> Some "'string'"
                | n when n.StartsWith "Func" -> Some "'function'"
                | _ -> Some "'object'"
            | "Microsoft.FSharp.Core.Unit" -> Some "'undefined'"
            | _ -> Some "'object'"
        | DeclaredType _
        | DefiningType -> Some "'object'"
        | _ -> None

    let TransformValue opt csharp t =
        match t with
        | FunctionType f when not csharp ->
            let trFunc args tr = 
                match f.This with
                | None -> InteropType (FSFunctionType (args, f.ReturnType), tr)    
                | Some this -> InteropType (FSFunctionType (this, FSFunctionType (args, f.ReturnType)), tr)
            match f.This, f.Parameters.Length, f.ParamArray with
            | None, l, None when l > 1 -> 
                trFunc (TupleType (f.Parameters |> List.map snd |> List.rev)) argsTransform   

            | None, 0, Some pa ->
                trFunc (ArrayType (1, pa)) (restTransform 0)
            | None, 1, Some pa ->
                trFunc (TupleType [ArrayType (1, pa); snd f.Parameters.[0]]) (restTransform 1)
            | None, l, Some pa ->
                trFunc (TupleType (ArrayType (1, pa) :: (f.Parameters |> List.map snd |> List.rev))) (restTransform l)       
            
            | Some this, 0, None -> 
                InteropType (FSFunctionType (this, f.ReturnType), onlyThisTransform)
            | Some _, 1, None -> 
                trFunc (snd f.Parameters.[0]) thisTransform
            | Some _, _, None -> 
                trFunc (TupleType (f.Parameters |> List.map snd |> List.rev)) thisArgsTransform
            
            | Some _, 0, Some pa ->
                trFunc (ArrayType (1, pa)) (thisTransform * restTransform 0)
            | Some _, 1, Some pa ->
                trFunc (TupleType [ArrayType (1, pa); snd f.Parameters.[0]]) (thisTransform * restTransform 1)
            | Some _, l, Some pa ->
                trFunc (TupleType (ArrayType (1, pa) :: (f.Parameters |> List.map snd |> List.rev))) (thisTransform * restTransform l)
            | _ -> t
        | UnionOf ts ->
            if (ts |> Seq.exists (function ArrayType _ -> true | _ -> false))
                && (ts |> Seq.exists (function TupleType _ -> true | _ -> false)) then t else
            let tts = ts |> Seq.choose GetJSType |> Seq.distinct |> List.ofSeq
            if List.length tts = List.length ts then
                let ts, tts =
                    (ts, tts) ||> List.zip |> List.sortBy snd |> List.unzip
                ChoiceType ts
            else t
        | _ -> t

    let TransformOption csharp t =
        match t with
        | OptionType t -> OptionType (TransformValue true csharp t)
        | _ -> TransformValue false csharp t

    let HasFSharpFuncOverload f =
        seq {
            for _, p in f.Parameters -> p
            match f.ParamArray with
            | Some p -> yield p
            | None -> ()
        }
        |> Seq.exists (
            function
            | FunctionType { Parameters = ([] | [_]); This = None; ParamArray = None } -> false
            | FunctionType _ -> true
            | _ -> false
        )

    let TransformArgs t =
        match t with 
        | FunctionType f ->
            [
                for c in (if HasFSharpFuncOverload f then [false; true] else [true]) ->
                    FunctionType 
                        { f with
                            Parameters = f.Parameters |> List.map (fun (n, p) -> n, TransformValue false c p)
                            ReturnType = f.ReturnType |> TransformOption c
                        }
                    , c
            ]
        | _ -> [t, false]

    let WithFSharpOverloads t =
        match t with 
        | FunctionType f when HasFSharpFuncOverload f ->
            [t, false; t, true]
        | _ -> [t, true]
