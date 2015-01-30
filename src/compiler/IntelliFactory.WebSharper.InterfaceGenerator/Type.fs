// $begin{copyright}
//
// This file is part of WebSharper
//
// Copyright (c) 2008-2014 IntelliFactory
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

namespace IntelliFactory.WebSharper.InterfaceGenerator

/// Defines meta-objects for describing JavaScript types,
/// in particular object contracts and member signatures.
/// The meta-objects are defined with overloaded operators and
/// helper types to support an F#-embedded DSL.
module Type =
    module R = IntelliFactory.WebSharper.Core.Reflection

    /// Represents type identifiers that are used to match types to classes.
    [<Sealed>]
    type Id() = class end

    /// Represents types used in bindings.
    type Type =
        internal
        | ArrayType of int * Type
        | DeclaredType of Id
        | FunctionType of Function
        | GenericType of int //of string * ref<list<Type>>
        | SpecializedType of Type * list<Type>
        | SystemType of R.Type
        | TupleType of list<Type>
        | UnionType of Type * Type
        | InteropType of Type * InlineTransforms
        | FSFunctionType of Type * Type * Type option
        | ArgumentsType of Type
        | ChoiceType of Type list
        | OptionType of Type
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
            InTranform : string -> string
            OutTransform : string -> string
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
        static member ( * ) (a: Parameter, b: IParameter) =
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
        let rec heads = function
            | [] -> [[]]
            | x :: xs -> [] :: List.map (fun a -> x :: a) (heads xs)
        let rec variants : list<Parameter> -> _ = function
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
        | SystemType t when t.FullName = "Microsoft.FSharp.Core.Unit" -> Unit
        | _ -> NonUnit

    let Unit = SystemType (R.Type.FromType typeof<unit>)

    /// Computes the normal forms for a type by eliminating the Union case.
    /// The returned list is always non-empty.
    let rec Normalize (t: Type) : list<Type> =
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
        let normArgs args =
            match args with
            | [(_, Unit)] -> []
            | _ -> args
        let rec norm = function
            | ArrayType (rank, t) ->
                List.map (fun x -> ArrayType (rank, x)) (norm t)
            | FunctionType x ->
                [ for this in normo x.This do
                    for returnType in norm x.ReturnType do
                        // for if there is a generic or obj[] paramarray but no 
                        // other parameters, create a single parameter overload
                        match x.Parameters, x.ParamArray with
                        | [], Some (GenericType _ | ArrayType (1, _) as pa) ->
                            for p in norm pa do
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
                        | _ -> 
                        for paramArray in normo x.ParamArray do
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
                ]
            | SpecializedType (x, y) ->
                [ for x in norm x do
                    for y in norms y do
                        yield SpecializedType (x, y)
                ]
            | SystemType t ->
                [normSys t]
            | TupleType xs ->
                [for n in norms xs -> TupleType xs]
            | UnionType (x, y) ->
                norm x @ norm y
            | InteropType (t, tr) ->
                norm t |> List.map (fun t -> InteropType (t, tr))
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
        norm t

    type private Overload =
        | BasicOverload of Type
        | FunctionOverload of list<Type> * option<Type>

    /// Computes the distinct overloads for a type.
    let DistinctOverloads (ts: list<Type>) : list<Type> =
        let key = function
            | FunctionType f ->
                FunctionOverload (List.map snd f.Parameters, f.ParamArray)
            | t -> BasicOverload t
        ts
        |> Seq.distinctBy key
        |> Seq.toList

    let private argsTransform = 
        {
            InTranform = fun x -> "$wsruntime.CreateFuncWithArgs(" + x + ")"
            OutTransform = fun x -> "function(args) { return (" + x + ").apply(null, args) }"
        }

    let private thisArgsTransform = 
        {
            InTranform = fun x -> "$wsruntime.CreateFuncWithThisArgs(" + x + ")"
            OutTransform = fun x -> "function(args) { return (" + x + ").apply(this, args) }"
        }

    let (|UnionOf|_|) t =
        let rec getTypes t =
            match t with
            | UnionType (t1, t2) -> Seq.append (getTypes t1) (getTypes t2)
            | _ -> Seq.singleton t
        match getTypes t |> Seq.distinct |> List.ofSeq with
        | [_] -> None
        | ts -> Some ts

    let rec GetJSType t = 
        match t with
        | ArrayType _
        | TupleType _ -> Some "array"
        | FunctionType _
        | FSFunctionType _ -> Some "function"
        | SpecializedType (t, _)
        | InteropType (t, _) -> GetJSType t
        | SystemType t ->
            match t.FullName with
            | "System.String" -> Some "string"
            | "System.Boolean" -> Some "boolean"
            | "System.Int32"
            | "System.Int64"
            | "System.Double"
            | "System.Byte" -> Some "number"
            | _ -> Some "object"
        | DeclaredType _
        | ArgumentsType _ 
        | DefiningType -> Some "object"
        | _ -> None

    let TransformValue t =
        match t with
        | FunctionType f ->
            let getTransform() =
                match f.This with
                | None -> argsTransform
                | Some _ -> thisArgsTransform
            match f.Parameters.Length, f.ParamArray with
            | l, None when l > 1 -> 
                InteropType (FSFunctionType (TupleType (f.Parameters |> List.map snd), f.ReturnType, f.This), getTransform())
            | 0, Some pa -> 
                InteropType (FSFunctionType (ArgumentsType pa, f.ReturnType, f.This), getTransform())
            | _ -> t
        | UnionOf ts ->
            let tts = ts |> Seq.choose GetJSType |> Seq.distinct |> List.ofSeq
            if List.length tts = List.length ts then
                InteropType (ChoiceType ts, 
                    {
                        InTranform = fun x -> x + ".$0"
                        OutTransform = fun x -> "$wsruntime.UnionByType([\"" + String.concat "\", \"" tts + "\"]," + x + ")"    
                    }
                )
            else t
        | _ -> t

    let TransformOption t =
        match t with
        | OptionType t -> OptionType (TransformValue t)
        | _ -> TransformValue t

    let TransformArgs t =
        match t with 
        | FunctionType f ->
            FunctionType 
                { f with
                    Parameters = f.Parameters |> List.map (fun (n, p) -> n, TransformValue p)
                    ReturnType = f.ReturnType |> TransformValue   
                }
        | _ -> t
