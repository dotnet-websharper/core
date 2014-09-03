// $begin{copyright}
//
// This file is part of WebSharper
//
// Copyright (c) 2008-2013 IntelliFactory
//
// GNU Affero General Public License Usage
// WebSharper is free software: you can redistribute it and/or modify it under
// the terms of the GNU Affero General Public License, version 3, as published
// by the Free Software Foundation.
//
// WebSharper is distributed in the hope that it will be useful, but WITHOUT
// ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
// FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License
// for more details at <http://www.gnu.org/licenses/>.
//
// If you are unsure which license is appropriate for your use, please contact
// IntelliFactory at http://intellifactory.com/contact.
//
// $end{copyright}

namespace IntelliFactory.WebSharper.InterfaceGenerator

/// Defines meta-objects for describing JavaScript types,
/// in particular object contracts and member signatures.
/// The meta-objects are defined with overloaded operators and
/// helper types to support an F#-embedded DSL.
module Type =
    open System
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
        | GenericType of string * ref<list<Type>>
        | SpecializedType of Type * list<Type>
        | SystemType of R.Type
        | TupleType of list<Type>
        | UnionType of Type * Type

        member this.Item
            with get ([<ParamArray>] x : IType []) =
                SpecializedType (this, [for t in x -> t.Type])

        /// `T?x` constructs a `Parameter` named "x" of type `T`.
        static member op_Dynamic(ty: Type, name: string) =
            {
                Name = Some name
                Type = ty
                Optional = false
            }

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

        /// Converts the type to a parameter.
        member this.Parameter = 
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
            | DeclaredType id ->
                [DeclaredType id]
            | FunctionType x ->
                [ for this in normo x.This do
                    for returnType in norm x.ReturnType do
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
            | GenericType (x, cs) ->
                [GenericType (x, cs)]
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
