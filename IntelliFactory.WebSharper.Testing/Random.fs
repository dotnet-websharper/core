// $begin{copyright}
//
// This file is part of WebSharper
//
// Copyright (c) 2008-2012 IntelliFactory
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

/// Random test case generators and their combinators.
module IntelliFactory.WebSharper.Testing.Random

open IntelliFactory.WebSharper

/// A test case generator.
type Generator<'T> =
    {
        /// An array of values that must be tested against.
        Base: 'T []
        /// A function generating a new random value.
        Next: unit -> 'T
    }
/// Maps a function over a generator.
[<JavaScript>]
let Map f gen =
    {
        Base = Array.map f gen.Base
        Next = gen.Next >> f
    }

/// Logical implication.
[<JavaScript>]
let Implies a b =
    not a || b

/// Logical implication (alias).
[<Name "Imply">]
[<JavaScript>]
let ( ==> ) a b =
    Implies a b

[<Inline "Math.random()">]
let private Random () = X<double>

/// Standard uniform distribution sampler.
[<JavaScript>]
let StandardUniform : Generator<float> =
    {
        Base = [||]
        Next = Random
    }

/// Exponential distribution sampler.
[<JavaScript>]
let Exponential (lambda: float) : Generator<float> =
    {
        Base = [||]
        Next =
            fun () ->
                let p = StandardUniform.Next()
                - log (1. - p) / lambda
    }

/// Generates random booleans.
[<JavaScript>]
let Boolean : Generator<bool> =
    {
        Base = [| true; false |]
        Next = fun () -> StandardUniform.Next() > 0.5
    }

/// Generates random doubles.
[<JavaScript>]
let Float : Generator<float> =
    {
        Base = [| 0. |]
        Next =
            fun () ->
                let sign = if Boolean.Next() then 1. else -1.
                sign * Exponential(0.1).Next()
    }

/// Generates random doubles, including corner cases.
[<JavaScript>]
let FloatExhaustive : Generator<float> =
    {
        Base = [| 0.; nan; infinity; -infinity |]
        Next = fun () -> Float.Next()
    }

/// Generates random int values.
[<JavaScript>]
let Int : Generator<int> =
    {
        Base = [| 0; 1; -1 |]
        Next = fun () -> As<int> (round (Float.Next()))
    }

/// Generates random natural numbers (0, 1, ..).
[<JavaScript>]
let Natural : Generator<int> =
    {
        Base = [| 0; 1 |]
        Next = abs << Int.Next
    }

/// Generates integers within a range.
[<JavaScript>]
let Within (low: int) (hi: int) : Generator<int> =
    {
        Base = [| low; hi |]
        Next =
            fun () ->
                (Natural.Next() % (hi - low)) + low
    }

/// Generates integers within a range.
[<JavaScript>]
let FloatWithin (low: float, hi: float) : Generator<float> =
    {
        Base = [| low; hi |]
        Next =
            fun () ->
                low + (hi - low) * Random ()
    }

/// Generates random arrays.
[<JavaScript>]
let ArrayOf (generator: Generator<'T>) : Generator<'T[]> =
    {
        Base = [| [||] |]
        Next =
            fun () ->
                let len = Natural.Next() % 100
                Array.init len (fun _ -> generator.Next())
    }

/// Generates random lists.
[<JavaScript>]
let ListOf (generator: Generator<'T>) : Generator<list<'T>> =
    ArrayOf generator
    |> Map Array.toList

/// Generates random strings.
[<JavaScript>]
let String: Generator<string> =
    {
        Base = [| "" |]
        Next = fun () ->
            let len = Natural.Next() % 100
            let cs = Array.init len (fun _ ->
                char (Int.Next() % 256))
            new System.String(cs)
    }

/// Generates random strings including nulls.
[<JavaScript>]
let StringExhaustive =
    {
        Base = [| null; "" |]
        Next = String.Next
    }

/// Promotes a tuple of generators to a generator of tuples.
[<JavaScript>]
let Tuple2Of (a: Generator<'A>, b: Generator<'B>) : Generator<'A*'B> =
    {
        Base =
            [|
                for x in a.Base do
                    for y in b.Base do
                        yield (x, y)
            |]
        Next = fun () -> (a.Next(), b.Next())
    }

/// Promotes a triple of generators to a generator of triples.
[<JavaScript>]
let Tuple3Of (a: Generator<'A>, b: Generator<'B>, c: Generator<'C>) :
                Generator<'A*'B*'C> =
    {
        Base =
            [|
                for x in a.Base do
                    for y in b.Base do
                        for z in c.Base do
                            yield (x, y, z)
            |]
        Next = fun () -> (a.Next(), b.Next(), c.Next())
    }

/// Creates a generator with a random uniform distribution over a set
/// of values supplied in the `seeds` array.
[<JavaScript>]
let OneOf (seeds: 'T[]) : Generator<'T> =
    {
        Base = seeds
        Next =
            let index = Within 1 seeds.Length
            fun () -> seeds.[index.Next() - 1]
    }

/// Mixes two generators without bias.
[<JavaScript>]
let Mix (a: Generator<'T>) (b: Generator<'T>) : Generator<'T> =
    {
        Base = Array.append a.Base b.Base
        Next =
            let left = ref false in
            fun () ->
                left := not !left
                if !left then a.Next() else b.Next()
    }

/// Creates a generator that always generates the given value.
[<JavaScript>]
let Const x =
    {
        Base = [| x |]
        Next = fun () -> x
    }

/// Promotes a generator to a generator of options.
[<JavaScript>]
let OptionOf (generator: Generator<'A>) : Generator<option<'A>> =
    Mix (Const None) (Map Some generator)



