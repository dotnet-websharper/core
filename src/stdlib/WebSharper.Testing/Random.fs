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

/// Random test case generators and their combinators.
module WebSharper.Testing.Random

open WebSharper
open WebSharper.JavaScript

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

module Q = WebSharper.Core.Quotations
module R = WebSharper.Core.Reflection
module T = WebSharper.Core.Reflection.Type
module J = WebSharper.Core.JavaScript.Core

module internal Internal =
    let cString s = !~ (J.String s)
    let inline cInt i = !~ (J.Integer (int64 i))
    let cCall t m x = J.Call (t, cString m, x)
    let cCallG l m x = cCall (J.Global l) m x
    let cCallR m x = Choice1Of2 (cCallG ["WebSharper"; "Testing"; "Random"] m x)
    let (|T|_|) n (t: R.TypeDefinition) =
        if t.FullName = n then Some() else None
    let (>>=) (m: Choice<J.Expression, string>) (f: J.Expression -> Choice<J.Expression, string>) =
        match m with
        | Choice1Of2 e -> f e
        | Choice2Of2 _ -> m
    let fail x = Choice2Of2 x : Choice<J.Expression, string>

    let mkGenerator t =
        let rec mkGenerator = function
            | T.Array (t, 1) ->
                mkGenerator t >>= fun x ->
                cCallR "ArrayOf" [x]
            | T.Array _ ->
                fail "Random generators for multidimensional arrays are not supported."
            | T.Concrete (T "Microsoft.FSharp.Core.Unit", []) ->
                cCallR "Const" [!~J.Null]
            | T.Concrete (T "System.Boolean", []) ->
                cCallR "Boolean" []
            | T.Concrete (T "System.Double", []) ->
                cCallR "Float" []
            | T.Concrete (T "System.Int32", []) ->
                cCallR "Int" []
            | T.Concrete (T "System.String", []) ->
                cCallR "String" []
            | T.Concrete (T "Microsoft.FSharp.Collections.FSharpList`1", [t]) ->
                mkGenerator t >>= fun x ->
                cCallR "ListOf" [x]
            | T.Concrete (T "System.Tuple`2", [t1; t2]) ->
                mkGenerator t1 >>= fun x1 ->
                mkGenerator t2 >>= fun x2 ->
                cCallR "Tuple2Of" [x1; x2]
            | T.Concrete (T "System.Tuple`3", [t1; t2; t3]) ->
                mkGenerator t1 >>= fun x1 ->
                mkGenerator t2 >>= fun x2 ->
                mkGenerator t3 >>= fun x3 ->
                cCallR "Tuple3Of" [x1; x2; x3]
            | T.Concrete (t, targs) ->
                fail ("Random generator not supported for type: " + t.FullName)
            | T.Generic x ->
                fail ("Cannot create a random generator for a generic type " + string x)
        match mkGenerator t with
        | Choice1Of2 x -> x
        | Choice2Of2 msg -> failwithf "%A: %s" t msg

    let mkSample g count =
        cCallG ["WebSharper"; "Testing"; "Random"; "Sample"] "Make" [g; count]

    type AutoGeneratorMacro() =
        interface Core.Macros.IMacro with
            member this.Translate(q, tr) =
                match q with
                // Auto<'A>()
                | Q.CallModule({Generics = [t]}, _) -> mkGenerator t
                | _ -> tr q

    type SampleMacro() =
        interface Core.Macros.IMacro with
            member this.Translate(q, tr) =
                match q with
                // new Sample<'A>()
                | Q.NewObject({Generics = [t]}, []) -> mkSample (mkGenerator t) (cInt 100)
                // new Sample<'A>(count)
                | Q.NewObject({Generics = [t]}, [count]) -> mkSample (mkGenerator t) (tr count)
                | _ -> tr q

[<Macro(typeof<Internal.AutoGeneratorMacro>)>]
let Auto<'A>() = X<Generator<'A>>

[<JavaScript>]
type Sample<'A> (data: list<'A>) =

    static member Make<'A> generator count =
        new Sample<'A>(generator, count)

    member this.Data = data

    new (generator: Generator<'A>, count: int) =
        let data =
            [
                for i = 0 to generator.Base.Length - 1 do yield generator.Base.[i]
                for i = 1 to count do yield (generator.Next())
            ]
        new Sample<'A>(data)

    [<Macro(typeof<Internal.SampleMacro>)>]
    new () = new Sample<'A>(Auto<'A>())

    [<Macro(typeof<Internal.SampleMacro>)>]
    new (count: int) = new Sample<'A>(Auto<'A>(), count)

    new (generator: Generator<'A>) = new Sample<'A>(generator, 100)
