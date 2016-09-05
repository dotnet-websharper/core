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

let private SysRandom = System.Random()

[<Inline "Math.random()">]
let private Random () = SysRandom.NextDouble()

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
        Next = fun () -> int (round (Float.Next()))
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

/// Mixes many generators witohut bias.
[<JavaScript>]
let MixMany (gs: Generator<'T>[]) : Generator<'T> =
    {
        Base = Array.concat [| for g in gs -> g.Base |]
        Next =
            let i = ref 0 in
            fun () ->
                i := (!i + 1) % gs.Length
                gs.[!i].Next()
    }

/// Mixes many generators ignoring their bases.
[<JavaScript>]
let MixManyWithoutBases (gs: Generator<'T>[]) : Generator<'T> =
    {
        Base = [||]
        Next =
            let i = ref 0 in
            fun () ->
                i := (!i + 1) % gs.Length
                gs.[!i].Next()
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

/// Boxes the generated data.
[<Inline "$g">]
let Box (g: Generator<'A>) = Map box g

[<JavaScript>]
let private allTypes =
    let bases =
        [|
            Box Int
            Box Float
            Box Boolean
            Box String
        |]
    let compose (gs: Generator<obj>[]) =
        [|
            for g in gs do
                for h in gs do
                    yield Box (Tuple2Of(g, h))
                    for i in gs do
                        yield Box (Tuple3Of(g, h, i))
                yield Box (ListOf g)
                yield Box (ArrayOf g)
        |]
    let composed = compose bases
    Array.append bases composed

/// Generates ints, floats, bools, strings and tuples, lists, arrays, options thereof.
[<JavaScript>]
let Anything : Generator<obj> =
    MixManyWithoutBases allTypes

[<JavaScript>]
let private Choose (gens: Generator<'A>[]) (f: Generator<'A> -> Generator<'B>) : Generator<'B> =
    let gengen = Within 0 (gens.Length - 1) |> Map (fun i -> gens.[i])
    {
        Base = [||]
        Next = fun () ->
            let gen = gengen.Next()
            (f gen).Next()
    }

//module Q = WebSharper.Core.Quotations
//module R = WebSharper.Core.Reflection
//module J = WebSharper.Core.JavaScript.Core

module internal Internal =
    open WebSharper.Core.AST

    type E = Expression
    type T = Type

//    let cString s = !~ (J.String s)
    let inline cInt i = Value (Int i)
    let cCall t m x = Application(ItemGet (t, Value (String m)), x, false, None)
    let cCallG l x = Application(Global l, x, false, None)
    let cCallR m x = Application(Global (["WebSharper"; "Testing"; "Random"] @ [m]), x, true, None)
////    let (|T|) (t: R.TypeDefinition) = t.FullName                                   
    let (>>=) (wrap: (E -> E), m: Choice<E, string> as x) (f: (E -> E) -> E -> (E -> E) * Choice<E, string>) =
        match m with
        | Choice1Of2 e -> f wrap e
        | Choice2Of2 _ -> x
    let fail x = id, Choice2Of2 x : (E -> E) * Choice<E, string>

    let mkGenerator (t: WebSharper.Core.AST.Type) =
        let rec mkGenerator wrap = function
            | T.ArrayType (t, 1) ->
                mkGenerator wrap t >>= fun wrap x ->
                wrap, Choice1Of2 (cCallR "ArrayOf" [x])
            | T.ArrayType _ ->
                fail "Random generators for multidimensional arrays are not supported."
            | T.VoidType ->
                wrap, Choice1Of2 (cCallR "Const" [Value Null])
            | T.ConcreteType { Entity = e } when e.Value.FullName = "System.Boolean" ->
                wrap, Choice1Of2 (cCallR "Boolean" [])
            | T.ConcreteType { Entity = e } when e.Value.FullName = "System.Double" ->
                wrap, Choice1Of2 (cCallR "Float" [])
            | T.ConcreteType { Entity = e } when e.Value.FullName = "System.Int32" ->
                wrap, Choice1Of2 (cCallR "Int" [])
            | T.ConcreteType { Entity = e } when e.Value.FullName = "System.String" ->
                wrap, Choice1Of2 (cCallR "String" [])
            | T.ConcreteType { Entity = e; Generics = [t] } when e.Value.FullName = "Microsoft.FSharp.Collections.FSharpList`1" ->
                mkGenerator wrap t >>= fun wrap x ->
                wrap, Choice1Of2 (cCallR "ListOf" [x])
            | T.TupleType [t1; t2] ->
                mkGenerator wrap t1 >>= fun wrap x1 ->
                mkGenerator wrap t2 >>= fun wrap x2 ->
                wrap, Choice1Of2 (cCallR "Tuple2Of" [x1; x2])
            | T.TupleType [t1; t2; t3] ->
                mkGenerator wrap t1 >>= fun wrap x1 ->
                mkGenerator wrap t2 >>= fun wrap x2 ->
                mkGenerator wrap t3 >>= fun wrap x3 ->
                wrap, Choice1Of2 (cCallR "Tuple3Of" [x1; x2; x3])
            | T.TupleType _ -> fail "Tuple types larger than 3 items are not supported"
            | T.ConcreteType { Entity = e } when e.Value.FullName = "System.Object" ->
                wrap, Choice1Of2 (cCallR "Anything" [])
            | T.TypeParameter _ ->
                wrap, Choice1Of2 (cCallR "Anything" [])
            | T.ConcreteType { Entity = e } when e.Value.FullName = "System.IComparable" ->
                // We use Choose because it is necessary for a given generated value
                // that all occurrences of this type are the same type.
                // With Anything, we could get e.g for IComparable[]: [|1; "test"; (2.3, true)|]
                let id = Id.New(mut = false)
                let wrap' (e: E) =
                    cCallR "Choose" [
                        cCallR "allTypes" []
                        Function([id], Return e)
                    ]
                wrap' >> wrap, Choice1Of2 (Var id)
            | T.ConcreteType { Entity = e; Generics = [t] } when e.Value.FullName = "System.IEquatable`1" ||  e.Value.FullName = "System.IComparable`1" ->
                mkGenerator wrap t
            | T.ConcreteType { Entity = e } when e.Value.FullName = "System.Collections.IEnumerable" ->
                mkGenerator wrap (T.ArrayType (T.TypeParameter 1, 1))
            | T.ConcreteType { Entity = e; Generics = [t] } when e.Value.FullName = "System.Collections.Generic.IEnumerable`1" ->
                mkGenerator wrap (T.ArrayType (t, 1))
            | _ ->
                fail ("Random generator not supported for type: " + t.AssemblyQualifiedName)
        match mkGenerator id t with
        | wrap, Choice1Of2 x -> wrap x
        | _, Choice2Of2 msg -> failwithf "%A: %s" t msg

    let mkSample g count =
        cCallG ["WebSharper"; "Testing"; "Random"; "Sample"; "Make"]  [g; count]

    type AutoGeneratorMacro() =
        inherit Core.Macro()

        override this.TranslateCall(c) =
            mkGenerator c.Method.Generics.Head |> Core.MacroOk

    type SampleMacro() =
        inherit Core.Macro()

        override this.TranslateCtor(c) =
            match c.Arguments with
            | [] -> mkSample (mkGenerator c.DefiningType.Generics.Head) (cInt 100) 
            | [count] -> mkSample (mkGenerator c.DefiningType.Generics.Head) count
            | _ -> failwith "Wrong number of arguments" 
            |> Core.MacroOk

[<Macro(typeof<Internal.AutoGeneratorMacro>)>]
let Auto<'A>() : Generator<'A> =
    {
        Base = [||]
        Next = fun () -> X<'A>
    }

type ObjTest (x: int) =
    
    new () =
        if true then
            new ObjTest(0)
        else
            new ObjTest(3)

[<JavaScript>]
[<Name "WebSharper.Testing.Random.Sample">]
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
