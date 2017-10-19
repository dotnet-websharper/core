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

/// Filter the values of a generator by a predicate.
[<JavaScript>]
let SuchThat f r =
    let rec next() =
        let x = r.Next()
        if f x then x else next()
    {
        Base = Array.filter f r.Base
        Next = next
    }

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

/// Generates random resizable arrays.
[<JavaScript>]
let ResizeArrayOf (generator: Generator<'T>) : Generator<ResizeArray<'T>> =
    ArrayOf generator
    |> Map ResizeArray

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

/// Generates random readable strings.
[<JavaScript>]
let StringReadable: Generator<string> =
    {
        Base = [| "" |]
        Next = fun () ->
            let len = Natural.Next() % 100
            let cs = Array.init len (fun _ ->
                char ((Int.Next() % 95) + 32))
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

/// Promotes a triple of generators to a generator of triples.
[<JavaScript>]
let Tuple4Of (a: Generator<'A>, b: Generator<'B>, c: Generator<'C>, d: Generator<'D>) :
                Generator<'A*'B*'C*'D> =
    {
        Base =
            [|
                for xa in a.Base do
                    for xb in b.Base do
                        for xc in c.Base do
                            for xd in d.Base do
                            yield (xa, xb, xc, xd)
            |]
        Next = fun () -> (a.Next(), b.Next(), c.Next(), d.Next())
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

type private Box =
    static member Box (g: Generator<'A>) = Map box g

[<Proxy(typeof<Box>)>]
type private BoxProxy =
    static member Box (g: Generator<'A>) = As<Generator<obj>> g

[<Inline>]
let Box g = Box.Box g

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

module internal Internal =
    open WebSharper.Core
    open WebSharper.Core.AST

    type E = Expression
    type T = Type

    let inline (@@) t a = GenericType t a
    let inline (!@) t = NonGenericType t
    let asm = "WebSharper.Testing"
    let ty name = Hashed { Assembly = asm; FullName = name }
    let random = NonGeneric (ty "WebSharper.Testing.Random")
    let sampleOf t = ty "WebSharper.Testing.Random+Sample`1" @@[t]
    let generatorOf t = ty "WebSharper.Testing.Random+Generator`1" @@[t]
    let staticSampleOf t = Generic (ty "WebSharper.Testing.Random+Sample`1") [t]
    let arrayOf t = ArrayType(t, 1)
    let tupleOf ts = TupleType(ts, false)
    let (^->) tin tout = FSharpFuncType(tin, tout)
    let meth name args res = fun param ->
        Generic
            (Hashed { MethodName = name; Parameters = args; ReturnType = res; Generics = List.length param })
            param
    let callR meth args = E.Call(None, random, meth, args)
    let inline cInt i = Value (Int i)

    let nonGenericGen name e =
        let m = meth name [] (generatorOf !@e)
        callR (m []) []
    let genericGen name t genType x =
        let argType = generatorOf (TypeParameter 0)
        let resType = generatorOf (genType [TypeParameter 0])
        let m = meth name [argType] resType
        callR (m [t]) [x]
    let genericGen2 name t1 t2 genType x1 x2 =
        let arg1Type = generatorOf (TypeParameter 0)
        let arg2Type = generatorOf (TypeParameter 1)
        let resType = generatorOf (genType [TypeParameter 0; TypeParameter 1])
        let m = meth name [arg1Type; arg2Type] resType
        callR (m [t1; t2]) [x1; x2]
    let genericGen3 name t1 t2 t3 genType x1 x2 x3 =
        let arg1Type = generatorOf (TypeParameter 0)
        let arg2Type = generatorOf (TypeParameter 1)
        let arg3Type = generatorOf (TypeParameter 2)
        let resType = generatorOf (genType [TypeParameter 0; TypeParameter 1; TypeParameter 2])
        let m = meth name [arg1Type; arg2Type; arg3Type] resType
        callR (m [t1; t2; t3]) [x1; x2; x3]

    let (>>=) (wrap: (E -> E), m: Choice<E, string> as x) (f: (E -> E) -> E -> (E -> E) * Choice<E, string>) =
        match m with
        | Choice1Of2 e -> f wrap e
        | Choice2Of2 _ -> x
    let fail x = id, Choice2Of2 x : (E -> E) * Choice<E, string>

    let mkGenerator (ty: WebSharper.Core.AST.Type) =
        let rec mkGenerator wrap = function
            | T.ArrayType (t, 1) ->
                mkGenerator wrap t >>= fun wrap x ->
                wrap, Choice1Of2 (genericGen "ArrayOf" t (fun t -> ArrayType(List.head t, 1)) x)
            | T.ArrayType _ ->
                fail "Random generators for multidimensional arrays are not supported."
            | T.VoidType ->
                let m = meth "Const" [TypeParameter 0] (generatorOf (TypeParameter 0))
                wrap, Choice1Of2 (callR (m [ty]) [Value Null])
            | T.ConcreteType { Entity = e } when e.Value.FullName = "System.Boolean" ->
                wrap, Choice1Of2 (nonGenericGen "Boolean" e)
            | T.ConcreteType { Entity = e } when e.Value.FullName = "System.Double" ->
                wrap, Choice1Of2 (nonGenericGen "Float" e)
            | T.ConcreteType { Entity = e } when e.Value.FullName = "System.Int32" ->
                wrap, Choice1Of2 (nonGenericGen "Int" e)
            | T.ConcreteType { Entity = e } when e.Value.FullName = "System.String" ->
                wrap, Choice1Of2 (nonGenericGen "String" e)
            | T.ConcreteType { Entity = e; Generics = [t] } when e.Value.FullName = "Microsoft.FSharp.Collections.FSharpList`1" ->
                mkGenerator wrap t >>= fun wrap x ->
                wrap, Choice1Of2 (genericGen "ListOf" t ((@@) Definitions.FSharpList) x)
            | T.ConcreteType { Entity = e; Generics = [t] } when e.Value.FullName = "System.Collections.Generic.List`1" ->
                mkGenerator wrap t >>= fun wrap x ->
                wrap, Choice1Of2 (genericGen "ResizeArrayOf" t ((@@) Definitions.ResizeArray) x)
            | T.TupleType ([t1; t2], _) ->
                mkGenerator wrap t1 >>= fun wrap x1 ->
                mkGenerator wrap t2 >>= fun wrap x2 ->
                wrap, Choice1Of2 (genericGen2 "Tuple2Of" t1 t2 tupleOf x1 x2)
            | T.TupleType ([t1; t2; t3], _) ->
                mkGenerator wrap t1 >>= fun wrap x1 ->
                mkGenerator wrap t2 >>= fun wrap x2 ->
                mkGenerator wrap t3 >>= fun wrap x3 ->
                wrap, Choice1Of2 (genericGen3 "Tuple3Of" t1 t2 t3 tupleOf x1 x2 x3)
            | T.TupleType _ -> fail "Tuple types larger than 3 items are not supported"
            | T.ConcreteType { Entity = e } when e.Value.FullName = "System.Object" ->
                wrap, Choice1Of2 (nonGenericGen "Anything" Definitions.Object)
            | T.TypeParameter _ ->
                wrap, Choice1Of2 (nonGenericGen "Anything" Definitions.Object)
            | T.ConcreteType { Entity = e } when e.Value.FullName = "System.IComparable" ->
                // We use Choose because it is necessary for a given generated value
                // that all occurrences of this type are the same type.
                // With Anything, we could get e.g for IComparable[]: [|1; "test"; (2.3, true)|]
                let id = Id.New(mut = false)
                let wrap' (e: E) =
                    let mAllTypes =
                        meth "get_allTypes" [] (arrayOf (generatorOf !@Definitions.Object))
                    let a = TypeParameter 0
                    let b = TypeParameter 1
                    let mChoose =
                        meth "Choose" [
                            arrayOf (generatorOf a)
                            generatorOf a ^-> generatorOf b
                        ] (generatorOf b)
                    callR (mChoose [!@Definitions.Object; !@Definitions.Object]) [
                        callR (mAllTypes []) []
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
                fail ("Random generator not supported for type: " + ty.AssemblyQualifiedName)
        match mkGenerator id ty with
        | wrap, Choice1Of2 x -> wrap x
        | _, Choice2Of2 msg -> failwithf "%A: %s" ty msg

    let mkSample t g count =
        let a = TypeParameter 1
        let m = meth "Make" [generatorOf a; !@Definitions.Int] (sampleOf a)
        E.Call(None, staticSampleOf t, m [ t ], [g; count])

    type AutoGeneratorMacro() =
        inherit Core.Macro()

        override this.TranslateCall(c) =
            mkGenerator c.Method.Generics.Head |> Core.MacroOk

    type SampleMacro() =
        inherit Core.Macro()

        override this.TranslateCtor(c) =
            let t = c.DefiningType.Generics.Head
            match c.Arguments with
            | [] -> mkSample t (mkGenerator t) (cInt 100) 
            | [count] -> mkSample t (mkGenerator t) count
            | _ -> failwith "Wrong number of arguments" 
            |> Core.MacroOk

[<Macro(typeof<Internal.AutoGeneratorMacro>)>]
let Auto<'A>() : Generator<'A> =
    {
        Base = [||]
        Next = fun () -> X<'A>
    }

[<JavaScript>]
[<Name "WebSharper.Testing.Random.Sample">]
type Sample<'A> (data: list<'A>) =

    static member Make<'T> generator count =
        new Sample<'T>(generator, count)

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
