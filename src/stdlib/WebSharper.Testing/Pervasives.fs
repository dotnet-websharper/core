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

/// Common utilities for testing.
[<AutoOpen>]
module WebSharper.Testing.Pervasives

open WebSharper
open WebSharper.JavaScript

module internal Internal =

    open WebSharper.Core
    open WebSharper.Core.AST

    open WebSharper.Testing.RandomValues.Internal

    let asserter = ty "WebSharper.QUnit+Asserter"
    let runnerOf t = !@asserter ^-> Definitions.FSharpChoice 2 @@[t; Definitions.FSharpAsync @@[t]]

    type TestPropertyMacro() =
        inherit Macro()

        let m =
            let t = TypeParameter 0
            let a = TypeParameter 1
            let b = TypeParameter 2
            meth "PropertyWithSample" [runnerOf a; a ^-> sampleOf t; a ^-> t ^-> runnerOf b] (runnerOf a)

        override this.TranslateCall(c) =
            let t = List.head c.Method.Generics
            match c.Arguments with  
            | [runner; gen; attempt] ->
                let id = Id.New(mut = false)
                Call(c.This, c.DefiningType, m c.Method.Generics,
                    [
                        runner
                        Function([id], true, Some t, 
                            Return (mkSample t (Appl(gen, [Var id], Pure, Some 1)) (cInt 100)))
                        attempt
                    ]
                )
                |> MacroOk
            | [runner; attempt] ->
                Call(c.This, c.DefiningType, m c.Method.Generics,
                    [
                        runner
                        Function([], true, Some t, Return (mkSample t (mkGenerator c.Method.Generics.Head) (cInt 100)))
                        attempt
                    ]
                )
                |> MacroOk
            | _ -> MacroFallback

    type PropertyMacro() =
        inherit Macro()

        let pervasives = NonGeneric (ty "WebSharper.Testing.Pervasives")
        let m =
            let a = TypeParameter 0
            let b = TypeParameter 1
            meth "PropertyWith" [
                !@Definitions.String
                generatorOf a
                a ^-> runnerOf b
            ] VoidType

        override this.TranslateCall(c) =
            match c.Arguments with 
            | [name; f] ->
                Call(None, pervasives, m c.Method.Generics, [name; mkGenerator c.Method.Generics.Head; f])
                |> MacroOk
            | _ -> MacroFallback

type TestCategory = internal { name : string; run : (unit -> unit) }

[<JavaScript>]                   
type TestCategoryBuilder(name: string) =

    [<Inline>]
    member this.Delay(f) = { name = name; run = f }

    [<Inline>]
    member this.Zero() = ()

    [<Inline "QUnit.module($s.name),$s">]
    member this.Run(s: TestCategory) =
        s

[<JavaScript>]
let TestCategory name = new TestCategoryBuilder(name)

// This could simply be (Asserter -> Async<'A>), but since QUnit's performance
// degrades a lot when used in asynchronous mode, we want to use it in
// synchronous mode whenever possible (ie. when all assertions in a test
// are synchronous).
type Runner<'A> = QUnit.Asserter -> Choice<'A, Async<'A>>

[<JavaScript>]
module private Runner =

    let ToAsync x =
        match x with
        | Choice1Of2 args -> async { return args }
        | Choice2Of2 args -> args

    let Map f x =
        match x with
        | Choice1Of2 args -> Choice1Of2 (f args)
        | Choice2Of2 args -> Choice2Of2 (async {
            let! args = args
            return f args
        })

    let Bind f x =
        match x with
        | Choice1Of2 args -> f args
        | Choice2Of2 args -> Choice2Of2 (async {
            let! args = args
            return! ToAsync (f args)
        })

    let MapAsync f x =
        match x with
        | Choice1Of2 args -> Choice2Of2 (f args)
        | Choice2Of2 args -> Choice2Of2 (async {
            let! args = args
            return! f args
        })

    let AddTest t r =
        ()
        fun asserter ->
            r asserter |> Map (fun args ->
                t asserter args
                args)

    let AddTestAsync t r =
        ()
        fun asserter ->
            r asserter |> MapAsync (fun args -> async {
                do! t asserter args
                return args
            })

    let WithTimeout timeOut a = a // TODO: enable timeout
//        async {
//            let! child = Async.StartChild (a, timeOut)
//            return! child
//        }

    let Expect (assertionCount: 'A -> int) (r: Runner<'A>) =
        r |> AddTest (fun asserter args ->
            asserter.Expect(assertionCount args)
        )

    let Equal (actual: 'A -> 'T) (expected: 'A -> 'T) (r: Runner<'A>) =
        r |> AddTest (fun asserter args ->
            let actual = actual args
            let expected = expected args
            asserter.Push((actual = expected), actual, expected)
        )

    let EqualMsg (actual: 'A -> 'T) (expected: 'A -> 'T) (message: string) (r: Runner<'A>) =
        r |> AddTest (fun asserter args ->
            let actual = actual args
            let expected = expected args
            asserter.Push((actual = expected), actual, expected, message)
        )

    let EqualAsync (actual: 'A -> Async<'T>) (expected: 'A -> 'T) (r: Runner<'A>) =
        r |> AddTestAsync (fun asserter args -> async {
            let expected = expected args
            let! actual = actual args 
            return asserter.Push((actual = expected), actual, expected)
        })

    let EqualMsgAsync (actual: 'A -> Async<'T>) (expected: 'A -> 'T) (message: string) (r: Runner<'A>) =
        r |> AddTestAsync (fun asserter args -> async {
            let expected = expected args
            let! actual = actual args 
            return asserter.Push((actual = expected), actual, expected, message)
        })

    let NotEqual (actual: 'A -> 'T) (expected: 'A -> 'T) (r: Runner<'A>) =
        r |> AddTest (fun asserter args ->
            let actual = actual args
            let expected = expected args
            asserter.Push((actual <> expected), actual, expected)
        )

    let NotEqualMsg (actual: 'A -> 'T) (expected: 'A -> 'T) (message: string) (r: Runner<'A>) =
        r |> AddTest (fun asserter args ->
            let actual = actual args
            let expected = expected args
            asserter.Push((actual <> expected), actual, expected, message)
        )

    let NotEqualAsync (actual: 'A -> Async<'T>) (expected: 'A -> 'T) (r: Runner<'A>) =
        r |> AddTestAsync (fun asserter args -> async {
            let expected = expected args
            let! actual = actual args 
            return asserter.Push((actual <> expected), actual, expected)
        })

    let NotEqualMsgAsync (actual: 'A -> Async<'T>) (expected: 'A -> 'T) (message: string) (r: Runner<'A>) =
        r |> AddTestAsync (fun asserter args -> async {
            let expected = expected args
            let! actual = actual args 
            return asserter.Push((actual <> expected), actual, expected, message)
        })

    let JsEqual (actual: 'A -> 'T) (expected: 'A -> 'T) (r: Runner<'A>) =
        r |> AddTest (fun asserter args ->
            asserter.Equal(actual args, expected args)
        )

    let JsEqualMsg (actual: 'A -> 'T) (expected: 'A -> 'T) (message: string) (r: Runner<'A>) =
        r |> AddTest (fun asserter args ->
            asserter.Equal(actual args, expected args, message)
        )

    let JsEqualAsync (actual: 'A -> Async<'T>) (expected: 'A -> 'T) (r: Runner<'A>) =
        r |> AddTestAsync (fun asserter args -> async {
            let expected = expected args
            let! actual = actual args 
            return asserter.Equal(actual, expected)
        })

    let JsEqualMsgAsync (actual: 'A -> Async<'T>) (expected: 'A -> 'T) (message: string) (r: Runner<'A>) =
        r |> AddTestAsync (fun asserter args -> async {
            let expected = expected args
            let! actual = actual args 
            return asserter.Equal(actual, expected, message)
        })

    let NotJsEqual (actual: 'A -> 'T) (expected: 'A -> 'T) (r: Runner<'A>) =
        r |> AddTest (fun asserter args ->
            asserter.NotEqual(actual args, expected args)
        )

    let NotJsEqualMsg (actual: 'A -> 'T) (expected: 'A -> 'T) (message: string) (r: Runner<'A>) =
        r |> AddTest (fun asserter args ->
            asserter.NotEqual(actual args, expected args, message)
        )

    let NotJsEqualAsync (actual: 'A -> Async<'T>) (expected: 'A -> 'T) (r: Runner<'A>) =
        r |> AddTestAsync (fun asserter args -> async {
            let expected = expected args
            let! actual = actual args 
            return asserter.NotEqual(actual, expected)
        })

    let NotJsEqualMsgAsync (actual: 'A -> Async<'T>) (expected: 'A -> 'T) (message: string) (r: Runner<'A>) =
        r |> AddTestAsync (fun asserter args -> async {
            let expected = expected args
            let! actual = actual args 
            return asserter.NotEqual(actual, expected, message)
        })

    let DeepEqual (actual: 'A -> 'T) (expected: 'A -> 'T) (r: Runner<'A>) =
        r |> AddTest (fun asserter args ->
            asserter.DeepEqual(actual args, expected args)
        )

    let DeepEqualMsg (actual: 'A -> 'T) (expected: 'A -> 'T) (message: string) (r: Runner<'A>) =
        r |> AddTest (fun asserter args ->
            asserter.DeepEqual(actual args, expected args, message)
        )

    let DeepEqualAsync (actual: 'A -> Async<'T>) (expected: 'A -> 'T) (r: Runner<'A>) =
        r |> AddTestAsync (fun asserter args -> async {
            let expected = expected args
            let! actual = actual args 
            return asserter.DeepEqual(actual, expected)
        })

    let DeepEqualMsgAsync (actual: 'A -> Async<'T>) (expected: 'A -> 'T) (message: string) (r: Runner<'A>) =
        r |> AddTestAsync (fun asserter args -> async {
            let expected = expected args
            let! actual = actual args 
            return asserter.DeepEqual(actual, expected, message)
        })

    let NotDeepEqual (actual: 'A -> 'T) (expected: 'A -> 'T) (r: Runner<'A>) =
        r |> AddTest (fun asserter args ->
            asserter.NotDeepEqual(actual args, expected args)
        )

    let NotDeepEqualMsg (actual: 'A -> 'T) (expected: 'A -> 'T) (message: string) (r: Runner<'A>) =
        r |> AddTest (fun asserter args ->
            asserter.NotDeepEqual(actual args, expected args, message)
        )

    let NotDeepEqualAsync (actual: 'A -> Async<'T>) (expected: 'A -> 'T) (r: Runner<'A>) =
        r |> AddTestAsync (fun asserter args -> async {
            let expected = expected args
            let! actual = actual args 
            return asserter.NotDeepEqual(actual, expected)
        })

    let NotDeepEqualMsgAsync (actual: 'A -> Async<'T>) (expected: 'A -> 'T) (message: string) (r: Runner<'A>) =
        r |> AddTestAsync (fun asserter args -> async {
            let expected = expected args
            let! actual = actual args 
            return asserter.NotDeepEqual(actual, expected, message)
        })

    let StrictEqual (actual: 'A -> 'T) (expected: 'A -> 'T) (r: Runner<'A>) =
        r |> AddTest (fun asserter args ->
            asserter.StrictEqual(actual args, expected args)
        )

    let StrictEqualMsg (actual: 'A -> 'T) (expected: 'A -> 'T) (message: string) (r: Runner<'A>) =
        r |> AddTest (fun asserter args ->
            asserter.StrictEqual(actual args, expected args, message)
        )

    let StrictEqualAsync (actual: 'A -> Async<'T>) (expected: 'A -> 'T) (r: Runner<'A>) =
        r |> AddTestAsync (fun asserter args -> async {
            let expected = expected args
            let! actual = actual args 
            return asserter.StrictEqual(actual, expected)
        })

    let StrictEqualMsgAsync (actual: 'A -> Async<'T>) (expected: 'A -> 'T) (message: string) (r: Runner<'A>) =
        r |> AddTestAsync (fun asserter args -> async {
            let expected = expected args
            let! actual = actual args 
            return asserter.StrictEqual(actual, expected, message)
        })

    let NotStrictEqual (actual: 'A -> 'T) (expected: 'A -> 'T) (r: Runner<'A>) =
        r |> AddTest (fun asserter args ->
            asserter.NotStrictEqual(actual args, expected args)
        )

    let NotStrictEqualMsg (actual: 'A -> 'T) (expected: 'A -> 'T) (message: string) (r: Runner<'A>) =
        r |> AddTest (fun asserter args ->
            asserter.NotStrictEqual(actual args, expected args, message)
        )

    let NotStrictEqualAsync (actual: 'A -> Async<'T>) (expected: 'A -> 'T) (r: Runner<'A>) =
        r |> AddTestAsync (fun asserter args -> async {
            let expected = expected args
            let! actual = actual args 
            return asserter.NotStrictEqual(actual, expected)
        })

    let NotStrictEqualMsgAsync (actual: 'A -> Async<'T>) (expected: 'A -> 'T) (message: string) (r: Runner<'A>) =
        r |> AddTestAsync (fun asserter args -> async {
            let expected = expected args
            let! actual = actual args 
            return asserter.NotStrictEqual(actual, expected, message)
        })

    let PropEqual (actual: 'A -> 'T) (expected: 'A -> 'T) (r: Runner<'A>) =
        r |> AddTest (fun asserter args ->
            asserter.PropEqual(actual args, expected args)
        )

    let PropEqualMsg (actual: 'A -> 'T) (expected: 'A -> 'T) (message: string) (r: Runner<'A>) =
        r |> AddTest (fun asserter args ->
            asserter.PropEqual(actual args, expected args, message)
        )

    let PropEqualAsync (actual: 'A -> Async<'T>) (expected: 'A -> 'T) (r: Runner<'A>) =
        r |> AddTestAsync (fun asserter args -> async {
            let expected = expected args
            let! actual = actual args 
            return asserter.PropEqual(actual, expected)
        })

    let PropEqualMsgAsync (actual: 'A -> Async<'T>) (expected: 'A -> 'T) (message: string) (r: Runner<'A>) =
        r |> AddTestAsync (fun asserter args -> async {
            let expected = expected args
            let! actual = actual args 
            return asserter.PropEqual(actual, expected, message)
        })

    let NotPropEqual (actual: 'A -> 'T) (expected: 'A -> 'T) (r: Runner<'A>) =
        r |> AddTest (fun asserter args ->
            asserter.NotPropEqual(actual args, expected args)
        )

    let NotPropEqualMsg (actual: 'A -> 'T) (expected: 'A -> 'T) (message: string) (r: Runner<'A>) =
        r |> AddTest (fun asserter args ->
            asserter.NotPropEqual(actual args, expected args, message)
        )

    let NotPropEqualAsync (actual: 'A -> Async<'T>) (expected: 'A -> 'T) (r: Runner<'A>) =
        r |> AddTestAsync (fun asserter args -> async {
            let expected = expected args
            let! actual = actual args 
            return asserter.NotPropEqual(actual, expected)
        })

    let NotPropEqualMsgAsync (actual: 'A -> Async<'T>) (expected: 'A -> 'T) (message: string) (r: Runner<'A>) =
        r |> AddTestAsync (fun asserter args -> async {
            let expected = expected args
            let! actual = actual args 
            return asserter.NotPropEqual(actual, expected, message)
        })

    let ApproxEqual (actual: 'A -> float) (expected: 'A -> float) (r: Runner<'A>) =
        r |> AddTest (fun asserter args ->
            let actual = actual args
            let expected = expected args
            asserter.Push(abs (actual - expected) < 0.0001, actual, expected)
        )

    let ApproxEqualMsg (actual: 'A -> float) (expected: 'A -> float) (message: string) (r: Runner<'A>) =
        r |> AddTest (fun asserter args ->
            let actual = actual args
            let expected = expected args
            asserter.Push(abs (actual - expected) < 0.0001, actual, expected, message)
        )

    let ApproxEqualAsync (actual: 'A -> Async<float>) (expected: 'A -> float) (r: Runner<'A>) =
        r |> AddTestAsync (fun asserter args -> async {
            let expected = expected args
            let! actual = actual args 
            return asserter.Push(abs (actual - expected) < 0.0001, actual, expected)
        })

    let ApproxEqualMsgAsync (actual: 'A -> Async<float>) (expected: 'A -> float) (message: string) (r: Runner<'A>) =
        r |> AddTestAsync (fun asserter args -> async {
            let expected = expected args
            let! actual = actual args 
            return asserter.Push(abs (actual - expected) < 0.0001, actual, expected, message)
        })

    let NotApproxEqual (actual: 'A -> float) (expected: 'A -> float) (r: Runner<'A>) =
        r |> AddTest (fun asserter args ->
            let actual = actual args
            let expected = expected args
            asserter.Push(abs (actual - expected) > 0.0001, actual, expected)
        )

    let NotApproxEqualMsg (actual: 'A -> float) (expected: 'A -> float) (message: string) (r: Runner<'A>) =
        r |> AddTest (fun asserter args ->
            let actual = actual args
            let expected = expected args
            asserter.Push(abs (actual - expected) > 0.0001, actual, expected, message)
        )

    let NotApproxEqualAsync (actual: 'A -> Async<float>) (expected: 'A -> float) (r: Runner<'A>) =
        r |> AddTestAsync (fun asserter args -> async {
            let expected = expected args
            let! actual = actual args 
            return asserter.Push(abs (actual - expected) > 0.0001, actual, expected)
        })

    let NotApproxEqualMsgAsync (actual: 'A -> Async<float>) (expected: 'A -> float) (message: string) (r: Runner<'A>) =
        r |> AddTestAsync (fun asserter args -> async {
            let expected = expected args
            let! actual = actual args 
            return asserter.Push(abs (actual - expected) > 0.0001, actual, expected, message)
        })

    let IsTrue (value: 'A -> bool) (r: Runner<'A>) =
        r |> AddTest (fun asserter args ->
            asserter.StrictEqual(value args, true)
        )

    let IsTrueMsg (value: 'A -> bool) (message: string) (r: Runner<'A>) =
        r |> AddTest (fun asserter args ->
            asserter.StrictEqual(value args, true, message)
        )

    let IsTrueAsync (value: 'A -> Async<bool>) (r: Runner<'A>) =
        r |> AddTestAsync (fun asserter args -> async {
            let! value = value args 
            return asserter.StrictEqual(value, true)
        })

    let IsTrueMsgAsync (value: 'A -> Async<bool>) (message: string) (r: Runner<'A>) =
        r |> AddTestAsync (fun asserter args -> async {
            let! value = value args 
            return asserter.StrictEqual(value, true, message)
        })

    let IsFalse (value: 'A -> bool) (r: Runner<'A>) =
        r |> AddTest (fun asserter args ->
            asserter.StrictEqual(value args, false)
        )

    let IsFalseMsg (value: 'A -> bool) (message: string) (r: Runner<'A>) =
        r |> AddTest (fun asserter args ->
            asserter.StrictEqual(value args, false, message)
        )

    let IsFalseAsync (value: 'A -> Async<bool>) (r: Runner<'A>) =
        r |> AddTestAsync (fun asserter args -> async {
            let! value = value args 
            return asserter.StrictEqual(value, false)
        })

    let IsFalseMsgAsync (value: 'A -> Async<bool>) (message: string) (r: Runner<'A>) =
        r |> AddTestAsync (fun asserter args -> async {
            let! value = value args 
            return asserter.StrictEqual(value, false, message)
        })

    let ForEach (src: 'A -> #seq<'T>) (attempt: 'A -> 'T -> Runner<'B>) (r: Runner<'A>) =
        ()
        fun asserter ->
            let rec loop (attempt: 'T -> Runner<'B>) (acc: Choice<'A, Async<'A>>) (src: list<'T>) =
                match src with
                | [] -> acc
                | e :: l ->
                    let r = attempt e
                    loop attempt (acc |> Bind (fun args -> r asserter |> Map (fun _ -> args))) l
            r asserter |> Bind (fun args ->
                loop (attempt args) (Choice1Of2 args) (List.ofSeq (src args))
            )

    let PropertyWithSample (sample: 'A -> RandomValues.Sample<'T>) (attempt: 'A -> 'T -> Runner<'B>) (r: Runner<'A>) =
        ()
        fun asserter ->
            let rec loop (attempt: 'T -> Runner<'B>) (acc: Choice<'A, Async<'A>>) (src: list<'T>) =
                match src with
                | [] -> acc
                | e :: l ->
                    let r = attempt e
                    loop attempt
                        (acc |> Bind (fun args ->
                            r asserter |> Map (fun _ -> args)))
                        l
            r asserter |> Bind (fun args ->
                let sample = sample args
                loop (attempt args) (Choice1Of2 args) sample.Data
            )

    let ForSample (f: 'A -> Runner<'B>) (sample: RandomValues.Sample<'A>) : Runner<'B> =
        ()
        fun asserter ->
            let rec loop (acc: Choice<'B, Async<'B>>) (src: list<'A>) =
                match src with
                | [] -> acc
                | e :: l ->
                    let r = f e
                    loop (acc |> Bind (fun _ -> r asserter)) l
            loop (Choice1Of2 JS.Undefined) sample.Data

    let PropertyWith (gen: 'A -> RandomValues.Generator<'T>) (attempt: 'A -> 'T -> Runner<'B>) (r: Runner<'A>) =
        PropertyWithSample (fun args -> RandomValues.Sample<'T>(gen args)) attempt r

    let ForGenerator (f: 'A -> Runner<'B>) (gen: RandomValues.Generator<'A>) : Runner<'B> =
        ForSample f (RandomValues.Sample(gen))

    let Property (attempt: 'A -> 'T -> Runner<'B>) (r: Runner<'A>) = 
        PropertyWithSample (fun _ -> RandomValues.Sample<'T>()) attempt r

    let Raises (value: 'A -> 'T) (r: Runner<'A>) =
        r |> AddTest (fun asserter args ->
            try
                value args |> ignore
                asserter.Ok(false, "Expected raised exception")
            with _ ->
                asserter.Ok(true)
        )

    let RaisesMsg (value: 'A -> 'T) (message: string) (r: Runner<'A>) =
        r |> AddTest (fun asserter args ->
            try
                value args |> ignore
                asserter.Ok(false, message)
            with _ ->
                asserter.Ok(true, message)
        )

    let RaisesAsync (value: 'A -> Async<'T>) (r: Runner<'A>) =
        r |> AddTestAsync (fun asserter args ->
            let value = value args 
            async {
                try
                    let! _ = value
                    return asserter.Ok(false, "Expected raised exception")
                with _ ->
                    return asserter.Ok(true)
            }
        )

    let RaisesMsgAsync (value: 'A -> Async<'T>) (message: string) (r: Runner<'A>) =
        r |> AddTestAsync (fun asserter args ->
            let value = value args 
            async {
                try
                    let! _ = value
                    return asserter.Ok(false, message)
                with _ ->
                    return asserter.Ok(true, message)
            }
        )

    let RunSubtest (subtest: 'A -> Runner<'B>) (r: Runner<'A>) =
        ()
        fun asserter ->
            r asserter |> Bind (fun args -> subtest args asserter |> Map (fun _ -> args))

    let BindAsync (f: 'A -> Runner<'B>) (a: Async<'A>) : Runner<'B> =
        fun asserter ->
            Choice2Of2 (async {
                let! a = a
                match f a asserter with
                | Choice1Of2 b -> return b
                | Choice2Of2 b -> return! b
            })

    let BindPromise (f: 'A -> Runner<'B>) (a: Promise<'A>) : Runner<'B> =
        BindAsync f (Promise.AsAsync a)

    let Yield(x) = 
        ()
        fun asserter -> Choice1Of2 x

    let Return(x) = 
        ()
        fun asserter -> Choice1Of2 x

    let Zero() = 
        ()
        fun asserter -> Choice1Of2 JS.Undefined

    let For (y: 'A -> Runner<'B>) (r: Runner<'A>) =
        ()
        fun asserter ->
            match r asserter with
            | Choice1Of2 a ->
                y a asserter
            | Choice2Of2 a ->
                Choice2Of2 (async {
                    let! a = a
                    match y a asserter with
                    | Choice1Of2 b -> return b
                    | Choice2Of2 b -> return! b
                })


[<JavaScript>]
type SubtestBuilder () =

    [<CustomOperation("expect", MaintainsVariableSpace = true); Inline>]
    member this.Expect
        (
            r: Runner<'A>,
            [<ProjectionParameter>] assertionCount: 'A -> int
        ) : Runner<'A> =
        r |> Runner.Expect assertionCount

    /// Tests equality between two values using F# `=`.
    [<CustomOperation("equal", MaintainsVariableSpace = true); Inline>]
    member this.Equal<'T, 'A when 'T : equality>
        (
            r: Runner<'A>,
            [<ProjectionParameter>] actual: 'A -> 'T,
            [<ProjectionParameter>] expected: 'A -> 'T
        ) : Runner<'A> =
        r |> Runner.Equal actual expected

    /// Tests equality between two values using F# `=`.
    [<CustomOperation("equalMsg", MaintainsVariableSpace = true); Inline>]
    member this.EqualMsg<'T, 'A when 'T : equality>
        (
            r: Runner<'A>,
            [<ProjectionParameter>] actual: 'A -> 'T,
            [<ProjectionParameter>] expected: 'A -> 'T,
            message: string
        ) : Runner<'A> =
        r |> Runner.EqualMsg actual expected message

    /// Tests equality between two values using F# `=`.
    [<CustomOperation("equalAsync", MaintainsVariableSpace = true); Inline>]
    member this.EqualAsync<'T, 'A when 'T : equality>
        (
            r: Runner<'A>,
            [<ProjectionParameter>] actual: 'A -> Async<'T>,
            [<ProjectionParameter>] expected: 'A -> 'T
        ) : Runner<'A> =
        r |> Runner.EqualAsync actual expected

    /// Tests equality between two values using F# `=`.
    [<CustomOperation("equalMsgAsync", MaintainsVariableSpace = true); Inline>]
    member this.EqualMsgAsync<'T, 'A when 'T : equality>
        (
            r: Runner<'A>,
            [<ProjectionParameter>] actual: 'A -> Async<'T>,
            [<ProjectionParameter>] expected: 'A -> 'T,
            message: string
        ) : Runner<'A> =
        r |> Runner.EqualMsgAsync actual expected message

    /// Tests equality between two values using F# `=`.
    [<CustomOperation("notEqual", MaintainsVariableSpace = true); Inline>]
    member this.NotEqual<'T, 'A when 'T : equality>
        (
            r: Runner<'A>,
            [<ProjectionParameter>] actual: 'A -> 'T,
            [<ProjectionParameter>] expected: 'A -> 'T
        ) : Runner<'A> =
        r |> Runner.NotEqual actual expected

    /// Tests equality between two values using F# `=`.
    [<CustomOperation("notEqualMsg", MaintainsVariableSpace = true); Inline>]
    member this.NotEqualMsg<'T, 'A when 'T : equality>
        (
            r: Runner<'A>,
            [<ProjectionParameter>] actual: 'A -> 'T,
            [<ProjectionParameter>] expected: 'A -> 'T,
            message: string
        ) : Runner<'A> =
        r |> Runner.NotEqualMsg actual expected message

    /// Tests equality between two values using F# `=`.
    [<CustomOperation("notEqualAsync", MaintainsVariableSpace = true); Inline>]
    member this.NotEqualAsync<'T, 'A when 'T : equality>
        (
            r: Runner<'A>,
            [<ProjectionParameter>] actual: 'A -> Async<'T>,
            [<ProjectionParameter>] expected: 'A -> 'T
        ) : Runner<'A> =
        r |> Runner.NotEqualAsync actual expected

    /// Tests equality between two values using F# `=`.
    [<CustomOperation("notEqualMsgAsync", MaintainsVariableSpace = true); Inline>]
    member this.NotEqualMsgAsync<'T, 'A when 'T : equality>
        (
            r: Runner<'A>,
            [<ProjectionParameter>] actual: 'A -> Async<'T>,
            [<ProjectionParameter>] expected: 'A -> 'T,
            message: string
        ) : Runner<'A> =
        r |> Runner.NotEqualMsgAsync actual expected message

    /// Tests equality between two values using JavaScript `==`.
    [<CustomOperation("jsEqual", MaintainsVariableSpace = true); Inline>]
    member this.JsEqual<'T, 'A>
        (
            r: Runner<'A>,
            [<ProjectionParameter>] actual: 'A -> 'T,
            [<ProjectionParameter>] expected: 'A -> 'T
        ) : Runner<'A> =
        r |> Runner.JsEqual actual expected

    /// Tests equality between two values using JavaScript `==`.
    [<CustomOperation("jsEqualMsg", MaintainsVariableSpace = true); Inline>]
    member this.JsEqualMsg<'T, 'A>
        (
            r: Runner<'A>,
            [<ProjectionParameter>] actual: 'A -> 'T,
            [<ProjectionParameter>] expected: 'A -> 'T,
            message: string
        ) : Runner<'A> =
        r |> Runner.JsEqualMsg actual expected message

    /// Tests equality between two values using JavaScript `==`.
    [<CustomOperation("jsEqualAsync", MaintainsVariableSpace = true); Inline>]
    member this.JsEqualAsync<'T, 'A>
        (
            r: Runner<'A>,
            [<ProjectionParameter>] actual: 'A -> Async<'T>,
            [<ProjectionParameter>] expected: 'A -> 'T
        ) : Runner<'A> =
        r |> Runner.JsEqualAsync actual expected

    /// Tests equality between two values using JavaScript `==`.
    [<CustomOperation("jsEqualMsgAsync", MaintainsVariableSpace = true); Inline>]
    member this.JsEqualMsgAsync<'T, 'A>
        (
            r: Runner<'A>,
            [<ProjectionParameter>] actual: 'A -> Async<'T>,
            [<ProjectionParameter>] expected: 'A -> 'T,
            message: string
        ) : Runner<'A> =
        r |> Runner.JsEqualMsgAsync actual expected message

    /// Tests equality between two values using JavaScript `==`.
    [<CustomOperation("notJsEqual", MaintainsVariableSpace = true); Inline>]
    member this.NotJsEqual<'T, 'A>
        (
            r: Runner<'A>,
            [<ProjectionParameter>] actual: 'A -> 'T,
            [<ProjectionParameter>] expected: 'A -> 'T
        ) : Runner<'A> =
        r |> Runner.NotJsEqual actual expected

    /// Tests equality between two values using JavaScript `==`.
    [<CustomOperation("notJsEqualMsg", MaintainsVariableSpace = true); Inline>]
    member this.NotJsEqualMsg<'T, 'A>
        (
            r: Runner<'A>,
            [<ProjectionParameter>] actual: 'A -> 'T,
            [<ProjectionParameter>] expected: 'A -> 'T,
            message: string
        ) : Runner<'A> =
        r |> Runner.NotJsEqualMsg actual expected message

    /// Tests equality between two values using JavaScript `==`.
    [<CustomOperation("notJsEqualAsync", MaintainsVariableSpace = true); Inline>]
    member this.NotJsEqualAsync<'T, 'A>
        (
            r: Runner<'A>,
            [<ProjectionParameter>] actual: 'A -> Async<'T>,
            [<ProjectionParameter>] expected: 'A -> 'T
        ) : Runner<'A> =
        r |> Runner.NotJsEqualAsync actual expected

    /// Tests equality between two values using JavaScript `==`.
    [<CustomOperation("notJsEqualMsgAsync", MaintainsVariableSpace = true); Inline>]
    member this.NotJsEqualMsgAsync<'T, 'A>
        (
            r: Runner<'A>,
            [<ProjectionParameter>] actual: 'A -> Async<'T>,
            [<ProjectionParameter>] expected: 'A -> 'T,
            message: string
        ) : Runner<'A> =
        r |> Runner.NotJsEqualMsgAsync actual expected message

    /// Tests equality between two values using QUnit's deep equality test.
    [<CustomOperation("deepEqual", MaintainsVariableSpace = true); Inline>]
    member this.DeepEqual<'T, 'A>
        (
            r: Runner<'A>,
            [<ProjectionParameter>] actual: 'A -> 'T,
            [<ProjectionParameter>] expected: 'A -> 'T
        ) : Runner<'A> =
        r |> Runner.DeepEqual actual expected

    /// Tests equality between two values using QUnit's deep equality test.
    [<CustomOperation("deepEqualMsg", MaintainsVariableSpace = true); Inline>]
    member this.DeepEqualMsg<'T, 'A>
        (
            r: Runner<'A>,
            [<ProjectionParameter>] actual: 'A -> 'T,
            [<ProjectionParameter>] expected: 'A -> 'T,
            message: string
        ) : Runner<'A> =
        r |> Runner.DeepEqualMsg actual expected message

    /// Tests equality between two values using QUnit's deep equality test.
    [<CustomOperation("deepEqualAsync", MaintainsVariableSpace = true); Inline>]
    member this.DeepEqualAsync<'T, 'A>
        (
            r: Runner<'A>,
            [<ProjectionParameter>] actual: 'A -> Async<'T>,
            [<ProjectionParameter>] expected: 'A -> 'T
        ) : Runner<'A> =
        r |> Runner.DeepEqualAsync actual expected

    /// Tests equality between two values using QUnit's deep equality test.
    [<CustomOperation("deepEqualMsgAsync", MaintainsVariableSpace = true); Inline>]
    member this.DeepEqualMsgAsync<'T, 'A>
        (
            r: Runner<'A>,
            [<ProjectionParameter>] actual: 'A -> Async<'T>,
            [<ProjectionParameter>] expected: 'A -> 'T,
            message: string
        ) : Runner<'A> =
        r |> Runner.DeepEqualMsgAsync actual expected message

    /// Tests equality between two values using QUnit's deep equality test.
    [<CustomOperation("notDeepEqual", MaintainsVariableSpace = true); Inline>]
    member this.NotDeepEqual<'T, 'A>
        (
            r: Runner<'A>,
            [<ProjectionParameter>] actual: 'A -> 'T,
            [<ProjectionParameter>] expected: 'A -> 'T
        ) : Runner<'A> =
        r |> Runner.NotDeepEqual actual expected

    /// Tests equality between two values using QUnit's deep equality test.
    [<CustomOperation("notDeepEqualMsg", MaintainsVariableSpace = true); Inline>]
    member this.NotDeepEqualMsg<'T, 'A>
        (
            r: Runner<'A>,
            [<ProjectionParameter>] actual: 'A -> 'T,
            [<ProjectionParameter>] expected: 'A -> 'T,
            message: string
        ) : Runner<'A> =
        r |> Runner.NotDeepEqualMsg actual expected message

    /// Tests equality between two values using QUnit's deep equality test.
    [<CustomOperation("notDeepEqualAsync", MaintainsVariableSpace = true); Inline>]
    member this.NotDeepEqualAsync<'T, 'A>
        (
            r: Runner<'A>,
            [<ProjectionParameter>] actual: 'A -> Async<'T>,
            [<ProjectionParameter>] expected: 'A -> 'T
        ) : Runner<'A> =
        r |> Runner.NotDeepEqualAsync actual expected

    /// Tests equality between two values using QUnit's deep equality test.
    [<CustomOperation("notDeepEqualMsgAsync", MaintainsVariableSpace = true); Inline>]
    member this.NotDeepEqualMsgAsync<'T, 'A>
        (
            r: Runner<'A>,
            [<ProjectionParameter>] actual: 'A -> Async<'T>,
            [<ProjectionParameter>] expected: 'A -> 'T,
            message: string
        ) : Runner<'A> =
        r |> Runner.NotDeepEqualMsgAsync actual expected message

    /// Tests equality between two values using JavaScript `===`.
    [<CustomOperation("strictEqual", MaintainsVariableSpace = true); Inline>]
    member this.StrictEqual<'T, 'A>
        (
            r: Runner<'A>,
            [<ProjectionParameter>] actual: 'A -> 'T,
            [<ProjectionParameter>] expected: 'A -> 'T
        ) : Runner<'A> =
        r |> Runner.StrictEqual actual expected

    /// Tests equality between two values using JavaScript `===`.
    [<CustomOperation("strictEqualMsg", MaintainsVariableSpace = true); Inline>]
    member this.StrictEqualMsg<'T, 'A>
        (
            r: Runner<'A>,
            [<ProjectionParameter>] actual: 'A -> 'T,
            [<ProjectionParameter>] expected: 'A -> 'T,
            message: string
        ) : Runner<'A> =
        r |> Runner.StrictEqualMsg actual expected message

    /// Tests equality between two values using JavaScript `===`.
    [<CustomOperation("strictEqualAsync", MaintainsVariableSpace = true); Inline>]
    member this.StrictEqualAsync<'T, 'A>
        (
            r: Runner<'A>,
            [<ProjectionParameter>] actual: 'A -> Async<'T>,
            [<ProjectionParameter>] expected: 'A -> 'T
        ) : Runner<'A> =
        r |> Runner.StrictEqualAsync actual expected

    /// Tests equality between two values using JavaScript `===`.
    [<CustomOperation("strictEqualMsgAsync", MaintainsVariableSpace = true); Inline>]
    member this.StrictEqualMsgAsync<'T, 'A>
        (
            r: Runner<'A>,
            [<ProjectionParameter>] actual: 'A -> Async<'T>,
            [<ProjectionParameter>] expected: 'A -> 'T,
            message: string
        ) : Runner<'A> =
        r |> Runner.StrictEqualMsgAsync actual expected message

    /// Tests equality between two values using JavaScript `===`.
    [<CustomOperation("notStrictEqual", MaintainsVariableSpace = true); Inline>]
    member this.NotStrictEqual<'T, 'A>
        (
            r: Runner<'A>,
            [<ProjectionParameter>] actual: 'A -> 'T,
            [<ProjectionParameter>] expected: 'A -> 'T
        ) : Runner<'A> =
        r |> Runner.NotStrictEqual actual expected

    /// Tests equality between two values using JavaScript `===`.
    [<CustomOperation("notStrictEqualMsg", MaintainsVariableSpace = true); Inline>]
    member this.NotStrictEqualMsg<'T, 'A>
        (
            r: Runner<'A>,
            [<ProjectionParameter>] actual: 'A -> 'T,
            [<ProjectionParameter>] expected: 'A -> 'T,
            message: string
        ) : Runner<'A> =
        r |> Runner.NotStrictEqualMsg actual expected message

    /// Tests equality between two values using JavaScript `===`.
    [<CustomOperation("notStrictEqualAsync", MaintainsVariableSpace = true); Inline>]
    member this.NotStrictEqualAsync<'T, 'A>
        (
            r: Runner<'A>,
            [<ProjectionParameter>] actual: 'A -> Async<'T>,
            [<ProjectionParameter>] expected: 'A -> 'T
        ) : Runner<'A> =
        r |> Runner.NotStrictEqualAsync actual expected

    /// Tests equality between two values using JavaScript `===`.
    [<CustomOperation("notStrictEqualMsgAsync", MaintainsVariableSpace = true); Inline>]
    member this.NotStrictEqualMsgAsync<'T, 'A>
        (
            r: Runner<'A>,
            [<ProjectionParameter>] actual: 'A -> Async<'T>,
            [<ProjectionParameter>] expected: 'A -> 'T,
            message: string
        ) : Runner<'A> =
        r |> Runner.NotStrictEqualMsgAsync actual expected message

    /// Tests equality between two values using QUnit's prop equality test.
    [<CustomOperation("propEqual", MaintainsVariableSpace = true); Inline>]
    member this.PropEqual<'T, 'A>
        (
            r: Runner<'A>,
            [<ProjectionParameter>] actual: 'A -> 'T,
            [<ProjectionParameter>] expected: 'A -> 'T
        ) : Runner<'A> =
        r |> Runner.PropEqual actual expected

    /// Tests equality between two values using QUnit's prop equality test.
    [<CustomOperation("propEqualMsg", MaintainsVariableSpace = true); Inline>]
    member this.PropEqualMsg<'T, 'A>
        (
            r: Runner<'A>,
            [<ProjectionParameter>] actual: 'A -> 'T,
            [<ProjectionParameter>] expected: 'A -> 'T,
            message: string
        ) : Runner<'A> =
        r |> Runner.PropEqualMsg actual expected message

    /// Tests equality between two values using QUnit's prop equality test.
    [<CustomOperation("propEqualAsync", MaintainsVariableSpace = true); Inline>]
    member this.PropEqualAsync<'T, 'A>
        (
            r: Runner<'A>,
            [<ProjectionParameter>] actual: 'A -> Async<'T>,
            [<ProjectionParameter>] expected: 'A -> 'T
        ) : Runner<'A> =
        r |> Runner.PropEqualAsync actual expected

    /// Tests equality between two values using QUnit's prop equality test.
    [<CustomOperation("propEqualMsgAsync", MaintainsVariableSpace = true); Inline>]
    member this.PropEqualMsgAsync<'T, 'A>
        (
            r: Runner<'A>,
            [<ProjectionParameter>] actual: 'A -> Async<'T>,
            [<ProjectionParameter>] expected: 'A -> 'T,
            message: string
        ) : Runner<'A> =
        r |> Runner.PropEqualMsgAsync actual expected message

    /// Tests equality between two values using QUnit's prop equality test.
    [<CustomOperation("notPropEqual", MaintainsVariableSpace = true); Inline>]
    member this.NotPropEqual<'T, 'A>
        (
            r: Runner<'A>,
            [<ProjectionParameter>] actual: 'A -> 'T,
            [<ProjectionParameter>] expected: 'A -> 'T
        ) : Runner<'A> =
        r |> Runner.NotPropEqual actual expected

    /// Tests equality between two values using QUnit's prop equality test.
    [<CustomOperation("notPropEqualMsg", MaintainsVariableSpace = true); Inline>]
    member this.NotPropEqualMsg<'T, 'A>
        (
            r: Runner<'A>,
            [<ProjectionParameter>] actual: 'A -> 'T,
            [<ProjectionParameter>] expected: 'A -> 'T,
            message: string
        ) : Runner<'A> =
        r |> Runner.NotPropEqualMsg actual expected message

    /// Tests equality between two values using QUnit's prop equality test.
    [<CustomOperation("notPropEqualAsync", MaintainsVariableSpace = true); Inline>]
    member this.NotPropEqualAsync<'T, 'A>
        (
            r: Runner<'A>,
            [<ProjectionParameter>] actual: 'A -> Async<'T>,
            [<ProjectionParameter>] expected: 'A -> 'T
        ) : Runner<'A> =
        r |> Runner.NotPropEqualAsync actual expected

    /// Tests equality between two values using QUnit's prop equality test.
    [<CustomOperation("notPropEqualMsgAsync", MaintainsVariableSpace = true); Inline>]
    member this.NotPropEqualMsgAsync<'T, 'A>
        (
            r: Runner<'A>,
            [<ProjectionParameter>] actual: 'A -> Async<'T>,
            [<ProjectionParameter>] expected: 'A -> 'T,
            message: string
        ) : Runner<'A> =
        r |> Runner.NotPropEqualMsgAsync actual expected message

    /// Tests approximate equality between two floats.
    [<CustomOperation("approxEqual", MaintainsVariableSpace = true); Inline>]
    member this.ApproxEqual<'T, 'A>
        (
            r: Runner<'A>,
            [<ProjectionParameter>] actual: 'A -> float,
            [<ProjectionParameter>] expected: 'A -> float
        ) : Runner<'A> =
        r |> Runner.ApproxEqual actual expected

    /// Tests approximate equality between two floats.
    [<CustomOperation("approxEqualMsg", MaintainsVariableSpace = true); Inline>]
    member this.ApproxEqualMsg<'T, 'A>
        (
            r: Runner<'A>,
            [<ProjectionParameter>] actual: 'A -> float,
            [<ProjectionParameter>] expected: 'A -> float,
            message: string
        ) : Runner<'A> =
        r |> Runner.ApproxEqualMsg actual expected message

    /// Tests approximate equality between two floats.
    [<CustomOperation("approxEqualAsync", MaintainsVariableSpace = true); Inline>]
    member this.ApproxEqualAsync<'T, 'A>
        (
            r: Runner<'A>,
            [<ProjectionParameter>] actual: 'A -> Async<float>,
            [<ProjectionParameter>] expected: 'A -> float
        ) : Runner<'A> =
        r |> Runner.ApproxEqualAsync actual expected

    /// Tests approximate equality between two floats.
    [<CustomOperation("approxEqualMsgAsync", MaintainsVariableSpace = true); Inline>]
    member this.ApproxEqualMsgAsync<'T, 'A>
        (
            r: Runner<'A>,
            [<ProjectionParameter>] actual: 'A -> Async<float>,
            [<ProjectionParameter>] expected: 'A -> float,
            message: string
        ) : Runner<'A> =
        r |> Runner.ApproxEqualMsgAsync actual expected message

    /// Tests approximate inequality between two floats.
    [<CustomOperation("notApproxEqual", MaintainsVariableSpace = true); Inline>]
    member this.NotApproxEqual<'T, 'A>
        (
            r: Runner<'A>,
            [<ProjectionParameter>] actual: 'A -> float,
            [<ProjectionParameter>] expected: 'A -> float
        ) : Runner<'A> =
        r |> Runner.NotApproxEqual actual expected

    /// Tests approximate inequality between two floats.
    [<CustomOperation("notApproxEqualMsg", MaintainsVariableSpace = true); Inline>]
    member this.NotApproxEqualMsg<'T, 'A>
        (
            r: Runner<'A>,
            [<ProjectionParameter>] actual: 'A -> float,
            [<ProjectionParameter>] expected: 'A -> float,
            message: string
        ) : Runner<'A> =
        r |> Runner.NotApproxEqualMsg actual expected message

    /// Tests approximate inequality between two floats.
    [<CustomOperation("notApproxEqualAsync", MaintainsVariableSpace = true); Inline>]
    member this.NotApproxEqualAsync<'T, 'A>
        (
            r: Runner<'A>,
            [<ProjectionParameter>] actual: 'A -> Async<float>,
            [<ProjectionParameter>] expected: 'A -> float
        ) : Runner<'A> =
        r |> Runner.NotApproxEqualAsync actual expected

    /// Tests approximate inequality between two floats.
    [<CustomOperation("notApproxEqualMsgAsync", MaintainsVariableSpace = true); Inline>]
    member this.NotApproxEqualMsgAsync<'T, 'A>
        (
            r: Runner<'A>,
            [<ProjectionParameter>] actual: 'A -> Async<float>,
            [<ProjectionParameter>] expected: 'A -> float,
            message: string
        ) : Runner<'A> =
        r |> Runner.NotApproxEqualMsgAsync actual expected message

    /// Checks that a boolean is true.
    [<CustomOperation("isTrue", MaintainsVariableSpace = true); Inline>]
    member this.IsTrue<'A>
        (
            r: Runner<'A>,
            [<ProjectionParameter>] value: 'A -> bool
        ) : Runner<'A> =
        r |> Runner.IsTrue value

    /// Checks that a boolean is true.
    [<CustomOperation("isTrueMsg", MaintainsVariableSpace = true); Inline>]
    member this.IsTrueMsg<'A>
        (
            r: Runner<'A>,
            [<ProjectionParameter>] value: 'A -> bool,
            message: string
        ) : Runner<'A> =
        r |> Runner.IsTrueMsg value message

    /// Checks that a boolean is true.
    [<CustomOperation("isTrueAsync", MaintainsVariableSpace = true); Inline>]
    member this.IsTrueAsync<'A>
        (
            r: Runner<'A>,
            [<ProjectionParameter>] value: 'A -> Async<bool>
        ) : Runner<'A> =
        r |> Runner.IsTrueAsync value

    /// Checks that a boolean is true.
    [<CustomOperation("isTrueMsgAsync", MaintainsVariableSpace = true); Inline>]
    member this.IsTrueMsgAsync<'A>
        (
            r: Runner<'A>,
            [<ProjectionParameter>] value: 'A -> Async<bool>,
            message: string
        ) : Runner<'A> =
        r |> Runner.IsTrueMsgAsync value message

    /// Checks that a boolean is false.
    [<CustomOperation("isFalse", MaintainsVariableSpace = true); Inline>]
    member this.IsFalse<'A>
        (
            r: Runner<'A>,
            [<ProjectionParameter>] value: 'A -> bool
        ) : Runner<'A> =
        r |> Runner.IsFalse value

    /// Checks that a boolean is false.
    [<CustomOperation("isFalseMsg", MaintainsVariableSpace = true); Inline>]
    member this.IsFalseMsg<'A>
        (
            r: Runner<'A>,
            [<ProjectionParameter>] value: 'A -> bool,
            message: string
        ) : Runner<'A> =
        r |> Runner.IsFalseMsg value message

    /// Checks that a boolean is false.
    [<CustomOperation("isFalseAsync", MaintainsVariableSpace = true); Inline>]
    member this.IsFalseAsync<'A>
        (
            r: Runner<'A>,
            [<ProjectionParameter>] value: 'A -> Async<bool>
        ) : Runner<'A> =
        r |> Runner.IsFalseAsync value

    /// Checks that a boolean is false.
    [<CustomOperation("isFalseMsgAsync", MaintainsVariableSpace = true); Inline>]
    member this.IsFalseMsgAsync<'A>
        (
            r: Runner<'A>,
            [<ProjectionParameter>] value: 'A -> Async<bool>,
            message: string
        ) : Runner<'A> =
        r |> Runner.IsFalseMsgAsync value message

    /// Runs a test for each element in a sequence.
    [<CustomOperation("forEach", MaintainsVariableSpace = true); Inline>]
    member this.ForEach
        (
            r: Runner<'A>,
            [<ProjectionParameter>] src: 'A -> #seq<'T>,
            [<ProjectionParameter>] attempt: 'A -> 'T -> Runner<'B>
        ) : Runner<'A> =
        r |> Runner.ForEach src attempt

    /// Runs a test for each element in a randomly generated set.
    [<CustomOperation("propertyWithSample", MaintainsVariableSpace = true); Inline>]
    member this.PropertyWithSample<'T, 'A, 'B>
        (
            r: Runner<'A>,
            [<ProjectionParameter>] sample: 'A -> RandomValues.Sample<'T>,
            [<ProjectionParameter>] attempt: 'A -> 'T -> Runner<'B>
        ) : Runner<'A> =
        r |> Runner.PropertyWithSample sample attempt

    /// Runs a test for each element in a randomly generated sample.
    [<Inline>]
    member this.For(sample: RandomValues.Sample<'A>, f: 'A -> Runner<'B>) : Runner<'B> =
        Runner.ForSample f sample

    /// Runs a test for 100 occurrences of a random generator.
    [<CustomOperation("propertyWith", MaintainsVariableSpace = true); Inline>]
    [<Macro(typeof<Internal.TestPropertyMacro>)>]
    member this.PropertyWith<'T, 'A, 'B>
        (
            r: Runner<'A>,
            [<ProjectionParameter>] gen: 'A -> RandomValues.Generator<'T>,
            [<ProjectionParameter>] attempt: 'A -> 'T -> Runner<'B>
        ) : Runner<'A> =
            r |> Runner.PropertyWith gen attempt

    /// Runs a test for 100 occurrences of a random generator.
    [<Inline>]
    member this.For(gen: RandomValues.Generator<'A>, f: 'A -> Runner<'B>) : Runner<'B> =
        Runner.ForGenerator f gen

    /// Runs a test for 100 random occurrences.
    [<CustomOperation("property", MaintainsVariableSpace = true); Inline>]
    [<Macro(typeof<Internal.TestPropertyMacro>)>]
    member this.Property<'T, 'A, 'B>
        (
            r: Runner<'A>,
            [<ProjectionParameter>] attempt: 'A -> 'T -> Runner<'B>
        ) : Runner<'A> =
            r |> Runner.Property attempt

    /// Checks that an expression raises an exception.
    [<CustomOperation("raises", MaintainsVariableSpace = true); Inline>]
    member this.Raises<'T, 'A>
        (
            r: Runner<'A>,
            [<ProjectionParameter>] value: 'A -> 'T
        ) : Runner<'A> =
        r |> Runner.Raises value

    /// Checks that an expression raises an exception.
    [<CustomOperation("raisesMsg", MaintainsVariableSpace = true); Inline>]
    member this.RaisesMsg<'T, 'A>
        (
            r: Runner<'A>,
            [<ProjectionParameter>] value: 'A -> 'T,
            message: string
        ) : Runner<'A> =
        r |> Runner.RaisesMsg value message

    /// Checks that an expression raises an exception.
    [<CustomOperation("raisesAsync", MaintainsVariableSpace = true); Inline>]
    member this.RaisesAsync<'T, 'A>
        (
            r: Runner<'A>,
            [<ProjectionParameter>] value: 'A -> Async<'T>
        ) : Runner<'A> =
        r |> Runner.RaisesAsync value

    /// Checks that an expression raises an exception.
    [<CustomOperation("raisesMsgAsync", MaintainsVariableSpace = true); Inline>]
    member this.RaisesMsgAsync<'T, 'A>
        (
            r: Runner<'A>,
            [<ProjectionParameter>] value: 'A -> Async<'T>,
            message: string
        ) : Runner<'A> =
        r |> Runner.RaisesMsgAsync value message

    /// Runs a sub-test.
    [<CustomOperation("run", MaintainsVariableSpace = true); Inline>]
    member this.RunSubtest
        (
            r: Runner<'A>,
            [<ProjectionParameter>] subtest: 'A -> Runner<'B>
        ) : Runner<'A> =
        r |> Runner.RunSubtest subtest

    [<Inline>]
    member this.Bind(a: Async<'A>, f: 'A -> Runner<'B>) : Runner<'B> =
        Runner.BindAsync f a

    [<Inline>]
    member this.Bind(a: Promise<'A>, f: 'A -> Runner<'B>) : Runner<'B> =
        Runner.BindPromise f a

    [<Inline>]
    member this.Yield(x) = Runner.Yield x

    [<Inline>]
    member this.Return(x) = Runner.Return x

    [<Inline>]
    member this.Zero() = Runner.Zero()

    [<Inline>]
    member this.For
        (
            r: Runner<'A>,
            y: 'A -> Runner<'B>
        ) : Runner<'B> =
        r |> Runner.For y 

[<JavaScript>]
type TestBuilder (run: (QUnit.Asserter -> unit) -> unit) =
    inherit SubtestBuilder ()

    member this.Run(e) =
        run (fun asserter ->
            try
                match e asserter with
                | Choice1Of2 _ -> ()
                | Choice2Of2 asy ->
                    let ``done`` = asserter.Async()
                    async {
                        try
                            try
                                let! _ = asy |> Runner.WithTimeout 1000 
                                return ()
                            with e ->
                                return asserter.Equal(e, null, "Test threw an unexpected asynchronous exception")
                        finally
                            ``done``()
                    }
                    |> Async.Start
            with e ->
                asserter.Equal(e, null, "Test threw an unexpected synchronous exception")
        )

[<JavaScript; Pure>]
let Test name = new TestBuilder(QUnit.Test name)

[<JavaScript; Pure>]
let TestIf bool name = new TestBuilder(if bool then QUnit.Test name else ignore)

[<JavaScript; Pure>]
let Skip name = new TestBuilder(QUnit.Skip name)

[<JavaScript; Pure>]
let SkipIf bool name = new TestBuilder(if bool then QUnit.Skip name else ignore)

[<JavaScript; Pure>]
let Todo name = new TestBuilder(QUnit.Todo name)

[<JavaScript; Pure>]
let TodoIf bool name = new TestBuilder(if bool then QUnit.Todo name else ignore)

[<JavaScript; Pure>]
let Do = new SubtestBuilder()

[<JavaScript; Pure>]
let PropertyWith name gen f =
    Test name { propertyWith gen f }

[<JavaScript; Pure>]
let PropertyWithSample name set f =
    Test name { propertyWithSample set f }

[<Macro(typeof<Internal.PropertyMacro>); Pure>]
let Property<'T, 'O> name (f: 'T -> Runner<'O>) =
    PropertyWith name (RandomValues.Auto<'T>()) f
