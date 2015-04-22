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

/// Common utilities for testing.
[<AutoOpen>]
module WebSharper.Testing.Pervasives

open WebSharper
open WebSharper.JavaScript

module QUnit =

    [<Stub>]
    type Asserter =

        [<Stub; Name "ok">]
        member this.Ok(value: bool) = X<unit>

        [<Stub; Name "ok">]
        member this.Ok(value: bool, message: string) = X<unit>

        [<Stub; Name "equal">]
        member this.Equal<'T>(actual: 'T, expected: 'T) = X<unit>

        [<Stub; Name "equal">]
        member this.Equal<'T>(actual: 'T, expected: 'T, message: string) = X<unit>

        [<Stub; Name "deepEqual">]
        member this.DeepEqual<'T>(actual: 'T, expected: 'T) = X<unit>

        [<Stub; Name "deepEqual">]
        member this.DeepEqual<'T>(actual: 'T, expected: 'T, message: string) = X<unit>

        [<Stub; Name "expect">]
        member this.Expect(assertionCount: int) = X<unit>

        [<Stub; Name "async">]
        member this.Async() = X<unit -> unit>

        [<Stub; Name "push">]
        member this.Push(result: bool, actual: 'T, expected: 'T, message: string) = X<unit>

        [<Stub; Name "push">]
        member this.Push(result: bool, actual: 'T, expected: 'T) = X<unit>

    // Test and Module use Unchecked.defaultof<_> instead of X<_>
    // because they are intended to be called from the top-level

    [<Inline "QUnit.test($name, $callback)">]
    let Test (name: string) (callback: Asserter -> unit) = Unchecked.defaultof<unit>

    [<Inline "QUnit.module($name)">]
    let Module (name: string) = Unchecked.defaultof<unit>

type Section = internal Section of name: string * run: (unit -> unit)

[<JavaScript>]
type SectionBuilder(name: string) =

    [<Inline>]
    member this.Delay(f) = Section (name, f)

    [<Inline>]
    member this.Zero() = ()

[<JavaScript>]
let Section name = new SectionBuilder(name)

[<JavaScript>]
let RunTests sections =
    for (Section(name, run)) in sections do
        QUnit.Module(name)
        run()

// This could simply be (Assert -> Async<'A>), but since QUnit's performance
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
        fun asserter ->
            r asserter |> Map (fun args ->
                t asserter args
                args)

    let AddTestAsync t r =
        fun asserter ->
            r asserter |> MapAsync (fun args -> async {
                do! t asserter args
                return args
            })

[<JavaScript>]
type SubtestBuilder () =

    [<CustomOperation("Expect", MaintainsVariableSpace = true)>]
    member this.Expect
        (
            r: Runner<'A>,
            [<ProjectionParameter>] assertionCount: 'A -> int
        ) : Runner<'A> =
        r |> Runner.AddTest (fun asserter args ->
            asserter.Expect(assertionCount args)
        )

    /// Tests equality between two values using F# `=`.
    [<CustomOperation("Equal", MaintainsVariableSpace = true)>]
    member this.Equal<'T, 'A when 'T : equality>
        (
            r: Runner<'A>,
            [<ProjectionParameter>] actual: 'A -> 'T,
            [<ProjectionParameter>] expected: 'A -> 'T
        ) : Runner<'A> =
        r |> Runner.AddTest (fun asserter args ->
            let actual = actual args
            let expected = expected args
            asserter.Push((actual = expected), actual, expected)
        )

    /// Tests equality between two values using F# `=`.
    [<CustomOperation("EqualM", MaintainsVariableSpace = true)>]
    member this.EqualM<'T, 'A when 'T : equality>
        (
            r: Runner<'A>,
            [<ProjectionParameter>] actual: 'A -> 'T,
            [<ProjectionParameter>] expected: 'A -> 'T,
            message: string
        ) : Runner<'A> =
        r |> Runner.AddTest (fun asserter args ->
            let actual = actual args
            let expected = expected args
            asserter.Push((actual = expected), actual, expected, message)
        )

    /// Tests equality between two values using F# `=`.
    [<CustomOperation("EqualA", MaintainsVariableSpace = true)>]
    member this.EqualA<'T, 'A when 'T : equality>
        (
            r: Runner<'A>,
            [<ProjectionParameter>] actual: 'A -> Async<'T>,
            [<ProjectionParameter>] expected: 'A -> 'T
        ) : Runner<'A> =
        r |> Runner.AddTestAsync (fun asserter args -> async {
            let expected = expected args
            let! actual = actual args
            return asserter.Push((actual = expected), actual, expected)
        })

    /// Tests equality between two values using F# `=`.
    [<CustomOperation("EqualMA", MaintainsVariableSpace = true)>]
    member this.EqualMA<'T, 'A when 'T : equality>
        (
            r: Runner<'A>,
            [<ProjectionParameter>] actual: 'A -> Async<'T>,
            [<ProjectionParameter>] expected: 'A -> 'T,
            message: string
        ) : Runner<'A> =
        r |> Runner.AddTestAsync (fun asserter args -> async {
            let expected = expected args
            let! actual = actual args
            return asserter.Push((actual = expected), actual, expected, message)
        })

    /// Tests equality between two values using F# `=`.
    [<CustomOperation("NotEqual", MaintainsVariableSpace = true)>]
    member this.NotEqual<'T, 'A when 'T : equality>
        (
            r: Runner<'A>,
            [<ProjectionParameter>] actual: 'A -> 'T,
            [<ProjectionParameter>] expected: 'A -> 'T
        ) : Runner<'A> =
        r |> Runner.AddTest (fun asserter args ->
            let actual = actual args
            let expected = expected args
            asserter.Push((actual <> expected), actual, expected)
        )

    /// Tests equality between two values using F# `=`.
    [<CustomOperation("NotEqualM", MaintainsVariableSpace = true)>]
    member this.NotEqualM<'T, 'A when 'T : equality>
        (
            r: Runner<'A>,
            [<ProjectionParameter>] actual: 'A -> 'T,
            [<ProjectionParameter>] expected: 'A -> 'T,
            message: string
        ) : Runner<'A> =
        r |> Runner.AddTest (fun asserter args ->
            let actual = actual args
            let expected = expected args
            asserter.Push((actual <> expected), actual, expected, message)
        )

    /// Tests equality between two values using F# `=`.
    [<CustomOperation("NotEqualA", MaintainsVariableSpace = true)>]
    member this.NotEqualA<'T, 'A when 'T : equality>
        (
            r: Runner<'A>,
            [<ProjectionParameter>] actual: 'A -> Async<'T>,
            [<ProjectionParameter>] expected: 'A -> 'T
        ) : Runner<'A> =
        r |> Runner.AddTestAsync (fun asserter args -> async {
            let expected = expected args
            let! actual = actual args
            return asserter.Push((actual <> expected), actual, expected)
        })

    /// Tests equality between two values using F# `=`.
    [<CustomOperation("NotEqualMA", MaintainsVariableSpace = true)>]
    member this.NotEqualMA<'T, 'A when 'T : equality>
        (
            r: Runner<'A>,
            [<ProjectionParameter>] actual: 'A -> Async<'T>,
            [<ProjectionParameter>] expected: 'A -> 'T,
            message: string
        ) : Runner<'A> =
        r |> Runner.AddTestAsync (fun asserter args -> async {
            let expected = expected args
            let! actual = actual args
            return asserter.Push((actual <> expected), actual, expected, message)
        })

    /// Tests equality between two values using JavaScript `==`.
    [<CustomOperation("JsEqual", MaintainsVariableSpace = true)>]
    member this.JsEqual<'T, 'A>
        (
            r: Runner<'A>,
            [<ProjectionParameter>] actual: 'A -> 'T,
            [<ProjectionParameter>] expected: 'A -> 'T
        ) : Runner<'A> =
        r |> Runner.AddTest (fun asserter args ->
            asserter.Equal(actual args, expected args)
        )

    /// Tests equality between two values using JavaScript `==`.
    [<CustomOperation("JsEqualM", MaintainsVariableSpace = true)>]
    member this.JsEqualM<'T, 'A>
        (
            r: Runner<'A>,
            [<ProjectionParameter>] actual: 'A -> 'T,
            [<ProjectionParameter>] expected: 'A -> 'T,
            message: string
        ) : Runner<'A> =
        r |> Runner.AddTest (fun asserter args ->
            asserter.Equal(actual args, expected args, message)
        )

    /// Tests equality between two values using JavaScript `==`.
    [<CustomOperation("JsEqualA", MaintainsVariableSpace = true)>]
    member this.JsEqualA<'T, 'A>
        (
            r: Runner<'A>,
            [<ProjectionParameter>] actual: 'A -> Async<'T>,
            [<ProjectionParameter>] expected: 'A -> 'T
        ) : Runner<'A> =
        r |> Runner.AddTestAsync (fun asserter args -> async {
            let expected = expected args
            let! actual = actual args
            return asserter.Equal(actual, expected)
        })

    /// Tests equality between two values using JavaScript `==`.
    [<CustomOperation("JsEqualMA", MaintainsVariableSpace = true)>]
    member this.JsEqualMA<'T, 'A>
        (
            r: Runner<'A>,
            [<ProjectionParameter>] actual: 'A -> Async<'T>,
            [<ProjectionParameter>] expected: 'A -> 'T,
            message: string
        ) : Runner<'A> =
        r |> Runner.AddTestAsync (fun asserter args -> async {
            let expected = expected args
            let! actual = actual args
            return asserter.Equal(actual, expected, message)
        })

    /// Tests equality between two values using JavaScript `===`.
    [<CustomOperation("DeepEqual", MaintainsVariableSpace = true)>]
    member this.DeepEqual<'T, 'A>
        (
            r: Runner<'A>,
            [<ProjectionParameter>] actual: 'A -> 'T,
            [<ProjectionParameter>] expected: 'A -> 'T
        ) : Runner<'A> =
        r |> Runner.AddTest (fun asserter args ->
            asserter.DeepEqual(actual args, expected args)
        )

    /// Tests equality between two values using JavaScript `===`.
    [<CustomOperation("DeepEqualM", MaintainsVariableSpace = true)>]
    member this.DeepEqualM<'T, 'A>
        (
            r: Runner<'A>,
            [<ProjectionParameter>] actual: 'A -> 'T,
            [<ProjectionParameter>] expected: 'A -> 'T,
            message: string
        ) : Runner<'A> =
        r |> Runner.AddTest (fun asserter args ->
            asserter.DeepEqual(actual args, expected args, message)
        )

    /// Tests equality between two values using JavaScript `===`.
    [<CustomOperation("DeepEqualA", MaintainsVariableSpace = true)>]
    member this.DeepEqualA<'T, 'A>
        (
            r: Runner<'A>,
            [<ProjectionParameter>] actual: 'A -> Async<'T>,
            [<ProjectionParameter>] expected: 'A -> 'T
        ) : Runner<'A> =
        r |> Runner.AddTestAsync (fun asserter args -> async {
            let expected = expected args
            let! actual = actual args
            return asserter.DeepEqual(actual, expected)
        })

    /// Tests equality between two values using JavaScript `===`.
    [<CustomOperation("DeepEqualMA", MaintainsVariableSpace = true)>]
    member this.DeepEqualMA<'T, 'A>
        (
            r: Runner<'A>,
            [<ProjectionParameter>] actual: 'A -> Async<'T>,
            [<ProjectionParameter>] expected: 'A -> 'T,
            message: string
        ) : Runner<'A> =
        r |> Runner.AddTestAsync (fun asserter args -> async {
            let expected = expected args
            let! actual = actual args
            return asserter.DeepEqual(actual, expected, message)
        })

    [<CustomOperation("ApproxEqual", MaintainsVariableSpace = true)>]
    member this.ApproxEqual<'T, 'A>
        (
            r: Runner<'A>,
            [<ProjectionParameter>] actual: 'A -> float,
            [<ProjectionParameter>] expected: 'A -> float
        ) : Runner<'A> =
        r |> Runner.AddTest (fun asserter args ->
            let actual = actual args
            let expected = expected args
            asserter.Push(abs (actual - expected) < 0.0001, actual, expected)
        )

    [<CustomOperation("ApproxEqualM", MaintainsVariableSpace = true)>]
    member this.ApproxEqualM<'T, 'A>
        (
            r: Runner<'A>,
            [<ProjectionParameter>] actual: 'A -> float,
            [<ProjectionParameter>] expected: 'A -> float,
            message: string
        ) : Runner<'A> =
        r |> Runner.AddTest (fun asserter args ->
            let actual = actual args
            let expected = expected args
            asserter.Push(abs (actual - expected) < 0.0001, actual, expected, message)
        )

    [<CustomOperation("NotApproxEqual", MaintainsVariableSpace = true)>]
    member this.NotApproxEqual<'T, 'A>
        (
            r: Runner<'A>,
            [<ProjectionParameter>] actual: 'A -> float,
            [<ProjectionParameter>] expected: 'A -> float
        ) : Runner<'A> =
        r |> Runner.AddTest (fun asserter args ->
            let actual = actual args
            let expected = expected args
            asserter.Push(abs (actual - expected) > 0.0001, actual, expected)
        )

    [<CustomOperation("NotApproxEqualM", MaintainsVariableSpace = true)>]
    member this.NotApproxEqualM<'T, 'A>
        (
            r: Runner<'A>,
            [<ProjectionParameter>] actual: 'A -> float,
            [<ProjectionParameter>] expected: 'A -> float,
            message: string
        ) : Runner<'A> =
        r |> Runner.AddTest (fun asserter args ->
            let actual = actual args
            let expected = expected args
            asserter.Push(abs (actual - expected) > 0.0001, actual, expected, message)
        )

    /// Checks that a boolean is true.
    [<CustomOperation("True", MaintainsVariableSpace = true)>]
    member this.True<'A>
        (
            r: Runner<'A>,
            [<ProjectionParameter>] value: 'A -> bool
        ) : Runner<'A> =
        r |> Runner.AddTest (fun asserter args ->
            asserter.Ok(value args)
        )

    /// Checks that a boolean is true.
    [<CustomOperation("TrueM", MaintainsVariableSpace = true)>]
    member this.TrueM<'A>
        (
            r: Runner<'A>,
            [<ProjectionParameter>] value: 'A -> bool,
            message: string
        ) : Runner<'A> =
        r |> Runner.AddTest (fun asserter args ->
            asserter.Ok(value args, message)
        )

    /// Checks that a boolean is true.
    [<CustomOperation("TrueA", MaintainsVariableSpace = true)>]
    member this.TrueA<'A>
        (
            r: Runner<'A>,
            [<ProjectionParameter>] value: 'A -> Async<bool>
        ) : Runner<'A> =
        r |> Runner.AddTestAsync (fun asserter args -> async {
            let! value = value args
            return asserter.Ok(value)
        })

    /// Checks that a boolean is true.
    [<CustomOperation("TrueMA", MaintainsVariableSpace = true)>]
    member this.TrueMA<'A>
        (
            r: Runner<'A>,
            [<ProjectionParameter>] value: 'A -> Async<bool>,
            message: string
        ) : Runner<'A> =
        r |> Runner.AddTestAsync (fun asserter args -> async {
            let! value = value args
            return asserter.Ok(value, message)
        })

    /// Checks that a boolean is false.
    [<CustomOperation("False", MaintainsVariableSpace = true)>]
    member this.False<'A>
        (
            r: Runner<'A>,
            [<ProjectionParameter>] value: 'A -> bool
        ) : Runner<'A> =
        r |> Runner.AddTest (fun asserter args ->
            asserter.Ok(not (value args))
        )

    /// Checks that a boolean is false.
    [<CustomOperation("FalseM", MaintainsVariableSpace = true)>]
    member this.FalseM<'A>
        (
            r: Runner<'A>,
            [<ProjectionParameter>] value: 'A -> bool,
            message: string
        ) : Runner<'A> =
        r |> Runner.AddTest (fun asserter args ->
            asserter.Ok(not (value args), message)
        )

    /// Checks that a boolean is false.
    [<CustomOperation("FalseA", MaintainsVariableSpace = true)>]
    member this.FalseA<'A>
        (
            r: Runner<'A>,
            [<ProjectionParameter>] value: 'A -> Async<bool>
        ) : Runner<'A> =
        r |> Runner.AddTestAsync (fun asserter args -> async {
            let! value = value args
            return asserter.Ok(not value)
        })

    /// Checks that a boolean is false.
    [<CustomOperation("FalseMA", MaintainsVariableSpace = true)>]
    member this.FalseMA<'A>
        (
            r: Runner<'A>,
            [<ProjectionParameter>] value: 'A -> Async<bool>,
            message: string
        ) : Runner<'A> =
        r |> Runner.AddTestAsync (fun asserter args -> async {
            let! value = value args
            return asserter.Ok(not value, message)
        })

    [<CustomOperation("For", MaintainsVariableSpace = true)>]
    member this.For
        (
            r: Runner<'A>,
            [<ProjectionParameter>] src: 'A -> #seq<'T>,
            [<ProjectionParameter>] attempt: 'A -> 'T -> Runner<'B>
        ) : Runner<'A> =
        fun asserter ->
            let rec loop (attempt: 'T -> Runner<'B>) (acc: Choice<'A, Async<'A>>) (src: list<'T>) =
                match src with
                | [] -> acc
                | e :: l ->
                    let r = attempt e
                    loop attempt (acc |> Runner.Bind (fun args -> r asserter |> Runner.Map (fun _ -> args))) l
            r asserter |> Runner.Bind (fun args ->
                loop (attempt args) (Choice1Of2 args) (List.ofSeq (src args))
            )

    [<CustomOperation("ForR", MaintainsVariableSpace = true)>]
    member this.ForRandom<'T, 'A, 'B>
        (
            r: Runner<'A>,
            [<ProjectionParameter>] times: 'A -> int,
            [<ProjectionParameter>] gen: 'A -> Random.Generator<'T>,
            [<ProjectionParameter>] attempt: 'A -> 'T -> Runner<'B>
        ) : Runner<'A> =
        fun asserter ->
            let rec loop (attempt: 'T -> Runner<'B>) (acc: Choice<'A, Async<'A>>) (src: list<'T>) =
                match src with
                | [] -> acc
                | e :: l ->
                    let r = attempt e
                    loop attempt
                        (acc |> Runner.Bind (fun args ->
                            r asserter |> Runner.Map (fun _ -> args)))
                        l
            r asserter |> Runner.Bind (fun args ->
                let gen = gen args
                let times = times args
                loop (attempt args) (Choice1Of2 args) [
                    for i = 0 to gen.Base.Length - 1 do yield gen.Base.[i]
                    for i = 1 to times do yield (gen.Next())
                ]
            )

    [<CustomOperation("Raises", MaintainsVariableSpace = true)>]
    member this.Raises<'T, 'A>
        (
            r: Runner<'A>,
            [<ProjectionParameter>] value: 'A -> 'T
        ) : Runner<'A> =
        r |> Runner.AddTest (fun asserter args ->
            try
                value args |> ignore
                asserter.Ok(false, "Expected raised exception")
            with _ ->
                asserter.Ok(true)
        )

    [<CustomOperation("RaisesM", MaintainsVariableSpace = true)>]
    member this.RaisesM<'T, 'A>
        (
            r: Runner<'A>,
            [<ProjectionParameter>] value: 'A -> 'T,
            message: string
        ) : Runner<'A> =
        r |> Runner.AddTest (fun asserter args ->
            try
                value args |> ignore
                asserter.Ok(false, message)
            with _ ->
                asserter.Ok(true, message)
        )

    [<CustomOperation("RaisesA", MaintainsVariableSpace = true)>]
    member this.RaisesA<'T, 'A>
        (
            r: Runner<'A>,
            [<ProjectionParameter>] value: 'A -> Async<'T>
        ) : Runner<'A> =
        r |> Runner.AddTestAsync (fun asserter args ->
            let value = value args
            async {
                try
                    let! _ = value
                    return asserter.Ok(false, "Expected raised exception")
                with _ ->
                    return asserter.Ok(true)
            }
        )

    [<CustomOperation("RaisesMA", MaintainsVariableSpace = true)>]
    member this.RaisesMA<'T, 'A>
        (
            r: Runner<'A>,
            [<ProjectionParameter>] value: 'A -> Async<'T>,
            message: string
        ) : Runner<'A> =
        r |> Runner.AddTestAsync (fun asserter args ->
            let value = value args
            async {
                try
                    let! _ = value
                    return asserter.Ok(false, message)
                with _ ->
                    return asserter.Ok(true, message)
            }
        )

    [<CustomOperation("Run", MaintainsVariableSpace = true)>]
    member this.RunSubtest
        (
            r: Runner<'A>,
            [<ProjectionParameter>] subtest: 'A -> Runner<'B>
        ) : Runner<'B> =
        fun asserter ->
            r asserter |> Runner.Bind (fun a -> subtest a asserter)

    member this.Bind(a: Async<'A>, f: 'A -> Runner<'B>) : Runner<'B> =
        fun asserter ->
            Choice2Of2 (async {
                let! a = a
                match f a asserter with
                | Choice1Of2 b -> return b
                | Choice2Of2 b -> return! b
            })

    member this.Yield(x) = fun asserter -> Choice1Of2 x

    member this.Return(x) = fun asserter -> Choice1Of2 x

    member this.Zero() = fun asserter -> Choice1Of2 Unchecked.defaultof<_>

    member this.For
        (
            r: Runner<'A>,
            y: 'A -> Runner<'B>
        ) : Runner<'B> =
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
type TestBuilder (name: string) =
    inherit SubtestBuilder ()

    member this.Run(e) =
        QUnit.Test name (fun asserter ->
            try
                match e asserter with
                | Choice1Of2 _ -> ()
                | Choice2Of2 asy ->
                    let ``done`` = asserter.Async()
                    async {
                        try
                            try
                                let! _ = asy
                                return ()
                            with e ->
                                return asserter.Ok(false, "Test threw an unexpected asynchronous exception")
                        finally
                            ``done``()
                    }
                    |> Async.Start
            with e ->
                asserter.Ok(false, "Test threw an unexpected synchronous exception")
        )

[<JavaScript>]
let Test name = new TestBuilder(name)

[<JavaScript>]
let Do = new SubtestBuilder()
