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

    open WebSharper.Testing.Random.Internal

    let asserter = ty "WebSharper.Testing.Pervasives+QUnit+Asserter"
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
                        Function([id],
                            // cast to "any" is needed because sometimes we are calling a generator function that does
                            // not take any arguments
                            Return (mkSample t (Application(Cast(Var TSType.Any, gen), [Var id], Pure, Some 1)) (cInt 100)))
                        attempt
                    ]
                )
                |> MacroOk
            | [runner; attempt] ->
                Call(c.This, c.DefiningType, m c.Method.Generics,
                    [
                        runner
                        Function([], Return (mkSample t (mkGenerator c.Method.Generics.Head) (cInt 100)))
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

module QUnit =

    [<Stub>]
    type Asserter =

        [<Stub; Name "ok">]
        member this.Ok(value: bool) = X<unit>

        [<Stub; Name "ok">]
        member this.Ok(value: bool, message: string) = X<unit>

        [<Stub; Name "notOk">]
        member this.NotOk(value: bool) = X<unit>

        [<Stub; Name "notOk">]
        member this.NotOk(value: bool, message: string) = X<unit>

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

    // Unlike the methods above, Test and Module must not be implemented as X<_>.
    // They are meant to be called from the top-level, which means they will be called
    // from the server side too. Since X<_>'s .NET implementation throws an exception,
    // it is not suitable in this case.

    [<Inline "QUnit.test($name, $callback)">]
    let Test (name: string) (callback: Asserter -> unit) = ()

    [<Inline "QUnit.module($name)">]
    let Module (name: string) = ()

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

    let WithTimeout timeOut a = a // TODO: enable timeout
//        async {
//            let! child = Async.StartChild (a, timeOut)
//            return! child
//        }

[<JavaScript>]
type SubtestBuilder () =

    [<CustomOperation("expect", MaintainsVariableSpace = true)>]
    member this.Expect
        (
            r: Runner<'A>,
            [<ProjectionParameter>] assertionCount: 'A -> int
        ) : Runner<'A> =
        r |> Runner.AddTest (fun asserter args ->
            asserter.Expect(assertionCount args)
        )

    /// Tests equality between two values using F# `=`.
    [<CustomOperation("equal", MaintainsVariableSpace = true)>]
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
    [<CustomOperation("equalMsg", MaintainsVariableSpace = true)>]
    member this.EqualMsg<'T, 'A when 'T : equality>
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
    [<CustomOperation("equalAsync", MaintainsVariableSpace = true)>]
    member this.EqualAsync<'T, 'A when 'T : equality>
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
    [<CustomOperation("equalMsgAsync", MaintainsVariableSpace = true)>]
    member this.EqualMsgAsync<'T, 'A when 'T : equality>
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
    [<CustomOperation("notEqual", MaintainsVariableSpace = true)>]
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
    [<CustomOperation("notEqualMsg", MaintainsVariableSpace = true)>]
    member this.NotEqualMsg<'T, 'A when 'T : equality>
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
    [<CustomOperation("notEqualAsync", MaintainsVariableSpace = true)>]
    member this.NotEqualAsync<'T, 'A when 'T : equality>
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
    [<CustomOperation("notEqualMsgAsync", MaintainsVariableSpace = true)>]
    member this.NotEqualMsgAsync<'T, 'A when 'T : equality>
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
    [<CustomOperation("jsEqual", MaintainsVariableSpace = true)>]
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
    [<CustomOperation("jsEqualMsg", MaintainsVariableSpace = true)>]
    member this.JsEqualMsg<'T, 'A>
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
    [<CustomOperation("jsEqualAsync", MaintainsVariableSpace = true)>]
    member this.JsEqualAsync<'T, 'A>
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
    [<CustomOperation("jsEqualMsgAsync", MaintainsVariableSpace = true)>]
    member this.JsEqualMsgAsync<'T, 'A>
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
    [<CustomOperation("deepEqual", MaintainsVariableSpace = true)>]
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
    [<CustomOperation("deepEqualMsg", MaintainsVariableSpace = true)>]
    member this.DeepEqualMsg<'T, 'A>
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
    [<CustomOperation("deepEqualAsync", MaintainsVariableSpace = true)>]
    member this.DeepEqualAsync<'T, 'A>
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
    [<CustomOperation("deepEqualMsgAsync", MaintainsVariableSpace = true)>]
    member this.DeepEqualMsgAsync<'T, 'A>
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

    /// Tests approximate equality between two floats.
    [<CustomOperation("approxEqual", MaintainsVariableSpace = true)>]
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

    /// Tests approximate equality between two floats.
    [<CustomOperation("approxEqualMsg", MaintainsVariableSpace = true)>]
    member this.ApproxEqualMsg<'T, 'A>
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

    /// Tests approximate equality between two floats.
    [<CustomOperation("approxEqualAsync", MaintainsVariableSpace = true)>]
    member this.ApproxEqualAsync<'T, 'A>
        (
            r: Runner<'A>,
            [<ProjectionParameter>] actual: 'A -> Async<float>,
            [<ProjectionParameter>] expected: 'A -> float
        ) : Runner<'A> =
        r |> Runner.AddTestAsync (fun asserter args -> async {
            let expected = expected args
            let! actual = actual args 
            return asserter.Push(abs (actual - expected) < 0.0001, actual, expected)
        })

    /// Tests approximate equality between two floats.
    [<CustomOperation("approxEqualMsgAsync", MaintainsVariableSpace = true)>]
    member this.ApproxEqualMsgAsync<'T, 'A>
        (
            r: Runner<'A>,
            [<ProjectionParameter>] actual: 'A -> Async<float>,
            [<ProjectionParameter>] expected: 'A -> float,
            message: string
        ) : Runner<'A> =
        r |> Runner.AddTestAsync (fun asserter args -> async {
            let expected = expected args
            let! actual = actual args 
            return asserter.Push(abs (actual - expected) < 0.0001, actual, expected, message)
        })

    /// Tests approximate inequality between two floats.
    [<CustomOperation("notApproxEqual", MaintainsVariableSpace = true)>]
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

    /// Tests approximate inequality between two floats.
    [<CustomOperation("notApproxEqualMsg", MaintainsVariableSpace = true)>]
    member this.NotApproxEqualMsg<'T, 'A>
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

    /// Tests approximate inequality between two floats.
    [<CustomOperation("notApproxEqualAsync", MaintainsVariableSpace = true)>]
    member this.NotApproxEqualAsync<'T, 'A>
        (
            r: Runner<'A>,
            [<ProjectionParameter>] actual: 'A -> Async<float>,
            [<ProjectionParameter>] expected: 'A -> float
        ) : Runner<'A> =
        r |> Runner.AddTestAsync (fun asserter args -> async {
            let expected = expected args
            let! actual = actual args 
            return asserter.Push(abs (actual - expected) > 0.0001, actual, expected)
        })

    /// Tests approximate inequality between two floats.
    [<CustomOperation("notApproxEqualMsgAsync", MaintainsVariableSpace = true)>]
    member this.NotApproxEqualMsgAsync<'T, 'A>
        (
            r: Runner<'A>,
            [<ProjectionParameter>] actual: 'A -> Async<float>,
            [<ProjectionParameter>] expected: 'A -> float,
            message: string
        ) : Runner<'A> =
        r |> Runner.AddTestAsync (fun asserter args -> async {
            let expected = expected args
            let! actual = actual args 
            return asserter.Push(abs (actual - expected) > 0.0001, actual, expected, message)
        })

    /// Checks that a boolean is true.
    [<CustomOperation("isTrue", MaintainsVariableSpace = true)>]
    member this.IsTrue<'A>
        (
            r: Runner<'A>,
            [<ProjectionParameter>] value: 'A -> bool
        ) : Runner<'A> =
        r |> Runner.AddTest (fun asserter args ->
            asserter.Ok(value args)
        )

    /// Checks that a boolean is true.
    [<CustomOperation("isTrueMsg", MaintainsVariableSpace = true)>]
    member this.IsTrueMsg<'A>
        (
            r: Runner<'A>,
            [<ProjectionParameter>] value: 'A -> bool,
            message: string
        ) : Runner<'A> =
        r |> Runner.AddTest (fun asserter args ->
            asserter.Ok(value args, message)
        )

    /// Checks that a boolean is true.
    [<CustomOperation("isTrueAsync", MaintainsVariableSpace = true)>]
    member this.IsTrueAsync<'A>
        (
            r: Runner<'A>,
            [<ProjectionParameter>] value: 'A -> Async<bool>
        ) : Runner<'A> =
        r |> Runner.AddTestAsync (fun asserter args -> async {
            let! value = value args 
            return asserter.Ok(value)
        })

    /// Checks that a boolean is true.
    [<CustomOperation("isTrueMsgAsync", MaintainsVariableSpace = true)>]
    member this.IsTrueMsgAsync<'A>
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
    [<CustomOperation("isFalse", MaintainsVariableSpace = true)>]
    member this.IsFalse<'A>
        (
            r: Runner<'A>,
            [<ProjectionParameter>] value: 'A -> bool
        ) : Runner<'A> =
        r |> Runner.AddTest (fun asserter args ->
            asserter.NotOk(value args)
        )

    /// Checks that a boolean is false.
    [<CustomOperation("isFalseMsg", MaintainsVariableSpace = true)>]
    member this.IsFalseMsg<'A>
        (
            r: Runner<'A>,
            [<ProjectionParameter>] value: 'A -> bool,
            message: string
        ) : Runner<'A> =
        r |> Runner.AddTest (fun asserter args ->
            asserter.NotOk(value args, message)
        )

    /// Checks that a boolean is false.
    [<CustomOperation("isFalseAsync", MaintainsVariableSpace = true)>]
    member this.IsFalseAsync<'A>
        (
            r: Runner<'A>,
            [<ProjectionParameter>] value: 'A -> Async<bool>
        ) : Runner<'A> =
        r |> Runner.AddTestAsync (fun asserter args -> async {
            let! value = value args 
            return asserter.NotOk(value)
        })

    /// Checks that a boolean is false.
    [<CustomOperation("isFalseMsgAsync", MaintainsVariableSpace = true)>]
    member this.IsFalseAsync<'A>
        (
            r: Runner<'A>,
            [<ProjectionParameter>] value: 'A -> Async<bool>,
            message: string
        ) : Runner<'A> =
        r |> Runner.AddTestAsync (fun asserter args -> async {
            let! value = value args 
            return asserter.NotOk(value, message)
        })

    /// Runs a test for each element in a sequence.
    [<CustomOperation("forEach", MaintainsVariableSpace = true)>]
    member this.ForEach
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

    /// Runs a test for each element in a randomly generated set.
    [<CustomOperation("propertyWithSample", MaintainsVariableSpace = true)>]
    member this.PropertyWithSample<'T, 'A, 'B>
        (
            r: Runner<'A>,
            [<ProjectionParameter>] sample: 'A -> Random.Sample<'T>,
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
                let sample = sample args
                loop (attempt args) (Choice1Of2 args) sample.Data
            )

    /// Runs a test for each element in a randomly generated sample.
    member this.For(sample: Random.Sample<'A>, f: 'A -> Runner<'B>) : Runner<'B> =
        fun asserter ->
            let rec loop (acc: Choice<'B, Async<'B>>) (src: list<'A>) =
                match src with
                | [] -> acc
                | e :: l ->
                    let r = f e
                    loop (acc |> Runner.Bind (fun _ -> r asserter)) l
            loop (Choice1Of2 JS.Undefined) sample.Data

    /// Runs a test for 100 occurrences of a random generator.
    [<CustomOperation("propertyWith", MaintainsVariableSpace = true)>]
    [<Macro(typeof<Internal.TestPropertyMacro>)>]
    member this.PropertyWith<'T, 'A, 'B>
        (
            r: Runner<'A>,
            [<ProjectionParameter>] gen: 'A -> Random.Generator<'T>,
            [<ProjectionParameter>] attempt: 'A -> 'T -> Runner<'B>
        ) : Runner<'A> =
            this.PropertyWithSample(r, (fun args -> Random.Sample<'T>(gen args)), attempt)

    /// Runs a test for 100 occurrences of a random generator.
    member this.For(gen: Random.Generator<'A>, f: 'A -> Runner<'B>) : Runner<'B> =
        this.For(Random.Sample(gen), f)

    /// Runs a test for 100 random occurrences.
    [<CustomOperation("property", MaintainsVariableSpace = true)>]
    [<Macro(typeof<Internal.TestPropertyMacro>)>]
    member this.Property<'T, 'A, 'B>
        (
            r: Runner<'A>,
            [<ProjectionParameter>] attempt: 'A -> 'T -> Runner<'B>
        ) : Runner<'A> =
            this.PropertyWithSample(r, (fun _ -> Random.Sample<'T>()), attempt)

    /// Checks that an expression raises an exception.
    [<CustomOperation("raises", MaintainsVariableSpace = true)>]
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

    /// Checks that an expression raises an exception.
    [<CustomOperation("raisesMsg", MaintainsVariableSpace = true)>]
    member this.RaisesMsg<'T, 'A>
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

    /// Checks that an expression raises an exception.
    [<CustomOperation("raisesAsync", MaintainsVariableSpace = true)>]
    member this.RaisesAsync<'T, 'A>
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

    /// Checks that an expression raises an exception.
    [<CustomOperation("raisesMsgAsync", MaintainsVariableSpace = true)>]
    member this.RaisesMsgAsync<'T, 'A>
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

    /// Runs a sub-test.
    [<CustomOperation("run", MaintainsVariableSpace = true)>]
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

    member this.Zero() = fun asserter -> Choice1Of2 JS.Undefined

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

[<JavaScript>]
let Test name = new TestBuilder(name)

[<JavaScript>]
let Do = new SubtestBuilder()

[<JavaScript>]
let PropertyWith name gen f =
    Test name { propertyWith gen f }

[<JavaScript>]
let PropertyWithSample name set f =
    Test name { propertyWithSample set f }

[<Macro(typeof<Internal.PropertyMacro>)>]
let Property<'T, 'O> name (f: 'T -> Runner<'O>) =
    PropertyWith name (Random.Auto<'T>()) f
