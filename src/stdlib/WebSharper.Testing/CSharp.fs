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
namespace WebSharper.Testing

open System
open System.Threading.Tasks
open Microsoft.FSharp.Quotations
open WebSharper
open WebSharper.Core
type private A = System.Attribute
type private T = System.AttributeTargets
type private U = System.AttributeUsageAttribute
type private BF = System.Reflection.BindingFlags

type TestKind =
    | Test = 0
    | Skip = 1
    | Todo = 2

[<JavaScript>]
type TestCategory() =

    let mutable asserter : QUnit.Asserter = Unchecked.defaultof<_>

    let run kind name f =
        match kind with
        | TestKind.Test -> QUnit.Test name f
        | TestKind.Skip -> QUnit.Skip name f
        | TestKind.Todo -> QUnit.Todo name f
        | _ -> invalidArg "kind" "Invalid test kind"

    // So that mistyping Equal as Equals gives a warning immediately
    [<JavaScript(false); Obsolete "Use Equal for testing, or object.Equals if that is needed">]
    static member Equals(x: obj, y: obj) = false

    member this.Equal(x: 'T, y: 'T) =
        asserter.Push((x = y), x, y)

    member this.Equal(x: 'T, y: 'T, message) =
        asserter.Push((x = y), x, y, message)

    member this.NotEqual(x: 'T, y: 'T) =
        asserter.Push((x <> y), x, y)

    member this.NotEqual(x: 'T, y: 'T, message) =
        asserter.Push((x <> y), x, y, message)

    member this.JsEqual(x: 'T, y: 'T) =
        asserter.Equal(x, y)

    member this.JsEqual(x: 'T, y: 'T, message) =
        asserter.Equal(x, y, message)

    member this.NotJsEqual(x: 'T, y: 'T) =
        asserter.NotEqual(x, y)

    member this.NotJsEqual(x: 'T, y: 'T, message) =
        asserter.NotEqual(x, y, message)

    member this.DeepEqual(x: 'T, y: 'T) =
        asserter.DeepEqual(x, y)

    member this.DeepEqual(x: 'T, y: 'T, message) =
        asserter.DeepEqual(x, y, message)

    member this.NotDeepEqual(x: 'T, y: 'T) =
        asserter.NotDeepEqual(x, y)

    member this.NotDeepEqual(x: 'T, y: 'T, message) =
        asserter.NotDeepEqual(x, y, message)

    member this.StrictEqual(x: 'T, y: 'T) =
        asserter.StrictEqual(x, y)

    member this.StrictEqual(x: 'T, y: 'T, message) =
        asserter.StrictEqual(x, y, message)

    member this.NotStrictEqual(x: 'T, y: 'T) =
        asserter.NotStrictEqual(x, y)

    member this.NotStrictEqual(x: 'T, y: 'T, message) =
        asserter.NotStrictEqual(x, y, message)

    member this.PropEqual(x: 'T, y: 'T) =
        asserter.PropEqual(x, y)

    member this.PropEqual(x: 'T, y: 'T, message) =
        asserter.PropEqual(x, y, message)

    member this.NotPropEqual(x: 'T, y: 'T) =
        asserter.NotPropEqual(x, y)

    member this.NotPropEqual(x: 'T, y: 'T, message) =
        asserter.NotPropEqual(x, y, message)

    member this.ApproxEqual(x: float, y: float) =
        asserter.Push(abs (x - y) < 0.0001, x, y)

    member this.ApproxEqual(x: float, y: float, epsilon: float) =
        asserter.Push(abs (x - y) < epsilon, x, y)

    member this.ApproxEqual(x: float, y: float, message) =
        asserter.Push(abs (x - y) < 0.0001, x, y, message)

    member this.ApproxEqual(x: float, y: float, epsilon: float, message) =
        asserter.Push(abs (x - y) < epsilon, x, y, message)

    member this.NotApproxEqual(x: float, y: float) =
        asserter.Push(abs (x - y) > 0.0001, x, y)

    member this.NotApproxEqual(x: float, y: float, epsilon: float) =
        asserter.Push(abs (x - y) > epsilon, x, y)

    member this.NotApproxEqual(x: float, y: float, message) =
        asserter.Push(abs (x - y) > 0.0001, x, y, message)

    member this.NotApproxEqual(x: float, y: float, epsilon: float, message) =
        asserter.Push(abs (x - y) > epsilon, x, y, message)

    member this.IsTrue(value) =
        asserter.Ok(value)

    member this.IsTrue(value, message) =
        asserter.Ok(value, message)

    member this.IsFalse(value) =
        asserter.NotOk(value)

    member this.IsFalse(value, message) =
        asserter.NotOk(value)

    member this.Raises(expr: Func<'T>) =
        try
            expr.Invoke() |> ignore
            asserter.Ok(false, "Expected raised exception")
        with _ ->
            asserter.Ok(true)

    member this.Raises(expr: Func<'T>, message) =
        try
            expr.Invoke() |> ignore
            asserter.Ok(false, message)
        with _ ->
            asserter.Ok(true, message)

    member this.Raises(expr: Action) =
        try
            expr.Invoke()
            asserter.Ok(false, "Expected raised exception")
        with _ ->
            asserter.Ok(true)

    member this.Raises(expr: Action, message) =
        try
            expr.Invoke()
            asserter.Ok(false, message)
        with _ ->
            asserter.Ok(true, message)

    member this.Expect(n: int) =
        asserter.Expect(n)

    member internal this.Run(name: string, kind: TestKind, f: unit -> unit) =
        run kind name (fun a ->
            asserter <- a
            f())

    member internal this.Run(name: string, kind: TestKind, f: unit -> Task) =
        run kind name (fun a ->
            asserter <- a
            let ``done`` = asserter.Async()
            let t = f()
            t.ContinueWith(fun (x: Task) -> ``done``()) |> ignore
            if t.Status = TaskStatus.Created then t.Start())

[<Sealed; U(T.Class|||T.Method)>]
type TestAttribute(name: string, kind: TestKind) =
    inherit Attribute()
    new(name) = TestAttribute(name, TestKind.Test)
    new() = TestAttribute(null)
    member this.Name = name
    member this.Kind = kind

[<AutoOpen>]
module private Internals =

    let coerce<'T> (e: Expr) : Expr<'T> =
        Expr.Coerce(e, typeof<'T>)
        |> Expr.Cast

    type ChoiceBuilder() =
        member this.Bind(x: Choice<'a, 'b>, f: 'a -> Choice<'c, 'b>) =
            match x with
            | Choice1Of2 x -> f x
            | Choice2Of2 m -> Choice2Of2 m
        member this.Return(x) = Choice1Of2 x
        member this.ReturnFrom(x: Choice<'a, 'b>) = x
    let choice = ChoiceBuilder()

type TestGenerator() =
    inherit Generator()

    let findTestMethods (x: Type) =
        x.GetMethods(BF.Instance ||| BF.Public)
        |> Array.choose (fun m ->
            m.GetCustomAttributes(false)
            |> Array.tryPick (function
                | :? TestAttribute as a ->
                    let name = match a.Name with null -> m.Name | n -> n
                    Some (m, name, a.Kind)
                | _ -> None))

    let genTestCategory' (t: Type) =
        match t.GetConstructor([||]) with
        | null -> Choice2Of2 (t.FullName + " must have a default constructor.")
        | ctor ->
            let make = Expr.NewObject(ctor, [])
            let methods = findTestMethods t
            let findRunFor (m: Reflection.MethodInfo) name kind v = choice {
                let fullName = m.DeclaringType.FullName + "." + m.Name
                let! e =
                    match m.GetParameters() with
                    | [||] -> Choice1Of2 (Expr.Call(v, m, []))
                    | [|p|] ->
                        let sampleTy = typedefof<RandomValues.Sample<_>>.MakeGenericType(p.ParameterType)
                        let sampleCtor = sampleTy.GetConstructor([||])
                        let sample = Expr.NewObject(sampleCtor, [])
                        let dataMeth = sampleTy.GetProperty("Data").GetGetMethod()
                        let data = Expr.Call(sample, dataMeth, [])
                        let x = Var("x", p.ParameterType)
                        let fn = Expr.Lambda(x, Expr.Call(v, m, [Expr.Var x]))
                        let iter =
                            typedefof<option<_>>.Assembly.GetType("Microsoft.FSharp.Collections.ListModule")
                                .GetMethod("Iterate")
                                .MakeGenericMethod(p.ParameterType)
                        Expr.Call(iter, [fn; data])
                        |> Choice1Of2
                    | _ -> Choice2Of2 (fullName + " has invalid argument count, \
                        should have none (for a simple test) or one (for a property test).")
                let v = coerce<TestCategory> v
                if m.ReturnType = typeof<Void> then
                    return <@ (%v).Run(name, kind, fun () -> %%e : unit) @>
                elif m.ReturnType = typeof<Task> then
                    return <@ (%v).Run(name, kind, fun () -> %%e : Task) @>
                else
                    let msg = fullName + " has invalid type, should take no argument \
                        and return either void or Task. It will not be run."
                    return! Choice2Of2 msg
            }
            ((<@ () @>, []), methods)
            ||> Array.fold (fun (pred, w) (m, name, kind) ->
                let var = Var("x", t)
                let v = Expr.Var var
                match findRunFor m name kind v with
                | Choice1Of2 run ->
                    <@ %pred; (%%(Expr.Let(var, make, <@ %run @>))) @>, w
                | Choice2Of2 msg -> pred, msg :: w)
            |> Choice1Of2

    override this.Generate(m) =
        try
            let asm =
                match m.Parameter with
                | None ->
                    match m.Member with
                    | GeneratedMethod (typ, _) ->
                        try (WebSharper.Core.AST.Reflection.LoadTypeDefinition typ).Assembly
                        with e -> failwithf "Could not load containing type %A:\n%O" typ e
                    | _ -> failwith "TestGenerator must be used on a non-implementation method."
                | Some (:? string as n) ->
                    try Reflection.Assembly.Load(n)
                    with _ -> failwithf "Failed to load assembly: %s" n
                | Some _ -> failwithf "Argument to TestGenerator must be either an assembly name or none."
            asm.GetTypes()
            |> Array.choose (fun t ->
                t.GetCustomAttributesData()
                |> Seq.tryPick (fun cad ->
                    if cad.Constructor.DeclaringType.FullName = typeof<TestAttribute>.FullName then
                        match cad.ConstructorArguments.Count with
                        | 0 -> Some (t, t.Name)
                        | 1 -> Some (t, cad.ConstructorArguments.[0].Value :?> string)
                        | _ -> failwith "Impossible"
                    else None))
            |> Array.fold (fun x (t, category) ->
                match x with
                | Choice1Of2 (e, w) ->
                    match genTestCategory' t with
                    | Choice1Of2 (e', w') ->
                        Choice1Of2 (<@ { name = category; run = fun () -> %e' } :: %e @>, w @ w')
                    | Choice2Of2 err -> Choice2Of2 err
                | Choice2Of2 err -> Choice2Of2 err
            ) (Choice1Of2 (<@ [] @>, []))
            |> function
            | Choice1Of2 (e, warnings) ->
                (GeneratedQuotation <@@ fun () -> Runner.RunTests %e @@>, warnings)
                ||> List.fold (fun r w -> GeneratorWarning (w, r))
            | Choice2Of2 err -> GeneratorError err
        with e -> GeneratorError ("TestGenerator error:" + e.Message)
