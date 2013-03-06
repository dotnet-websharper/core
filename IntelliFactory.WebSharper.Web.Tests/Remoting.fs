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

/// Tests Remoting functionality, including instance and static
/// remote functions, returning async, unit and sync values, and
/// sending/returning unions, lists, options, scalars and records.
namespace IntelliFactory.WebSharper.Web.Tests

open IntelliFactory.WebSharper
module H = IntelliFactory.WebSharper.Html.Default

module Server =

    let counter = ref 0

    [<Remote>]
    let reset () =
        counter := 0
        async.Return ()

    [<Remote>]
    let sleep () =
        System.Threading.Thread.Sleep 100
        async.Return ()

    [<Remote>]
    let f1 () =
        incr counter

    [<Remote>]
    let f2 () =
        incr counter
        async { return () }

    [<Remote>]
    let f3 (x: int) =
        x + 1

    [<Remote>]
    let f4 (x: int) =
        async { return x + 1 }

    [<Remote>]
    let f5 (x: option<int>) =
        match x with
        | None   -> 0
        | Some x -> x + 1
        |> async.Return

    [<Remote>]
    let f6 (x: string) (y: string) =
        x + y
        |> async.Return

    [<Remote>]
    let f7 (x: string, y: string) =
        x + y
        |> async.Return

    [<Remote>]
    let f8 (xy: float * float) =
        fst xy + snd xy
        |> async.Return

    [<Remote>]
    let f9 (x: list<int>) =
        List.rev x
        |> async.Return

    [<Remote>]
    let f10 (x: System.DateTime) =
        x.AddDays 1.0
        |> async.Return

    [<Remote>]
    let f11 (x: int) =
        (x, x + 1)
        |> async.Return

    [<Remote>]
    let f12 (x: System.TimeSpan) min =
        x.Add(System.TimeSpan.FromMinutes min)
        |> async.Return

    type T1 =
        | A of int
        | B of int * T1

        [<JavaScript>]
        member this.Head =
            match this with
            | A x      -> x
            | B (x, _) -> x

    [<Remote>]
    let f13 x y =
        B (x, y)
        |> async.Return

    type T2 =
        {
            X : string
        }

        [<JavaScript>]
        member this.Body =
            this.X

    [<Remote>]
    let f14 x =
        { x with X = x.X + "!" }
        |> async.Return

    [<Remote>]
    let reverse (x: string) =
        new System.String(Array.rev (x.ToCharArray()))
        |> async.Return

    type IHandler =

        [<Remote>]
        abstract member M1 : unit -> unit

        [<Remote>]
        abstract member M2 : unit -> Async<unit>

        [<Remote>]
        abstract member M3 : int -> Async<int>

        [<Remote>]
        abstract member M4 : int * int -> Async<int>

        [<Remote>]
        abstract member M5 : int -> int -> Async<int>

    type Handler() =
        interface IHandler with

            member this.M1() =
                incr counter

            member this.M2() =
                incr counter
                async.Return ()

            member this.M3 x =
                async.Return (x + 1)

            member this.M4 (a, b) =
                async.Return (a + b)

            member this.M5 a b =
                async.Return (a + b)

    let Initialize () =
        SetRpcHandlerFactory {
            new IRpcHandlerFactory with
                member this.Create t =
                    if t = typeof<IHandler> then
                        Some (Handler() :> obj)
                    else
                        None
        }

    [<Remote>]
    let count () = async.Return counter.Value

type Harness [<JavaScript>] () =
    let mutable expected = 0
    let mutable executed = 0
    let mutable passed = 0
    let mutable name = "?"

    [<JavaScript>]
    member this.Test t =
        name <- t

    [<JavaScript>]
    member this.AsyncEquals a b =
        expected <- expected + 1
        async {
            let! v = a
            do executed <- executed + 1
            return
                if v = b then
                    passed <- passed + 1
                    JavaScript.Log("pass:", name)
                else
                    JavaScript.Log("fail:", name, v, b)
        }

    [<JavaScript>]
    member this.AsyncSatisfy a prop =
        expected <- expected + 1
        async {
            let! v = a
            do executed <- executed + 1
            return
                if prop v then
                    passed <- passed + 1
                    JavaScript.Log("pass:", name)
                else
                    JavaScript.Log("fail:", name, v)
        }

    [<JavaScript>]
    member this.AssertEquals a b =
        expected <- expected + 1
        executed <- executed + 1
        if a = b then
            passed <- passed + 1
            JavaScript.Log("pass:", name)
        else
            JavaScript.Log("fail:", name, a, b)

    [<JavaScript>]
    member this.Summarize() =
        H.Div [
            H.Div [H.Text ("Expected: " + string expected)]
            H.Div [H.Text ("Executed: " + string executed)]
            H.Div [H.Text ("Passed: " + string passed)]
        ]

module RemotingTestSuite =

    [<JavaScript>]
    let harness = Harness()

    [<JavaScript>]
    let test n = harness.Test n

    [<JavaScript>]
    let satisfy a b = harness.AsyncSatisfy a b

    [<JavaScript>]
    let ( =? ) a b = harness.AsyncEquals a b

    [<JavaScript>]
    let ( =?!) a b = harness.AssertEquals a b

    [<JavaScript>]
    let RunTests (dom: IntelliFactory.WebSharper.Dom.Element) =
        JavaScript.Log("Starting tests", dom)
        async {
            do test "unit -> unit"
            do! Server.reset()
            do Server.f1()
            do! Server.sleep ()
            do! Server.count() =? 1

            do test "unit -> Async<unit>"
            do! Server.f2()
            do! Server.count() =? 2

            do test "int -> int"
            do Server.f3 15 =?! 16

            do test "int -> Async<int>"
            do! Server.f4 8 =? 9

            do test "option<int> -> Async<int>"
            do! Server.f5 None =? 0
            do! Server.f5 (Some -40) =? -39

            do test "string -> string -> Async<string>"
            do! Server.f6 "a" "b" =? "ab"

            do test "string * string -> Async<string>"
            do! Server.f7 ("a", "b") =? "ab"

            do test "(float * float) -> Async<float>"
            do! Server.f8 (2.3, 4.5) =? 2.3 + 4.5

            do test "list<int> -> Async<list<int>>"
            do! Server.f9 [1; 2; 3] =? [3; 2; 1]
            do! satisfy (Server.f9 [1; 2; 3]) (fun list ->
                Array.ofSeq list = [| 3; 2; 1 |])

            do test "DateTime -> Async<DateTime>"
            let dt = System.DateTime.UtcNow
            do! Server.f10 dt =? dt.AddDays 1.0

            do test "int -> Async<int * int>"
            do! Server.f11 40 =? (40, 41)

            do test "TimeSpan -> float -> Async<TimeSpan>"
            let ts = System.TimeSpan.FromSeconds 14123.
            do! Server.f12 ts 1.25 =? ts.Add (System.TimeSpan.FromMinutes 1.25)

            do test "int -> T1 -> Async<T1>"
            do! Server.f13 40 (Server.B (8, Server.A 9)) =? 
                Server.B (40, Server.B (8, Server.A 9))
            do! satisfy (Server.f13 8 (Server.A 9)) (fun x ->
                x.Head = 8)

            do test "T2 -> Async<T2>"
            do! Server.f14 { X = "X" } =? { X = "X!" }
            do! satisfy (Server.f14 {X = "X"}) (fun x ->
                x.Body = "X!")

            do test "M1"
            do Remote<Server.IHandler>.M1()
            do! Server.sleep()
            do! Server.count() =? 3

            do test "M2"
            do! Remote<Server.IHandler>.M2()
            do! Server.count() =? 4

            do test "M3"
            do! Remote<Server.IHandler>.M3 40 =? 41

            do test "M4"
            do! Remote<Server.IHandler>.M4 (1, 2) =? 3

            do test "M5"
            do! Remote<Server.IHandler>.M5 3 6 =? 9

            do test "reverse"
            do! Server.reverse "abc#quit;;" =? ";;tiuq#cba"
            do! Server.reverse "c#" =? "#c"
            do! Server.reverse "\u00EF\u00BB\u00BF" =? "\u00BF\u00BB\u00EF"
            do! Server.reverse "c\127\127\127#" =? "#\127\127\127c"
            do ignore (dom.AppendChild (harness.Summarize().Dom))
        }
        |> Async.Start

type RemotingTests() =
    inherit Web.Control()

    [<JavaScript>]
    override this.Body =
        let d = H.Div []
        RemotingTestSuite.RunTests d.Dom
        d :> _
