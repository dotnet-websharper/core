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

// Tests Remoting functionality, including instance and static
// remote functions, returning async, unit and sync values, and
// sending/returning unions, lists, options, scalars and records.
namespace WebSharper.Web.Tests

open WebSharper
open WebSharper.JavaScript

module Server =
    open System

    let counter1 = ref 123
    let counter2 = ref 176
    let counterM1 = ref 243
    let counterM2 = ref 367

    [<JavaScript>]
    type OptionsRecord =
        {
            [<OptionalField>] x : option<int>
            y : option<int>
        }

    [<JavaScript>]
    type RecordUnion =
        | Record of Record

    and [<JavaScript>] Record =
        {
            a : int
            b : string
        }

    [<JavaScript>]
    type UnionWithConstants =
        | [<Constant "itsastring">] UString
        | [<Constant 1436>] UInt
        | [<Constant true>] UBool
        | UNotConst

    [<Prototype false>]
    type NoProtoTypes =
        {
            [<CompiledName "$TYPES">]
            Types: string[][]
        }

    [<Remote>]
    let reset1 () =
        counter1 := 123
        async.Return ()

    [<Remote>]
    let reset2 () =
        counter2 := 176
        async.Return ()

    [<Remote>]
    let resetM1 () =
        counterM1 := 243
        async.Return ()

    [<Remote>]
    let resetM2 () =
        counterM2 := 367
        async.Return ()

    [<Remote>]
    let f1 () =
        incr counter1

    [<Remote>]
    let f2 () =
        incr counter2
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
    let f9_1 (x: list<Record>) =
        List.rev x
        |> async.Return

    [<Remote>]
    let f10 (x: System.DateTime) =
        if System.DateTime.UtcNow - x < TimeSpan.FromMinutes 1. then
            x.AddDays 1.0
            |> async.Return
        else 
            failwith "DateTime remoting not getting current UTC time from client correctly"

    [<Remote>]
    let f26 (x: System.DateTimeOffset) days =
        if System.DateTimeOffset.UtcNow - x < TimeSpan.FromMinutes 1. then
            x.AddDays days
            |> async.Return
        else 
            failwith "DateTimeOffset remoting not getting current UTC time from client correctly"

    [<Remote>]
    let sendDTO (o: System.DateTimeOffset) =
        async.Return (o.UtcDateTime.Hour, o.Hour, o.Offset.Hours)  

    [<Remote>]
    let getDTO () =        
        let d = System.DateTime(2011, 9, 5, 14, 48, 0, System.DateTimeKind.Utc)
        let offset = System.TimeSpan.FromHours -5.
        let o = System.DateTimeOffset(d.Add(offset).Ticks, offset)
        async.Return o

    [<Remote>]
    let f11 (x: int) =
        (x, x + 1)
        |> async.Return

    [<Remote>]
    let f12 (x: System.TimeSpan) min =
        x.Add(System.TimeSpan.FromMinutes min)
        |> async.Return

    [<Remote>]
    let add2_2ToMap m =
        m |> Map.add 2 2
        |> async.Return

    [<Remote>]
    let add2_a_2ToMap m =
        m |> Map.add { a = 2; b = "a" } 2
        |> async.Return

    [<Remote>]
    let getEmptyMap() =
        async.Return Map.empty<Record, int>

    [<Remote>]
    let add2ToSet s =
        s |> Set.add 2
        |> async.Return

    [<Remote>]
    let add2_aToSet s =
        s |> Set.add { a = 2; b = "a" }
        |> async.Return

    [<Remote>]
    let getEmptySet() =
        async.Return Set.empty<Record>

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
    let f15 (x: string) = async.Return x

    [<Remote>]
    let f16 (r: OptionsRecord) =
        async.Return { x = r.y; y = r.x }

    [<Remote>]
    let f19 (u: UnionWithConstants) (v: UnionWithConstants) =
        async { return (v, u) }

    [<Remote>]
    let LoginAs (username: string) =
        let ctx = Web.Remoting.GetContext()
        async {
            do! ctx.UserSession.LoginUser(username)
            return! ctx.UserSession.GetLoggedInUser()
        }

    [<Remote>]
    let GetLoggedInUser () =
        async {
            do! Async.SwitchToNewThread() // test AsyncLocal behavior
            let ctx = Web.Remoting.GetContext()
            return! ctx.UserSession.GetLoggedInUser()
        }

    [<Remote>]
    let Logout () =
        let ctx = Web.Remoting.GetContext()
        async {
            do! ctx.UserSession.Logout()
            return! ctx.UserSession.GetLoggedInUser()
        }

    [<JavaScript>]
    [<System.Serializable>]
    type BaseClass() =
        let mutable x = 0
        member this.Zero = x

    [<JavaScript>]
    [<System.Serializable>]
    type DescendantClass() =
        inherit BaseClass()
        let mutable x = 1
        member this.One = x

    [<JavaScript>]
    [<System.Serializable>]
    type AutoProperty() =
        member val X = 0 with get, set

    [<Remote>]
    let f17 (x: DescendantClass) =
        if x.Zero = 0 && x.One = 1
        then Some (DescendantClass())
        else None
        |> async.Return

    [<Remote>]
    let f18 (Record x) =
        async {
            return Record { a = x.a + 1; b = x.b + "_" }
        }

    [<Remote>]
    let f20 (x: AutoProperty) =
        async {
            x.X <- x.X + 1
            return x
        }

    [<JavaScript; Struct; System.Serializable>]
    type Struct =
        val public X : int
        [<Name "yyStructTest">]
        val public YStructTest : string
        new (x, y) = { X = x; YStructTest = y }

    [<Remote>]
    let f21 (x: Struct) =
        async {
            return Struct(x.X + 1, x.YStructTest + "a")
        }

    [<Remote>]
    let f22 (a: ResizeArray<string>) =
        a.Add("test")
        async { return a }

    [<Remote>]
    let f23 (a: System.Collections.Generic.Queue<string>) =
        a.Enqueue("test")
        let d = a.Dequeue()
        async { return (d, a) }

    [<Remote>]
    let f24 (a: System.Collections.Generic.Stack<string>) =
        let d = a.Pop()
        a.Push("test")
        async { return (d, a) }

    [<Remote>]
    let f25 () =
        async { return { Types = [| [| "Serializing record with field $TYPES" |] |] } }

    [<Remote>]
    let f27 (xy: struct (int * int)) =
        let (struct (x, y)) = xy
        struct (x + 1, y + 1)
        |> async.Return

    [<Remote>]
    let f28 (a: {| x: int; y: int |}) =
        a.x + a.y
        |> async.Return

    [<Remote>]
    let f29 (a: {| x: int; y: {| a: int; b: int |} |}) =
        a.x + a.y.a + a.y.b
        |> async.Return

    [<Remote>]
    let f30 (x: int, y: int) =
        {| x = x; y = y |}
        |> async.Return

    [<Remote>]
    let f31 (x: int, a: int, b: int) =
        {| x = x; y = {| a = a; b = b |} |}
        |> async.Return

    [<Remote>]
    let OptionToNullable (x: int option) =
        match x with
        | Some v -> System.Nullable v
        | _ -> System.Nullable() 
        |> async.Return
              
    [<Remote>]
    let NullableToOption (x: System.Nullable<int>) =
        if x.HasValue then Some x.Value else None         
        |> async.Return

    [<Remote>]
    let reverse (x: string) =
        new System.String(Array.rev (x.ToCharArray()))
        |> async.Return

    [<JavaScript>]
    type MyRemotingProvider() =
        inherit Remoting.AjaxRemotingProvider()

        override this.AsyncBase(m, data) =
            Console.Log("Calling RPC with MyRemotingProvider:", m, data)
            base.AsyncBase(m, data)

    [<RemotingProvider(typeof<MyRemotingProvider>)>]
    [<AbstractClass>]
    type Handler() =

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

    type IntfHandler =

        [<Remote>]
        abstract member IM3 : int -> Async<int>

    type HandlerImpl() =
        inherit Handler()

        override this.M1() =
            incr counterM1

        override this.M2() =
            incr counterM2
            async.Return ()

        override this.M3 x =
            async.Return (x + 1)

        override this.M4 (a, b) =
            async.Return (a + b)

        override this.M5 a b =
            async.Return (a + b)

    type RecordRemote =
        {
            [<Remote>]
            R1 : string -> Async<string>
            [<Remote>]
            R2 : unit -> Async<int>
            [<Remote>]
            R3 : string * int -> Async<string>
        }

    let recordRemoteImpl =
        {
            R1 = fun x -> 
                async {
                    return (x + "_fromRecordRemote")
                }
            R2 = fun _ ->
                async {
                    return 42
                }
            R3 = fun (x, y) ->
                async {
                    return (x + "+" + string y)
                }
        }

    do AddRpcHandler typeof<Handler> (HandlerImpl())

    let intfHandlerImpl =
        { new IntfHandler with
            member this.IM3 x =
                async.Return (x + 1)
        }

    do AddRpcHandler typeof<IntfHandler> intfHandlerImpl

    do AddRpcHandler typeof<RecordRemote> recordRemoteImpl

    [<Remote>]
    let count1 () = async.Return counter1.Value

    [<Remote>]
    let count2 () = async.Return counter2.Value

    [<Remote>]
    let countM1 () = async.Return counterM1.Value

    [<Remote>]
    let countM2 () = async.Return counterM2.Value

module Remoting =

    open WebSharper.Testing
    open System.Collections.Generic

    [<JavaScript>]
    let Tests =
        TestCategory "Remoting" {

            Test "unit -> unit" {
                do! Server.reset1()
                do Server.f1()
                do! Async.Sleep(200)
                let! x = Server.count1()
                equal x 124
            }

            Test "unit -> Async<unit>" {
                do! Server.reset2()
                do! Server.f2()
                let! x = Server.count2()
                equal x 177
            }

            Test "int -> int" {
                equal (Server.f3 15) 16
            }

            Test "int -> Async<int>" {
                let! x = Server.f4 8
                equal x 9
            }

            Test "option<int> -> Async<int>" {
                let! x = Server.f5 None
                equal x 0
                let! x = Server.f5 (Some -40)
                equal x -39
            }

            Test "string -> string -> Async<string>" {
                let! x = Server.f6 "a" "b"
                equal x "ab"
            }

            Test "string * string -> Async<string>" {
                let! x = Server.f7 ("a", "b")
                equal x "ab"
            }

            Test "float * float -> Async<float>" {
                let! x = Server.f8 (2.3, 4.5)
                equal x (2.3 + 4.5)
            }

            Test "list<int> -> Async<list<int>>" {
                let! x = Server.f9 [1;2;3]
                equal x [ 3; 2; 1 ]
            }

            Test "list<record> -> Async<list<record>>" {
                let! x =
                    Server.f9_1 [
                        { a = 1; b = "4" }
                        { a = 2; b = "5" }
                        { a = 3; b = "6" }
                    ]
                equal x [
                    { a = 3; b = "6" }
                    { a = 2; b = "5" }
                    { a = 1; b = "4" }
                ]
            }

            Test "DateTime -> Async<DateTime>" {
                let dt = System.DateTime.UtcNow
                let! x = Server.f10 dt
                equal x (dt.AddDays 1.0)
            }

            Test "DateTimeOffset -> Async<int * int>" {
                let d = Date(Date.UTC(2011, 9, 5, 14, 48, 0)).Self
                let o = System.DateTimeOffset(d, System.TimeSpan.FromHours -5.)
                let! utchour, hour, offset = Server.sendDTO o
                equal utchour 14
                equal hour 9 
                equal offset -5
            }

            Test "unit -> Async<DateTimeOffset>" {
                let! x = Server.getDTO()
                equal (x.DateTime.JS.GetUTCHours()) 14
                equal x.Offset.Hours -5
            }

            Test "DateTimeOffset -> Async<DateTimeOffset>" {
                let dt =  System.DateTimeOffset.UtcNow
                let! x = Server.f26 dt 0.0
                equal x dt
                let dtl = System.DateTimeOffset.Now
                let! x = Server.f26 dtl 0.0
                equal x dtl
                let! x = Server.f26 dt 1.0
                equal x (dt.AddDays 1.0)
            }

            Test "int -> Async<int * int>" {
                let! x = Server.f11 40
                equal x (40, 41)
            }

            Test "TimeSpan -> float -> Async<TimeSpan>" {
                let ts = System.TimeSpan.FromSeconds 14123.
                let! x = Server.f12 ts 1.25
                equal x (ts.Add (System.TimeSpan.FromMinutes 1.25))
            }

            Test "int -> T1 -> Async<T1>" {
                equalAsync (Server.f13 40 (Server.B (8, Server.A 9)))
                    (Server.B (40, Server.B (8, Server.A 9)))
                let! x = Server.f13 8 (Server.A 9)
                equal x.Head 8
            }

            Test "T2 -> Async<T2>" {
                equalAsync (Server.f14 { X = "X" }) { X = "X!" }
                let! x = Server.f14 {X = "X"}
                equal x.Body "X!"
            }

            Test "Null string" {
                equalAsync (Server.f15 null) null
            }

            Test "{None; Some} -> {Some; None}" {
                equalAsync (Server.f16 { x = None; y = Some 12 }) { x = Some 12; y = None }
            }

            Test "{Some; None} -> {None; Some}" {
                equalAsync (Server.f16 { x = Some 12; y = None }) { x = None; y = Some 12 }
            }

            Test "Union with constants" {
                equalAsync (Server.f19 Server.UString Server.UInt) (Server.UInt, Server.UString)
                equalAsync (Server.f19 Server.UBool Server.UNotConst) (Server.UNotConst, Server.UBool)
            }

            Skip "Automatic field rename" {
                let! x = Server.f17 (Server.DescendantClass())
                isTrue (x |> Option.exists (fun x -> x.Zero = 0 && x.One = 1))
            }

            Test "Auto property" {
                let x = Server.AutoProperty()
                x.X <- 5;
                let! y = Server.f20 x
                equal y.X 6
            }

            Test "ResizeArray" {
                let! x = ResizeArray [ "Hello"; "world" ] |> Server.f22
                equal (x.ToArray()) [| "Hello"; "world"; "test" |]
            }

            Test "Queue" {
                let! x, q = Queue [ "Hello"; "world" ] |> Server.f23
                equal x "Hello"
                equal (q.ToArray()) [| "world"; "test" |]
            }

            Test "Stack" {
                let s = Stack()
                s.Push("Hello")
                s.Push("world")
                let! x, s2 = Server.f24 s
                equal x "world"
                equal (s2.Pop()) "test"
                equal (s2.Pop()) "Hello"
            }

            Skip "Record with field named $TYPES" {
                let! x = Server.f25 ()
                equal x.Types [| [| "Serializing record with field $TYPES" |] |]
            }

            Test "Struct" {
                let x = Server.Struct(1, "h")
                let! y = Server.f21 x
                equal y (Server.Struct(2, "ha"))
            }

            Test "Single record in union" {
                let! (Server.Record r) = Server.f18 (Server.Record {a = 3; b = "xyz"})
                equal r.a 4
                equal r.b "xyz_"
            }

            Test "Empty Map" {
                equalAsync (Server.getEmptyMap()) Map.empty
            }

            Test "Map<int,int> -> Map<int,int>" {
                equalAsync (Server.add2_2ToMap Map.empty) (Map.ofArray [| 2, 2 |])
                equalAsync (Server.add2_2ToMap (Map.ofArray [| 1, 1 |])) (Map.ofArray [| 1, 1; 2, 2 |])
            }

            Test "Map with non-trivial key type" {
                equalAsync (Server.add2_a_2ToMap Map.empty) (Map.ofArray [| { a = 2; b = "a" }, 2 |])
                equalAsync (Server.add2_a_2ToMap (Map.ofArray [| { a = 2; b = "b" }, 1 |])) (Map.ofArray [| { a = 2; b = "b" }, 1; { a = 2; b = "a" }, 2 |])
            }

            Test "Empty Set" {
                equalAsync (Server.getEmptySet()) Set.empty
            }

            Test "Set<int> -> Set<int>" {
                equalAsync (Server.add2ToSet Set.empty) (Set.ofArray [| 2 |])
                equalAsync (Server.add2ToSet (Set.ofArray [| 0; 1; 3; 4 |])) (Set.ofArray [| 0 .. 4 |])
            }

            Test "Set with non-trivial key type" {
                equalAsync (Server.add2_aToSet Set.empty) (Set.ofArray [| { a = 2; b = "a" } |])
                equalAsync (Server.add2_aToSet (Set.ofArray [| { a = 1; b = "a" } |])) (Set.ofArray [| { a = 1; b = "a" } ; { a = 2; b = "a" } |])
            }

            Test "struct (int * int) -> struct (int * int)" {
                equalAsync (Server.f27 (struct (27, 37))) (struct (28, 38))
            }

            Test "LoginUser()" {
                equalAsync (Server.LoginAs("some_test_user")) (Some "some_test_user")
                equalAsync (Server.GetLoggedInUser()) (Some "some_test_user")
            }

            Test "Logout()" {
                equalAsync (Server.Logout()) None
                equalAsync (Server.GetLoggedInUser()) None
            }

//             TODO : Remote interface methods
            Test "M1" {
                do! Server.resetM1()
                do Remote<Server.Handler>.M1()
                do! Async.Sleep 200
                equalAsync (Server.countM1()) 244
            }

            Test "M2" {
                do! Server.resetM2()
                do! Remote<Server.Handler>.M2()
                equalAsync (Server.countM2()) 368
            }

            Test "M3" {
                equalAsync (Remote<Server.Handler>.M3 40) 41
            }

            Test "M4" {
                equalAsync (Remote<Server.Handler>.M4 (1, 2)) 3
            }

            Test "M5" {
                equalAsync (Remote<Server.Handler>.M5 3 6) 9
            }

            Test "IM3" {
                equalAsync (Remote<Server.IntfHandler>.IM3 40) 41
            }

            Test "reverse" {
                equalAsync (Server.reverse "abc#quit;;") ";;tiuq#cba"
                equalAsync (Server.reverse "c#") "#c"
                equalAsync (Server.reverse "\u00EF\u00BB\u00BF") "\u00BF\u00BB\u00EF"
                equalAsync (Server.reverse "c\127\127\127#") "#\127\127\127c"
            }

            Test "Nullable" {
                equalAsync (Server.NullableToOption (System.Nullable())) None
                equalAsync (Server.NullableToOption (System.Nullable 3)) (Some 3)
                jsEqualAsync (Server.OptionToNullable None) (System.Nullable())
                equalAsync (Server.OptionToNullable (Some 3)) (System.Nullable 3)
            }

            Test "Anonymous F# records" {
                equalAsync (Server.f28 {| x = 1; y = 2 |}) 3
                equalAsync (Server.f29 {| x = 1; y = {| a = 2; b = 3 |} |}) 6
                equalAsync (Server.f30 (1, 2)) {| x = 1; y = 2 |}
                equalAsync (Server.f31 (1, 2, 3)) {| x = 1; y = {| a = 2; b = 3 |} |}
            }

            Test "Anonymous F# records from another project" {
                equalAsync (WebSharper.Sitelets.Tests.AnonRecordServer.f28 {| x = 1; y = 2 |}) 3
                equalAsync (WebSharper.Sitelets.Tests.AnonRecordServer.f29 {| x = 1; y = {| a = 2; b = 3 |} |}) 6
                equalAsync (WebSharper.Sitelets.Tests.AnonRecordServer.f30 (1, 2)) {| x = 1; y = 2 |}
                equalAsync (WebSharper.Sitelets.Tests.AnonRecordServer.f31 (1, 2, 3)) {| x = 1; y = {| a = 2; b = 3 |} |}
            }

            Test "Record remote instantiation" {
                equalAsync (Remote<Server.RecordRemote>.R1 "myTestString") "myTestString_fromRecordRemote"
                equalAsync (Remote<Server.RecordRemote>.R2 ()) 42
                equalAsync (Remote<Server.RecordRemote>.R3 ("2", 1)) "2+1"
            }
        }
