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

namespace WebSharper.Sitelets.Tests

open WebSharper
open WebSharper.Sitelets

module PerformanceTests =
    
    type RecTest =
        {
            A : string
            B : int
            C : bool
        }
    
    type RecQTest =
        {
            A : string
            [<Query>] BQ : int
            [<Query>] CQ : bool
        }

    type MultipleTest =
        | [<EndPoint "/a" >] A
        | [<EndPoint "/a" >] A1 of int
    
    type Action =
        | [<EndPoint "/">] URoot
        | [<EndPoint "/string">] UString of string
        | [<EndPoint "/tuple">] UTuple of p: int * string * bool
        | [<EndPoint "/tuple-with-queries"; Query("a", "b")>] UTupleQ of p: int * a: string * b: bool
        | [<EndPoint "/nullable">] UNullable of System.Nullable<int>
        | [<EndPoint "/option-query"; Query "q">] UOptionQuery of q : option<int>
        | [<EndPoint "/nullable-query"; Query "q">] UNullableQuery of q : System.Nullable<int>
        | [<EndPoint "/recursive">] URecursive of option<Action>
        | [<EndPoint "/record">] URecord of RecTest
        | [<EndPoint "/record-with-queries">] URecordQ of RecQTest
        | [<EndPoint "/list">] UList of list<int * string>
        | [<EndPoint "/array">] UArray of (int * string)[]
        | [<Method "POST"; EndPoint "/post">] UPost of int
        | [<Method "PUT"; EndPoint "/post">] UPut of int
        | [<EndPoint "POST /post2">] UPost2 of int
        | [<EndPoint "/json-input"; Json "json">] UJsonInput of json: RecTest 
        | [<EndPoint "/json-input"; Json "json">] UJsonInt of json: int 
        | [<EndPoint "/formdata"; FormData "data">] UFormData of data: string 
        | [<EndPoint "/multiple" >] UMultiple
        | [<EndPoint "/multiple" >] UMultiple2 of int
        | [<EndPoint "/multiple" >] UMultiple3 of int * int
        | [<EndPoint "/wildcard-string"; Wildcard>] UWildcardString of string
        | [<EndPoint "/wildcard-array"; Wildcard>] UWildcardArray of int[]
        | [<EndPoint "/wildcard-list"; Wildcard>] UWildcardList of int list
        | [<EndPoint "/two-unions">] UTwoUnions of MultipleTest * MultipleTest

    let TestValues =
        [
            URoot
            UString "hello"
            UTuple (1, "hi", true)
            UTupleQ (1, "hi", true)
            UNullable (System.Nullable())
            UNullable (System.Nullable 2)
            UOptionQuery None
            UOptionQuery (Some 5)
            UNullableQuery (System.Nullable())
            UNullableQuery (System.Nullable 6)
            URecursive (Some (URecursive (Some (UTuple (1, "hi", true)))))
            URecord { A = "hello"; B = 123; C = false }
            URecordQ { A = "hello"; BQ = 123; CQ = false }
            UList [ 1, "hi"; 2, "hi!"; 3, "hi!!" ]
            UArray [| 1, "hi"; 2, "hi!"; 3, "hi!!" |]
            UPost 1
            UPut 2
            UPost2 3
            UJsonInput { A = "hello"; B = 123; C = false }
            UJsonInt 4
            UFormData "hello"
            UMultiple
            UMultiple2 1
            UMultiple3 (1, 2)
            UWildcardString "1/2/3/hi"
            UWildcardArray [| 1; 2; 3 |]
            UWildcardList [ 1; 2; 3 ]
            UTwoUnions (A, A)
            UTwoUnions (A1 1, A)
            UTwoUnions (A1 1, A1 1)
        ]

    let Site =
        Sitelet.Infer<Action> (fun ctx act -> 
            for i in 1 .. 49 do
                ctx.Link act |> ignore // stress-test writing links 
            Content.Text (
                ctx.Link act
            ) 
        )

    let ShiftedRouter = 
        Router.Infer<Action>()
        |> Router.Shift "perf-tests"
        
    [<Remote>]
    let GetTestValues() =
        TestValues |> Seq.map (fun v ->
            v, ShiftedRouter.Link v
        ) |> Array.ofSeq |> async.Return
               
// test urls:
// http://localhost:50668/perf-tests/
// http://localhost:50668/perf-tests/string/hellothere
// http://localhost:50668/perf-tests/tuple/1/hi/True
// http://localhost:50668/perf-tests/tuple-with-queries/1?a=hi&b=True
// http://localhost:50668/perf-tests/recursive/Some/recursive/Some/tuple/1/hi/True
// http://localhost:50668/perf-tests/record/hello/123/False
// http://localhost:50668/perf-tests/record-with-queries/hello?BQ=123&CQ=False
// http://localhost:50668/perf-tests/list/4/232/grer/232/grer/232/grer/232/grer
// http://localhost:50668/perf-tests/array/4/232/grer/232/grer/232/grer/232/grer
// POST http://localhost:50668/perf-tests/post/1
// PUT http://localhost:50668/perf-tests/post/1
// POST http://localhost:50668/perf-tests/post2/1
// http://localhost:50668/perf-tests/json-input BODY: { "A": "x", "B": 12, "C": false }
// http://localhost:50668/perf-tests/json-input BODY: 123
// POST http://localhost:50668/perf-tests/formdata BODY: data=thisisdata HEADER: Content-Type: application/x-www-form-urlencoded
// http://localhost:50668/perf-tests/multiple 
// http://localhost:50668/perf-tests/multiple/1 
// http://localhost:50668/perf-tests/multiple/1/2 
// http://localhost:50668/perf-tests/wildcard-string/anything/here/is/ok
// http://localhost:50668/perf-tests/wildcard-array/0/1/2/3
// http://localhost:50668/perf-tests/wildcard-list/0/1/2/3
// http://localhost:50668/perf-tests/two-unions/a/1/a/2

// check that this fails:
// GET http://localhost:50668/perf-tests/post/1
// GET http://localhost:50668/perf-tests/post2/1

module CombinatorTests =

    type PersonData =
        {
            Name : string
            Age : int
        }
        
    type RouterTest =
        | Root
        | About of int option * p: PersonData option

    open RouterOperators

    [<JavaScript>]
    let constructed =
        let rPersonData =
            rString / rInt |> Router.Map (fun (n, a) -> { Name = n; Age = a }) (fun p -> p.Name, p.Age) 
        Router.Sum [
            rRoot |> Router.MapTo Root
            "about" / Router.QueryOption "q" rInt / Router.Option rPersonData |> Router.Embed About (function About (i, p) -> Some (i, p) | _ -> None)
        ]

    let TestValues =
        [
            Root
            About (None, None)
            About (Some 1, None)
            About (Some 1, Some { Name = "Bob"; Age = 32 })
        ]
 
    [<Remote>]
    let GetTestValues() =
        TestValues |> Seq.map (fun v ->
            v, constructed.Link v
        ) |> Array.ofSeq |> async.Return
