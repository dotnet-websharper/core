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

open System
open WebSharper
open WebSharper.Sitelets
open WebSharper.CSharp.Sitelets.Tests

module PerformanceTests =
    open WebSharper.Remoting
    
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

    type MultipleFormData =
        {
            [<FormData; Name "text">] Text: string
            [<FormData; OptionalField; Name "id">] Id: int option
            [<FormData; Name "flag">] Flag: bool
        }

    type SubAction = 
        | [<EndPoint "/sub1">] Sub1
        | [<EndPoint "/sub2">] Sub2 of string

    type CorsA =
        | [<EndPoint "POST /a">] CorsA_A

    type CorsApi =
        | [<EndPoint "/a">] CorsA of CorsA
        | [<EndPoint "PUT /b">] CorsB of body: MultipleFormData
        | [<EndPoint "POST /c"; Json "body">] CorsC of body: RecTest
        | [<EndPoint "GET /d">] CorsD

    type Action =
        | [<EndPoint "/">] URoot
        | [<EndPoint "/">] USubAction of SubAction
        | [<EndPoint ("/string", "/stringtoo")>] UString of string
        | [<EndPoint "/query"; Query "s">] UQuery of s: string
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
        | [<Method "PUT"; EndPoint "/put">] UPut of int
        | [<EndPoint "POST /post2">] UPost2 of int
        | [<EndPoint "/json-input"; Json "json">] UJsonInput of json: RecTest 
        | [<EndPoint "/json-input"; Json "json">] UJsonInt of json: int 
        | [<EndPoint "/formdata"; FormData "data">] UFormData of data: string 
        | [<EndPoint "/multi-formdata">] UMultiFormData of data: MultipleFormData 
        | [<EndPoint "/multiple" >] UMultiple
        | [<EndPoint "/multiple" >] UMultiple2 of int
        | [<EndPoint "/multiple" >] UMultiple3 of int * int
        | [<EndPoint "/wildcard-string">] UWildcardStringEmpty
        | [<EndPoint "/wildcard-string/special-case">] UWildcardStringSpecialCase
        | [<EndPoint "/wildcard-string"; Wildcard>] UWildcardString of string
        | [<EndPoint "/wildcard-array"; Wildcard>] UWildcardArray of int[]
        | [<EndPoint "/wildcard-list"; Wildcard>] UWildcardList of int list
        | [<EndPoint "/two-unions">] UTwoUnions of MultipleTest * MultipleTest
        | [<EndPoint "/csharp">] UCSharp of CSharpEndPointRoot
        | [<EndPoint "/type-tests">] TypeTests of 
            Guid * single * double * sbyte * byte * int16 * uint16 * uint32 * int64 * uint64  
        | [<EndPoint "/cors">] UCors of Cors<CorsApi>

    let TestValues =
        [
            URoot
            USubAction Sub1
            USubAction (Sub2 "x")
            UString "hello"
            UString """{} ## @!~~ +++ fe öüóőúéáű /\ `$%^&*  ->%20<- .,;"""
            UQuery "hello"
            UQuery """{} ## @!~~ +++ fe öüóőúéáű /\ `$%^&*  ->%20<- .,;"""
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
            UFormData """{} ## @!~~ +++ fe öüóőúéáű /\ `$%^&*  ->%20<- .,;"""
            UMultiFormData { Text = "hello"; Id = None; Flag = true }
            UMultiFormData { Text = "hello"; Id = Some 2; Flag = false }
            UMultiple
            UMultiple2 1
            UMultiple3 (1, 2)
            UWildcardStringEmpty
            UWildcardStringSpecialCase
            UWildcardString "1/2/3/hi"
            UWildcardArray [| 1; 2; 3 |]
            UWildcardList [ 1; 2; 3 ]
            UTwoUnions (A, A)
            UTwoUnions (A1 1, A)
            UTwoUnions (A1 1, A1 1)
            UCSharp (new CSharpEndPointRoot())
            UCSharp (new CSharpEndPointRoot.Sub1(X = 42))
            TypeTests (Guid.NewGuid(), 1.3f, 1.4, 15y, 16uy, 64s, 65us, 66u, 67L, 68UL)
            UCors <| Cors.Of (CorsA CorsA_A)
            UCors <| Cors.Of (CorsB { Text = "helloCorsB"; Id = Some 2; Flag = false })
            UCors <| Cors.Of (CorsC { A = "helloCorsC"; B = 123; C = false })
            UCors <| Cors.Of CorsD
        ]

    let ExtraTestValues =
        [
            UString "xx", "/stringtoo/xx"
            UCSharp (new CSharpEndPointRoot()), "/csharp/home"
            UCSharp (new CSharpEndPointRoot.Sub1(X = 42)), "/csharp/sub1full/42"
        ]

    let mutable expecting = None

    [<Remote>]
    let Expect (ep: Action) =
        expecting <- Some ep
        async.Zero()    
    
    let Site =
        Sitelet.New (Router.Infer<Action>()) (fun ctx act ->
            let ok() =
                let act =
                    match act with
                    | UCors (Cors.Of x) -> UCors (Cors.Of x) // remove DefaultAllows
                    | act -> act
                let def() =
                    for i in 1 .. 49 do
                        ctx.Link act |> ignore // stress-test writing links 
                    Content.Text (
                        ctx.Link act
                    ) 
                match expecting with
                | Some exp ->
                    if exp <> act && (match exp with UCSharp _ -> false | _ -> true) then
                        Content.Text (
                            sprintf "Wrong endpoint parsed, expecting %A, got %A" exp act
                        ) 
                    else
                        expecting <- None
                        def()
                | _ -> def()
            match act with
            | UCors cors ->
                Content.Cors cors (fun allows ->
                    { allows with
                        Origins = ["*"]
                        Headers = ["Content-Type"] }
                ) (fun _ -> ok())
            | _ -> ok()
        )

    let ShiftedRouter = 
        Router.Infer<Action>()
        |> Router.Shift "perf-tests"
        
    [<Remote>]
    let GetTestValues() =
        TestValues |> Seq.map (fun v ->
            let l = ShiftedRouter.Link v 
            v, ShiftedRouter.Link v, Router.Parse ShiftedRouter (Route.FromUrl l)
        ) |> Array.ofSeq |> async.Return

    [<Remote>]
    let GetExtraTestValues() =
        ExtraTestValues |> Seq.map (fun (v, p) ->
            let l = "/perf-tests" + p 
            v, l, Router.Parse ShiftedRouter (Route.FromUrl l)
        ) |> Array.ofSeq |> async.Return
               
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

module Bug940 =
    type Fails =
        | [<EndPoint "/">] Home
        | [<EndPoint "/about">] About

    [<Remote>]
    let Test() =
        let r = Router.Infer<Fails>()
        let l = { Route.Segment "about" with Method = Some "GET" }
        match l |> Router.Parse r with
        | Some About -> None
        | Some _ -> Some "Failed to parse /about correctly"
        | None -> Some "Failed to parse /about"
        |> async.Return
