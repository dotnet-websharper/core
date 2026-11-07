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

    type FetchEcho =
        {
            ContentType : string
            Accept : string
            Body : RecTest
        }

    type Action =
        | [<EndPoint "/">] URoot
        | [<EndPoint "/">] USubAction of SubAction
        | [<EndPoint ("/string", "/stringtoo")>] UString of string
        | [<EndPoint "/query"; Query "s">] UQuery of s: string
        | [<EndPoint "/query?{a}&{b}">] UQuery2 of a: int * b: string
        | [<EndPoint "/query2?what={w}&qwe={q}">] UQuery3 of w: int * q: string
        | [<EndPoint "/tuple">] UTuple of p: int * string * bool
        | [<EndPoint "/tuple-with-queries"; Query("a", "b")>] UTupleQ of p: int * a: string * b: bool
        | [<EndPoint "/nullable">] UNullable of System.Nullable<int>
        | [<EndPoint "/option-query"; Query "q">] UOptionQuery of q : option<int>
        | [<EndPoint "/nullable-query"; Query "q">] UNullableQuery of q : System.Nullable<int>
        | [<EndPoint "/union-query"; Query "q">] UUnionQuery of q : SubAction
        | [<EndPoint "/union-option-query"; Query "q">] UUnionOptionQuery of q : option<SubAction>
        | [<EndPoint "/record-query"; Query "q">] URecordQuery of q : RecTest
        | [<EndPoint "/recursive">] URecursive of option<Action>
        | [<EndPoint "/record">] URecord of RecTest
        | [<EndPoint "/record-with-queries">] URecordQ of RecQTest
        | [<EndPoint "/list">] UList of list<int * string>
        | [<EndPoint "/array">] UArray of (int * string)[]
        | [<EndPoint "/priority">] UPriority1
        | [<EndPoint "/priority">] UPriority2 of int
        | [<EndPoint "/priority">] UPriority3 of string
        | [<EndPoint "/priority">] UPriority4 of int * string
        | [<EndPoint "/priority">] UPriority5 of string * string
        | [<Method "POST"; EndPoint "/post">] UPost of int
        | [<Method "PUT"; EndPoint "/put">] UPut of int
        | [<EndPoint "POST /post2">] UPost2 of int
        | [<Method "QUERY"; EndPoint "/query-method">] UQueryMethod of int
        | [<EndPoint "QUERY /query-json"; Json "body">] UQueryJson of body: RecTest
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
        | [<EndPoint "POST /fetch-echo"; Json "body">] UFetchEcho of body: RecTest
        | [<EndPoint "GET /fetch-status">] UFetchStatus of int

    let TestValues =
        [
            URoot
            USubAction Sub1
            USubAction (Sub2 "x")
            UString ""
            UString "hello"
            UString """{} ## @!~~ +++ fe öüóőúéáű /\ `$%^&*  ->%20<- .,;"""
            UQuery "hello"
            UQuery """{} ## @!~~ +++ fe öüóőúéáű /\ `$%^&*  ->%20<- .,;"""
            UQuery2 (1, "hi")
            UQuery3 (1, "qwe-hi")
            UTuple (1, "hi", true)
            UTupleQ (1, "hi", true)
            UNullable (System.Nullable())
            UNullable (System.Nullable 2)
            UOptionQuery None
            UOptionQuery (Some 5)
            UNullableQuery (System.Nullable())
            UNullableQuery (System.Nullable 6)
            UUnionQuery Sub1
            UUnionQuery (Sub2 "x")
            UUnionQuery (Sub2 "a/b")
            UUnionOptionQuery None
            UUnionOptionQuery (Some (Sub2 "x"))
            URecordQuery { A = "hello"; B = 123; C = false }
            URecursive (Some (URecursive (Some (UTuple (1, "hi", true)))))
            URecord { A = "hello"; B = 123; C = false }
            URecordQ { A = "hello"; BQ = 123; CQ = false }
            UList [ 1, "hi"; 2, "hi!"; 3, "hi!!" ]
            UArray [| 1, "hi"; 2, "hi!"; 3, "hi!!" |]
            UPost 1
            UPut 2
            UPost2 3
            UQueryMethod 4
            UQueryJson { A = "query"; B = 5; C = true }
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
            UString "", "/string"
            UString "xx", "/stringtoo/xx"
            UCSharp (new CSharpEndPointRoot()), "/csharp/home"
            UCSharp (new CSharpEndPointRoot.Sub1(X = 42)), "/csharp/sub1full/42"
            UPriority1, "/priority"
            UPriority2 123, "/priority/123"
            UPriority3 "xx", "/priority/xx"
            UPriority4 (123, "xx"), "/priority/123/xx"
            UPriority5 ("xx", "yy"), "/priority/xx/yy"
            UString "a/b", "/string/a%2Fb"
            UString "a/b", "/string/a%2fb"
            UPriority5 ("a/b", "c"), "/priority/a%2Fb/c"
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
                        Headers = ["Content-Type"]
                        CsrfProtection = true }
                ) (fun _ -> ok())
            | UFetchEcho body ->
                let header name =
                    ctx.Request.Headers
                    |> Seq.tryFind (fun h -> h.Name.ToLowerInvariant() = name)
                    |> Option.map (fun h -> h.Value)
                    |> Option.defaultValue ""
                Content.Json { ContentType = header "content-type"; Accept = header "accept"; Body = body }
            | UFetchStatus code ->
                Content.Text "fetch-status-body"
                |> Content.SetStatus (Http.Status.Custom code None)
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

module HttpMethods =
    open WebSharper.Sitelets.UrlHelpers

    let private requestWith (m: Http.Method) (uri: string) (query: list<string * string>) =
        let get = Http.ParametersFromMap (Map.ofList query)
        let post = Http.ParametersFromMap (Map.ofList [ ("f", "2") ])
        let empty = Http.Request.Empty "/"
        { new Http.Request() with
            override x.Method = m
            override x.Uri = Uri(uri, UriKind.RelativeOrAbsolute)
            override x.Headers = empty.Headers
            override x.Post = post
            override x.Get = get
            override x.ServerVariables = empty.ServerVariables
            override x.Body = empty.Body
            override x.Files = empty.Files
            override x.Cookies = empty.Cookies
            override x.BodyText = empty.BodyText
            override x.IsBodyTextCompleted = empty.IsBodyTextCompleted
        }

    let private request m uri = requestWith m uri []

    let private describe (req: Http.Request) =
        [
            match req with GET -> "GET" | _ -> ()
            match req with POST -> "POST" | _ -> ()
            match req with PUT -> "PUT" | _ -> ()
            match req with DELETE -> "DELETE" | _ -> ()
            match req with OPTIONS -> "OPTIONS" | _ -> ()
            match req with TRACE -> "TRACE" | _ -> ()
            match req with QUERY -> "QUERY" | _ -> ()
            match req with HEAD -> "HEAD" | _ -> ()
            match req with CONNECT -> "CONNECT" | _ -> ()
            match req with CUSTOM m -> "CUSTOM " + m | _ -> ()
        ]
        |> String.concat ", "

    /// Returns the method names and the UrlHelpers active patterns matching them.
    [<Remote>]
    let MatchMethods () =
        [| "GET"; "POST"; "PUT"; "DELETE"; "OPTIONS"; "TRACE"; "QUERY"; "HEAD"; "CONNECT"; "PATCH" |]
        |> Array.map (fun m ->
            let meth = Http.Method.OfString m
            string meth, describe (request meth "/x")
        )
        |> Array.append (
            [| "GET"; "QUERY"; "HEAD" |]
            |> Array.map (fun m -> "Custom " + m, describe (request (Http.Method.Custom m) "/x"))
        )
        |> async.Return

    type Endpoint =
        | Blog of int * int
        | BlogDefault of int
        | Search of string
        | Page of string list
        | Unknown

    let private route (req: Http.Request) =
        match req with
        | GET & PATH ["blog"; INT id] & PARAM "page" (INT page) -> Blog (id, page)
        | GET & PATH ["blog"; INT id] -> BlogDefault id
        | QUERY & PATH ["search"] & FORM "f" f -> Search f
        | GET & PATH p -> Page p
        | _ -> Unknown

    /// Routes requests with the UrlHelpers request patterns.
    [<Remote>]
    let RouteRequests () =
        [|
            route (request Http.Method.Get "/blog/12")
            route (requestWith Http.Method.Get "/blog/12?page=3" [ ("page", "3") ])
            route (requestWith Http.Method.Get "/blog/12?page=x" [ ("page", "x") ])
            route (request Http.Method.Get "/blog/+12")
            route (request Http.Method.Get "/blog/%2012")
            route (request Http.Method.Query "/search")
            route (request Http.Method.Post "/search")
            route (request Http.Method.Get "/a%2Fb//c%20d/")
            route (request Http.Method.Get "http://example.com/a%2Fb/c?x=1")
        |]
        |> async.Return

    /// Parses numbers with the UrlHelpers number patterns under a culture with a decimal comma.
    [<Remote>]
    let ParseNumbers () =
        let culture = System.Globalization.CultureInfo.CurrentCulture
        try
            System.Globalization.CultureInfo.CurrentCulture <- System.Globalization.CultureInfo "de-DE"
            let f s = match s with FLOAT x -> Some x | _ -> None
            let i s = match s with INT x -> Some x | _ -> None
            let u s = match s with UINT x -> Some x | _ -> None
            async.Return (
                [| f "1.5"; f "1,5"; f "-2e3"; f " 1" |],
                [| i "-12"; i "12"; i " 12"; i "1.000" |],
                [| u "12"; u "-12"; u "+12" |]
            )
        finally
            System.Globalization.CultureInfo.CurrentCulture <- culture

    /// Parses QUERY routes with QUERY and GET methods.
    [<Remote>]
    let ParseQueryMethod () =
        let router = Router.Infer<PerformanceTests.Action>()
        let parse m =
            { Route.FromUrl "/query-method/4" with Method = Some m }
            |> Router.Parse router
        async.Return (parse "QUERY", parse "GET")

