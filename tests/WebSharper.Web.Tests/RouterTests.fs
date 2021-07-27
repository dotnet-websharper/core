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

open WebSharper
open WebSharper.JavaScript
open WebSharper.Testing
open WebSharper.Sitelets
open PerformanceTests
open RouterOperators

[<JavaScript>]
module ClientServerTests =

    let ShiftedRouter = 
        Router.Shift "perf-tests" <| Router.Infer<Action>()

    let Tests apiBaseUri corsBaseUri runServerTests =
        let parse router p =
            Route.FromUrl(p) |> Router.Parse router    
        let parseHash router p =
            Route.FromHash(p, true) |> Router.Parse router
        let rUnit = rRoot |> Router.MapTo ()

        let writeAction (v: Action) =
            try sprintf "%A" v
            with _ -> "Failed writing action value"

        let hasMethodOrBody (v: Action) =
            match v with
            | UPost _ 
            | UPut _
            | UPost2 _
            | UJsonInput _
            | UJsonInt _
            | UFormData _
            | UMultiFormData _
            | UCors _
                -> true
            | _ -> false

        TestCategory "Sitelets Client-server routing" {
            TestIf runServerTests "compatibility tests" {
                let! serverResults = GetTestValues()
                let! extraServerResults = GetExtraTestValues()
                let testValues = serverResults |> Array.map (fun (testValue, _, _) -> testValue)
                let serverLinks = serverResults |> Array.map (fun (_, serverLink, _) -> serverLink)
                let clientLinks = testValues |> Array.map ShiftedRouter.Link
                forEach (Array.zip3 testValues serverLinks clientLinks) (fun (v, s, c) ->
                    Do { equalMsg s c ("Generated link equal: " + writeAction v) }
                )
                let extraValues = extraServerResults |> Array.map (fun (testValue, _, _) -> testValue)
                let allServerLinks = Array.append serverLinks (extraServerResults |> Array.map (fun (_, serverLink, _) -> serverLink))
                let allTestValues = Array.append testValues extraValues
                let serverParsed = Array.append serverResults extraServerResults |> Array.map (fun (_, _, testValue) -> testValue)
                forEach (Array.zip3 allTestValues allServerLinks serverParsed) (fun (v, l, p) ->
                    if hasMethodOrBody v then Do.Zero() else
                    Do { equalMsg p (Some v) ("Parsing back on the server: " + l) }
                )
                let extraLinks = extraServerResults |> Array.map (fun (_, serverLink, _) -> serverLink)
                let clientParsed = Array.append clientLinks extraLinks |> Array.map (fun l -> Router.Parse ShiftedRouter (Route.FromUrl l))
                forEach (Array.zip3 allTestValues allServerLinks clientParsed) (fun (v, l, p) ->
                    if hasMethodOrBody v then Do.Zero() else
                    Do { equalMsg p (Some v) ("Parsing back on the client: " + l) }
                )
            }

            // TestIf runServerTests "Router.Ajax" {
            //     let settings ep =
            //         let s = JQuery.AjaxSettings()
            //         match ep with
            //         | UCors _ -> corsBaseUri |> Option.iter (fun uri -> s.Url <- uri)
            //         | _ -> ()
            //         s
            //     let! serverResults = GetTestValues()
            //     let! ajaxResults =
            //         async {
            //             let arr = ResizeArray()
            //             for testValue, _, _ in serverResults do
            //                 try
            //                     do! Expect testValue
            //                     let! res = Router.AjaxWith (settings testValue) ShiftedRouter testValue
            //                     arr.Add (res)
            //                 with e ->
            //                     arr.Add (e.StackTrace)
            //             return arr.ToArray()
            //         }
            //     let expectedResults = serverResults |> Array.map (fun (_, serverLink, _) -> serverLink)
            //     forEach (Array.zip ajaxResults expectedResults) (fun (r, v) -> Do {
            //         equalMsg r v (sprintf "Ajax call for: " + string v)
            //     })
            // }

            TestIf runServerTests "Router.Fetch" {
                let baseUri ep =
                    match ep with
                    | UCors _ -> corsBaseUri
                    | _ -> None
                let! serverResults = GetTestValues()
                let! ajaxResults =
                    async {
                        let arr = ResizeArray()
                        for testValue, _, _ in serverResults do
                            try
                                do! Expect testValue
                                let! res = Router.FetchWith (baseUri testValue) (RequestOptions()) ShiftedRouter testValue
                                            |> JavaScript.Promise.AsAsync
                                let! text = res.Text() |> JavaScript.Promise.AsAsync
                                arr.Add (text)
                            with e ->
                                arr.Add (e.StackTrace)
                        return arr.ToArray()
                    }
                let expectedResults = serverResults |> Array.map (fun (_, serverLink, _) -> serverLink)
                forEach (Array.zip ajaxResults expectedResults) (fun (r, v) -> Do {
                    equalMsg r v (sprintf "Fetch call for: " + string v)
                })
            }

            Test "Router primitives" {
                equal (Router.Link rUnit ()) "/"
                equal (parse rUnit "/") (Some ())
                equal (Router.Link rInt 2) "/2"
                equal (parse rInt "/2") (Some 2)
            }

            Test "Router.HashLink" {
                equal (Router.HashLink rUnit ()) "#/"
                equal (parseHash rUnit "") (Some ())
                equal (parseHash rUnit "#") (Some ())
                equal (parseHash rUnit "#/") (Some ())
                equal (Router.HashLink rInt 2) "#/2"
                equal (parseHash rInt "#/2") (Some 2)     
            }

            TestIf runServerTests "Router combinator" {
                let! testValuesAndServerLinks = CombinatorTests.GetTestValues()
                let testValuesAndClientLinks =
                    testValuesAndServerLinks |> Array.map (fun (testValue, _) ->
                        testValue, CombinatorTests.constructed.Link testValue    
                    )
                equal testValuesAndServerLinks testValuesAndClientLinks
            }

            TestIf runServerTests "#940 'GET /' union case" {
                let! errOpt = Bug940.Test()
                equal errOpt None
            }
        }

    let RunTests apiBaseUri corsBaseUri runServerTests =
        Runner.RunTests [|
            Tests apiBaseUri corsBaseUri runServerTests
        |]

