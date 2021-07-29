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

[<JavaScript>]
module ApiTests =
    open Api
    
    let Tests apiBaseUri =
        let getPerson id =
            Async.FromContinuations <| fun (ok, ko, _) ->
                let xhr = XMLHttpRequest()
                xhr.Open("GET", (apiBaseUri + "person?id=" + string id))
                xhr.SetRequestHeader("Content-Type", "application/json")
                xhr.ResponseType <- XMLHttpRequestResponseType.Json
                xhr.Onload <- fun _ ->
                    try
                        match Json.Decode<Result<PersonDataNoDates>> (As<string> xhr.Response) with
                        | Result.Success p -> ok p
                        | Result.Failure e -> ko (exn e)
                    with e -> ko e
                xhr.Onerror <- fun _ -> ko <| exn xhr.StatusText
                xhr.Send()

        let updatePerson id person =
            Async.FromContinuations <| fun (ok, ko, _) ->
                let xhr = XMLHttpRequest()
                xhr.Open("POST", (apiBaseUri + "update-person?id=" + string id))
                xhr.SetRequestHeader("Content-Type", "application/json")
                xhr.ResponseType <- XMLHttpRequestResponseType.Json
                let data = Json.Serialize<PersonDataNoDates> person
                xhr.Onload <- fun _ -> ok ()
                xhr.Onerror <- fun _ -> ko <| exn xhr.StatusText
                xhr.Send(data)

        let testDateTime date =
            Async.FromContinuations <| fun (ok, ko, _) ->
                let xhr = XMLHttpRequest()
                xhr.Open("GET", (apiBaseUri + "test-datetime-format/" + date))
                xhr.Onload <- fun _ -> ok (As<string> xhr.Response)
                xhr.Onerror <- fun _ -> ko <| exn xhr.StatusText
                xhr.Send()
                |> ignore

        TestCategory "Sitelets JSON API" {
            Test "Equality" {
                isTrue (1 = 1)
                isTrueMsg (1 = 1) "One equals one"
                isTrueAsync (async { return 1 = 1 }) 
                isTrueMsgAsync (async { return 1 = 1 }) "One equals one async version"
            }
            Test "get/update" {
                let! person1 = getPerson 1
                isTrue (person1.firstName = "Alonzo" || person1.firstName = "Updated")

                do! updatePerson 1 { person1 with firstName = "Updated" }
                let! person1again = getPerson 1
                equal person1again.firstName "Updated"
            }
            Test "DateTimeFormat" {
                let! res = testDateTime "2018-03-14-08-15-35"
                isFalse (System.String.IsNullOrEmpty res)
            }
        }

    let RunTests apiBaseUri =
        Runner.RunTests [|
            Tests apiBaseUri
        |]

