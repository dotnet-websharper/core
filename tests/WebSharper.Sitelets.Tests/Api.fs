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
open System.Collections.Generic
open WebSharper
open WebSharper.Sitelets

/// This module implements the tutorial REST API from http://websharper.com/tutorials/rest-api
/// It's a full CRUD application maintaining a basic in-memory database of people.
module Api =

    /// The type of actions, ie. REST API entry points.
    type Action =
        /// GET /person?id=123
        | [<EndPoint "GET /person"; Query "id">]
            GetPerson of id: int
        /// POST /person (with JSON body)
        | [<EndPoint "POST /person"; Json "personData">]
            PostPerson of personData: PersonData
        /// PUT /person?id=123 (with JSON body)
        | [<EndPoint "PUT /person"; Query "id"; Json "personData">]
            PutPerson of id: int * personData: PersonData
        /// DELETE /person?id=123
        | [<EndPoint "DELETE /person"; Query "id">]
            DeletePerson of id: int
        // alternative update for testing untyped JSON body, and without DateTimeFormat
        /// POST /person (with untyped JSON body)
        | [<EndPoint "POST /update-person"; Query "id">]
            UpdatePerson of id: option<int>
        | [<EndPoint "/test-datetime-format"; DateTimeFormat("date", "yyyy-MM-dd-HH-mm-ss")>]
            TestDateTimeFormat of date: DateTime

    /// Data about a person. Used both for storage and JSON parsing/writing.
    and PersonData =
        { firstName: string
          lastName: string
          /// DateTimeFormat indicates how JSON parses and writes this date.
          [<DateTimeFormat "yyyy-MM-dd">] born: System.DateTime
          /// Since this is an option, this field is only present in JSON for Some value.
          [<DateTimeFormat "yyyy-MM-dd">] died: option<System.DateTime> }

    and PersonDataNoDates =
        { firstName: string
          lastName: string
        }

    /// Type used for all JSON responses to indicate success or failure.
    [<NamedUnionCases "result">]
    type Result<'T> =
        /// JSON: {"result": "success", /* fields of 'T... */}
        | [<Name "success">] Success of 'T
        /// JSON: {"result": "failure", "message": "error message..."}
        | [<Name "failure">] Failure of message: string

    /// Result value for PostPerson.
    type Id = { id : int }

    module private ApplicationLogic =

        /// The people database.
        let people = new Dictionary<int, PersonData>()
        /// The highest id used so far, incremented each time a person is POSTed.
        let lastId = ref 0

        let getPerson (id: int) : Result<PersonData> =
            lock people <| fun () ->
                match people.TryGetValue(id) with
                | true, person -> Success person
                | false, _ -> Failure "Person not found."

        let postPerson (data: PersonData) : Result<Id> =
            lock people <| fun () ->
                incr lastId
                people.[!lastId] <- data
                Success { id = !lastId }

        let putPerson (id: int) (data: PersonData) : Result<option<unit>> =
            lock people <| fun () ->
                match people.TryGetValue(id) with
                | true, _ ->
                    people.[id] <- data
                    Success None
                | false, _ -> Failure "Person not found."

        let deletePerson (id: int) : Result<option<unit>> =
            lock people <| fun () ->
                match people.TryGetValue(id) with
                | true, _ ->
                    people.Remove(id) |> ignore
                    Success None
                | false, _ -> Failure "Person not found."

    let ApiContent (ctx: Context<Action>) (action: Action) : Async<Content<Action>> =
        match action with
        | GetPerson id ->
            Content.Json (ApplicationLogic.getPerson id)
        | PostPerson personData ->
            Content.Json (ApplicationLogic.postPerson personData)
        | PutPerson (id, personData) ->
            Content.Json (ApplicationLogic.putPerson id personData)
        | DeletePerson id ->
            Content.Json (ApplicationLogic.deletePerson id)
        | UpdatePerson id ->
            let data = (new System.IO.StreamReader(ctx.Request.Body)).ReadToEnd()
            let p = Json.Deserialize<PersonDataNoDates> data
            let personData =
                { 
                    firstName = p.firstName
                    lastName = p.lastName
                    born = System.DateTime()
                    died = None
                }
            Content.Json (ApplicationLogic.putPerson (defaultArg id 1) personData)
        | TestDateTimeFormat date ->
            Content.Text (string date)

    let Sitelet = Sitelet.Infer ApiContent

    // Pre-fill the database with a few people.
    do Seq.iter (ApplicationLogic.postPerson >> ignore) [
        { firstName = "Alonzo"
          lastName = "Church"
          born = DateTime(1903, 6, 14)
          died = Some(DateTime(1995, 8, 11)) }
        { firstName = "Alan"
          lastName = "Turing"
          born = DateTime(1912, 6, 23)
          died = Some(DateTime(1954, 6, 7)) }
        { firstName = "Bertrand"
          lastName = "Russell"
          born = DateTime(1872, 5, 18)
          died = Some(DateTime(1970, 2, 2)) }
        { firstName = "Noam"
          lastName = "Chomsky"
          born = DateTime(1928, 12, 7)
          died = None }
    ]
