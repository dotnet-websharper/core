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

namespace IntelliFactory.WebSharper.Sitelets.Tests

open System
open System.Collections.Generic
open IntelliFactory.WebSharper
open IntelliFactory.WebSharper.Sitelets

module Api =

    type Action =
        | [<CompiledName "person"; Method "GET">]
            GetPerson of string
        | [<CompiledName "person"; Method "POST"; Json "userdata">]
            PostPerson of string * userdata: UserData

    and [<NamedUnionCases "result">]
        Result<'T> =
        | [<CompiledName "success">] Success of data: 'T
        | [<CompiledName "error">] Failure of message: string

    and UserData =
        {
            [<Name "firstName">]    FirstName   : string
            [<Name "lastName">]     LastName    : string
            [<Name "born">]         Born        : DateTime
            [<Name "died">]         Died        : DateTime option
        }

    let usersDatabase =
        Dictionary(
            Map [
                "alonzo.church", {
                    FirstName = "Alonzo"
                    LastName = "Church"
                    Born = DateTime(1903, 6, 14)
                    Died = Some(DateTime(1995, 8, 11))
                }
                "alan.turing", {
                    FirstName = "Alan"
                    LastName = "Turing"
                    Born = DateTime(1912, 6, 23)
                    Died = Some(DateTime(1954, 6, 7))
                }
                "bertrand.russell", {
                    FirstName = "Bertrand"
                    LastName = "Russell"
                    Born = DateTime(1872, 5, 18)
                    Died = Some(DateTime(1970, 2, 2))
                }
                "noam.chomsky", {
                    FirstName = "Noam"
                    LastName = "Chomsky"
                    Born = DateTime(1928, 12, 7)
                    Died = None
                }
            ])

    let Sitelet =
        Sitelet.Infer <| function
            | GetPerson ident ->
                Content.JsonContent <| fun ctx ->
                    lock usersDatabase <| fun () ->
                        match usersDatabase.TryGetValue ident with
                        | true, u -> Success u
                        | false, _ -> Failure "User not found"
            | PostPerson (ident, data) ->
                Content.JsonContent <| fun ctx ->
                    lock usersDatabase <| fun () ->
                        match usersDatabase.TryGetValue ident with
                        | true, _ -> Failure "User already registered"
                        | false, _ ->
                            usersDatabase.[ident] <- data
                            Success ()
