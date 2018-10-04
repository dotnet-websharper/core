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
module WebSharper.JavaScript.Cookies.Definition

open WebSharper.InterfaceGenerator
open WebSharper.JavaScript

let Res = Resource "Cookies" "Cookies.js"

let Options =
    Pattern.Config "Cookies.Options" {
        Required = []
        Optional =
            [
                "path", T<string>
                "domain", T<string>
                "expires", Ecma.Definition.EcmaDate.Type
                "secure", T<bool>
            ]
    }

let Cookies =
    Class "Cookies"
    |=> Nested [Options]
    |+> Static [
        "get" => T<string>?key ^-> !?T<string>
        "set" => T<string>?key * T<string>?value * !?Options ^-> T<unit>
        "expire" => T<string>?key * !?Options ^-> T<unit>
        "defaults" =? Options
        "enabled" =? T<bool>
    ]
    |> Requires [Res]

let Namespaces =
    [
        Namespace "WebSharper.JavaScript" [
            Cookies
        ]
        Namespace "WebSharper.JavaScript.Resources" [
            Res
        ]
    ]