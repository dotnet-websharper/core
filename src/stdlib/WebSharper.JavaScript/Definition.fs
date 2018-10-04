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

module WebSharper.JavaScript.Definition

open WebSharper.InterfaceGenerator
open WebSharper.JavaScript

/// The JavaScript global properties and functions can be used with all the built-in JavaScript objects.
let Global =
    Namespace "WebSharper.JavaScript" [
        Class "console"
        |+> Static [
            "count" => !? T<string> ^-> T<unit>
            "dir" => T<obj> ^-> T<unit>
            Generic - fun t -> "error" => t ^-> T<unit>
            Generic - fun t -> "error" => t *+ T<obj> ^-> T<unit>
            "group" => T<string> *+ T<obj> ^-> T<unit>
            "groupEnd" => T<unit> ^-> T<unit>
            Generic - fun t -> "info" => t ^-> T<unit>
            Generic - fun t -> "info" => t *+ T<obj> ^-> T<unit>
            Generic - fun t -> "log" => t ^-> T<unit>
            Generic - fun t -> "log" => t *+ T<obj> ^-> T<unit>
            "profile" => T<string> ^-> T<unit>
            "profileEnd" => T<unit> ^-> T<unit>
            "time" => T<string> ^-> T<unit>
            "timeEnd" => T<string> ^-> T<unit>
            "trace" => T<unit> ^-> T<unit>
            Generic - fun t -> "warn" => t ^-> T<unit>
            Generic - fun t -> "warn" => t *+ T<obj> ^-> T<unit>
        ]
    ]

let Assembly =
    Assembly [
        yield! Ecma.Definition.Namespaces
        yield! Dom.Definition.Namespaces
        yield! Html5.Definition.Namespaces
        yield! Cookies.Definition.Namespaces
        yield Global
    ]

[<Sealed>]
type DomExtension() =
    interface IExtension with
        member x.Assembly = Assembly

[<assembly: Extension(typeof<DomExtension>)>]
do ()
