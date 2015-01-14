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

module IntelliFactory.WebSharper.JavaScript.Definition

open IntelliFactory.WebSharper.InterfaceGenerator
open IntelliFactory.WebSharper.JavaScript

/// The JavaScript global properties and functions can be used with all the built-in JavaScript objects.
let Global =
    Namespace "IntelliFactory.WebSharper.JavaScript" [
        Class "Console"
        |+> Static [
            "count" => !?T<string> ^-> T<unit>
            |> WithInline "console?console.count.apply(console,$0):undefined"
            "dir" => T<obj> ^-> T<unit>
            |> WithInline "console?console.dir($0):undefined"
            "error" => !+T<obj> ^-> T<unit>
            |> WithInline "console?console.error.apply(console,$0):undefined"
            "group" => T<string> *+ T<obj> ^-> T<unit>
            |> WithInline "console?console.group.apply(console,[$0].concat($1)):undefined"
            "groupEnd" => T<unit> ^-> T<unit>
            |> WithInline "console?console.groupEnd():undefined"
            "info" => !+T<obj> ^-> T<unit>
            |> WithInline "console?console.info.apply(console,$0):undefined"
            "log" => !+T<obj> ^-> T<unit>
            |> WithInline "console?console.log.apply(console,$0):undefined"
            "profile" => T<string> ^-> T<unit>
            |> WithInline "console?console.profile($0):undefined"
            "profileEnd" => T<unit> ^-> T<unit>
            |> WithInline "console?console.profileEnd():undefined"
            "time" => T<string> ^-> T<unit>
            |> WithInline "console?console.time($0):undefined"
            "timeEnd" => T<string> ^-> T<unit>
            |> WithInline "console?console.timeEnd($0):undefined"
            "trace" => T<unit> ^-> T<unit>
            |> WithInline "console?console.trace():undefined"
            "warn" => !+T<obj> ^-> T<unit>
            |> WithInline "console?console.warn.apply(console,$0):undefined"
        ]

        Class "JS"
        |+> Static [
                "window" =? Html5.General.Window |> WithGetterInline "window"
                "document" =? Dom.Interfaces.Document |> WithGetterInline "document"
                "NaN" =? T<double> |> WithGetterInline "$global.NaN"
                "Infinity" =? T<double> |> WithGetterInline "$global.Infinity"
                "undefined" =? T<obj> |> WithGetterInline "$global.undefined"
                "eval" => T<string->obj> |> WithInline "$global.eval($0)"
                "parseInt" => T<string> * !?T<int>?radix ^-> T<int> |> WithInline "$global.parseInt($0, $1)"
                "parseFloat" => T<string->double> |> WithInline "$global.parseFloat($0)"
                "isNaN" => T<obj> ^-> T<bool> |> WithInline "$global.isNaN($0)"
                "isFinite" => (T<int> + T<float>) ^-> T<bool> |> WithInline "$global.isFinite($0)"
                "decodeURI" => T<string->string> |> WithInline "$global.decodeURI($0)"
                "decodeURIComponent" => T<string->string> |> WithInline "$global.decodeURIComponent($0)"
                "encodeURI" => T<string->string> |> WithInline "$global.encodeURI($0)"
                "encodeURIComponent" => T<string->string> |> WithInline "$global.encodeURIComponent($0)"
            ]
    ]

let Assembly =
    Assembly [
        yield! Ecma.Definition.Namespaces
        yield! Dom.Definition.Namespaces
        yield! Html5.Definition.Namespaces
        yield Global
    ]

[<Sealed>]
type DomExtension() =
    interface IExtension with
        member x.Assembly = Assembly

[<assembly: Extension(typeof<DomExtension>)>]
do ()
