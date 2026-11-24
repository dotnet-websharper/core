// $begin{copyright}
//
// This file is part of WebSharper
//
// Copyright (c) 2008-2026 IntelliFactory
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
namespace Conformance

open System
open System.Collections.Generic
open WebSharper

[<JavaScript>]
module Runner =
    let private kind (e: exn) =
        match e with
        | :? ArgumentOutOfRangeException -> "ArgumentOutOfRange"
        | :? ArgumentNullException -> "ArgumentNull"
        | :? ArgumentException -> "Argument"
        | :? KeyNotFoundException -> "KeyNotFound"
        | :? IndexOutOfRangeException -> "IndexOutOfRange"
        | :? InvalidOperationException -> "InvalidOperation"
        | :? DivideByZeroException -> "DivideByZero"
        | :? OverflowException -> "Overflow"
        | :? FormatException -> "Format"
        | :? NullReferenceException -> "NullReference"
        | :? InvalidCastException -> "InvalidCast"
        | :? MatchFailureException -> "MatchFailure"
        | :? NotSupportedException -> "NotSupported"
        | :? NotImplementedException -> "NotImplemented"
        | _ -> "Exception"

    let Run () : string[] =
        let all =
            List.concat [
                Numbers.Cases ()
                Strings.Cases ()
                Formatting.Cases ()
                Lists.Cases ()
                Arrays.Cases ()
                Seqs.Cases ()
                Maps.Cases ()
                Records.Cases ()
                Patterns.Cases ()
                Language.Cases ()
                Dates.Cases ()
            ]
        [|
            for (name, f) in all ->
                let v =
                    try f ()
                    with e ->
#if JS
                        "EXN " + kind e + " :: " + e.Message
#else
                        "EXN " + kind e
#endif
                // one line per case: %A output can span lines
                // a JavaScript case can return undefined where .NET throws
                let v = if isNull v then "<null>" else v
                name + " = " + v.Replace("\r", "").Replace("\n", "\\n")
        |]
