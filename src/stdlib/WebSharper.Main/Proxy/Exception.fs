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

namespace WebSharper

open WebSharper.JavaScript

[<Proxy(typeof<System.Exception>)>]
type private ExceptionProxy =

    [<Inline "new Error()">]
    new () = {}

    [<Inline "new Error($message)">]
    new (message: string) = {}

    member this.Message with [<Inline "$this.message">] get () = X<string>

[<Proxy(typeof<MatchFailureException>)>]
type private MatchFailureExceptionProxy =

    [<Inline "new Error()">]
    new () = {}

    [<Inline "new Error($message + ' at ' + $line + ':' + $column)">]
    new (message: string, line: int, column: int) =
        MatchFailureExceptionProxy()

[<Proxy(typeof<System.IndexOutOfRangeException>)>]
type private IndexOutOfRangeExceptionProxy =

    [<Inline "new Error(\"IndexOutOfRangeException\")">]
    new () = {}

[<Proxy(typeof<System.OperationCanceledException>)>]
type OperationCanceledExceptionProxy =

    [<Inline "new Error(\"OperationCanceledException\")">]
    new () = {}

[<Proxy(typeof<System.ArgumentException>)>]
type ArgumentExceptionProxy =

    [<Inline "new Error(\"ArgumentException\")">]
    new () = {}

[<Proxy(typeof<System.InvalidOperationException>)>]
type InvalidOperationExceptionProxy =

    [<Inline "new Error(\"InvalidOperationException\")">]
    new () = {}

[<Proxy(typeof<System.AggregateException>)>]
[<Name "AggregateException">]
type AggregateExceptionProxy =

    [<Direct "e = new Error(\"AggregateException\"), e.InnerExceptions = $innerExceptions, e">]
    new (innerExceptions: exn[]) = {}

    [<Inline "$this.InnerExceptions">]
    member this.InnerExceptions = X<System.Collections.ObjectModel.ReadOnlyCollection<exn>>
