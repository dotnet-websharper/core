// $begin{copyright}
//
// This file is part of WebSharper
//
// Copyright (c) 2008-2013 IntelliFactory
//
// GNU Affero General Public License Usage
// WebSharper is free software: you can redistribute it and/or modify it under
// the terms of the GNU Affero General Public License, version 3, as published
// by the Free Software Foundation.
//
// WebSharper is distributed in the hope that it will be useful, but WITHOUT
// ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
// FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License
// for more details at <http://www.gnu.org/licenses/>.
//
// If you are unsure which license is appropriate for your use, please contact
// IntelliFactory at http://intellifactory.com/contact.
//
// $end{copyright}

namespace IntelliFactory.WebSharper

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

