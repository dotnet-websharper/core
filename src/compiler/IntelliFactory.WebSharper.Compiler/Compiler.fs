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

namespace IntelliFactory.WebSharper.Compiler

type Name = string

type Priority =
    | Critical
    | Error
    | Warning

    override this.ToString() =
        match this with
        | Critical -> "critical"
        | Error -> "error"
        | Warning -> "warning"

type SourceLocation =
    {
        File: string
        Line: int
        Column: int
    }

type ReadableLocation = string

type Location =
    {
        ReadableLocation: ReadableLocation
        SourceLocation: option<SourceLocation>
    }

    override this.ToString() =
        match this.SourceLocation with
        | None -> this.ReadableLocation
        | Some s -> System.String.Format("{0}({1})", s.File, s.Line)

type Message =
    {
        Location: Location
        Priority: Priority
        Text: string
    }

    override this.ToString() =
        System.String.Format(
            "{0}: {1}: {2}",
            this.Location,
            this.Priority,
            this.Text
        )

type ErrorLimit = int

exception ErrorLimitExceeded

type Logger =
    {
        mutable count: int
        log: Message -> unit
        limit: ErrorLimit
    }

    static member Create log limit =
        {
            count = 0
            log = log
            limit = limit
        }

    member this.Log message =
        this.log message
        match message.Priority with
        | Critical -> this.count <- this.limit
        | Error -> this.count <- this.count + 1
        | Warning -> ()
        if this.count >= this.limit then
            raise ErrorLimitExceeded

type MemberScope =
    | Instance
    | Static

type Value =
    | Bool of bool
    | Double of double
    | Int of int
    | String of string
