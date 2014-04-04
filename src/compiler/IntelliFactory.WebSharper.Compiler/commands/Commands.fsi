// $begin{copyright}
//
// This file is part of WebSharper
//
// Copyright (c) 2008-2014 IntelliFactory
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

/// Support for defining compiler operations as first-class Command values.
module Commands =

    /// Environment for command execution.
    [<Sealed>]
    type Environment =
        static member Create : unit -> Environment

    /// Results of executing the command
    type Result =
        | Errors of list<string>
        | Ok

    /// Results of parsing the command.
    type ParseResult<'T> =
        | NotRecognized
        | Parsed of 'T
        | ParseFailed of list<string>

    /// Common interface implemented by all commands.
    type ICommand =

        /// Parses command-line arguments.
        abstract Parse : argv: list<string> -> ParseResult<Environment->Result>

        /// One-liner description of what the command does.
        abstract Description : string

        /// Short identifier, such as "bundle".
        abstract Id : string

        /// Detailed description of what the command does.
        abstract Usage : string

    /// Common interface implemented by all commands.
    /// Parameterized by the config object.
    type ICommand<'T> =
        inherit ICommand

        /// Executes a command.
        abstract Execute : Environment * 'T -> Result

        /// Parses command-line arguments.
        abstract Parse : argv: list<string> -> ParseResult<'T>

    /// Used internally to define commands.
    val DefineCommand<'T> :
        id: string -> desc: string -> usage: string ->
        parse: (list<string> -> ParseResult<'T>) ->
        exec: (Environment -> 'T -> Result) ->
        ICommand<'T>

    val internal MkDir : string -> unit
    val internal IsFile : string -> bool
    val internal NoFile : string -> bool
    val internal NoDir : string -> bool
