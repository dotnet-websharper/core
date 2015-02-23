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

namespace WebSharper.Compiler

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
