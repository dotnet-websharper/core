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

module Commands =

    [<Sealed>]
    type Environment() =
        static member Create() =
            Environment()

    type Result =
        | Errors of list<string>
        | Ok

    type ParseResult<'T> =
        | NotRecognized
        | Parsed of 'T
        | ParseFailed of list<string>

    type ICommand =
        abstract Parse : argv: list<string> -> ParseResult<Environment->Result>
        abstract Description : string
        abstract Id : string
        abstract Usage : string

    type ICommand<'T> =
        inherit ICommand
        abstract Execute : Environment * 'T -> Result
        abstract Parse : argv: list<string> -> ParseResult<'T>

    let DefineCommand<'T> id desc usage parse exec =
        {
            new ICommand<'T> with
                member this.Execute(env, cfg) = exec env cfg
                member this.Parse(argv) = parse argv
            interface ICommand with
                member this.Parse(argv) =
                    match parse argv with
                    | NotRecognized -> NotRecognized
                    | Parsed cfg -> Parsed (fun env -> exec env cfg)
                    | ParseFailed reasons -> ParseFailed reasons
                member this.Description = desc
                member this.Id = id
                member this.Usage = usage
        }

    let MkDir (d: string) =
        let d = DirectoryInfo(d)
        if not d.Exists then
            d.Create()

    let IsFile (s: string) =
        FileInfo(s).Exists

    let NoFile (s: string) =
        FileInfo(s).Exists |> not

    let NoDir (s: string) =
        DirectoryInfo(s).Exists |> not
