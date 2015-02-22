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
