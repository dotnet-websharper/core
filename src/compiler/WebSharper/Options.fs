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

/// Provides command-line option parsing for WebSharper.
module internal WebSharper.Options

open System
open System.Diagnostics
open System.IO
open System.Security
module A = WebSharper.Arguments
type Path = string

type CompilationOptions =
    {
        ErrorLimit : int
        Extraction : list<Path * string * Path>
        Input : Path
        KeyPair : option<Path>
        References : list<Path>
        Output : Path
        OutputJavaScript : option<Path>
        OutputMinified : option<Path>
        OutputTypeScript : option<Path>
        IncludeSourceMap : bool
        TailCalls : bool
    }

type T =
    | Compile of CompilationOptions
    | Dependencies of Path

let version =
    try
        let vn =
            typeof<T>.Assembly.Location
            |> FileVersionInfo.GetVersionInfo
        Version vn.FileVersion
    with _ ->
        Version "3.2.0.0"

let usage =
    String.Format("\
Usage: WebSharper.exe [options] input.dll output.dll

WebSharper (TM) Compiler V{0}
Copyright (c) IntelliFactory, 2004-2014.

Compiles F#-produced assemblies to JavaScript, adding the resulting
code as an embedded resource to the assembly.", version)

let file =
    let ok x =
        let info =
            try Choice1Of2 (FileInfo x)
            with :? ArgumentException as e -> Choice2Of2 (string e)
        match info with
        | Choice1Of2 info ->
            if not info.Exists
            then Some (String.Format("File does not exist: [{0}]", x))
            else None
        | Choice2Of2 _ ->
            Some (String.Format("File does not exist: [{0}]", x))
    A.Filter ok A.String

let spec =
    A.Do {
        let! refs =
            A.Keyword "-r"
                "References another .NET assembly by path."
                file
            |> A.Many
        let! errors =
            A.Keyword "-errors"
                "The error limit. Default: 20"
                A.Int32
            |> A.Default 20
        let! snk =
            A.Keyword "-snk"
                "The path to the strong name key pair file."
                file
            |> A.Optional
        let! tramp =
            A.Keyword "-trampoline"
                "Enables trampolines for global tail-call optimization."
                (A.Do.Return true)
            |> A.Default false
        let! sm =
            A.Keyword "-sm"
                "Enables source map generation."
                (A.Do.Return true)
            |> A.Default false
        let! js =
            A.Keyword "-js"
                "The path for the generated JavaScript."
                A.String
            |> A.Optional
        let! jsmin =
            A.Keyword "-jsmin"
                "The path for the generated minified JavaScript."
                A.String
            |> A.Optional
        let! dts =
            A.Keyword "-dts"
                "The path for the generated TypeScript declarations."
                A.String
            |> A.Optional
        let! extract =
            A.Keyword "-extract"
                "Extracts a DLL resource of the given name to the target path."
                (A.Tuple3 file A.String A.String)
            |> A.Many
        let! deps =
            A.Keyword "-dep"
                "Prints resource dependency information for a given assembly."
                file
            |> A.Optional
        match deps with
        | Some file ->
            return Dependencies file
        | None ->
            let! input  = file
            let! output = A.String
            return
                Compile {
                    ErrorLimit = errors
                    Extraction = extract
                    Input = input
                    KeyPair = snk
                    References = Seq.toList (Seq.distinct refs)
                    Output = output
                    OutputJavaScript = js
                    OutputMinified = jsmin
                    OutputTypeScript = dts
                    IncludeSourceMap = sm
                    TailCalls = tramp
                }
    }

let Run main args =
    A.Run args usage spec main
