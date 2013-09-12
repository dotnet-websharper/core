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

/// Provides command-line option parsing for WebSharper.
module internal IntelliFactory.WebSharper.Options

open System
open System.Diagnostics
open System.IO
open System.Security
module A = IntelliFactory.WebSharper.Arguments
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
        TailCalls : bool
    }

type T =
    | Compile of CompilationOptions
    | Dependencies of Path
    | Unpack of Path * list<Path>

let version =
    try
        let vn =
            typeof<T>.Assembly.Location
            |> FileVersionInfo.GetVersionInfo
        Version vn.FileVersion
    with _ ->
        Version "2.5.0.0"

let usage =
    String.Format("\
Usage: WebSharper.exe [options] input.dll output.dll

WebSharper (TM) Compiler V{0}
Copyright (c) IntelliFactory, 2004-2013.

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
        let! unpack =
            A.Keyword "-unpack"
                "Unpacks JavaScript from WebSharper-compiled assemblies."
                (A.Do {
                    let! x = A.String
                    let! y = A.Several file
                    return (x, y)
                })
            |> A.Optional
        let! deps =
            A.Keyword "-dep"
                "Prints resource dependency information for a given assembly."
                file
            |> A.Optional
        match deps, unpack with
        | Some file, _ ->
            return Dependencies file
        | None, None ->
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
                    TailCalls = tramp
                }
        | None, Some u ->
            return Unpack u
    }

let Run plugins main args =
    A.Run plugins args usage spec main
