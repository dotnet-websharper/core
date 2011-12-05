// $begin{copyright}
// 
// This file is part of WebSharper
// 
// Copyright (c) 2008-2011 IntelliFactory
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
module internal WebSharper.Options

module A          = Arguments
type private Path = string

type CompilationOptions =
    {
        ErrorLimit          : int
        Extraction          : list<Path * string * Path>
        Input               : Path
        KeyPair             : option<Path>
        References          : list<Path>
        Output              : Path
        OutputJavaScript    : option<Path>
        OutputMinified      : option<Path>
        TailCalls           : bool
    }

type T =
    | Compile of CompilationOptions
    | Dependencies of Path
    | Unpack of Path * list<Path>

let private version =
    typeof<T>.Assembly.Location
    |> System.Diagnostics.FileVersionInfo.GetVersionInfo

let private usage =
    System.String.Format("\
Usage: WebSharper.exe [options] input.dll output.dll

WebSharper (TM) Compiler V{0}
Copyright (c) IntelliFactory, 2004-2011.

Compiles F#-produced assemblies to JavaScript, adding the resulting
code as an embedded resource to the assembly.", version.FileVersion)

let private file =
    let ok x =
        if not (System.IO.File.Exists x)
        then Some ("File does not exist: " + x)
        else None
    A.Filter ok A.String

let private spec =
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
                    ErrorLimit          = errors
                    Extraction          = extract
                    Input               = input
                    KeyPair             = snk
                    References          = Seq.toList (Seq.distinct refs)
                    Output              = output
                    OutputJavaScript    = js
                    OutputMinified      = jsmin
                    TailCalls           = tramp
                }
        | None, Some u ->
            return Unpack u
    }

let Run main = A.Run usage spec main
