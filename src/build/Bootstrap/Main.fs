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

/// This is the first entry point of the build system.
module IntelliFactory.WebSharper.Bootstrap.Main

open System
open System.Diagnostics
open System.Collections
open System.Collections.Specialized
open System.IO

/// Finds a command in path.
let FindCommand cmd =
    Environment.GetEnvironmentVariable("PATH").Split(Path.PathSeparator)
    |> Seq.map (fun dir -> Path.Combine(dir, cmd))
    |> Seq.tryFind File.Exists

/// Executes a command.
let Exec env (command: string) (arguments: string) =
    let psi = ProcessStartInfo()
    psi.FileName <-
        if File.Exists(command) then command else
            match FindCommand command with
            | Some command -> command
            | None -> failwithf "Unknown command: %s" command
    psi.Arguments <- arguments
    psi.CreateNoWindow <- true
    psi.UseShellExecute <- false
    psi.RedirectStandardError <- true
    psi.RedirectStandardOutput <- true
    for (k, v) in env do
        if psi.EnvironmentVariables.ContainsKey(k) |> not then
            psi.EnvironmentVariables.Add(k, v)
    use proc = new Process()
    proc.StartInfo <- psi
    proc.EnableRaisingEvents <- true
    proc.ErrorDataReceived.Add(fun d -> if String.IsNullOrEmpty(d.Data) |> not then eprintfn "%s" d.Data)
    proc.OutputDataReceived.Add(fun d -> if String.IsNullOrEmpty(d.Data) |> not then printfn "%s" d.Data)
    let ok = proc.Start()
    proc.BeginErrorReadLine()
    proc.BeginOutputReadLine()
    proc.WaitForExit()
    if proc.ExitCode <> 0 then
        failwithf "ERROR in %s %s" command arguments

/// Restores NuGet packages.
let RestorePackages () =
    if Directory.Exists("packages") |> not then
        let env = ["EnableNuGetPackageRestore", "true"]
        let nuget = Exec env "tools/NuGet/NuGet.exe"
        nuget "install IntelliFactory.Core -pre -o packages -excludeVersion -nocache"
        nuget "install IntelliFactory.Build -pre -o packages -excludeVersion -nocache"
        nuget "install IntelliFactory.Xml -pre -o packages -excludeVersion -nocache"
        nuget "install FSharp.Core.3 -pre -o packages -excludeVersion -nocache"
        nuget "install sharpcompress -version 0.10.3 -o packages -excludeVersion"
        nuget "install NuGet.Core -version 2.8.0 -o packages -excludeVersion"
        nuget "install Mono.Cecil -version 0.9.5.4 -o packages -excludeVersion"
        nuget "install AjaxMin -version 5.8.5172.27710 -o packages -excludeVersion"
        nuget "install FsNuGet -o packages -excludeVersion -nocache"

[<EntryPoint>]
let Start args =
    RestorePackages ()
    0
