open System
open System.Diagnostics
open System.Collections
open System.Collections.Specialized
open System.IO

let env =
    ["EnableNuGetPackageRestore", "true"]

let exec (fileName: string) (arguments: string) =
    let psi = ProcessStartInfo()
    psi.FileName <- fileName
    psi.Arguments <- arguments
    psi.CreateNoWindow <- true
    psi.UseShellExecute <- false
    psi.RedirectStandardError <- true
    psi.RedirectStandardOutput <- true
    for (k, v) in env do
        if psi.EnvironmentVariables.ContainsKey(k) |> not then
            psi.EnvironmentVariables.Add(k, v)
    let proc = Process.Start(psi)
    proc.WaitForExit()
    proc.StandardError.ReadToEnd() |> stderr.Write
    proc.StandardOutput.ReadToEnd() |> stdout.Write
    proc.ExitCode = 0

let loc path =
    Path.Combine(__SOURCE_DIRECTORY__, path)

let nuget cmd =
    exec (loc "NuGet/NuGet.exe") cmd

let ok =
    nuget "install IntelliFactory.Core -pre -o packages -excludeVersion -nocache"
    && nuget "install IntelliFactory.Build -pre -o packages -excludeVersion -nocache"
    && nuget "install IntelliFactory.Xml -pre -o packages -excludeVersion -nocache"
    && nuget "install FSharp.Core.3 -pre -o packages -excludeVersion -nocache"
    && nuget "install NuGet.Core -version 2.8.0 -o packages -excludeVersion"
    && nuget "install Mono.Cecil -version 0.9.5.4 -o packages -excludeVersion"
    && nuget "install AjaxMin -version 5.8.5172.27710 -o packages -excludeVersion"
