open System
open System.Diagnostics
open System.IO

let exec (fileName: string) (arguments: string) =
    let psi = ProcessStartInfo()
    psi.FileName <- fileName
    psi.Arguments <- arguments
    psi.CreateNoWindow <- true
    psi.UseShellExecute <- false
    psi.RedirectStandardError <- true
    psi.RedirectStandardOutput <- true
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
    && nuget "install NuGet.Core -o packages -excludeVersion"
    && nuget "install Mono.Cecil -o packages -excludeVersion"
    && nuget "install AjaxMin -o packages -excludeVersion"

