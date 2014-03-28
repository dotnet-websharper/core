open System
open System.IO

for proj in Directory.EnumerateFiles(__SOURCE_DIRECTORY__, "*.fsproj", SearchOption.AllDirectories) do
    if File.ReadAllText(proj).Contains("PathConventions") then
        printfn "%s" proj
