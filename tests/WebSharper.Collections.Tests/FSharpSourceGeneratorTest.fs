module FSharpSourceGeneratorTest

open System.IO

let GEN (file: string) =
    let outputFile = Path.ChangeExtension(file, ".fs")
    File.Copy(file, outputFile, true)
    [| outputFile |]

[<assembly: WebSharper.FSharpSourceGenerator("ext")>]
do ()