module MyNamespace.FSharpSourceGeneratorTest

open System.IO

type ExtGenerator() =
    interface WebSharper.ISourceGenerator with
        member this.Generate file = 
            let outputFile = Path.ChangeExtension(file, ".fs")
            File.Copy(file, outputFile, true)
            [| outputFile |]

[<assembly: WebSharper.FSharpSourceGenerator("ext", typeof<ExtGenerator>)>]
do ()