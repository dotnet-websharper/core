module MyNamespace.FSharpSourceGeneratorTest

open System.IO

type ExtGenerator() =
    interface WebSharper.ISourceGenerator with
        member this.Generate input = 
            printfn $"PreviousOutputFiles: %A{input.PreviousOutputFiles}"
            if Option.isSome input.PreviousOutputFiles then
                None
            else
                let outputFile = Path.ChangeExtension(input.FilePath, ".fs")
                File.Copy(input.FilePath, outputFile, true)
                printfn "(with console) Generated: %s" outputFile
                input.Print (sprintf "(with logger) Generated file: %s" outputFile)
                Some [| outputFile |]

[<assembly: WebSharper.FSharpSourceGenerator("ext", typeof<ExtGenerator>)>]
do ()