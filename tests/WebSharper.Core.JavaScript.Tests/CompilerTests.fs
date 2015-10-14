module WebSharper.Core.JavaScript.Test.CompilerTests

let Run () =
    
    Section "Compiler"

    let comp = WebSharper.Compiler.FSharp.Main.translateProject None @"D:\repo\websharper.csharp\src\stdlib\WebSharper.Main\WebSharper.Main.fsproj"
    WebSharper.Compiler.Packager.packageAssembly comp
    |> WebSharper.Compiler.Packager.exprToString WebSharper.Core.JavaScript.Readable
    |> printfn "%s"
