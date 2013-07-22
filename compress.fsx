#load "tools/includes.fsx"
#load "build/compress.includes.fsx"

open System.IO
open IntelliFactory.Build

let RootDir = __SOURCE_DIRECTORY__

let ( +/ ) a b =
    Path.Combine(a, b)

let RawJavaScriptFiles =
    [
        RootDir +/ "IntelliFactory.JavaScript" +/ "Runtime.js"
        RootDir +/ "IntelliFactory.WebSharper" +/ "Json.js"
    ]

let CompressJavaScript () =
    let jc = Yahoo.Yui.Compressor.JavaScriptCompressor()
    for i in RawJavaScriptFiles do
        let cj =
            File.ReadAllText(i)
            |> jc.Compress
        let p = Path.ChangeExtension(i, ".min.js")
        FileSystem.TextContent(cj).WriteFile(p)

do CompressJavaScript ()
