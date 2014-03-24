#r "../packages/AjaxMin/lib/net40/AjaxMin.dll"

open System
open System.IO
open Microsoft.Ajax.Utilities

let loc path =
    Path.Combine(__SOURCE_DIRECTORY__, "..", path)

let needsBuilding input output =
    let i = FileInfo(input)
    let o = FileInfo(output)
    not o.Exists || o.LastWriteTimeUtc < i.LastWriteTimeUtc

let minify (file: string) =
    let min = Minifier()
    let path = loc file
    let out = Path.ChangeExtension(path, ".min.js")
    if needsBuilding path out then
        let raw = File.ReadAllText(path)
        let mjs = min.MinifyJavaScript(raw)
        File.WriteAllText(Path.ChangeExtension(path, ".min.js"), mjs)
        stdout.WriteLine("Written {0}", out)

minify "src/compiler/IntelliFactory.JavaScript/Runtime.js"
minify "src/stdlib/IntelliFactory.WebSharper/Json.js"
