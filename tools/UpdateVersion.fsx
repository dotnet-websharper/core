open System.IO
open System.Text.RegularExpressions

let ReplacePerLine (filePath: string) (updateLine: string -> string) =
    let lines = File.ReadAllLines(filePath)
    File.WriteAllLines(filePath, Array.map updateLine lines)

let UpdateExeConfig (filePath: string) (version: string) =
    ReplacePerLine filePath (fun l ->
        if l.Contains "WebSharper" then
            Regex("version=\"[0-9.]+\"")
                .Replace(l, "version=\"" + version + ".0.0\"")
        else l
    )

let UpdateAsmVersionAttr (filePath: string) (version: string) =
    ReplacePerLine filePath (fun l ->
        if l.Contains "AssemblyVersion" then
            Regex("\"[0-9.]+\"")
                .Replace(l, "\"" + version + ".0.0\"")
        else l
    )

let UpdateConfigFs (filePath: string) (version: string) =
    ReplacePerLine filePath (fun l ->
        if l.Contains "let PackageVersion" then
            Regex("\"[0-9.]+\"")
                .Replace(l, "\"" + version + "\"")
        else l
    )

let UpdateWebsiteConfigFs (filePath: string) (version: string) =
    ReplacePerLine filePath (fun l ->
        if l.Contains "let Version" then
            Regex("\"[0-9.]+\"")
                .Replace(l, "\"" + version + "\"")
        else l
    )

let UpdateOptionsFs (filePath: string) (version: string) =
    ReplacePerLine filePath (fun l ->
        if l.Contains "Version \"" then
            Regex("\"[0-9.]+\"")
                .Replace(l, "\"" + version + ".0.0\"")
        else l
    )

let (+/) p1 p2 = Path.Combine(p1, p2)

do
    match fsi.CommandLineArgs with
    | [| _fsx; v |] ->
        printfn "Setting version to: %s" v
        let sln = __SOURCE_DIRECTORY__ +/ ".."
        let src = sln +/ "src"
        let wsbuild = src +/ "build" +/ "WebSharper.Build"
        UpdateExeConfig (wsbuild +/ "WebSharper.exe.config") v
        UpdateExeConfig (wsbuild +/ "WebSharper31.exe.config") v
        UpdateAsmVersionAttr (sln +/ "msbuild" +/ "AssemblyInfo.fs") v
        UpdateConfigFs (wsbuild +/ "Config.fs") v
        UpdateWebsiteConfigFs (sln +/ "tests" +/ "Website" +/ "Config.fs") v
        UpdateOptionsFs (src +/ "compiler" +/ "WebSharper" +/ "Options.fs") v
    | args ->
        eprintfn "Usage: UpdateVersion VERSION"
        eprintfn "%A" args
