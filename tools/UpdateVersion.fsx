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

let UpdateConfigFs (filePath: string) (version: string) (suffix: option<string>) =
    ReplacePerLine filePath (fun l ->
        if l.Contains "let PlainVersion" then
            Regex("\"[0-9.]+\"")
                .Replace(l, "\"" + version + "\"")
        elif l.Contains "let VersionSuffix" then
            Regex("None|Some \".*\"")
                .Replace(l,
                    match suffix with
                    | None -> "None"
                    | Some suffix -> "Some \"" + suffix + "\"")
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
    let run v s =
        printfn "Setting version to: %s" v
        let sln = __SOURCE_DIRECTORY__ +/ ".."
        let src = sln +/ "src"
        let wsbuild = src +/ "build" +/ "WebSharper.Build"
        UpdateExeConfig (wsbuild +/ "WebSharper.exe.config") v
        UpdateExeConfig (wsbuild +/ "WebSharper31.exe.config") v
        UpdateExeConfig (wsbuild +/ "WebSharper40.exe.config") v
        UpdateAsmVersionAttr (sln +/ "msbuild" +/ "AssemblyInfo.fs") v
        UpdateConfigFs (wsbuild +/ "Config.fs") v s
        UpdateWebsiteConfigFs (sln +/ "tests" +/ "Website" +/ "Config.fs") v
        UpdateOptionsFs (src +/ "compiler" +/ "WebSharper" +/ "Options.fs") v
    match fsi.CommandLineArgs with
    | [| _fsx; v |] -> run v None
    | [| _fsx; v; s |] -> run v (Some ("-" + s))
    | args ->
        eprintfn "Usage: UpdateVersion VERSION [SUFFIX]"
        eprintfn "%A" args
