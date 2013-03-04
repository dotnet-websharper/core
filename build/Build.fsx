open System.IO
open System.Net

let baseDir =
    Path.GetDirectoryName(__SOURCE_DIRECTORY__)

let ( ++ ) a b =
    Path.Combine(a, b)

let libDir =
    baseDir ++ "Lib"

let makeDir path =
    if not (Directory.Exists(path)) then
        Directory.CreateDirectory(path)
        |> ignore

let baseUrl =
    "http://bitbucket.org/IntelliFactory/websharper/downloads/"

let download url path =
    if not (File.Exists(path)) then
        printfn "Downloading: %s" url
        use client = new WebClient()
        client.DownloadFile(url, path)
        |> ignore

let downloadFile (filename: string) =
    let url = baseUrl + filename
    let path = baseDir ++ "Lib" ++ filename
    download url path

makeDir libDir
downloadFile "AjaxMin.dll"
downloadFile "AjaxMin.tasks"
downloadFile "AjaxMinTask.dll"
downloadFile "Mono.Cecil.dll"
downloadFile "Mono.Cecil.Mdb.dll"
downloadFile "Mono.Cecil.Pdb.dll"
downloadFile "IntelliFactory.Xml.dll"
