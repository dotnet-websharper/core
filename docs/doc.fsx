open System
open System.IO
open System.Text.RegularExpressions

let linkPattern =
    Regex(@"\((\w+)[.]md\)", RegexOptions.Multiline)

type Link =
    {
        Source : string
        Target : string
    }

    override link.ToString() =
        String.Format("{0} -> {1}", link.Source, link.Target)

let getLinks (file: FileInfo) =
    let t = File.ReadAllText(file.FullName)
    Set [|
        for m in linkPattern.Matches(t) ->
            m.Groups.[1].Value
    |]
    |> Set.map (fun info ->
        {
            Source = file.Name
            Target = info
        })

let manualPage =
    FileInfo(Path.Combine(__SOURCE_DIRECTORY__, "WebSharper.md"))

let tocLinks =
    getLinks manualPage
    |> Set.map (fun link -> link.Target)

let d = DirectoryInfo(__SOURCE_DIRECTORY__)

type Problem =
    | Missing of Link
    | Orphan of Link

    override p.ToString() =
        match p with
        | Missing k -> "missing : " + string k
        | Orphan k -> "orphan  : " + string k

let doesExist name =
    let p = Path.Combine(__SOURCE_DIRECTORY__, name + ".md")
    File.Exists(p)

let problems =
    [|
        for file in d.EnumerateFiles("*.md") do
            let links = getLinks file
            yield!
                links
                |> Seq.choose (fun link ->
                    if not (doesExist link.Target) then
                        Some (Missing link)
                    elif not (tocLinks.Contains(link.Target)) then
                        Some (Orphan link)
                    else
                        None)
    |]

if problems.Length > 0 then
    printfn "Problems:"
    Seq.iter (printfn "  %O") problems

