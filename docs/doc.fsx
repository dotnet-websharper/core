open System
open System.IO
open System.Text.RegularExpressions

let linkPattern =
    Regex(@"\(([^.]+)[.]md\)")

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

let orphans =
    [|
        for file in d.EnumerateFiles("*.md") do
            let links = getLinks file
            yield!
                links
                |> Seq.filter (fun link ->
                    not <| tocLinks.Contains(link.Target) )
    |]

if orphans.Length > 0 then
    printfn "Orphan pages:"
    Seq.iter (printfn "  %O") orphans
