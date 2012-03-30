module internal IntelliFactory.WebSharper.Sitelets.Offline.Extra

open System.IO

// the type of an extra, either a File or an entire Directory, to be copied
type t = File of string | Directory of string

// naively copies a directory. fine for our purposes. 
let rec CopyDirectory src dest =
    if Directory.Exists dest |> not then
        Directory.CreateDirectory dest |> ignore

    for s in Directory.GetFiles src do
        let d = Path.Combine(dest, Path.GetFileName s)
        File.Copy(s, d, true)

    for dir in Directory.GetDirectories src do
        let d = Path.Combine(dest, Path.GetFileName dir)
        CopyDirectory dir d

// copies a t
let copy dest = function
| File s ->  File.Copy(s, dest, true)
| Directory s -> CopyDirectory s dest
     
// gets the name of a t
let GetName = function File s | Directory s -> s

// parses extra.config into a list of paths
// format
// <project-name> {
// <file-pattern>*
// }
// ...
// example
// WebProject {
//   css/*
//   images/*
// }
// param path : the path to where all the dlls get dumped
// param text : the text of the file
let parse (path : string) (text : string) : List<t> =
    let mutable l = []
    let matches = System.Text.RegularExpressions.Regex.Matches(text, "[^\s]+\s*{[^}]*}")
    for m in matches do
        let path = path + "\\..\\..\\..\\" + m.Value.Substring(0, m.Value.IndexOf(' ')) + "\\"
        let cur = m.Value.Substring(m.Value.IndexOf(' ') + 1)
        let sub_matches = System.Text.RegularExpressions.Regex.Matches(cur, "[^{}\s]+")
        for m in sub_matches do
            let s = path + m.Value
            let t = 
                if (s.Chars (s.Length - 1)) = '*' then
                    Directory (s.Substring(0, s.Length - 1))
                else
                File s
            l <- t :: l
    l

// our entry point
let CopyFiles (options: Options.T) = 
    let cwd = Directory.GetCurrentDirectory()
    let name = cwd + "\\..\\..\\extra.files"
    let dest =  cwd + "\\..\\html\\"
    if System.IO.File.Exists name then
        System.IO.File.ReadAllLines name
        |> Array.filter (fun l -> l.TrimStart([| '\t'; ' ' |]).IndexOf("//") <> 0) // filter out comments first
        |> String.concat "\n" // put the rest back together
        |> parse cwd
        |> List.iter (fun x -> (copy (dest + System.IO.Path.GetFileName (GetName x))) x)
