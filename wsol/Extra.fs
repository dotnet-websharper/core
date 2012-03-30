module internal IntelliFactory.WebSharper.Sitelets.Offline.Extra

open System.IO

// the type of an extra, either a File or an entire Directory, to be copied
type t = File of string | Directory of string

let ( ++ ) a b = System.IO.Path.Combine(a, b)

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
let copy dest t = match t with
| File s ->  File.Copy(s, dest, true)
| Directory s -> CopyDirectory s dest
     
// gets the name of a t
let GetName t = match t with File s | Directory s -> s

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
// param path : the directory containing extra.files
// param text : the text of the file
let parse (path : string) (text : string) : List<t> =
    let mutable l = []
    let matches = System.Text.RegularExpressions.Regex.Matches(text, "[^\s]+\s*{[^}]*}")
    for m in matches do
        let cur = m.Value.Substring(m.Value.IndexOf(' ') + 1)
        let sub_matches = System.Text.RegularExpressions.Regex.Matches(cur, "[^{}\s]+")
        for m in sub_matches do
            let s = path ++ m.Value
            let t = 
                if (s.Chars (s.Length - 1)) = '*' then
                    Directory (s.Substring(0, s.Length - 1))
                else
                File s
            l <- t :: l
    l

// our entry point
// dir is the directory of extra.files
let CopyFiles dir dest = 
    let extras = dir ++ "extra.files"
    if System.IO.File.Exists extras then
        printf "Copying files specified in %s\n" extras
        System.IO.File.ReadAllLines extras
        |> Array.filter (fun l -> l.TrimStart([| '\t'; ' ' |]).IndexOf("//") <> 0) // filter out comments first
        |> String.concat "\n" // put the rest back together
        |> parse dir
        |> List.iter (fun x -> GetName x |> printf "Copying %s\n"; copy (dest ++ System.IO.Path.GetFileName (GetName x)) x)
    else
        printf "No extra.files specified. Skipping...\n"