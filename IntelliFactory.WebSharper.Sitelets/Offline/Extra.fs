// $begin{copyright}
//
// This file is part of WebSharper
//
// Copyright (c) 2008-2012 IntelliFactory
//
// GNU Affero General Public License Usage
// WebSharper is free software: you can redistribute it and/or modify it under
// the terms of the GNU Affero General Public License, version 3, as published
// by the Free Software Foundation.
//
// WebSharper is distributed in the hope that it will be useful, but WITHOUT
// ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
// FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License
// for more details at <http://www.gnu.org/licenses/>.
//
// If you are unsure which license is appropriate for your use, please contact
// IntelliFactory at http://intellifactory.com/contact.
//
// $end{copyright}

module IntelliFactory.WebSharper.Sitelets.Offline.Extra

open System.IO
open System.Text.RegularExpressions

// the type of an extra, either a File or an entire Directory, to be copied
type T =
    | File of string
    | Directory of string

let ( ++ ) a b = Path.Combine(a, b)

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
let copy dest t =
    match t with
    | File s ->  File.Copy(s, dest, true)
    | Directory s -> CopyDirectory s dest

// gets the name of a t
let GetName t =
    match t with
    | File s | Directory s -> s

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
let parse (path: string) (text: string) : List<T> =
    let mutable l = []
    let matches = Regex.Matches(text, "[^\s]+")
    for m in matches do
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
    printf "%s\n" dir
    let extras = dir ++ "extra.files"
    if File.Exists extras then
        printf "Copying files specified in %s\n" extras
        File.ReadAllLines extras
        |> Array.filter (fun l -> l.TrimStart([| '\t'; ' ' |]).IndexOf("//") <> 0) // filter out comments first
        |> String.concat "\n" // put the rest back together
        |> parse dir
        |> List.iter (fun x ->
            GetName x
            |> printf "Copying %s\n"
            copy (dest ++ Path.GetFileName (GetName x)) x)
    else
        printf "No extra.files specified. Skipping...\n"
