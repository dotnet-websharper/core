// $begin{copyright}
//
// This file is part of WebSharper
//
// Copyright (c) 2008-2014 IntelliFactory
//
// Licensed under the Apache License, Version 2.0 (the "License"); you
// may not use this file except in compliance with the License.  You may
// obtain a copy of the License at
//
//     http://www.apache.org/licenses/LICENSE-2.0
//
// Unless required by applicable law or agreed to in writing, software
// distributed under the License is distributed on an "AS IS" BASIS,
// WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or
// implied.  See the License for the specific language governing
// permissions and limitations under the License.
//
// $end{copyright}

namespace WebSharper.Build

#nowarn "25"

open System.IO
open System.Text.RegularExpressions

module Tags =

    let tagsFilePath = "src/htmllib/tags.csv"

    let groupByFst (s: seq<'a * 'b>) : seq<'a * seq<'b>> =
        s
        |> Seq.groupBy fst
        |> Seq.map (fun (k, s) -> k, Seq.map snd s)

    let Parse() =
        File.ReadAllLines(tagsFilePath)
        |> Array.map (fun line ->
            let [|``type``; status; name; srcname|] = line.Split ','
            (``type``, (status, (name, srcname))))
        |> groupByFst
        |> Seq.map (fun (k, s) -> (k, groupByFst s |> Map.ofSeq))
        |> Map.ofSeq

    let start = Regex("^(\s*)// *{{ *([a-z]+)( *([a-z]+))*")
    let finish = Regex("// *}}")

    let RunOn (path: string) (all: Map<string, Map<string, seq<string * string>>>) (f: string -> string -> string -> string[]) =
        if NeedsBuilding tagsFilePath path then
            let e = (File.ReadAllLines(path) :> seq<_>).GetEnumerator()
            let newLines =
                [|
                    while e.MoveNext() do
                        yield e.Current
                        let m = start.Match(e.Current)
                        if m.Success then
                            while e.MoveNext() && not (finish.Match(e.Current).Success) do ()
                            let indent = m.Groups.[1].Value
                            let ``type`` = m.Groups.[2].Value
                            let allType =
                                seq {
                                    for s in m.Groups.[4].Captures do
                                        match Map.tryFind s.Value all.[``type``] with
                                        | None -> ()
                                        | Some elts -> yield! elts
                                }
                                |> Seq.sortBy (snd >> fun s -> s.ToLower())
                            for name, srcname in allType do
                                for l in f ``type`` name srcname do
                                    yield indent + l
                            yield e.Current
                |]
            File.WriteAllLines(path, newLines)

    let Run() =
        let all = Parse()
        RunOn "src/htmllib/WebSharper.Html.Server/Tags.fs" all <| fun _ name srcname ->
            [|
                sprintf """let %s x = Html.NewTag "%s" x""" srcname name
            |]
        RunOn "src/htmllib/WebSharper.Html.Server/Attributes.fs" all <| fun _ name srcname ->
            [|
                sprintf """let %s x = Html.NewAttr "%s" x""" srcname name
            |]
        RunOn "src/htmllib/WebSharper.Html.Client/Tag.fs" all <| fun _ name srcname ->
            [|
                "[<Inline>]"
                "[<JavaScript>]"
                sprintf """member this.%s x = this.NewTag "%s" x""" srcname name
            |]
        RunOn "src/htmllib/WebSharper.Html.Client/Attr.fs" all <| fun _ name srcname ->
            [|
                "[<Inline>]"
                "[<JavaScript>]"
                sprintf """member this.%s x = this.NewAttr "%s" x""" srcname name
            |]
        RunOn "src/htmllib/WebSharper.Html.Client/Html.fs" all <| fun ty name srcname ->
            if ty = "tag" then
                [|
                    "[<Inline>]"
                    "[<JavaScript>]"
                    sprintf """let %s x = Tags.%s x""" srcname srcname
                |]
            else
                [|
                    "[<Inline>]"
                    "[<JavaScript>]"
                    sprintf """let %s x = Attr.%s x""" srcname srcname
                |]
