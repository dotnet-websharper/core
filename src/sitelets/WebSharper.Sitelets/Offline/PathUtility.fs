// $begin{copyright}
//
// This file is part of WebSharper
//
// Copyright (c) 2008-2015 IntelliFactory
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

/// Implements offline sitelet HTML generation.
namespace WebSharper.Sitelets.Offline

open System
open System.Collections
open System.Collections.Generic
open System.IO

[<AutoOpen>]
module internal PathUtility =

    let isInvalidPathChar =
        HashSet(Path.GetInvalidPathChars()).Contains

    let isInvalidFileNameChar =
        HashSet(Path.GetInvalidFileNameChars()).Contains

    let isInvalidFilePatternChar =
        let h = HashSet(Path.GetInvalidFileNameChars())
        h.Remove('*') |> ignore
        h.Remove('?') |> ignore
        h.Contains

    let isReserved (name: string) =
        match name with
        | null -> false
        | _ ->
            match name.ToUpper() with
            | "CON" | "PRN" | "AUX" | "NUL"
            | "COM1" | "COM2" | "COM3" | "COM4" | "COM5"
            | "COM6" | "COM7" | "COM8" | "COM9"
            | "LPT1" | "LPT2" | "LPT3" | "LPT4"
            | "LPT5" | "LPT6" | "LPT7" | "LPT8" | "LPT9" -> true
            | _ -> false

    let isValidFileName (filename: string) =
        match filename with
        | null | "" -> false
        | f when isReserved f -> false
        | _ -> not (Seq.exists isInvalidPathChar filename)

    let isValidFilePattern (pat: string) =
        match pat with
        | null | "" -> false
        | f when isReserved f -> false
        | _ -> not (Seq.exists isInvalidFilePatternChar pat)

    type Path =
        | P0
        | P1 of Path * string

    let pathToFrags pat =
        let rec loop acc pat =
            match pat with
            | P0 -> acc
            | P1 (x, y) -> loop (y :: acc) x
        loop [] pat

    let (|RootPath|SubPath|) path =
        match path with
        | P0 -> RootPath
        | P1 (a, b) -> SubPath (a, b)

    let SubPath this subPath =
        if isValidFileName subPath then
            match subPath, this with
            | ".", _
            | "..", P0 -> this
            | "..", P1 (x, _) -> x
            | _, _ -> P1 (this, subPath)
        else
            failwithf "Invalid filename: %s" (string subPath)

    let RootPath = P0

    type Pattern =
        | Pa0
        | Pa1 of Pattern * string

    let patternToFrags pat =
        let rec loop acc pat =
            match pat with
            | Pa0 -> acc
            | Pa1 (x, y) -> loop (y :: acc) x
        loop [] pat

    let (|RootPattern|SubPattern|) path =
        match path with
        | Pa0 -> RootPattern
        | Pa1 (a, b) -> SubPattern (a, b)

    let RootPattern = Pa0

    let SubPattern this subPath =
        if isValidFilePattern subPath then
            match subPath, this with
            | ".", _
            | "..", Pa0 -> this
            | "..", Pa1 (x, _) -> x
            | _, _ -> Pa1 (this, subPath)
        else
            failwithf "Invalid filename pattern: %s" (string subPath)

    let splitPath =
        let chars =
            [|
                Path.AltDirectorySeparatorChar
                Path.DirectorySeparatorChar
            |]
        fun (path: string) ->
            path.Split(chars, StringSplitOptions.RemoveEmptyEntries)

    let CreatePath frags =
        List.fold SubPath P0 frags

    let CreatePattern frags =
        List.fold SubPattern Pa0 frags

    let tryParse create ok path =
        match path with
        | null -> None
        | _ ->
            let all = splitPath path
            if Array.forall ok all then
                Some (create (List.ofArray all))
            else
                None

    let TryParsePath path =
        tryParse CreatePath isValidFileName path

    let TryParsePattern pat =
        tryParse CreatePattern isValidFilePattern pat

    let ParsePath path =
        match TryParsePath path with
        | None -> invalidArg "path" "Invalid path"
        | Some r -> r

    let ParsePattern pat =
        match TryParsePattern pat with
        | None -> invalidArg "pat" "Invalid path pattern"
        | Some r -> r

    let ShowPath path =
        match path with
        | P0 -> "."
        | _ ->
            pathToFrags path
            |> String.concat "/"

    let ShowPattern pat =
        match pat with
        | Pa0 -> "."
        | _ ->
            patternToFrags pat
            |> String.concat "/"

    let ToAbsolute dir path =
        match path with
        | P0 -> dir
        | _ -> Path.Combine(dir, ShowPath path)
