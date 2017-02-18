// $begin{copyright}
//
// This file is part of WebSharper
//
// Copyright (c) 2008-2016 IntelliFactory
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

let tArgs i =
    if i = 1 then 
        [ "'T" ]
    else
        List.init i (fun j -> "'T" + string (j + 1))

let args i =
    if i = 1 then 
        [ "arg: 'T" ]
    else
        List.init i (fun j -> "arg" + string (j + 1) + ": 'T" + string (j + 1))

let concatE s l =
    l |> List.map (fun a -> a + s) |> String.concat ""

let maxArgCount = 6

let replaceGenerated path code =
    let allCode = 
        [|
            let mutable incl = true
            for l in System.IO.File.ReadAllLines(path) do
                if incl then yield l
                if l.Contains "// {{"
                then 
                    incl <- false 
                    yield! code
                elif l.Contains "// }}"
                then
                    incl <- true
                    yield l
        |]

    System.IO.File.WriteAllLines(path, allCode)

let toAnonTypArgs ts = if List.isEmpty ts then "" else "<" + String.concat "," (ts |> Seq.map (fun _ -> "_")) + ">"

let jsPervasives =
    let code = ResizeArray()
    let inline cprintfn x = Printf.kprintf code.Add x 

    for i = 2 to 7 do
        cprintfn "    /// Converts an F# Choice value to a JavaScript erased union"
        cprintfn "    [<Inline>]"
        cprintfn "    let ofChoice%d x =" i
        cprintfn "        match x with"
        for j = 1 to i do
            cprintfn "        | Choice%dOf%d v -> Union%dOf%d v" j i j i
        cprintfn "    /// Converts a JavaScript erased union to an F# option value"
        cprintfn "    [<Inline>]"
        cprintfn "    let toChoice%d x =" i
        cprintfn "        match x with"
        for j = 1 to i do
            cprintfn "        | Union%dOf%d v -> Choice%dOf%d v" j i j i
    
    code.ToArray()

replaceGenerated (__SOURCE_DIRECTORY__ + @"\JavaScript.Pervasives.fs") jsPervasives

let interop = 
    let code = ResizeArray()
    let inline cprintfn x = Printf.kprintf code.Add x 

    for pars in [ false; true ] do   
        for this in [ false; true ] do    
            if this || pars then
                for ret in [ false; true ] do 
                    let del = (if ret then "Func" else "Action")
                    let thisPars = (if this then "This" else "") + (if pars then "Params" else "")
                    let name = thisPars + del
                    let inTr = thisPars + "Func"
                    for i = 0 to maxArgCount do
                        let t = (if this then ["'TThis"] else[]) @ tArgs i @ (if pars then ["'TParams"] else []) @ (if ret then ["'TResult"] else [])
                        let toTypArgs ts = if List.isEmpty ts then "" else "<" + String.concat ", " ts + ">"
                        let a = (if this then ["thisArg: 'TThis"] else[]) @ args i @ (if pars then ["[<PA>] rest: 'TParams[]"] else [])
                        cprintfn "[<Proxy (typeof<%s%s>)>]" name (toAnonTypArgs t)
                        cprintfn "type %sProxy%s =" name (toTypArgs t)
                        cprintfn "    [<Inline \"$wsruntime.%s($del)\">]" inTr
                        cprintfn "    new (del: System.%s%s) = { }" del (toTypArgs t)
                        if this then
                            cprintfn "    [<Inline \"$this.bind($thisArg)\">]"
                            cprintfn "    member this.Bind(thisArg: 'TThis) = X<%s%s%s>" (if pars then "Params" else "System.") del (toTypArgs (List.tail t))
                        cprintfn "    [<Macro(typeof<Macro.JS%sCall>)>]" thisPars
                        cprintfn "    member this.Call(%s) = X<%s>" (a |> String.concat ", ") (if ret then "'TResult" else "unit")

    for i = 2 to 7 do
        let t = tArgs i
        cprintfn "[<Proxy (typeof<Union%s>)>]" (toAnonTypArgs t)
        cprintfn "type UnionProxy<%s> =" (t |> String.concat ", ")
        for j = 1 to i do
            cprintfn "    | Union%dOf%d of 'T%d" j i j
        for j = 1 to i do
            cprintfn "    [<Inline>]"
            cprintfn "    member this.Value%d = As<'T%d> this" j j
    
    code.ToArray()

replaceGenerated (__SOURCE_DIRECTORY__ + @"\Interop.fs") interop
