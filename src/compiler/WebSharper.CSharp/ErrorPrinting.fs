// $begin{copyright}
//
// This file is part of WebSharper
//
// Copyright (c) 2008-2018 IntelliFactory
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

module WebSharper.CSharp.ErrorPrinting

open WebSharper.Core
open WebSharper.Compiler
open WebSharper.Compiler.ErrorPrinting

let PrintGlobalError err =
    eprintfn "WebSharper error: %s" (NormalizeErrorString err)

let PrintGlobalWarning err =
    eprintfn "WebSharper warning: %s" (NormalizeErrorString err)

let PrintWebSharperErrors warnOnly (comp: Compilation) =
    let printWebSharperError (pos: AST.SourcePos option) isError msg =
        let severity = if isError && not warnOnly then "error WS9001" else "warning WS9002"
        match pos with
        | Some pos ->
            eprintfn "%s(%d,%d,%d,%d): WebSharper %s: %s" 
                pos.FileName (fst pos.Start) (snd pos.Start) (fst pos.End) (snd pos.End) 
                severity (NormalizeErrorString msg)
        | _ ->
            eprintfn "WebSharper %s: %s" severity (NormalizeErrorString msg)    
    for pos, err in comp.Errors do
        printWebSharperError pos true (string err)
    for pos, err in comp.Warnings do
        printWebSharperError pos false (string err)
