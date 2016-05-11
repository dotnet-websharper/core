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

module WebSharper.Compiler.ErrorPrinting

let private stringThatIsAProxyForANewlineInFlatErrors = string (char 29)
let NormalizeErrorString (text : string) =    
    if text = null then nullArg "text"
    let text = text.Trim()

    let buf = System.Text.StringBuilder()
    let mutable i = 0
    while i < text.Length do
        let delta = 
            match text.[i] with
            | '\r' when i + 1 < text.Length && text.[i + 1] = '\n' ->
                // handle \r\n sequence - replace it with one single space
                buf.Append(stringThatIsAProxyForANewlineInFlatErrors) |> ignore
                2
            | '\n' ->
                buf.Append(stringThatIsAProxyForANewlineInFlatErrors) |> ignore
                1
            | c ->
                // handle remaining chars: control - replace with space, others - keep unchanged
                let c = if System.Char.IsControl(c) then ' ' else c
                buf.Append(c) |> ignore
                1
        i <- i + delta
    buf.ToString()

