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

namespace WebSharper

open WebSharper.JavaScript
open Concurrency
open System.Runtime.InteropServices

[<Proxy(typeof<System.Guid>)>]
[<Prototype(false)>]
type internal GuidProxy =
    
    [<Inline "$g">]
    new (g: string) = {}
    
    [<Direct "return 'xxxxxxxx-xxxx-4xxx-yxxx-xxxxxxxxxxxx'.replace(/[xy]/g, function(c) {
        var r = Math.random()*16|0, v = c == 'x' ? r : (r&3|8);
        return v.toString(16); });">]
    static member NewGuid() = X<System.Guid>

    [<Constant "00000000-0000-0000-0000-000000000000">]
    static member Empty = X<System.Guid>

    [<Inline "$this">]
    override this.ToString() = X<string>

    [<Inline "$a == $b">]
    static member op_Equality(a: System.Guid, b: System.Guid) = X<bool>

    [<Inline "$a != $b">]
    static member op_Inequality(a: System.Guid, b: System.Guid) = X<bool>

    [<Inline>]
    member this.CompareTo(s: System.Guid) =
        Unchecked.compare (this :> obj) (s :> obj)

    [<Inline>]
    member this.CompareTo(s: obj) =
        Unchecked.compare (this :> obj) s

    static member FormatError() =
        raise (FormatException """Format String can be only "D", "d", "N", "n", "P", "p", "B", "b", "X" or "x".""")

    static member HexaError() =
        raise (FormatException "Hexadecimal Guid printing not implemented by WebSharper.")

    static member ShapeError() =
        raise (FormatException "Guid should contain 32 digits with 4 dashes (xxxxxxxx-xxxx-xxxx-xxxx-xxxxxxxxxxxx).")

    static member ParseError() =
        raise (FormatException "Unrecognized Guid format.")

    member this.ToString(format: string) =
        match format.ToUpper() with
        | "N" -> (As<string> this).Replace("-", "")
        | "D" -> As<string> this
        | "B" -> "{" + As<string> this + "}"
        | "P" -> "(" + As<string> this + ")"
        | "X" ->
            let s = As<string> this
            "{0x" + s.Substring(0, 8) + ",0x" + s.Substring(9, 4) + ",0x" + s.Substring(14, 4) + ",{0x"
            + s.Substring(19, 2) + ",0x" + s.Substring(21, 2) + ",0x" + s.Substring(24, 2) + ",0x"
            + s.Substring(26, 2) + ",0x" + s.Substring(28, 2) + ",0x" + s.Substring(30, 2) + ",0x" 
            + s.Substring(32, 2) + ",0x" + s.Substring(34, 2) + "}}"
        | _ -> GuidProxy.FormatError()
    
    static member Parse(input: string) =
        try GuidProxy.ParseExact(input, "D") 
        with _ ->
        try GuidProxy.ParseExact(input, "B") 
        with _ ->
        try GuidProxy.ParseExact(input, "P") 
        with _ ->
        try GuidProxy.ParseExact(input, "N") 
        with _ ->
            GuidProxy.ParseExact(input, "X") 

    static member ParseExact(input: string, format: string) =
        let parseD (s: string) =
            for i = 0 to 35 do
                match i with 
                | 8 | 13 | 18 | 23 -> if s.[i] <> '-' then GuidProxy.ShapeError()
                | _ ->
                    let c = s.[i]
                    if not (('0' <= c && c <= '9') || ('a' <= c && c <= 'f')) then GuidProxy.ShapeError()   
            As<System.Guid> s    

        match format.ToUpper() with
        | "N" -> 
            let s = input.Trim().ToLower()
            if s.Length <> 32 then GuidProxy.ShapeError()
            for i = 0 to 31 do
                let c = s.[i]
                if not (('0' <= c && c <= '9') || ('a' <= c && c <= 'f')) then GuidProxy.ShapeError()   
            s.Substring(0, 8) + "-" + s.Substring(8, 4) + "-" + s.Substring(12, 4) + "-" 
            + s.Substring(16, 4) + "-" + s.Substring(20)
            |> As<System.Guid>
        | "D" ->
            let s = input.Trim().ToLower()
            if s.Length <> 36 then GuidProxy.ShapeError()
            parseD s
        | "B" ->
            let s = input.Trim().ToLower()
            if s.Length <> 38 || s.[0] <> '{' || s.[17] <> '}' then GuidProxy.ShapeError()
            parseD (s.Substring(1, 36))
        | "P" ->
            let s = input.Trim().ToLower()
            if s.Length <> 38 || s.[0] <> '(' || s.[17] <> ')' then GuidProxy.ShapeError()
            parseD (s.Substring(1, 36))
        | "X" ->
            let s = input.Trim().ToLower()
            if s.Length <> 68 then GuidProxy.ShapeError()
            for i = 0 to 67 do
                match i with 
                | 0 | 26 -> if s.[i] <> '{' then GuidProxy.ShapeError()
                | 1 | 12 | 19 | 27 | 32 | 37 | 42 | 47 | 52 | 57 | 62 -> if s.[i] <> '0' then GuidProxy.ShapeError()
                | 2 | 13 | 20 | 28 | 33 | 38 | 43 | 48 | 53 | 58 | 63 -> if s.[i] <> 'x' then GuidProxy.ShapeError()
                | 11 | 18 | 25 | 31 | 36 | 41 | 46 | 51 | 56 | 61 -> if s.[i] <> ',' then GuidProxy.ShapeError()
                | 66 | 67 -> if s.[i] <> '{' then GuidProxy.ShapeError()
                | _ ->
                    let c = s.[i]
                    if not (('0' <= c && c <= '9') || ('a' <= c && c <= 'f')) then GuidProxy.ShapeError() 
            s.Substring(3, 8) + "-" + s.Substring(14, 4) + "-" + s.Substring(21, 4) + "-" + s.Substring(29, 2) 
            + s.Substring(34, 2) + "-" + s.Substring(39, 2) + s.Substring(44, 2) + s.Substring(49, 2) 
            + s.Substring(54, 2) + s.Substring(59, 2) + s.Substring(64, 2)
            |> As<System.Guid>
        | _ -> GuidProxy.FormatError()

    static member TryParse(input: string, [<Out>] output: byref<System.Guid>) =
        try 
            output <- GuidProxy.Parse(input)
            true
        with _ -> false

    static member TryParseExact(input: string, format: string, [<Out>] output: byref<System.Guid>) =
        try 
            output <- GuidProxy.ParseExact(input, format)
            true
        with _ -> false
