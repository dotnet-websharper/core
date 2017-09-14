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
    member this.ToString() = X<string>

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
        | "X" -> GuidProxy.HexaError()
        | _ -> GuidProxy.FormatError()
    
    static member Parse(input: string) =
        let s = input.Trim().ToLower()
        if s.Length <> 36 then GuidProxy.ShapeError()
        for i = 0 to 35 do
            match i with 
            | 8 | 13 | 18 | 23 -> if s.[i] <> '-' then GuidProxy.ShapeError()
            | _ ->
                let c = s.[i]
                if not (('0' <= c && c <= '9') || ('a' <= c && c <= 'f')) then GuidProxy.ShapeError()   
        As<System.Guid> s    

    static member ParseExact(input: string, format: string) =
        match format.ToUpper() with
        | "N" -> 
            let s = input.Trim().ToLower()
            if s.Length <> 32 then GuidProxy.ShapeError()
            for i = 0 to 31 do
                let c = s.[i]
                if not (('0' <= c && c <= '9') || ('a' <= c && c <= 'f')) then GuidProxy.ShapeError()   
            As<System.Guid> (s.Substring(0, 8) + "-" + s.Substring(8, 4) + "-" + s.Substring(12, 4) + "-" + s.Substring(16, 4) + "-" + s.Substring(20))
        | "D" ->
            GuidProxy.Parse(input)
        | "B" ->
            let s = input.Trim().ToLower()
            if s.Length <> 38 || s.[0] <> '{' || s.[17] <> '}' then GuidProxy.ShapeError()
            GuidProxy.Parse(s.Substring(1, 36))
        | "P" ->
            let s = input.Trim().ToLower()
            if s.Length <> 38 || s.[0] <> '(' || s.[17] <> ')' then GuidProxy.ShapeError()
            GuidProxy.Parse(s.Substring(1, 36))
        | "X" -> raise (FormatException "Hexadecimal Guid parsing not implemented by WebSharper.")
        | _ -> raise (FormatException """Format String can be only "D", "d", "N", "n", "P", "p", "B", "b", "X" or "x".""")

    static member TryParse(input: string, [<Out>] output: byref<System.Guid>) =
        try 
            output <- GuidProxy.Parse(input)
            true
        with :? FormatException as e -> false

    static member TryParseExact(input: string, format: string, [<Out>] output: byref<System.Guid>) =
        try 
            output <- GuidProxy.ParseExact(input, format)
            true
        with :? FormatException as e -> false