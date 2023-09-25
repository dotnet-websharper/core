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

module internal WebSharper.Console
open WebSharper.JavaScript

type C = JavaScript.Console

[<Proxy(typeof<System.IO.TextWriter>)>]
type TextWriterProxy =
    new() = {}
    
    abstract Encoding : System.Text.Encoding
    default _.Encoding
        with get():System.Text.Encoding = null

    abstract Write : bool -> unit
    default _.Write(x: bool) = C.Log(x)
    abstract Write : char -> unit
    default _.Write(x: char) = C.Log(string x)
    abstract Write : char[]*int*int -> unit
    default _.Write(x: char[], s:int, c:int) = C.Log(System.String(Array.sub x s c)) // todo
    abstract Write : string*obj[] -> unit
    default _.Write(x: string, o: obj[]) = C.Log(System.String.Format(x,o))
    abstract Write : string * obj -> unit
    default _.Write(x: string, o: obj) = C.Log(System.String.Format(x,o))
    abstract Write : uint64 -> unit
    default _.Write(x: uint64) = C.Log(string x)
    abstract Write : uint32 -> unit
    default _.Write(x: uint32) = C.Log(string x)
    abstract Write : string -> unit
    default _.Write(x: string) = C.Log(x)
    abstract Write : float -> unit
    default _.Write(x: float) = C.Log(x)
    abstract Write : single -> unit
    default _.Write(x: single) = C.Log(x)
    abstract Write : string * obj * obj -> unit
    default _.Write(x: string, o1: obj, o2: obj) = C.Log(x)
    abstract Write : obj -> unit
    default _.Write(o: obj) = C.Log(string o)
    abstract Write : int64 -> unit
    default _.Write(x: int64) = C.Log(string x)
    abstract Write : int32 -> unit
    default _.Write(x: int32) = C.Log(string x)
    abstract Write : char[] -> unit
    default _.Write(x: char[]) = C.Log(System.String(x))
    
    abstract WriteLine : unit -> unit
    default _.WriteLine() = C.Log("")
    
    abstract WriteLine : bool -> unit
    default _.WriteLine(x: bool) = C.Log(x)
    abstract WriteLine : char -> unit
    default _.WriteLine(x: char) = C.Log(string x)
    abstract WriteLine : char[]*int*int -> unit
    default _.WriteLine(x: char[], s:int, c:int) = C.Log(System.String(Array.sub x s c)) // todo
    abstract WriteLine : string*obj[] -> unit
    default _.WriteLine(x: string, o: obj[]) = C.Log(System.String.Format(x,o))
    abstract WriteLine : string * obj -> unit
    default _.WriteLine(x: string, o: obj) = C.Log(System.String.Format(x,o))
    abstract WriteLine : uint64 -> unit
    default _.WriteLine(x: uint64) = C.Log(string x)
    abstract WriteLine : uint32 -> unit
    default _.WriteLine(x: uint32) = C.Log(string x)
    abstract WriteLine : string -> unit
    default _.WriteLine(x: string) = C.Log(x)
    abstract WriteLine : float -> unit
    default _.WriteLine(x: float) = C.Log(x)
    abstract WriteLine : single -> unit
    default _.WriteLine(x: single) = C.Log(x)
    abstract WriteLine : string * obj * obj -> unit
    default _.WriteLine(x: string, o1: obj, o2: obj) = C.Log(x)
    abstract WriteLine : string * obj * obj * obj -> unit
    default _.WriteLine(x: string, o1: obj, o2: obj, o3: obj) = C.Log(x)
    abstract WriteLine : obj -> unit
    default _.WriteLine(o: obj) = C.Log(string o)
    abstract WriteLine : int64 -> unit
    default _.WriteLine(x: int64) = C.Log(string x)
    abstract WriteLine : int32 -> unit
    default _.WriteLine(x: int32) = C.Log(string x)
    abstract WriteLine : char[] -> unit
    default _.WriteLine(x: char[]) = C.Log(System.String(x))


[<JavaScript>]
type OutTextWriter() =
    inherit System.IO.TextWriter()

    override _.Encoding
        with get() : System.Text.Encoding = null


[<JavaScript>]
type ErrorTextWriter() =
    inherit System.IO.TextWriter()

    override _.Encoding
        with get() : System.Text.Encoding = null

    override _.Write(x: bool) = C.Error(x)
    override _.Write(x: char) = C.Error(string x)
    override _.Write(x: char[], s:int, c:int) = C.Error(System.String(Array.sub x s c)) // todo
    override _.Write(x: string, o: obj[]) = C.Error(System.String.Format(x,o))
    override _.Write(x: string, o: obj) = C.Error(System.String.Format(x,o))
    override _.Write(x: uint64) = C.Error(string x)
    override _.Write(x: uint32) = C.Error(string x)
    override _.Write(x: string) = C.Error(x)
    override _.Write(x: float) = C.Error(x)
    override _.Write(x: single) = C.Error(x)
    override _.Write(x: string, o1: obj, o2: obj) = C.Error(x)
    override _.Write(o: obj) = C.Error(string o)
    override _.Write(x: int64) = C.Error(string x)
    override _.Write(x: int32) = C.Error(string x)
    override _.Write(x: char[]) = C.Error(System.String(x))
    override _.WriteLine() = C.Error("")
        
    override _.WriteLine(x: bool) = C.Error(x)
        
    override _.WriteLine(x: char) = C.Error(string x)
        
    override _.WriteLine(x: char[]) = C.Error(System.String(x))
        
    override _.WriteLine(x: char[], s: int, c: int) = C.Error(System.String(Array.sub x s c))
        
    override _.WriteLine(x: float) = C.Error(x)
        
    override _.WriteLine(x: int) = C.Error(x)
        
    override _.WriteLine(x: int64) = C.Error(x)
        
    override _.WriteLine(x: obj) = C.Error(string x)
        
    override _.WriteLine(x: single) = C.Error(x)
        
    override _.WriteLine(x: string) = C.Error(x)
        
    override _.WriteLine(x: string, o1: obj) = C.Error(System.String.Format(x, o1))
        
    override _.WriteLine(x: string, o1: obj, o2: obj) = C.Error(System.String.Format(x, o1, o2))
        
    override _.WriteLine(x: string, o1: obj, o2: obj, o3: obj) = C.Error(System.String.Format(x, o1, o2, o3))
        
    override _.WriteLine(x: string, o: obj[]) =  C.Error(System.String.Format(x, o))
        
    override _.WriteLine(x: uint32) = C.Error(x)
        
    override _.WriteLine(x: uint64) = C.Error(x)

let [<JavaScript>] errorWriter = new ErrorTextWriter()
let [<JavaScript>] outWriter = new OutTextWriter()
[<Proxy(typeof<System.Console>)>]
type ConsoleProxy =
    
    static member Error 
        with [<Inline;JavaScript>] get() : System.IO.TextWriter = errorWriter

    static member Out 
        with [<Inline;JavaScript>] get() : System.IO.TextWriter = outWriter
    [<Inline>]
    static member WriteLine() = C.Log("")
    [<Inline>]
    static member WriteLine(x: bool) = C.Log(x)
    [<Inline>]
    static member WriteLine(x: char) = C.Log(string x)
    [<Inline>]
    static member WriteLine(x: char[]) = C.Log(System.String(x))
    [<Inline>]
    static member WriteLine(x: char[], s: int, c: int) = C.Log(System.String(Array.sub x s c))
    [<Inline>]
    static member WriteLine(x: float) = C.Log(x)
    [<Inline>]
    static member WriteLine(x: int) = C.Log(x)
    [<Inline>]
    static member WriteLine(x: int64) = C.Log(x)
    [<Inline>]
    static member WriteLine(x: obj) = C.Log(string x)
    [<Inline>]
    static member WriteLine(x: single) = C.Log(x)
    [<Inline>]
    static member WriteLine(x: string) = C.Log(x)
    [<Inline>]
    static member WriteLine(x: string, o1: obj) = C.Log(System.String.Format(x, o1))
    [<Inline>]
    static member WriteLine(x: string, o1: obj, o2: obj) = C.Log(System.String.Format(x, o1, o2))
    [<Inline>]
    static member WriteLine(x: string, o1: obj, o2: obj, o3: obj) = C.Log(System.String.Format(x, o1, o2, o3))
    [<Inline>]
    static member WriteLine(x: string, o: obj[]) =  C.Log(System.String.Format(x, o))
    [<Inline>]
    static member WriteLine(x: uint32) = C.Log(x)
    [<Inline>]
    static member WriteLine(x: uint64) = C.Log(x)