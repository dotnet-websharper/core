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

namespace WebSharper.JavaScript

open System.Runtime.CompilerServices

open WebSharper

[<Extension; Sealed>]
type JavaScriptExtensions =
    [<Extension; Inline "$0">]
    static member ToJS(x: obj) = X<Object>

    [<Extension; Inline; Macro(typeof<WebSharper.Macro.GetJS>)>]
    static member GetJS<'T>(x: obj, [<System.ParamArray>] i: string[]) = GetJS<'T> x i

    [<Extension; Inline "$0">]
    static member ToJS(x: 'T[]) = X<Array<'T>>

    [<Extension; Inline "$0">]
    static member ToJS(x: string) = X<String>

    [<Extension; Inline "$0">]
    static member ToJS(x: exn) = X<Error>

    [<Extension; Inline "$0">]
    static member ToJS(x: bool) = X<Boolean>

    [<Extension; Inline "$0">]
    static member ToJS(x: System.DateTime) = X<Date>

    [<Extension; Inline "$0">]
    static member ToJS(x: byte) = X<Number>

    [<Extension; Inline "$0">]
    static member ToJS(x: sbyte) = X<Number>

    [<Extension; Inline "$0">]
    static member ToJS(x: int16) = X<Number>

    [<Extension; Inline "$0">]
    static member ToJS(x: int32) = X<Number>

    [<Extension; Inline "$0">]
    static member ToJS(x: int64) = X<Number>

    [<Extension; Inline "$0">]
    static member ToJS(x: uint16) = X<Number>

    [<Extension; Inline "$0">]
    static member ToJS(x: uint32) = X<Number>

    [<Extension; Inline "$0">]
    static member ToJS(x: uint64) = X<Number>

    [<Extension; Inline "$0">]
    static member ToJS(x: single) = X<Number>

    [<Extension; Inline "$0">]
    static member ToJS(x: double) = X<Number>

    [<Extension; Inline "$0">]
    static member ToJS(x: decimal) = X<Number>

[<AutoOpen>]
module Extensions =
    open WebSharper

    type System.Object with
        [<Inline "$0">]
        [<System.Obsolete "Deprecated. Use JS property instead.">]
        member this.ToEcma() = X<Object>
        [<Inline "$0">]
        member this.JS = X<Object>

    type ``[]``<'T> with
        [<Inline "$0">]
        [<System.Obsolete "Deprecated. Use JS property instead.">]
        member this.ToEcma() = X<Array<'T>>
        [<Inline "$0">]
        member this.JS = X<Array<'T>>

    type System.String with
        [<Inline "$0">]
        [<System.Obsolete "Deprecated. Use JS property instead.">]
        member this.ToEcma() = X<String>
        [<Inline "$0">]
        member this.JS = X<String>

    type System.Exception with
        [<Inline "$0">]
        [<System.Obsolete "Deprecated. Use JS property instead.">]
        member this.ToEcma() = X<Error>
        [<Inline "$0">]
        member this.JS = X<Error>

    type Boolean with
        [<Inline "$0">]
        [<System.Obsolete "Deprecated. Use Self property instead.">]
        member this.ToDotNet() = X<bool>

    type System.Boolean with
        [<Inline "$0">]
        [<System.Obsolete "Deprecated. Use ToJS extension method instead.">]
        member this.ToEcma() = X<Boolean>   
        [<Inline "$0">]
        member this.ToJS() = X<Boolean>   

    type Date with
        [<Inline "$0.getTime()">]
        [<System.Obsolete "Deprecated. Use Self property instead.">]
        member this.ToDotNet() = X<System.DateTime>

    type System.DateTime with
        [<Inline "new Date($0)">]
        [<System.Obsolete "Deprecated. Use JS property instead.">]
        member this.ToEcma() = X<Date>
        [<Inline "new Date($0)">]
        member this.JS = X<Date>

    type Function with
        [<Inline "$0">]
        [<System.Obsolete "Unsafe.">]
        member this.ToDotNet<'T, 'R>() = X<'T -> 'R>

    type Number with
        [<Inline "$0">]
        [<System.Obsolete "Deprecated. Use Self property instead.">]
        member this.ToDotNet<'T when 'T: struct>() = X<'T>

    type System.Byte    with
        [<Inline "$0">]
        [<System.Obsolete "Deprecated. Use JS property instead.">]
        member this.ToEcma() = X<Number>
        [<Inline "$0">]
        member this.JS = X<Number>

    type System.SByte   with
        [<Inline "$0">]
        [<System.Obsolete "Deprecated. Use JS property instead.">]
        member this.ToEcma() = X<Number>
        [<Inline "$0">]
        member this.JS = X<Number>

    type System.Int16   with
        [<Inline "$0">]
        [<System.Obsolete "Deprecated. Use JS property instead.">]
        member this.ToEcma() = X<Number>
        [<Inline "$0">]
        member this.JS = X<Number>

    type System.Int32   with
        [<Inline "$0">]
        [<System.Obsolete "Deprecated. Use JS property instead.">]
        member this.ToEcma() = X<Number>      
        [<Inline "$0">]                       
        member this.JS = X<Number>            

    type System.Int64   with                  
        [<Inline "$0">]                       
        [<System.Obsolete "Deprecated. Use JS property instead.">]
        member this.ToEcma() = X<Number>      
        [<Inline "$0">]                       
        member this.JS = X<Number>            

    type System.UInt16  with                  
        [<Inline "$0">]                       
        [<System.Obsolete "Deprecated. Use JS property instead.">]
        member this.ToEcma() = X<Number>      
        [<Inline "$0">]                       
        member this.JS = X<Number>            

    type System.UInt32  with                  
        [<Inline "$0">]                       
        [<System.Obsolete "Deprecated. Use JS property instead.">]
        member this.ToEcma() = X<Number>
        [<Inline "$0">]
        member this.JS = X<Number>

    type System.UInt64  with
        [<Inline "$0">]
        [<System.Obsolete "Deprecated. Use JS property instead.">]
        member this.ToEcma() = X<Number>
        [<Inline "$0">]
        member this.JS = X<Number>

    type System.Single  with
        [<Inline "$0">]
        [<System.Obsolete "Deprecated. Use JS property instead.">]
        member this.ToEcma() = X<Number>
        [<Inline "$0">]
        member this.JS = X<Number>

    type System.Double  with
        [<Inline "$0">]
        [<System.Obsolete "Deprecated. Use JS property instead.">]
        member this.ToEcma() = X<Number>
        [<Inline "$0">]
        member this.JS = X<Number>

    type System.Decimal with
        [<Inline "$0">]
        [<System.Obsolete "Deprecated. Use JS property instead.">]
        member this.ToEcma() = X<Number>
        [<Inline "$0">]
        member this.JS = X<Number>

[<assembly: Extension>]
do ()
