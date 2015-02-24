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

namespace WebSharper.JavaScript

open System.Runtime.CompilerServices

[<AutoOpen; Extension>]
module Extensions =
    open WebSharper

    [<Extension>]
    type System.Object with
        [<Extension; Inline "$0">]
        [<System.Obsolete "Deprecated. Use JS property instead.">]
        member this.ToEcma() = X<Object>
        [<Extension; Inline "$0">]
        member this.JS = X<Object>

    [<Extension>]
    type ``[]``<'T> with
        [<Extension; Inline "$0">]
        [<System.Obsolete "Deprecated. Use JS property instead.">]
        member this.ToEcma() = X<Array<'T>>
        [<Extension; Inline "$0">]
        member this.JS = X<Array<'T>>

    [<Extension>]
    type System.String with
        [<Extension; Inline "$0">]
        [<System.Obsolete "Deprecated. Use JS property instead.">]
        member this.ToEcma() = X<String>
        [<Extension; Inline "$0">]
        member this.JS = X<String>

    [<Extension>]
    type System.Exception with
        [<Extension; Inline "$0">]
        [<System.Obsolete "Deprecated. Use JS property instead.">]
        member this.ToEcma() = X<Error>
        [<Extension; Inline "$0">]
        member this.JS = X<Error>

    [<Extension>]
    type Boolean with
        [<Inline "$0">]
        [<System.Obsolete "Deprecated. Use Self property instead.">]
        member this.ToDotNet() = X<bool>

    [<Extension>]
    type System.Boolean with
        [<Extension; Inline "$0">]
        [<System.Obsolete "Deprecated. Use ToJS extension method instead.">]
        member this.ToEcma() = X<Boolean>   
        [<Extension; Inline "$0">]
        member this.ToJS() = X<Boolean>   

    [<Extension>]
    type Date with
        [<Extension; Inline "$0.getTime()">]
        [<System.Obsolete "Deprecated. Use Self property instead.">]
        member this.ToDotNet() = X<System.DateTime>

    [<Extension>]
    type System.DateTime with
        [<Extension; Inline "new Date($0)">]
        [<System.Obsolete "Deprecated. Use ToJS property instead.">]
        member this.ToEcma() = X<Date>
        [<Extension; Inline "new Date($0)">]
        member this.JS = X<Date>

    [<Extension>]
    type Function with
        [<Extension; Inline "$0">]
        [<System.Obsolete "Unsafe.">]
        member this.ToDotNet<'T, 'R>() = X<'T -> 'R>

    [<Extension>]
    type Number with
        [<Extension; Inline "$0">]
        [<System.Obsolete "Deprecated. Use Self property instead.">]
        member this.ToDotNet<'T when 'T: struct>() = X<'T>

    [<Extension>]
    type System.Byte    with
        [<Extension; Inline "$0">]
        [<System.Obsolete "Deprecated. Use JS property instead.">]
        member this.ToEcma() = X<Number>
        [<Extension; Inline "$0">]
        member this.JS = X<Number>

    [<Extension>]
    type System.SByte   with
        [<Extension; Inline "$0">]
        [<System.Obsolete "Deprecated. Use JS property instead.">]
        member this.ToEcma() = X<Number>
        [<Extension; Inline "$0">]
        member this.JS = X<Number>

    [<Extension>]
    type System.Int16   with
        [<Extension; Inline "$0">]
        [<System.Obsolete "Deprecated. Use JS property instead.">]
        member this.ToEcma() = X<Number>
        [<Extension; Inline "$0">]
        member this.JS = X<Number>

    [<Extension>]
    type System.Int32   with
        [<Extension; Inline "$0">]
        [<System.Obsolete "Deprecated. Use JS property instead.">]
        member this.ToEcma() = X<Number>      
        [<Extension; Inline "$0">]                       
        member this.JS = X<Number>            

    [<Extension>]
    type System.Int64   with                  
        [<Extension; Inline "$0">]                       
        [<System.Obsolete "Deprecated. Use JS property instead.">]
        member this.ToEcma() = X<Number>      
        [<Extension; Inline "$0">]                       
        member this.JS = X<Number>            

    [<Extension>]
    type System.UInt16  with                  
        [<Extension; Inline "$0">]                       
        [<System.Obsolete "Deprecated. Use JS property instead.">]
        member this.ToEcma() = X<Number>      
        [<Extension; Inline "$0">]                       
        member this.JS = X<Number>            

    [<Extension>]
    type System.UInt32  with                  
        [<Extension; Inline "$0">]                       
        [<System.Obsolete "Deprecated. Use JS property instead.">]
        member this.ToEcma() = X<Number>
        [<Extension; Inline "$0">]
        member this.JS = X<Number>

    [<Extension>]
    type System.UInt64  with
        [<Extension; Inline "$0">]
        [<System.Obsolete "Deprecated. Use JS property instead.">]
        member this.ToEcma() = X<Number>
        [<Extension; Inline "$0">]
        member this.JS = X<Number>

    [<Extension>]
    type System.Single  with
        [<Extension; Inline "$0">]
        [<System.Obsolete "Deprecated. Use JS property instead.">]
        member this.ToEcma() = X<Number>
        [<Extension; Inline "$0">]
        member this.JS = X<Number>

    [<Extension>]
    type System.Double  with
        [<Extension; Inline "$0">]
        [<System.Obsolete "Deprecated. Use JS property instead.">]
        member this.ToEcma() = X<Number>
        [<Extension; Inline "$0">]
        member this.JS = X<Number>

    [<Extension>]
    type System.Decimal with
        [<Extension; Inline "$0">]
        [<System.Obsolete "Deprecated. Use JS property instead.">]
        member this.ToEcma() = X<Number>
        [<Extension; Inline "$0">]
        member this.JS = X<Number>

    [<assembly: Extension>]
    do ()