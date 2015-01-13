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

namespace IntelliFactory.WebSharper.JavaScript

[<AutoOpen>]
module Extensions =
    open IntelliFactory.WebSharper

    type Array<'T> with 
        [<Inline "$0">] 
        member this.ToDotNet() = X<'T[]>
    type 'T ``[]`` with
        [<Inline "$0">]
        member this.ToEcma() = X<Array<'T>>   

    type Boolean with
        [<Inline "$0">]
        member this.ToDotNet() = X<bool>
    type System.Boolean with
        [<Inline "$0">]
        member this.ToEcma() = X<Boolean>   

    type Date with
        [<Inline "$0.getTime()">]
        member this.ToDotNet() = X<System.DateTime>
    type System.DateTime with
        [<Inline "new Date($0)">]
        member this.ToEcma() = X<Date>

    type Error with
        [<Inline "$0">]
        member this.ToDotNet() = X<System.Exception>
    type System.Exception with
        [<Inline "$0">]
        member this.ToEcma() = X<Error>

    type Function with
        [<Inline "$0">]
        member this.ToDotNet<'T, 'R>() = X<'T -> 'R>
    type FSharpFunc<'T, 'R> with
        [<Inline "$0">]
        member this.ToEcma() = X<Function>

    type Number with
        [<Inline "$0">]
        member this.ToDotNet<'T when 'T: struct>() = X<'T>
    type System.Byte    with
        [<Inline "$0">]
        member this.ToEcma() = X<Number>
    type System.SByte   with
        [<Inline "$0">]
        member this.ToEcma() = X<Number>
    type System.Int16   with
        [<Inline "$0">]
        member this.ToEcma() = X<Number>
    type System.Int32   with
        [<Inline "$0">]
        member this.ToEcma() = X<Number>
    type System.Int64   with
        [<Inline "$0">]
        member this.ToEcma() = X<Number>
    type System.UInt16  with
        [<Inline "$0">]
        member this.ToEcma() = X<Number>
    type System.UInt32  with
        [<Inline "$0">]
        member this.ToEcma() = X<Number>
    type System.UInt64  with
        [<Inline "$0">]
        member this.ToEcma() = X<Number>
    type System.Single  with
        [<Inline "$0">]
        member this.ToEcma() = X<Number>
    type System.Double  with
        [<Inline "$0">]
        member this.ToEcma() = X<Number>
    type System.Decimal with
        [<Inline "$0">]
        member this.ToEcma() = X<Number>

    type System.Object with
        [<Inline "$0">]
        member this.ToEcma() = X<Object>

    type String with
        [<Inline "$0">]
        member this.ToDotNet() = X<string>
    type System.String with
        [<Inline "$0">]
        member this.ToEcma() = X<String>
