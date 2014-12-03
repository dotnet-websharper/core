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

namespace IntelliFactory.WebSharper.EcmaScript

module F = IntelliFactory.WebSharper.Core.Functions

module Extensions =
    open IntelliFactory.WebSharper

    type EcmaScript.Array<'T> with 
        [<Inline "$0">] 
        member this.ToDotNet() = X<'T[]>
    type 'T ``[]`` with
        [<Inline "$0">]
        member this.ToEcma() = X<EcmaScript.Array<'T>>   

    type EcmaScript.Boolean with
        [<Inline "$0">]
        member this.ToDotNet() = X<bool>
    type System.Boolean with
        [<Inline "$0">]
        member this.ToEcma() = X<EcmaScript.Boolean>   

    type EcmaScript.Date with
        [<Inline "$0.getTime()">]
        member this.ToDotNet() = X<System.DateTime>
    type System.DateTime with
        [<Inline "new Date($0)">]
        member this.ToEcma() = X<EcmaScript.Date>

    type EcmaScript.Error with
        [<Inline "$0">]
        member this.ToDotNet() = X<System.Exception>
    type System.Exception with
        [<Inline "$0">]
        member this.ToEcma() = X<EcmaScript.Error>

    type EcmaScript.Function with
        [<Inline "$0">]
        member this.ToDotNet<'T, 'R>() = X<'T -> 'R>
        member this.ToWSFunc<'T1, 'T2>() = X<F.Func<'T1,'T2>>
        member this.ToWSFunc<'T1,'T2,'T3>() = X<F.Func<'T1,'T2,'T3>>
        member this.ToWSFunc<'T1,'T2,'T3,'T4>() = X<F.Func<'T1,'T2,'T3,'T4>>
        member this.ToWSFunc<'T1,'T2,'T3,'T4,'T5>() = X<F.Func<'T1,'T2,'T3,'T4,'T5>>
        member this.ToWSFunc<'T1,'T2,'T3,'T4,'T5,'T6>() = X<F.Func<'T1,'T2,'T3,'T4,'T5,'T6>>
        member this.ToWSFunc<'T1,'T2,'T3,'T4,'T5,'T6,'T7>() = X<F.Func<'T1,'T2,'T3,'T4,'T5,'T6,'T7>>
        member this.ToWSFunc<'T1,'T2,'T3,'T4,'T5,'T6,'T7,'T8>() = X<F.Func<'T1,'T2,'T3,'T4,'T5,'T6,'T7,'T8>>
        member this.ToWSFunc<'T1,'T2,'T3,'T4,'T5,'T6,'T7,'T8,'T9>() = X<F.Func<'T1,'T2,'T3,'T4,'T5,'T6,'T7,'T8,'T9>>
    type FSharpFunc<'T, 'R> with
        [<Inline "$0">]
        member this.ToEcma() = X<EcmaScript.Function>
    type F.Func<'T1,'T2> with
        [<Inline "$0">]
        member this.ToEcma() = X<EcmaScript.Function>
    type F.Func<'T1,'T2,'T3> with
        [<Inline "$0">]
        member this.ToEcma() = X<EcmaScript.Function>
    type F.Func<'T1,'T2,'T3,'T4> with
        [<Inline "$0">]
        member this.ToEcma() = X<EcmaScript.Function>
    type F.Func<'T1,'T2,'T3,'T4,'T5> with
        [<Inline "$0">]
        member this.ToEcma() = X<EcmaScript.Function>
    type F.Func<'T1,'T2,'T3,'T4,'T5,'T6> with
        [<Inline "$0">]
        member this.ToEcma() = X<EcmaScript.Function>
    type F.Func<'T1,'T2,'T3,'T4,'T5,'T6,'T7> with
        [<Inline "$0">]
        member this.ToEcma() = X<EcmaScript.Function>
    type F.Func<'T1,'T2,'T3,'T4,'T5,'T6,'T7,'T8> with
        [<Inline "$0">]
        member this.ToEcma() = X<EcmaScript.Function>
    type F.Func<'T1,'T2,'T3,'T4,'T5,'T6,'T7,'T8,'T9> with
        [<Inline "$0">]
        member this.ToEcma() = X<EcmaScript.Function>

    type EcmaScript.Number with
        [<Inline "$0">]
        member this.ToDotNet<'T when 'T: struct>() = X<'T>
    type System.Byte    with
        [<Inline "$0">]
        member this.ToEcma() = X<EcmaScript.Number>
    type System.SByte   with
        [<Inline "$0">]
        member this.ToEcma() = X<EcmaScript.Number>
    type System.Int16   with
        [<Inline "$0">]
        member this.ToEcma() = X<EcmaScript.Number>
    type System.Int32   with
        [<Inline "$0">]
        member this.ToEcma() = X<EcmaScript.Number>
    type System.Int64   with
        [<Inline "$0">]
        member this.ToEcma() = X<EcmaScript.Number>
    type System.UInt16  with
        [<Inline "$0">]
        member this.ToEcma() = X<EcmaScript.Number>
    type System.UInt32  with
        [<Inline "$0">]
        member this.ToEcma() = X<EcmaScript.Number>
    type System.UInt64  with
        [<Inline "$0">]
        member this.ToEcma() = X<EcmaScript.Number>
    type System.Single  with
        [<Inline "$0">]
        member this.ToEcma() = X<EcmaScript.Number>
    type System.Double  with
        [<Inline "$0">]
        member this.ToEcma() = X<EcmaScript.Number>
    type System.Decimal with
        [<Inline "$0">]
        member this.ToEcma() = X<EcmaScript.Number>

    type System.Object with
        [<Inline "$0">]
        member this.ToEcma() = X<EcmaScript.Object>

    type EcmaScript.String with
        [<Inline "$0">]
        member this.ToDotNet() = X<string>
    type System.String with
        [<Inline "$0">]
        member this.ToEcma() = X<EcmaScript.String>

[<assembly: AutoOpen "IntelliFactory.WebSharper.EcmaScript.Extensions">]
()
