// $begin{copyright}
//
// This file is part of WebSharper
//
// Copyright (c) 2008-2013 IntelliFactory
//
// GNU Affero General Public License Usage
// WebSharper is free software: you can redistribute it and/or modify it under
// the terms of the GNU Affero General Public License, version 3, as published
// by the Free Software Foundation.
//
// WebSharper is distributed in the hope that it will be useful, but WITHOUT
// ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
// FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License
// for more details at <http://www.gnu.org/licenses/>.
//
// If you are unsure which license is appropriate for your use, please contact
// IntelliFactory at http://intellifactory.com/contact.
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