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

[<AutoOpen>]
module IntelliFactory.WebSharper.EcmaExtensions

open IntelliFactory.WebSharper

type EcmaScript.Array with 
    [<Inline "$0">] 
    member this.ToDotNet<'T>() = X<'T[]>
type System.Array with
    [<Inline "$0">]
    member this.ToEcma() = X<EcmaScript.Array>   

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