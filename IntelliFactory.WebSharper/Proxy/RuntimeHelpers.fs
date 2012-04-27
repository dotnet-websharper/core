// $begin{copyright}
//
// This file is part of WebSharper
//
// Copyright (c) 2008-2012 IntelliFactory
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

[<IntelliFactory.WebSharper.Core.Attributes.Proxy
    "Microsoft.FSharp.Core.CompilerServices.RuntimeHelpers, \
     FSharp.Core, Version=2.0.0.0, Culture=neutral, \
     PublicKeyToken=b03f5f7f11d50a3a">]
module private IntelliFactory.WebSharper.RuntimeHelpersProxy

#nowarn "40"

type IE<'T> = System.Collections.Generic.IEnumerator<'T>

[<JavaScript>]
[<Name "IntelliFactory.WebSharper.Seq.enumFinally">]
let EnumerateThenFinally (s: seq<'T>) (f: unit -> unit) : seq<'T> =
    Enumerable.Of (fun () ->
        let e = try Enumerator.Get s with e -> f(); raise e
        Enumerator.New () (fun x ->
            try
                if e.MoveNext() then
                    x.Current <- e.Current
                    true
                else
                    f ()
                    false
            with e ->
                f ()
                raise e))

[<JavaScript>]
[<Name "IntelliFactory.WebSharper.Seq.enumUsing">]
let EnumerateUsing<'T1,'T2,'T3 when 'T1 :> System.IDisposable
                                and 'T2 :> seq<'T3>>
        (x: 'T1) (f: 'T1 -> 'T2) : seq<'T3> =
    f x :> _

[<JavaScript>]
[<Name "IntelliFactory.WebSharper.Seq.enumWhile">]
let EnumerateWhile (f: unit -> bool) (s: seq<'T>) : seq<'T> =
    Enumerable.Of (fun () ->
        let rec next (en: Enumerator.T<option<IE<_>>,'T>) =
            match en.State with
            | None ->
                if f () then
                    en.State <- Some (Enumerator.Get s)
                    next en
                else
                    false
            | Some e ->
                if e.MoveNext() then
                    en.Current <- e.Current
                    true
                else
                    en.State <- None
                    next en
        Enumerator.New None next)
