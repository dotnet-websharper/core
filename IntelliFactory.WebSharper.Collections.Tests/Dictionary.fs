// $begin{copyright}
// 
// This file is part of WebSharper
// 
// Copyright (c) 2008-2011 IntelliFactory
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

module IntelliFactory.WebSharper.Collections.Tests.Dictionary

open System
open System.Collections.Generic
open IntelliFactory.WebSharper
open IntelliFactory.WebSharper.Testing

type Foo = {Foo:string}

[<JavaScript>]
let Tests =
    Section "Dictionary"

    Test "New" {
        let x = Dictionary()
        ()
    }

    Test "Add" {
        let d = Dictionary()
        d.Add(1,"a")
        d.[1] =? "a"
    }

    Test "Clear" {
        let d = Dictionary()
        d.Add(1,"a")
        d.Clear()
        Assert.Raises (fun () -> ignore (d.[1]))
    }

    Test "Count" {
        let d = Dictionary()
        d.Count =? 0
        [1..100]
        |> List.iter (fun i -> d.Add(i,i))
        d.Count =? 100
    }

    Test "Objects" {
        let d = Dictionary()
        let foo = ""
        let f1 = {Foo = "1"}
        let f2 = {Foo = "2"}
        let f3 = f1
        d.Add(f1,1)
        d.Add(f2,2)
        d.Item f3 =? 1
        d.Item f2 =? 2
    }

    Test "ContainsKey" {
        let d = Dictionary ()
        let foo = ""
        let f1 = {Foo = "1"}
        let f2 = {Foo = "2"}
        let f3 = f1
        d.Add(f1,1)
        d.ContainsKey f1 =? true
        d.ContainsKey f2 =? false
    }

    Test "GetEnumerator" {
        let d = Dictionary()
        let foo = ""
        let f1 = {Foo = "1"}
        let f2 = {Foo = "2"}
        let f3 = f1
        d.Add(f1,1)
        d.Add(f2,2)
        let enum = (d :> seq<_>).GetEnumerator()
        let kArr = Array.zeroCreate d.Count
        let vArr = Array.zeroCreate d.Count
        let mutable ix = 0
        let _ =
            while enum.MoveNext() do
                let kvp = enum.Current
                vArr.[ix] <- kvp.Value
                kArr.[ix] <- kvp.Key
                ix <- ix + 1
        vArr =? [| 1; 2 |]
        Array.map (fun o -> o.Foo) kArr =? [| "1"; "2" |]
    }
