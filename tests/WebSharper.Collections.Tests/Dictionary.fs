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

module WebSharper.Collections.Tests.Dictionary

open System
open System.Collections.Generic
open WebSharper
open WebSharper.Testing

[<JavaScript>]
type Foo = {Foo:string}

[<JavaScript; CustomEquality; NoComparison>]
type Bar = 
    {Bar:string}
    override this.GetHashCode() = 0
    override this.Equals(o) =
        match o with 
        | :? Bar as o -> this.Bar = o.Bar
        | _ -> false

[<JavaScript>]
let Tests =
    TestCategory "Dictionary" {

        Test "New" {
            let x = Dictionary()
            let y = Dictionary(10)
            expect 0
        }

        Test "Add" {
            let d = Dictionary()
            d.Add(1,"a")
            equal d.[1] "a"
        }

        Test "Clear" {
            let d = Dictionary()
            d.Add(1,"a")
            d.Clear()
            equal d.Count 0
            raises d.[1]
        }

        Test "Count" {
            let d = Dictionary()
            equal d.Count 0
            [1..100]
            |> List.iter (fun i -> d.Add(i,i))
            equal d.Count 100
        }

        Test "Objects" {
            let d = Dictionary()
            let foo = ""
            let f1 = {Foo = "1"}
            let f2 = {Foo = "2"}
            let f3 = f1
            d.Add(f1,1)
            d.Add(f2,2)
            equal (d.Item f3) 1
            equal (d.Item f2) 2
        }

        Test "ContainsKey" {
            let d = Dictionary ()
            let foo = ""
            let f1 = {Foo = "1"}
            let f2 = {Foo = "2"}
            let f3 = f1
            d.Add(f1,1)
            isTrue (d.ContainsKey f1)
            isFalse (d.ContainsKey f2)
        }

        Test "TryGetValue" {
            let d = Dictionary ()
            let foo = ""
            let f1 = {Foo = "1"}
            let f2 = {Foo = "2"}
            let f3 = f1
            d.Add(f1,1)
            equal (d.TryGetValue f1) (true, 1)
            isFalse (d.TryGetValue f2 |> fst)
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
            do
                let mutable ix = 0
                while enum.MoveNext() do
                    let kvp = enum.Current
                    vArr.[ix] <- kvp.Value
                    kArr.[ix] <- kvp.Key
                    ix <- ix + 1
            equal vArr [| 1; 2 |]
            equal (Array.map (fun o -> o.Foo) kArr) [| "1"; "2" |]
        }

        Test "Hashing keys" {
            let d = Dictionary<Bar, string>()
            let b1 = { Bar = "1" }
            let b2 = { Bar = "2" }
            d.Add(b1, "foo");
            notEqual b1 b2
            equal (hash b1) (hash b2)
            isFalse (d.ContainsKey b2)
        }

    }
