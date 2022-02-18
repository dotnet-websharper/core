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

module WebSharper.Collections.Tests.Implementations

open WebSharper
open WebSharper.JavaScript
open WebSharper.Testing

module R = WebSharper.Testing.RandomValues
        
[<JavaScript>]
let Tests =
    let TestIEnumerable (x: System.Collections.IEnumerable) expectedItem1 expectedItem2 =
        let e = x.GetEnumerator()
        Do {
            raises (e.Current)
            isTrue (e.MoveNext())
            equal (e.Current) expectedItem1
            isTrue (e.MoveNext())
            equal (e.Current) expectedItem2
            isFalse (e.MoveNext())
            raises (e.Current)
        }
        
    let TestIEnumerableGeneric (x: System.Collections.Generic.IEnumerable<'T>) expectedItem1 expectedItem2 =
        let e = x.GetEnumerator()
        Do {
            raises (e.Current)
            isTrue (e.MoveNext())
            equal (e.Current) expectedItem1
            isTrue (e.MoveNext())
            equal (e.Current) expectedItem2
            isFalse (e.MoveNext())
            raises (e.Current)
        }
    
    TestCategory "Collection interface implementations" {
        Test "Array as IEnumerable" {
            let arr = [| 1; 3 |]
            run (TestIEnumerable arr 1 3)
            run (TestIEnumerableGeneric arr 1 3)
        }

        Test "ResizeArray as IEnumerable" {
            let arr = ResizeArray [ 1; 3 ]
            run (TestIEnumerable arr 1 3)
            run (TestIEnumerableGeneric arr 1 3)
        }

        Test "String as IEnumerable" {
            let s = "ac"
            run (TestIEnumerable s 'a' 'c')
            run (TestIEnumerableGeneric s 'a' 'c')
        }

        Test "Dictionary as IEnumerable" {
            let d = new System.Collections.Generic.Dictionary<int, int>()
            d.Add(1, 2)
            d.Add(2, 4)
            let kv1 = System.Collections.Generic.KeyValuePair(1, 2)
            let kv2 = System.Collections.Generic.KeyValuePair(2, 4)            
            run (TestIEnumerable d kv1 kv2)
            run (TestIEnumerableGeneric d kv1 kv2)
        }

        Test "dict as IEnumerable" {
            let d = dict [ 1, 2; 2, 4 ]
            let kv1 = System.Collections.Generic.KeyValuePair(1, 2)
            let kv2 = System.Collections.Generic.KeyValuePair(2, 4)            
            run (TestIEnumerable d kv1 kv2)
            run (TestIEnumerableGeneric d kv1 kv2)
        }

        Test "readOnlyDict as IEnumerable" {
            let d = readOnlyDict [ 1, 2; 2, 4 ]
            let kv1 = System.Collections.Generic.KeyValuePair(1, 2)
            let kv2 = System.Collections.Generic.KeyValuePair(2, 4)            
            run (TestIEnumerable d kv1 kv2)
            run (TestIEnumerableGeneric d kv1 kv2)
        }

        Test "HashSet as IEnumerable" {
            let s = System.Collections.Generic.HashSet<int>()
            s.Add(1) |> ignore
            s.Add(3) |> ignore
            run (TestIEnumerable s 1 3)
            run (TestIEnumerableGeneric s 1 3)
        }

        Test "F# list as IEnumerable" {
            let l = [ 1; 3 ]
            run (TestIEnumerable l 1 3)
            run (TestIEnumerableGeneric l 1 3)
        }
        
        Test "LinkedList as IEnumerable" {
            let l = System.Collections.Generic.LinkedList<int>()
            l.AddLast(3) |> ignore
            l.AddFirst(1) |> ignore
            run (TestIEnumerable l 1 3)
            run (TestIEnumerableGeneric l 1 3)
        }

        Test "Queue as IEnumerable" {
            let q = System.Collections.Generic.Queue<int>()
            q.Enqueue(1)
            q.Enqueue(3)
            run (TestIEnumerable q 1 3)
            run (TestIEnumerableGeneric q 1 3)
        }

        Test "Stack as IEnumerable" {
            let s = System.Collections.Generic.Stack<int>()
            s.Push(1)
            s.Push(3)
            run (TestIEnumerable s 3 1)
            run (TestIEnumerableGeneric s 3 1)
        }

        Test "F# Map as IEnumerable" {
            let m = Map [ 1, 2; 2, 4 ]
            let kv1 = System.Collections.Generic.KeyValuePair(1, 2)
            let kv2 = System.Collections.Generic.KeyValuePair(2, 4)            
            run (TestIEnumerable m kv1 kv2)
            run (TestIEnumerableGeneric m kv1 kv2)
        }

        Test "F# Set as IEnumerable" {
            let s = Set [ 1; 3 ]
            run (TestIEnumerable s 1 3)
            run (TestIEnumerableGeneric s 1 3)
        }

        Test "ReadOnlyCollection as IEnumerable" {
            let c = System.Collections.ObjectModel.ReadOnlyCollection<int>([| 1; 3 |])
            run (TestIEnumerable c 1 3)
            run (TestIEnumerableGeneric c 1 3)
        }
    }
