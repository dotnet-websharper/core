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
            raisesMsg (e.Current) "IEnumerator.Current throws before starting enumeration"
            isTrue (e.MoveNext())
            equal (e.Current) expectedItem1
            isTrue (e.MoveNext())
            equal (e.Current) expectedItem2
            isFalse (e.MoveNext())
            raisesMsg (e.Current) "IEnumerator.Current throws after finishing enumeration"
        }
        
    let TestIEnumerableGeneric (x: System.Collections.Generic.IEnumerable<'T>) expectedItem1 expectedItem2 =
        let e = x.GetEnumerator()
        Do {
            raisesMsg (e.Current) "IEnumerator<T>.Current throws before starting enumeration"
            isTrue (e.MoveNext())
            equal (e.Current) expectedItem1
            isTrue (e.MoveNext())
            equal (e.Current) expectedItem2
            isFalse (e.MoveNext())
            raisesMsg (e.Current) "IEnumerator<T>.Current throws after finishing enumeration"
        }

    let TestICollection (x: System.Collections.ICollection) (expectedArray: 'T[]) =
        let a = Array.zeroCreate expectedArray.Length
        Do {
            equalMsg x.Count expectedArray.Length "ICollection.Count"
            x.CopyTo(a, 0)
            equalMsg a expectedArray "ICollection.CopyTo"
        }

    let TestICollectionGeneric (x: System.Collections.Generic.ICollection<'T>) newItem (expectedArray: 'T[]) =
        let a = Array.zeroCreate expectedArray.Length
        Do {
            isFalseMsg (x.IsReadOnly) "ICollection<T>.IsReadOnly false"
            x.Add(newItem)
            equalMsg x.Count expectedArray.Length "ICollection<T>.Count"
            x.CopyTo(a, 0)
            equalMsg a expectedArray "ICollection<T>.CopyTo"
            isTrueMsg (x.Contains newItem) "ICollection<T>.Contains true"
            isTrueMsg (x.Remove(newItem)) "ICollection<T>.Remove true"
            isFalseMsg (x.Contains newItem) "ICollection<T>.Contains false"
            isFalseMsg (x.Remove(newItem)) "ICollection<T>.Remove false"
            x.Clear()
            equalMsg x.Count 0 "ICollection<T>.Clear"
        }

    let TestICollectionGenericReadOnly (x: System.Collections.Generic.ICollection<'T>) missingItem (expectedArray: 'T[])  =
        let a = Array.zeroCreate expectedArray.Length
        Do {
            isTrueMsg (x.IsReadOnly) "ICollection<T>.IsReadOnly true"
            raisesMsg (x.Add(missingItem)) "ICollection<T>.Add throws"
            equalMsg x.Count expectedArray.Length "ICollection<T>.Count"
            x.CopyTo(a, 0)
            equalMsg a expectedArray "ICollection<T>.CopyTo"
            isTrueMsg (x.Contains (Array.last expectedArray)) "ICollection<T>.Contains true"
            isFalseMsg (x.Contains missingItem) "ICollection<T>.Contains false"
            raisesMsg (x.Remove(missingItem)) "ICollection<T>.Remove throws"
            raisesMsg (x.Clear()) "ICollection<T>.Clear throws"
            equalMsg x.Count expectedArray.Length "ICollection<T>.Count"
        }
    
    TestCategory "Collection interface implementations" {
        Test "Array" {
            let arr = [| 1; 3 |]
            run (TestIEnumerable arr 1 3)
            run (TestIEnumerableGeneric arr 1 3)
            run (TestICollection arr [| 1; 3 |])
            run (TestICollectionGenericReadOnly arr 2 [| 1; 3 |])
        }

        Test "ResizeArray" {
            let arr = ResizeArray [ 1; 3 ]
            run (TestIEnumerable arr 1 3)
            run (TestIEnumerableGeneric arr 1 3)
            run (TestICollection arr [| 1; 3 |])
            run (TestICollectionGeneric arr 2 [| 1; 3; 2 |])
        }

        Test "String" {
            let s = "ac"
            run (TestIEnumerable s 'a' 'c')
            run (TestIEnumerableGeneric s 'a' 'c')
        }

        Test "Dictionary" {
            let d = new System.Collections.Generic.Dictionary<int, int>()
            d.Add(1, 2)
            d.Add(2, 4)
            let kv1 = System.Collections.Generic.KeyValuePair(1, 2)
            let kv2 = System.Collections.Generic.KeyValuePair(2, 4)            
            let kv3 = System.Collections.Generic.KeyValuePair(3, 6)            
            run (TestIEnumerable d kv1 kv2)
            run (TestIEnumerableGeneric d kv1 kv2)
            run (TestICollectionGeneric d kv3 [| kv1; kv2; kv3 |])
        }

        Test "dict" {
            let d = dict [ 1, 2; 2, 4 ]
            let kv1 = System.Collections.Generic.KeyValuePair(1, 2)
            let kv2 = System.Collections.Generic.KeyValuePair(2, 4)            
            let kv3 = System.Collections.Generic.KeyValuePair(3, 6)            
            run (TestIEnumerable d kv1 kv2)
            run (TestIEnumerableGeneric d kv1 kv2)
            run (TestICollectionGeneric d kv3 [| kv1; kv2; kv3 |])
        }

        Test "readOnlyDict" {
            let d = readOnlyDict [ 1, 2; 2, 4 ]
            let kv1 = System.Collections.Generic.KeyValuePair(1, 2)
            let kv2 = System.Collections.Generic.KeyValuePair(2, 4)            
            let kv3 = System.Collections.Generic.KeyValuePair(3, 6)            
            run (TestIEnumerable d kv1 kv2)
            run (TestIEnumerableGeneric d kv1 kv2)
        }

        Test "HashSet" {
            let s = System.Collections.Generic.HashSet<int>()
            s.Add(1) |> ignore
            s.Add(3) |> ignore
            run (TestIEnumerable s 1 3)
            run (TestIEnumerableGeneric s 1 3)
            run (TestICollectionGeneric s 2 [| 1; 2; 3 |]) // order is sorted, specific to WebSharper's implementation, but .NET HashSet has no guarantees on ordering
        }

        Test "F# list" {
            let l = [ 1; 3 ]
            run (TestIEnumerable l 1 3)
            run (TestIEnumerableGeneric l 1 3)
        }
        
        Test "LinkedList" {
            let l = System.Collections.Generic.LinkedList<int>()
            l.AddLast(3) |> ignore
            l.AddFirst(1) |> ignore
            run (TestIEnumerable l 1 3)
            run (TestIEnumerableGeneric l 1 3)
            run (TestICollection l [| 1; 3 |])
            run (TestICollectionGeneric l 2 [| 1; 3; 2 |])
        }

        Test "Queue" {
            let q = System.Collections.Generic.Queue<int>()
            q.Enqueue(1)
            q.Enqueue(3)
            run (TestIEnumerable q 1 3)
            run (TestIEnumerableGeneric q 1 3)
            run (TestICollection q [| 1; 3 |])
        }

        Test "Stack" {
            let s = System.Collections.Generic.Stack<int>()
            s.Push(1)
            s.Push(3)
            run (TestIEnumerable s 3 1)
            run (TestIEnumerableGeneric s 3 1)
            run (TestICollection s [| 3; 1 |])
        }

        Test "F# Map" {
            let m = Map [ 1, 2; 2, 4 ]
            let kv1 = System.Collections.Generic.KeyValuePair(1, 2)
            let kv2 = System.Collections.Generic.KeyValuePair(2, 4)        
            let kv3 = System.Collections.Generic.KeyValuePair(3, 6)            
            run (TestIEnumerable m kv1 kv2)
            run (TestIEnumerableGeneric m kv1 kv2)
            run (TestICollectionGenericReadOnly m kv3 [| kv1; kv2 |])
        }

        Test "F# Set" {
            let s = Set [ 1; 3 ]
            run (TestIEnumerable s 1 3)
            run (TestIEnumerableGeneric s 1 3)
            run (TestICollectionGenericReadOnly s 2 [| 1; 3 |])
        }

        Test "ReadOnlyCollection" {
            let c = System.Collections.ObjectModel.ReadOnlyCollection<int>([| 1; 3 |])
            run (TestIEnumerable c 1 3)
            run (TestIEnumerableGeneric c 1 3)
            run (TestICollection c [| 1; 3 |])
            run (TestICollectionGenericReadOnly c 2 [| 1; 3 |])
        }
    }
