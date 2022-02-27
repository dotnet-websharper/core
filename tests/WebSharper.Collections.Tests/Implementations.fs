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

    let TestICollectionGenericReadOnly (x: System.Collections.Generic.ICollection<'T>) missingItem (expectedArray: 'T[]) =
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

    let TestIList (x: System.Collections.IList) newItem newItem2 =
        Do {
            isFalseMsg (x.IsFixedSize) "IList.IsFixedSize false"
            isFalseMsg (x.IsReadOnly) "IList.IsReadOnly false"
            isFalseMsg (x.Contains newItem) "IList.Contains false" 
            x[0] <- newItem 
            equalMsg (x[0]) newItem "IList.Item"
            isTrueMsg (x.Contains newItem) "IList.Contains true" 
            equalMsg (x.Add(newItem2)) 2 "IList.Add"
            equalMsg (x.IndexOf(newItem2)) 2 "IList.IndexOf"
            x.Insert(0, newItem2)
            equalMsg (x[0]) newItem2 "IList.Insert"
            equalMsg (x[1]) newItem "IList.Insert"
            x.Remove(newItem2)
            equalMsg (x[0]) newItem "IList.Remove"
            x.RemoveAt(0)
            equalMsg (x[1]) newItem2 "IList.Remove"
            x.Clear()
            equalMsg (x.Count) 0 "IList.Clear"
        }
        
    let TestIListGeneric (x: System.Collections.Generic.IList<'T>) newItem newItem2 =
        Do {
            x[0] <- newItem 
            equalMsg (x[0]) newItem "IList.Item"
            equalMsg (x.IndexOf(newItem)) 0 "IList.IndexOf"
            x.Insert(0, newItem2)
            equalMsg (x.IndexOf(newItem2)) 0 "IList.Insert"
            x.RemoveAt(0)
            equalMsg (x.IndexOf(newItem)) 0 "IList.RemoveAt"
        }

    let TestIListFixedSize (x: System.Collections.IList) newItem =
        Do {
            isTrueMsg (x.IsFixedSize) "IList.IsFixedSize true"
            isFalseMsg (x.IsReadOnly) "IList.IsReadOnly false"
            isFalseMsg (x.Contains newItem) "IList.Contains false" 
            x[0] <- newItem 
            equalMsg (x[0]) newItem "IList.Item"
            isTrueMsg (x.Contains newItem) "IList.Contains true" 
            raisesMsg (x.Add(newItem)) "IList.Add throws"
            equalMsg (x.IndexOf(newItem)) 0 "IList.IndexOf"
            raisesMsg (x.Insert(0, newItem)) "IList.Insert throws"
            raisesMsg (x.Remove(newItem)) "IList.Remove throws"
            raisesMsg (x.RemoveAt(0)) "IList.RemoveAt throws"
            x.Clear()
            equalMsg (x[0]) null "IList.Clear" // instead of default value, JS translation will clear to null only without type information
        }
        
    let TestIListGenericFixedSize (x: System.Collections.Generic.IList<'T>) newItem =
        Do {
            x[0] <- newItem 
            equalMsg (x[0]) newItem "IList<T>.Item"
            equalMsg (x.IndexOf(newItem)) 0 "IList<T>.IndexOf"
            raisesMsg (x.Insert(0, newItem)) "IList<T>.Insert throws"
            raisesMsg (x.RemoveAt(0)) "IList<T>.RemoveAt throws"
        }
    
    let TestIListReadOnly (x: System.Collections.IList) firstItem missingItem =
        Do {
            isTrueMsg (x.IsFixedSize) "IList.IsFixedSize true"
            isTrueMsg (x.IsReadOnly) "IList.IsReadOnly true"
            raisesMsg (x[0] <- missingItem) "IList.Item setter throws" 
            isFalseMsg (x.Contains missingItem) "IList.Contains false" 
            equalMsg (x[0]) firstItem "IList.Item"
            isTrueMsg (x.Contains firstItem) "IList.Contains true" 
            raisesMsg (x.Add(firstItem)) "IList.Add throws"
            equalMsg (x.IndexOf(firstItem)) 0 "IList.IndexOf"
            raisesMsg (x.Insert(0, missingItem)) "IList.Insert throws"
            raisesMsg (x.Remove(firstItem)) "IList.Remove throws"
            raisesMsg (x.RemoveAt(0)) "IList.RemoveAt throws"
            raisesMsg (x.Clear()) "IList.Clear throws"
        }
    
    let TestIListGenericReadOnly (x: System.Collections.Generic.IList<'T>) firstItem missingItem =
        Do {
            raisesMsg (x[0] <- missingItem) "IList<T>.Item setter throws" 
            equalMsg (x[0]) firstItem "IList<T>.Item"
            equalMsg (x.IndexOf(firstItem)) 0 "IList<T>.IndexOf"
            raisesMsg (x.Insert(0, missingItem)) "IList<T>.Insert throws"
            raisesMsg (x.RemoveAt(0)) "IList<T>.RemoveAt throws"
        }

    let TestISetGeneric (x: System.Collections.Generic.ISet<'T>) startingItems missingItem firstItem =
        Do {
            isTrueMsg (x.Add(missingItem)) "ISet<T>.Add true"
            isFalseMsg (x.Add(missingItem)) "ISet<T>.Add false"
            x.ExceptWith (Seq.singleton missingItem)
            isTrueMsg (x.Add(missingItem)) "ISet<T>.ExceptWith"
            x.IntersectWith startingItems
            isTrueMsg (x.Add(missingItem)) "ISet<T>.IntersectWith"
            isTrueMsg (x.IsProperSupersetOf(startingItems)) "ISet<T>.IsProperSupersetOf"
            isTrueMsg (x.IsSupersetOf(startingItems)) "ISet<T>.IsSupersetOf"
            isTrueMsg (x.Overlaps(startingItems)) "ISet<T>.IsSupersetOf"
            x.SymmetricExceptWith ([ missingItem; firstItem ])
            isTrueMsg (x.IsProperSubsetOf(startingItems)) "ISet<T>.IsProperSupersetOf"
            isTrueMsg (x.IsSubsetOf(startingItems)) "ISet<T>.IsSupersetOf"
            x.UnionWith (Seq.singleton  firstItem)
            isTrueMsg (x.SetEquals(startingItems)) "ISet<T>.UnionWith/SetEquals"
        }

    let TestIDictionary (x: System.Collections.IDictionary) firstKey firstValue newFirstValue expectedKeys expectedValues missingKey =
        Do {
            isFalseMsg (x.IsFixedSize) "IDictionary.IsFixedSize false"
            isFalseMsg (x.IsReadOnly) "IDictionary.IsReadOnly false"
            equalMsg (x[firstKey]) firstValue "IDictionary.Item"
            x[firstKey] <- newFirstValue
            equalMsg (x[firstKey]) newFirstValue "IDictionary.Item setter"
            run (TestICollection x.Keys expectedKeys)
            run (TestICollection x.Values expectedValues)
            isFalseMsg (x.Contains(missingKey)) "IDictionary.Contains false"
            x.Add(missingKey, newFirstValue)
            isTrueMsg (x.Contains(missingKey)) "IDictionary.Add"
            x.Remove(missingKey)
            isFalseMsg (x.Contains(missingKey)) "IDictionary.Remove"
            let e = x.GetEnumerator()
            raisesMsg (e.Current) "IDictionaryEnumerator.Current throws before starting enumeration"
            raisesMsg (e.Entry) "IDictionaryEnumerator.Entry throws before starting enumeration"
            raisesMsg (e.Key) "IDictionaryEnumerator.Key throws before starting enumeration"
            raisesMsg (e.Value) "IDictionaryEnumerator.Value throws before starting enumeration"
            isTrue (e.MoveNext())
            equal (e.Current) (System.Collections.Generic.KeyValuePair(expectedKeys[0], expectedValues[0]))
            equal (e.Entry) (System.Collections.DictionaryEntry(expectedKeys[0], expectedValues[0]))
            equal (e.Key) expectedKeys[0]
            equal (e.Value) expectedValues[0]
            isTrue (e.MoveNext())
            equal (e.Current) (System.Collections.Generic.KeyValuePair(expectedKeys[1], expectedValues[1]))
            equal (e.Entry) (System.Collections.DictionaryEntry(expectedKeys[1], expectedValues[1]))
            equal (e.Key) expectedKeys[1]
            equal (e.Value) expectedValues[1]
            isFalse (e.MoveNext())
            raisesMsg (e.Current) "IDictionaryEnumerator.Current throws after finishing enumeration"
            raisesMsg (e.Entry) "IDictionaryEnumerator.Entry throws after finishing enumeration"
            raisesMsg (e.Key) "IDictionaryEnumerator.Key throws after finishing enumeration"
            raisesMsg (e.Value) "IDictionaryEnumerator.Value throws after finishing enumeration"
            x.Clear()
            equalMsg x.Count 0 "IDictionary.Clear"
        }

    let TestIDictionaryGeneric (x: System.Collections.Generic.IDictionary<'K, 'V>) firstKey firstValue newFirstValue (expectedKeys: 'K[]) (expectedValues: 'V[]) missingKey =
        Do {
            equalMsg (x[firstKey]) firstValue "IDictionary<K, V>.Item"
            x[firstKey] <- newFirstValue
            equalMsg (x[firstKey]) newFirstValue "IDictionary<K, V>.Item setter"
            equalMsg (match x.TryGetValue(firstKey) with true, v -> Some v | _ -> None) (Some newFirstValue) "IDictionary<K, V>.TryGetValue"
            run (TestIEnumerable x.Keys expectedKeys[0] expectedKeys[1])
            run (TestIEnumerableGeneric x.Keys expectedKeys[0] expectedKeys[1])
            run (TestICollectionGenericReadOnly x.Keys missingKey expectedKeys)
            run (TestIEnumerable x.Values expectedValues[0] expectedValues[1])
            run (TestIEnumerableGeneric x.Values expectedValues[0] expectedValues[1])
            run (TestICollectionGenericReadOnly x.Values firstValue expectedValues)
            isFalseMsg (x.ContainsKey(missingKey)) "IDictionary<K, V>.ContainsKey false"
            x.Add(missingKey, newFirstValue)
            isTrueMsg (x.ContainsKey(missingKey)) "IDictionary<K, V>.Add"
            isTrueMsg (x.Remove(missingKey)) "IDictionary<K, V>.Remove"
            isFalseMsg (x.ContainsKey(missingKey)) "IDictionary<K, V>.ContainsKey after remove"
        }
        
    let TestIReadOnlyDictionaryGeneric (x: System.Collections.Generic.IReadOnlyDictionary<'K, 'V>) (expectedKeys: 'K[]) (expectedValues: 'V[]) missingKey =
        let firstKey = expectedKeys[0]
        let firstValue = expectedValues[0]
        Do {
            equalMsg (x[firstKey]) firstValue "IDictionary<K, V>.Item"
            equalMsg (match x.TryGetValue(firstKey) with true, v -> Some v | _ -> None) (Some firstValue) "IDictionary<K, V>.TryGetValue"
            run (TestIEnumerable x.Keys expectedKeys[0] expectedKeys[1])
            run (TestIEnumerableGeneric x.Keys expectedKeys[0] expectedKeys[1])
            run (TestIEnumerable x.Values expectedValues[0] expectedValues[1])
            run (TestIEnumerableGeneric x.Values expectedValues[0] expectedValues[1])
            isFalseMsg (x.ContainsKey(missingKey)) "IDictionary<K, V>.ContainsKey false"
            isTrueMsg (x.ContainsKey(firstKey)) "IDictionary<K, V>.ContainsKey true"
        }

    TestCategory "Collection interface implementations" {
        Test "Array" {
            let arr = [| 1; 3 |]
            run (TestIEnumerable arr 1 3)
            run (TestIEnumerableGeneric arr 1 3)
            run (TestICollection arr [| 1; 3 |])
            run (TestICollectionGenericReadOnly arr 2 [| 1; 3 |])
            run (TestIListFixedSize arr 2)
            run (TestIListGenericFixedSize arr 4)
        }

        Test "ResizeArray" {
            let arr = ResizeArray [ 1; 3 ]
            run (TestIEnumerable arr 1 3)
            run (TestIEnumerableGeneric arr 1 3)
            run (TestICollection arr [| 1; 3 |])
            run (TestICollectionGeneric arr 2 [| 1; 3; 2 |])
            let arr2 = ResizeArray [ 1; 3 ]
            run (TestIList arr2 2 0)
            let arr3 = ResizeArray [ 1; 3 ]
            run (TestIListGeneric arr3 4 5)
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
            run (TestICollection d [| kv1; kv2 |])
            run (TestICollectionGeneric d kv3 [| kv1; kv2; kv3 |])
            let d2 = new System.Collections.Generic.Dictionary<int, int>()
            d2.Add(1, 2)
            d2.Add(2, 4)
            run (TestIDictionary d2 1 2 3 [| 1; 2 |] [| 3; 4 |] 5)
            let d3 = new System.Collections.Generic.Dictionary<int, int>()
            d3.Add(1, 2)
            d3.Add(2, 4)
            run (TestIDictionaryGeneric d3 1 2 3 [| 1; 2 |] [| 3; 4 |] 5)
        }

        Test "dict" {
            let d = dict [ 1, 2; 2, 4 ]
            let kv1 = System.Collections.Generic.KeyValuePair(1, 2)
            let kv2 = System.Collections.Generic.KeyValuePair(2, 4)            
            let kv3 = System.Collections.Generic.KeyValuePair(3, 6)            
            run (TestIEnumerable d kv1 kv2)
            run (TestIEnumerableGeneric d kv1 kv2)
            run (TestICollectionGeneric d kv3 [| kv1; kv2; kv3 |])
            let d2 = dict [ 1, 2; 2, 4 ]
            run (TestIDictionaryGeneric d2 1 2 3 [| 1; 2 |] [| 3; 4 |] 5)
        }

        Test "readOnlyDict" {
            let d = readOnlyDict [ 1, 2; 2, 4 ]
            let kv1 = System.Collections.Generic.KeyValuePair(1, 2)
            let kv2 = System.Collections.Generic.KeyValuePair(2, 4)            
            run (TestIEnumerable d kv1 kv2)
            run (TestIEnumerableGeneric d kv1 kv2)
            run (TestIReadOnlyDictionaryGeneric d [| 1; 2 |] [| 2; 4 |] 5)
        }

        Test "HashSet" {
            let s = System.Collections.Generic.HashSet<int>()
            s.Add(1) |> ignore
            s.Add(3) |> ignore
            run (TestIEnumerable s 1 3)
            run (TestIEnumerableGeneric s 1 3)
            run (TestICollectionGeneric s 2 [| 1; 2; 3 |]) // order is sorted, specific to WebSharper's implementation, but .NET HashSet has no guarantees on ordering
            let s2 = System.Collections.Generic.HashSet<int>([| 1; 3 |])
            run (TestISetGeneric s2 (Seq.ofArray [| 1; 3 |]) 2 1)
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
            run (TestIListReadOnly c 1 0)
            run (TestIListGenericReadOnly c 1 0)
        }
    }
