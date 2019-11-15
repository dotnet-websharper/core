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

module WebSharper.Tests.Collections

open WebSharper
open WebSharper.JavaScript
open WebSharper.Testing

module R = WebSharper.Testing.RandomValues

[<JavaScript>]
type CollectionTest () =
    let innerCollection = [|1;3;5|]
    interface System.Collections.ICollection with
        member this.CopyTo(arr, ind) = Array.blit innerCollection ind (arr :?> int []) 0 (innerCollection.Length)
        member this.Count = innerCollection.Length
        member this.IsSynchronized = false
        member this.SyncRoot = this :> obj
        member this.GetEnumerator () = innerCollection.GetEnumerator()

[<JavaScript>]
type CollectionTest<'T when 'T : equality> () =
    let mutable innerCollection : 'T [] = [||]
    interface System.Collections.Generic.ICollection<'T> with
        member this.GetEnumerator () = innerCollection.GetEnumerator()
        member this.GetEnumerator () : System.Collections.Generic.IEnumerator<'T> = null
        member this.CopyTo(arr, ind) = Array.blit innerCollection ind arr 0 (innerCollection.Length)
        member this.Count = innerCollection.Length
        member this.IsReadOnly = false
        member this.Add x = innerCollection <- Array.append innerCollection [|x|]
        member this.Clear () = innerCollection <- [||]
        member this.Remove x =
            match Array.tryFindIndex (fun item -> item = x) innerCollection with
            | None -> false
            | Some index ->
                innerCollection <- Array.filter (fun item -> item <> x) innerCollection
                true
        member this.Contains x = Array.contains x innerCollection

    member this.Set(x: 'T []) =
        innerCollection <- x

[<JavaScript>]
type ListTest() =
    let mutable innerCollection : obj [] = [||]
    interface System.Collections.IList with
        member this.Count = innerCollection.Length
        member this.GetEnumerator () = innerCollection.GetEnumerator()
        member this.IsSynchronized = false
        member this.SyncRoot = this :> obj
        member this.CopyTo(arr, ind) = Array.blit innerCollection ind (arr :?> obj []) 0 (innerCollection.Length)
        member this.IsFixedSize = false
        member this.IsReadOnly = false
        member this.Item with get ind = innerCollection.[ind] and set ind v = innerCollection.[ind] <- v
        member this.IndexOf (v) = Array.tryFindIndex (fun x -> x = v) innerCollection |> Option.defaultValue -1
        member this.Add x =
            innerCollection <- Array.append innerCollection [|x|]
            innerCollection.Length
        member this.Clear () = innerCollection <- [||]
        member this.Remove x =
            match Array.tryFindIndex (fun item -> item = x) innerCollection with
            | None -> ()
            | Some index ->
                innerCollection <- Array.filter (fun item -> item <> x) innerCollection
        member this.Contains x = Array.contains x innerCollection
        member this.RemoveAt ind =
            if innerCollection.Length <= ind then
                ()
            else if innerCollection.Length = ind - 1 then
                innerCollection <- innerCollection.[0..ind-1]
            else
                innerCollection <- Array.append (innerCollection.[0..ind-1]) (innerCollection.[ind+1..innerCollection.Length])
        member this.Insert (ind, v) =
            if innerCollection.Length < ind then
                ()
            else if innerCollection.Length = ind then
                innerCollection <- Array.append innerCollection [|v|]
            else
                innerCollection <- Array.concat [|(innerCollection.[0..ind-1]); [|v|]; (innerCollection.[ind..innerCollection.Length])|]

[<JavaScript>]
type ListTest<'T when 'T : equality>() =
    let mutable innerCollection : 'T [] = [||]
    interface System.Collections.Generic.IList<'T> with
        member this.Count = innerCollection.Length
        member this.GetEnumerator () = innerCollection.GetEnumerator()
        member this.GetEnumerator () : System.Collections.Generic.IEnumerator<'T> = null
        member this.CopyTo(arr, ind) = Array.blit innerCollection ind arr 0 (innerCollection.Length)
        member this.IsReadOnly = false
        member this.Item with get ind = innerCollection.[ind] and set ind v = innerCollection.[ind] <- v
        member this.IndexOf (v) = Array.tryFindIndex (fun x -> x = v) innerCollection |> Option.defaultValue -1
        member this.Add x =
            innerCollection <- Array.append innerCollection [|x|]
        member this.Clear () = innerCollection <- [||]
        member this.Remove x =
            match Array.tryFindIndex (fun item -> item = x) innerCollection with
            | None -> false
            | Some index ->
                innerCollection <- Array.filter (fun item -> item <> x) innerCollection
                true
        member this.Contains x = Array.contains x innerCollection
        member this.RemoveAt ind =
            if innerCollection.Length <= ind then
                ()
            else if innerCollection.Length = ind - 1 then
                innerCollection <- innerCollection.[0..ind-1]
            else
                innerCollection <- Array.append (innerCollection.[0..ind-1]) (innerCollection.[ind+1..innerCollection.Length])
        member this.Insert (ind, v) =
            if innerCollection.Length < ind then
                ()
            else if innerCollection.Length = ind then
                innerCollection <- Array.append innerCollection [|v|]
            else
                innerCollection <- Array.concat [|(innerCollection.[0..ind-1]); [|v|]; (innerCollection.[ind..innerCollection.Length])|]

[<JavaScript>]
type DictTest<'T, 'U when 'T : equality and 'U : equality>() =
    let mutable innerCollection : ('T * 'U) list = []
    interface System.Collections.Generic.IDictionary<'T, 'U> with
        member this.CopyTo(arr, ind) = Array.blit (innerCollection |> List.map (fun (k, v) -> System.Collections.Generic.KeyValuePair(k, v)) |> List.toArray) ind arr 0 (innerCollection.Length)
        member this.Count = innerCollection.Length
        member this.IsReadOnly = false
        member this.GetEnumerator () = (innerCollection |> List.toArray).GetEnumerator()
        member this.GetEnumerator () : System.Collections.Generic.IEnumerator<System.Collections.Generic.KeyValuePair<'T, 'U>> = null
        member this.Clear () = innerCollection <- []
        member this.Contains kvp =
            innerCollection |> List.tryFind (fun (key,v) -> key = kvp.Key && v = kvp.Value) |> Option.isSome
        member this.Add (k, v) =
            match innerCollection |> List.tryFind (fun (key,_) -> key = k) with
            | None -> innerCollection <- List.append innerCollection [k,v]
            | Some _ -> ()
        member this.Add kvp =
            match innerCollection |> List.tryFind (fun (key,va) -> key = kvp.Key && va = kvp.Value) with
            | None -> innerCollection <- List.append innerCollection [kvp.Key,kvp.Value]
            | Some _ -> ()
        member this.ContainsKey k =
            innerCollection |> List.tryFind (fun (key,_) -> key = k) |> Option.isSome
        member this.Remove (k: 'T) =
            let res = innerCollection |> List.tryFind (fun (key,_) -> key = k) |> Option.isSome
            innerCollection <- List.filter (fun (key, _) -> k <> key) innerCollection
            res
        member this.Remove (k: System.Collections.Generic.KeyValuePair<'T, 'U>) =
            let res = innerCollection |> List.tryFind (fun (key, v) -> key = k.Key && v = k.Value) |> Option.isSome
            innerCollection <- List.filter (fun (key, v) -> not (k.Key = key && v = k.Value)) innerCollection
            res
        member this.TryGetValue (k, v) =
            match innerCollection |> List.tryFind (fun (key,_) -> key = k) with
            | None -> false
            | Some (_, av) ->
                v <- av
                true
        member this.Item with
                get ind = innerCollection |> List.find (fun (k, _) -> ind = k) |> snd
            and
                set ind v = innerCollection <- (innerCollection |> List.map (fun (k, ov) -> if k = ind then k,v else k,ov))
        member this.Keys =
            (innerCollection |> List.map (fst) |> List.toArray) :> System.Collections.Generic.ICollection<'T>
        member this.Values =
            (innerCollection |> List.map (snd) |> List.toArray) :> System.Collections.Generic.ICollection<'U>
            
[<JavaScript>]
type SetTest<'T when 'T : equality>() =
    let mutable innerCollection : 'T [] = [||]
    interface System.Collections.Generic.ISet<'T> with
        member this.CopyTo(arr, ind) = Array.blit innerCollection ind arr 0 (innerCollection.Length)
        member this.Count = innerCollection.Length
        member this.IsReadOnly = false
        member this.GetEnumerator () = innerCollection.GetEnumerator()
        member this.GetEnumerator () : System.Collections.Generic.IEnumerator<'T> = null
        member this.Clear () = innerCollection <- [||]
        member this.Contains x =
            innerCollection |> Array.contains x
        member this.Remove x =
            let res = innerCollection |> Array.contains x
            innerCollection <- Array.filter (fun item -> item <> x) innerCollection
            res
        member this.Add x : bool =
            if innerCollection |> Array.contains x then
                false
            else
                innerCollection <- Array.append innerCollection [|x|]
                true
        member this.Add x : unit =
            if innerCollection |> Array.contains x then
                ()
            else
                innerCollection <- Array.append innerCollection [|x|]
        member this.UnionWith x =
            innerCollection <- Seq.append innerCollection x |> Array.ofSeq |> Array.distinct
        member this.IntersectWith x =
            innerCollection <- innerCollection |> Array.filter (fun item -> Seq.contains item x)
        member this.IsSubsetOf x =
            innerCollection |> Array.forall (fun item -> Seq.contains item x)
        member this.IsSupersetOf x =
            x |> Seq.forall (fun item -> Seq.contains item innerCollection)
        member this.IsProperSubsetOf x =
            innerCollection |> Array.forall (fun item -> Seq.contains item x) && Seq.length x <> Array.length innerCollection
        member this.IsProperSupersetOf x =
            x |> Seq.forall (fun item -> Seq.contains item innerCollection) && Seq.length x <> Array.length innerCollection
        member this.ExceptWith x =
            innerCollection <- innerCollection |> Array.filter (fun item -> not <| Seq.contains item x)
        member this.SetEquals x =
            innerCollection |> Array.forall (fun item -> Seq.contains item x) &&
            x |> Seq.forall (fun item -> Seq.contains item innerCollection)
        member this.Overlaps x =
            innerCollection |> Array.exists (fun item -> Seq.contains item x)
        member this.SymmetricExceptWith x =
            let notInX = innerCollection |> Array.filter (fun item -> not <| Seq.contains item x)
            let notInOriginal = x |> Array.ofSeq |> Array.filter (fun item -> not <| Array.contains item innerCollection)
            innerCollection <- Array.concat [|notInOriginal;notInX|]

[<JavaScript>]
type DictTest() =
    let mutable innerCollection : (obj * obj) [] = [||]
    interface System.Collections.IDictionary with
        member this.CopyTo(arr, ind) = Array.blit innerCollection ind (arr :?> (obj * obj) []) 0 (innerCollection.Length)
        member this.Count = innerCollection.Length
        member this.IsReadOnly = false
        member this.GetEnumerator () = innerCollection.GetEnumerator()
        member this.GetEnumerator () : System.Collections.IDictionaryEnumerator = null
        member this.Clear () = innerCollection <- [||]
        member this.IsFixedSize = false
        member this.SyncRoot = null
        member this.IsSynchronized = false
        member this.Item with
                get ind = innerCollection |> Array.find (fun (k, _) -> ind = k) |> snd
            and
                set ind v = innerCollection <- (innerCollection |> Array.map (fun (k, ov) -> if k = ind then k,v else k,ov))
        member this.Keys =
            (innerCollection |> Array.map (fst)) :> System.Collections.ICollection
        member this.Values =
            (innerCollection |> Array.map (snd)) :> System.Collections.ICollection
        member this.Contains x =
            innerCollection |> Array.exists (fun (k, _) -> x = k)
        member this.Remove x =
            innerCollection <- innerCollection |> Array.filter (fun (k, _) -> x <> k)
        member this.Add (k, v) =
            if innerCollection |> Array.contains (k, v) then
                ()
            else
                innerCollection <- Array.append innerCollection [|k,v|]
        
[<JavaScript>]
let Tests =
    TestCategory "Generic Collections" {
        Test "ICollection" {
            let cT = CollectionTest()
            let expected = [|1;3;5|]
            let arr = Array.create 3 0
            (cT :> System.Collections.ICollection).CopyTo(arr, 0)
            equal arr expected
            equal ((cT :> System.Collections.ICollection).Count) 3
            equal ((cT :> System.Collections.ICollection).SyncRoot) (cT :> obj)
            equal ((cT :> System.Collections.ICollection).IsSynchronized) false
        }
        
        Test "ICollection'T" {
            let cT = CollectionTest<int>()
            equal ((cT :> System.Collections.Generic.ICollection<int>).Count) 0
            cT.Set([|1;3;5|])
            equal ((cT :> System.Collections.Generic.ICollection<int>).Count) 3
            (cT :> System.Collections.Generic.ICollection<int>).Add(4)
            equal ((cT :> System.Collections.Generic.ICollection<int>).Count) 4
            equal ((cT :> System.Collections.Generic.ICollection<int>).Remove 1) true
            equal ((cT :> System.Collections.Generic.ICollection<int>).Remove 1) false
            equal ((cT :> System.Collections.Generic.ICollection<int>).Count) 3
            equal ((cT :> System.Collections.Generic.ICollection<int>).Contains 4) true
            equal ((cT :> System.Collections.Generic.ICollection<int>).Contains 1) false
            (cT :> System.Collections.Generic.ICollection<int>).Clear()
            equal ((cT :> System.Collections.Generic.ICollection<int>).Count) 0
        }

        Test "IList" {
            let lT = ListTest()
            equal ((lT :> System.Collections.ICollection).Count) 0
            equal ((lT :> System.Collections.IList).Add 5) 1
            equal ((lT :> System.Collections.ICollection).Count) 1
            (lT :> System.Collections.IList).Insert (0, 1)
            equal ((lT :> System.Collections.ICollection).Count) 2
            equal ((lT :> System.Collections.IList).Contains 5) true
            equal ((lT :> System.Collections.IList).Contains 4) false
            equal ((lT :> System.Collections.IList).IndexOf 5) 1
            equal ((lT :> System.Collections.IList).Add 2) 3
            (lT :> System.Collections.IList).Remove 5
            equal ((lT :> System.Collections.ICollection).Count) 2
            (lT :> System.Collections.IList).RemoveAt 5
            equal ((lT :> System.Collections.ICollection).Count) 2
            (lT :> System.Collections.IList).RemoveAt 1
            equal ((lT :> System.Collections.ICollection).Count) 1
            (lT :> System.Collections.IList).Clear ()
            equal ((lT :> System.Collections.ICollection).Count) 0
        }

        Test "IList'T" {
            let lT = ListTest<int>()
            equal ((lT :> System.Collections.Generic.ICollection<int>).Count) 0
            (lT :> System.Collections.Generic.ICollection<int>).Add 5
            equal ((lT :> System.Collections.Generic.ICollection<int>).Count) 1
            (lT :> System.Collections.Generic.IList<int>).Insert (0, 1)
            equal ((lT :> System.Collections.Generic.ICollection<int>).Count) 2
            equal ((lT :> System.Collections.Generic.ICollection<int>).Contains 5) true
            equal ((lT :> System.Collections.Generic.ICollection<int>).Contains 4) false
            equal ((lT :> System.Collections.Generic.IList<int>).IndexOf 5) 1
            (lT :> System.Collections.Generic.ICollection<int>).Add 2
            equal ((lT :> System.Collections.Generic.ICollection<int>).Remove 5) true
            equal ((lT :> System.Collections.Generic.ICollection<int>).Count) 2
            (lT :> System.Collections.Generic.IList<int>).RemoveAt 5
            equal ((lT :> System.Collections.Generic.ICollection<int>).Count) 2
            (lT :> System.Collections.Generic.IList<int>).RemoveAt 1
            equal ((lT :> System.Collections.Generic.ICollection<int>).Count) 1
            (lT :> System.Collections.Generic.IList<int>).Clear ()
            equal ((lT :> System.Collections.Generic.ICollection<int>).Count) 0
        }

        Test "IDictionary'T'U" {
            let dT = DictTest<string, int>()
            equal ((dT :> System.Collections.Generic.ICollection<System.Collections.Generic.KeyValuePair<string, int>>).Count) 0
            (dT :> System.Collections.Generic.IDictionary<string, int>).Add("a", 5)
            equal ((dT :> System.Collections.Generic.ICollection<System.Collections.Generic.KeyValuePair<string, int>>).Count) 1
            (dT :> System.Collections.Generic.IDictionary<string, int>).Add("b", 6)
            equal ((dT :> System.Collections.Generic.ICollection<System.Collections.Generic.KeyValuePair<string, int>>).Count) 2
            equal ((dT :> System.Collections.Generic.ICollection<System.Collections.Generic.KeyValuePair<string, int>>).Contains(System.Collections.Generic.KeyValuePair("b", 6))) true
            equal ((dT :> System.Collections.Generic.ICollection<System.Collections.Generic.KeyValuePair<string, int>>).Contains(System.Collections.Generic.KeyValuePair("a", 6))) false
            equal ((dT :> System.Collections.Generic.IDictionary<string, int>).ContainsKey("c")) false
            equal ((dT :> System.Collections.Generic.IDictionary<string, int>).ContainsKey("a")) true
            equal ((dT :> System.Collections.Generic.IDictionary<string, int>).Remove(System.Collections.Generic.KeyValuePair("b", 7))) false
            equal ((dT :> System.Collections.Generic.IDictionary<string, int>).Remove(System.Collections.Generic.KeyValuePair("b", 6))) true
            equal ((dT :> System.Collections.Generic.ICollection<System.Collections.Generic.KeyValuePair<string, int>>).Count) 1
            equal ((dT :> System.Collections.Generic.IDictionary<string, int>).Remove("c")) false
            equal ((dT :> System.Collections.Generic.IDictionary<string, int>).Remove("a")) true
            equal ((dT :> System.Collections.Generic.ICollection<System.Collections.Generic.KeyValuePair<string, int>>).Count) 0
        }

        Test "ISet'T" {
            let sT = SetTest<int>()
            equal ((sT :> System.Collections.Generic.ICollection<int>).Count) 0
            (sT :> System.Collections.Generic.ICollection<int>).Add(5)
            equal ((sT :> System.Collections.Generic.ISet<int>).Add(6)) true
            equal ((sT :> System.Collections.Generic.ICollection<int>).Count) 2
            equal ((sT :> System.Collections.Generic.ISet<int>).IsSubsetOf([5;6;7])) true
            equal ((sT :> System.Collections.Generic.ISet<int>).IsSubsetOf([7;8;9])) false
            equal ((sT :> System.Collections.Generic.ISet<int>).IsSupersetOf([5;6])) true
            equal ((sT :> System.Collections.Generic.ISet<int>).IsSupersetOf([5])) true
            equal ((sT :> System.Collections.Generic.ISet<int>).IsSupersetOf([5;7])) false
            equal ((sT :> System.Collections.Generic.ISet<int>).IsProperSubsetOf([5;6;7])) true
            equal ((sT :> System.Collections.Generic.ISet<int>).IsProperSubsetOf([5;6])) false
            equal ((sT :> System.Collections.Generic.ISet<int>).IsProperSupersetOf([5;6])) false
            equal ((sT :> System.Collections.Generic.ISet<int>).IsProperSupersetOf([5])) true
            equal ((sT :> System.Collections.Generic.ISet<int>).Overlaps([5;9])) true
            equal ((sT :> System.Collections.Generic.ISet<int>).Overlaps([7;9])) false
            (sT :> System.Collections.Generic.ISet<int>).UnionWith([7;9])
            equal ((sT :> System.Collections.Generic.ICollection<int>).Count) 4
            (sT :> System.Collections.Generic.ISet<int>).IntersectWith([5;6;7;8])
            equal ((sT :> System.Collections.Generic.ICollection<int>).Count) 3
            (sT :> System.Collections.Generic.ISet<int>).ExceptWith([7;9])
            equal ((sT :> System.Collections.Generic.ICollection<int>).Count) 2
            (sT :> System.Collections.Generic.ISet<int>).SymmetricExceptWith([7;9])
            equal ((sT :> System.Collections.Generic.ICollection<int>).Count) 4
        }

        Test "IDictionary" {
            let dT = DictTest()
            equal ((dT :> System.Collections.ICollection).Count) 0
            (dT :> System.Collections.IDictionary).Add("a", 5)
            equal ((dT :> System.Collections.ICollection).Count) 1
            (dT :> System.Collections.IDictionary).Add("b", 6)
            equal ((dT :> System.Collections.ICollection).Count) 2
            equal ((dT :> System.Collections.IDictionary).Contains("c")) false
            equal ((dT :> System.Collections.IDictionary).Contains("a")) true
            (dT :> System.Collections.IDictionary).Remove("b") 
            equal ((dT :> System.Collections.ICollection).Count) 1
            (dT :> System.Collections.IDictionary).Remove("b") 
            equal ((dT :> System.Collections.ICollection).Count) 1
            (dT :> System.Collections.IDictionary).Remove("c")
            (dT :> System.Collections.IDictionary).Remove("a")
            equal ((dT :> System.Collections.ICollection).Count) 0
        }
    }
