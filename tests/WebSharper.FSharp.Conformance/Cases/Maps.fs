// $begin{copyright}
//
// This file is part of WebSharper
//
// Copyright (c) 2008-2026 IntelliFactory
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
namespace Conformance

open System
open System.Collections.Generic
open WebSharper
open Conformance.H

[<JavaScript>]
module Maps =
    let private m () = Map.ofList [ 3, "c"; 1, "a"; 2, "b" ]
    let private kv (k: 'K, v: string) = string k + "=" + v

    let Cases () = [
        C "map.sorted.keys" (fun () -> J (m () |> Map.toList |> List.map kv))
        C "map.string.keys" (fun () -> J (Map.ofList [ "b", "1"; "B", "2"; "a", "3" ] |> Map.toList |> List.map kv))
        C "map.find" (fun () -> Map.find 2 (m ()) + "," + (m ()).[3])
        C "map.find.missing" (fun () -> Map.find 9 (m ()))
        C "map.tryfind" (fun () -> string (Map.tryFind 9 (m ())) + "," + string (Map.tryFind 1 (m ())))
        C "map.add.replace" (fun () -> J (m () |> Map.add 1 "z" |> Map.add 0 "y" |> Map.toList |> List.map kv))
        C "map.remove" (fun () -> J (m () |> Map.remove 2 |> Map.remove 9 |> Map.toList |> List.map kv))
        C "map.count" (fun () -> string (Map.count (m ())) + "," + string (m ()).Count + "," + B (Map.isEmpty Map.empty))
        C "map.containskey" (fun () -> B (Map.containsKey 1 (m ())) + "," + B ((m ()).ContainsKey 7))
        C "map.fold" (fun () -> Map.fold (fun s k v -> s + string k + v) "" (m ()))
        C "map.foldback" (fun () -> Map.foldBack (fun k v s -> s + string k + v) (m ()) "")
        C "map.map.filter" (fun () -> J (m () |> Map.filter (fun k _ -> k > 1) |> Map.map (fun k v -> v + v) |> Map.toList |> List.map kv))
        C "map.change" (fun () -> J (m () |> Map.change 1 (Option.map (fun v -> v + "!")) |> Map.change 5 (fun _ -> Some "e") |> Map.toList |> List.map kv))
        C "map.equality" (fun () -> B (Map.ofList [ 1, "a"; 2, "b" ] = Map.ofList [ 2, "b"; 1, "a" ]))
        C "map.compare" (fun () -> string (compare (Map.ofList [ 1, "a" ]) (Map.ofList [ 1, "b" ])))
        C "map.tuple.keys" (fun () -> J (Map.ofList [ (2, "a"), "x"; (1, "b"), "y"; (1, "a"), "z" ] |> Map.toList |> List.map snd))
        C "map.record.keys" (fun () -> string (Map.count (Map.ofList [ Some 1, "a"; Some 1, "b"; None, "c" ])))
        C "map.minkey" (fun () -> string (Map.minKeyValue (m ()) |> fst) + "," + string (Map.maxKeyValue (m ()) |> fst))
        C "map.keys.values" (fun () -> Ji (m ()).Keys + J (m ()).Values)
        C "map.partition" (fun () -> let a, b = Map.partition (fun k _ -> k % 2 = 1) (m ()) in string a.Count + string b.Count)
        C "map.findkey" (fun () -> string (Map.findKey (fun _ v -> v = "b") (m ())))
        C "map.iterate" (fun () -> let mutable s = "" in (for KeyValue (k, v) in m () do s <- s + string k + v); s)
        C "set.sorted" (fun () -> Ji (Set.ofList [ 3; 1; 2; 3 ]))
        C "set.ops" (fun () -> let a, b = set [ 1; 2; 3 ], set [ 2; 3; 4 ] in Ji (Set.union a b) + Ji (Set.intersect a b) + Ji (Set.difference a b))
        C "set.subset" (fun () -> B (Set.isSubset (set [ 1 ]) (set [ 1; 2 ])) + "," + B (Set.isProperSuperset (set [ 1 ]) (set [ 1 ])))
        C "set.contains" (fun () -> B (Set.contains 2 (set [ 1; 2 ])) + "," + string (Set.count (set [ "a"; "a" ])))
        C "set.equality" (fun () -> B (set [ 1; 2 ] = set [ 2; 1 ]) + "," + string (compare (set [ 1; 2 ]) (set [ 1; 3 ])))
        C "set.minmax" (fun () -> string (Set.minElement (set [ 5; 2; 8 ])) + "," + string (Set.maxElement (set [ 5; 2; 8 ])))
        C "set.tuples" (fun () -> J (Set.ofList [ (2, "a"); (1, "b"); (1, "a") ] |> Seq.map (fun (a, b) -> string a + b)))
        C "set.remove" (fun () -> Ji (Set.remove 2 (set [ 1; 2; 3 ])))
        C "dictionary.order" (fun () -> let d = Dictionary<string, int>() in d.["b"] <- 1; d.["a"] <- 2; d.["c"] <- 3; J (d.Keys |> Seq.map id))
        C "dictionary.missing" (fun () -> let d = Dictionary<int, int>() in string d.[5])
        C "dictionary.add.duplicate" (fun () -> let d = Dictionary<int, int>() in d.Add(1, 1); d.Add(1, 2); "added")
        C "dictionary.trygetvalue" (fun () -> let d = dict [ 1, "a" ] in match d.TryGetValue 1 with | true, v -> v | _ -> "none")
        C "dictionary.trygetvalue.missing" (fun () -> let d = Dictionary<int, string>() in match d.TryGetValue 1 with | true, v -> v | _, v -> "none:" + string (isNull v))
        C "dictionary.remove" (fun () -> let d = Dictionary<int, int>() in d.[1] <- 1; B (d.Remove 1) + B (d.Remove 1) + string d.Count)
        C "dictionary.tuple.key" (fun () -> let d = Dictionary<int * string, int>() in d.[(1, "a")] <- 5; string d.[(1, "a")])
        C "dictionary.union.key" (fun () -> let d = Dictionary<int option, int>() in d.[Some 1] <- 5; d.[None] <- 6; string d.[Some 1] + string d.[None])
        C "dictionary.readonly.dict" (fun () -> let d = dict [ 1, "a" ] in try d.[2] <- "b"; "set" with e -> "EXN")
        C "dictionary.kvp" (fun () -> J (Dictionary(dict [ 1, "x"; 2, "y" ]) |> Seq.map (fun kv -> string kv.Key + kv.Value)))
        C "readonlydict" (fun () -> let d = readOnlyDict [ "a", 1 ] in string d.["a"] + "," + string d.Count)
        C "hashset" (fun () -> let h = HashSet<int>([ 3; 1; 3 ]) in B (h.Add 1) + B (h.Add 2) + string h.Count + Ji h)
        C "hashset.ops" (fun () -> let h = HashSet<int>([ 1; 2; 3 ]) in h.IntersectWith [ 2; 3; 4 ]; Ji h)
        C "hashset.tuples" (fun () -> let h = HashSet<int * int>() in h.Add((1, 2)) |> ignore; B (h.Contains((1, 2))))
        C "resizearray" (fun () -> let r = ResizeArray [ 1; 2 ] in r.Add 3; r.Insert(0, 0); r.RemoveAt 1; Ji r + string r.Count)
        C "resizearray.out.of.range" (fun () -> let r = ResizeArray [ 1 ] in string r.[3])
        C "resizearray.sort" (fun () -> let r = ResizeArray [ 3; 1; 2 ] in r.Sort(); Ji r)
        C "resizearray.modify.during.enum" (fun () -> let r = ResizeArray [ 1; 2 ] in (for x in r do if x = 1 then r.Add 3); string r.Count)
        C "queue.stack" (fun () -> let q = Queue<int>([ 1; 2 ]) in let s = Stack<int>([ 1; 2 ]) in string (q.Dequeue()) + string (s.Pop()))
        C "queue.empty" (fun () -> let q = Queue<int>() in string (q.Dequeue()))
    ]
