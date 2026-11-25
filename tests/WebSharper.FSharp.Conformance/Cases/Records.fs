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
type Person = { Name: string; Age: int }

[<JavaScript>]
type Shape =
    | Circle of radius: float
    | Rect of w: float * h: float
    | Empty

[<JavaScript>]
[<CustomEquality; NoComparison>]
type Loose =
    { Key: string; Note: string }
    override this.Equals(o) = match o with :? Loose as l -> l.Key = this.Key | _ -> false
    override this.GetHashCode() = hash this.Key

[<JavaScript>]
[<Struct>]
type SPoint = { X: int; Y: int }

[<JavaScript>]
[<Struct>]
type SUnion = SA of a: int | SB of b: string

[<JavaScript>]
type Tree = Leaf | Node of Tree * int * Tree

[<JavaScript>]
type Mutable = { mutable Count: int }

[<JavaScript>]
type Anon = { Inner: {| A: int; B: string |} }

[<JavaScript>]
type Color = Red = 1 | Green = 2 | Blue = 4

[<JavaScript; System.Flags>]
type Perm = None = 0 | Read = 1 | Write = 2 | Exec = 4

[<JavaScript>]
type Wrapper = Wrapper of int

[<JavaScript>]
module Records =
    let private p n a = { Name = n; Age = a }
    let rec private sum t = match t with Leaf -> 0 | Node (l, v, r) -> sum l + v + sum r

    let Cases () = [
        C "record.equality" (fun () -> B (p "a" 1 = p "a" 1) + "," + B (p "a" 1 = p "a" 2) + "," + B (obj.ReferenceEquals(p "a" 1, p "a" 1)))
        C "record.comparison" (fun () -> string (compare (p "a" 2) (p "b" 1)) + "," + string (compare (p "a" 2) (p "a" 1)))
        C "record.sort" (fun () -> J ([ p "b" 1; p "a" 2; p "a" 1 ] |> List.sort |> List.map (fun x -> x.Name + string x.Age)))
        C "record.hash" (fun () -> B (hash (p "a" 1) = hash (p "a" 1)))
        C "record.copy.update" (fun () -> let a = p "a" 1 in let b = { a with Age = 5 } in string a.Age + string b.Age)
        C "record.distinct" (fun () -> string (List.length (List.distinct [ p "a" 1; p "a" 1; p "b" 1 ])))
        C "record.set" (fun () -> string (Set.count (set [ p "a" 1; p "a" 1 ])))
        C "record.dictionary.key" (fun () -> let d = Dictionary<Person, int>() in d.[p "a" 1] <- 1; d.[p "a" 1] <- 2; string d.Count + string d.[p "a" 1])
        C "record.hashset" (fun () -> let h = HashSet<Person>() in h.Add(p "a" 1) |> ignore; B (h.Add(p "a" 1)))
        C "record.custom.equality" (fun () -> B ({ Key = "k"; Note = "1" } = { Key = "k"; Note = "2" }))
        C "record.custom.dictionary" (fun () -> let d = Dictionary<Loose, int>() in d.[{ Key = "k"; Note = "1" }] <- 1; string d.[{ Key = "k"; Note = "2" }])
        C "record.mutable.field" (fun () -> let m = { Count = 0 } in m.Count <- m.Count + 2; string m.Count)
        C "record.mutable.alias" (fun () -> let m = { Count = 0 } in let n = m in n.Count <- 3; string m.Count)
        C "record.struct.copy" (fun () -> let a = { X = 1; Y = 2 } in let b = { a with X = 5 } in string a.X + string b.X + B (a = { X = 1; Y = 2 }))
        C "record.struct.compare" (fun () -> string (compare { X = 1; Y = 2 } { X = 1; Y = 3 }))
        C "record.anonymous" (fun () -> let a = {| A = 1; B = "x" |} in string a.A + a.B + B (a = {| A = 1; B = "x" |}))
        C "record.anonymous.copy" (fun () -> let a = {| A = 1; B = "x" |} in let b = {| a with A = 2 |} in string b.A + b.B)
        C "record.anonymous.nested" (fun () -> let x = { Inner = {| A = 1; B = "b" |} } in x.Inner.B)
        C "union.equality" (fun () -> B (Circle 1.0 = Circle 1.0) + "," + B (Circle 1.0 = Rect (1.0, 1.0)) + "," + B (Empty = Empty))
        C "union.comparison" (fun () -> string (sign (compare (Circle 5.0) (Rect (1.0, 1.0)))) + "," + string (sign (compare Empty (Circle 0.0))) + "," + string (sign (compare (Rect (1.0, 2.0)) (Rect (1.0, 1.0)))))
        C "union.sort" (fun () -> string (List.sort [ Empty; Rect (1.0, 1.0); Circle 2.0; Circle 1.0 ] |> List.map (function Circle r -> "C" + string r | Rect _ -> "R" | Empty -> "E") |> String.concat ""))
        C "union.hash" (fun () -> B (hash (Rect (1.0, 2.0)) = hash (Rect (1.0, 2.0))))
        C "union.match" (fun () -> J ([ Circle 1.0; Rect (2.0, 3.0); Empty ] |> List.map (function Circle r -> string (r * 2.0) | Rect (w, h) -> string (w * h) | Empty -> "-")))
        C "union.named.fields" (fun () -> match Rect (h = 2.0, w = 3.0) with Rect (w = w) -> string w | _ -> "?")
        C "union.recursive" (fun () -> string (sum (Node (Node (Leaf, 1, Leaf), 2, Node (Leaf, 3, Leaf)))))
        C "union.struct" (fun () -> B (SA 1 = SA 1) + "," + string (compare (SA 5) (SB "a")) + "," + (match SB "z" with SB b -> b | SA _ -> "?"))
        C "union.single.case" (fun () -> let (Wrapper w) = Wrapper 5 in string w + B (Wrapper 1 < Wrapper 2))
        C "union.isCase" (fun () -> B (Circle 1.0).IsCircle + "," + B Empty.IsRect)
        C "union.map.key" (fun () -> let m = Map.ofList [ Rect (1.0, 1.0), "r"; Empty, "e"; Circle 1.0, "c" ] in J (Map.toList m |> List.map snd))
        C "option.equality" (fun () -> B (Some 1 = Some 1) + "," + B (None = (None: int option)) + "," + string (compare None (Some 1)))
        C "option.functions" (fun () -> string (Option.map ((+) 1) (Some 1)) + "," + string (Option.defaultValue 9 None) + "," + string (Option.bind (fun x -> if x > 0 then Some x else None) (Some -1)))
        C "option.value.none" (fun () -> string (Option.get (None: int option)))
        C "option.value.prop" (fun () -> string (None: int option).Value)
        C "option.isnone" (fun () -> B (Option.isNone None) + "," + B (Some 1).IsSome)
        C "option.tolist" (fun () -> Ji (Option.toList (Some 3)) + "," + string (Option.count None))
        C "option.null" (fun () -> B (obj.ReferenceEquals(None, null)))
        C "option.ofobj" (fun () -> string (Option.ofObj (null: string)) + "," + string (Option.ofObj "x"))
        C "voption" (fun () -> let v = ValueSome 3 in string (ValueOption.defaultValue 0 v) + B (ValueNone = (ValueNone: int voption)))
        C "result" (fun () -> let r : Result<int, string> = Ok 1 in string (Result.map ((+) 1) r) + "," + string (Result.bind (fun _ -> Error "e") r : Result<int, string>))
        C "result.match" (fun () -> match (Error "bad" : Result<int, string>) with Ok v -> string v | Error e -> e)
        C "result.equality" (fun () -> B ((Ok 1 : Result<int, string>) = Ok 1) + "," + string (compare (Ok 1 : Result<int, int>) (Error 0)))
        C "tuple.equality" (fun () -> B ((1, "a") = (1, "a")) + "," + string (compare (1, "b") (1, "a")) + "," + string (compare (2, "a") (10, "a")))
        C "tuple.struct" (fun () -> let struct (a, b) = struct (1, "x") in string a + b)
        C "tuple.fst.snd" (fun () -> string (fst (1, 2)) + string (snd (1, 2)))
        C "enum.tostring" (fun () -> string Color.Green + "," + string (int Color.Blue))
        C "enum.flags.tostring" (fun () -> string (Color.Red ||| Color.Blue))
        C "enum.of.int" (fun () -> string (enum<Color> 2) + "," + string (enum<Color> 3))
        C "enum.tostring.method" (fun () -> Color.Blue.ToString() + "|" + string (enum<Color> 42) + "|" + string DayOfWeek.Sunday)
        C "enum.flags.names" (fun () -> string (Perm.Read ||| Perm.Write) + "|" + string Perm.None + "|" + (Perm.Exec).ToString() + "|" + string (enum<Perm> 8))
        C "enum.hasflag" (fun () -> B ((Color.Red ||| Color.Blue).HasFlag Color.Blue))
        C "enum.compare" (fun () -> B (Color.Red < Color.Blue) + "," + string (compare Color.Blue Color.Green))
#if !JS
        C "enum.parse" (fun () -> string (Enum.Parse(typeof<Color>, "Blue") :?> Color))
#else
        C "enum.parse" (fun () -> "WS-COMPILE-ERROR")
#endif
        C "unit.equality" (fun () -> B (() = ()))
        C "structural.nested" (fun () -> B ([ Some (p "a" 1, [| 1 |]) ] = [ Some (p "a" 1, [| 1 |]) ]))
        C "physical.equality" (fun () -> let a = p "a" 1 in B (LanguagePrimitives.PhysicalEquality a a) + "," + B (LanguagePrimitives.PhysicalEquality a (p "a" 1)))
        C "compare.mixed.strings" (fun () -> J (List.sort [ "a"; "B"; "_"; "1"; "é" ]))
        C "compare.floats.nan.sort" (fun () -> J (List.sort [ 2.0; nan; 1.0 ] |> List.map string))
        C "compare.char" (fun () -> string (compare 'a' 'B'))
        C "hash.string.stable" (fun () -> B (hash "abc" = hash ("ab" + "c")))
        C "hash.int" (fun () -> string (hash 42) + "," + string (hash -1))
        C "equals.boxed" (fun () -> B ((box 1).Equals(box 1)) + "," + B ((box 1).Equals(box 1L)) + "," + B (box 1 = box 1.0))
    ]
