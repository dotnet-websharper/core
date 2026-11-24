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
module Patterns =
    let private (|Even|Odd|) (x: int) = if x % 2 = 0 then Even else Odd
    let private (|Int|_|) (s: string) = match Int32.TryParse s with | true, v -> Some v | _ -> None
    [<return: Struct>]
    let private (|Positive|_|) (x: int) = if x > 0 then ValueSome x else ValueNone
    let private (|DivisibleBy|_|) (d: int) (x: int) = if x % d = 0 then Some (x / d) else None
    let private (|Lower|) (s: string) = s.ToLower()
    let private classify (o: obj) =
        match o with
        | :? int as i -> "int " + string i
        | :? string as s -> "string " + s
        | :? (int list) as l -> "list " + string (List.length l)
        | null -> "null"
        | _ -> "other"
    let private fizz x =
        match x with
        | DivisibleBy 15 _ -> "FizzBuzz"
        | DivisibleBy 3 _ -> "Fizz"
        | DivisibleBy 5 _ -> "Buzz"
        | _ -> string x
    let private n (x: int) = x

    let Cases () = [
        C "match.constant" (fun () -> match n 2 with 1 -> "one" | 2 -> "two" | _ -> "many")
        C "match.guard" (fun () -> match n 5 with x when x > 3 -> "big" | _ -> "small")
        C "match.or" (fun () -> match n 3 with 1 | 3 | 5 -> "odd" | _ -> "?")
        C "match.and" (fun () -> match (1, 2) with (a, _) & (_, b) -> string (a + b))
        C "match.as" (fun () -> match [ 1; 2 ] with (x :: _) as l -> string x + string l.Length | [] -> "")
        C "match.list" (fun () -> J ([ []; [ 1 ]; [ 1; 2 ]; [ 1; 2; 3 ] ] |> List.map (function [] -> "e" | [ a ] -> "1" | [ a; b ] -> "2" | _ :: _ :: rest -> "n" + string rest.Length)))
        C "match.array" (fun () -> match [| 1; 2 |] with [| a; b |] -> string (a + b) | _ -> "?")
        C "match.tuple" (fun () -> match (n 1, "x") with (1, s) -> s | _ -> "?")
        C "match.record" (fun () -> match { Name = "a"; Age = 3 } with { Age = 3; Name = nm } -> nm | _ -> "?")
        C "match.option.nested" (fun () -> match Some (Some 3) with Some (Some x) -> string x | Some None -> "sn" | None -> "n")
        C "match.string" (fun () -> match "b" + "c" with "bc" -> "yes" | _ -> "no")
        C "match.char.range" (fun () -> match 'q' with c when c >= 'a' && c <= 'z' -> "lower" | _ -> "?")
        C "match.null" (fun () -> match (null: string) with null -> "null" | s -> s)
        C "match.typetest" (fun () -> J ([ box 1; box "s"; box [ 1; 2 ]; null; box 1.5 ] |> List.map classify))
        C "match.typetest.float.int" (fun () -> match box 2.0 with :? int -> "int" | :? float -> "float" | _ -> "?")
        C "match.typetest.int64" (fun () -> match box 2L with :? int -> "int" | :? int64 -> "int64" | _ -> "?")
        C "match.typetest.char.string" (fun () -> match box 'c' with :? string -> "string" | :? char -> "char" | _ -> "?")
        C "match.incomplete" (fun () -> let f x = match x with 1 -> "one" in f (n 2))
        C "match.active.total" (fun () -> J ([ 1; 2 ] |> List.map (function Even -> "even" | Odd -> "odd")))
        C "match.active.partial" (fun () -> match "42" with Int v -> string (v + 1) | _ -> "nan")
        C "match.active.struct" (fun () -> match n 5 with Positive v -> string v | _ -> "no")
        C "match.active.param" (fun () -> J (List.map fizz [ 3; 5; 15; 7 ]))
        C "match.active.single" (fun () -> let (Lower s) = "ABC" in s)
        C "match.function" (fun () -> let f = function Some x -> x | None -> 0 in string (f (Some 4) + f None))
        C "match.let.tuple" (fun () -> let a, (b, c) = 1, (2, 3) in string (a + b + c))
        C "match.wildcard.lambda" (fun () -> Ji (List.map (fun (_, b) -> b) [ (1, 2); (3, 4) ]))
        C "match.cons.string.list" (fun () -> match [ "a"; "b" ] with h :: t -> h + string t.Length | [] -> "")
        C "match.enum" (fun () -> match Color.Green with Color.Red -> "r" | Color.Green -> "g" | _ -> "?")
        C "match.enum.undefined" (fun () -> match enum<Color> 7 with Color.Red -> "r" | _ -> "other")
        C "match.union.nested" (fun () -> match Node (Leaf, 1, Node (Leaf, 2, Leaf)) with Node (_, _, Node (_, v, _)) -> string v | _ -> "?")
        C "match.exception" (fun () -> try raise (ArgumentException "x") with :? ArgumentException as e -> "arg" | _ -> "other")
        C "match.kvp" (fun () -> match KeyValuePair(1, "a") with KeyValue (k, v) -> string k + v)
    ]
