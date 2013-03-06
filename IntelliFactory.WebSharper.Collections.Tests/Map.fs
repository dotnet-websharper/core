// $begin{copyright}
//
// This file is part of WebSharper
//
// Copyright (c) 2008-2013 IntelliFactory
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

module IntelliFactory.WebSharper.Collections.Tests.Map

open System
open IntelliFactory.WebSharper
open IntelliFactory.WebSharper.Testing

[<JavaScript>]
let Tests =

    Section "Map"

    Test "Map.add" {
        Assert.Equals false <| Map.isEmpty (Map.add 1 2 Map.empty)
    }

    Test "Map.remove" {
        let map2 = Map.ofSeq [(1,2); (3,4)]
        let map1 = Map.remove 1 map2 // remove existing element 
        Assert.Equals 1 map1.Count
        let map1 = Map.remove 100 map1 // remove non-existing element
        Assert.Equals 1 map1.Count
        let map0 = Map.remove 3 map1 // remove existing element
        Assert.Equals 0 map0.Count
    }

    Test "Map.empty" {
        Assert.Equals true <| Map.isEmpty Map.empty
    }

    Test "Map.exists" {
        let m = Map.ofList [(1, 5); (2, 9); (3, 6); (4, 13)]
        Assert.True (not <| Map.exists (fun x y -> x = 7 && y = 6) m)
        Assert.True (not <| Map.exists (fun _ _ -> true) Map.empty)
        Assert.True (Map.exists (fun x y -> x = 3 && y = 6) m)
    }

    Test "Map/array consistency" {
        let a = [| (1, 5); (2, 9); (3, 6); (4, 13) |]
        Assert.Equals a (Map.toArray <| Map.ofArray a)
        let g = Random.ArrayOf (Random.Tuple2Of (Random.Int, Random.Const ()))
        Assert.For 100 g (fun x ->
            let x1 = x |> Map.ofArray |> Map.toArray
            let x2 = x |> Set.ofArray |> Set.toArray
            x1 =? x2)
    }

    Test "Map.filter" {
        let even x = x % 2 = 0
        let trueF (x: int) = true
        let falseF (x: int) = false
        let ltF x = x < 10
        let check (xs: (int * unit) []) (f : int -> bool) =
            let xs1 =
                Map.ofArray xs |> Map.filter (fun k _ -> f k) |> Map.toArray
            let xs2 =
                xs
                |> Array.sort
                |> Seq.distinct
                |> Seq.filter (fun (k, _) -> f k)
                |> Seq.toArray
            Assert.Equals xs2 xs1
        Assert.For 100
            (Random.ArrayOf (Random.Tuple2Of (Random.Int, Random.Const ())))
            (fun xs ->
                check xs even
                check xs trueF
                check xs falseF
                check xs ltF)
    }

    Test "Map.find" {
        let m = Map.ofList [(1, 5); (2, 9); (3, 6); (4, 13); (5, 67); (6, 6); (7, 8); (8, 9)]
        Assert.Equals 5 <| Map.find 1 m
        Assert.Equals 9 <| Map.find 8 m
        Assert.Equals 6 <| Map.find 3 m
        Assert.True <| try Map.empty |> Map.find 4 |> ignore; false with | _ -> true
    }

    Test "Map.findKey" {
        let m = Map.ofList [(1, 5); (2, 9); (3, 6); (4, 13); (5, 4); (6, 9)]
        Assert.Equals 6 <| Map.findKey (fun k v -> k = 6 && v = 9) m
        Assert.Equals 4 <| Map.findKey (fun x y -> y = 13) m
        Assert.True <|
            try
                let k = Map.findKey (fun x y -> y = 100) m
                false
            with | _ -> true
    }

    Test "Map.fold" {
        let m = Map.ofList [(1, 5); (2, 9); (3, 6); (4, 13); (5, 5); (6, 9)]
        let f x y z = x || (y % 2 = 0 && z % 2 = 0)
        Assert.Equals  68 <| Map.fold (fun x y z -> x + y + z) 0 m
        Assert.Equals false <| Map.fold f false m
        Assert.Equals true <| Map.fold f true m
        Assert.Equals 0 <| Map.fold (fun x _ _ -> x) 0 m
    }

    Test "Map.fold/foldBack" {
        Assert.For 100 (Random.ListOf (Random.Tuple2Of (Random.Int, Random.Int))) (fun x ->
            let f = fun y z x -> x + y + z
            Map.fold f 0 (Map.ofList x) =?
                Map.foldBack f (Map.ofList x) 0)
    }

    Test "Map.foldBack" {
        let m = Map.ofList [(1, 5); (2, 9); (3, 6); (4, 13); (5, 5); (6, 9)]
        let f y z x = x || (y % 2 = 0 && z % 2 = 0)
        Assert.Equals  68 <| Map.foldBack (fun y z x -> x + y + z) m 0
        Assert.Equals false <| Map.foldBack f m false
        Assert.True <| Map.foldBack f m true
        Assert.Equals 0 <| Map.foldBack (fun _ _ x -> x) m 0
    }

    Test "Map.forall" {
        let m = Map.ofList [(1, 5); (2, 9); (3, 6); (4, 13); (5, 5); (6, 9)]
        Assert.True (Map.forall (fun x y -> x = 6 && y = 9) m |> not)
        Assert.Equals false <| Map.forall (fun x y -> y = 13) m
        Assert.True (Map.forall (<>) m |> not)
        Assert.True (Map.forall (<>) Map.empty)
    }

    Test "Map.isEmpty" {
        let emp = Map.remove 1 <| Map.add 1 2 Map.empty
        Assert.Equals true <| Map.isEmpty Map.empty
        Assert.Equals false <| Map.isEmpty (Map.add 1 2 Map.empty)
        Assert.Equals true <| Map.isEmpty emp
    }

    Test "Map.partition" {
        Assert.For 100 (Random.ListOf (Random.Tuple2Of (Random.Int, Random.Const ()))) (
            fun xs ->
                let xs = xs |> Set.ofList |> Set.toList
                let f (k : int) () = k % 2 = 0
                let (a, b) =
                    List.partition (fun (k, _) -> k % 2 = 0) xs
                let (x, y) =
                    Map.partition (fun k _ -> k % 2 = 0) (Map.ofList xs)
                Map.ofList a =? x
                Map.ofList b =? y
        )
    }

    Test "Map.pick" {
        let m = Map.ofList [(1, 5); (2, 9); (3, 6); (4, 13); (5, 4); (6, 9)]
        let finder x y = if x = 5 && y = 4 then Some x else None
        Assert.Equals 5 (Map.pick finder m)
    }

    Test "Map.tryFind" {
        let findList = [None; None; Some 4; None]
        let m = Map.ofList [(1, 5); (2, 9); (3, 6); (4, 13); (5, 5); (6, 9)]
        let wasFound = Map.tryFind 2 m
        let notFound = Map.tryFind 8 m
        Assert.Equals (Some 9) wasFound
        Assert.Equals None notFound
    }

    Test "Map.tryPick" {
        let m = Map.ofList [(1, 5); (2, 9); (3, 6); (4, 13); (5, 4); (6, 9)]
        let finder x y =
            if x = 5 && y = 4 then
                Some "foo"
            else
                None
        Assert.Equals (Some "foo") (Map.tryPick finder m)
        Assert.Equals None (Map.tryPick finder (Map.remove 5 m))
    }
