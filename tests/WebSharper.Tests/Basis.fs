// $begin{copyright}
//
// This file is part of WebSharper
//
// Copyright (c) 2008-2014 IntelliFactory
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

module WebSharper.Tests.Basis

open WebSharper
open WebSharper.JavaScript
open WebSharper.Testing

[<JavaScript>]
let rec private fac : int -> int = function
    | 0 -> 1
    | n -> n * fac (n - 1)

[<JavaScript>]
let rec private factorial n =
    match n with
    | 0 -> 1
    | n -> n * factorial (n - 1)

[<JavaScript>]
let rec private forall f = function
    | []      -> true
    | x :: xs -> if f x then forall f xs else false

[<JavaScript>]
let private shadow bar  =
    let bar = bar
    bar

[<JavaScript>]
let rec private shadowRec bar =
    let bar = bar
    bar

module private Peano =

    type Peano = Z | S of Peano

    [<JavaScript>]
    let rec toNat = function
        | Z   -> 0
        | S x -> 1 + toNat x

    [<JavaScript>]
    let rec ofNat = function
        | 0 -> Z
        | x -> S (ofNat (x - 1))

type private T1 [<JavaScript>] () =

    [<JavaScript>]
    member this.Property = "Initial Value"

    [<JavaScript>]
    member this.Member(s:string) = "Member: " + s

    [<JavaScript>]
    static member StaticMember(s:string) = "Static Member: " + s

    [<JavaScript>]
    static member StaticMemberUnit() = ()

    [<JavaScript>]
    static member StaticMemberCurry(s:string)(s2:string) = s + s2

    [<JavaScript>]
    static member StaticProperty = "Initial Static Value"

type private T2 = { [<Name "Y">] X : int }

[<Inline "isNaN($x)">]
let private isNaN (x: double) = System.Double.IsNaN x

[<JavaScript>]
let Tests =
    Section "Basis"

    Test "Comparisons" {
        not (not true) |? "not (not true)"
        not false      |? "not false"
        not (1 <> 1)   |? "1 <> 1"
        (1 <> 2)       |? "1 <> 2"
        (1 < 2)        |? "1 < 2"
        not (1 < 1)    |? "1 < 1"
        not (1 < 0)    |? "1 < 0"
        (2 > 1)        |? "2 > 1"
        not (2 > 2)    |? "2 > 2"
        not (2 > 3)    |? "2 > 3"
        (1 <= 2)       |? "1 <= 2"
        (1 <= 1)       |? "1 <= 1"
        not (1 <= 0)   |? "1 <= 0"
        (2 >= 1)       |? "1 >= 1"
        (2 >= 2)       |? "1 >= 2"
        not (2 >= 3)   |? "2 >= 3"
    }

    let closedLet =
        let a : list<int> = List.empty
        fun () -> a

    Test "Let" {
        ([] = closedLet()) |? "[] = closedLet"
    }

    Test "Factorial" {
        (6 * 5 * 4 * 3 * 2 = fac 6)       |? "fac 6"
        (6 * 5 * 4 * 3 * 2 = factorial 6) |? "factorial 6"
    }

    let propPeano x = x = Peano.toNat (Peano.ofNat x)

    Test "forall" {
        forall (fun x -> x > 0) [1..10] |? "forall x in 1..10: x > 0"
    }

    Test "Peano" {
        propPeano 0              |? "propPeano 0"
        propPeano 1              |? "propPeano 1"
        propPeano 2              |? "propPeano 2"
        forall propPeano [3..10] |? "propPeano 3..10"
    }

    let rec fact = function
        | 0 -> 1
        | n -> n * (fact (n - 1))

    Test "Nesting" {
        (fact 6 = 720) |? "fact 6 = 720"
    }

    Test "While" {
        let i     = ref 0
        let accum = ref 0
        let _ =
            while !i <= 3 do
                accum := !accum + !i
                i := !i + 1
        !accum =? 6
    }

    Test "For" {
        let accum = ref 0
        let _ =
            for i in 0 .. 3 do
                accum := !accum + i
        !accum =? 6
    }

    Test "Floats" {
        1. <> 2.              |? "1. <> 2."
        1. = 1.               |? "1. = 1."
        3./2. = 1.5           |? "3./2. = 1.5"
        1 + 2 * 4 / 6 = 2     |? "1 + 2 * 4 / 6 = 2"
        let fEpsilon = 1.40129846e-45f
        1.f + fEpsilon =  1.f |? "1.f + \\epsilon = 1.f"
        let dEpsilon = 4.940656458e-324
        1. + dEpsilon  =  1.  |? "1. + \\epsilon = 1."
        fEpsilon <> 0.f       |? "\\epsilon <> 0.f"
        dEpsilon <> 0.        |? "\\epsilon <> 0."
    }

    Test "NaN" {
        box nan <> null |? "box nan <> null"
        let _ =
            for op in [(+); (-); (*); (/)] do
                for x in 0. .. 10. do
                    isNaN (op x nan)
                    |? ("op(x,nan) = nan where x=" + string x)
        ()
    }

    Test "Infinity" {
        box infinity <> null |? "infinity <> null"
        1./0. = infinity     |? "1./0. = infinity"
        let _ =
            for x in 0. .. 10. do
                x / infinity = 0. |? ("x/infinity = 0 where x = " + string x)
        ()
    }

    Test "Booleans" {
        (true && true)  |? "true && true"
        (true || false) |? "true || false"
    }

    Test "Ranges" {
        ([|1..5|] = [|1;2;3;4;5|]) |? "1..5"
        ([1..5] = [1;2;3;4;5]) |? "1..5"
    }

    Test "Tuples" {
        let (a, b, c) = (1, 2, 3)
        a + b + c =? 6
        let t = ("Hello ", "Szia ", "Hej")
        let (t1, t2, t3) = t
        (t1 + t2 + t3) =? "Hello Szia Hej"
    }

    Test "Currying" {
        let add (x, y) = x + y
        let add' y = add y
        add' (1, 2) =? 3
    }

    Test "Recursion" {
        let rec odd =
            function  0 -> false
                    | n -> even (n-1)
        and even =
            function  0 -> true
                    | n -> odd (n-1)
        even 12 |? "even 12"
        odd 23  |? "odd 23"
    }

    Test "Shadowing" {
        shadow 1 =? 1
        shadowRec 1 =? 1
    }

    Test "Equality" {
        (1 = 1)                       |? "1 = 1"
        (1 <> 2)                      |? "1 <> 2"
        box (1,2,3) <> box (1,(2,3))  |? "(1,2,3) <> (1,(2,3))"
        null = null                   |? "null = null"
        "" <> null                    |? "\"\" <> null"
        "Szia" = "Szia"               |? "\"Szia\" = \"Szia\""
        "Szia" <> "Hello"             |? "\"Szia\" = \"Hello\""
        [1;2;3;4] = [1..4]            |? "[1;2;3;4] = [1;2;3;4]"
        Some 3 = Some 3               |? "Some 3 = Some 3"
        [|1;2;3;4|] = [|1..4|]        |? "[|1;2;3;4|] = [|1;2;3;4|]"
        [|1;2;3|] <> [|1..4|]         |? "[|1;2;3|] <> [|1;2;3;4|]"
        [1;2;3] <> [1..4]             |? "[1;2;3] <> [1;2;3;4]"
        (1,2,3) = (1,2,3)             |? "(1,2,3) = (1,2,3)"
        forall (fun x -> x=x) [0..10] |? "forall x in 0..10, x = x"
        (1,"b",3) <> (1,"a",3)        |? "(1,\"b\",3) <> (1,\"a\",3)"
        box 0 <> box ""               |? "0 <> \"\""
        box 0 <> box "0"              |? "0 <> \"0\""
        box 0 <> box false            |? "0 <> false"
        box 0 <> box JS.Undefined     |? "0 <> undefined"
        box false <> box JS.Undefined |? "false <> undefined"
        null <> box JS.Undefined      |? "null <> undefined"
        box " \t\r\n" <> box 0        |? "whitespace <> 0"
    }

    Test "Objects" {
        let t = T1()
        t.Member "X" = "Member: X"                 |? "t1.Member"
        t.Property   = "Initial Value"             |? "t1.Property"
        T1.StaticMember "X" = "Static Member: X"   |? "T1.StaticMember"
        T1.StaticProperty = "Initial Static Value" |? "T1.StaticProperty"
        T1.StaticMemberCurry "x" "y" = "xy"        |? "T1.StaticMemberCurry"
    }

    Test "Renaming" {
        {X=1}?Y =? 1
    }

    Test "JavaScript object" {
        let o = New [ "a", box "1"; "b", box 2 ]
        (o?a = "1" && o?b = 2) |? "List of tuples"
        let o2 = [ "a", 1; "b", 2 ] |> List.map (fun (n, v) -> n, box (v + 1)) |> New
        (o2?a = 2 && o2?b = 3) |? "Mapped list of tuples"
        let o3 = New [| "a", box "1"; "b", box 2 |]
        (o3?a = "1" && o3?b = 2) |? "Array of tuples"
        let o4 = New (seq { yield "a", box "1"; yield "b", box 2; })
        (o4?a = "1" && o4?b = 2) |? "Sequence of tuples"
        let o5 = New [ "a" => "1"; "b" => 2 ]
        (o5?a = "1" && o5?b = 2) |? "List of (=>) calls"
        let o6 = [ "a", 1; "b", 2 ] |> List.map (fun (n, v) -> n, box (v + 1)) |> New
        (o6?a = 2 && o6?b = 3) |? "Mapped list of (=>) calls"
        let o7 = New [| "a" => "1"; "b" => 2 |]
        (o7?a = "1" && o7?b = 2) |? "Array of (=>) calls"
        let o8 = New (seq { yield "a" => "1"; yield "b" => 2; })
        (o8?a = "1" && o8?b = 2) |? "Sequence of (=>) calls"
    }


//module CoreTests =
//    open WebSharper
//    open WebSharper.Testing
//
//    [<JavaScript>]
//    let TestIntEquality() =
//        Assert.For 100 Random.Int (fun x -> x = x)
//
//    [<JavaScript>]
//    let TestIntListEquality() =
//        Assert.For 100 (Random.ListOf Random.Int) (fun x -> x = x)
//
//    [<JavaScript>]
//    let TestIntArrayEquality() =
//        Assert.For 100 (Random.ArrayOf Random.Int) (fun x -> x = x)
//
//    [<JavaScript>]
//    let TestStringIntConversion() =
//        Assert.For 100 Random.Int (fun x -> int (string x) = x)
