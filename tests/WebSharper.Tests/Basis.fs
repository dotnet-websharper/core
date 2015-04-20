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
        TrueM (not (not true))  "not (not true)"
        TrueM (not false)       "not false"
        TrueM (not (1 <> 1))    "1 <> 1"
        TrueM (1 <> 2)          "1 <> 2"
        TrueM (1 < 2)           "1 < 2"
        TrueM (not (1 < 1))     "1 < 1"
        TrueM (not (1 < 0))     "1 < 0"
        TrueM (2 > 1)           "2 > 1"
        TrueM (not (2 > 2))     "2 > 2"
        TrueM (not (2 > 3))     "2 > 3"
        TrueM (1 <= 2)          "1 <= 2"
        TrueM (1 <= 1)          "1 <= 1"
        TrueM (not (1 <= 0))    "1 <= 0"
        TrueM (2 >= 1)          "1 >= 1"
        TrueM (2 >= 2)          "1 >= 2"
        TrueM (not (2 >= 3))    "2 >= 3"
    }

    let closedLet =
        let a : list<int> = List.empty
        fun () -> a

    Test "Let" {
        EqualM [] (closedLet()) "[] = closedLet"
    }

    Test "Factorial" {
        EqualM (6 * 5 * 4 * 3 * 2) (fac 6)       "fac 6"
        EqualM (6 * 5 * 4 * 3 * 2) (factorial 6) "factorial 6"
    }

    let propPeano x = x = Peano.toNat (Peano.ofNat x)

    Test "forall" {
        TrueM (forall (fun x -> x > 0) [1..10]) "forall x in 1..10: x > 0"
    }

    Test "Peano" {
        TrueM (propPeano 0)              "propPeano 0"
        TrueM (propPeano 1)              "propPeano 1"
        TrueM (propPeano 2)              "propPeano 2"
        TrueM (forall propPeano [3..10]) "propPeano 3..10"
    }

    let rec fact = function
        | 0 -> 1
        | n -> n * (fact (n - 1))

    Test "Nesting" {
        EqualM (fact 6) 720 "fact 6 = 720"
    }

    Test "While" {
        let i     = ref 0
        let accum = ref 0
        let _ =
            while !i <= 3 do
                accum := !accum + !i
                i := !i + 1
        Equal !accum 6
    }

    Test "For" {
        let accum = ref 0
        let _ =
            for i in 0 .. 3 do
                accum := !accum + i
        Equal !accum 6
    }

    Test "Floats" {
        NotEqualM 1. 2.             "1. <> 2."
        EqualM 1. 1.                "1. = 1."
        EqualM (3./2.) 1.5          "3./2. = 1.5"
        EqualM (1 + 2 * 4 / 6) 2    "1 + 2 * 4 / 6 = 2"
        let fEpsilon = 1.40129846e-45f
        EqualM (1.f + fEpsilon) 1.f "1.f + \\epsilon = 1.f"
        let dEpsilon = 4.940656458e-324
        EqualM (1. + dEpsilon) 1.   "1. + \\epsilon = 1."
        NotEqualM fEpsilon 0.f      "\\epsilon <> 0.f"
        NotEqualM dEpsilon 0.       "\\epsilon <> 0."
    }

    Test "NaN" {
        NotEqualM (box nan) null "box nan <> null"
        For [(+); (-); (*); (/)] (fun op -> Do {
            For { 0. .. 10. } (fun x -> Do {
                TrueM (isNaN (op x nan)) ("op(x,nan) = nan where x=" + string x)
            })
        })
    }

    Test "Infinity" {
        NotEqualM (box infinity) null   "infinity <> null"
        EqualM (1./0.) infinity         "1./0. = infinity"
        For { 0. .. 10. } (fun x -> Do {
            EqualM (x / infinity) 0. ("x/infinity = 0 where x = " + string x)
        })
    }

    Test "Booleans" {
        TrueM (true && true)  "true && true"
        TrueM (true || false) "true || false"
    }

    Test "Ranges" {
        EqualM [|1..5|] [|1;2;3;4;5|] "1..5"
        EqualM [1..5] [1;2;3;4;5] "1..5"
    }

    Test "Tuples" {
        let (a, b, c) = (1, 2, 3)
        Equal (a + b + c) 6
        let t = ("Hello ", "Szia ", "Hej")
        let (t1, t2, t3) = t
        Equal (t1 + t2 + t3) "Hello Szia Hej"
    }

    Test "Currying" {
        let add (x, y) = x + y
        let add' y = add y
        Equal (add' (1, 2)) 3
    }

    let rec odd =
        function  0 -> false
                | n -> even (n-1)
    and even =
        function  0 -> true
                | n -> odd (n-1)

    Test "Recursion" {
        TrueM (even 12) "even 12"
        TrueM (odd 23) "odd 23"
    }

    Test "Shadowing" {
        Equal (shadow 1) 1
        Equal (shadowRec 1) 1
    }

    Test "Equality" {
        TrueM (1 = 1)                         "1 = 1"
        TrueM (1 <> 2)                        "1 <> 2"
        TrueM (box (1,2,3) <> box (1,(2,3)))  "(1,2,3) <> (1,(2,3))"
        TrueM (null = null)                   "null = null"
        TrueM ("" <> null)                    "\"\" <> null"
        TrueM ("Szia" = "Szia")               "\"Szia\" = \"Szia\""
        TrueM ("Szia" <> "Hello")             "\"Szia\" = \"Hello\""
        TrueM ([1;2;3;4] = [1..4])            "[1;2;3;4] = [1;2;3;4]"
        TrueM (Some 3 = Some 3)               "Some 3 = Some 3"
        TrueM ([|1;2;3;4|] = [|1..4|])        "[|1;2;3;4|] = [|1;2;3;4|]"
        TrueM ([|1;2;3|] <> [|1..4|])         "[|1;2;3|] <> [|1;2;3;4|]"
        TrueM ([1;2;3] <> [1..4])             "[1;2;3] <> [1;2;3;4]"
        TrueM ((1,2,3) = (1,2,3))             "(1,2,3) = (1,2,3)"
        TrueM (forall (fun x -> x=x) [0..10]) "forall x in 0..10, x = x"
        TrueM ((1,"b",3) <> (1,"a",3))        "(1,\"b\",3) <> (1,\"a\",3)"
        TrueM (box 0 <> box "")               "0 <> \"\""
        TrueM (box 0 <> box "0")              "0 <> \"0\""
        TrueM (box 0 <> box false)            "0 <> false"
        TrueM (box 0 <> box JS.Undefined)     "0 <> undefined"
        TrueM (box false <> box JS.Undefined) "false <> undefined"
        TrueM (null <> box JS.Undefined)      "null <> undefined"
        TrueM (box " \t\r\n" <> box 0)        "whitespace <> 0"
    }

    Test "Objects" {
        let t = T1()
        EqualM (t.Member "X") "Member: X"                 "t1.Member"
        EqualM t.Property "Initial Value"                 "t1.Property"
        EqualM (T1.StaticMember "X") "Static Member: X"   "T1.StaticMember"
        EqualM T1.StaticProperty "Initial Static Value"   "T1.StaticProperty"
        EqualM (T1.StaticMemberCurry "x" "y") "xy"        "T1.StaticMemberCurry"
    }

    Test "Renaming" {
        Equal {X=1}?Y 1
    }

    Test "JavaScript object" {
        let o = New [ "a", box "1"; "b", box 2 ]
        TrueM (o?a = "1" && o?b = 2) "List of tuples"
        let o2 = [ "a", 1; "b", 2 ] |> List.map (fun (n, v) -> n, box (v + 1)) |> New
        TrueM (o2?a = 2 && o2?b = 3) "Mapped list of tuples"
        let o3 = New [| "a", box "1"; "b", box 2 |]
        TrueM (o3?a = "1" && o3?b = 2) "Array of tuples"
        let o4 = New (seq { yield "a", box "1"; yield "b", box 2; })
        TrueM (o4?a = "1" && o4?b = 2) "Sequence of tuples"
        let o5 = New [ "a" => "1"; "b" => 2 ]
        TrueM (o5?a = "1" && o5?b = 2) "List of (=>) calls"
        let o6 = [ "a", 1; "b", 2 ] |> List.map (fun (n, v) -> n, box (v + 1)) |> New
        TrueM (o6?a = 2 && o6?b = 3) "Mapped list of (=>) calls"
        let o7 = New [| "a" => "1"; "b" => 2 |]
        TrueM (o7?a = "1" && o7?b = 2) "Array of (=>) calls"
        let o8 = New (seq { yield "a" => "1"; yield "b" => 2; })
        TrueM (o8?a = "1" && o8?b = 2) "Sequence of (=>) calls"
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
