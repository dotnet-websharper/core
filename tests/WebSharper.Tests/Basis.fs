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

module WebSharper.Tests.Basis

open System.Runtime.InteropServices

open WebSharper
open WebSharper.JavaScript
open WebSharper.Testing

#nowarn "40" // recursive values

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

[<JavaScript>] type private T2 = { [<Name "Y">] X : int }

[<Inline "isNaN($x)">]
let private isNaN (x: double) = System.Double.IsNaN x

[<Inline "var a = 21; a = 2*a; return a">]
let inlineReturn () = X<int>

[<Inline "r[0] = 3;">]
let inlineStatement (r: ref<int>) = X<unit>

[<Inline "r[0] = 5; ++r[0];">]
let inlineStatements (r: ref<int>) = X<unit>

[<JavaScript>]
let InnerGenerics pred l =
    let rec loop l cont =
        match l with
        | [] -> ([],[])
        | x::[] when pred x -> 
            (cont l, [])
        | x::xs when not (pred x) -> (cont [], l)
        | x::xs when pred x -> loop xs (fun rest -> cont (x::rest))
        | _ -> failwith "Unrecognized pattern"
    loop l id

[<JavaScript>]
let (|Odd|Even|) x = if x % 2 = 0 then Even else Odd

[<JavaScript>]
let (|Negative|_|) x = if x < 0 then Some -x else None

type System.Int32 with
    [<JavaScript>]
    static member TryParseOpt(s: string) =
        match System.Int32.TryParse(s) with
        | true, i -> Some i
        | _ -> None

type TestOptionals() =
    member this.Optionals([<Optional; DefaultParameterValue 1>] x : int, [<Optional; DefaultParameterValue 2>] y: int, [<Optional>] z: int) =
        x + y + z

    member this.TestOptionals() =
        this.Optionals(4)

[<JavaScript>]
type SameName() =
    member this.X() = 3
    
[<JavaScript>]
module SameName =
    let X() = 4

[<Inline>]
let RunTwice f =
    f()
    f()

[<Inline """
    function namedFunc(x) {
        return x*x;
    }
    var funcVar = function(x) {
        return x*x;
    };
    return {a: funcVar($0), b: namedFunc($0)};
""">]
let testFunc (x:int) = X<obj>

[<JavaScript>]
let Tests =
    TestCategory "Basis" {

        Test "Comparisons" {
            isTrueMsg (not (not true))  "not (not true)"
            isTrueMsg (not false)       "not false"
            isTrueMsg (not (1 <> 1))    "1 <> 1"
            isTrueMsg (1 <> 2)          "1 <> 2"
            isTrueMsg (1 < 2)           "1 < 2"
            isTrueMsg (not (1 < 1))     "1 < 1"
            isTrueMsg (not (1 < 0))     "1 < 0"
            isTrueMsg (2 > 1)           "2 > 1"
            isTrueMsg (not (2 > 2))     "2 > 2"
            isTrueMsg (not (2 > 3))     "2 > 3"
            isTrueMsg (1 <= 2)          "1 <= 2"
            isTrueMsg (1 <= 1)          "1 <= 1"
            isTrueMsg (not (1 <= 0))    "1 <= 0"
            isTrueMsg (2 >= 1)          "1 >= 1"
            isTrueMsg (2 >= 2)          "1 >= 2"
            isTrueMsg (not (2 >= 3))    "2 >= 3"
        }

        let closedLet =
            let a : list<int> = List.empty
            fun () -> a

        Test "Let" {
            equalMsg (closedLet()) [] "[] = closedLet"
        }

        Test "Factorial" {
            equalMsg (fac 6) (6 * 5 * 4 * 3 * 2) "fac 6"
            equalMsg (factorial 6) (6 * 5 * 4 * 3 * 2) "factorial 6"
        }

        let propPeano x = x = Peano.toNat (Peano.ofNat x)

        Test "forall" {
            isTrueMsg (forall (fun x -> x > 0) [1..10]) "forall x in 1..10: x > 0"
        }

        Test "Peano" {
            isTrueMsg (propPeano 0)              "propPeano 0"
            isTrueMsg (propPeano 1)              "propPeano 1"
            isTrueMsg (propPeano 2)              "propPeano 2"
            isTrueMsg (forall propPeano [3..10]) "propPeano 3..10"
        }

        let rec fact = function
            | 0 -> 1
            | n -> n * (fact (n - 1))

        Test "Nesting" {
            equalMsg (fact 6) 720 "fact 6 = 720"
        }

        Test "While" {
            let i     = ref 0
            let accum = ref 0
            let _ =
                while !i <= 3 do
                    accum := !accum + !i
                    i := !i + 1
            equal !accum 6
        }

        Test "For" {
            let accum = ref 0
            let _ =
                for i in 0 .. 3 do
                    accum := !accum + i
            equal !accum 6
        }

        Test "Floats" {
            notEqualMsg 1. 2.             "1. <> 2."
            equalMsg 1. 1.                "1. = 1."
            equalMsg (3./2.) 1.5          "3./2. = 1.5"
            equalMsg (1 + 2 * 4 / 6) 2    "1 + 2 * 4 / 6 = 2"
            let fEpsilon = 1.40129846e-45f
            equalMsg (1.f + fEpsilon) 1.f "1.f + \\epsilon = 1.f"
            let dEpsilon = 4.940656458e-324
            equalMsg (1. + dEpsilon) 1.   "1. + \\epsilon = 1."
            notEqualMsg fEpsilon 0.f      "\\epsilon <> 0.f"
            notEqualMsg dEpsilon 0.       "\\epsilon <> 0."
        }

        Test "NaN" {
            notEqualMsg (box nan) null "box nan <> null"
            forEach [(+); (-); (*); (/)] (fun op -> Do {
                forEach { 0. .. 10. } (fun x -> Do {
                    isTrueMsg (isNaN (op x nan)) ("op(x,nan) = nan where x=" + string x)
                })
            })
        }

        Test "Infinity" {
            notEqualMsg (box infinity) null   "infinity <> null"
            equalMsg (1./0.) infinity         "1./0. = infinity"
            forEach { 0. .. 10. } (fun x -> Do {
                equalMsg (x / infinity) 0. ("x/infinity = 0 where x = " + string x)
            })
        }

        Test "Booleans" {
            isTrueMsg (true && true)  "true && true"
            isTrueMsg (true || false) "true || false"
        }

        Test "Ranges" {
            equalMsg [|1..5|] [|1;2;3;4;5|] "1..5"
            equalMsg [1..5] [1;2;3;4;5] "1..5"
        }

        Test "Tuples" {
            let (a, b, c) = (1, 2, 3)
            equal (a + b + c) 6
            let t = ("Hello ", "Szia ", "Hej")
            let (t1, t2, t3) = t
            equal (t1 + t2 + t3) "Hello Szia Hej"
            isTrueMsg ((1, 2) < (1, 3)) "(1, 2) < (1, 3)"
            isTrueMsg ((1, 2) > (1, 1)) "(1, 2) > (1, 1)"
            isTrueMsg ((1, 8) < (2, 1)) "(1, 8) < (2, 1)"
        }

        Test "Struct tuples" {
            let f () =
                let t = struct (0, 0, 0)
                let mutable struct (a, b, c) = t
                a <- 1
                b <- 2
                c <- 3
                t
            let struct (a, b, c) = struct (1, 2, 3)
            equal (a + b + c) 6
            let t = struct ("Hello ", "Szia ", "Hej")
            let struct (t1, t2, t3) = t
            equal (t1 + t2 + t3) "Hello Szia Hej"
            isTrueMsg (struct (1, 2) < struct (1, 3)) "(1, 2) < (1, 3)"
            isTrueMsg (struct (1, 2) > struct (1, 1)) "(1, 2) > (1, 1)"
            isTrueMsg (struct (1, 8) < struct (2, 1)) "(1, 8) < (2, 1)"
        }

        Test "Currying" {
            let add (x, y) = x + y
            let add' y = add y
            equal (add' (1, 2)) 3
        }

        let rec odd =
            function  0 -> false
                    | n -> even (n-1)
        and even =
            function  0 -> true
                    | n -> odd (n-1)

        Test "Recursion" {
            isTrueMsg (even 12) "even 12"
            isTrueMsg (odd 23) "odd 23"
        }

        Test "Shadowing" {
            equal (shadow 1) 1
            equal (shadowRec 1) 1
        }

        Test "Equality" {
            isTrueMsg (1 = 1)                         "1 = 1"
            isTrueMsg (1 <> 2)                        "1 <> 2"
            isTrueMsg (box (1,2,3) <> box (1,(2,3)))  "(1,2,3) <> (1,(2,3))"
            isTrueMsg (null = null)                   "null = null"
            isTrueMsg ("" <> null)                    "\"\" <> null"
            isTrueMsg ("Szia" = "Szia")               "\"Szia\" = \"Szia\""
            isTrueMsg ("Szia" <> "Hello")             "\"Szia\" = \"Hello\""
            isTrueMsg ([1;2;3;4] = [1..4])            "[1;2;3;4] = [1;2;3;4]"
            isTrueMsg (Some 3 = Some 3)               "Some 3 = Some 3"
            isTrueMsg ([|1;2;3;4|] = [|1..4|])        "[|1;2;3;4|] = [|1;2;3;4|]"
            isTrueMsg ([|1;2;3|] <> [|1..4|])         "[|1;2;3|] <> [|1;2;3;4|]"
            isTrueMsg ([1;2;3] <> [1..4])             "[1;2;3] <> [1;2;3;4]"
            isTrueMsg ((1,2,3) = (1,2,3))             "(1,2,3) = (1,2,3)"
            isTrueMsg (forall (fun x -> x=x) [0..10]) "forall x in 0..10, x = x"
            isTrueMsg ((1,"b",3) <> (1,"a",3))        "(1,\"b\",3) <> (1,\"a\",3)"
            isTrueMsg (box 0 <> box "")               "0 <> \"\""
            isTrueMsg (box 0 <> box "0")              "0 <> \"0\""
            isTrueMsg (box 0 <> box false)            "0 <> false"
            isTrueMsg (box 0 <> box JS.Undefined)     "0 <> undefined"
            isTrueMsg (box false <> box JS.Undefined) "false <> undefined"
            isTrueMsg (null <> box JS.Undefined)      "null <> undefined"
            isTrueMsg (box " \t\r\n" <> box 0)        "whitespace <> 0"
        }

        Test "Objects" {
            let t = T1()
            equalMsg (t.Member "X") "Member: X"                 "t1.Member"
            equalMsg t.Property "Initial Value"                 "t1.Property"
            equalMsg (T1.StaticMember "X") "Static Member: X"   "T1.StaticMember"
            equalMsg T1.StaticProperty "Initial Static Value"   "T1.StaticProperty"
            equalMsg (T1.StaticMemberCurry "x" "y") "xy"        "T1.StaticMemberCurry"
        }

        Test "Renaming" {
            equal {X=1}?Y 1
        }

        Test "JavaScript object" {
            let o = New [ "a", box "1"; "b", box 2 ]
            isTrueMsg (o?a = "1" && o?b = 2) "List of tuples"
            let o2 = [ "a", 1; "b", 2 ] |> List.map (fun (n, v) -> n, box (v + 1)) |> New
            isTrueMsg (o2?a = 2 && o2?b = 3) "Mapped list of tuples"
            let o3 = New [| "a", box "1"; "b", box 2 |]
            isTrueMsg (o3?a = "1" && o3?b = 2) "Array of tuples"
            let o4 = New (seq { yield "a", box "1"; yield "b", box 2; })
            isTrueMsg (o4?a = "1" && o4?b = 2) "Sequence of tuples"
            let o5 = New [ "a" => "1"; "b" => 2 ]
            isTrueMsg (o5?a = "1" && o5?b = 2) "List of (=>) calls"
            let o6 = [ "a", 1; "b", 2 ] |> List.map (fun (n, v) -> n, box (v + 1)) |> New
            isTrueMsg (o6?a = 2 && o6?b = 3) "Mapped list of (=>) calls"
            let o7 = New [| "a" => "1"; "b" => 2 |]
            isTrueMsg (o7?a = "1" && o7?b = 2) "Array of (=>) calls"
            let o8 = New (seq { yield "a" => "1"; yield "b" => 2; })
            isTrueMsg (o8?a = "1" && o8?b = 2) "Sequence of (=>) calls"
        }

        Test "JS.Inline" {
            equal (JS.Inline "1 + 2") 3
            equal (JS.Inline("$0 + $1", 1, 2)) 3
            let r = ref 0
            let next() =
                incr r 
                !r
            equal (JS.Inline("$0 + $1 + $1", next(), next())) 5
        }

        Test "simplified match" {
            let res =
                let mutable r = 0
                match 1 + 1 with
                | 0 -> r <- 1
                | 1
                | 2 -> r <- 3
                | 5 -> r <- 6
                | _ -> ()
                r
            equal res 3
        }

        Test "Let rec initialization" {
            let res =
                let rec a = b() + 1
                and b() = 2
                a
            equal res 3 
            let res2 =
                let rec a = b + 1
                and b = 2
                a
            equal res2 3           
        }

        Test "Local function" {
            let res =
                let f x = x + 1
                f 1, f 2
            equal res (2, 3)
        }

        Test "Active patterns" {
            let isOdd x = 
                match x with 
                | Odd -> true
                | Even -> false
            isTrue (isOdd 1)
            isFalse (isOdd 2)

            let (|LocalOdd|LocalEven|) x = if x % 2 = 0 then LocalEven else LocalOdd

            let isOddLocal x = 
                match x with 
                | LocalOdd -> true
                | LocalEven -> false
            isTrue (isOddLocal 1)
            isFalse (isOddLocal 2)

            let testNegativePattern x =
                match x with
                | Negative y -> x = -y
                | _ -> x >= 0
            isTrue (testNegativePattern -5)
            isTrue (testNegativePattern 0)
            isTrue (testNegativePattern 5)                
        }

        Test "Static type augmentation" {
            equal (System.Int32.TryParseOpt "no") None
            equal (System.Int32.TryParseOpt "123") (Some 123)
        }

        Test "Inlined return statement" {
            equal (inlineReturn()) 42
        }

        Test "Inlined statement" {
            let r = ref 0
            inlineStatement(r)
            equal !r 3
            inlineStatements(r)
            equal !r 6
        }

        Test "Inlined local function" {
            let x = testFunc 3
            equal x?a 9
            equal x?b 9
        }
        
        Test "F# 4.1 syntax" {
            let a = 1_024
            equalMsg a 1024 "underscores in numeric literals"                
            equalMsg (TestOptionals().TestOptionals()) 6 "Optional and DefaultParameterValue respected in F# within the same project"
            equalMsg (SameName().X(), SameName.X()) (3, 4) "implicit Module suffix"
        }
    }
