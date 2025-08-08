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

module IM = WebSharper.Collections.Tests.Interop.Module

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

let serverOnlyZero() = 0

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

[<Inline "inlineStatementTest = true;">]
let inlineStatement () = X<unit>

[<Inline "inlineStatementTest1 = true; inlineStatementTest2 = true;">]
let inlineStatements () = X<unit>

[<Inline("$you_should_NOT_see_this + $you_should_see_this + $you_should_see_this_too")>]
let inline unmarkedDollarInlines (you_should_NOT_see_this: int) = X<unit>

[<Inline("$x + $you_should_NOT_see_this + $you_should_NOT_see_this_either",
         UsingDollarVariables = "$you_should_NOT_see_this, you_should_NOT_see_this_either")>]
let inline markedDollarInlines (x: int) = X<unit>

[<Direct("$you_should_NOT_see_this + $you_should_see_this + $you_should_see_this_too")>]
let inline unmarkedDollarDirects (you_should_NOT_see_this: int) = X<unit>

[<Direct("$x + $you_should_NOT_see_this + $you_should_NOT_see_this_either",
         UsingDollarVariables = "$you_should_NOT_see_this, you_should_NOT_see_this_either")>]
let inline markedDollarDirects (x: int) = X<unit>

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

[<return: Struct>]
[<JavaScript>]
let (|NegativeStruct|_|) x = if x < 0 then ValueSome -x else ValueNone

[<JavaScript>]
let (|Negative|_|) x = if x < 0 then Some -x else None

[<JavaScript>]
let (|IsNegative|_|) x = x < 0

type System.Int32 with
    [<JavaScript>]
    static member TryParseOpt(s: string) =
        match System.Int32.TryParse(s) with
        | true, i -> Some i
        | _ -> None

// JavaScript scope set by assembly-level attribute
type TestOptionals() =
    member this.Optionals([<Optional; DefaultParameterValue 1>] x : int, [<Optional; DefaultParameterValue 2>] y: int, [<Optional>] z: int) =
        x + y + z

    member this.TestOptionals() =
        this.Optionals(4)

// JavaScript scope set by wsconfig.json
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
module ModuleValues =
    let a, b = 1, 2

[<JavaScript>]
let mA, mB = 1, 2

[<JavaScript>]
module MutableModuleValues =
    let mutable a, b = 1, 2
    do 
        a <- b + 1
        b <- a + 1

//[<Inline>]
//let importTestJsAll : obj = JS.ImportAll "/modules/test.js"

//[<Inline>]
//let importTestJs : obj = JS.Import("testExport", "/modules/test.js")

//[<Inline>]
//let importTestJsDefault : obj = JS.ImportDefault "/modules/test.js"

//[<Inline "import('/modules/test.js')">]
//let importInline = X<obj>

[<Literal>]
let ComputedLiteral = 1 + 2 

[<Literal>]
[<JavaScript>]
let ComputedLiteralJS = 1 + 2 

[<JavaScript>]
module ApplicativeCE =
    // First, define a 'zip' function
    module Result =
        let zip x1 x2 = 
            match x1,x2 with
            | Ok x1res, Ok x2res -> Ok (x1res, x2res)
            | Error e, _ -> Result.Error e
            | _, Error e -> Result.Error e

    // Next, define a builder with 'MergeSources' and 'BindReturn'
    type ResultBuilder() = 
        member _.MergeSources(t1: Result<'T,'U>, t2: Result<'T1,'U>) = Result.zip t1 t2
        member _.BindReturn(x: Result<'T,'U>, f) = Result.map f x

    let result = ResultBuilder()

    let run r1 r2 r3 =        
        // And here is our applicative!
        let res1: Result<int, string> =
            result { 
                let! a = r1 
                and! b = r2
                and! c = r3
                return a + b - c 
            }

        match res1 with
        | Ok x -> $"{nameof res1} is: {x}"
        | Error e -> $"{nameof res1} is: {e}"

    //let printApplicatives () =
    //    let r1 = Ok 2
    //    let r2 = Ok 3 // Error "fail!"
    //    let r3 = Ok 4

    //    run r1 r2 r3
    //    run r1 (Result.Error "failure!") r3

[<JavaScript>]
module rec ModuleRecTest = 
    let y = (X().x())

    type X() =
        member this.x() = 1

[<JavaScript>]
module LetRecTest = 
    let rec x = y + z
    and y = z
    and z = 1

[<JavaScript>]
module LetMutableTest =
    let mutable x = 1
    let y =
        x <- 2
        x

[<JavaScript>]
type type' =
    static member myFunction'<'t>(x: 't) = x
        
[<JavaScript>]
type IValue<'T> =
    abstract member Value : 'T with get

[<JavaScript>]
type OptionalValueArg([<Struct>] ?a) =
    let b = defaultValueArg a 2
    member _.B = b

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
            
            let testNegativeStructPattern x =
                match x with
                | NegativeStruct y -> x = -y
                | _ -> x >= 0
            isTrue (testNegativeStructPattern -5)
            isTrue (testNegativeStructPattern 0)
            isTrue (testNegativeStructPattern 5)                
        }

        Test "Static type augmentation" {
            equal (System.Int32.TryParseOpt "no") None
            equal (System.Int32.TryParseOpt "123") (Some 123)
        }

        Test "Inlined return statement" {
            equal (inlineReturn()) 42
        }

        Test "Inlined statement" {
            inlineStatement()
            isTrue JS.Window?inlineStatementTest
            inlineStatements()
            isTrue JS.Window?inlineStatementTest1
            isTrue JS.Window?inlineStatementTest2
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

        Test "IsClient" {
            equal (if IsClient then 1 else serverOnlyZero()) 1
            equal (if not IsClient then serverOnlyZero() else 1) 1
        }

        Test "Module level let pattern match" {
            equal ModuleValues.a 1
            equal ModuleValues.b 2
            equal mA 1
            equal mB 2            
        }

        Test "Module level let pattern match with mutable" {
            equal MutableModuleValues.a 3
            equal MutableModuleValues.b 4
        }

        Test "ValueOption" {
            raises ValueNone.Value
            equalMsg (ValueSome(1).Value) 1 "ValueOption.Value"
        }

        Test "Reserved global identifiers" {
            isTrue (
                let self = 10
                let window = 20
                self = 10 && window = 20 && self + window = 30 && JS.Global :? WebSharper.JavaScript.Window
            )
        }

        Test "Anonymous records" {
            let x = IM.AnonRecord({| A = 42 |})
            equal x.B 42
            equal {| x with C = 3|} (New ["B" => 42; "C" => 3])
            let a, b = 
                match IM.AnonRecordInUnion() with
                | IM.AnonRecordTest r -> r.A, r.B
            equal a 3
            equal b "hi"
            equal {| A = 1; B = ValueNone; C = ValueSome 3 |} (New ["A" => 1; "C" => 3])
            equal {| A = 1; B = None; C = Some 3 |} (New ["A" => 1; "C" => 3])
            let s = struct {| A = 5 |}
            equal s.A 5
        }

        Test "Implicit yield" {
            let l =
                [
                    1 
                    if true then
                        2
                    3
                ]
            equal l [1; 2; 3]
        }

        Test "OptimizedClosures" {
            let f2 = OptimizedClosures.FSharpFunc<_,_,_>.Adapt(fun a b -> a + b)
            let f3 = OptimizedClosures.FSharpFunc<_,_,_,_>.Adapt(fun a b c -> a + b + c)
            let f4 = OptimizedClosures.FSharpFunc<_,_,_,_,_>.Adapt(fun a b c d -> a + b + c + d)
            let f5 = OptimizedClosures.FSharpFunc<_,_,_,_,_,_>.Adapt(fun a b c d e -> a + b + c + d + e)
            equal (f2.Invoke(1) 2) 3
            equal (f2.Invoke(1, 2)) 3
            equal (f3.Invoke(1) 2 3) 6
            equal (f3.Invoke(1, 2, 3)) 6
            equal (f4.Invoke(1) 2 3 4) 10
            equal (f4.Invoke(1, 2, 3, 4)) 10
            equal (f5.Invoke(1) 2 3 4 5) 15
            equal (f5.Invoke(1, 2, 3, 4, 5)) 15
        }

        Test "LanguagePrimitives" {
            equal LanguagePrimitives.GenericZero 0
            equal LanguagePrimitives.GenericOne 1
            equal (LanguagePrimitives.FastGenericComparer<int>.Compare(1, 2)) -1
        }

        Test "Proxy project" {
            equal (ProxyProjectTest.Functions.add 1 2) 3
            isTrue (ProxyProjectTest.Functions.isJS())
        }

        Test "Proxy project no namespace" {
            equal (NoNamespace.Base.add1 1) 2
            equal (NoNamespace.Sub.add1 1) 2
            equal (NoNamespace.Sub.add2 1) 3
        }

        Test "nameof" {
            equal (nameof System) "System"
            equal (nameof System.Int32) "Int32"
            let x = 1 + 1
            equal (nameof x) "x"
            equal (nameof ModuleValues.a) "a"
            equal (nameof (+)) "+"
        }

        Test "F# 5 slicing" {
            let l = [ 1..10 ]
            let a = [| 1..10 |]
            let s = "hello!"
            equal (l.[-2..(-1)] |> Array.ofList) [||]
            equal (a.[-2..(-1)]) [||]
            equal (s.[-2..(-1)]) ""
        }

        Test "F# 6 indexing" {
            let l = [ 1..10 ]
            equal l[1] 2
            equal (l[1 .. 2] |> Array.ofList) [| 2; 3 |]
        }

        Test "F# 6 as pattern" {
            let resOk =
                match 1, 2 with
                | _ as (a, b) ->
                    a = 1 && b = 2
            isTrue resOk
        }

        Test "F# 6 implicit conversion" {
            let res : int64 =
                if true then 1L else 2
            equal res 1L
        }

        Test "F# module rec" {
            equal ModuleRecTest.y 1
        }

        Test "F# let rec" {
            equal LetRecTest.x 2
        }

        Test "F# let mutable in module" {
            equal LetMutableTest.y 2
        }

        Test "JS.Verbatim" {
            equal (JS.Verbatim "1 + 1") 2
            equal (JS.Verbatim $"1 + { 1 + 1 }") 3
            let x = 2
            equal (JS.Verbatim $"{x} * {x}") 4
        }

        Test "Type and function name escaping" {
            equal (type'.myFunction' "hi") "hi"
        }

        Test "_.Property shorthand" {
            let hl = "hello" |> _.Length
            equal hl 5
        }

        Test "Computed literal" {
            equal ComputedLiteral 3
            equal ComputedLiteralJS 3
        }

        Test "Local inlines" {
            let test1 =
                let inline ( !! ) (o: ^x) : ^a = (^x: (member Value: ^a with get) o)
                let i = { new IValue<int> with member this.Value = 4 }
                !!i
            equal test1 4

            let test2 =
                let inline (+@) x y = x + x * y
                1 +@ 1, 1.0 +@ 0.5
            equal test2 (2, 1.5)
        }

        Test "Partial active patterns can return bool instead of unit option" {
            let testIsNegativePattern x =
                match x with
                | IsNegative -> true
                | _ -> false
            isTrue (testIsNegativePattern -5)
            isFalse (testIsNegativePattern 5)       
        }

        Test "Convert.ToBoolean" {
            equalMsg true (System.Convert.ToBoolean("true")) ""
            equal true (System.Convert.ToBoolean(1))
            equal true (System.Convert.ToBoolean(1.))
            equal false (System.Convert.ToBoolean("false"))
            equal false (System.Convert.ToBoolean(0))
            equal false (System.Convert.ToBoolean(0.))
        }

        Test "Optional ValueOption argument" {
            let o = OptionalValueArg()
            equal o.B 2
            let o1 = OptionalValueArg(1)
            equal o1.B 1
        }

        Test "Allow _ in use! bindings" {
            let x = ref 0
            let getDisposable() =
                async.Return { 
                    new System.IDisposable with
                        member this.Dispose() = 
                            incr x
                }
            let a : Async<int> =
                async {
                    use! _ = getDisposable()
                    return x.Value
                }
            let! res: int = a
            equal res 0
            equal x.Value 1
        }
    }
