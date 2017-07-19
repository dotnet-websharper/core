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

module WebSharper.Tests.Optimizations

open WebSharper
open WebSharper.JavaScript
open WebSharper.Testing
module R = WebSharper.Testing.Random

[<JavaScript>]
let GlobalTupled (a, b) =
    a + 2 * b 

[<JavaScript>]
let GlobalCurried a b =
    a + 2 * b

[<JavaScript>]
let TupledArg f : int =
    f (1, 2)

[<JavaScript>]
let CurriedArg f : int =
    f 1 2

[<JavaScript>]
let rec RecCurriedArg f : int =
    let r = f 1 2 
    if r = 5 then
        RecCurriedArg (fun x y -> f x (y + 1))
    else
        r 

[<JavaScript>]
type TypeWithTupled(f) =
    let g = JavaScript.FuncWithArgs(f)
    member this.Apply() = g.Call(1, 2) : int

    static member TupledArg f = 
        f (1, 2)

type ICurried =
    [<Name "C">]
    abstract Add: int -> int -> int

[<JavaScript>]
type Curried() =
    interface ICurried with
        member this.Add a b = a + b

[<JavaScript>]
type TypeWithCurried(f) =
    let g = JavaScript.FuncWithArgs(fun (a, b) -> f a b)
    member this.Apply() = g.Call(1, 2) : int

    member this.Other a b h = h a b

    [<Inline>]
    member this.OtherU(a, b, h) = this.Other a b h

    static member CurriedArg f = 
        f 1 2

[<JavaScript>]
let CurriedArg2 v a x = v x a

[<Inline>]
let InlinedCurriedArg v a x = v x a

[<JavaScript>]
module Bug663 =
    let Map2 f x y = f x y
    let Apply f x = f x
    let Zip3 a b c =
        Apply (Map2 (fun x y z -> (x, y, z)) a b) c

[<JavaScript>]
type Bug663S =
    static member Map2 f x y = f x y
    static member Apply f x = f x
    static member Zip3 a b c =
        Bug663S.Apply(Bug663S.Map2 (fun x y z -> (x, y, z)) a b) c

[<JavaScript>]
type Bug663I() =
    member this.Map2 f x y = f x y
    member this.Apply f x = f x
    member this.Zip3 a b c =
        this.Apply(this.Map2 (fun x y z -> (x, y, z)) a b) c

[<JavaScript>]
let tailRecFactorialCurried n =
    let rec factorial acc n =
        match n with
        | 0 -> acc
        | n -> factorial (n * acc) (n - 1)
    factorial 1 n

[<JavaScript>]
let tailRecFactorialCurried2 n =
    let rec factorial acc = function
        | 0 -> acc
        | n -> factorial (n * acc) (n - 1)
    factorial 1 n

[<JavaScript>]
let tailRecFactorialTupled n =
    let rec factorial (acc, n) =
        match n with
        | 0 -> acc
        | n -> factorial (n * acc, n - 1)
    factorial (1, n)

[<JavaScript>]
let tailRecSingle n =
    let rec f n =
        if n > 0 then f (n - 1) else 0
    f n

[<Inline>]
let tailRecSingleInline n =
    let rec f n =
        if n > 0 then f (n - 1) else 0
    f n

[<JavaScript>]
let tailRecScoping n =
    let rec f acc n =
        if n > 0 then f ((fun () -> n) :: acc) (n - 1) else acc
    f [] n

[<JavaScript>]
let tailRecSingleNoReturn n =
    let rec f n =
        if n > 0 then f (n - 1)
    f n

[<JavaScript>]
let rec moduleTailRecSingleNoReturn l =
    match l with
    | h :: r -> moduleTailRecSingleNoReturn r
    | _ -> ()

[<JavaScript>]
let tailRecSingleUsedInside n =
    let mutable setf = fun x -> 0
    let rec f n =
        setf <- fun x -> f x
        if n > 0 then f (n - 1) else 3
    f n + setf n

[<JavaScript>]
let tailRecMultiple n =
    let rec f n =
        if n > 0 then g (n - 1) else 0
    and g n =
        if n > 0 then f (n - 1) else 1
    f n

[<JavaScript>]
let tailRecMultipleNoReturn n =
    let rec f n =
        if n > 0 then g (n - 1)
    and g n =
        if n > 0 then f (n - 1)
    f n

[<JavaScript>]
let rec moduleTailRecMultipleNoReturn n =
    if n > 0 then moduleTailRecMultipleNoReturn2 (n - 1)
and [<JavaScript>] moduleTailRecMultipleNoReturn2 n =
    if n > 0 then moduleTailRecMultipleNoReturn (n - 1)

[<JavaScript>]
let tailRecWithMatch l =
    let rec f acc l =
        match l with
        | [] -> acc
        | h :: t -> f (h :: acc) t
    f [] l

[<JavaScript>]
let rec moduleTailRecSingle n =
    if n > 0 then moduleTailRecSingle (n - 1) else 0

let rec [<JavaScript>] moduleTailRecMultiple1 n =
    if n > 0 then moduleTailRecMultiple2 (n - 1) else 0
and [<JavaScript>] moduleTailRecMultiple2 n =
    if n > 0 then moduleTailRecMultiple1 (n - 1) else 1

[<JavaScript>]
type TailRec() =
    let rec classTailRecSingle n =
        if n > 0 then classTailRecSingle (n - 1) else 0

    let rec classTailRecCurried n m =
        if n > 0 then classTailRecCurried (n - 1) (m - 1) else 0

    let rec classTailRecMultiple1 n =
        if n > 0 then classTailRecMultiple2 (n - 1) else 0
    and classTailRecMultiple2 n =
        if n > 0 then classTailRecMultiple1 (n - 1) else 1

    member this.TailRecSingle n = classTailRecSingle n
    member this.TailRecMultiple n = classTailRecMultiple1 n

    member this.TailRecSingle2 n =
        if n > 0 then this.TailRecSingle2 (n - 1) else 0

    member this.TailRecMultiple1 n =
        if n > 0 then this.TailRecMultiple2 (n - 1) else 0
    member this.TailRecMultiple2 n =
        if n > 0 then this.TailRecMultiple1 (n - 1) else 1

    member this.TailRecSingleNoReturn l =
        match l with
        | h :: r -> moduleTailRecSingleNoReturn r
        | _ -> ()

[<JavaScript>]
let tailRecWithValue n =
    let rec f n =
        if n > 0 then f (n - i) else i
    and i = 1
    f n

[<JavaScript>]
let tailRecMultipleWithValue n =
    let rec f n =
        if n > 0 then g (n - i) else 0
    and g n =
        if n > 0 then f (n - i) else 1
    and i = 1
    f n

[<JavaScript>]
let rec rev_map_acc f l acc = 
    match l with
    | [] -> acc
    | h :: t -> rev_map_acc f t (f h :: acc)

[<JavaScript>]
let Tests =

    let LocalTupled (a, b) =
        a + 2 * b 

    let LocalCurried a b =
        a + 2 * b

    TestCategory "Optimizations" {
        Test "Tupled function" {
            equal (GlobalTupled (1, 2)) 5
            equal (LocalTupled (1, 2)) 5
        }

        Test "Tupled function argument" {
            equal (TupledArg (fun (a, b) -> a + 2 * b)) 5
            equal (TupledArg GlobalTupled) 5
            equal (TupledArg LocalTupled) 5
            equal (TypeWithTupled(fun (a, b) -> a + 2 * b).Apply()) 5
            equal (TypeWithTupled.TupledArg GlobalTupled) 5
        }

        Test "Curried function" {
            equal (GlobalCurried 1 2) 5
            equal (LocalCurried 1 2) 5
            equal ((Curried() :> ICurried).Add 1 2) 3
        }

        Test "Curried function argument" {
            equal (CurriedArg (fun a b -> a + 2 * b)) 5
            equal (CurriedArg GlobalCurried) 5
            equal (CurriedArg LocalCurried) 5
            equal (TypeWithCurried(fun a b -> a + 2 * b).Apply()) 5
            equal (TypeWithCurried.CurriedArg GlobalCurried) 5
            equal (RecCurriedArg GlobalCurried) 7
        }

        Test "Curried method" {
            equal (TypeWithCurried(fun a b -> 0).Other 1 2 (fun a b -> a + 2 * b)) 5
            equal (TypeWithCurried(fun a b -> 0).OtherU(1, 2, (fun a b -> a + 2 * b))) 5
        }

        Test "Passing around argument" {
            let f = CurriedArg2 GlobalCurried 1
            equal (f 0) 2
            equal (f 5) 7            
        }

        Test "Inline curried function argument" {
            let f = InlinedCurriedArg GlobalCurried 1
            equal (f 0) 2
            equal (f 5) 7
        }

        Test "Optimizate curried argument only for correct length" {
            equal (Bug663.Zip3 1 2 3) (1, 2, 3)
            equal (Bug663S.Zip3 1 2 3) (1, 2, 3)
            equal (Bug663I().Zip3 1 2 3) (1, 2, 3)
        }

        Test "Tail calls" {
            equalMsg (tailRecFactorialCurried 6) (6 * 5 * 4 * 3 * 2) "curried tail call"
            equalMsg (tailRecFactorialCurried2 6) (6 * 5 * 4 * 3 * 2) "curried tail call with function"
            equalMsg (tailRecFactorialTupled 6) (6 * 5 * 4 * 3 * 2) "tupled tail call"
            equalMsg (tailRecSingle 5) 0 "single let rec"
            let inl =
                let mutable n = 5
                let r = tailRecSingleInline n
                r, n 
            equalMsg inl (0, 5) "single let rec inlined"
            equalMsg (tailRecScoping 5 |> List.map (fun f -> f())) [1; 2; 3; 4; 5] "scoping while tail call optimizing"
            equalMsg (tailRecWithMatch [ 3; 2; 1 ]) [ 1; 2; 3 ] "single let rec with non-inlined match expression"
            equalMsg (tailRecMultiple 5) 1 "mutually recursive let rec"
            equalMsg (tailRecWithValue 5) 1 "mutually recursive let rec with a function and a value"
            equalMsg (tailRecMultipleWithValue 5) 1 "mutually recursive let rec with two functions and a value"
            equalMsg (moduleTailRecSingle 5) 0 "single let rec in module"
            equalMsg (moduleTailRecMultiple1 5) 1 "mutually recursive let rec in module 1"
            equalMsg (moduleTailRecMultiple2 5) 0 "mutually recursive let rec in module 2"
            equalMsg (tailRecSingleUsedInside 5) 6 "recursive function used as a value inside"
            let o = TailRec()
            equalMsg (o.TailRecSingle 5) 0 "single let rec in class constructor"
            equalMsg (o.TailRecMultiple 5) 1 "mutually recursive let rec in class constructor"
            equalMsg (o.TailRecSingle2 5) 0 "single recursive class member"
            equalMsg (o.TailRecMultiple1 5) 1 "mutually recursive class members"

            let dbl x = x * 2 
            equalMsg (rev_map_acc dbl [ 1; 3; 4 ] []) [ 8; 6; 2 ] "module let rec with list accumulator"

            let res =
                let rec loc_rev_map_acc f l acc = 
                    match l with
                    | [] -> acc
                    | h :: t -> loc_rev_map_acc f t (f h :: acc)
                loc_rev_map_acc dbl [ 1; 3; 4 ] []
            equalMsg res [ 8; 6; 2 ] "let rec with list accumulator"

            // test if there is no infinite loop
            tailRecSingleNoReturn 5
            moduleTailRecSingleNoReturn [ 1 .. 5 ]
            tailRecMultipleNoReturn 5
            moduleTailRecMultipleNoReturn 5
            o.TailRecSingleNoReturn [ 1 .. 5 ]
        }
    }

[<JavaScript>]
let TupledArgWithGlobal() =
    TupledArg GlobalTupled

[<JavaScript>]
let TupledArgWithLocal() =
    let LocalTupled (a, b) =
        a + b 
    TupledArg LocalTupled

[<JavaScript>]
let CurriedArgWithGlobal() =
    CurriedArg GlobalCurried

[<JavaScript>]
let CurriedArgWithLocal() =
    let LocalCurried a b =
        a + b
    CurriedArg LocalCurried

[<JavaScript>]
let CollectJSObject() =
    let x = New [ "a" => 1 ]
    x?b <- 
        Console.Log(2)
        2
    x?c <- 
        Console.Log(3)
        3
    x

[<JavaScript>]
let InlineValues() =
    ("a", "b") ||> fun a b -> Console.Log(a, b)    

[<JavaScript>]
let InlineValues2() =
    ((Console.Log("a"); "a"), (Console.Log("b"); "b")) ||> fun a b -> Console.Log(a, b)    

[<JavaScript>]
let InlineValues3() =
    ((Console.Log("a"); "a"), (Console.Log("b"); "b")) ||> fun a b -> Array(a, b)    