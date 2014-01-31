#load "Syntax.fs"
#load "Preferences.fs"
#load "Identifier.fs"
#load "Extensions.fs"
#load "Core.fs"
open IntelliFactory.JavaScript

module C = Core

// ist.length >> add1 >> twice) [1; 2] =? 6

let lam f =
    let v = C.Id()
    C.Lambda (None, [v], f (C.Var v))

let app f x =
    C.Application (f, [x])

let compose =
    lam (fun f -> lam (fun g -> lam (fun x -> app f (app g x))))

let if_ c t e = C.IfThenElse (c, t, e)

let ( *. ) f x = app f x

let i (x: int) =
    !~ (C.Integer (int64 x))

let tru = !~C.True
let fals = !~ C.False

let ( >. ) a b =
    C.Binary (a, C.BinaryOperator.``>``, b)

let (!) x = C.Var x

let evenOdd =
    let even = C.Id "even"
    let odd = C.Id "odd"
    let n = C.Id "n"

    let bindings =
        [
            (even, lam (fun n -> if_ (n >. i 0) (!odd *. (n - i 1)) tru))
            (odd, lam (fun n -> if_ (n >. i 0) (!even *. (n - i 1)) fals))
        ]
    C.LetRecursive (bindings, C.Var even *. C.Var n)

//
//                        var odd, even;
//                                odd = function (_arg2) {
//                                    var even1;
//                                    return _arg2 === 0 ? false : even1(_arg2 - 1);
//                                };
//                                even = function (_arg3) {
//                                    var odd1;
//                                    return _arg3 === 0 ? true : odd1(_arg3 - 1);
//                                };
//                                ok(even(12), "even 12");
//                                ok(odd(23), "odd 23");
//                                return _builder_224_713.Zero();

let expr =
    let length = C.Id "length"
    let add1 = C.Id "add1"
    let twice = C.Id "twice"
    let n = C.Id "N"
    let ( >> ) f g = compose *. f *. g
    (C.Var length >> C.Var add1 >> C.Var twice) *. C.Var n

type T =
    | A of T * T
    | B of T * C.BinaryOperator * T
    | F of string * T
    | FG of T * T
    | FS of T * T * T
    | I of T * T * T
    | N of int
    | L of string * T * T
    | S of T * T
    | V of string
    | W of T * T
    | X of C.Expression

let v (x: C.Id) = defaultArg x.Name (string x)

let rec toT e =
    match e with
    | C.Application (f, [x]) -> A (toT f, toT x)
    | C.Binary (a, op, b) -> B (toT a, op, toT b)
    | C.Constant (C.Integer n) -> N (int n)
    | C.FieldGet (obj, fld) -> FG (toT obj, toT fld)
    | C.FieldSet (obj, fld, value) -> FS (toT obj, toT fld, toT value)
    | C.IfThenElse (a, b, c) -> I (toT a, toT b, toT c)
    | C.Lambda (None, [x], b) -> F (v x, toT b)
    | C.Let (var, value, body) -> L (v var, toT value, toT body)
    | C.Sequential (a, b) -> S (toT a, toT b)
    | C.WhileLoop (expr, loop) -> W (toT expr, toT loop)
    | C.Var x -> V (v x)
    | _ -> X e

let miniLR =
    let a = C.Id "a"
    let bindings = [a, !a *. i 1]
    let body = i 1
    C.LetRecursive (bindings, body)


|> C.GetFreeIds


evenOdd
|> C.AlphaNormalize

|> C.GetFreeIds

C.Optimize evenOdd



//open C
//
//
//let exprO = C.Optimize expr
//toT exprO
//
//match exprO with
//| Let (var, value, body) ->
//    toT (inlineLetX exprO var value body)
//
//toT (C.Optimize expr)

//
//    Unchecked.defaultof<_>
//


//
//let e  =
//    let x = C.Id("x")
//    let y = C.Id("y")
//    let i = C.Id("i", true)
//    let accum = C.Id("accum", true)
//    let zero = C.Id("zero")
//    C.Lambda (None, [x; y; zero],
//        C.Let (accum, C.Var zero,
//            C.Sequential (
//                C.ForIntegerRangeLoop (i, C.Var x, C.Var y,
//                    C.VarSet (accum, C.Binary (C.Var accum, C.BinaryOperator.``+``, C.Var i))),
//                C.Var accum)))
//
//
//
//e
//|> C.GetFreeIdSet
//
// unalias e
