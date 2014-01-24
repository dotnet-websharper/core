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

let ( *. ) f x = app f x

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
    | F of string * T
    | L of string * T * T
    | V of string

let v (x: C.Id) = defaultArg x.Name (string x)

let rec toT e =
    match e with
    | C.Application (f, [x]) -> A (toT f, toT x)
    | C.Lambda (None, [x], b) -> F (v x, toT b)
    | C.Let (var, value, body) -> L (v var, toT value, toT body)
    | C.Var x -> V (v x)


open C


let exprO = C.Optimize expr
toT exprO

match exprO with
| Let (var, value, body) ->
    toT (inlineLetX exprO var value body)

toT (C.Optimize expr)

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
