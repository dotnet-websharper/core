// $begin{copyright}
//
// This file is part of WebSharper
//
// Copyright (c) 2008-2012 IntelliFactory
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

module IntelliFactory.WebSharper.Compiler.Corrector

module C = IntelliFactory.JavaScript.Core
type Currying = list<int>

type Correction =
    | Constructor of Currying
    | Field
    | Method of Currying * MemberScope

exception UncurryError

let (|L|_|) q =
    let (|L|_|) q =
        match q with
        | C.Lambda (None, [id], b) -> Some (id, b)
        | _ -> None
    match q with
    | C.Call (C.Runtime, C.Constant (C.String "Tupled"), [L (id, b)])
    | L (id, b) -> Some (id, b)
    | _ -> None

let unsafeUncurry (curr: list<int>) (q: C.Expression) =
    let (|I|_|) x =
        match x with
        | C.Constant (C.Integer x) -> Some (int x)
        | _ -> None
    match curr with
    | [] -> q
    | _  ->
        let rec loop (vars: list<C.Id>) tuples spec q =
            match spec with
            | [] -> (List.rev vars, tuples, q)
            | 1 :: xs ->
                match q with
                | L (id, q) -> loop (id :: vars) tuples xs q
                | _ -> raise UncurryError
            | k :: xs ->
                match q with
                | L (id, q) ->
                    let rec detuple tup j q =
                        match q with
                        | C.Let (vn, C.FieldGet (C.Var tuple, I n), q)
                            when tuple = id && int n = j ->
                            if j + 1 = k then
                                let tup = vn :: tup
                                (tup, q)
                            else
                                detuple (vn :: tup) (j + 1) q
                        | xs ->
                            raise UncurryError
                    let (tup, q) = detuple [] 0 q
                    loop (tup @ vars) ((id, List.rev tup) :: tuples) xs q
                | _ ->
                    raise UncurryError
        let (vars, tuples, q) = loop [] [] curr q
        let free = C.GetFreeIds q
        let rec build x y =
            match x with
            | [] -> y
            | (id, ids) :: rest ->
                if free.Contains id then
                    C.Let (id, C.NewArray (List.map C.Var ids @ [!~ (C.String "unsafeUncurry")]), y)
                    |> build rest
                else y
        C.Lambda (None, vars, build tuples q)

let uncurry curr q =
    try unsafeUncurry curr q with UncurryError ->
        let app f x =
            match f with
            | L (v, b) -> C.Let (v, x, b)
            | _ -> C.Application (f, [x])
        let (body, vars) =
            List.fold (fun (q, vars) k ->
                match k with
                | 1 ->
                    let v = C.Id ()
                    let x = C.Var v
                    (app q x, v :: vars)
                | _ ->
                    let vs = List.init k (fun _ -> C.Id())
                    let xs = C.NewArray (List.map C.Var vs)
                    (app q xs, List.rev vs @ vars))
                (q, [])
                curr
        C.Lambda (None, List.rev vars, body)

let fixThisUse scope (q: C.Expression) =
    match scope with
    | Static -> q
    | Instance ->
        match q with
        | C.Lambda (None, x :: args, body) -> C.Lambda (Some x, args, body)
        | _ -> q

let fixCtor (q: C.Expression) =
    let free = C.GetFreeIds q
    let self =
        if free.IsEmpty then C.Id "r" else
            let e = free.MinimumElement
            e.Name <- Some "r"
            e
    let ret t = C.Call (C.Runtime, !~(C.String "New"), [C.Var t; C.Var self])
    match q with
    | C.Lambda (None, args, body) ->
        let this = C.Id ()
        let (inst, rest) =
            match body with
            | C.Sequential (obj, x) -> (obj, x)
            | obj -> (obj, !~C.Undefined)
        let body = C.Let (self, inst, C.Sequential (rest, ret this))
        C.Lambda (Some this, args, body)
    | _ -> q

let removeUnitVar q =
    let (|U|_|) (x: C.Id) =
        match x.Name with
        | Some i when i.StartsWith "unitVar" -> Some x
        | _ -> None
    match q with
    | C.Lambda (t, [U i], q) ->
        if not ((C.GetFreeIds q).Contains i) then
            C.Lambda (t, [], q)
        else q
    | _ -> q

let Correct correction quotation =
    match correction with
    | Constructor c ->
        uncurry c quotation
        |> removeUnitVar
        |> fixCtor
    | Field ->
        C.Lambda (None, [], removeUnitVar quotation)
    | Method (c, s) ->
        let lam x =
            match x with
            | C.Lambda _ -> x
            | x -> C.Lambda (None, [], x)
        uncurry c quotation
        |> fixThisUse s
        |> removeUnitVar
        |> lam
