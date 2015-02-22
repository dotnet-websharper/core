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

module WebSharper.Compiler.Corrector

module C = WebSharper.Core.JavaScript.Core
type Currying = list<int>

type Correction =
    | Constructor of Currying
    | Field
    | Method of Currying * MemberScope

exception UncurryError

let (|L|_|) q =
    match q with
    | C.Lambda (None, [id], b) -> Some (id, b)
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
    let ret t self = C.New (C.Var t, [self])
    match q with
    | C.Lambda (None, args, body) ->
        let this = C.Id ()
        let (inst, rest) =
            match body with
            | C.Sequential (obj, x) -> (obj, x)
            | obj -> (obj, !~C.Undefined)
        let body = C.Let (self, ret this inst, C.Sequential (rest, C.Var self))
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
