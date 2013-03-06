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

module IntelliFactory.JavaScript.Core

module S = Syntax

type Dictionary<'T1,'T2> = System.Collections.Generic.Dictionary<'T1,'T2>
type HashSet<'T> = System.Collections.Generic.HashSet<'T>

type SB = Syntax.BinaryOperator
type SP = Syntax.PostfixOperator
type SU = Syntax.UnaryOperator

[<Sealed>]
type Id(id: int, ?name: string) =
    static let root = obj ()
    static let mutable n = 0
    static let next () = lock root (fun () -> let k = n in n <- k + 1; k)
    let mutable name = name

    new (name: option<string>) =
        match name with
        | None -> new Id()
        | Some n -> new Id(n)

    new () = new Id(next ())
    new n = new Id(next (), n)

    member this.Id = id

    member this.Name
        with get () = name
        and set x = name <- x

    override this.GetHashCode() = id

    override this.Equals obj =
        match obj with
        | :? Id as obj -> id = obj.Id
        | _ -> false

    interface System.IComparable with
        member this.CompareTo obj =
            match obj with
            | :? Id as obj -> compare this.Id obj.Id
            | _ -> invalidArg "obj" "Invalid type for comparison."

    override this.ToString() =
        let n =
            match name with
            | None -> "id"
            | Some n -> n
        System.String.Format("{0}#{1:x}", n, id)

type UnaryOperator =
    | ``~`` = 0
    | ``-`` = 1
    | ``!`` = 2
    | ``+`` = 3
    | ``typeof`` = 4
    | ``void`` = 5

type BinaryOperator =
    | ``!==`` = 0
    | ``!=`` = 1
    | ``%`` = 2
    | ``&&`` = 3
    | ``&`` = 4
    | ``*`` = 5
    | ``+`` = 6
    | ``-`` = 7
    | ``/`` = 8
    | ``<<`` = 9
    | ``<=`` = 10
    | ``<`` = 11
    | ``===`` = 12
    | ``==`` = 13
    | ``>=`` = 14
    | ``>>>`` = 15
    | ``>>`` = 16
    | ``>`` = 17
    | ``^`` = 18
    | ``in`` = 19
    | ``instanceof`` = 20
    | ``|`` = 21
    | ``||`` = 22

type B = BinaryOperator
type U = UnaryOperator

type Literal =
    | Double of double
    | False
    | Integer of int64
    | Null
    | String of string
    | True
    | Undefined

    override this.ToString() =
        match this with
        | Double x -> string x
        | False -> "false"
        | Integer x -> string x
        | Null -> "null"
        | String x -> System.String.Format("\"{0}\"", x)
        | True -> "true"
        | Undefined -> "undefined"

    static member ( !~ ) x = Constant x

and Expression =
    | Application of E * list<E>
    | Binary of E * BinaryOperator * E
    | Call of E * E * list<E>
    | Constant of Literal
    | FieldDelete of E * E
    | FieldGet of E * E
    | FieldSet of E * E * E
    | ForEachField of Id * E * E
    | ForIntegerRangeLoop of Id * E * E * E
    | Global of list<string>
    | IfThenElse of E * E * E
    | Lambda of option<Id> * list<Id> * E
    | Let of Id * E * E
    | LetRecursive of list<Id * E> * E
    | New of E * list<E>
    | NewArray of list<E>
    | NewObject of list<string * E>
    | NewRegex of string
    | Runtime
    | Sequential of E * E
    | Throw of E
    | TryFinally of E * E
    | TryWith of E * Id * E
    | Unary of UnaryOperator * E
    | Var of Id
    | WhileLoop of E * E

    static member ( + ) (a, b) = Binary (a, B.``+``, b)
    static member ( - ) (a, b) = Binary (a, B.``-``, b)
    static member ( * ) (a, b) = Binary (a, B.``*``, b)
    static member ( / ) (a, b) = Binary (a, B.``/``, b)
    static member ( % ) (a, b) = Binary (a, B.``%``, b)

    static member ( &== )  (a, b) = Binary (a, B.``==``, b)
    static member ( &!= )  (a, b) = Binary (a, B.``!=``, b)
    static member ( &=== ) (a, b) = Binary (a, B.``===``, b)
    static member ( &!== ) (a, b) = Binary (a, B.``!==``, b)
    static member ( &< )   (a, b) = Binary (a, B.``<``, b)
    static member ( &> )   (a, b) = Binary (a, B.``>``, b)
    static member ( &<= )  (a, b) = Binary (a, B.``<=``, b)
    static member ( &>= )  (a, b) = Binary (a, B.``>=``, b)

    static member ( ! ) a = Unary (UnaryOperator.``!``, a)
    static member ( ~+ ) a = Unary (UnaryOperator.``+``, a)
    static member ( ~- ) a = Unary (UnaryOperator.``-``, a)

    member this.Void = Unary (UnaryOperator.``void``, this)
    member this.TypeOf = Unary (UnaryOperator.``typeof``, this)

    member this.In x = Binary (this, B.``in``, x)
    member this.InstanceOf x = Binary (this, B.``instanceof``, x)

    member this.Item with get (x: E) = FieldGet (this, x)
    member this.Item with get xs = Application (this, xs)

    static member ( ? ) (e: E, msg: string) =
        FieldGet (e, Constant (String msg))

and E = Expression

exception TransformError

let Transform (!) expr =
    let (!!) = List.map (!)
    let t v x =
        match !(Lambda (None, [v], x)) with
        | Lambda (None, [v], x) -> (v, x)
        | Var f -> (v, Application (Var f, [Var v]))
        | _ -> raise TransformError
    let letRec vs b =
        let (vars, bs) = List.unzip vs
        match !(Lambda (None, vars, Sequential (NewArray bs, b))) with
        | Lambda (None, vars, Sequential (NewArray bs, b))
            when bs.Length = vars.Length ->
                LetRecursive (List.zip vars bs, b)
        | _ -> raise TransformError
    match expr with
    | Application (x, xs) -> Application (!x, !!xs)
    | Binary (x, o, y) -> Binary (!x, o, !y)
    | Call (f, m, xs) -> Call (!f, !m, !!xs)
    | FieldDelete (x, y) -> FieldDelete (!x, !y)
    | FieldGet (x, y) -> FieldGet (!x, !y)
    | FieldSet (x, y, z) -> FieldSet (!x, !y, !z)
    | ForEachField (v, x, y) ->
        let x = !x
        let (v, y) = t v y
        ForEachField (v, x, y)
    | ForIntegerRangeLoop (v, x, y, z) ->
        let x = !x
        let y = !y
        let (v, z) = t v z
        ForIntegerRangeLoop (v, x, y, z)
    | IfThenElse (x, y, z) -> IfThenElse (!x, !y, !z)
    | Lambda (this, vars, x) -> Lambda (this, vars, !x)
    | Let (v, x, y) ->
        let x = !x
        let (v, y) = t v y
        Let (v, x, y)
    | LetRecursive ([], b) -> LetRecursive ([], !b)
    | LetRecursive (vs, b) -> letRec vs b
    | New (f, xs) -> New (!f, !!xs)
    | NewArray x -> NewArray !!x
    | NewObject xs ->
        let t (k, v) = (k, !v)
        NewObject (List.map t xs)
    | Sequential (x, y) -> Sequential (!x, !y)
    | Throw x -> Throw !x
    | TryWith (x, v, y) ->
        let x = !x
        let (v, y) = t v y
        TryWith (x, v, y)
    | TryFinally (x, y) -> TryFinally (!x, !y)
    | Unary (o, x) -> Unary (o, !x)
    | WhileLoop (x, y) -> WhileLoop (!x, !y)
    | _ ->  expr

let Fold f init expr =
    let state = ref init
    let g x = state := f !state x; x
    ignore (Transform g expr)
    !state

let IsAlphaNormalized e =
    let set = HashSet()
    let rec ok = function
        | Lambda (v, vs, b) ->
            if v.IsSome && set.Contains v.Value
               || Seq.exists set.Contains vs then
                false
            else
                if v.IsSome then
                    set.Add v.Value |> ignore
                Seq.iter (set.Add >> ignore) vs
                ok b
        | expr ->
            Fold (fun s e -> s && ok e) true expr
    ok e

let AlphaNormalize e =
    let rec norm env expr =
        match expr with
        | Lambda (this, vars, body) ->
            let (this, env) =
                match this with
                | None -> (None, env)
                | Some this ->
                    let v = Id this.Name
                    (Some v, Map.add this v env)
            let (vars, env) =
                (vars, ([], env))
                ||> List.foldBack (fun var (vars, env) ->
                    let v = Id var.Name
                    (v :: vars, Map.add var v env))
            Lambda (this, vars, norm env body)
        | Var v ->
            match env.TryFind v with
            | Some v -> Var v
            | None -> Var v
        | expr ->
            Transform (norm env) expr
    if IsAlphaNormalized e then e else norm Map.empty e

let GetFreeIds e =
    let bound = HashSet()
    let free = HashSet()
    let rec visit = function
        | Lambda (v, vs, b) ->
            if v.IsSome then
                bound.Add v.Value |> ignore
            Seq.iter (bound.Add >> ignore) vs
            visit b
        | Var v ->
            if bound.Contains v then () else
                free.Add v |> ignore
        | e ->
            Fold (fun () e -> visit e) () e
    visit (AlphaNormalize e)
    Set.ofSeq free

let IsGround e =
    let bound = HashSet()
    let rec ok = function
        | Lambda (v, vs, b) ->
            if v.IsSome then
                bound.Add v.Value |> ignore
            Seq.iter (bound.Add >> ignore) vs
            ok b
        | Var v ->
            bound.Contains v
        | e ->
            Fold (fun s e -> s && ok e) true e
    ok (AlphaNormalize e)

let Substitute f e =
    let bound = HashSet()
    let rec sub = function
        | Lambda (v, vs, b) ->
            if v.IsSome then
                bound.Add v.Value |> ignore
            Seq.iter (bound.Add >> ignore) vs
            Lambda (v, vs, sub b)
        | Var v ->
            if bound.Contains v then Var v else
                match f v with
                | None -> Var v
                | Some e -> e
        | e ->
            Transform sub e
    sub (AlphaNormalize e)

// Utilities ------------------------------------------------------------------

let ClosedSet values =
    let h = HashSet()
    for x in values do
        ignore (h.Add x)
    h.Contains

let ClosedMap pairs =
    let d = Dictionary()
    for (x, y) in pairs do
        d.[x] <- y
    fun x ->
        match d.TryGetValue x with
        | true, y -> Some y
        | _ -> None

let GlobalName prefs =
    match prefs with
    | Compact -> "$$"
    | Readable -> "Global"

let RuntimeName prefs =
    match prefs with
    | Compact -> "$"
    | Readable -> "Runtime"

module Scope =

    type T =
        private {
            Children : HashSet<T>
            Count : ref<int>
            Formatter : int -> S.Id
            Formals : HashSet<Id>
            Mode : Preferences
            Parent : option<T>
            Table : Dictionary<Id,S.Id>
            This : Id
            Used : HashSet<S.Id>
        }

    let New mode =
        let common = ["undefined"; "Infinity"; "NaN"; "IntelliFactory"]
        let used = [RuntimeName mode; GlobalName mode] @ common
        {
            Children = HashSet()
            Count = ref 0
            Formatter = Identifier.MakeFormatter()
            Formals = HashSet()
            Mode = mode
            Parent = None
            Table = Dictionary()
            This = Id()
            Used = HashSet used
        }

    let Use scope id =
        scope.Used.Add id |> ignore

    let private IsUsed id scope =
        let u x =
            x.Used.Contains id
        let rec pu x =
            match x.Parent with
            | Some p -> u p || pu p
            | None -> false
        let rec cu x =
            x.Children
            |> Seq.exists (fun x -> u x || cu x)
        u scope || pu scope || cu scope

    let private PickCompactName id scope =
        let rec pick k =
            let n = scope.Formatter k
            if IsUsed n scope then
                pick (k + 1)
            else
                (k, n)
        let (k, n) = pick !scope.Count
        incr scope.Count
        n

    let private PickReadableName (id: Id) scope =
        let fmt (x: string) (n: int) =
            if n = 0 then x else
                System.String.Format("{0}{1:x}", x, n)
        let n = defaultArg id.Name "_"
        let i = Identifier.MakeValid n
        let rec pick name k =
            let res = fmt name k
            if IsUsed res scope then
                pick name (k + 1)
            else
                res
        pick i 0

    let rec private Bind id scope =
        let name =
            match scope.Mode with
            | Compact -> PickCompactName id scope
            | Readable -> PickReadableName id scope
        scope.Table.[id] <- name
        Use scope name
        name

    let Expression scope id =
        let rec lookup scope id k =
            match scope.Table.TryGetValue id with
            | true, value ->
                Some (S.Var value)
            | _ ->
                if scope.This = id then
                    if k = 0 then Some S.This else
                        Some (S.Var (Bind id scope))
                else
                    match scope.Parent with
                    | Some p -> lookup p id (k + 1)
                    | None -> None
        match lookup scope id 0 with
        | None -> S.Var (Bind id scope)
        | Some v -> v

    let Id scope id =
        match Expression scope id with
        | S.This -> Bind id scope
        | S.Var x -> x
        | _ -> failwith "Unreachable."

    let Nest scope this formals =
        let nS =
            {
                Children = HashSet()
                Count = scope.Count
                Formatter = scope.Formatter
                Formals = HashSet(Seq.ofList formals)
                Parent = Some scope
                Mode = scope.Mode
                Table = Dictionary()
                This = defaultArg this (new Id())
                Used = HashSet()
            }
        scope.Children.Add nS |> ignore
        nS

    let Vars scope =
        [
            for KeyValue (k, v) in scope.Table do
                if scope.This = k then
                    yield (v, Some S.This)
                elif not (scope.Formals.Contains k) then
                    yield (v, None)
        ]

    let WithVars scope body =
        match Vars scope with
        | [] -> body
        | xs -> S.Action (S.Vars xs) :: body

// Optimization ---------------------------------------------------------------

/// Transforms local Let- or LetRecursive-bound curried lambda functions to
/// multi-argument functions when such transformations are possible - the
/// functions are strictly local, do not escape the scope, and are always
/// called with the correct number of arguments.
let Uncurry expression =
    let (|CurriedApplication|_|) expr =
        let rec loop n acc = function
            | Application (f, [x]) -> loop (n + 1) (x :: acc) f
            | f -> (n, f, acc)
        match loop 0 [] expr with
        | n, f, x when n > 0 -> Some (n, f, x)
        | _ -> None
    let (|CurriedLambda|_|) expr =
        let rec loop n acc = function
            | Lambda (None, [x], y) -> loop (n + 1) (x :: acc) y
            | b -> (n, List.rev acc, b)
        match loop 0 [] expr with
        | (n, vars, body) when n > 0 -> Some (n, vars, body)
        | _ -> None
    let arities = Dictionary()
    let rec analyze = function
        | CurriedApplication (k, Var f, _) ->
            match arities.TryGetValue f with
            | true, n ->
                if n <> k then
                    arities.[f] <- 0
            | false, _ ->
                arities.[f] <- k
        | Var x ->
            arities.[x] <- 0
        | expr ->
            Fold (fun () e -> analyze e) () expr
    let rec optimize (fs: Set<_>) expr =
        match expr with
        | CurriedApplication (_, Var f, xs) when fs.Contains f ->
            Application (Var f, List.map (optimize fs) xs)
        | Let (var, value, body) ->
            match value with
            | CurriedLambda (j, vars, b) when j > 1 ->
                match arities.TryGetValue var with
                | true, k when k = j ->
                    Let (
                        var,
                        Lambda (None, vars, optimize fs b),
                        optimize (Set.add var fs) body
                    )
                | _ ->
                    Let (var, optimize fs value, optimize fs body)
            | _ ->
                Let (var, optimize fs value, optimize fs body)
        | LetRecursive (bindings, body) ->
            let fs =
                (fs, bindings)
                ||> List.fold (fun s (v, b) ->
                    match b with
                    | CurriedLambda (j, vars, body) when j > 1 ->
                        match arities.TryGetValue v with
                        | true, k when k = j -> Set.add v s
                        | _ -> s
                    | _ -> s)
            LetRecursive (
                bindings
                |> List.map (fun (v, b) ->
                    if fs.Contains v then
                        match b with
                        | CurriedLambda (_, vs, b) ->
                            (v, Lambda (None, vs, optimize fs b))
                        | _ ->
                            failwith "Unreachable."
                    else
                        (v, optimize fs b)),
                optimize fs body
            )
        | _ ->
            Transform (optimize fs) expr
    analyze expression
    optimize Set.empty expression

/// Analyses an expression to find loop-like
/// LetRecursive expressions in O(N) time. A LetRecursive
/// expression can be compiled to a loop if all the variables that
/// it binds are either not used in the branches and body, or used
/// as tail call targets within the original scope.
let IsLoop expr =
    let rec analyze loops vars labels expr =
        let add x (a: HashSet<_>) =
            ignore (a.Add x)
        match expr with
        | Application (Var f, a) ->
            add f labels
            List.iter (analyze loops vars vars) a
        | IfThenElse (cond, body, alt) ->
            analyze loops vars vars cond
            analyze loops vars labels body
            analyze loops vars labels alt
        | Lambda (_, _, body) ->
            analyze loops vars vars body
        | Let (_, value, body) ->
            analyze loops vars vars value
            analyze loops vars labels body
        | LetRecursive (bindings, body) ->
            let jumps = HashSet()
            analyze loops vars jumps body
            let vs =
                bindings
                |> List.map (fun (var, branch) ->
                    match branch with
                    | Lambda (None, _, body) -> body
                    | _ -> branch
                    |> analyze loops vars jumps
                    var)
            let isLoop =
                let ok (var, body) =
                    match body with
                    | Lambda (None, _, _) -> not (vars.Contains var)
                    | _ -> false
                List.forall ok bindings
            if isLoop then
                add expr loops
                labels.UnionWith jumps
            else
                vars.UnionWith jumps
        | Sequential (x, y) ->
            analyze loops vars vars x
            analyze loops vars labels y
        | Var v ->
            add v vars
        | expr ->
            Fold (fun () e -> analyze loops vars vars e) () expr
    let loops = HashSet()
    let vars = HashSet()
    analyze loops vars vars expr
    loops.Contains

/// Compiles LetRecursive expressions to loops when possible.
let RemoveLoops expr =
    let isLoop = IsLoop expr
    let labels = Dictionary()
    let slots = Dictionary()
    let i x = Constant (Integer (int64 x))
    let ( ++ ) a b = Sequential (a, b)
    let rec t ret expr =
        match expr with
        | Application (Var f, a) when labels.ContainsKey f ->
            let (args, p) = labels.[f]
            let rec g k bs ss = function
                | [] ->
                    let s =
                        (ss, FieldSet (args, i 0, i p))
                        ||> List.foldBack (++)
                    (bs, s)
                    ||> List.foldBack (fun (k, v) x -> Let (k, v, x))
                | x :: xs ->
                    let v = Id ()
                    let bs = (v, t id x) :: bs
                    let ss = FieldSet (args, i k, Var v) :: ss
                    g (k + 1) bs ss xs
            g 1 [] [] a
        | IfThenElse (cond, body, alt) ->
            IfThenElse (t id cond, t ret body, t ret alt)
        | Lambda (this, formals, body) ->
            ret (Lambda (this, formals, t id body))
        | Let (var, value, body) ->
            Let (var, t id value, t ret body)
        | LetRecursive (bindings, body) ->
            if isLoop expr then
                loop ret bindings body
            else
                let bindings = [for (k, v) in bindings -> (k, t id v)]
                LetRecursive (bindings, t ret body)
        | Sequential (x, y) ->
            Sequential (t id x, t ret y)
        | Var v ->
            match slots.TryGetValue v with
            | true, (_, 0) -> Constant Undefined
            | true, (a, k) -> FieldGet (a, i k)
            | _ -> expr
            |> ret
        | _ ->
            ret (Transform (t id) expr)
    and loop ret bindings body =
        let argId = Id "loop"
        let args = Var argId
        bindings
        |> List.iteri (fun i (k, v) ->
            labels.[k] <- (args, i + 1)
            match v with
            | Lambda (None, formals, body) ->
                formals
                |> List.iteri (fun j v ->
                    slots.[v] <- (args, j + 1))
            | _ ->
                failwith "Unreachable.")
        let exit x =
            FieldSet (args, i 0, i 0)
            ++ FieldSet (args, i 1, x)
        let next = FieldGet (args, i 0)
        let switch x cases =
            let rec f k = function
                | [] -> Unary (UnaryOperator.``void``, x)
                | [c] -> c
                | c::cs -> IfThenElse (Binary (x, B.``===``, i k),
                                       c, f (k+1) cs)
            f 1 cases
        let getBody = function
            | (_, Lambda (None, _, b)) -> b
            | _ -> failwith "Unreachable."
        let states = List.map (getBody >> t exit) bindings
        let cycle = WhileLoop (next, switch next states)
        let res = ret (FieldGet (args, i 1))
        Let (argId, NewArray [], t exit body ++ cycle ++ res)
    t id expr

let rec removeRedexes expr =
    let apply f xs =
        match f with
        | Lambda (None, args, body)
            when List.length args = List.length xs ->
            let bind key value body = Let (key, value, body)
            List.foldBack2 bind args xs body
        | _ -> Application (f, xs)
    match expr with
    | Application (f, xs) ->
        let f = removeRedexes f
        let xs = List.map removeRedexes xs
        match f with
        | Let (key, var, value) -> Let (key, var, apply value xs)
        | _ -> apply f xs
    | _ -> Transform removeRedexes expr

let unalias expr =
    let rec norm env expr =
        match expr with
        | Let (var, value, body) ->
            match value with
            | Constant _ | Var _ ->
                norm (Map.add var (norm env value) env) body
            | _ ->
                let v = Id var.Name
                Let (v, norm env value, norm (Map.add var (Var v) env) body)
        | Lambda (this, vars, body) ->
            let (this, env) =
                match this with
                | None -> (None, env)
                | Some this ->
                    let v = Id this.Name
                    (Some v, Map.add this (Var v) env)
            let (vars, env) =
                (vars, ([], env))
                ||> List.foldBack (fun var (vars, env) ->
                    let v = Id var.Name
                    (v :: vars, Map.add var (Var v) env))
            Lambda (this, vars, norm env body)
        | Var id ->
            match env.TryFind id with
            | Some expr -> expr
            | None -> expr
        | expr ->
            Transform (norm env) expr
    norm Map.empty expr

let rec eta expr =
    match expr with
    | Lambda (None, [x], Application (Var f, [Var y])) when x = y -> Var f
    | Lambda (t, v, x) -> Lambda (t, v, eta x)
    | _ -> Transform eta expr

let Optimize expr =
    AlphaNormalize expr
    |> Uncurry
    |> RemoveLoops
    |> removeRedexes
    |> unalias
    |> eta

// Elaboration ----------------------------------------------------------------

let ElaborateBinaryOperator op =
    match op with
    | B.``!==`` -> SB.``!==``
    | B.``!=`` -> SB.``!=``
    | B.``%`` -> SB.``%``
    | B.``&&`` -> SB.``&&``
    | B.``&`` -> SB.``&``
    | B.``*`` -> SB.``*``
    | B.``+`` -> SB.``+``
    | B.``-`` -> SB.``-``
    | B.``/`` -> SB.``/``
    | B.``<<`` -> SB.``<<``
    | B.``<=`` -> SB.``<=``
    | B.``<`` -> SB.``<``
    | B.``===`` -> SB.``===``
    | B.``==`` -> SB.``==``
    | B.``>=`` -> SB.``>=``
    | B.``>>>`` -> SB.``>>>``
    | B.``>>`` -> SB.``>>``
    | B.``>`` -> SB.``>``
    | B.``^`` -> SB.``^``
    | B.``in`` -> SB.``in``
    | B.``instanceof`` -> SB.``instanceof``
    | B.``|`` -> SB.``|``
    | _ -> SB.``||``

let ElaborateUnaryOperator op =
    match op with
    | U.``~`` -> SU.``~``
    | U.``-`` -> SU.``-``
    | U.``!`` -> SU.``!``
    | U.``+`` -> SU.``+``
    | U.``typeof`` -> SU.``typeof``
    | _ -> SU.``void``

let ElaborateConstant c =
    let ne x = S.Unary (S.UnaryOperator.``-``, x)
    let elaborateDouble x =
        match x with
        | x when System.Double.IsNaN x -> S.Var "NaN"
        | x when System.Double.IsPositiveInfinity x -> S.Var "Infinity"
        | x when System.Double.IsNegativeInfinity x -> ne (S.Var "Infinity")
        | x when x >= 0. -> S.Constant (S.Number (string x))
        | _ -> ne (S.Constant (S.Number (string (abs x))))
    let elaborateInteger x =
        if x >= 0L then S.Constant (S.Number (string x))
                   else ne (S.Constant (S.Number (string (abs x))))
    match c with
    | Double x -> elaborateDouble x
    | False -> S.Constant S.False
    | Integer x -> elaborateInteger x
    | Null -> S.Constant S.Null
    | String x -> S.Constant (S.String x)
    | True -> S.Constant S.True
    | Undefined -> S.Var "undefined"

let CapturesVariables expr =
    not (GetFreeIds expr).IsEmpty

type ExpressionMode =
    | Ignored
    | Used

type StatementMode =
    | Discard
    | Return

let ToProgram prefs expr =
    let scope = Scope.New prefs
    let lib = RuntimeName prefs
    let glob = GlobalName prefs
    let kw s = (S.Var lib).[!~(S.String s)]
    let rec toExpr scope mode expr =
        let (!) : _ -> S.Expression = toExpr scope mode
        let (!-) : _ -> S.Expression = toExpr scope Ignored
        let (!!) = List.map (!)
        let (!^) = Scope.Expression scope
        match expr with
        | Application (FieldGet (t, m), a) ->
            (!-t).[!-m]?call.[!~S.Null :: !!a]
        | Application (f, a) ->
            (!f).[!!a]
        | Binary (x, o, y) ->
            S.Binary (!x, ElaborateBinaryOperator o, !y)
        | Call (t, m, a) ->
            (!t).[!m].[!!a]
        | Constant c ->
            ElaborateConstant c
        | FieldDelete (t, f) ->
            let d = (!t).[!f].Delete
            match mode with
            | Ignored -> d
            | Used -> d.Void
        | FieldGet (t, f) ->
            (!t).[!f]
        | FieldSet (t, f, v) ->
            let s = (!t).[!f] ^= !v
            match mode with
            | Ignored -> s
            | Used -> s.Void
        | ForEachField (i, o, b) ->
            let body = Lambda (None, [i], b.Void)
            S.Application (kw "ForEach", [!body])
        | ForIntegerRangeLoop (i, l, h, b) ->
            let body = Lambda (None, [i], b.Void)
            S.Application (kw "For", [!l; !h; !body])
        | Global name ->
            (S.Var glob, name)
            ||> Seq.fold (fun s x -> s.[!~(S.String x)])
        | IfThenElse (c, t, e) ->
            S.Conditional (!c, !t, !e)
        | Lambda (this, vars, body) ->
            toLambda scope this vars body
        | Let (var, value, body) ->
            S.Binary (!^var ^= !value, S.BinaryOperator.``,``, !body)
        | LetRecursive (bindings, body) ->
            for (var, _) in bindings do
                ignore !^var
            (bindings, !body)
            ||> List.foldBack (fun (var, value) body ->
                match value with
                | Constant Undefined -> body
                | _ ->
                    S.Binary (!^var ^= !value,
                        S.BinaryOperator.``,``, body))
        | New (f, a) ->
            S.New (!f, !!a)
        | NewArray a ->
            S.NewArray [for e in a -> Some !e]
        | NewObject o ->
            S.NewObject [for (k, v) in o -> (k, !v)]
        | NewRegex x ->
            S.NewRegex x
        | Runtime ->
            S.Var lib
        | Sequential (x, y) ->
            S.Binary (!-x, S.BinaryOperator.``,``, !y)
        | Throw x ->
            S.Application (kw "Throw", [!x])
        | TryFinally (block, guard) ->
            let block = Lambda (None, [], block)
            let guard = Lambda (None, [], guard.Void)
            S.Application (kw "TryFinally", [!block; !guard])
        | TryWith (block, var, guard) ->
            let block = Lambda (None, [], block)
            let guard = Lambda (None, [var], guard)
            S.Application (kw "Try", [!block; !guard])
        | Unary (UnaryOperator.``void``, x) ->
            match mode with
            | Ignored -> !-x
            | Used ->
                match !-x with
                | S.Unary (S.UnaryOperator.``void``, _) as e -> e
                | e -> e.Void
        | Unary (o, x) ->
            S.Unary (ElaborateUnaryOperator o, !x)
        | Var v ->
            !^v
        | WhileLoop (c, b) ->
            let c = Lambda (None, [], c)
            let b = Lambda (None, [], b.Void)
            S.Application (kw "While", [!c; !b])
    and toLambda scope this vars body =
        let scope = Scope.Nest scope this vars
        let formals = List.map (Scope.Id scope) vars
        let mode =
            match body with
            | Unary (UnaryOperator.``void``, body) -> Discard
            | body -> Return
        let body =
            toStmts scope mode [] body
            |> List.rev
            |> List.map S.Action
        S.Lambda (None, formals, Scope.WithVars scope body)
    and toStmts scope (mode: StatementMode) acc expr =
        let eS = toStmts scope
        let eB mode = S.Block << List.rev << eS mode []
        let eE = toExpr scope
        let eV = Scope.Expression scope
        let eID = Scope.Id scope
        match expr with
        | FieldDelete _ | FieldSet _ ->
            S.Ignore (eE Ignored expr) :: acc
        | ForEachField (i, o, b) ->
            if CapturesVariables b
            then S.Ignore (eE Ignored expr) :: acc
            else S.ForIn (eV i, eE Used o, eB Discard b) :: acc
        | ForIntegerRangeLoop (i, l, h, b) ->
            if CapturesVariables b then
                S.Ignore (eE Ignored expr) :: acc
            else
                let v = eV i
                let e1 = v ^=  eE Used l
                let e2 = v &<= eE Used h
                let e3 = S.Postfix(v, S.PostfixOperator.``++``)
                let body = eB Discard b
                S.For (Some e1, Some e2, Some e3, body) :: acc
        | IfThenElse (c, t, e) ->
            S.If (eE Used c, eB mode t, eB mode e) :: acc
        | Let (var, value, body) ->
            eS mode (S.Ignore (eV var ^= eE Used value) :: acc) body
        | LetRecursive (bindings, body) ->
            for (k, _) in bindings do
                ignore (eV k)
            let acc =
                (acc, bindings)
                ||> List.fold (fun acc (var, value) ->
                    match value with
                    | Constant Undefined -> acc
                    | _ -> S.Ignore (eV var ^= eE Used value) :: acc)
            eS mode acc body
        | Sequential (x, y) -> eS mode (eS Discard acc x) y
        | Throw x -> S.Throw (eE Used x) :: acc
        | TryFinally (block, guard) ->
            S.TryFinally (eB mode block, eB Discard guard) :: acc
        | TryWith (block, var, guard) ->
            S.TryWith (eB mode block, eID var, eB mode guard, None) :: acc
        | Unary (UnaryOperator.``void``, x) ->
            S.Ignore (eE Ignored x) :: acc
        | WhileLoop (c, b) ->
            if CapturesVariables b
                then S.Ignore (eE Ignored expr) :: acc
                else S.While (eE Used c, eB Discard b) :: acc
        | _ ->
            match expr, mode with
            | Constant Undefined, _
            | Constant _, Discard -> acc
            | _, Discard -> S.Ignore (eE Ignored expr) :: acc
            | _, Return -> S.Return (Some (eE Used expr)) :: acc
    let main = toStmts scope Discard [] expr
    let vars =
        S.Vars ((glob, Some S.This)
        :: (lib, Some S.This?IntelliFactory?Runtime)
        :: Scope.Vars scope)
    vars :: List.rev main
    |> List.map S.Action

exception RecognitionError

let Recognize expr =
    let rec rE this (env: Map<_,_>) used expr =
        let (!) = rE this env true
        let (!!) = List.map (!)
        match expr with
        | S.Application (S.Binary (t, SB.``.``, f), xs) ->
            Call (!t, !f, !!xs)
        | S.Application (f, xs) ->
            Application (!f, !!xs)
        | S.Binary (x, o, y) ->
            let parse = function
                | SB.``!==`` -> Some B.``!==``
                | SB.``!=`` -> Some B.``!=``
                | SB.``%`` -> Some B.``%``
                | SB.``&&`` -> Some B.``&&``
                | SB.``&`` -> Some B.``&``
                | SB.``*`` -> Some B.``*``
                | SB.``+`` -> Some B.``+``
                | SB.``-`` -> Some B.``-``
                | SB.``/`` -> Some B.``/``
                | SB.``<<`` -> Some B.``<<``
                | SB.``<=`` -> Some B.``<=``
                | SB.``<`` -> Some B.``<``
                | SB.``===`` -> Some B.``===``
                | SB.``==`` -> Some B.``==``
                | SB.``>=`` -> Some B.``>=``
                | SB.``>>>`` -> Some B.``>>>``
                | SB.``>>`` -> Some B.``>>``
                | SB.``>`` -> Some B.``>``
                | SB.``^`` -> Some B.``^``
                | SB.``in`` -> Some B.``in``
                | SB.``instanceof`` -> Some B.``instanceof``
                | SB.``|`` -> Some B.``|``
                | SB.``||`` -> Some B.``||``
                | _ -> None
            match o with
            | SB.``.`` -> FieldGet (!x, !y)
            | SB.``,`` -> Sequential (rE this env false x, !y)
            | SB.``=`` ->
                match x with
                | S.Binary (a, SB.``.``, b) ->
                    if not used then
                        FieldSet (!a, !b, !y)
                    else
                        let target = Id()
                        let field = Id ()
                        let value = Id()
                        let it = Var value
                        let assign = FieldSet (Var target, Var field, it)
                        Let (target, !a,
                            Let (field, !b,
                                Let (value, !y,
                                    Sequential (assign, it))))
                | _ -> raise RecognitionError
            | _ ->
                match parse o with
                | Some o -> Binary (!x, o, !y)
                | None -> raise RecognitionError
        | S.Conditional (a, b, c) ->
            IfThenElse (!a, !b, !c)
        | S.Constant c ->
            let rN n =
                match System.Int64.FromString n with
                | Some i -> !~ (Integer i)
                | None ->
                    match System.Double.FromString n with
                    | Some i -> !~ (Double i)
                    | None -> raise RecognitionError
            match c with
            | S.False -> !~False
            | S.Null -> !~Null
            | S.Number n -> rN n
            | S.String x -> !~(String x)
            | S.True -> !~True
        | S.Lambda (name, vars, body) ->
            match name with
            | None ->
                let this = Id()
                let env =
                    (env, vars)
                    ||> List.fold (fun env v -> Map.add v (Id v) env)
                let body =
                    body
                    |> List.map (function
                        | S.Action s -> s
                        | _ -> raise RecognitionError)
                    |> S.Block
                Lambda (
                    Some this,
                    [for v in vars -> env.[v]],
                    rS (Some this) env true body
                )
            | Some _ ->
                raise RecognitionError
        | S.NewArray x ->
            let f = function
                | None -> !~Undefined
                | Some x -> !x
            NewArray (List.map f x)
        | S.NewObject o ->
            NewObject [for (k, v) in o -> (k, !v)]
        | S.New (x, xs) ->
            New (!x, !!xs)
        | S.Postfix _ ->
            raise RecognitionError
        | S.Unary (o, e) ->
            match o with
            | SU.``~`` -> Unary (U.``~``, !e)
            | SU.``-`` -> Unary (U.``-``, !e)
            | SU.``!`` -> Unary (U.``!``, !e)
            | SU.``+`` -> Unary (U.``+``, !e)
            | SU.``typeof`` -> Unary (U.``typeof``, !e)
            | SU.``void`` -> Unary (U.``void``, rE this env false e)
            | _ -> raise RecognitionError
        | S.NewRegex x ->
            NewRegex x
        | S.This ->
            match this with
            | Some id -> Var id
            | None -> raise RecognitionError
        | S.Var id ->
            match env.TryFind id with
            | Some id -> Var id
            | None -> Global [id]
    and rS this (env: Map<_,_>) tail (stmt: S.Statement) =
        let (!) = rE this env true
        let rS = rS this
        match stmt with
        | S.Block x ->
            let rec f acc = function
                | [] -> acc
                | [x] -> Sequential (acc, rS env tail x)
                | x::xs -> f (Sequential (acc, rS env false x)) xs
            and g = function
                | [] -> !~Undefined
                | [x] -> rS env tail x
                | x::xs -> f (rS env false x) xs
            g (Seq.toList x)
        | S.Empty ->
            !~Undefined
        | S.If (a, b, c) ->
            IfThenElse (!a, rS env tail b, rS env tail c)
        | S.Ignore x ->
            let x = rE this env false x
            if tail then x.Void else x
        | S.Return x ->
            if tail then
                match x with
                | None -> !~Undefined
                | Some x -> !x
            else
                raise RecognitionError
        | S.Throw e ->
            Throw !e
        | S.TryFinally (a, b) ->
            TryFinally (rS env tail a, rS env false b)
        | S.TryWith (a, b, c, d) ->
            match d with
            | None ->
                let id = Id b
                let envc = Map.add b id env
                TryWith (rS env tail a, id, rS envc tail c)
            | Some _ ->
                raise RecognitionError
        | S.While (e, s) ->
            WhileLoop (!e, rS env false s)
        | S.Break _
        | S.Continue _
        | S.Debugger
        | S.Do _ 
        | S.For _
        | S.ForIn _
        | S.ForVarIn _
        | S.ForVars _
        | S.Labelled _
        | S.Switch _
        | S.Vars _
        | S.With _ ->
            raise RecognitionError
    try Some (rE None Map.empty true expr) with RecognitionError -> None
