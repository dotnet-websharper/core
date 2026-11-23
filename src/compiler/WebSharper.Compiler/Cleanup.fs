// $begin{copyright}
//
// This file is part of WebSharper
//
// Copyright (c) 2008-2026 IntelliFactory
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

/// Removes variables left over after breaking expressions into statements:
/// aliases of other variables, temporaries used only once or never read,
/// and F# ref cells that never leave the function.
module WebSharper.Compiler.Cleanup

open System.Collections.Generic
open WebSharper.Core
open WebSharper.Core.AST

module I = IgnoreSourcePos

/// A variable created by the compiler, not named in source code
let private isTemp (v: Id) =
    not v.HasStrongName &&
    match v.Name with
    | None -> true
    | Some n -> n.StartsWith "$"

/// A variable, possibly with type casts added for TypeScript
let rec private (|CastVar|_|) e =
    match IgnoreExprSourcePos e with
    | Var v -> Some v
    | Cast (_, CastVar v) -> Some v
    | _ -> None

/// The index of the first element of an array (or the field named 0)
let private (|ZeroIndex|_|) e =
    match IgnoreExprSourcePos e with
    | Value (Int 0) | Value (String "0") | Value (JSNumber "0") -> Some ()
    | Value (Double d) when d = 0.0 -> Some ()
    | _ -> None

/// A single-element array, as used for F# ref cells, possibly with type casts
let rec private (|NewRefCell|_|) e =
    match IgnoreExprSourcePos e with
    | NewArray [ init ]
    | NewTuple ([ init ], _) -> Some init
    | Cast (_, NewRefCell init) -> Some init
    | _ -> None

let private sameName (a: Id) (b: Id) =
    match a.Name, b.Name with
    | Some an, Some bn -> an = bn
    | _ -> false

/// Collects where variables are declared, assigned and read
type private VarInfo() =
    inherit Visitor()

    let reads = Dictionary<Id, int>()
    let assigned = HashSet<Id>()
    let assignCount = Dictionary<Id, int>()
    let declScope = Dictionary<Id, obj>()
    let assignScopes = Dictionary<Id, HashSet<obj>>(HashIdentity.Structural)
    let scopes = Stack<obj>()
    do scopes.Push(obj())

    let declare (v: Id) =
        declScope[v] <- scopes.Peek()

    let assign (v: Id) =
        assigned.Add v |> ignore
        assignCount[v] <- (match assignCount.TryGetValue v with | true, n -> n + 1 | _ -> 1)
        match assignScopes.TryGetValue v with
        | true, s -> s.Add(scopes.Peek()) |> ignore
        | _ -> assignScopes[v] <- HashSet([ scopes.Peek() ], HashIdentity.Reference)

    member this.Reads(v: Id) =
        match reads.TryGetValue v with
        | true, n -> n
        | _ -> 0

    member this.AddReads(v: Id, n: int) =
        reads[v] <- this.Reads v + n

    member this.IsAssigned(v: Id) = assigned.Contains v

    member this.IsDeclared(v: Id) = declScope.ContainsKey v

    /// An assignment became the declaration of the variable
    member this.AssignmentToDeclaration(v: Id) =
        match assignCount.TryGetValue v with
        | true, 1 ->
            assignCount.Remove v |> ignore
            assigned.Remove v |> ignore
        | true, n -> assignCount[v] <- n - 1
        | _ -> ()

    member this.SetAssigned(v: Id) =
        assigned.Add v |> ignore
        assignScopes[v] <- HashSet([ obj() ], HashIdentity.Reference)

    /// The variable is assigned only directly in the function that declares it,
    /// which is the function given by `scope`
    member this.AssignedOnlyIn(v: Id, scope: obj) =
        match declScope.TryGetValue v with
        | true, d when obj.ReferenceEquals(d, scope) ->
            match assignScopes.TryGetValue v with
            | true, s -> s |> Seq.forall (fun x -> obj.ReferenceEquals(x, d))
            | _ -> true
        | _ -> false

    member this.RootScope = scopes.Peek()

    override this.VisitVar(v) =
        reads[v] <- this.Reads v + 1

    override this.VisitExprStatement(e) =
        // pure expression statements are removed
        if not (isPureExpr e) then this.VisitExpression e

    override this.VisitVarSet(v, e) =
        assign v
        this.VisitExpression e

    override this.VisitMutatingUnary(op, e) =
        match IgnoreExprSourcePos e with
        | Var v -> assign v
        | _ -> ()
        this.VisitExpression e

    override this.VisitMutatingBinary(a, op, b) =
        match IgnoreExprSourcePos a with
        | Var v -> assign v
        | _ -> ()
        this.VisitExpression a
        this.VisitExpression b

    override this.VisitVarDeclaration(v, e) =
        declare v
        this.VisitExpression e

    override this.VisitLet(v, e, b) =
        declare v
        this.VisitExpression e
        this.VisitExpression b

    override this.VisitNewVar(v, e) =
        declare v
        this.VisitExpression e

    override this.VisitForOf(v, e, b) =
        declare v
        this.VisitExpression e
        this.VisitStatement b

    override this.VisitForIn(v, e, b) =
        declare v
        this.VisitExpression e
        this.VisitStatement b

    override this.VisitTryWith(b, v, c) =
        this.VisitStatement b
        v |> Option.iter declare
        this.VisitStatement c

    override this.VisitFunction(args, thisVar, _, body) =
        scopes.Push(box body)
        args |> List.iter declare
        thisVar |> Option.iter declare
        this.VisitStatement body
        scopes.Pop() |> ignore

    override this.VisitFuncDeclaration(f, args, thisVar, body, _) =
        declare f
        scopes.Push(box body)
        args |> List.iter declare
        thisVar |> Option.iter declare
        this.VisitStatement body
        scopes.Pop() |> ignore

/// Checks that all occurrences of a variable are reads and writes of its first element
type private OnlyFirstItemAccess(v: Id) =
    inherit Visitor()

    let mutable ok = true

    member this.Ok = ok

    override this.VisitItemGet(o, i, p) =
        match o, i with
        | CastVar x, ZeroIndex when x = v -> ()
        | _ -> base.VisitItemGet(o, i, p)

    override this.VisitItemSet(o, i, e) =
        match o, i with
        | CastVar x, ZeroIndex when x = v -> this.VisitExpression e
        | _ -> base.VisitItemSet(o, i, e)

    override this.VisitId(x) =
        if x = v then ok <- false

/// Replaces the first element of a single-element array with a variable
type private FirstItemToVar(v: Id, nv: Id) =
    inherit Transformer()

    override this.TransformItemGet(o, i, p) =
        match o, i with
        | CastVar x, ZeroIndex when x = v -> Var nv
        | _ -> base.TransformItemGet(o, i, p)

    override this.TransformItemSet(o, i, e) =
        match o, i with
        | CastVar x, ZeroIndex when x = v -> VarSet(nv, this.TransformExpression e)
        | _ -> base.TransformItemSet(o, i, e)

/// Checks if an expression or statement assigns a variable, including within functions
type private Assigns(v: Id) =
    inherit Visitor()

    let mutable found = false

    member this.Found = found

    override this.VisitVarSet(x, e) =
        if x = v then found <- true
        this.VisitExpression e

    override this.VisitMutatingUnary(op, e) =
        match IgnoreExprSourcePos e with
        | Var x when x = v -> found <- true
        | _ -> ()
        this.VisitExpression e

    override this.VisitMutatingBinary(a, op, b) =
        match IgnoreExprSourcePos a with
        | Var x when x = v -> found <- true
        | _ -> ()
        this.VisitExpression a
        this.VisitExpression b

let private assignsSt v (s: Statement) =
    let a = Assigns(v)
    a.VisitStatement s
    a.Found

let private assignsE v (e: Expression) =
    let a = Assigns(v)
    a.VisitExpression e
    a.Found

/// Counts reads of a variable, and the ones inside functions
type private CountReads(v: Id) =
    inherit Visitor()

    let mutable count = 0
    let mutable inFunc = 0
    let mutable countInFunc = 0

    member this.Count = count
    member this.CountInFunc = countInFunc

    override this.VisitVar(x) =
        if x = v then
            count <- count + 1
            if inFunc > 0 then countInFunc <- countInFunc + 1

    override this.VisitFunction(a, t, r, b) =
        inFunc <- inFunc + 1
        base.VisitFunction(a, t, r, b)
        inFunc <- inFunc - 1

    override this.VisitFuncDeclaration(f, a, t, b, g) =
        inFunc <- inFunc + 1
        base.VisitFuncDeclaration(f, a, t, b, g)
        inFunc <- inFunc - 1

let private countReads v (ss: seq<Statement>) =
    let c = CountReads(v)
    for s in ss do c.VisitStatement s
    c.Count, c.CountInFunc

let rec private (|Decl|_|) s =
    match s with
    | VarDeclaration (v, e) -> Some (v, e)
    | StatementSourcePos (_, Decl d) -> Some d
    | _ -> None

let rec private (|SetVar|_|) s =
    match s with
    | ExprStatement (I.VarSet (v, e))
    | ExprStatement (I.Unary (UnaryOperator.``void``, I.VarSet (v, e))) -> Some (v, e)
    | StatementSourcePos (_, SetVar d) -> Some d
    | _ -> None

/// The expression a statement evaluates first, before anything else it contains
let rec private firstEvaluated s =
    match s with
    | ExprStatement e
    | VarDeclaration (_, e)
    | Return e
    | Throw e
    | If (e, _, _) -> Some e
    | StatementSourcePos (_, s) -> firstEvaluated s
    | _ -> None

let private isSmallLiteral (v: Literal) =
    match v with
    | String s -> s.Length <= 20
    | ByteArray _ -> false
    | _ -> true

type private Cleaner(info: VarInfo) =
    inherit Transformer()

    let scopes = Stack<obj>()
    do scopes.Push(info.RootScope)
    let renames = Dictionary<Id, Id>()

    let isConst (v: Id) = info.IsDeclared v && not (info.IsAssigned v)

    /// Compiler-generated variables that are assigned but never read
    let isDead (v: Id) = isTemp v && info.Reads v = 0 && info.IsDeclared v

    member this.Renames = renames

    member this.ResolvedRenames() =
        let rec resolve v =
            match renames.TryGetValue v with
            | true, w -> resolve w
            | _ -> v
        let all = Dictionary<Id, Id>()
        for KeyValue(k, _) in renames do
            all[k] <- resolve k
        all

    override this.TransformFunction(args, thisVar, typ, body) =
        scopes.Push(box body)
        let res = base.TransformFunction(args, thisVar, typ, body)
        scopes.Pop() |> ignore
        res

    override this.TransformFuncDeclaration(f, args, thisVar, body, gen) =
        scopes.Push(box body)
        let res = base.TransformFuncDeclaration(f, args, thisVar, body, gen)
        scopes.Pop() |> ignore
        res

    override this.TransformVarSet(v, e) =
        if isDead v then this.TransformExpression e
        else base.TransformVarSet(v, e)

    override this.TransformVarDeclaration(v, e) =
        if isDead v then this.TransformExprStatement e
        else base.TransformVarDeclaration(v, e)

    override this.TransformExprStatement(e) =
        let e = this.TransformExpression e
        if isPureExpr e then Empty else ExprStatement (removePureParts e)

    override this.TransformSequential(es) =
        match List.map this.TransformExpression es |> List.rev with
        | [] -> Undefined
        | last :: init ->
            // the value comes from the last item, only the others can be dropped
            match CombineExpressions (init |> List.rev |> List.map removePureParts) with
            | I.Undefined -> last
            | I.Sequential i -> Sequential (i @ [ last ])
            | i -> Sequential [ i; last ]

    override this.TransformIf(c, a, b) =
        match this.TransformStatement a, this.TransformStatement b with
        | Empty, Empty -> this.TransformExprStatement c
        | Empty, b -> If (Unary (UnaryOperator.``!``, this.TransformExpression c), b, Empty)
        | a, b -> If (this.TransformExpression c, a, b)

    override this.TransformBlock(ss) =
        // nested blocks are written without braces, their variables are in scope after them
        let rec flat (s: Statement) =
            match s with
            | Block ss
            | StatementSourcePos (_, Block ss) -> List.collect flat ss
            | s -> [ s ]
        ss |> List.collect flat |> List.map this.TransformStatement |> this.CleanBlock |> Block

    member this.CleanBlock(ss: Statement list) =
        // sequences are written as separate statements, split them so that each part can be looked at
        let rec split (s: Statement) =
            let parts (es: Expression list) (last: Expression -> Statement) =
                match List.rev es with
                | l :: init when not init.IsEmpty -> 
                    (List.rev init |> List.collect (fun e -> split (ExprStatement e))) @ [ last l ]
                | _ -> [ s ]
            match s with
            | ExprStatement (I.Sequential es) -> parts es ExprStatement
            | Return (I.Sequential es) -> parts es Return
            | Throw (I.Sequential es) -> parts es Throw
            | VarDeclaration (v, I.Sequential es) -> parts es (fun l -> VarDeclaration (v, l))
            | Block b -> b |> List.collect split
            | Empty -> []
            | _ -> [ s ]
        let arr = ResizeArray(ss |> List.collect split)
        let rest i = Seq.init (arr.Count - i - 1) (fun j -> arr[i + 1 + j])
        let mapRest i (tr: Transformer) =
            for j in i + 1 .. arr.Count - 1 do
                arr[j] <- tr.TransformStatement arr[j]
        let scope = scopes.Peek()
        let occurs v s = CountVarOccurence(v).GetForStatement(s) > 0
        // declare variables with their first value: `let x; ...; x = v` to `const x = v`
        for i in 0 .. arr.Count - 1 do
            match arr[i] with
            | Decl (v, I.Undefined) ->
                match seq { i + 1 .. arr.Count - 1 } |> Seq.tryFind (fun j -> occurs v arr[j]) with
                | Some j ->
                    match arr[j] with
                    | SetVar (w, e) when w = v && not (occurs v (ExprStatement e)) ->
                        arr[j] <- VarDeclaration(v, e)
                        arr[i] <- Empty
                        info.AssignmentToDeclaration v
                    | _ -> ()
                | None -> ()
            | _ -> ()
        for i in arr.Count - 1 .. -1 .. 0 do
            match arr[i] with
            | Decl (a, value) when not a.HasStrongName && isConst a && not (renames.ContainsKey a) ->
                match IgnoreExprSourcePos value with
                | NewRefCell init when (let c = OnlyFirstItemAccess(a) in (for s in rest i do c.VisitStatement s); c.Ok) ->
                    // a ref cell that does not leave the function: a mutable variable can do the same
                    let nv = Id.New(?name = a.Name, mut = true)
                    info.SetAssigned nv
                    arr[i] <- VarDeclaration(nv, init)
                    mapRest i (FirstItemToVar(a, nv))
                | CastVar b when b <> a && info.IsDeclared b ->
                    let hasCast = match IgnoreExprSourcePos value with Var _ -> false | _ -> true
                    let aReads, _ = countReads a (rest i)
                    if not (info.IsAssigned b) then
                        // both variables have the same value everywhere, keep the one with the better name
                        if isTemp a || sameName a b || b.HasStrongName then
                            arr[i] <- Empty
                            mapRest i (SubstituteVar(a, value))
                            info.AddReads(b, aReads - 1)
                        elif not hasCast && (isTemp b || info.Reads b = 1) && not b.IsRest && not b.IsOptional then
                            arr[i] <- Empty
                            renames[b] <- a
                            info.AddReads(a, info.Reads b - 1)
                    elif (isTemp a || sameName a b) && info.AssignedOnlyIn(b, scope) then
                        // a copy of a variable that changes later: it can be used directly
                        // while it is not changed, `b = f(a)` included
                        let firstAssign =
                            seq { i + 1 .. arr.Count - 1 } |> Seq.tryFind (fun j -> assignsSt b arr[j])
                        let lastUse =
                            match firstAssign with
                            | None -> Some (arr.Count - 1)
                            | Some j ->
                                let later, _ = countReads a (Seq.init (arr.Count - j - 1) (fun k -> arr[j + 1 + k]))
                                if later > 0 then None else
                                match arr[j] with
                                | SetVar (v, e) when v = b && not (assignsE b e) -> Some j
                                | s when fst (countReads a [ s ]) = 0 -> Some j
                                | _ -> None
                        match lastUse with
                        | Some l ->
                            let _, inFunc = countReads a (Seq.init (l - i) (fun k -> arr[i + 1 + k]))
                            if inFunc = 0 then
                                arr[i] <- Empty
                                let tr = SubstituteVar(a, value)
                                for j in i + 1 .. l do
                                    arr[j] <- tr.TransformStatement arr[j]
                                info.AddReads(b, aReads - 1)
                        | None -> ()
                | Value v when isTemp a && isSmallLiteral v ->
                    arr[i] <- Empty
                    mapRest i (SubstituteVar(a, Value v))
                | e when isTemp a && i + 1 < arr.Count ->
                    // a temporary used once, in the next statement, before any side effects there
                    let n = 
                        seq { i + 1 .. arr.Count - 1 } |> Seq.tryFind (fun j -> match arr[j] with Empty -> false | _ -> true)
                    match e, n with
                    | (Function _ | FuncWithKind _), _ | _, None -> ()
                    | _, Some n ->
                    match firstEvaluated arr[n] with
                    | Some next ->
                        let total, _ = countReads a (rest i)
                        if total = 1 && fst (countReads a [ ExprStatement next ]) = 1
                            && varEvalOrderWith true isConst [ a ] next then
                            arr[n] <- SubstituteVar(a, value).TransformStatement arr[n]
                            arr[i] <- Empty
                    | None -> ()
                | _ -> ()
            | _ -> ()
        arr |> Seq.filter (function Empty -> false | _ -> true) |> List.ofSeq

/// Removes compiler-generated variables that are aliases of other variables, used once or not read,
/// and replaces F# ref cells that do not leave the function with mutable variables.
let cleanupVars (e: Expression) =
    let info = VarInfo()
    info.VisitExpression e
    let c = Cleaner(info)
    let res = c.TransformExpression e
    if c.Renames.Count = 0 then res else
    ReplaceIds(c.ResolvedRenames()).TransformExpression res

/// Same as `cleanupVars`, for a statement.
let cleanupVarsSt (s: Statement) =
    let info = VarInfo()
    info.VisitStatement s
    let c = Cleaner(info)
    let res = c.TransformStatement s
    if c.Renames.Count = 0 then res else
    ReplaceIds(c.ResolvedRenames()).TransformStatement res
