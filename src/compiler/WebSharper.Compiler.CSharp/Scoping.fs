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

module internal WebSharper.Compiler.CSharp.Scoping

open WebSharper.Core.AST

open System.Collections.Generic

type HasCapturedDeclarations() =
    inherit Visitor()

    let vars = HashSet()
    let mutable scope = 0
    let mutable hasCapture = false

    override this.VisitVarDeclaration (a, b) =
        if scope = 0 then
            vars.Add(a) |> ignore
        this.VisitExpression(b)

    override this.VisitFunction (_, _, _, b) =
        scope <- scope + 1    
        this.VisitStatement(b)
        scope <- scope - 1   

    override this.VisitId a =
        if scope > 0 && vars.Contains a then
            hasCapture <- true

    member this.Check a =
        this.VisitStatement a
        hasCapture

// 0: return
// 1: break
// 2: continue

let i0 = Value (Int 0)
let i1 = Value (Int 1)
let i2 = Value (Int 2)
    
type BlockScoping() =
    inherit Transformer()
    
    let mutable hasReturn = false
    let breaks = ResizeArray() 
    let conts = ResizeArray() 

    override this.TransformFunction(a, arr, ret, b) = Function(a, arr, ret, b)

    override this.TransformReturn(a) =
        hasReturn <- true
        match a with
        | Undefined -> Return(NewArray [ i0 ])
        | _ -> Return(NewArray [ i0; a ])

    override this.TransformBreak(a) =
        breaks.Add a
        match a with 
        | None -> Return(NewArray [ i1 ]) 
        | Some l ->
            Return(NewArray [ i1; Value (String l.Name.Value) ])

    override this.TransformContinue(a) =
        conts.Add a
        match a with 
        | None -> Return(NewArray [ i2 ]) 
        | Some l -> Return(NewArray [ i2; Value (String l.Name.Value) ])

     member this.CloseBlock(a) =
        let res = Id.New()
        if hasReturn || breaks.Count > 0 || conts.Count > 0 then
            Block [
                VarDeclaration(res, Appl(Function([], None, None, Block a), [], NonPure, Some 0))

                If (Var res, 
                    Switch(ItemGet(Var res, i0, Pure),
                        [
                            if hasReturn then 
                                yield (Some i0, Return (ItemGet(Var res, i1, Pure)))
                            if breaks.Count > 0 then 
                                yield (Some i1,
                                    Switch(ItemGet(Var res, i1, Pure),
                                        [
                                            for b in breaks do
                                                match b with
                                                | None ->
                                                    yield (Some Undefined, Break None)
                                                | Some l ->
                                                    yield (Some (Value (String l.Name.Value)), Break b)
                                        ]
                                    )
                                )
                            if conts.Count > 0 then 
                                yield (Some i2,
                                    Switch(ItemGet(Var res, i1, Pure),
                                        [
                                            for b in conts do
                                                match b with
                                                | None ->
                                                    yield (Some Undefined, Continue None)
                                                | Some l ->
                                                    yield (Some (Value (String l.Name.Value)), Continue b)
                                        ]
                                    )
                                )
                        ]
                    )
                , Empty)
            ]
        else ExprStatement <| Appl(Function([], None, None, Block a), [], NonPure, Some 0)

type FixScoping() =
    inherit Transformer()
    
    member this.TransformLoopBody(a) =
        match IgnoreStatementSourcePos a with
        | Block bs -> 
            if HasCapturedDeclarations().Check(a) then
                BlockScoping().CloseBlock(bs)   
                // TODO : restore source pos info
            else a  
        | _ -> a

    override this.TransformFor(a, b, c, d) =
        For (
            a, 
            b |> Option.map this.TransformExpression, 
            c |> Option.map this.TransformExpression,
            d |> this.TransformStatement |> this.TransformLoopBody
        )

    override this.TransformWhile(a, b) =
        While(
            a |> this.TransformExpression,
            b |> this.TransformStatement |> this.TransformLoopBody
        )

    override this.TransformDoWhile(a, b) =
        DoWhile(
            a |> this.TransformStatement |> this.TransformLoopBody,
            b |> this.TransformExpression
        )
    
let private fixInst = FixScoping() 

/// Wraps loop bodies that declare captured variables in a function, to get a
/// new binding of them on each iteration. Only needed when the method is
/// turned into a state machine (iterators, async methods, goto), as these
/// move all variable declarations to the top of the function.
let fix s =  fixInst.TransformStatement s

/// Collects the labels targeted by `goto` statements
type private CollectGotoTargets() =
    inherit StatementVisitor()

    let targets = HashSet<Id>()

    override this.VisitGoto(a) = targets.Add a |> ignore

    member this.Targets = targets

    static member Collect(s: Statement) =
        let c = CollectGotoTargets()
        c.VisitStatement s
        c.Targets

/// Checks if a label is used by a `break` or `continue` statement
type private HasLabelReference(label: Id) =
    inherit StatementVisitor()

    let mutable found = false

    override this.VisitBreak(a) = if a = Some label then found <- true
    override this.VisitContinue(a) = if a = Some label then found <- true

    static member Check(label, s: Statement) =
        let c = HasLabelReference(label)
        c.VisitStatement s
        c.Found

    member this.Found = found

type private ReplaceGoto(label: Id, breakLabel: Id) =
    inherit StatementTransformer()

    override this.TransformGoto(a) =
        if a = label then Break (Some breakLabel) else Goto a

let rec private flattenBlocks (s: Statement list) =
    s |> List.collect (fun st ->
        match st with
        | Block b -> flattenBlocks b
        | _ -> [ st ]
    )

/// Moves declarations at the top level of a statement out, so that
/// wrapping the statement in a block does not change their scope.
let rec private hoistDeclarations (s: Statement) : Id list * Statement =
    match s with
    | VarDeclaration (v, e) ->
        [ v ],
        match e with
        | Undefined -> Empty
        | _ -> ExprStatement (VarSet (v, e))
    | StatementSourcePos (p, s) ->
        let vs, s = hoistDeclarations s
        vs, StatementSourcePos (p, s)
    | Block b ->
        let r = b |> List.map hoistDeclarations
        List.collect fst r, Block (List.map snd r)
    | _ -> [], s

let rec private hasTopLevelFuncDeclaration (s: Statement) =
    match s with
    | FuncDeclaration _ -> true
    | StatementSourcePos (_, s) -> hasTopLevelFuncDeclaration s
    | Block b -> b |> List.exists hasTopLevelFuncDeclaration
    | _ -> false

/// Rewrites forward `goto` jumps to a later statement of an enclosing block
/// as `break` out of a labeled block, so that no state machine is needed:
/// `{ A; if (c) goto L; B; L: C }` becomes `{ L_skip: { A; if (c) break L_skip; B; } L: C }`
type private ForwardGotosToBreaks() =
    inherit StatementTransformer()

    override this.TransformBlock(statements) =
        let statements = statements |> List.map this.TransformStatement
        let labelOf s =
            match IgnoreStatementSourcePos s with
            | Labeled (l, _) -> Some l
            | _ -> None
        let flat = flattenBlocks statements
        if flat |> List.forall (labelOf >> Option.isNone) then Block statements else
        let mutable cur = flat
        let mutable i = 0
        while i < List.length cur do
            match labelOf cur.[i] with
            | Some l ->
                let gotoIn (s: Statement) = (CollectGotoTargets.Collect s).Contains l
                let before = List.take i cur
                let isBackwardTarget = List.skip i cur |> List.exists gotoIn
                match before |> List.tryFindIndex gotoIn with
                | Some j when not isBackwardTarget ->
                    let range = before |> List.skip j
                    if range |> List.exists hasTopLevelFuncDeclaration then
                        i <- i + 1
                    else
                        let breakLabel = Id.New(defaultArg l.Name "L" + "_skip")
                        let replace = ReplaceGoto(l, breakLabel)
                        let hoisted = range |> List.map (replace.TransformStatement >> hoistDeclarations)
                        let decls = hoisted |> List.collect fst |> List.map (fun v -> VarDeclaration(v, Undefined))
                        let wrapped = Labeled(breakLabel, Block (List.map snd hoisted))
                        // all jumps to the label are replaced, drop it if nothing else uses it
                        let target =
                            match IgnoreStatementSourcePos cur.[i] with
                            | Labeled (_, body) when not (cur |> List.exists (fun s -> HasLabelReference.Check(l, s))) ->
                                match IgnoreStatementSourcePos body with
                                | Empty -> []
                                | _ -> [ body ]
                            | _ -> [ cur.[i] ]
                        cur <- List.take j cur @ decls @ [ wrapped ] @ target @ List.skip (i + 1) cur
                        i <- j + decls.Length + 1 + target.Length
                | _ ->
                    i <- i + 1
            | None ->
                i <- i + 1
        Block cur

let private forwardGotosInst = ForwardGotosToBreaks()

/// Removes forward gotos that jump out of nested statements, see ForwardGotosToBreaks
let forwardGotosToBreaks s = forwardGotosInst.TransformStatement s
