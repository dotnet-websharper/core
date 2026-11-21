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

module WebSharper.Core.JavaScript.Test.SubstitutionTests

open NUnit.Framework
open WebSharper.Core.AST
open WebSharper.Compiler

/// Collects every Id an expression mentions.
type private CollectIds() =
    inherit Visitor()
    let ids = System.Collections.Generic.HashSet<Id>()
    member this.Ids = ids
    override this.VisitId i = ids.Add i |> ignore

let private idsOf e =
    let c = CollectIds()
    c.VisitExpression e
    c.Ids

// #1623: an inline body read from another assembly's metadata keeps that
// assembly's Id numbers. Every variable it binds must get a fresh Id when it is
// inlined, or it can collide with an Id of the current compilation, and the
// Breaker then drops its declaration (MISSINGVAR in the output).
[<Test>]
let ``Inlining refreshes variables bound by statements`` () =
    let res = Id.New()
    let err = Id.New("e")
    let key = Id.New("k")
    // try { res = $0 } catch (e) { res = e } finally { for (k in $0) {} }; res
    let body =
        StatementExpr(
            TryFinally(
                TryWith(VarSetStatement(res, Hole 0), Some err, VarSetStatement(res, Var err)),
                ForIn(key, Hole 0, Block [ ExprStatement (Var key) ])
            ),
            Some res
        )
    let original = idsOf body
    let inline1 = Substitution([ Value (Int 1) ]).TransformExpression body
    let inline2 = Substitution([ Value (Int 2) ]).TransformExpression body
    let ids1 = idsOf inline1
    let ids2 = idsOf inline2
    Assert.AreEqual(3, ids1.Count, "each bound variable stays a single Id within one inlined copy")
    Assert.IsFalse(ids1.Overlaps original, "inlined copy reuses an Id of the inline body")
    Assert.IsFalse(ids2.Overlaps original, "inlined copy reuses an Id of the inline body")
    Assert.IsFalse(ids1.Overlaps ids2, "two inlined copies share an Id")
    match inline1 with
    | StatementExpr (TryFinally (TryWith (ExprStatement (VarSet (r1, _)), Some e1, ExprStatement (VarSet (r2, Var e2))), ForIn (k1, _, Block [ ExprStatement (Var k2) ])), Some r3) ->
        Assert.AreEqual(r1, r2)
        Assert.AreEqual(r1, r3)
        Assert.AreEqual(e1, e2)
        Assert.AreEqual(k1, k2)
    | e -> Assert.Fail(sprintf "Unexpected shape: %s" (Debug.PrintExpression e))

// The same for the variables of `for...of` loops and optional chains.
[<Test>]
let ``Inlining refreshes for...of and optional chain variables`` () =
    let item = Id.New("x")
    let chained = Id.New("o")
    // { for (x of $0) { x?.length } }
    let body =
        StatementExpr(
            ForOf(item, Hole 0, ExprStatement (OptionalChain(Var item, chained, ItemGet(Var chained, Value (String "length"), Pure)))),
            None
        )
    let original = idsOf body
    let inline1 = Substitution([ Value (Int 1) ]).TransformExpression body
    let inline2 = Substitution([ Value (Int 2) ]).TransformExpression body
    let ids1 = idsOf inline1
    Assert.AreEqual(2, ids1.Count, "each bound variable stays a single Id within one inlined copy")
    Assert.IsFalse(ids1.Overlaps original, "inlined copy reuses an Id of the inline body")
    Assert.IsFalse(ids1.Overlaps (idsOf inline2), "two inlined copies share an Id")
    match inline1 with
    | StatementExpr (ForOf (x1, _, ExprStatement (OptionalChain (Var x2, o1, ItemGet (Var o2, _, _)))), None) ->
        Assert.AreEqual(x1, x2)
        Assert.AreEqual(o1, o2)
    | e -> Assert.Fail(sprintf "Unexpected shape: %s" (Debug.PrintExpression e))
