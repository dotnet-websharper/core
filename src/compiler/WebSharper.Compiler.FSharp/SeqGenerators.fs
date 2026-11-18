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

/// Translates F# sequence expressions to JavaScript generator functions,
/// list and array comprehensions to building an array with a loop, and
/// task expressions to JavaScript async functions.
/// F# gives sequence expressions as nested calls of Seq.delay, Seq.append,
/// Seq.collect and similar combinators. The recognized combinators become
/// statements, any other sub-sequence is enumerated with `yield*`, so the
/// translation applies to every `seq { ... }`.
/// Task expressions are calls of TaskBuilder methods. The recognized ones become
/// statements and `await`, any other part runs as a task of its own and is awaited.
module internal WebSharper.Compiler.FSharp.SeqGenerators

open WebSharper.Core
open WebSharper.Core.AST

module I = IgnoreSourcePos

let private (|ModuleCall|_|) (typeName: string) (name: string) e =
    match e with
    | I.Call (None, td, m, args) when m.Entity.Value.MethodName = name && td.Entity.Value.FullName = typeName -> Some args
    | _ -> None

let private (|SeqCall|_|) name e = (|ModuleCall|_|) "Microsoft.FSharp.Collections.SeqModule" name e
let private (|HelperCall|_|) name e = (|ModuleCall|_|) "Microsoft.FSharp.Core.CompilerServices.RuntimeHelpers" name e
let private (|OperatorsCall|_|) name e = (|ModuleCall|_|) "Microsoft.FSharp.Core.Operators" name e

let rec private (|IgnoreCoerce|) e =
    match e with
    | I.Coerce (e, _, _) -> (|IgnoreCoerce|) e
    | _ -> e

/// `seq { ... }` is `CreateSequence (Seq.delay (fun () -> ...))`, also accept a plain `Seq.delay`
let private (|SeqExpr|_|) e =
    match e with
    | IgnoreCoerce (OperatorsCall "CreateSequence" [ IgnoreCoerce (SeqCall "Delay" [ I.Function ([], None, _, I.Return body) ]) ])
    | IgnoreCoerce (SeqCall "Delay" [ I.Function ([], None, _, I.Return body) ]) -> Some body
    | _ -> None

let private disposableType, disposeMethod =
    match <@ (null: System.IDisposable).Dispose() @> with
    | FSharp.Quotations.Patterns.Call (_, mi, _) ->
        Reflection.ReadTypeDefinition mi.DeclaringType,
        Reflection.ReadMethod mi
    | _ -> failwith "Expecting a Call pattern"

/// A JavaScript iterable for a .NET sequence, arrays are used directly
let private iterable (e: Expression) =
    match e with
    | I.Coerce (src, ArrayType (_, 1), _) -> src
    | _ -> Appl (GlobalAccess (Address.Runtime "Iter"), [ e ], NonPure, Some 1)

/// How the items of the sequence are produced
type private Emit =
    {
        /// A single item
        Item : Expression -> Statement
        /// All items of a .NET sequence
        All : Expression -> Statement
    }

/// Statements producing the items of a sequence expression.
/// `tr` is applied to every expression that is not part of the sequence structure.
let rec private gen (emit: Emit) (tr: Expression -> Expression) (e: Expression) : Statement =
    let g = gen emit tr
    match e with
    | ExprSourcePos (p, e) -> StatementSourcePos (p, g e)
    | Coerce (e, _, _) -> g e
    | SeqExpr body -> g body
    | SeqCall "Singleton" [ v ] -> emit.Item (tr v)
    | SeqCall "Empty" [] -> Empty
    | SeqCall "Append" [ a; b ] -> Block [ g a; g b ]
    | SeqCall "Collect" [ I.Function ([ x ], None, _, I.Return body); src ] ->
        ForOf (x, iterable (tr src), g body)
    | SeqCall "Map" [ I.Function ([ x ], None, _, I.Return v); src ] ->
        ForOf (x, iterable (tr src), emit.Item (tr v))
    | HelperCall "EnumerateWhile" [ I.Function ([], None, _, I.Return cond); body ] ->
        While (tr cond, g body)
    | HelperCall "EnumerateThenFinally" [ body; I.Function ([], None, _, fin) ] ->
        TryFinally (g body, trSt tr fin)
    | HelperCall "EnumerateUsing" [ res; I.Function ([ x ], None, _, I.Return body) ] ->
        Block [
            VarDeclaration (x, tr res)
            TryFinally (
                g body,
                If (Binary (Var x, BinaryOperator.``!=``, Value Null),
                    ExprStatement (Call (Some (Var x), NonGeneric disposableType, NonGeneric disposeMethod, [])),
                    Empty)
            )
        ]
    | I.Conditional (c, a, b) -> If (tr c, g a, g b)
    | I.Let (x, v, body) -> Block [ VarDeclaration (x, tr v); g body ]
    | I.Sequential es ->
        match List.rev es with
        | last :: restRev ->
            Block [
                for r in List.rev restRev do
                    match r with
                    | I.NewVar (x, v) -> yield VarDeclaration (x, tr v)
                    | _ -> yield ExprStatement (tr r)
                yield g last
            ]
        | [] -> Empty
    | _ -> emit.All (tr e)

/// Applies an expression transformation to the expressions inside a statement
and private trSt (tr: Expression -> Expression) (s: Statement) =
    { new Transformer() with
        override this.TransformExpression e = tr e
    }.TransformStatement s

let private isTaskBuilder (td: TypeDefinition) =
    match td.Value.FullName with
    | "Microsoft.FSharp.Control.TaskBuilder"
    | "Microsoft.FSharp.Control.TaskBuilderBase" -> true
    | _ -> false

/// A method of the task builder
let private (|TaskCall|_|) name e =
    match e with
    | I.Call (Some _, td, m, args) when isTaskBuilder td.Entity && m.Entity.Value.MethodName = name -> Some args
    | _ -> None

/// An extension method of the task builder, `priority` is HighPriority (tasks), MediumPriority (F# async)
/// or LowPriority (IDisposable)
let private (|TaskExtCall|_|) (priority: string, name: string) e =
    match e with
    | I.Call (None, td, m, _ :: args)
        when td.Entity.Value.FullName = "Microsoft.FSharp.Control.TaskBuilderExtensions." + priority
            && m.Entity.Value.MethodName = name -> Some args
    | _ -> None

let private asyncDisposableType, disposeAsyncMethod =
    match <@ (null: System.IAsyncDisposable).DisposeAsync() @> with
    | FSharp.Quotations.Patterns.Call (_, mi, _) ->
        Reflection.ReadTypeDefinition mi.DeclaringType,
        Reflection.ReadMethod mi
    | _ -> failwith "Expecting a Call pattern"

/// Statements of a task expression, `tail`: the value of the code is the result of the task
/// (`return` is only for tail positions, F# allows a value only there).
/// `run` makes a task of any other resumable code, which is then awaited.
let rec private genTask (run: Expression -> Expression) (tr: Expression -> Expression) (tail: bool) (e: Expression) : Statement =
    let g = genTask run tr
    let result v = if tail then Return v else ExprStatement v
    let bindTo (cont: Expression) (value: Expression) =
        match cont with
        | I.Function ([ x ], None, _, I.Return body) -> Some (Block [ VarDeclaration (x, value); g tail body ])
        | I.Function ([], None, _, I.Return body) -> Some (Block [ ExprStatement value; g tail body ])
        | _ -> None
    let fallback () = result (Await (run (tr e)))
    match e with
    | ExprSourcePos (p, e) -> StatementSourcePos (p, g tail e)
    | TaskCall "Delay" [ I.Function ([], None, _, I.Return body) ] -> g tail body
    | TaskCall "Return" [ v ] -> result (tr v)
    | TaskCall "Zero" [] -> Empty
    | TaskCall "Combine" [ a; b ] -> Block [ g false a; g tail b ]
    | TaskCall "While" [ I.Function ([], None, _, I.Return cond); body ] ->
        While (tr cond, g false body)
    | TaskCall "For" [ src; I.Function ([ x ], None, _, I.Return body) ] ->
        ForOf (x, iterable (tr src), g false body)
    | TaskCall "TryWith" [ body; I.Function ([ x ], None, _, I.Return handler) ] ->
        TryWith (g tail body, Some x, g tail handler)
    | TaskCall "TryFinally" [ body; I.Function ([], None, _, fin) ] ->
        TryFinally (g tail body, trSt tr fin)
    | TaskCall "Using" [ res; I.Function ([ x ], None, _, I.Return body) ] ->
        // IAsyncDisposable
        Block [
            VarDeclaration (x, tr res)
            TryFinally (
                g tail body,
                If (Binary (Var x, BinaryOperator.``!=``, Value Null),
                    ExprStatement (Await (Call (Some (Var x), NonGeneric asyncDisposableType, NonGeneric disposeAsyncMethod, []))),
                    Empty)
            )
        ]
    | TaskExtCall ("LowPriority", "TaskBuilderBase.Using") [ res; I.Function ([ x ], None, _, I.Return body) ] ->
        Block [
            VarDeclaration (x, tr res)
            TryFinally (
                g tail body,
                If (Binary (Var x, BinaryOperator.``!=``, Value Null),
                    ExprStatement (Call (Some (Var x), NonGeneric disposableType, NonGeneric disposeMethod, [])),
                    Empty)
            )
        ]
    | TaskExtCall ("HighPriority", "TaskBuilderBase.Bind") [ t; cont ] ->
        bindTo cont (Await (tr t)) |> Option.defaultWith fallback
    | TaskExtCall ("HighPriority", "TaskBuilderBase.ReturnFrom") [ t ] ->
        result (Await (tr t))
    | I.Conditional (c, a, b) -> If (tr c, g tail a, g tail b)
    | I.Let (x, v, body) -> Block [ VarDeclaration (x, tr v); g tail body ]
    | I.Sequential es ->
        match List.rev es with
        | last :: restRev ->
            Block [
                for r in List.rev restRev do
                    match r with
                    | I.NewVar (x, v) -> yield VarDeclaration (x, tr v)
                    | _ -> yield ExprStatement (tr r)
                yield g tail last
            ]
        | [] -> Empty
    | _ -> fallback ()

/// Translates sequence expressions to generators and list/array comprehensions to loops
type private SeqTransformer() =
    inherit Transformer()

    override this.TransformExpression e =
        let tr = this.TransformExpression
        match e with
        | I.Call (None, td, m, [ SeqExpr body ])
            when td.Entity.Value.FullName = "Microsoft.FSharp.Collections.SeqModule"
                && m.Entity.Value.MethodName = "ToArray" ->
            // [| ... |]: push the items to an array
            let a = Id.New("a", mut = false)
            let push v = ExprStatement (Appl (ItemGet (Var a, Value (String "push"), NonPure), [ v ], NonPure, Some 1))
            let emit =
                {
                    Item = push
                    All = fun s ->
                        let x = Id.New("x", mut = false)
                        ForOf (x, iterable s, push (Var x))
                }
            Let (a, NewArray [], Sequential [ IgnoredStatementExpr (gen emit tr body); Var a ])
        | I.Call (None, td, m, [ SeqExpr body ])
            when td.Entity.Value.FullName = "Microsoft.FSharp.Collections.SeqModule"
                && m.Entity.Value.MethodName = "ToList" && m.Generics.Length = 1 ->
            // [ ... ]: build the list from its head by setting the tail of the last cell,
            // like the List module does internally. `head` is a sentinel cell before the first item.
            let listType = Generic Definitions.FSharpList m.Generics
            let cons h = NewUnionCase (listType, "Cons", [ h; Undefined ])
            let head = Id.New("l", mut = false)
            let last = Id.New("last")
            let setTail t = ExprStatement (ItemSet (Var last, Value (String "$1"), t))
            let add v =
                let c = Id.New("c", mut = false)
                Block [
                    VarDeclaration (c, cons v)
                    setTail (Var c)
                    ExprStatement (VarSet (last, Var c))
                ]
            let emit =
                {
                    Item = add
                    All = fun s ->
                        let x = Id.New("x", mut = false)
                        ForOf (x, iterable s, add (Var x))
                }
            Let (head, cons Undefined,
                Sequential [
                    IgnoredStatementExpr (
                        Block [
                            VarDeclaration (last, Var head)
                            gen emit tr body
                            setTail (NewUnionCase (listType, "Empty", []))
                        ]
                    )
                    ItemGet (Var head, Value (String "$1"), NonPure)
                ]
            )
        | I.Call (Some builder, td, m, [ body ])
            when td.Entity.Value.FullName = "Microsoft.FSharp.Control.TaskBuilder" && m.Entity.Value.MethodName = "Run" ->
            // task { ... }: a JavaScript async function
            let run code = Call (Some builder, td, m, [ code ])
            let boxed =
                match m.Generics with
                | [ t ] -> WebSharper.Compiler.CompilationHelpers.AsyncTask.resultMayBeThenable t
                | _ -> true
            WebSharper.Compiler.CompilationHelpers.AsyncTask.ofBody boxed (genTask run tr true body)
        | SeqExpr body ->
            // seq { ... }: a generator function enumerated by the runtime
            let emit =
                {
                    Item = fun v -> ExprStatement (YieldExpr (Some v, false))
                    All = fun s -> ExprStatement (YieldExpr (Some (iterable s), true))
                }
            let genFunc = FuncWithKind (FuncKind.Generator, Function ([], None, None, gen emit tr body))
            Appl (GlobalAccess (Address.Runtime "Iterate"), [ genFunc ], Pure, Some 1)
        | _ -> base.TransformExpression e

let private transformer = SeqTransformer()

/// Translates sequence expressions and list/array comprehensions in an expression
let transform (e: Expression) = transformer.TransformExpression e
