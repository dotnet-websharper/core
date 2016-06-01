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

// Reads F# quotations and ReflectedDefinitions as WebSharper.Core.AST 
module WebSharper.Compiler.QuotationReader

open FSharp.Quotations

open WebSharper.Core
open WebSharper.Core.AST

module A = WebSharper.Compiler.AttributeReader

type VarKind =
    | LocalVar 
    | ByRefArg
    | ThisArg

type Environment =
    {
        Vars : System.Collections.Generic.Dictionary<Var, Id * VarKind>
//        TParams : Map<string, int>
        Exception : option<Id>
//        MatchVars : option<Id * Id>
        Compilation : Compilation
    }
    static member New(comp) = 
        { 
            Vars = System.Collections.Generic.Dictionary() 
//            TParams = tparams |> Seq.mapi (fun i p -> p, i) |> Map.ofSeq
            Exception = None
//            MatchVars = None
            Compilation = comp
        }

//    member this.WithTParams tparams =
//        if List.isEmpty tparams then this else
//        { this with 
//            TParams = 
//                ((this.TParams, this.TParams.Count), tparams) 
//                ||> List.fold (fun (m, i) p -> m |> Map.add p i, i + 1) 
//                |> fst
//        }

    member this.AddVar (i: Id, v: Var, ?k) =
        if not (this.Vars.ContainsKey v) then 
            this.Vars.Add(v, (i, defaultArg k LocalVar))

    member this.WithException (i: Id, v: Var) =
        this.AddVar(i, v)
        { this with Exception = Some i }

    member this.LookupVar (v: Var) =
        this.Vars.[v]

let getOptSourcePos (expr: Expr) =
    expr.CustomAttributes |> List.tryPick (
        function 
        | Patterns.NewTuple
            [
                Patterns.Value (debugRange, _)//(String "DebugRange")
                Patterns.NewTuple [
                    Patterns.Value (fileName, _)
                    Patterns.Value (startLine, _)
                    Patterns.Value (startCol, _)
                    Patterns.Value (endLine, _)
                    Patterns.Value (endCol, _)
                ]
            ] when obj.Equals(debugRange, "DebugRange") ->
                Some {   
                    FileName = fileName :?> string
                    Start = startLine :?> int, startCol :?> int
                    End = endLine :?> int, endCol :?> int
                }
        | _ -> None
    )

let withOptSourcePos (expr: Expr) (e: Expression) =
    match getOptSourcePos expr with
    | Some p -> ExprSourcePos(p, e)
    | _ -> e

exception ParseError of string
let parsefailf x =
    Printf.kprintf (fun s -> raise <| ParseError s) x

let errorPlaceholder = Value (String "$$ERROR$$")

let rec transformExpression (env: Environment) (expr: Expr) =
    let inline tr x = transformExpression env x
    let call this (meth: System.Reflection.MethodInfo) args =
        let td = 
            Generic
                (Reflection.ReadTypeDefinition meth.DeclaringType)
                (meth.DeclaringType.GetGenericArguments() |> Seq.map Reflection.ReadType |> List.ofSeq)
        let md =
            Generic
                (Reflection.ReadMethod meth)
                (meth.GetGenericArguments() |> Seq.map Reflection.ReadType |> List.ofSeq)
        Call(this |> Option.map tr, td, md, args |> List.map tr )
    try
        match expr with
        | Patterns.Var var ->
            let v, k = env.LookupVar var 
            match k with
            | LocalVar -> Var v  
            | ByRefArg -> GetRef (Var v)
            | ThisArg -> This
        | Patterns.Lambda (arg, body) ->
            let lArg =
                if arg.Type = typeof<unit> then
                    []
                else
                    let i = Id.New(arg.Name)  
                    env.AddVar(i, arg)
                    [i]
            Lambda(lArg, (tr body))
        | Patterns.Application(func, arg) ->
            Application(tr func, [tr arg], false, Some 1) // TODO: pure functions
        | Patterns.Let(id, value, body) ->
            let i = Id.New(id.Name)
            env.AddVar(i, id, if id.Type.IsByRef then ByRefArg else LocalVar)
            if id.IsMutable then
                Sequential [ NewVar(i, tr value); tr body ]
            else
                Let (i, tr value, tr body)
        | Patterns.LetRecursive(defs, body) ->
            let ids = defs |> List.map (fun (id, _) ->
                let i = Id.New(id.Name)
                env.AddVar(i, id, if id.Type.IsByRef then ByRefArg else LocalVar)
                i      
            )
            LetRec (
                Seq.zip ids defs 
                |> Seq.map (fun (i, (_, v)) -> i, tr v) |> List.ofSeq, 
                tr body
            )
        | Patterns.Call(this, meth, args) ->
            call this meth args
        | Patterns.PropertyGet(this, prop, args) ->
            call this (prop.GetGetMethod()) args
        | Patterns.PropertySet(this, prop, args, value) ->
            call this (prop.GetSetMethod()) (args @ [value]) 
        | Patterns.Sequential _ ->
            let rec getSeq acc expr =
                match expr with            
                | Patterns.Sequential (f, s) ->
                    getSeq (f :: acc) s   
                | _ -> expr :: acc
            getSeq [] expr |> List.rev |> List.map tr |> Sequential
        | Patterns.Value (value, _) ->
            match value with
            | x when obj.ReferenceEquals(x, null) -> Null      
            | :? bool   as x -> Bool   x
            | :? byte   as x -> Byte   x
            | :? char   as x -> Char   x
            | :? double as x -> Double x
            | :? int    as x -> Int    x
            | :? int16  as x -> Int16  x
            | :? int64  as x -> Int64  x
            | :? sbyte  as x -> SByte  x
            | :? single as x -> Single x
            | :? string as x -> String x
            | :? uint16 as x -> UInt16 x
            | :? uint32 as x -> UInt32 x
            | :? uint64 as x -> UInt64 x
            | :? Expr -> parsefailf "F# quotation found as value, missing splicing."
            | _ -> parsefailf "F# constant value not recognized: %A" value
            |> Value
        | Patterns.IfThenElse (cond, then_, else_) ->
            Conditional(tr cond, tr then_, tr else_)    
        | Patterns.NewObject (ctor, args) -> 
            let td = 
                Generic
                    (Reflection.ReadTypeDefinition ctor.DeclaringType)
                    (ctor.DeclaringType.GetGenericArguments() |> Seq.map Reflection.ReadType |> List.ofSeq)
            let cd = Reflection.ReadConstructor ctor
            Ctor(td, cd, args |> List.map tr )
        | Patterns.TryFinally (body, final) ->
            let res = Id.New ()
            StatementExpr (TryFinally(VarSetStatement(res, tr body), ExprStatement (tr final)), Some res)
        | Patterns.TryWith (body, var, filter, e, catch) -> // TODO: var, filter?
            let err = Id.New e.Name
            let res = Id.New ()
            StatementExpr (
                TryWith(VarSetStatement(res, tr body), 
                    Some err, 
                    (VarSetStatement(res, transformExpression (env.WithException(err, e)) catch))), Some res)
        | Patterns.NewArray (_, items) ->
            NewArray (items |> List.map tr)              
        | Patterns.NewTuple (items) ->
            NewArray (items |> List.map tr)              
        | Patterns.WhileLoop (cond, body) ->
            IgnoredStatementExpr(While(tr cond, ExprStatement (Capturing().CaptureValueIfNeeded(tr body))))
        | Patterns.VarSet (var, value) ->
            let v, k = env.LookupVar var
            match k with
            | LocalVar -> VarSet(v, tr value) 
            | ByRefArg -> SetRef (Var v) (tr value)
            | ThisArg -> parsefailf "'this' parameter cannot be set"
        | Patterns.TupleGet (tuple, i) ->
            ItemGet(tr tuple, Value (Int i))   
        | Patterns.ForIntegerRangeLoop (var, start, end_, body) ->
            let i = Id.New var.Name
            env.AddVar(i, var)
            let j = Id.New "j"
            For (
                Some (Sequential [NewVar(i, tr start); NewVar (j, tr end_)]), 
                Some (Binary(Var i, BinaryOperator.``<=``, Var j)), 
                Some (MutatingUnary(MutatingUnaryOperator.``()++``, Var i)), 
                ExprStatement (Capturing(i).CaptureValueIfNeeded(tr body))
            ) |> IgnoredStatementExpr
        | Patterns.TypeTest (expr, typ) ->
            TypeCheck (tr expr, Reflection.ReadType typ) //env.TParams typ)
        | Patterns.Coerce (expr, typ) ->
            tr expr // TODO: type check when possible
        | Patterns.NewUnionCase (case, exprs) ->
            let annot = A.attrReader.GetMemberAnnot(A.TypeAnnotation.Empty, case.GetCustomAttributesData()) 
            match annot.Kind with
            | Some (A.MemberKind.Constant c) -> Value c
            | _ ->
            let i = case.Tag
            CopyCtor(
                Reflection.ReadTypeDefinition case.DeclaringType,
                Object (
                    ("$", Value (Int i)) ::
                    (exprs |> List.mapi (fun j e -> "$" + string j, tr e)) 
                )
            )
        | Patterns.UnionCaseTest (expr, case) ->
            let annot = A.attrReader.GetMemberAnnot(A.TypeAnnotation.Empty, case.GetCustomAttributesData()) 
            match annot.Kind with
            | Some (A.MemberKind.Constant c) -> Binary (tr expr, BinaryOperator.``==``, Value c)
            | _ ->
            let i = case.Tag
            Binary(ItemGet(tr expr, Value (String "$")), BinaryOperator.``==``, Value (Int i))
        | Patterns.NewRecord (typ, items) ->
            let t =
                match Reflection.ReadType typ with
                | ConcreteType ct -> ct
                | _ -> parsefailf "Expected a record type"
            NewRecord (t, List.map tr items)
        | Patterns.FieldGet (thisOpt, field) ->
            let t = 
                match Reflection.ReadType field.DeclaringType with
                | ConcreteType ct -> ct
                | _ -> parsefailf "Expected a record type"
            FieldGet(thisOpt |> Option.map tr, t, field.Name)
        | Patterns.FieldSet (thisOpt, field, value) ->
            let t = 
                match Reflection.ReadType field.DeclaringType with
                | ConcreteType ct -> ct
                | _ -> parsefailf "Expected a record type"
            FieldSet(thisOpt |> Option.map tr, t, field.Name, tr value)
        | Patterns.AddressOf expr ->
            match IgnoreExprSourcePos (tr expr) with
            | Var v as e ->
                MakeRef e (fun value -> VarSet(v, value))
            | ItemGet(o, i) as e ->
                MakeRef e (fun value -> ItemSet(o, i, value))
            | FieldGet(o, t, f) as e ->
                MakeRef e (fun value -> FieldSet(o, t, f, value))                
            | e -> parsefailf "AddressOf error" // not on a Var or ItemGet: %+A" e 
        | Patterns.AddressSet (addr, value) ->
            match addr with
            | Patterns.Var(var) ->
                let v, _ = env.LookupVar var
                SetRef (Var v) (tr value)
            | _ -> parsefailf "AddressSet not on a Value"
        | Patterns.DefaultValue typ ->
            Value Null
        | Patterns.QuoteTyped expr 
        | Patterns.QuoteRaw expr -> tr expr
        | Patterns.NewDelegate _ -> parsefailf "TODO : NewDelegate quotation form"
        | Patterns.WithValue (_,_,def) -> tr def
        | _ -> parsefailf "Unrecognized quotation %+A" expr

        |> withOptSourcePos expr
    with e ->
        let msg =
            match e with
            | ParseError m -> m
            | _ -> "Error while reading F# quotation: " + e.Message //+ " " + e.StackTrace
        env.Compilation.AddError(getOptSourcePos expr, SourceError msg)
        errorPlaceholder        
