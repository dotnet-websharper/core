// $begin{copyright}
//
// This file is part of WebSharper
//
// Copyright (c) 2008-2015 IntelliFactory
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

module WebSharper.Compiler.Translator

module C = WebSharper.Core.JavaScript.Core
module M = WebSharper.Compiler.Metadata
module P = WebSharper.Core.JavaScript.Packager
module Q = WebSharper.Core.Quotations
module R = WebSharper.Core.Reflection
module V = WebSharper.Compiler.Validator

type Dictionary<'T1,'T2> =
    System.Collections.Generic.Dictionary<'T1,'T2>

exception InvalidQuotation

let Literal x =
    match x with
    | Value.Bool true -> C.True
    | Value.Bool false -> C.False
    | Value.Double x -> C.Double x
    | Value.Int x -> C.Integer (int64 x)
    | Value.String x -> C.String x

let ( % ) format arg =
    System.String.Format(format, [| arg |])

let undef = !~ C.Undefined

let glob addr =
    let rec loop acc addr =
        match addr with
        | P.Global name -> name :: acc
        | P.Local (x, y) -> loop (y :: acc) x
    C.Global (loop [] addr)

let globParent addr =
    match addr with
    | P.Global _ -> C.Global []
    | P.Local (a, _) -> glob a

let str x = !~ (C.String x)

let i (x: int) = !~ (C.Integer (int64 x))

let call obj name args =
    C.Call (obj, str name, args)

let callAt addr args =
    let (obj, name) =
        match addr with
        | P.Global name -> (C.Global [], name)
        | P.Local (addr, name) -> (glob addr, name)
    call obj name args

let fieldNameOpt (meta: Metadata.T) (n: string) (t: R.TypeDefinition) =
    match meta.DataType t with
    | Some (M.Record (_, fs)) | Some (M.Object fs) | Some (M.Class (_, _, fs)) ->
        let ok (x, y, opt) = if x = n then Some (str y, opt) else None
        match List.tryPick ok fs with
        | Some res -> res
        | None -> str n, false
    | _ ->
        str n, false

let (|Reraise|_|) (q: Q.Expression) =
    match q with
    | Q.Call (m, [])
    | Q.CallModule (m, []) ->
        if m.Entity.Name = "Reraise" || m.Entity.Name = "Rethrow"
           && m.Entity.DeclaringType.FullName = "Microsoft.FSharp.Core.Operators"
        then Some ()
        else None
    | _ ->
        None

let ofArray =
    match <@ List.ofArray [| 0 |] @> with
    | Quotations.Patterns.Call (_, m, _) -> R.Method.Parse m
    | _ -> failwith "Unreachable."

let (|ListLiteral|_|) q =
    let fN = typedefof<list<_>>.FullName
    let rec loop acc q =
        match q with
        | Q.NewUnionCase (t, args) ->
            if t.Entity.DeclaringType.FullName = fN then
                match args with
                | [] -> Some (List.rev acc)
                | [x; xs] -> loop (x :: acc) xs
                | _ -> None
            else None
        | _ -> None
    match loop [] q with
    | None | Some [] -> None
    | Some xs ->
        match q with
        | Q.NewUnionCase (t, _) ->
            let eT = t.Generics.[0]
            Q.CallModule (
                {Entity=ofArray; Generics=[eT]},
                [Q.NewArray (eT, xs)]
            )
            |> Some
        | _ -> None

#nowarn "25"

type Var =
    {
        Id: C.Id
        IsRef: bool
    }

    member this.Init(x) =
        if this.IsRef then C.NewArray [x] else x

    member this.Get =
        if this.IsRef then
            C.FieldGet(C.Var this.Id, !~(C.Integer 0L))
        else
            C.Var this.Id

    member this.Set(x) =
        if this.IsRef then
            C.FieldSet(C.Var this.Id, !~(C.Integer 0L), x)
        else
            C.VarSet(this.Id, x)

    static member SimpleVar v =
        {
            Id = v
            IsRef = false
        }

    static member Ref (v: C.Id) =
        assert v.IsMutable
        {
            Id = v
            IsRef = true
        }

let rec containsInClosure (<?) (<!) checkVar (v: Q.Id) (q: Q.Expression) =
    let (!?) e = v <? e
    let (!??) = List.exists (!?)
    let (!!) e = v <! e
    let isNotV (v': Q.Id) = v.Name <> v'.Name
    match q with
    | Q.DefaultValue _
    | Q.FieldGetStatic _
    | Q.Hole _
    | Q.Value _ ->
        false
    | Q.AddressOf x
    | Q.Coerce (_, x)
    | Q.FieldGetInstance (x, _)
    | Q.FieldGetRecord (x, _)
    | Q.FieldGetUnion (x, _, _)
    | Q.FieldSetStatic (_, x)
    | Q.NewDelegate (_, x)
    | Q.Quote x
    | Q.TupleGet (_, x)
    | Q.TypeTest (_, x)
    | Q.UnionCaseTest (_, x) ->
        !?x
    | Q.AddressSet (x, y)
    | Q.Application (x, y)
    | Q.FieldSetInstance (x, _, y)
    | Q.FieldSetRecord (x, _, y)
    | Q.Sequential (x, y)
    | Q.TryFinally (x, y)
    | Q.WhileLoop (x, y) ->
        !?x || !?y
    | Q.IfThenElse (x, y, z) ->
        !?x || !?y || !?z
    | Q.Call (_, xs)
    | Q.CallModule (_, xs)
    | Q.NewArray (_, xs)
    | Q.NewObject (_, xs)
    | Q.NewRecord (_, xs)
    | Q.NewTuple xs
    | Q.NewUnionCase (_, xs)
    | Q.PropertyGet (_, xs)
    | Q.PropertySet (_, xs) ->
        !??xs
    | Q.ForIntegerRangeLoop (v', x, y, z) ->
        !?x || !?y || (isNotV v' && !?z)
    | Q.Lambda (v', x) ->
        isNotV v' && !!x
    | Q.Let (v', x, y) ->
        !?x || (isNotV v' && !?y)
    | Q.LetRecursive (vs, x) ->
        (
            (vs |> List.forall (fun (v', _) -> isNotV v')) &&
            (vs |> List.exists (fun (_, y) -> !?y))
        ) || !?x
    | Q.TryWith (x, vy, y, vz, z) ->
        !?x || (isNotV vy && !?y) || (isNotV vz && !?z)
    | Q.Var v' -> checkVar v'
    | Q.VarSet (v', x) -> checkVar v' || !?x
    | Q.SourcePos (q, _) -> !?q
    | q -> failwithf "Unknown quotation: %A" q
/// True if q contains a closed-over reference to v.
and (<?) v q = containsInClosure (<?) (<!) (fun _ -> false) v q
/// True if q contains any kind of reference to v.
and (<!) v q = containsInClosure (<!) (<!) ((=) v) v q

let Translate (logger: Logger) (iP: Inlining.Pool) (mP: Reflector.Pool) remotingProvider
    (meta: Metadata.T) (here: Location) (expr: Q.Expression) =

    let log priority message =
        logger.Log {
            Priority = priority
            Location = here
            Text = message
        }

    let warn msg = log Warning msg; undef
    let error msg = log Error msg; undef
    let err msg x = error (System.String.Format("{0}: {1}.", msg, x))

    let d = Dictionary()
    /// let-bound var, potentially mutable, transform to a ref if necessary
    let mkVar (x: Q.Id) (e: Q.Expression) =
        let isMutableToRef = x.Mutable && x <? e
        let y =
            C.Id(x.Name, x.Mutable)
            |> if isMutableToRef then Var.Ref else Var.SimpleVar
        d.Add(x, y)
        y.Id, y.Init
    /// other types of variable binding, can't be mutable
    let simpleVar (x: Q.Id) =
        match d.TryGetValue(x) with
        | true, v -> v.Id
        | false, _ ->
            let v = C.Id(x.Name, x.Mutable)
            d.Add(x, Var.SimpleVar v)
            v
    let varGet x =
        match d.TryGetValue(x) with
        | true, v -> v.Get
        | false, _ ->
            // should only happen when x is the "this" of a method being defined
            let v = C.Id(x.Name, x.Mutable)
            d.[x] <- Var.SimpleVar v
            C.Var v
    let varSet x =
        match d.TryGetValue(x) with
        | true, v -> v.Set
        | false, _ -> fun _ -> err "Unknown id" x

    let rec tCall exn allowMacro q methodKind args =
        let inline (!) q = tExpr exn allowMacro q
        let inline (!!) q = List.map (!) q
        let invalidQuot() =
            printfn "Invalid quotation, method kind: %A" methodKind
            raise InvalidQuotation
        let methodKind =
            if allowMacro then methodKind else
            match methodKind with M.MacroMethod(_, Some m) -> m | _ -> methodKind
        match methodKind with
        | M.BasicInstanceMethod x ->
            match args with
            | t :: xs -> call !t x !!xs
            | [] -> invalidQuot()
        | M.BasicStaticMethod fn ->
            callAt fn !!args
        | M.InlineMethod f ->
            match iP.Parse f with
            | Inlining.Transformer f -> f (!) !!args
            | _ -> error "Unexpected inline expansion error."
        | M.MacroMethod (t, _) ->
            (mP.LoadMacro t).Translate(Q.NoMacro q, (!))
        | M.RemoteMethod (scope, kind, handle) ->
            let name =
                match kind with
                | V.RemoteAsync -> "Async"
                | V.RemoteSend -> "Send"
                | V.RemoteSync -> "Sync"
            let str x = !~ (C.String x)
            let args =
                match scope, args with
                | Instance, _ :: args | _, args -> C.NewArray !!args
            let provider = C.Global remotingProvider
            C.Call (provider, str name, [str (handle.Pack()); args])
    and tCons exn q consKind args =
        let inline (!) q = tExpr exn true q
        let inline (!!) q = List.map (!) q
        match consKind with
        | M.BasicConstructor fn ->
            callAt fn !!args
        | M.InlineConstructor f ->
            let i = iP.Parse f
            match i with
            | Inlining.Transformer f -> f (!) !!args
            | _ -> error "Internal inline substitution error."
        | M.MacroConstructor (t, _) ->
            (mP.LoadMacro t).Translate(Q.NoMacro q, (!))
        | M.StubConstructor fn ->
            C.New (glob fn, !!args)
    and tExpr exn allowMacro quotation =
        let inline (!) q = tExpr exn true q
        let inline (!!) q = List.map (!) q
        let invalidQuot() =
            printfn "Invalid quotation: %A" quotation
            raise InvalidQuotation
        match quotation with
        | Q.SourcePos (x, pos) ->
            !x |> C.WithPos pos
        | Q.NoMacro x ->
            tExpr exn false x        
        | Q.AddressOf x ->
            error "Explicit address capture is not supported."
        | Q.AddressSet _ ->
            error "Explicit address assignment is not supported."
        | Q.Application (f, x) ->
            C.Application (!f, [!x])
        | Reraise ->
            match exn with
            | Some id -> C.Throw (C.Var id)
            | None -> invalidQuot()
        | Q.Call (m, args)
        | Q.CallModule (m, args) as q ->
            match meta.Method m.Entity with
            | Some (M.MacroMethod (_, b)) when not allowMacro ->
                match b with
                | Some bkind -> tCall exn true q bkind args
                | None -> err "Failed to translate a method call with macro fallback." m.Entity 
            | Some k -> tCall exn true q k args
            | None -> err "Failed to translate a method call." m.Entity
        | Q.Coerce (t, x) ->
            match t with
            | R.Type.Concrete (tD, _) when tD.FullName = "System.Object" ->
                let warnTupled() = log Warning "Tupled function coerced to object."
                let warnCurried() = log Warning "Curried function coerced to object."
                match x with
                | Q.Lambda (v, _) when v.Name = "tupledArg" -> warnTupled()
                | Q.Lambda (_, Q.Lambda _) -> warnCurried()
                | Q.Var v ->
                    match v.Type with
                    | R.Type.Concrete (tD, [d; r]) when tD.FullName.StartsWith "Microsoft.FSharp.Core.FSharpFunc" ->
                        match d, r with
                        | R.Type.Concrete (tD, _), _ when tD.FullName.StartsWith "System.Tuple" ->
                            warnTupled()
                        | _, R.Type.Concrete (tD, _) when tD.FullName.StartsWith "Microsoft.FSharp.Core.FSharpFunc" -> 
                            warnCurried()
                        | _ -> ()
                    | _ -> ()
                | _ -> ()
            | _ -> ()
            !x
        | Q.DefaultValue _ ->
            undef
        | Q.FieldGetInstance (e, f) ->
            let fn, opt = fieldNameOpt meta f.Entity.Name f.Entity.DeclaringType
            if opt then call C.Runtime "GetOptional" [(!e).[fn]] else (!e).[fn]
        | Q.FieldGetRecord (e, f) ->
            let fn, opt = fieldNameOpt meta f.Entity.Name f.Entity.DeclaringType
            if opt then call C.Runtime "GetOptional" [(!e).[fn]] else (!e).[fn]
        | Q.FieldGetStatic f
        | Q.FieldSetStatic (f, _) ->
            err "Static fields are not supported" f.Entity
        | Q.FieldGetUnion (e, uc, k) ->
            (!e).[str ("$" + string k)]
        | Q.FieldSetInstance (t, f, v) ->
            let fn, opt = fieldNameOpt meta f.Entity.Name f.Entity.DeclaringType
            if opt then call C.Runtime "SetOptional" [!t; fn; !v] else C.FieldSet (!t, fn, !v)
        | Q.FieldSetRecord (t, f, v) ->
            let fn, opt = fieldNameOpt meta f.Entity.Name f.Entity.DeclaringType
            if opt then call C.Runtime "SetOptional" [!t; fn; !v] else C.FieldSet (!t, fn, !v)
        | Q.ForIntegerRangeLoop (v, min, max, body) ->
            C.ForIntegerRangeLoop (simpleVar v, !min, !max, !body)
        | Q.Hole _ ->
            error "Quotations holes are not supported."
        | Q.IfThenElse (c, t, e) ->
            C.IfThenElse (!c, !t, !e)
        | Q.Lambda (v, b) ->
            C.Lambda (None, [simpleVar v], !b)
        | Q.Let (var, value, body) ->
            let var, v = mkVar var body
            C.Let (var, v !value, !body)
        | Q.LetRecursive (vs, b) ->
            let f (var: Q.Id, value) = simpleVar var, !value
            C.LetRecursive (List.map f vs, !b)
        | Q.NewArray (_, x) ->
            C.NewArray (List.map (!) x)
        | Q.NewDelegate (t, x) ->
            let rec loop acc = function
                | Q.Lambda (var, body) -> loop (var :: acc) body
                | body -> (List.rev acc, body)
            match loop [] x with
            | (this :: vars, body) ->
                C.Lambda (Some (simpleVar this), List.map simpleVar vars, !body)
            | ([], Q.Application (f, Q.Value Q.Unit)) -> !f
            | _ -> err "Failed to translate delegate creation" t.FullName 
        | Q.NewObject (c, args) as q ->
            let tOpt = 
                match meta.Constructor c.Entity with
                | Some (M.MacroConstructor (_, b)) when not allowMacro ->
                    match b with
                    | Some bkind -> tCons exn q bkind args |> Some   
                    | None -> None
                | Some kind -> tCons exn q kind args |> Some 
                | None -> None
            match tOpt with
            | Some t -> t
            | _ ->
                match meta.DataType c.Entity.DeclaringType with
                | Some (M.Exception fn) ->
                    let init =
                        !!args
                        |> List.mapi (fun i a ->
                            ("$" + string i, a))
                        |> C.NewObject
                    call C.Runtime "New" [glob fn; init]
                | _ ->
                    err "Failed to translate object creation"
                        c.Entity.DeclaringType.FullName
        | Q.NewRecord (t, args) ->
            let getObjFromFields fields =
                if List.length fields = args.Length then
                    let init =
                        fields
                        |> List.map2 (fun v (_, f, o) -> 
                            f, if o then C.FieldGet (v, str "$0") else v
                        ) !!args
                    let optFields = 
                        fields |> List.choose (fun (_, f, o) -> 
                            if o then Some (str f) else None)
                    let obj = C.NewObject init
                    if List.isEmpty optFields then obj 
                    else call C.Runtime "DeleteEmptyFields" [obj; C.NewArray optFields]
                else
                    invalidQuot()
            match meta.DataType t.DeclaringType with
            | Some (M.Class (fn, _, _)) ->
                C.New (glob fn, !!args)
            | Some (M.Record (fn, fields)) ->                
                call C.Runtime "New" [glob fn; getObjFromFields fields]
            | Some (M.Object fields) ->
                getObjFromFields fields
            | _ ->
                err "Failed to translate record creation" t.FullName
        | Q.NewTuple x ->
            C.NewArray (List.map (!) x)
        | Q.NewUnionCase (uc, args) ->
            match quotation with
            | ListLiteral x -> !x
            | _ ->
                let mkUnion (tag: int) =
                    ("$", i tag)
                    :: List.mapi (fun i x -> ("${0}" % i, !x)) args
                    |> C.NewObject
                match meta.UnionCase uc.Entity with
                | Some (M.BasicUnionCase k) ->
                    mkUnion k
                | Some (M.CompiledUnionCase (fn, k)) ->
                    call C.Runtime "New" [glob fn; mkUnion k]
                | Some (M.ConstantUnionCase x) ->
                    !~ (Literal x)
                | None ->
                    err "Failed to translate union creation" uc.Entity
        | Q.PropertyGet (p, xs) as q ->
            match meta.Property p.Entity with
            | Some (M.BasicProperty (getter, setter)) ->
                match getter with
                | Some g -> tCall exn false q g xs
                | None -> invalidQuot()
            | Some (M.InstanceOptProperty x) ->
                match xs with
                | t :: _ -> call C.Runtime "GetOptional" [(!t).[str x]]
                | _ -> invalidQuot()
            | Some (M.StaticOptProperty fn) ->
                call C.Runtime "GetOptional" [(globParent fn).[str fn.LocalName]]    
            | Some (M.InterfaceProperty x) ->
                match xs with
                | t :: xs -> C.Call (!t, str ("get_" + x), !!xs)
                | _ -> invalidQuot()
            | Some (M.InstanceStubProperty x) ->
                match xs with
                | t :: _ -> (!t).[str x]
                | _ -> invalidQuot()
            | Some (M.StaticStubProperty fn) ->
                (globParent fn).[str fn.LocalName]
            | Some (M.FieldProperty k) ->
                match xs with
                | [this] -> (!this).[str ("$" + string k)]
                | _ -> err "Failed to translate property access" p.Entity
            | None ->
                err "Failed to translate property access" p.Entity
        | Q.PropertySet (p, xs) as q ->
            match meta.Property p.Entity with
            | Some (M.BasicProperty (getter, setter)) ->
                match setter with
                | Some f -> tCall exn false q f xs
                | None ->
                    match getter, xs with
                    | Some (M.BasicStaticMethod fn), [v] ->
                        let id = C.Id()
                        C.Let (id, !v,
                            C.FieldSet (globParent fn, str fn.LocalName,
                                C.Lambda (None, [], C.Var id)))
                    | Some (M.InlineMethod _), _ ->
                        err "Cannot assign to an inline property" p.Entity
                    | _ ->
                        err "Failed to translate assignment" p.Entity
            | Some (M.InstanceOptProperty x) ->
                match xs with
                | t :: v :: _ -> call C.Runtime "SetOptional" [!t; str x; !v]
                | _ -> invalidQuot()
            | Some (M.StaticOptProperty fn) ->
                match xs with
                | [v] -> call C.Runtime "SetOptional" [globParent fn; str fn.LocalName; !v]
                | _ -> err "Cannot set an indexed optional property" fn.LocalName
            | Some (M.InstanceStubProperty x) ->
                match xs with
                | t :: v :: _ -> C.FieldSet (!t, str x, !v)
                | _ -> invalidQuot()
            | Some (M.InterfaceProperty x) ->
                match xs with
                | t :: xs -> C.Call (!t, str ("set" + x), !!xs)
                | _ -> invalidQuot()
            | Some (M.StaticStubProperty fn) ->
                match xs with
                | [v] -> C.FieldSet (globParent fn, str fn.LocalName, !v)
                | _ -> err "Cannot set an indexed stub property" fn.LocalName
            | None | Some (M.FieldProperty _) ->
                err "Failed to translate property assignment" p.Entity
        | Q.Quote q -> tExpr exn allowMacro q
        | Q.Sequential (x, y) ->
            C.Sequential (!x, !y)
        | Q.TryFinally (x, y) ->
            C.TryFinally (!x, !y)
        | Q.TryWith (x, _, _, v, y) ->
            let v = simpleVar v
            C.TryWith (!x, v, tExpr (Some v) true y)
        | Q.TupleGet (i, e) ->
            (!e).[!~ (C.Integer (int64 i))]
        | Q.TypeTest (t, e) ->
            let typeof x = (!e).TypeOf &== str x
            match t with
            | R.Type.Concrete (t, []) ->
                match t.FullName with
                | "Microsoft.FSharp.Core.Unit"
                | "System.Void" ->
                    typeof "undefined"
                | "System.Boolean" ->
                    typeof "boolean"
                | "System.Byte"
                | "System.SByte"
                | "System.Char"
                | "System.Single"
                | "System.Double"
                | "System.Int16"
                | "System.Int32"
                | "System.Int64"
                | "System.UInt16"
                | "System.UInt32"
                | "System.UInt64" ->
                    typeof "number"
                | "System.String" ->
                    typeof "string"
                | "System.IDisposable" ->
                    (!e).[!~(C.String "Dispose")] &!= !~C.Undefined
                | _ ->
                    match meta.DataType t with
                    | None | Some (M.Object _) | Some (M.Interface _) ->
                        err "Failed to compile a type test: " t.FullName
                    | Some (M.Class (fn, _, _))
                    | Some (M.Record (fn, _))
                    | Some (M.Exception fn) ->
                        (!e).InstanceOf(glob fn)
            | _ ->
                error "Type tests do not support generic and array types."
        | Q.UnionCaseTest (uc, e) ->
            match meta.UnionCase uc.Entity with
            | Some (M.BasicUnionCase k) | Some (M.CompiledUnionCase (_, k)) ->
                (!e).[str "$"] &== i k
            | Some (M.ConstantUnionCase x) ->
                !e &=== !~ (Literal x)
            | None ->
                err "Failed to compile union case test" uc.Entity
        | Q.Value lit ->
            let i x = !~ (C.Integer x)
            match lit with
            | Q.Unit -> !~ C.Null
            | Q.Bool x -> if x then !~C.True else !~C.False
            | Q.String x -> str x
            | Q.Int x -> i (int64 x)
            | Q.Double x ->
                if System.Double.IsNaN x then
                    C.Global ["NaN"]
                elif System.Double.IsPositiveInfinity x then
                    C.Global ["Infinity"]
                elif System.Double.IsNegativeInfinity x then
                    - (C.Global ["Infinity"])
                else
                    !~ (C.Double x)
            | Q.Single x ->
                if System.Single.IsNaN x then
                    C.Global ["NaN"]
                elif System.Single.IsPositiveInfinity x then
                    C.Global ["Infinity"]
                elif System.Single.IsNegativeInfinity x then
                    - (C.Global ["Infinity"])
                else
                    !~ (C.Double (double x))
            | Q.Char x -> !~ (C.Integer (int64 x))
            | Q.SByte x -> i (int64 x)
            | Q.Byte x -> i (int64 x)
            | Q.Int16 x -> i (int64 x)
            | Q.UInt16 x -> i (int64 x)
            | Q.UInt32 x -> i (int64 x)
            | Q.Int64 x -> i (int64 x)
            | Q.UInt64 x -> i (int64 x)
        | Q.Var x -> varGet x
        | Q.VarSet (x, y) -> varSet x !y
        | Q.WhileLoop (x, y) ->
            C.WhileLoop (!x, !y)

    tExpr None true expr
