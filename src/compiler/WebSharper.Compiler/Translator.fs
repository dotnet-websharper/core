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

let eliminateDispose q =
    match q with
    | Q.TryFinally
      (
        body,
        Q.IfThenElse (
            Q.TypeTest (iDisposable, _),
            Q.Call (dispose, _),
            _
        )
      ) when iDisposable.FullName = "System.IDisposable"
          && dispose.Entity.DeclaringType.FullName = "System.IDisposable"
          && dispose.Entity.Name = "Dispose" ->
        body
    | _ ->
        q

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

    let (!^) =
        let d = Dictionary()
        fun (x: Q.Id) ->
            match d.TryGetValue x with
            | true, y -> y
            | _ ->
                let y = C.Id (x.Name, x.Mutable)
                d.[x] <- y
                y

    let rec tCall exn q methodKind args =
        let (!) = tExpr exn
        let (!!) = List.map (!)
        let invalidQuot() =
            printfn "Invalid quotation, method kind: %A" methodKind
            raise InvalidQuotation
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
        | M.MacroMethod t ->
            (mP.Load t).Expand (!) q
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
    and tExpr exn quotation =
        let (!) = tExpr exn
        let (!!) = List.map (!)
        let invalidQuot() =
            printfn "Invalid quotation: %A" quotation
            raise InvalidQuotation
        match quotation with
        | Q.SourcePos (x, pos) ->
            !x |> C.WithPos pos
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
            | Some k -> tCall exn q k args
            | None -> err "Failed to translate a method call" m.Entity
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
            err "Static fields are not supported" f.Entity.Name
        | Q.FieldGetUnion (e, uc, k) ->
            (!e).[str ("$" + string k)]
        | Q.FieldSetInstance (t, f, v) ->
            let fn, opt = fieldNameOpt meta f.Entity.Name f.Entity.DeclaringType
            if opt then call C.Runtime "SetOptional" [!t; fn; !v] else C.FieldSet (!t, fn, !v)
        | Q.FieldSetRecord (t, f, v) ->
            let fn, opt = fieldNameOpt meta f.Entity.Name f.Entity.DeclaringType
            if opt then call C.Runtime "SetOptional" [!t; fn; !v] else C.FieldSet (!t, fn, !v)
        | Q.ForIntegerRangeLoop (v, min, max, body) ->
            C.ForIntegerRangeLoop (!^v, !min, !max, !body)
        | Q.Hole _ ->
            error "Quotations holes are not supported."
        | Q.IfThenElse (c, t, e) ->
            C.IfThenElse (!c, !t, !e)
        | Q.Lambda (v, b) ->
            C.Lambda (None, [!^v], !b)
        | Q.Let (var, value, body) ->
            C.Let (!^var, !value, !body)
        | Q.LetRecursive (vs, b) ->
            let f (var: Q.Id, value) = !^var, !value
            C.LetRecursive (List.map f vs, !b)
        | Q.NewArray (_, x) ->
            C.NewArray (List.map (!) x)
        | Q.NewDelegate (_, x) ->
            let rec loop acc = function
                | Q.Lambda (var, body) -> loop (var :: acc) body
                | body -> (List.rev acc, body)
            match loop [] x with
            | (this :: vars, body) ->
                C.Lambda (Some !^this, List.map (!^) vars, !body)
            | ([], Q.Application (f, Q.Value Q.Unit)) -> !f
            | _ -> invalidQuot()
        | Q.NewObject (c, args) ->
            match meta.Constructor c.Entity with
            | Some (M.BasicConstructor fn) ->
                callAt fn !!args
            | Some (M.InlineConstructor f) ->
                let i = iP.Parse f
                match i with
                | Inlining.Transformer f -> f (!) !!args
                | _ -> error "Internal inline substitution error."
            | Some (M.MacroConstructor t) ->
                (mP.Load t).Expand (!) quotation
            | Some (M.StubConstructor fn) ->
                C.New (glob fn, !!args)
            | None ->
                match meta.DataType c.Entity.DeclaringType with
                | Some (M.Exception fn) ->
                    let init =
                        !!args
                        |> List.mapi (fun i a ->
                            ("$" + string i, a))
                        |> C.NewObject
                    C.New (glob fn, [init])
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
                C.New (glob fn, [getObjFromFields fields])
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
                    C.New (glob fn, [mkUnion k])
                | Some (M.ConstantUnionCase x) ->
                    !~ (Literal x)
                | None ->
                    err "Failed to translate union creation" uc.Entity.Name
        | Q.PropertyGet (p, xs) as q ->
            match meta.Property p.Entity with
            | Some (M.BasicProperty (getter, setter)) ->
                match getter with
                | Some g -> tCall exn q g xs
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
                | _ -> err "Failed to translate property access" p.Entity.Name
            | None ->
                err "Failed to translate property access" p.Entity.Name
        | Q.PropertySet (p, xs) as q ->
            match meta.Property p.Entity with
            | Some (M.BasicProperty (getter, setter)) ->
                match setter with
                | Some f -> tCall exn q f xs
                | None ->
                    match getter, xs with
                    | Some (M.BasicStaticMethod fn), [v] ->
                        let id = C.Id()
                        C.Let (id, !v,
                            C.FieldSet (globParent fn, str fn.LocalName,
                                C.Lambda (None, [], C.Var id)))
                    | Some (M.InlineMethod _), _ ->
                        err "Cannot assign to an inline property" p.Entity.Name
                    | _ ->
                        err "Failed to translate assignment" p.Entity.Name
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
                err "Failed to translate property assignment" p.Entity.Name
        | Q.Quote _ ->
            error "Quotations are not supported."
        | Q.Sequential (x, y) ->
            C.Sequential (!x, !y)
        | Q.TryFinally (x, y) as q ->
            match eliminateDispose q with
            | Q.TryFinally (x, y) -> C.TryFinally (!x, !y)
            | q -> !q
        | Q.TryWith (x, _, _, v, y) ->
            let v = !^v
            C.TryWith (!x, v, tExpr (Some v) y)
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
                err "Failed to compile union case test" uc.Entity.Name
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
        | Q.Var x -> C.Var !^x
        | Q.VarSet (x, y) -> C.VarSet (!^x, !y)
        | Q.WhileLoop (x, y) ->
            C.WhileLoop (!x, !y)

    tExpr None expr
