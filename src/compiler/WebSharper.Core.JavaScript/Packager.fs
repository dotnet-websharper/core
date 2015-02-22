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

module WebSharper.Core.JavaScript.Packager

#nowarn "40"

module C = WebSharper.Core.JavaScript.Core
module S = WebSharper.Core.JavaScript.Syntax

type D<'T1,'T2> = System.Collections.Generic.Dictionary<'T1,'T2>
type S<'T> = System.Collections.Generic.HashSet<'T>
type Q<'T> = System.Collections.Generic.Queue<'T>

type Address =
    | Global of string
    | Local of Address * string

    member this.Parent =
        match this with
        | Global _ -> None
        | Local (a, _) -> Some a

    member this.LocalName =
        match this with
        | Global n | Local (_, n) -> n

    override this.ToString() =
        use w = new System.IO.StringWriter()
        let rec loop a =
            match a with
            | Local (a, n) -> loop a; w.Write '.'; w.Write n
            | Global n -> w.Write n
        loop this
        w.ToString()

type Expression =
    | Core of C.Expression
    | Syntax of S.Expression

    static member ( !! ) this =
        match this with
        | Core x -> x
        | Syntax s -> invalidArg "this" "Not a core expression."

type Member =
    | Field of Expression
    | Method of Expression

    override this.ToString() =
        match this with
        | Field x -> System.String.Format("(field {0}", x)
        | Method x -> System.String.Format("(method {0}", x)

type Binding =
    | Class of Class
    | Module of Module
    | Member of Member

    override this.ToString() =
        match this with
        | Class x -> string x
        | Member x -> string x
        | Module x -> string x

and Module = Map<string,Binding>

and Class =
    {
        Base : option<Address>
        Prototype : Map<string,Member>
        Static : Module
    }

type PackageInfo =
    {
        Definitions : Module
        Inheritance : D<C.Expression,C.Expression>
        SymbolBindings : Q<C.Id*C.Expression>
        SymbolTable : D<Address,C.Expression>
        SyntaxTable : D<S.Id,S.Expression>
    }

let rec convAddr (symbols: D<_,_>) (bindings: Q<_>) x =
    let safe x = C.Call (C.Runtime, !~ (C.String "Safe"), [x])
    match symbols.TryGetValue x with
    | true, e -> e
    | _ ->
        match x with
        | Global "WebSharper" ->
            C.Global ["WebSharper"]
        | Global name ->
            let id = C.Id name
            let e = C.Var id
            bindings.Enqueue(id, safe (C.Global [name]))
            symbols.[x] <- e
            e
        | Local (addr, name) ->
            let id = C.Id name
            let e = C.Var id
            let pA = convAddr symbols bindings addr
            bindings.Enqueue(id, safe ((?) pA name))
            symbols.[x] <- e
            e

let package (prefs: Preferences) (m: Module) : PackageInfo =
    let symbols = D()
    let bindings = Q()
    let inh = D()
    let syntax = D()
    let internSyntax (stx: S.Expression) =
        let id = string (System.Guid.NewGuid())
        syntax.[id] <- S.Close (Core.GlobalName prefs) stx
        C.Constant (C.String id)
    let rec mkAddr a =
        match a with
        | [x] -> Global x
        | x :: xs -> Local (mkAddr xs, x)
        | _ -> failwith "Unreachable."
    let convAddr = convAddr symbols bindings
    let convExpr e =
        let rec convExpr e =
            match e with
            | C.Global xs ->
                match xs with
                | [] -> e
                | ["undefined"] -> !~C.Undefined
                | xs -> convAddr (mkAddr (List.rev xs))
            | e -> C.Transform convExpr e
        match e with
        | Core e -> Core (convExpr e)
        | Syntax e -> Core (internSyntax e)
    let rec convClass (a: Address) (c: Class) =
        let bT =
            c.Base
            |> Option.map (fun b ->
                inh.[convAddr a] <- convAddr b
                b)
        let proto =
            c.Prototype
            |> Map.map (fun k v -> convMember v)
        let st = convModule a c.Static
        {
            Base = bT
            Prototype = proto
            Static = st
        }
    and convModule a (m: Module) =
        Map.map (convBinding (Some a)) m
    and convMember (v: Member) =
        match v with
        | Field e -> Field (convExpr e)
        | Method e -> Method (convExpr e)
    and convBinding a k v =
        let a =
            match a with
            | None -> Global k
            | Some a -> Local (a, k)
        match v with
        | Member e -> Binding.Member (convMember e)
        | Class c -> Binding.Class (convClass a c)
        | Module m -> Binding.Module (convModule a m)
    let m = Map.map (convBinding None) m
    {
        Definitions = m
        Inheritance = inh
        SymbolBindings = bindings
        SymbolTable = symbols
        SyntaxTable = syntax
    }

let definition (pkg: PackageInfo) =
    let defField e =
        C.Call (C.Runtime, !~(C.String "Field"), [!!e])
    let rec defMember m =
        match m with
        | Field e -> defField e
        | Method e -> !!e
    let rec defBinding m =
        match m with
        | Member x -> Some (defMember x)
        | Class c -> Some (defClass c)
        | Module m -> defModule m
    and defModule (m: Module) =
        if m.IsEmpty then None else
            C.NewObject [
                for KeyValue (k, v) in m do
                    let b = defBinding v
                    if b.IsSome then
                        yield (k, b.Value)
            ]
            |> Some
    and defClass (c: Class) =
        let args =
            [
                yield C.NewObject [
                    for KeyValue (k, v) in c.Prototype ->
                        (k, defMember v)
                ]
                yield! Option.toList (defModule c.Static)
            ]
        C.Call (C.Runtime, !~(C.String "Class"), args)
    let defs =
        match defModule pkg.Definitions with
        | None -> C.NewObject []
        | Some e -> e
    let args = [C.Global []; defs]
    C.Call (C.Runtime, !~(C.String "Define"), args)

let inheritance (pkg: PackageInfo) =
    let visited = S()
    let result = Q()
    let rec visit key =
        if not (visited.Contains key) then
            visited.Add key |> ignore
        match pkg.Inheritance.TryGetValue key with
        | true, value ->
            visit value
            let args = [key; value]
            result.Enqueue (C.Call (C.Runtime, !~(C.String "Inherit"), args))
        | _ ->
            ()
    Seq.iter visit pkg.Inheritance.Keys
    if Seq.isEmpty result then !~C.Undefined else
        let ( ++ ) a b = C.Sequential (a, b)
        C.Sequential (Seq.reduce (++) result, !~C.Undefined)

let initialization (pkg: PackageInfo) =
    let conv = convAddr pkg.SymbolTable pkg.SymbolBindings
    let run addr =
        match addr with
        | Global name -> C.Call (C.Global [], !~ (C.String name), [])
        | Local (addr, name) -> C.Call (conv addr, !~ (C.String name), [])
    let rec loop expr b (m: Module) =
        let loc x =
            match b with
            | None -> Global x
            | Some a -> Local (a, x)
        (expr, m)
        ||> Map.fold (fun e k v ->
            match v with
            | Member (Field _) -> C.Sequential (run (loc k), e)
            | Member _ -> e
            | Module m -> loop e (Some (loc k)) m
            | Class c ->
                let proto = loc "prototype"
                let e =
                    (e, c.Prototype)
                    ||> Seq.fold (fun e (KeyValue (k, v)) ->
                        match v with
                        | Field _ -> C.Sequential (run (Local (proto, k)), e)
                        | _ -> e)
                loop e (Some (loc k)) c.Static)
    loop !~C.Undefined None pkg.Definitions

let ( ++ ) a b =
    match a, b with
    | C.Constant C.Undefined, x
    | x, C.Constant C.Undefined -> x
    | _ -> C.Sequential (a, b)

[<Literal>]
let SET = "SET-3cb80272-711e-4d6d"

let withSymbols (pkg: PackageInfo) def expr =
    let tag = string (System.Guid.NewGuid())
    let str x = !~ (C.String x)
    let u = !~ C.Undefined
    let call t x y = C.Call (t, str x, y)
    let c x y = call C.Runtime x [C.Lambda (None, [], y)]
    let onInit =
        List.foldBack (fun (k, v) rest ->
            call C.Runtime SET [C.Var k; v] ++ rest)
            (List.ofSeq pkg.SymbolBindings)
            !~C.Undefined
        |> c "OnInit"
    let onLoad = c "OnLoad" expr
    if Seq.isEmpty pkg.SymbolBindings then
        def ++ onLoad
    else
        let bs = [for (k, _) in pkg.SymbolBindings -> (k, u)]
        C.LetRecursive (bs, def ++ onInit ++ onLoad)

let spliceSyntax (pkg: PackageInfo) (program: S.Program) : S.Program =
    let rec tE e =
        match e with
        | S.Constant (S.String x) ->
            match pkg.SyntaxTable.TryGetValue x with
            | true, e -> e
            | _ -> e
        | S.Application (S.Binary (_, S.BinaryOperator.``.``,
                            S.Constant (S.String SET)),
                            [S.Var id; value]) ->
            S.Binary(S.Var id, S.BinaryOperator.``=``, tE value)
        | _ ->
            S.TransformExpression tE tS e
    and tPE e =
        match e with
        | S.Action s -> S.Action (tS s)
        | S.Function (n, ids, body) -> S.Function (n, ids, tP body)
    and tP p = List.map tPE p
    and tS s = S.TransformStatement tE tS s
    tP program

let Package m prefs =
    let pkg = package prefs m
    let def =
        inheritance pkg ++ initialization pkg
        |> withSymbols pkg (definition pkg)
    let wrap x =
        (S.Lambda (None, [], spliceSyntax pkg x)).[[]]
        |> S.Optimize
        |> S.Ignore
        |> S.Action
    [wrap (C.ToProgram prefs def)]

let rec Simplify m =
    Map.toSeq m
    |> Seq.choose (fun (k, v) ->
        match v with
        | Binding.Module m ->
            let m = Simplify m
            if Map.isEmpty m then None else
                Some (k, Binding.Module m)
        | _ -> Some (k, v))
    |> Map.ofSeq
