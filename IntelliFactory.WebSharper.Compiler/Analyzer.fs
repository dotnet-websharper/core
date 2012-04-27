// $begin{copyright}
//
// This file is part of WebSharper
//
// Copyright (c) 2008-2012 IntelliFactory
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

module IntelliFactory.WebSharper.Compiler.Analyzer

module M = IntelliFactory.WebSharper.Core.Metadata
module Q = IntelliFactory.WebSharper.Core.Quotations
module R = IntelliFactory.WebSharper.Core.Reflection
module Re = IntelliFactory.WebSharper.Core.Resources
module V = IntelliFactory.WebSharper.Compiler.Validator

type D<'T1,'T2> = System.Collections.Generic.Dictionary<'T1,'T2>
type S<'T> = System.Collections.Generic.HashSet<'T>

/// Finds all outgoing calls, field accesses and
/// constructor expressions in the quotation.
let getCalls (expr: Q.Expression) =
    let rec f ds expr =
        match expr with
        | Q.Call (m, xs) ->
            let ds =
                M.MethodNode m.Entity
                :: M.TypeNode m.Entity.DeclaringType
                :: ds
            List.fold f ds xs
        | Q.CallModule (m, xs) ->
            let ds =
                M.MethodNode m.Entity
                :: M.TypeNode m.Entity.DeclaringType
                :: ds
            List.fold f ds xs
        | Q.FieldGetStatic fld ->
            M.TypeNode fld.Entity.DeclaringType :: ds
        | Q.FieldSetStatic (fld, x) ->
            f (M.TypeNode fld.Entity.DeclaringType :: ds) x
        | Q.NewObject (ctor, xs) ->
            let ds =
                M.ConstructorNode ctor.Entity
                :: M.TypeNode ctor.Entity.DeclaringType
                :: ds
            List.fold f ds xs
        | Q.NewRecord (t, xs) ->
            List.fold f (M.TypeNode t.DeclaringType :: ds) xs
        | Q.NewUnionCase (t, xs) ->
            List.fold f (M.TypeNode t.Entity.DeclaringType :: ds) xs
        | Q.PropertyGet (p, xs) ->
            List.fold f (M.TypeNode p.Entity.DeclaringType :: ds) xs
        | Q.PropertySet (p, xs) ->
            List.fold f (M.TypeNode p.Entity.DeclaringType :: ds) xs
        | _ ->
            Q.Fold f ds expr
    f [] expr
    |> Seq.distinct
    |> Seq.toList

let Analyze (metas: list<M.AssemblyInfo>) (assembly: V.Assembly) =
    let info = M.AssemblyInfo.Create assembly.Name
    let deps : Graphs.Graph<M.Node> = Graphs.Empty
    let visitConstructor dT (c: V.Constructor) =
        let src = M.ConstructorNode c.Reference
        deps.Connect src dT
        for rT in c.Requirements do
            deps.Connect src (M.ResourceNode rT)
        match c.Kind with
        | V.JavaScriptConstructor e ->
            List.iter (deps.Connect src) (getCalls e)
        | _ -> ()
    let visitMethod src (m: V.Method) =
        m.Requirements
        |> List.iter (fun rT -> deps.Connect src (M.ResourceNode rT))
        match m.Kind with
        | V.MacroMethod (_, mac) ->
            List.iter (deps.Connect src) mac.Requirements
        | V.InlineMethod inl ->
            match inl.Quotation with
            | None -> ()
            | Some e -> List.iter (deps.Connect src) (getCalls e)
        | V.JavaScriptMethod e ->
            List.iter (deps.Connect src) (getCalls e)
        | V.RemoteMethod (_, ref) ->
            ref := Some (info.AddRemoteMethod m.Reference)
        | V.StubMethod -> ()
    let visitProperty src (p: V.Property) =
        match p.Kind with
        | V.BasicProperty (g, s) ->
            Option.iter (visitMethod src) g
            Option.iter (visitMethod src) s
        | V.InlineModuleProperty i ->
            match i.Quotation with
            | None -> ()
            | Some e -> List.iter (deps.Connect src) (getCalls e)
        | V.JavaScriptModuleProperty e ->
            List.iter (deps.Connect src) (getCalls e)
        | _ -> ()
    let rec visitType assem ctx (t: V.Type) =
        let self =
            match t.Kind with
            | V.Resource ->
                let self = M.ResourceNode t.Reference
                deps.Add self
                self
            | _ ->
                let self = M.TypeNode t.Reference
                deps.Connect self ctx
                self
        t.Requirements
        |> List.iter (fun rT -> deps.Connect self (M.ResourceNode rT))
        t.Proxy
        |> Option.iter (fun pT -> deps.Connect (M.TypeNode pT) self)
        match t.Kind with
        | V.Class (_, bT, ctors, nested) ->
            bT
            |> Option.iter (fun bT ->
                deps.Connect self (M.TypeNode bT.DeclaringType))
            List.iter (visitConstructor self) ctors
            List.iter (visitMethod self) t.Methods
            List.iter (visitProperty self) t.Properties
            List.iter (visitType assem self) nested
        | V.Resource -> ()
        | V.Exception ->
            List.iter (visitMethod self) t.Methods
            List.iter (visitProperty self) t.Properties
        | V.Interface ->
            List.iter (visitMethod self) t.Methods
            List.iter (visitProperty self) t.Properties
        | V.Module nested ->
            let vm (m: V.Method) =
                let mn = M.MethodNode m.Reference
                deps.Connect mn self
                visitMethod mn m
            List.iter vm t.Methods
            List.iter (visitProperty assem) t.Properties
            List.iter (visitType assem self) nested
        | V.Record fields ->
            info.AddRecord t.Reference fields
            List.iter (visitMethod self) t.Methods
            List.iter (visitProperty self) t.Properties
        | V.Union cases ->
            List.iter (visitMethod self) t.Methods
            List.iter (visitProperty self) t.Properties
    let self = M.AssemblyNode (assembly.Name, assembly.Mode)
    assembly.Requirements
    |> List.iter (fun rT -> deps.Connect self (M.ResourceNode rT))
    assembly.Types
    |> List.iter (visitType self self)
    deps.Add self
    R.TypeDefinition.FromType typeof<Re.Runtime>
    |> M.ResourceNode
    |> deps.Connect self
    let rec visitType (ty: V.Type) =
        match ty.Status with
        | V.Compiled ->
            info.AddCompiledType (defaultArg ty.Proxy ty.Reference) ty.Name
        | _ -> ()
        List.iter visitType ty.Nested
    List.iter visitType assembly.Types
    let req = D()
    for m in metas do
        for (key, keys) in m.Requirements do
            req.[key] <- keys
    deps.Nodes
    |> List.iter (fun node ->
        let res =
            deps.Walk node
            |> Seq.collect (fun node ->
                let rest =
                    match req.TryGetValue node with
                    | false, _ -> []
                    | true, xs -> xs
                match node with
                | M.AssemblyNode (id, M.CompiledAssembly) ->
                    M.AssemblyResource id :: rest
                | M.ResourceNode id ->
                    M.UserResource id :: rest
                | _ -> rest)
            |> Seq.distinct
            |> Seq.toList
        match res with
        | [] -> ()
        | res -> info.AddRequirement node res)
    info
