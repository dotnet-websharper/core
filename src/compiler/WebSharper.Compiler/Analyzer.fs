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

module WebSharper.Compiler.Analyzer

open System
open System.Collections
open System.Collections.Generic

module M = WebSharper.Core.Metadata
module Ma = WebSharper.Core.Macros
module Q = WebSharper.Core.Quotations
module R = WebSharper.Core.Reflection
module Re = WebSharper.Core.Resources
module V = WebSharper.Compiler.Validator

type D<'T1,'T2> = Dictionary<'T1,'T2>
type S<'T> = HashSet<'T>

let rec Visit f (expr: Q.Expression) =
    f expr
    expr |> Q.Transform (fun e -> Visit f e; e) |> ignore

/// Finds all outgoing calls, field accesses and
/// constructor expressions in the quotation.
let GetCalls (expr: Q.Expression) =
    let res = ResizeArray()
    let ( ~+ ) s = res.Add(s) |> ignore
    let visitor expr =
        match expr with
        | Q.Call (m, xs) | Q.CallModule (m, xs) ->
            + M.MethodNode m.Entity;
            + M.TypeNode m.Entity.DeclaringType
        | Q.FieldGetRecord (_, prop) | Q.FieldSetRecord (_, prop, _) ->
            + M.TypeNode prop.Entity.DeclaringType
        | Q.FieldGetStatic fld | Q.FieldSetStatic (fld, _) ->
            + M.TypeNode fld.Entity.DeclaringType
        | Q.NewObject (ctor, xs) ->
            + M.ConstructorNode ctor.Entity;
            + M.TypeNode ctor.Entity.DeclaringType
        | Q.NewRecord (t, xs) ->
            + M.TypeNode t.DeclaringType
        | Q.NewUnionCase (t, xs) ->
            + M.TypeNode t.Entity.DeclaringType
        | Q.PropertyGet (p, _) | Q.PropertySet (p, _) ->
            + M.TypeNode p.Entity.DeclaringType
        | _ -> ()
    Visit visitor expr
    Seq.toList (Seq.distinct res)

let Analyze (metas: list<M.AssemblyInfo>) (assembly: V.Assembly) =
    let info = M.AssemblyInfo.Create assembly.Name
    let deps : Graphs.Graph<M.Node> = Graphs.Empty
    let visitConstructor dT (c: V.Constructor) =
        let src = M.ConstructorNode c.Reference
        deps.Connect src dT
        for rT in c.Requirements do
            deps.Connect src (M.ResourceNode rT)
        match c.Kind with
        | V.InlineConstructor inl ->
            match inl.Quotation with
            | None -> ()
            | Some e -> List.iter (deps.Connect src) (GetCalls e)
        | V.JavaScriptConstructor e ->
            List.iter (deps.Connect src) (GetCalls e)
        | _ -> ()
    let visitMethod src (m: V.Method) =
        m.Requirements
        |> List.iter (fun rT -> deps.Connect src (M.ResourceNode rT))
        match m.Kind with
        | V.InlineMethod inl ->
            match inl.Quotation with
            | None -> ()
            | Some e -> List.iter (deps.Connect src) (GetCalls e)
        | V.JavaScriptMethod e ->
            List.iter (deps.Connect src) (GetCalls e)
        | V.RemoteMethod (_, ref) ->
            ref := Some (info.AddRemoteMethod m.Reference)
        | _ -> ()
    let visitProperty src (p: V.Property) =
        match p.Kind with
        | V.BasicProperty (g, s) ->
            Option.iter (visitMethod src) g
            Option.iter (visitMethod src) s
        | V.InlineModuleProperty i ->
            match i.Quotation with
            | None -> ()
            | Some e -> List.iter (deps.Connect src) (GetCalls e)
        | V.JavaScriptModuleProperty e ->
            List.iter (deps.Connect src) (GetCalls e)
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
        | V.Class c ->
            c.BaseClass
            |> Option.iter (fun bT ->
                deps.Connect self (M.TypeNode bT.DeclaringType))
            List.iter (visitConstructor self) c.Constructors
            List.iter (visitMethod self) t.Methods
            List.iter (visitProperty self) t.Properties
            List.iter (visitType assem self) c.Nested
            info.AddRecord t.Reference [for (o, j, _) in c.FieldRenames -> (o, j)]
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
            if not (List.isEmpty t.Properties) then
                deps.Connect assem self
            List.iter vm t.Methods
            List.iter (visitProperty assem) t.Properties
            List.iter (visitType assem self) nested
        | V.Record fields ->
            info.AddRecord t.Reference [for f in fields -> (f.OriginalName, f.JavaScriptName)]
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
            |> Seq.collect (fun requirement ->
                let rest =
                    match req.TryGetValue(requirement) with
                    | false, _ -> []
                    | true, xs -> xs
                match requirement with
                | M.AssemblyNode (id, M.CompiledAssembly) ->
                    rest @ [M.AssemblyResource id]
                | M.ResourceNode id ->
                    rest @ [M.UserResource id]
                | _ -> rest)
            |> Seq.distinct
            |> Seq.toList
        match res with
        | [] -> ()
        | res -> info.AddRequirement node res)
    info
