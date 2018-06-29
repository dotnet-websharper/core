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

// Creates single .js files from WebSharper.Core.Metadata.Info
// (possibly filtered by code path analysis) 
module WebSharper.Compiler.Packager

open System.Collections.Generic

open WebSharper.Core
open WebSharper.Core.AST
module M = WebSharper.Core.Metadata

type EntryPointStyle =
    | OnLoadIfExists
    | ForceOnLoad
    | ForceImmediate

let packageAssembly (refMeta: M.Info) (current: M.Info) entryPointStyle scriptsBaseUrl =
    let addresses = Dictionary()
    let declarations = ResizeArray()
    let statements = ResizeArray()

    let g = Id.New "Global"
    let glob = Var g
    declarations.Add <| VarDeclaration (g, Var (Id.Global()))
    addresses.Add(Address [], glob)
    addresses.Add(Address [ "self" ], glob)
    let safeObject expr = Binary(expr, BinaryOperator.``||``, Object []) 
    
    let rec getAddress (address: Address) =
        match addresses.TryGetValue address with
        | true, v -> v
        | _ ->
            match address.Value with
            | [] -> glob
            | [ name ] ->
                let var = Id.New (if name.StartsWith "StartupCode$" then "SC$1" else name)
                let f = Value (String name)
                declarations.Add <| VarDeclaration (var, ItemSet(glob, f, ItemGet(glob, f, Pure) |> safeObject))                
                let res = Var var
                addresses.Add(address, res)
                res
            | name :: r ->
                let parent = getAddress (Hashed r)
                let f = Value (String name)
                let var = Id.New name
                declarations.Add <| VarDeclaration (var, ItemSet(parent, f, ItemGet(parent, f, Pure) |> safeObject))                
                let res = Var var
                addresses.Add(address, res)
                res

    let getFieldAddress (address: Address) =
        match address.Value with
        | name :: r ->
            getAddress (Hashed r), Value (String name)
        | _ -> failwith "packageAssembly: empty address"
    
    let rec getOrImportAddress (full: bool) (address: Address) =
        match addresses.TryGetValue address with
        | true, v -> v
        | _ ->
            match address.Value with
            | [] -> glob
            | h :: t ->
                let parent = getOrImportAddress false (Address t)
                let import = ItemGet(parent, Value (String h), Pure)
                if full then
                    import
                else
                    let var = Id.New (if h = "jQuery" && List.isEmpty t then "$" else h)
                    let importWithCheck =
                        if List.isEmpty t then import else parent ^&& import
                    declarations.Add <| VarDeclaration (var, importWithCheck)                
                    let res = Var var
                    addresses.Add(address, res)
                    res

    let globalAccessTransformer =
        { new Transformer() with
            override this.TransformGlobalAccess a = getOrImportAddress true a
        }
            
    let package a expr =
        let o, x = getFieldAddress a
        statements.Add <| ExprStatement (ItemSet (o, x, expr))    

    let packageCtor a expr =
        let o, x = getFieldAddress a
        let av = 
            match getAddress a with
            | Var v -> v
            | _ -> failwith "packageCtor error"
        statements.Add <| ExprStatement (VarSet (av, ItemSet (o, x, expr)))    

    let packageCctor a expr name =
        let o, x = getFieldAddress a
        match expr with
        | Function ([], body) ->
            let rem = ExprStatement (ItemSet (o, x, ItemGet(glob, Value (String "ignore"), Pure)))    
            let expr = Function([], Block [rem; body])
            statements.Add <| ExprStatement (ItemSet (o, x, expr))    
        | _ ->
            failwithf "Static constructor must be a function for type %s: %A" name (Debug.PrintExpression expr)

    let classes = Dictionary(current.Classes)

    let rec withoutMacros info =
        match info with
        | M.Macro (_, _, Some fb) -> withoutMacros fb
        | _ -> info 

    let rec packageClass (c: M.ClassInfo) name =

        match c.BaseClass with
        | Some b ->
            match classes.TryFind b with
            | Some bc ->
                classes.Remove b |> ignore
                packageClass bc b.Value.FullName
            | _ -> ()
        | _ -> ()

        match c.StaticConstructor with
        | Some(_, GlobalAccess a) when a.Value = [ "ignore" ] -> ()
        | Some (ccaddr, body) -> 
            packageCctor ccaddr body name
        | _ -> ()

        match c.Address with 
        | None -> ()
        | Some addr ->
            
            let prototype = 
                let prop info body =
                    match withoutMacros info with
                    | M.Instance mname ->
                        if body <> Undefined then
                            Some (mname, body)
                        else None
                    | _ -> None
                    
                Object [
                    for info, _, body in c.Methods.Values do
                        match prop info body with
                        | Some p -> yield p 
                        | _ -> ()
                    for info, body in c.Implementations.Values do
                        match prop info body with
                        | Some p -> yield p 
                        | _ -> ()
                ]
                            
            let baseType =
                let tryFindClass c =
                    match refMeta.Classes.TryFind c with
                    | Some _ as res -> res
                    | _ -> current.Classes.TryFind c
                match c.BaseClass |> Option.bind tryFindClass |> Option.bind (fun b -> b.Address) with
                | Some ba -> getOrImportAddress false ba
                | _ -> Value Null
             
            if c.HasWSPrototype then
                packageCtor addr <| JSRuntime.Class prototype baseType (GlobalAccess addr)

        for info, _, body in c.Methods.Values do
            match withoutMacros info with
            | M.Static maddr ->
                if body <> Undefined then
                    if body <> Undefined then
                        package maddr body
            | _ -> ()

        for info, _, body in c.Constructors.Values do
            match withoutMacros info with
            | M.Constructor caddr ->
                if body <> Undefined then
                    if c.HasWSPrototype && Option.isSome c.Address then
                        package caddr <| 
                            match c.Address with
                            | Some addr -> JSRuntime.Ctor body (GlobalAccess addr)
                            | _ -> body
                    else
                        package caddr body
            | M.Static maddr ->
                if body <> Undefined then
                    package maddr body
            | _ -> ()
            
    while classes.Count > 0 do
        let (KeyValue(t, c)) = classes |> Seq.head
        classes.Remove t |> ignore
        packageClass c t.Value.FullName

    scriptsBaseUrl |> Option.iter (fun url ->
        ItemSet(Global ["IntelliFactory"; "Runtime"], !~(String "ScriptBasePath"), !~(String url))
        |> ExprStatement
        |> statements.Add
    )

    match entryPointStyle, current.EntryPoint with
    | (OnLoadIfExists | ForceOnLoad), Some ep ->
        statements.Add <| ExprStatement (JSRuntime.OnLoad (Function([], ep)))
    | ForceImmediate, Some ep ->
        statements.Add ep
    | (ForceOnLoad | ForceImmediate), None ->
        failwith "Missing entry point or export. Add SPAEntryPoint attribute to a static method without arguments, or JavaScriptExport on types/methods to expose them."
    | OnLoadIfExists, None -> ()
    
    let trStatements = statements |> Seq.map globalAccessTransformer.TransformStatement |> List.ofSeq

    if List.isEmpty trStatements then Undefined else
        let allStatements = List.ofSeq (Seq.append declarations trStatements) 
        Application(Function([], Block allStatements), [], NonPure, Some 0)

let readMapFileSources mapFile =
    match Json.Parse mapFile with
    | Json.Object fields ->
        let getString j = match j with Json.String s -> s | _ -> failwith "string expected in map file"
        let sources = fields |> List.pick (function "sources", Json.Array s -> Some (s |> List.map getString) | _ -> None)  
        let sourcesContent = fields |> List.pick (function "sourcesContent", Json.Array s -> Some (s |> List.map getString) | _ -> None)  
        List.zip sources sourcesContent
    | _ -> failwith "map file JSON should be an object"

let exprToString pref (getWriter: unit -> WebSharper.Core.JavaScript.Writer.CodeWriter) statement =
    let env = WebSharper.Compiler.JavaScriptWriter.Environment.New(pref)
    let program =
        statement
        |> JavaScriptWriter.transformExpr env
        |> WebSharper.Core.JavaScript.Syntax.Ignore
        |> List.singleton

    let writer = getWriter()
    WebSharper.Core.JavaScript.Writer.WriteProgram pref writer program
    writer.GetCodeFile(), writer.GetMapFile()
