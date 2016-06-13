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

let packageAssembly (merged: M.Info) (current: M.Info) isBundle =
    let addresses = Dictionary()
    let declarations = ResizeArray()
    let statements = ResizeArray()

    let glob = Id.New("Global", false)
    declarations.Add <| VarDeclaration (glob, This)
    let glob = Var glob

    let safeObject expr = Binary(expr, BinaryOperator.``||``, Object []) 
    
    let rec getAddress (address: Address) =
        match addresses.TryGetValue address with
        | true, v -> v
        | _ ->
            match address.Value with
            | [] -> glob
            | [ name ] ->
                let var = Id.New (if name.StartsWith "StartupCode$" then "SC$" else name)
                let f = Value (String name)
                declarations.Add <| VarDeclaration (var, ItemSet(glob, f, ItemGet(glob, f) |> safeObject))                
                let res = Var var
                //topLevel.Add(name, res)
                addresses.Add(address, res)
                res
            | name :: r ->
                let parent = getAddress (Hashed r)
                let f = Value (String name)
                let var = Id.New name
                declarations.Add <| VarDeclaration (var, ItemSet(parent, f, ItemGet(parent, f) |> safeObject))                
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
                let import = ItemGet(parent, Value (String h))
                if full then
                    import
                else
                    let var = Id.New h
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

    let packageGlobal a expr =
        let o, x = getFieldAddress a
        statements.Add <| ExprStatement (ItemSet (o, x, expr))    

    let classes = Dictionary(current.Classes)

    let rec withoutMacros info =
        match info with
        | M.Macro (_, _, Some fb) -> withoutMacros fb
        | _ -> info 

    let rec packageClass (c: M.ClassInfo) =

        match c.BaseClass with
        | Some b ->
            match classes.TryFind b with
            | Some bc ->
                classes.Remove b |> ignore
                packageClass bc
            | _ -> ()
        | _ -> ()

        match c.StaticConstructor with
        | Some (ccaddr, body) -> packageGlobal ccaddr <| JSRuntime.Cctor body
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
                match c.BaseClass |> Option.bind merged.Classes.TryFind |> Option.bind (fun b -> b.Address) with
                | Some ba -> GlobalAccess ba
                | _ -> Value Null
             
            if not c.IsStatic then
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
                    if Option.isSome c.Address then
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
        packageClass c

    if isBundle then
        match current.EntryPoint with
        | Some ep -> statements.Add ep
        | _ -> failwith "Missing entry point. Add an SPAEntryPoint attribute to a static method without arguments."
    
    let trStatements = statements |> Seq.map globalAccessTransformer.TransformStatement |> List.ofSeq

    let allStatements = List.ofSeq (Seq.append declarations trStatements) 

    if List.isEmpty allStatements then Undefined else
        Application(Function([], Block allStatements), [], false, Some 0)

let exprToString asmName pref sourceMap statement =
    let program =
        statement
        |> JavaScriptWriter.transformExpr (WebSharper.Compiler.JavaScriptWriter.Environment.New(asmName, pref))
        |> WebSharper.Core.JavaScript.Syntax.Ignore
        |> WebSharper.Core.JavaScript.Syntax.Action
        |> fun x -> [ x ]
    let w = WebSharper.Core.JavaScript.Writer.CodeWriter(?assemblyName = if sourceMap then Some asmName else None)
    WebSharper.Core.JavaScript.Writer.WriteProgram pref w program
    w.GetCodeFile(), w.GetMapFile()
