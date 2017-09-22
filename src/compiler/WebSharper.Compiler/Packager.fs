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

let packageAssembly (refMeta: M.Info) (current: M.Info) isBundle =
    let addresses = Dictionary()
    let declarations = ResizeArray()
    let statements = ResizeArray()

    let g = Id.New "Global"
    let glob = Var g                    
    declarations.Add <| VarDeclaration (g, Var (Id.Global()))
    addresses.Add(Address.Global(), glob)
    addresses.Add(Address.Lib "window", glob)
    let safeObject expr = Binary(expr, BinaryOperator.``||``, Object []) 

    let rec getAddress (address: Address) =
        match addresses.TryGetValue address with
        | true, v -> v
        | _ ->
            let res =
                match address.Address.Value with
                | [] ->
                    match address.Module with
                    | StandardLibrary -> failwith "impossible, already handled"
                    | JavaScriptFile js ->
                        let fileName = js + ".js"
                        declarations.Add <| ImportAll (None, fileName)
                        glob
                    | TypeScriptModule ts ->
                        let var = Id.New (ts |> String.filter System.Char.IsUpper)
                        declarations.Add <| ImportAll (Some var, ts)
                        Var var
                    | CurrentModule -> failwith "empty local address"
                | [ name ] ->
                    match address.Module with
                    | CurrentModule 
                    | StandardLibrary -> GlobalAccess address
                    | _ ->
                    let var = Id.New (if name.StartsWith "StartupCode$" then "SC$1" else name)
                    let f = Value (String name)
                    declarations.Add <| VarDeclaration (var, ItemSet(glob, f, ItemGet(glob, f, Pure) |> safeObject))                
                    Var var
                | name :: r ->
                    match address.Module with
                    | CurrentModule -> GlobalAccess address
                    | _ ->
                    let parent = getAddress { address with Address = Hashed r }
                    let f = Value (String name)
                    let var = Id.New name
                    declarations.Add <| VarDeclaration (var, ItemSet(parent, f, ItemGet(parent, f, Pure) |> safeObject))                
                    Var var
            addresses.Add(address, res)
            res

    let mutable currentNamespace = ResizeArray() 
    let mutable currentNamespaceContents = [ statements ]

    let rec transformAddress (address: Address) =
        match addresses.TryGetValue address with
        | true, v -> v
        | _ ->
            match address.Module with
            | CurrentModule
            | TypeScriptModule _ -> getAddress address
            | _ ->
            match address.Address.Value with
            | [] -> glob
            | h :: t ->
                let parent = getAddress { address with Address = Hashed t }
                ItemGet(parent, Value (String h), Pure)

    let commonLength s1 s2 =
        Seq.zip s1 s2 |> Seq.takeWhile (fun (a, b) -> a = b) |> Seq.length

    let closeNamespace() =
        match currentNamespaceContents with
        | contents :: (parentContents :: _ as pc) ->
            let l = currentNamespace.Count - 1
            let name = currentNamespace.[l]
            currentNamespace.RemoveAt(l)
            currentNamespaceContents <- pc
            parentContents.Add(Export (Namespace (name, List.ofSeq contents)))
        | _  -> ()

    let toNamespace a =
        let r = List.rev a 
        let common = commonLength currentNamespace r
        let close = currentNamespace.Count - common
        for i = 1 to close do
            closeNamespace()
        for name in r |> List.skip common do
            currentNamespace.Add(name)
            currentNamespaceContents <- ResizeArray() :: currentNamespaceContents
    
    let toNamespaceWithName (a: Address) =
        match a.Address.Value with
        | n :: ns ->
            toNamespace ns
            n
        | _ -> failwith "empty address"

    let addStatement s =
        match currentNamespaceContents with
        | contents :: _ -> contents.Add(s)
        | _ -> failwith "impossible"

    let globalAccessTransformer =
        { new Transformer() with
            override this.TransformGlobalAccess a = transformAddress a
        }

    let package (a: Address) expr =
        let n = toNamespaceWithName a
        let i = Id.New n
        let exp =
            match expr with
            | Function(args, body) -> FuncDeclaration(i, args, body)
            | _ -> VarDeclaration (i, expr)
        addStatement <| Export exp

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

        for f, _, _ in c.Fields.Values do
            match f with
            | M.StaticField n ->
                package n Undefined  
            | _ -> ()

        match c.StaticConstructor with
        | Some(_, GlobalAccess { Module = JavaScriptFile "Runtime"; Address = a }) when a.Value = [ "ignore" ] -> ()
        | Some(ccaddr, body) -> 
            package ccaddr body
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
                | Some ba -> GlobalAccess ba
                | _ -> Value Null
             
            if c.HasWSPrototype then
                package addr <| JSRuntime.Class prototype baseType (GlobalAccess addr)

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

    toNamespace []

    if isBundle then
        match current.EntryPoint with
        | Some ep -> addStatement <| ExprStatement (JSRuntime.OnLoad (Function([], ep)))
        | _ -> failwith "Missing entry point. Add an SPAEntryPoint attribute to a static method without arguments."
    
    let trStatements = statements |> Seq.map globalAccessTransformer.TransformStatement |> List.ofSeq

    if List.isEmpty trStatements then 
        [] 
    else
        List.ofSeq declarations @ trStatements 

let readMapFileSources mapFile =
    match Json.Parse mapFile with
    | Json.Object fields ->
        let getString j = match j with Json.String s -> s | _ -> failwith "string expected in map file"
        let sources = fields |> List.pick (function "sources", Json.Array s -> Some (s |> List.map getString) | _ -> None)  
        let sourcesContent = fields |> List.pick (function "sourcesContent", Json.Array s -> Some (s |> List.map getString) | _ -> None)  
        List.zip sources sourcesContent
    | _ -> failwith "map file JSON should be an object"

let programToString pref (getWriter: unit -> WebSharper.Core.JavaScript.Writer.CodeWriter) statements =
    let program = statements |> JavaScriptWriter.transformProgram pref
    let writer = getWriter()
    WebSharper.Core.JavaScript.Writer.WriteProgram pref writer program
    writer.GetCodeFile(), writer.GetMapFile()
