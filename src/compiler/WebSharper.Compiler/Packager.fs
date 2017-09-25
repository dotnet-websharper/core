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

    let glob = Var (Id.Global())
    addresses.Add(Address.Global(), glob)
    addresses.Add(Address.Lib "window", glob)

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
                        declarations.Add <| ImportAll (None, js + ".js")
                        Undefined
                    | TypeScriptModule ts ->
                        let var = Id.New (ts |> String.filter System.Char.IsUpper)
                        declarations.Add <| ImportAll (Some var, "./" + ts)
                        Var var
                    | CurrentModule -> failwith "empty local address"
                | [ name ] ->
                    match address.Module with
                    | CurrentModule 
                    | StandardLibrary ->
                        GlobalAccess address
                    | JavaScriptFile _ ->
                        getAddress { address with Address = Hashed [] } |> ignore
                        let var = Id.New name
                        declarations.Add <| Declare (VarDeclaration(var, Undefined)) 
                        Var var
                    | TypeScriptModule _ ->
                        let parent = getAddress { address with Address = Hashed [] }
                        ItemGet(parent, Value (String name), Pure)
                | name :: r ->
                    match address.Module with
                    | CurrentModule -> GlobalAccess address
                    | _ ->
                    let parent = getAddress { address with Address = Hashed r }
                    ItemGet(parent, Value (String name), Pure)
            addresses.Add(address, res)
            res

    let mutable currentNamespace = ResizeArray() 
    let mutable currentNamespaceContents = [ statements ]

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
            override this.TransformGlobalAccess a = getAddress a
        }

    let package (a: Address) expr =
        let n = toNamespaceWithName a
        let i = Id.New (n, str = true)
        let exp =
            match expr with
            | Function(args, body) -> FuncDeclaration(i, args, body)
            | _ -> VarDeclaration (i, expr)
        addStatement <| Export exp

    let packageByName (a: Address) f =
        let n = toNamespaceWithName a
        addStatement <| Export (f n)

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

        let members = ResizeArray()
        
        for f, _, _ in c.Fields.Values do
            match f with
            | M.InstanceField n
            | M.OptionalField n ->
                members.Add (ClassProperty (false, n)) 
            | M.StaticField a ->
                package a Undefined 
            | _ -> ()

        match c.StaticConstructor with
        | Some(_, GlobalAccess { Module = JavaScriptFile "Runtime"; Address = a }) when a.Value = [ "ignore" ] -> ()
        | Some(ccaddr, body) ->
            package ccaddr body
        | _ -> ()

        match c.Address with 
        | None -> ()
        | Some addr ->
            let mem (m: Method) info body =
                match withoutMacros info with
                | M.Instance mname ->
                    match IgnoreExprSourcePos body with
                    | Function (args, b) ->
                        members.Add (ClassMethod(false, mname, args, Some b))
                    | Undefined ->
                        let args = m.Value.Parameters |> List.map (fun _ -> Id.New(mut = false))
                        members.Add (ClassMethod(false, mname, args, None))
                    | _ -> failwith "unexpected form for class member"
                | _ -> ()
                    
            for KeyValue(m, (info, _, body)) in c.Methods do
                mem m info body 
            for KeyValue((_, m), (info, body)) in c.Implementations do
                mem m info body
                            
            let baseType =
                let tryFindClass c =
                    match refMeta.Classes.TryFind c with
                    | Some _ as res -> res
                    | _ -> current.Classes.TryFind c
                match c.BaseClass |> Option.bind tryFindClass |> Option.bind (fun b -> b.Address) with
                | Some ba -> Some (GlobalAccess ba)
                | _ -> None

            if c.HasWSPrototype then
                let indexedCtors = Dictionary()
                for info, _, body in c.Constructors.Values do
                    match withoutMacros info with
                    | M.New ->
                        if body <> Undefined then
                            match body with
                            | Function ([], IgnoreSourcePos.Empty) -> 
                                ()
                            | Function (args, b) ->                  
                                members.Add (ClassConstructor (args, Some b))
                            | _ ->
                                failwithf "Invalid form for translated constructor"
                    | M.NewIndexed i ->
                        if body <> Undefined then
                            match body with
                            | Function (args, b) ->  
                                let index = Id.New("i: " + string i, str = true)
                                members.Add (ClassConstructor (index :: args, None))
                                indexedCtors.Add (i, (args, b))
                            | _ ->
                                failwithf "Invalid form for translated constructor"
                    | _ -> ()

                if indexedCtors.Count > 0 then
                    let index = Id.New("i", mut = false)
                    let maxArgs = indexedCtors.Values |> Seq.map (fst >> List.length) |> Seq.max
                    let cArgs = List.init maxArgs (fun _ -> Id.New(mut = false, opt = true))
                    let cBody =
                        Switch(Var index, 
                            indexedCtors |> Seq.map (fun (KeyValue(i, (args, b))) ->
                                Some (Value (Int i)), 
                                CombineStatements [
                                    ReplaceIds(Seq.zip args cArgs |> dict).TransformStatement(b)
                                    Break None
                                ]
                            ) |> List.ofSeq
                        )
                    members.Add (ClassConstructor (index :: cArgs, Some cBody))   

                packageByName addr <| fun n -> Class(n, baseType, [], List.ofSeq members)

        let smem info body = 
            match withoutMacros info with
            | M.Static maddr ->
                if body <> Undefined then
                    package maddr body
            | _ -> ()
        
        for info, _, body in c.Constructors.Values do
            smem info body
        for info, _, body in c.Methods.Values do
            smem info body
            
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
