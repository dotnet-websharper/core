// $begin{copyright}
//
// This file is part of WebSharper
//
// Copyright (c) 2008-2018 IntelliFactory
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
module WebSharper.Compiler.JavaScriptPackager

open System.Collections.Generic

open WebSharper.Core
open WebSharper.Core.AST
module M = WebSharper.Core.Metadata
module I = IgnoreSourcePos

type EntryPointStyle =
    | OnLoadIfExists
    | ForceOnLoad
    | ForceImmediate

let private Address a = { Module = CurrentModule; Address = Hashed a }

let packageAssembly (refMeta: M.Info) (current: M.Info) entryPoint entryPointStyle =
    let imports = ResizeArray()
    let addresses = Dictionary()
    let declarations = ResizeArray()
    let statements = ResizeArray()

    let g = Id.New "Global"
    let glob = Var g
    addresses.Add(Address [], glob)
    addresses.Add(Address [ "self" ], glob)
    addresses.Add(Address [ "import" ], Var (Id.Import()))
    let safeObject expr = Binary(expr, BinaryOperator.``||``, Object []) 
    
    let rec getAddress (address: Address) =
        match addresses.TryGetValue address with
        | true, v -> v
        | _ ->
            match address.Address.Value with
            | [] -> glob
            | [ name ] ->
                let var = Id.New (if name.StartsWith "StartupCode$" then "SC$1" else name)
                let f = Value (String name)
                declarations.Add <| VarDeclaration (var, ItemSet(glob, f, ItemGet(glob, f, Pure) |> safeObject))                
                let res = Var var
                addresses.Add(address, res)
                res
            | name :: r ->
                let parent = getAddress (Address r)
                let f = Value (String name)
                let var = Id.New name
                declarations.Add <| VarDeclaration (var, ItemSet(parent, f, ItemGet(parent, f, Pure) |> safeObject))                
                let res = Var var
                addresses.Add(address, res)
                res

    let getFieldAddress (address: Address) =
        match address.Address.Value with
        | name :: r ->
            getAddress (Address r), Value (String name)
        | _ -> failwith "packageAssembly: empty address"
    
    let rec getOrImportAddress (full: bool) (address: Address) =
        match addresses.TryGetValue address with
        | true, v -> v
        | _ ->
            let getModuleName (from: string) =
                let fn = from.Split('/') |> Array.last
                if fn.EndsWith(".js") then
                    fn.[.. fn.Length - 4].Replace(".", "$")
                else fn.Replace(".", "$")
            match address.Address.Value with
            | [] -> glob
            | [ from; "import" ] ->
                let name = "def$" + getModuleName from
                let id = Id.New (name, mut = false)
                let res = Var id
                imports.Add (from, None, id)
                addresses.Add(address, res)
                res
            | [ export; from; "import" ] -> 
                let name = 
                    match export with
                    | "*" -> getModuleName from
                    | n -> n
                let id = Id.New (name, mut = false) 
                let res = Var id
                imports.Add (from, Some export, id)
                addresses.Add(address, res)
                res
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
        //let oaExpr =
        //    ApplAny(Global ["Object"; "assign"], [])
        statements.Add <| ExprStatement (VarSet (av, ItemSet (o, x, expr)))    

    let packageCctor a expr name =
        let o, x = getFieldAddress a
        match expr with
        | Function ([], _, body) ->
            let rem = ExprStatement (ItemSet (o, x, ItemGet(glob, Value (String "ignore"), Pure)))    
            let expr = Function([], None, Block [rem; body])
            statements.Add <| ExprStatement (ItemSet (o, x, expr))    
        | _ ->
            let expr = Function([], None, ExprStatement expr)
            statements.Add <| ExprStatement (ItemSet (o, x, expr))            
            // TODO investigate
            //failwithf "Static constructor must be a function for type %s: %A" name (Debug.PrintExpression expr)

    let classes = Dictionary(current.Classes)

    let rec withoutMacros info =
        match info with
        | M.Macro (_, _, Some fb) -> withoutMacros fb
        | _ -> info 

    let rec packageClass (c: M.ClassInfo) addr (ct: M.CustomTypeInfo) name =

        match c.BaseClass with
        | Some { Entity = b } ->
            match classes.TryFind b with
            | Some (addr, ct, Some bc) ->
                classes.Remove b |> ignore
                packageClass bc addr ct b.Value.FullName
            | _ -> ()
        | _ -> ()

        let members = ResizeArray<Statement>()
                    
        let mem info body =
            match withoutMacros info with
            | M.Instance mname ->
                match IgnoreExprSourcePos body with
                | Function (args, _, b) ->
                    members.Add <| ClassMethod(false, mname, args, Some b, TSType.Any)
                | _ -> ()                    
            | _ -> ()

        for info, _, _, body in c.Methods.Values do
            mem info body
        
        for info, body in c.Implementations.Values do
            mem info body

        let indexedCtors = Dictionary()

        for KeyValue(ctor, (info, opts, body)) in c.Constructors do
            match withoutMacros info with
            | M.New ->
                if body <> Undefined then
                    match body with
                    | Function ([], _, I.Empty) 
                    | Function ([], _, I.ExprStatement(I.Application(I.Base, [], _))) -> 
                        ()
                    | Function (args, _, b) ->                  
                        let args = List.map (fun x -> x, Modifiers.None) args
                        members.Add (ClassConstructor (args, Some b, TSType.Any))
                    | _ ->
                        failwithf "Invalid form for translated constructor"
            | M.NewIndexed i ->
                if body <> Undefined then
                    match body with
                    | Function (args, _, b) ->  
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
            let allArgs = List.map (fun x -> x, Modifiers.None) (index :: cArgs)
            members.Add (ClassConstructor (allArgs, Some cBody, TSType.Any))   

        let baseType =
            let tryFindClass c =
                match refMeta.Classes.TryFind c with
                | Some _ as res -> res
                | _ -> current.Classes.TryFind c
            match c.BaseClass |> Option.bind (fun b -> tryFindClass b.Entity) with
            | Some (ba, _, _) -> Some (getOrImportAddress false ba)
            | _ -> None
        
        match ct with
        | M.FSharpUnionInfo u when Option.isNone c.Type ->
            let numArgs =
                u.Cases |> Seq.map (fun uc -> 
                    match uc.Kind with
                    | M.NormalFSharpUnionCase fields -> List.length fields
                    | M.SingletonFSharpUnionCase -> 0
                    | M.ConstantFSharpUnionCase _ -> 0
                )
                |> Seq.max
            let genCtor =
                let argNames = "$" :: List.init numArgs (fun i -> "$" + string i)
                let args = argNames |> List.map (fun n -> Id.New(n), Modifiers.None)
                let setters = 
                    Statement.Block (
                        args |> List.map (fun (a, _) -> ExprStatement (ItemSet(This, Value (Literal.String a.Name.Value), Var a)))  
                    )
                ClassConstructor(args, Some setters, TSType.Any)
            packageCtor (addr.Sub("$")) <| ClassExpr(None, baseType, genCtor :: List.ofSeq members)
        | _ ->
            if c.HasWSPrototype then
                packageCtor addr <| ClassExpr(None, baseType, List.ofSeq members) 

        match c.StaticConstructor with
        | Some(_, GlobalAccess a) when a.Address.Value = [ "ignore" ] -> ()
        | Some (ccaddr, body) -> 
            packageCctor ccaddr body name
        | _ -> ()

        for info, _, _, body in c.Methods.Values do
            match withoutMacros info with
            | M.Static maddr
            | M.AsStatic maddr ->
                if body <> Undefined then
                    if body <> Undefined then
                        package maddr body
            | _ -> ()

        for info, _, body in c.Constructors.Values do
            match withoutMacros info with
            | M.Static maddr 
            | M.AsStatic maddr ->
                if body <> Undefined then
                    package maddr body
            | _ -> ()
            
    while classes.Count > 0 do
        let (KeyValue(t, (a, ct, cOpt))) = classes |> Seq.head
        classes.Remove t |> ignore
        cOpt |> Option.iter (fun c -> packageClass c a ct t.Value.FullName)

    for KeyValue(td, i) in current.Interfaces do       

        let methodNames =
            i.Methods.Values |> Seq.map fst

        let addr =
            match i.Address.Address.Value with
            | fn :: a ->
                { i.Address with Address = PlainAddress (("is" + fn) :: a) }  
            | _ ->
                failwithf "Missing address for interface %s" td.Value.FullName
        
        let isIntf =
            let x = Id.New "x"
            if Seq.isEmpty methodNames then
                Function([x], None, Return (Value (Bool true)))
            else         
                let shortestName = methodNames |> Seq.minBy String.length
                let check = Binary(Value (String shortestName), BinaryOperator.``in``, Var x)
                Function([x], None, Return check)

        package addr isIntf

    match entryPointStyle, entryPoint with
    | (OnLoadIfExists | ForceOnLoad), Some ep ->
        statements.Add <| ExprStatement (JSRuntime.OnLoad (Function([], None, ep)))
    | ForceImmediate, Some ep ->
        statements.Add ep
    | (ForceOnLoad | ForceImmediate), None ->
        failwith "Missing entry point or export. Add SPAEntryPoint attribute to a static method without arguments, or JavaScriptExport on types/methods to expose them."
    | OnLoadIfExists, None -> ()
    
    let trStatements = statements |> Seq.map globalAccessTransformer.TransformStatement |> List.ofSeq

    if List.isEmpty trStatements then Undefined else
        let allStatements = List.ofSeq (Seq.append declarations trStatements) 
        let wsPkg = 
            Appl(Function([g], None, Block allStatements), [Var (Id.Global())], NonPure, Some 0)
        if imports.Count = 0 then
            wsPkg
        else
            Sequential (
                Seq.append (
                    imports |> Seq.map (fun (from, export, id) ->
                        let args =
                            match export with
                            | None -> [ Value (String from) ]
                            | Some e -> [  Value (String e); Value (String from) ]
                        StatementExpr(VarDeclaration(id, Appl(Var (Id.Import()), args, NonPure, Some 0)), None)
                    )
                ) (Seq.singleton wsPkg)
                |> List.ofSeq
            )

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
        match statement with
        | Sequential statements ->
            statements |> List.map (
                function
                | StatementExpr (st, None) ->
                    st 
                    |> JavaScriptWriter.transformStatement env
                | e ->
                    e
                    |> JavaScriptWriter.transformExpr env
                    |> WebSharper.Core.JavaScript.Syntax.Ignore
            )
        | _ ->
            statement
            |> JavaScriptWriter.transformExpr env
            |> WebSharper.Core.JavaScript.Syntax.Ignore
            |> List.singleton

    let writer = getWriter()
    WebSharper.Core.JavaScript.Writer.WriteProgram pref writer program
    writer.GetCodeFile(), writer.GetMapFile()
