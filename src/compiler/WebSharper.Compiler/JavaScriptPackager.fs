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

//let private Address a = { Module = CurrentModule; Address = Hashed a }

let packageType (refMeta: M.Info) (current: M.Info) (typ: TypeDefinition) entryPoint entryPointStyle =
    let imports = Dictionary<string, Dictionary<string, Id>>()
    let declarations = ResizeArray<Statement>()
    let addresses = Dictionary<Address, Expression>()
    let statements = ResizeArray<Statement>()

    let g = Id.New "Global"
    let glob = Var g
    addresses.Add(Address.Global(), glob)
    addresses.Add(Address.Lib "self", glob)
    addresses.Add(Address.Lib "import", Var (Id.Import()))
    let safeObject expr = Binary(expr, BinaryOperator.``||``, Object []) 
    
    //let rec getAddress (address: Address) =
    //    match addresses.TryGetValue address with
    //    | true, v -> v
    //    | _ ->
    //        match address.Address.Value with
    //        | [] -> glob
    //        | [ name ] ->
    //            let var = Id.New (if name.StartsWith "StartupCode$" then "SC$1" else name)
    //            let f = Value (String name)
    //            declarations.Add <| VarDeclaration (var, ItemSet(glob, f, ItemGet(glob, f, Pure) |> safeObject))                
    //            let res = Var var
    //            addresses.Add(address, res)
    //            res
    //        | name :: r ->
    //            let parent = getAddress (Address r)
    //            let f = Value (String name)
    //            let var = Id.New name
    //            declarations.Add <| VarDeclaration (var, ItemSet(parent, f, ItemGet(parent, f, Pure) |> safeObject))                
    //            let res = Var var
    //            addresses.Add(address, res)
    //            res

    //let getFieldAddress (address: Address) =
    //    match address.Address.Value with
    //    | name :: r ->
    //        getAddress (Address r), Value (String name)
    //    | _ -> failwith "packageAssembly: empty address"
    
    let rec getOrImportAddress (address: Address) =
        match addresses.TryGetValue address with
        | true, v -> v
        | _ ->
            //let getModuleName (from: string) =
            //    let fn = from.Split('/') |> Array.last
            //    if fn.EndsWith(".js") then
            //        fn.[.. fn.Length - 4].Replace(".", "$")
            //    else fn.Replace(".", "$")
            match address.Module with
            | StandardLibrary
            | JavaScriptFile _ ->
                GlobalAccess address    
            | JavaScriptModule m ->
                let moduleImports =
                    match imports.TryGetValue m with
                    | true, mi -> mi
                    | _ ->
                        let mi = Dictionary()
                        imports.Add(m, mi)
                        mi
                let importWhat, importAs =
                    match address.Address.Value with
                    | [] -> 
                        m.Split([| '/'; '.' |]) |> Array.last,
                        "*"
                    | a -> 
                        let n = List.last a
                        n, n
                let i =
                    match moduleImports.TryGetValue importWhat with
                    | true, i -> i
                    | _ ->
                        let i = Id.New(importAs)
                        moduleImports.Add(importWhat, i)
                        i
                let importedAddress =
                    match address.Address.Value with
                    | [] -> { address with Module = ImportedModule i }
                    | a -> { Module = ImportedModule i; Address = Hashed (a |> List.rev |> List.tail |> List.rev) }
                GlobalAccess importedAddress
            | _ -> GlobalAccess address          
                        

            
            //match address.Address.Value with
            //| [] -> glob
            //| [ from; "import" ] ->
            //    let name = "def$" + getModuleName from
            //    let id = Id.New (name, mut = false)
            //    let res = Var id
            //    imports.Add (from, None, id)
            //    addresses.Add(address, res)
            //    res
            //| [ export; from; "import" ] -> 
            //    let name = 
            //        match export with
            //        | "*" -> getModuleName from
            //        | n -> n
            //    let id = Id.New (name, mut = false) 
            //    let res = Var id
            //    imports.Add (from, Some export, id)
            //    addresses.Add(address, res)
            //    res
            //| h :: t ->
            //    let parent = getOrImportAddress false (Address t)
            //    let import = ItemGet(parent, Value (String h), Pure)
            //    if full then
            //        import
            //    else
            //        let var = Id.New (if h = "jQuery" && List.isEmpty t then "$" else h)
            //        let importWithCheck =
            //            if List.isEmpty t then import else parent ^&& import
            //        declarations.Add <| VarDeclaration (var, importWithCheck)                
            //        let res = Var var
            //        addresses.Add(address, res)
            //        res

    let bodyTransformer =
        { new Transformer() with
            override this.TransformGlobalAccess a = getOrImportAddress a
        }
            
    //let package a expr =
    //    let o, x = getFieldAddress a
    //    statements.Add <| ExprStatement (ItemSet (o, x, expr))    

    let rec withoutMacros info =
        match info with
        | M.Macro (_, _, Some fb) -> withoutMacros fb
        | _ -> info 

    let packageClass (c: M.ClassInfo) (addr: Address) (ct: M.CustomTypeInfo) name =

        let members = ResizeArray<Statement>()
                    
        let mem info body =
            match withoutMacros info with
            | M.Instance (mname, mkind) ->
                match IgnoreExprSourcePos body with
                | Function (args, _, b) ->
                    let info = 
                        {
                            IsStatic = false
                            IsPrivate = false // TODO
                            Kind = mkind
                        }
                    members.Add <| ClassMethod(info, mname, args, Some (bodyTransformer.TransformStatement b), TSType.Any)
                | _ -> ()       
            | M.Static (mname, mkind) ->
                match IgnoreExprSourcePos body with
                | Function (args, _, b) ->
                    let info = 
                        {
                            IsStatic = true
                            IsPrivate = false // TODO
                            Kind = mkind
                        }
                    members.Add <| ClassMethod(info, mname, args, Some b, TSType.Any)
                | _ -> ()   
            | M.Func name ->
                match IgnoreExprSourcePos body with
                | Function (args, _, b) ->
                    statements.Add <| Export (FuncDeclaration(Id.New(name, str = true), args, bodyTransformer.TransformStatement b, []))
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
                        members.Add (ClassConstructor (args, Some (bodyTransformer.TransformStatement b), TSType.Any))
                    | _ ->
                        failwithf "Invalid form for translated constructor"
            | M.NewIndexed i ->
                if body <> Undefined then
                    match body with
                    | Function (args, _, b) ->  
                        indexedCtors.Add (i, (args, bodyTransformer.TransformStatement b))
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
            | Some (ba, _, _) -> Some (getOrImportAddress ba)
            | _ -> None
        
        //match ct with
        //| M.FSharpUnionInfo u when Option.isNone c.Type ->
        //    let numArgs =
        //        u.Cases |> Seq.map (fun uc -> 
        //            match uc.Kind with
        //            | M.NormalFSharpUnionCase fields -> List.length fields
        //            | M.SingletonFSharpUnionCase -> 0
        //            | M.ConstantFSharpUnionCase _ -> 0
        //        )
        //        |> Seq.max
        //    let genCtor =
        //        let argNames = "$" :: List.init numArgs (fun i -> "$" + string i)
        //        let args = argNames |> List.map (fun n -> Id.New(n), Modifiers.None)
        //        let setters = 
        //            Statement.Block (
        //                args |> List.map (fun (a, _) -> ExprStatement (ItemSet(This, Value (Literal.String a.Name.Value), Var a)))  
        //            )
        //        ClassConstructor(args, Some setters, TSType.Any)
        //    packageCtor (addr.Sub("$")) <| ClassExpr(None, baseType, genCtor :: List.ofSeq members)
        //| _ ->
        //    if c.HasWSPrototype then
        //        packageCtor addr <| ClassExpr(None, baseType, List.ofSeq members) 

        match c.StaticConstructor with
        | Some st -> 
            members.Add <| ClassStatic(bodyTransformer.TransformStatement st)
        | _ -> ()

        let className = addr.Address.Value |> List.head

        if c.HasWSPrototype then
            statements.Add <| Export (Class (className, baseType, [], List.ofSeq members, []))

            
    match current.Classes.TryFind(typ) with
    | Some (a, ct, cOpt) ->
        cOpt |> Option.iter (fun c -> packageClass c a ct typ.Value.FullName)
    | None -> ()

    match current.Interfaces.TryFind(typ) with
    | Some i ->

        let methodNames =
            i.Methods.Values |> Seq.map (fun (i, _, _) -> i)

        let isFunctionName =
            match i.Address.Address.Value with
            | fn :: a -> "is" + fn
            | _ ->
                failwithf "Missing address for interface %s" typ.Value.FullName
        let funcId = Id.New(isFunctionName, str = true) 

        let isIntf =
            let x = Id.New "x"
            if Seq.isEmpty methodNames then
                FuncDeclaration(funcId, [x], Return (Value (Bool true)), [])
            else         
                let shortestName = methodNames |> Seq.minBy String.length
                let check = Binary(Value (String shortestName), BinaryOperator.``in``, Var x)
                FuncDeclaration(funcId, [x], Return check, [])

        statements.Add(Export isIntf)

    | None -> ()

    match entryPointStyle, entryPoint with
    | (OnLoadIfExists | ForceOnLoad), Some ep ->
        statements.Add <| ExprStatement (JSRuntime.OnLoad (Function([], None, ep)))
    | ForceImmediate, Some ep ->
        statements.Add ep
    | (ForceOnLoad | ForceImmediate), None ->
        failwith "Missing entry point or export. Add SPAEntryPoint attribute to a static method without arguments, or JavaScriptExport on types/methods to expose them."
    | OnLoadIfExists, None -> ()
    
    //let trStatements = statements |> Seq.map globalAccessTransformer.TransformStatement |> List.ofSeq
    
    if statements.Count = 0 then 
        [] 
    else
        List.ofSeq (Seq.concat [ declarations; statements ])

let packageAssembly (refMeta: M.Info) (current: M.Info) entryPoint entryPointStyle =
    Seq.append current.Classes.Keys current.Interfaces.Keys
    |> Seq.map (fun typ ->
        typ.Value.FullName, 
        packageType refMeta current typ entryPoint entryPointStyle
    )
    |> Array.ofSeq

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
