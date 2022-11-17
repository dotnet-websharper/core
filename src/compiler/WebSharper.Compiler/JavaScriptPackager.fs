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

let packageType (refMeta: M.Info) (current: M.Info) asmName (typ: TypeDefinition) entryPoint entryPointStyle =
    let imports = Dictionary<string, Dictionary<string, Id>>()
    let jsUsed = HashSet<string>()
    let declarations = ResizeArray<Statement>()
    let addresses = Dictionary<Address, Expression>()
    let statements = ResizeArray<Statement>()

    let className = (typ.Value.FullName.Split([|'.';'+'|]) |> Array.last).Split('`') |> Array.head
    let classId = 
        Id.New className
    let currentModuleName = asmName + "/" + typ.Value.FullName

    //let g = Id.New "Global"
    //let glob = Var g
    //addresses.Add(Address.Global(), glob)
    //addresses.Add(Address.Lib "self", glob)
    addresses.Add(Address.Lib "import", Var (Id.Import()))
    let currentClassAdds = Address.DefaultExport currentModuleName
    addresses.Add(currentClassAdds, Var classId)
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
                match address.Address.Value with
                | [] -> ()
                | l -> 
                    jsUsed.Add(List.last l) |> ignore
                GlobalAccess address    
            | JavaScriptModule m ->
                let importWhat, importAs =
                    let fromModuleName() = (m.Split([| '/'; '.' |]) |> Array.last).Split('`') |> Array.head
                    match address.Address.Value with
                    | [] -> 
                        "*", fromModuleName()
                    | a -> 
                        let n = List.last a
                        if n = "default" then
                            n, fromModuleName()
                        else
                            n, n
                if m = currentModuleName then
                    let currentAddress =
                        { address with Module = ImportedModule (Id.Global()) }
                    GlobalAccess currentAddress
                else
                    let moduleImports =
                        match imports.TryGetValue m with
                        | true, mi -> mi
                        | _ ->
                            let mi = Dictionary()
                            imports.Add(m, mi)
                            mi
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
            
    let staticThisTransformer =
        { new Transformer() with
            override this.TransformGlobalAccess a = 
                if a = currentClassAdds then
                    This
                else
                    GlobalAccess a
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
                | Function (args, _, _, b) ->
                    let info = 
                        {
                            IsStatic = false
                            IsPrivate = false // TODO
                            Kind = mkind
                        }
                    members.Add <| ClassMethod(info, mname, args, Some b, TSType.Any)
                | _ -> ()       
            | M.Static (mname, mkind) ->
                match IgnoreExprSourcePos body with
                | Function (args, _, _, b) ->
                    let info = 
                        {
                            IsStatic = true
                            IsPrivate = false // TODO
                            Kind = mkind
                        }
                    members.Add <| ClassMethod(info, mname, args, Some (staticThisTransformer.TransformStatement b), TSType.Any)
                | _ -> ()   
            | M.Func fname ->
                match IgnoreExprSourcePos body with
                | Function (args, _, _, b) ->
                    statements.Add <| ExportDecl (false, FuncDeclaration(Id.New(fname, str = true), args, bodyTransformer.TransformStatement b, []))
                | e ->
                    statements.Add <| ExportDecl (false, VarDeclaration(Id.New(fname, mut = false, str = true), bodyTransformer.TransformExpression e))
            | M.GlobalFunc addr ->
                let fname = addr.Address.Value.Head
                match IgnoreExprSourcePos body with
                | Function (args, _, _, b) ->
                    statements.Add <| ExportDecl (false, FuncDeclaration(Id.New(fname, str = true), args, bodyTransformer.TransformStatement b, []))
                | e ->
                    statements.Add <| ExportDecl (false, VarDeclaration(Id.New(fname, mut = false, str = true), bodyTransformer.TransformExpression e))
            | _ -> ()

        if c.HasWSPrototype then
            for f in c.Fields.Values do
                let info isStatic isPrivate =
                    {
                        IsStatic = isStatic
                        IsPrivate = isPrivate
                        IsOptional = false
                    }

                match f with
                | M.InstanceField name, _, _ 
                | M.OptionalField name, _, _ -> 
                    members.Add <| ClassProperty(info false false, name, TSType.Any)
                | M.StaticField name, _, _ ->
                    members.Add <| ClassProperty(info true false, name, TSType.Any)
                | M.IndexedField _, _, _ ->
                    ()
                | M.VarField v, _, _ ->
                    statements.Add <| VarDeclaration(v, Undefined)

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
                    | Function ([], _, _, I.Empty) 
                    | Function ([], _, _, I.ExprStatement(I.Application(I.Base, [], _))) -> 
                        ()
                    | Function (args, _, _, b) ->                  
                        let args = List.map (fun x -> x, Modifiers.None) args
                        members.Add (ClassConstructor (args, Some b, TSType.Any))
                    | _ ->
                        failwithf "Invalid form for translated constructor"
            | M.NewIndexed i ->
                if body <> Undefined then
                    match body with
                    | Function (args, _, _, b) ->  
                        indexedCtors.Add (i, (args, b))
                    | _ ->
                        failwithf "Invalid form for translated constructor"
            | M.Func name ->
                match body with 
                | Function (args, _, _, b) ->  
                    statements.Add <| ExportDecl(false, FuncDeclaration(Id.New(name, str = true), args, bodyTransformer.TransformStatement b, []))
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

        let baseType, isObjBase =
            let tryFindClass c =
                match refMeta.Classes.TryFind c with
                | Some _ as res -> res
                | _ -> current.Classes.TryFind c
            match c.BaseClass |> Option.bind (fun b -> tryFindClass b.Entity) with
            | Some (ba, _, _) -> Some (getOrImportAddress ba), c.BaseClass.Value.Entity = Definitions.Object
            | _ -> None, false
        
        match ct with
        | M.FSharpUnionInfo u when Option.isNone c.Type ->         
            let tags = u.Cases |> List.mapi (fun i c -> c.Name, Value (Int i)) |> Object
            statements.Add <| ExportDecl(false, VarDeclaration(Id.New("Tags", mut = false, str = true), tags))

            let numArgs =
                u.Cases |> Seq.map (fun uc -> 
                    match uc.Kind with
                    | M.NormalFSharpUnionCase fields -> List.length fields
                    | M.SingletonFSharpUnionCase -> 0
                    | M.ConstantFSharpUnionCase _ -> 0
                )
                |> Seq.max
            let genCtor =
                let arg = Id.New("$")
                let assign = ExprStatement (JSRuntime.ObjectAssign This (Var arg))
                ClassConstructor([ arg, Modifiers.None ], Some assign, TSType.Any)
                //let argNames = "$" :: List.init numArgs (fun i -> "$" + string i)
                //let args = argNames |> List.map (fun n -> Id.New(n), Modifiers.None)
                //let setters = 
                //    Statement.Block (
                //        args |> List.map (fun (a, _) -> ExprStatement (ItemSet(This, Value (Literal.String a.Name.Value), Var a)))  
                //    )
                //ClassConstructor(args, Some setters, TSType.Any)
            members.Add <| genCtor
        | _ -> ()
        //| _ ->
        //    if c.HasWSPrototype then
        //        packageCtor addr <| ClassExpr(None, baseType, List.ofSeq members) 

        match c.StaticConstructor with
        | Some st -> 
            members.Add <| ClassStatic(staticThisTransformer.TransformStatement st)
        | _ -> ()

        let packageLazyClass classExpr =
            statements.Add <| VarDeclaration(classId, bodyTransformer.TransformExpression (JSRuntime.Lazy classExpr classId))
            statements.Add <| ExportDecl(true, ExprStatement(Var classId))                

        let packageClass classExpr = 
            statements.Add <| ExportDecl(true, ExprStatement(classExpr))                

        if c.HasWSPrototype || members.Count > 0 then
            let classExpr = ClassExpr(Some className, baseType, List.ofSeq members)
            match baseType with
            | Some b ->
                let needsLazy = Option.isNone c.Type
                if needsLazy then
                    packageLazyClass <| 
                        if isObjBase then
                            classExpr
                        else
                            Sequential [
                                JSRuntime.Force(b)
                                classExpr
                            ]
                else
                    packageClass classExpr
            | None ->
                let needsLazy = c.HasWSPrototype && Option.isNone c.Type && typ <> Definitions.Object
                if needsLazy then
                    packageLazyClass classExpr
                else
                    packageClass classExpr
                            
    match current.Classes.TryFind(typ) with
    | Some (a, ct, cOpt) ->
        cOpt |> Option.iter (fun c -> packageClass c a ct typ.Value.FullName)
    | None -> ()

    match current.Interfaces.TryFind(typ) with
    | Some i ->

        let methodNames =
            i.Methods.Values |> Seq.map (fun (i, _, _) -> i)

        let isFunctionName =
            "is" + (typ.Value.FullName.Split([|'.'; '+'|]) |> Array.last).Replace('`', '$')
        let funcId = Id.New(isFunctionName, str = true) 

        let isIntf =
            let x = Id.New "x"
            if Seq.isEmpty methodNames then
                FuncDeclaration(funcId, [x], Return (Value (Bool true)), [])
            else         
                let shortestName = methodNames |> Seq.minBy String.length
                let check = Binary(Value (String shortestName), BinaryOperator.``in``, Var x)
                FuncDeclaration(funcId, [x], Return check, [])

        statements.Add(ExportDecl (false, isIntf))

    | None -> ()

    match entryPointStyle, entryPoint with
    | (OnLoadIfExists | ForceOnLoad), Some ep ->
        statements.Add <| ExprStatement (JSRuntime.OnLoad (Function([], true, None, bodyTransformer.TransformStatement ep)))
    | ForceImmediate, Some ep ->
        statements.Add ep
    | (ForceOnLoad | ForceImmediate), None ->
        failwith "Missing entry point or export. Add SPAEntryPoint attribute to a static method without arguments, or JavaScriptExport on types/methods to expose them."
    | OnLoadIfExists, None -> ()
        
    if statements.Count = 0 then 
        [] 
    else
        if jsUsed.Count > 0 then
            let namedImports = 
                jsUsed |> Seq.map (fun n -> n, Id.New(n, str = true)) |> List.ofSeq
            declarations.Add(Import(None, None, namedImports, ""))

        for KeyValue(m, i) in imports do
            if m <> currentModuleName then
                let defaultImport =
                    match i.TryGetValue("default") with
                    | true, d -> Some d
                    | _ -> None
                let fullImport =
                    match i.TryGetValue("*") with
                    | true, f -> Some f
                    | _ -> None
                let namedImports =
                    i |> Seq.choose (fun (KeyValue(n, i)) ->
                        if n = "default" || n = "*" then
                            None
                        else
                            Some (n, i)
                    )
                    |> List.ofSeq
                let fromModule =
                    if m.StartsWith (asmName + "/") then
                        "./" + m[asmName.Length + 1 ..] + ".js"
                    else
                        "../" + m + ".js"
                declarations.Add(Import(defaultImport, fullImport, namedImports, fromModule))

        List.ofSeq (Seq.concat [ declarations; statements ])

let packageAssembly (refMeta: M.Info) (current: M.Info) asmName entryPoint entryPointStyle =
    let pkgs = ResizeArray()
    let classes = HashSet(current.Classes.Keys)
    let pkgTyp (typ: TypeDefinition) =
        let p = packageType refMeta current asmName typ None entryPointStyle
        if not (List.isEmpty p) then
            pkgs.Add(typ.Value.FullName.Replace("+", "."), p)
    for typ in current.Interfaces.Keys do
        classes.Remove(typ) |> ignore
        pkgTyp typ
    for typ in classes do
        pkgTyp typ
    if Option.isSome entryPoint then
        let epTyp = TypeDefinition { Assembly = ""; FullName = "$EntryPoint" }     
        let p = packageType refMeta current asmName epTyp entryPoint entryPointStyle
        pkgs.Add("$EntryPoint", p)
    pkgs.ToArray()

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
