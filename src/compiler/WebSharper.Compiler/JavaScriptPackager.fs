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
    let currentModuleName = asmName + "/" + typ.Value.FullName.Replace('+', '.')

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
    
    let getOrImportAddress (address: Address) =
        match addresses.TryGetValue address with
        | true, v -> v
        | _ ->
            //let getModuleName (from: string) =
            //    let fn = from.Split('/') |> Array.last
            //    if fn.EndsWith(".js") then
            //        fn.[.. fn.Length - 4].Replace(".", "$")
            //    else fn.Replace(".", "$")
            let res =
                match address.Module with
                | StandardLibrary
                | JavaScriptFile _ ->
                    match address.Address.Value with
                    | [] -> ()
                    | l -> 
                        let fromJS = List.last l
                        if StandardLibNames.Set.Contains fromJS then
                            jsUsed.Add(fromJS) |> ignore
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
                        match address.Address.Value |> List.rev with
                        | "default" :: res ->
                            res |> List.fold (fun e i -> ItemGet(e, Value (String i), Pure)) (Var classId)
                        | _ ->
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
            addresses.Add(address, res)
            res
            
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

            override this.TransformGlobalAccessSet (a, v) = 
                match getOrImportAddress a with
                | GlobalAccess ga ->
                    GlobalAccessSet(ga, this.TransformExpression v)
                | ItemGet(e, i, _) ->
                    ItemSet(this.TransformExpression e, this.TransformExpression i, this.TransformExpression v)
                | _ ->
                    failwith "invalid address import"

            override this.TransformItemGet(e, i, p) =
                match e, i with
                | I.GlobalAccess a, I.Value (Literal.String n) when a.Address.Value.IsEmpty ->
                    a.Sub(n) |> getOrImportAddress
                | _ ->
                    base.TransformItemGet(e, i, p)

            override this.TransformItemSet(e, i, v) =
                match e, i with
                | I.GlobalAccess a, I.Value (Literal.String n) when a.Address.Value.IsEmpty ->
                    match a.Sub(n) |> getOrImportAddress with
                    | GlobalAccess ga ->
                        GlobalAccessSet(ga, this.TransformExpression v)
                    | ItemGet(e, i, _) ->
                        ItemSet(this.TransformExpression e, this.TransformExpression i, this.TransformExpression v)
                    | _ ->
                        failwith "invalid address import"
                | _ ->
                    base.TransformItemSet(e, i, v)
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
                | _ ->
                    let info = 
                        {
                            IsStatic = true
                            IsPrivate = false // TODO
                            IsOptional = false
                        }
                    members.Add <| ClassProperty(info, mname, TSType.Any, Some body)
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
                    members.Add <| ClassProperty(info false false, name, TSType.Any, None)
                | M.StaticField name, _, _ ->
                    members.Add <| ClassProperty(info true false, name, TSType.Any, None)
                | M.IndexedField _, _, _ ->
                    () //TODO
                | M.VarField v, _, _ -> ()

        for f in c.Fields.Values do
            match f with
            | M.VarField v, _, _ ->
                statements.Add <| VarDeclaration(v, Undefined)
            | _ -> ()

        for info, _, _, body in c.Methods.Values do
            mem info body
        
        for info, body in c.Implementations.Values do
            mem info body            

        let constructors = ResizeArray<string * Id list * Statement>()

        for KeyValue(ctor, (info, opts, body)) in c.Constructors do
            //let (|EmptyCtorBody|_|) expr =
            //    match body with
            //    | Function ([], _, _, I.Empty) 
            //    | Function ([], _, _, I.ExprStatement(I.Application(I.Base, [], _))) -> Some()
            //    | _ -> None
            match withoutMacros info with
            | M.New None ->
                if body <> Undefined then
                    match body with
                    //| EmptyCtorBody -> 
                    //    ()
                    | Function (args, _, _, b) ->                  
                        let args = List.map (fun x -> x, Modifiers.None) args
                        members.Add (ClassConstructor (args, Some b, TSType.Any))
                    | _ ->
                        failwithf "Invalid form for translated constructor"

            | M.New (Some name) ->
                match body with
                | Function (args, _, _, b) ->                  
                    constructors.Add(name, args, b)
                    let info =
                        {
                            IsStatic = true
                            IsPrivate = false
                            Kind = ClassMethodKind.Simple
                        }
                    let ctorBody =
                        Return (New (This, [], Value (String name) :: (args |> List.map Var)))
                    members.Add (ClassMethod(info, name, args, Some ctorBody, TSType.Any))
                | _ ->
                    failwithf "Invalid form for translated constructor"
            //| M.NewIndexed i ->
            //    if body <> Undefined then
            //        match body with
            //        | Function (args, _, _, b) ->  
            //            indexedCtors.Add (i, (args, b))
            //        | _ ->
            //            failwithf "Invalid form for translated constructor"
            | M.Func name ->
                match body with 
                | Function (args, _, _, b) ->  
                    statements.Add <| ExportDecl(false, FuncDeclaration(Id.New(name, str = true), args, bodyTransformer.TransformStatement b, []))
                | _ ->
                    failwithf "Invalid form for translated constructor"
            | M.Static (name, kind) ->
                match body with 
                | Function (args, _, _, b) ->  
                    let info =
                        {
                            IsStatic = true
                            IsPrivate = false
                            Kind = kind
                        }
                    members.Add (ClassMethod(info, name, args, Some b, TSType.Any))
                | _ ->
                    failwithf "Invalid form for translated constructor"
            | _ -> ()                            

        let baseType, isObjBase =
            let tryFindClass c =
                match refMeta.Classes.TryFind c with
                | Some _ as res -> res
                | _ -> current.Classes.TryFind c
            match c.BaseClass |> Option.bind (fun b -> tryFindClass b.Entity) with
            | Some (ba, _, _) -> Some (getOrImportAddress ba), c.BaseClass.Value.Entity = Definitions.Object
            | _ -> None, false

        if constructors.Count > 0 then
            let index = Id.New("i", mut = false)
            let maxArgs = constructors |> Seq.map (fun (_, a, _) -> List.length a) |> Seq.max
            let cArgs = List.init maxArgs (fun _ -> Id.New(mut = false, opt = true))
            
            let ctorData = Dictionary()
            for (name, args, body) in constructors do
                // TODO what if not at start
                let chainedCtor, bodyRest =
                    match body with
                    | I.Block (I.ExprStatement (I.Application(I.Self, I.Value(String baseName) :: baseArgs, _)) :: r) ->
                        Some (baseName, baseArgs), Some (Block r)                                        
                    | I.ExprStatement (I.Application(I.Self, I.Value(String baseName) :: baseArgs, _)) ->
                        Some (baseName, baseArgs), None
                    | I.ExprStatement (I.Sequential(I.Application(I.Self, I.Value(String baseName) :: baseArgs, _) :: r)) ->
                        Some (baseName, baseArgs), Some (ExprStatement (Sequential r))
                    
                    | I.ExprStatement (I.Application(I.Base, [], _)) ->
                        if Option.isSome baseType then
                            None, Some body
                        else
                            None, None
                    | _ -> 
                        None, Some body
                ctorData.Add(name, (args, chainedCtor, bodyRest))

            // calculate order of constructors so chained constructors work,
            // adding those with no dependencies first, so we will need to reverse in the end
            let addedCtors = HashSet()
            let orderedCtorData = ResizeArray<string * Id list * (string * Expression list) option * Statement option>()
            while ctorData.Count > 0 do
                for KeyValue(name, (args, chainedCtor, bodyRest)) in ctorData |> Array.ofSeq do
                    let okToAdd =
                        match chainedCtor with
                        | None -> true
                        | Some (ccName, _) -> addedCtors.Contains ccName
                    if okToAdd then
                        addedCtors.Add name |> ignore
                        //let chainedCtorArgsNum = match chainedCtor with Some (_, a) -> a.Length | None -> 0
                        orderedCtorData.Add (name, args, 
                            chainedCtor |> Option.map (fun (n, a) -> n, a), 
                            bodyRest
                        )
                        ctorData.Remove name |> ignore
            orderedCtorData.Reverse()

            let cBody =
                let origIndices = Dictionary()
                [ 
                    if baseType.IsSome then
                        yield 
                            If (Unary(UnaryOperator.TypeOf, Var index) ^!= Value (String "string"), 
                                Block [
                                    ExprStatement (Appl(Base, [Var index], NonPure, None))
                                    If (Var index, ExprStatement(JSRuntime.ObjectAssign This (Var index)), Empty)
                                ], Empty)
                    else
                        yield 
                            If (Unary(UnaryOperator.TypeOf, Var index) ^== Value (String "object"), 
                                ExprStatement (JSRuntime.ObjectAssign This (Var index)), Empty)
                    for (name, args, chainedCtor, bodyRest) in orderedCtorData do
                        match chainedCtor with
                        | Some (ccName, ccArgs) ->
                            let mutable oiOpt = None
                            match bodyRest with 
                            | Some _ ->
                                let oi = Id.New(mut=false)
                                yield VarDeclaration(oi, Undefined)
                                origIndices.Add(name, oi)
                                oiOpt <- Some oi
                            | _ -> ()
                            for a in args do
                                yield VarDeclaration (a, Undefined)
                            let setters =
                                [
                                    // set original arg vars
                                    match oiOpt with
                                    | Some oi -> 
                                        yield VarSetStatement (oi, Value (Bool true))
                                    | _ -> ()
                                    for (a, ca) in Seq.zip args cArgs do
                                        yield VarSetStatement (a, Var ca)
                                    // redirect to chained constructor
                                    yield VarSetStatement(index, Value (String ccName))
                                    for v, vv in Seq.zip cArgs ccArgs do
                                        yield VarSetStatement(v, vv)    
                                ]
                            yield If((Var index) ^== Value (String name), Block setters, Empty)
                        | _ -> ()
                    for (name, args, chainedCtor, bodyRest) in orderedCtorData do
                        match chainedCtor, bodyRest with
                        | None, Some br ->
                            let settersAndBody =
                                [
                                    // set original arg vars
                                    for (a, ca) in Seq.zip args cArgs do
                                        yield VarDeclaration (a, Var ca)
                                    yield br
                                ]
                            yield If((Var index) ^== Value (String name), Block settersAndBody, Empty)
                        | _ -> ()
                    for (name, args, _, bodyRest) in orderedCtorData do
                        match origIndices.TryGetValue name, bodyRest with
                        | (true, oi), Some br ->
                            yield If(Var oi, br, Empty)
                        | _ -> ()
                ]

            let allArgs = List.map (fun x -> x, Modifiers.None) (index :: cArgs)
            members.Add (ClassConstructor (allArgs, Some (Block cBody), TSType.Any))   
        
        let mutable isFSharpType = false

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
            isFSharpType <- true
        | M.FSharpRecordInfo r when Option.isNone c.Type ->     
            let genCtor = 
                let arg = Id.New("$")
                let assign = ExprStatement (JSRuntime.ObjectAssign This (Var arg))
                ClassConstructor([ arg, Modifiers.None ], Some assign, TSType.Any)
            members.Add <| genCtor
            //let newFunc =
            //    let args = r |> List.map (fun f -> Id.New(f.Name, mut = false))
            //    let newId = Id.New("New", str = true)
            //    let body =
            //        New(Var classId, [], [ Object (args |> List.map ) ])
            //    FuncDeclaration(newId, args, )
            //statements.Add <| ExportDecl(false, newFunc)
            isFSharpType <- true
        | _ -> ()
        //| _ ->
        //    if c.HasWSPrototype then
        //        packageCtor addr <| ClassExpr(None, baseType, List.ofSeq members) 

        match c.StaticConstructor with
        | Some st -> 
            members.Add <| ClassStatic(staticThisTransformer.TransformStatement st)
        | _ -> ()

        let lazyClassId = lazy Id.New("$c")

        let packageLazyClass classExpr =
            statements.Add <| VarDeclaration(lazyClassId.Value, bodyTransformer.TransformExpression (JSRuntime.Lazy classExpr))
            statements.Add <| ExportDecl(true, ExprStatement(Var lazyClassId.Value))                

        let packageClass classDecl = 
            statements.Add <| ExportDecl(true, bodyTransformer.TransformStatement classDecl)                

        if c.HasWSPrototype || members.Count > 0 then
            let classExpr setInstance = 
                ClassExpr(Some classId, baseType, 
                    ClassStatic (VarSetStatement(lazyClassId.Value, setInstance(This))) 
                    :: List.ofSeq members)
            let classDecl() = Class(classId, baseType, [], List.ofSeq members, [])
            match baseType with
            | Some b ->
                let needsLazy = Option.isNone c.Type
                if needsLazy then
                    packageLazyClass <| fun i ->
                        if isObjBase then
                            classExpr i
                        else
                            Sequential [
                                JSRuntime.Force(b)
                                classExpr i
                            ]
                else
                    packageClass <| classDecl()
            | None ->
                let needsLazy = c.HasWSPrototype && Option.isNone c.Type && typ <> Definitions.Object && not isFSharpType
                if needsLazy then
                    packageLazyClass <| classExpr
                else
                    packageClass <| classDecl()
                            
    match current.Classes.TryFind(typ) with
    | Some (a, ct, cOpt) ->
        cOpt |> Option.iter (fun c -> packageClass c a ct typ.Value.FullName)
    | None -> ()

    match current.Interfaces.TryFind(typ) with
    | Some i ->

        let methodNames =
            i.Methods.Values |> Seq.map (fun (i, _, _) -> i)

        let isFunctionName =
            isFunctionNameForInterface typ
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
