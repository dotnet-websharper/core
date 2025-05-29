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
type O = WebSharper.Core.JavaScript.Output

type ThisTransformer() =
    inherit Transformer()

    let mutable thisVars = [] : list<option<Id> * ref<bool>>

    override this.TransformFunction(args, thisVar, typ, body) =
        match thisVar with 
        | None -> base.TransformFunction(args, thisVar, typ, body)
        | Some t ->
            thisVars <- (thisVar, ref false) :: thisVars
            let trBody = base.TransformStatement(body)
            let res =
                match thisVars.Head with
                | _, captured when captured.Value ->
                    let trBody = Block [ VarDeclaration(t, JSThis); trBody ]
                    Function(args, thisVar, typ, trBody)    
                | _ ->
                    Function(args, thisVar, typ, trBody)    
            thisVars <- thisVars.Tail
            res

    override this.TransformFuncDeclaration(id, args, thisVar, body, ty) =
        thisVars <- (thisVar, ref false) :: thisVars
        let trBody = base.TransformStatement(body)
        let res =
            match thisVars.Head with
            | Some t, captured when captured.Value ->
                let trBody = Block [ VarDeclaration(t, JSThis); trBody ]
                FuncDeclaration(id, args, thisVar, trBody, ty)
            | _ ->
                FuncDeclaration(id, args, thisVar, trBody, ty)
        thisVars <- thisVars.Tail
        res

    override this.TransformClassMethod(i, n, args, thisVar, body, s) =
        thisVars <- (thisVar, ref false) :: thisVars
        let trBody = 
            match body with
            | Some b -> Some (base.TransformStatement(b))
            | _ -> None
        let res =
            match thisVars.Head with
            | Some t, captured when captured.Value ->
                let trBody = trBody |> Option.map (fun b -> Block [ VarDeclaration(t, JSThis); b ])
                ClassMethod(i, n, args, thisVar, trBody, s)
            | _ ->
                ClassMethod(i, n, args, thisVar, trBody, s)
        thisVars <- thisVars.Tail
        res

    override this.TransformClassConstructor(args, thisVar, body, s) =
        thisVars <- (thisVar, ref false) :: thisVars
        let trBody = 
            match body with
            | Some b -> Some (base.TransformStatement(b))
            | _ -> None
        let res =
            match thisVars.Head with
            | Some t, captured when captured.Value ->
                let trBody = trBody |> Option.map (fun b -> Block [ VarDeclaration(t, JSThis); b ])
                ClassConstructor(args, thisVar, trBody, s)
            | _ ->
                ClassConstructor(args, thisVar, trBody, s)
        thisVars <- thisVars.Tail
        res

    override this.TransformVar(id) =
        match thisVars with
        | (Some t, _) :: _ when t = id -> JSThis
        | _ :: rest ->
            match rest |> List.tryPick (function (Some t, captured) when t = id -> Some captured | _ -> None) with
            | Some captured ->
                captured.Value <- true
            | _ -> ()
            Var id
        | _ -> Var id

type LazyClassTransformer(needsLazy) =
    inherit Transformer()

    override this.TransformLazyClass(withoutLazy, withLazy) = 
        if needsLazy then
            withLazy
        else
            withoutLazy

    override this.TransformImport(defaultImport, fullImport, namedImports, fromModule) =
        if needsLazy || (fromModule <> "../WebSharper.Core.JavaScript/Runtime.js" && fromModule <> "./Runtime.js") then
            Import(defaultImport, fullImport, namedImports, fromModule)
        else
            let namedImportsWithoutLazy =
                namedImports |> List.filter (fun (n, _) -> n <> "Lazy" && n <> "Force")
            if List.isEmpty namedImportsWithoutLazy && Option.isNone defaultImport then
                Empty
            else
                Import(defaultImport, fullImport, namedImportsWithoutLazy, fromModule)

type EntryPointStyle =
    | OnLoadIfExists
    | ForceOnLoad
    | ForceImmediate
    | LibraryBundle
    | SiteletBundle of IDictionary<TypeDefinition, ISet<Method>>

type PackageContent =
    | SingleType of TypeDefinition
    | Bundle of TypeDefinition [] * EntryPointStyle * Statement option

type PackageTypeResults =
    {
        Statements: Statement list
        BundleExports: Dictionary<Address, Id>
        Imports: seq<AST.CodeResource>
        BaseClass: option<TypeDefinition> 
        HasStaticConstructor: bool
    }

let packageType (output: O) (refMeta: M.Info) (current: M.Info) asmName flattened (content: PackageContent) =
    let imports = Dictionary<AST.CodeResource, Dictionary<string, Id>>()
    let jsUsed = HashSet<string>()
    let declarations = ResizeArray<Statement>()
    let addresses = Dictionary<Address, Expression>()
    let statements = ResizeArray<Statement>()

    let classRes = Dictionary<TypeDefinition, Address * Id * Id>()
    let currentScope = HashSet<CodeResource>()
        
    let allClasses = MergedDictionary(refMeta.Classes, current.Classes)
    let allInterfaces = MergedDictionary(refMeta.Interfaces, current.Interfaces)

    let isSingleType =
        match content with
        | SingleType _ -> true
        | Bundle _ -> false

    let lazyClasses = HashSet()
    let orderedTypes =
        match content with
        | SingleType typ -> [| typ |]
        | Bundle (typs, _, _) ->
            let all = HashSet(typs)
            let ordered = ResizeArray()
            let rec visit typ =
                match current.Classes.TryFind(typ) with
                | None 
                | Some (_, _, None) -> ()
                | Some (_, _, Some c) ->
                    match c.BaseClass with
                    | Some bc ->
                        if all.Contains bc.Entity then
                            visit bc.Entity
                        if lazyClasses.Contains bc.Entity then
                            lazyClasses.Add typ |> ignore
                    | _ -> ()
                    match c.StaticConstructor with
                    | Some _ ->
                        lazyClasses.Add typ |> ignore
                    | _ -> ()
                all.Remove typ |> ignore
                ordered.Add typ
            while all.Count > 0 do
                let typ = Seq.head all
                visit typ
            ordered.ToArray()
    
    for typ in orderedTypes do
        let className = (typ.Value.FullName.Split([|'.';'+'|]) |> Array.last).Split('`') |> Array.head
        let classId = Id.New className
        let outerClassId = Id.New "_c"
        let classAddrs = ResizeArray()
        let addClassAddr a isMainAddr =
            match a with 
            | { Module = DotNetType m } ->
                if isSingleType then
                    classAddrs.Add(a, m, isMainAddr)
                else
                    for asm in m.Assembly.Split(',') do
                        let m = { m with Assembly = asm }
                        let a = { a with Module = DotNetType m }
                        classAddrs.Add(a, m, isMainAddr)
            | _ -> 
                ()

        let isUnionCase = 
            match current.Classes.TryGetValue(typ) with
            | true, (a, i, cls) ->
                addClassAddr a true
                match cls with
                | None -> ()
                | Some cls ->
                    for KeyValue(me, mi) in cls.Methods do
                        match mi.CompiledForm with
                        | M.GlobalFunc (a, _) ->
                            addClassAddr a false
                        | _ -> 
                            ()
                    for KeyValue(me, mi) in cls.Implementations do
                        match mi.CompiledForm with
                        | M.GlobalFunc (a, _) ->
                            addClassAddr a false
                        | _ -> 
                            ()
                match i with Metadata.CustomTypeInfo.FSharpUnionCaseInfo _ -> true | _ -> false 
            | _ ->
                match current.Interfaces.TryGetValue(typ) with
                | true, { Address = { Module = DotNetType m } as a } ->
                    classAddrs.Add (a, m, true)
                    false
                | _ ->
                    false
        let localClassId =
            if isSingleType || lazyClasses.Contains typ then
                outerClassId
            else
                classId    
        if not isUnionCase then
            for (classAddr, classCodeRes, isMainAddr) in classAddrs do
                if isMainAddr && output <> O.JavaScript && classAddr.Address <> [ "default" ] then
                    // add type export
                    let typeAddr = { classAddr with Address = [ "default" ] }
                    addresses.Add(typeAddr, Var classId)
                
                if isMainAddr && not (addresses.ContainsKey classAddr) then
                    addresses.Add(classAddr, Var localClassId)
                currentScope.Add(classCodeRes) |> ignore
                if isMainAddr && not (classRes.ContainsKey typ) then
                    classRes.Add(typ, (classAddr, classId, localClassId))
            if classAddrs.Count = 0 then
                // not represented as JS class
                classRes.Add(typ, (Address.Global(), classId, localClassId))

    let export isDefault statement =
        match content with
        | SingleType _ -> ExportDecl(isDefault, statement)
        | Bundle (_, SiteletBundle _, _) -> statement
        | Bundle _ -> 
            match statement with
            | ExprStatement(Var _) -> Empty
            | _ -> statement

    let bundleExports = Dictionary<Address, Id>()
    
    let exportWithBundleSupport isDefault typ mOpt addr id statement =
        match content with
        | Bundle (_, SiteletBundle exportedTypes, _) ->
            let bundleExport() =
                bundleExports[addr] <- id
                ExportDecl(false, statement)
            let ignoreVar() =
                match statement with
                | ExprStatement(Var _) -> Empty
                | _ -> statement

            match exportedTypes.TryGetValue(typ) with
            | true, null ->
                bundleExport()
            | true, methods ->
                match mOpt with
                | Some m ->
                    if methods.Contains m then
                        bundleExport()
                    else
                        ignoreVar()
                | _ -> 
                    bundleExport()
            | _ ->
                ignoreVar()
        | _ -> 
            export isDefault statement

    addresses.Add(Address.Lib "import", Var (Id.Import()))
    let safeObject expr = Binary(expr, BinaryOperator.``||``, Object []) 
        
    let getOrImportAddress (sideEffectingImport: bool) (address: Address) =
        match addresses.TryGetValue address with
        | true, v -> v
        | _ ->
            let res =
                match address.Module with
                | StandardLibrary ->
                    match address.Address with
                    | [] -> ()
                    | l -> 
                        let fromJS = List.last l
                        if StandardLibNames.Set.Contains fromJS then
                            jsUsed.Add(fromJS) |> ignore
                    GlobalAccess address    
                | JavaScriptFile m ->
                    if not (imports.ContainsKey m) then
                        let mi = Dictionary()
                        imports.Add(m, mi)
                    match address.Address with
                    | [] -> ()
                    | l -> 
                        let fromJS = List.last l
                        if StandardLibNames.Set.Contains fromJS then
                            jsUsed.Add(fromJS) |> ignore
                    GlobalAccess address    
                | JavaScriptModule m ->
                    let importWhat, importAs =
                        let fromModuleName() = m.Name.Replace('.', '_').Replace('`', '_')
                        match address.Address with
                        | [] -> 
                            "*", fromModuleName()
                        | a -> 
                            let n = List.last a
                            if n = "default" then
                                n, fromModuleName()
                            else
                                n, n
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
                        match address.Address with
                        | [] -> { address with Module = ImportedModule i }
                        | a -> { Module = ImportedModule i; Address = (a |> List.rev |> List.tail |> List.rev) }
                    GlobalAccess importedAddress
                | NpmPackage p ->
                    if sideEffectingImport then
                        let m = { Assembly = ""; Name = p }
                        match imports.TryGetValue m with
                        | true, mi -> ()
                        | _ ->
                            let mi = Dictionary()
                            imports.Add(m, mi)
                        Undefined
                    else
                        let importWhat, importAs =
                            let fromModuleName() = (p.Split('/') |> Array.last).Replace('.', '_').Replace('`', '_')
                            match address.Address with
                            | [] -> 
                                "*", fromModuleName()
                            | a -> 
                                let n = List.last a
                                if n = "default" then
                                    n, fromModuleName()
                                else
                                    n, n
                        let m = { Assembly = ""; Name = p }
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
                            match address.Address with
                            | [] -> { address with Module = ImportedModule i }
                            | a -> { Module = ImportedModule i; Address = (a |> List.rev |> List.tail |> List.rev) }
                        GlobalAccess importedAddress
                | DotNetType m ->
                    //let m = 
                    //    match output with
                    //    | O.JavaScript -> { m with Name = m.Name + ".js" }
                    //    | _ -> m
                    if currentScope.Contains m then
                        match address.Address |> List.rev with
                        | [] ->
                            Var (Id.Global())
                        | "default" :: res ->
                            let classVar = addresses[Address.TypeDefaultExport m]
                            res |> List.fold (fun e i -> ItemGet(e, Value (String i), Pure)) classVar
                        | _ ->
                            let name = address.Address.Head
                            let i = Id.New(name, str = isSingleType)
                            Var i
                    else
                        let importWhat, importAs =
                            let fromModuleName() = (m.Name.Split([| '/'; '.' |]) |> Array.last).Split('`') |> Array.head
                            match address.Address with
                            | [] -> 
                                "*", fromModuleName()
                            | a -> 
                                let n = List.last a
                                if n = "default" then
                                    n, fromModuleName()
                                else
                                    n, n
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
                            match address.Address with
                            | [] -> { address with Module = ImportedModule i }
                            | a -> { Module = ImportedModule i; Address = (a |> List.rev |> List.tail |> List.rev) }
                        GlobalAccess importedAddress
                | _ -> GlobalAccess address          
            addresses.Add(address, res)
            
            // TODO: remove, safety check only
            match res with 
            | GlobalAccess { Module = JavaScriptModule _ | DotNetType _ | NpmPackage _ } ->
                failwithf "unexpected import result for %A" address
            | _ -> ()
            
            res
            
    let classIdToOuter = 
        lazy 
        classRes.Values |> Seq.map (fun (_, classId, outerClassId) -> outerClassId, classId) |> dict

    let tsTypeOfAddress (a: Address) =
        let t = a.Address |> List.rev
        match a.Module with
        | StandardLibrary
        | JavaScriptFile _ -> TSType.Named t
        | JavaScriptModule _ 
        | NpmPackage _
        | DotNetType _ ->
            let a = 
                if a.Address.IsEmpty then 
                    { a with Address = [ "default" ] } 
                else 
                    a
            match getOrImportAddress false a with
            | GlobalAccess { Module = ImportedModule i; Address = a } ->
                TSType.Imported(i, a |> List.rev)
            | Var i ->
                match classIdToOuter.Value.TryGetValue(i) with
                | true, oi ->
                    TSType.Imported(oi, [])
                | _ ->
                    TSType.Imported(i, [])
            | _ ->
                TSType.Named t
        
        | ImportedModule v -> TSType.Imported(v, t)

    let lookupType (t: TypeDefinition) =
        match allInterfaces.TryFind t with
        | Some i -> TypeTranslator.Interface i
        | _ ->
        match allClasses.TryFind t with
        | Some (a, ct, c) -> TypeTranslator.Class (a, ct, c)
        | _ -> TypeTranslator.Unknown
    
    let typeTranslator = TypeTranslator.TypeTranslator(lookupType, tsTypeOfAddress) 
    
    let inline tsTypeOfDef t = typeTranslator.TSTypeOfDef t
    let inline tsTypeOfConcrete gs i = typeTranslator.TSTypeOfConcrete i
    let inline tsTypeOf gs t = typeTranslator.TSTypeOf t

    let getGenerics j (gs: list<M.GenericParam>) =
        if output = O.JavaScript then [] else
        let gsArr = Array.ofList gs
        gs |> Seq.indexed |> Seq.choose (fun (i, c) ->
            match c.Type with
            | Some t -> None
            | _ ->
                let p = TSType.Param (j + i)
                let cs = c.Constraints |> List.choose (fun t ->
                    match tsTypeOf gsArr t with
                    | TSType.Any -> None
                    | t -> Some t)
                match cs with 
                | [] -> p
                | cs -> TSType.Constraint(p, cs)
                |> Some
        ) |> List.ofSeq

    let addGenerics g t =
        match g with
        | [] -> t
        | _ -> 
            match t with
            | TSType.Any
            | TSType.Generic _ -> t
            | _ ->
                TSType.Generic(t, g)

    let resModule (t: TSType) =
        if output = O.JavaScript then t else 
        t.ResolveAddress (fun a ->
            let a =
                match a.Address with
                | [] -> { a with Address = [ "default" ] }
                | _ -> a
            match getOrImportAddress false a with 
            | Var v -> TSType.Imported (v, [])
            | GlobalAccess ({ Module = ImportedModule v; Address = ia }) -> TSType.Imported (v, ia)
            | _ -> TSType.Named a.Address
        )

    let bodyTransformer gsArr =
        { new Transformer() with
            override this.TransformGlobalAccess a = getOrImportAddress false a

            override this.TransformSideeffectingImport a = getOrImportAddress true a

            override this.TransformGlobalAccessSet (a, v) = 
                match getOrImportAddress false a with
                | GlobalAccess ga ->
                    GlobalAccessSet(ga, this.TransformExpression v)
                | ItemGet(e, i, _) ->
                    ItemSet(this.TransformExpression e, this.TransformExpression i, this.TransformExpression v)
                | _ ->
                    failwith "invalid address import"

            override this.TransformItemGet(e, i, p) =
                match e, i with
                | I.GlobalAccess a, I.Value (Literal.String n) when a.Address.IsEmpty ->
                    a.Sub(n) |> getOrImportAddress false
                | _ ->
                    base.TransformItemGet(e, i, p)

            override this.TransformItemSet(e, i, v) =
                match e, i with
                | I.GlobalAccess a, I.Value (Literal.String n) when a.Address.IsEmpty ->
                    match a.Sub(n) |> getOrImportAddress false with
                    | GlobalAccess ga ->
                        GlobalAccessSet(ga, this.TransformExpression v)
                    | ItemGet(e, i, _) ->
                        ItemSet(this.TransformExpression e, this.TransformExpression i, this.TransformExpression v)
                    | _ ->
                        failwith "invalid address import"
                | _ ->
                    base.TransformItemSet(e, i, v)

            override this.TransformId(a) =
                if output = O.JavaScript then 
                    a 
                else 
                    a.ToTSType(tsTypeOf gsArr)

            override this.TransformApplication(a, b, c) =
                if output = O.JavaScript then 
                    base.TransformApplication(a, b, c) 
                else 
                    base.TransformApplication(a, b, { c with Params = c.Params |> List.map resModule })

            override this.TransformNew(a, b, c) =
                if output = O.JavaScript then 
                    base.TransformNew(a, b, c) 
                else 
                    base.TransformNew(a, b |> List.map resModule, c)

            override this.TransformCast(a, b) =
                if output = O.JavaScript then 
                    this.TransformExpression b
                else 
                    base.TransformCast(resModule a, b)

            override this.TransformFuncDeclaration(a, b, c, d, e) =
                if output = O.JavaScript then 
                    base.TransformFuncDeclaration(a, b, c, d, e) 
                else 
                    base.TransformFuncDeclaration(a, b, c, d, e |> List.map resModule)
        }
            
    let addStatement st =
        statements.Add <| ThisTransformer().TransformStatement(st)

    match content with 
    | Bundle (_, SiteletBundle _, _) ->
        let runtime = Id.New("Runtime")
        declarations.Add(ExportDecl(false, Import(None, None, [ "default", runtime], "../WebSharper.Core.JavaScript/Runtime.js")))   
    | _ -> ()

    let implExpr, implSt, implStOpt, implExprOpt =
        if output = O.TypeScriptDeclaration then 
            (fun _ -> Undefined), (fun _ -> Empty), (fun _ -> None), (fun _ -> None)
        else
            (fun getImpl -> getImpl()), (fun getImpl -> getImpl()), (fun getImpl -> Some (getImpl())), (fun getImpl -> Some (getImpl()))
            
    let rec withoutMacros info =
        match info with
        | M.Macro (_, _, Some fb) -> withoutMacros fb
        | _ -> info 

    let mutable baseClass: option<TypeDefinition> = None
    let mutable hasStaticConstructor = false

    let packageClass (typ: TypeDefinition) (a: Address) (ct: M.CustomTypeInfo) (c: M.ClassInfo) =
        match classRes.TryGetValue typ with 
        | false, _ -> ()
        | true, (currentClassAddr, classId, outerClassId) ->

        let className = classId.Name.Value

        let isUnion =
            match ct with
            | M.FSharpUnionInfo _ -> true
            | _ -> false
        let className_T, classId_T = 
            if isUnion && c.HasWSPrototype then 
                let tn = className + "_T"
                tn, Id.New(tn, str = true)
            else    
                className, classId

        let gsArr = Array.ofList c.Generics
        let bTr() = bodyTransformer(gsArr)   

        let staticThisTransformer =
            { new Transformer() with
                override this.TransformGlobalAccess a = 
                    if a = currentClassAddr then
                        Var classId
                    else
                        GlobalAccess a
            }

        let typeOfParams (opts: M.Optimizations) gsArr (ps: list<Type>) =
            match opts.FuncArgs with
            | None -> ps |> List.map (tsTypeOf gsArr)
            | Some fa -> 
                (ps, fa) ||> List.map2 (fun p o ->
                    match o with
                    | NotOptimizedFuncArg -> tsTypeOf gsArr p
                    | CurriedFuncArg i ->
                        let rec decurry i acc t =
                            if i = 0 then
                                TSType.Lambda(List.rev acc, tsTypeOf gsArr t)
                            else
                                match t with
                                | FSharpFuncType (a, r) ->
                                    decurry (i - 1) (tsTypeOf gsArr a :: acc) r
                                | _ -> failwith "Error decurrying function parameter type"
                        decurry i [] p
                    | TupledFuncArg i -> 
                        match p with
                        | FSharpFuncType (TupleType (ts, _), r) ->
                            TSType.Lambda(ts |> List.map (tsTypeOf gsArr), tsTypeOf gsArr r)
                        | _ -> failwith "Error detupling function parameter type"
                    | OutRefArg ->
                        match p with
                        | ByRefType t ->
                            TSType.Generic (tsTypeOfAddress TypeTranslator.OutRefAddress, [tsTypeOf gsArr t])
                        | _ -> failwith "Error handling out ref argument"
                    | InRefArg ->
                        match p with
                        | ByRefType t ->
                            TSType.Generic (tsTypeOfAddress TypeTranslator.InRefAddress, [tsTypeOf gsArr t])
                        | _ -> failwith "Error handling in ref argument"                    
                )

        let cgenl = List.length c.Generics
        let thisTSTypeDef = lazy tsTypeOf gsArr (NonGenericType typ)
        let cgen = getGenerics 0 c.Generics
        let thisTSType = lazy (thisTSTypeDef.Value |> addGenerics cgen) 

        let thisTSTypeOrUnion =
            lazy 
            if isUnion && c.HasWSPrototype then 
                TSType.Basic className |> addGenerics cgen 
            else
                thisTSType.Value

        let members = ResizeArray<Statement>()
         
        let baseClassInfos =
            lazy
            let rec getBaseClassInfo (c: Concrete<TypeDefinition> option, gen: Type[]) =
                match c with
                | Some bc ->
                    match allClasses.[bc.Entity] with
                    | _, _, Some cls ->
                        let gen = bc.Generics |> List.map (fun t -> t.SubstituteGenerics gen) |> Array.ofList
                        Some ((bc.Entity, (cls, gen)), (cls.BaseClass, gen))
                    | _ -> None
                | _ -> None
            (c.BaseClass, Array.init c.Generics.Length TypeParameter) |> List.unfold getBaseClassInfo |> dict

        let mem (m: Method) info gc opts intfGen body =
            
            let gsArr = Array.append gsArr (Array.ofList gc)
            let bTr() = bodyTransformer(gsArr)   
            let getSignature baseGen isInstToStatic =         
                if output = O.JavaScript then TSType.Any else
                match IgnoreExprSourcePos body with
                | Function (_, _, ret, _) ->
                    //let isUnionCaseMethod =
                    //    if isUnion then
                    //        match m.Value.ReturnType with
                    //        | ConcreteType { Entity = e } ->
                    //            let fn = e.Value.FullName
                    //            fn.StartsWith(typ.Value.FullName + "+")
                    //        | _ -> false
                    //    else false
                    //if isUnionCaseMethod then
                    //    failwithf "unioncasemethod %s method %s" typ.Value.FullName m.Value.MethodName
                    //    let p, r = m.Value.Parameters, m.Value.ReturnType
                    //    let pts = typeOfParams opts gsArr p
                    //    TSType.Lambda(pts, TSType.Intersection [ tsTypeOf gsArr r; thisTSTypeOrUnion.Value ] ) 
                    //else
                        let p, r = 
                            match baseGen with 
                            | None -> 
                                m.Value.Parameters
                                , ret |> Option.defaultValue m.Value.ReturnType
                            | Some ig -> 
                                try
                                    m.Value.Parameters |> List.map (fun p -> p.SubstituteGenerics ig) 
                                    , m.Value.ReturnType.SubstituteGenerics ig 
                                with _ ->
                                    failwithf "failed to substitute interface/override generics: %A to %A" ig m
                        let r =
                            if cgenl = 0 && List.isEmpty gc then
                                match r with
                                | TSType _ -> r
                                | _ -> r.SubstituteGenericsToSame (TSType TSType.Any)
                            else 
                                r
                        let pts =
                            if isInstToStatic then
                                (tsTypeOf gsArr (NonGenericType typ) |> addGenerics cgen) :: (typeOfParams opts gsArr p)
                            else typeOfParams opts gsArr p
                        TSType.Lambda(pts, tsTypeOf gsArr r)
                | _ ->
                    tsTypeOf [||] m.Value.ReturnType
            let mgen = getGenerics cgenl gc
            
            let isTakingSingleGenericArg =
                match m.Value.Parameters with
                | [Type.TypeParameter _ | Type.StaticTypeParameter _ | Type.LocalTypeParameter _] -> true
                | _ -> false

            let transformSingleGenericArgToEmpty (t: TSType) =
                match t with
                | TSType.Function(this, [ _ ], rest, ret) ->
                    TSType.Function(this, [], rest, ret)  
                | _ -> t

            let func fromInst addr =
                let f = 
                    match getOrImportAddress false addr with
                    | Var f -> 
                        f
                    | a ->
                        failwithf "Func var lookup failed for %A, got %A while writing type %A currentScope=%A" addr a typ (Array.ofSeq currentScope)
                match IgnoreExprSourcePos body with
                | Function (args, thisVar, _, b) ->
                    let f, args =
                        if output = O.JavaScript then f, args else
                        match getSignature intfGen fromInst with
                        | TSType.Function(t, a, rest, r) ->
                            let aTyp = a |> Array.ofList
                            f.WithType(Some (TSType r)), 
                            args |> List.mapi (fun i a ->
                                match aTyp |> Array.tryItem i with
                                | Some (t, _) -> a.WithType(Some (TSType t))
                                | _ -> a
                            )
                        | t ->
                            f.WithType(Some (TSType t)), args
                    if isTakingSingleGenericArg && output <> O.JavaScript then
                        addStatement <| exportWithBundleSupport false typ (Some m) addr f 
                            (FuncSignature(f, [], thisVar, cgen @ mgen))
                        addStatement <| exportWithBundleSupport false typ (Some m) addr f 
                            (FuncSignature(f, args, thisVar, cgen @ mgen))
                        if output = O.TypeScript then
                            let optArg = args[0].ToOptional()
                            let b = SubstituteVar(args[0], Var optArg).TransformStatement(b)
                            addStatement <| exportWithBundleSupport false typ (Some m) addr f 
                                (FuncDeclaration(f, [optArg], thisVar, implSt(fun () -> bTr().TransformStatement b), cgen @ mgen))
                    else
                        addStatement <| exportWithBundleSupport false typ (Some m) addr f 
                            (FuncDeclaration(f, args, thisVar, implSt(fun () -> bTr().TransformStatement b), cgen @ mgen))
                | e ->
                    if output <> O.TypeScriptDeclaration then 
                        let f = 
                            if output = O.JavaScript then f else
                            f.WithType(Some (TSType (getSignature intfGen fromInst)))
                        addStatement <| export false (VarDeclaration(f, implExpr(fun () -> bTr().TransformExpression e)))
            
            match withoutMacros info with
            | M.Instance (mname, mkind, modifier) ->
                match IgnoreExprSourcePos body with
                | Function (args, thisVar, _, b) ->
                    let info = 
                        {
                            IsStatic = false
                            IsPrivate = false // TODO
                            IsAbstract = modifier = M.Modifier.Abstract
                            Kind = mkind
                        }
                    let baseGen =
                        match modifier with
                        | M.Modifier.Override bcls ->
                            match baseClassInfos.Value.TryGetValue bcls with
                            | true, (cls, clsGen) ->
                                match m.Value.Generics with
                                | 0 -> clsGen
                                | mgen -> Array.append clsGen (Array.init mgen (fun i -> TypeParameter (cgenl + i)))
                            | _ ->
                                if bcls = typ then
                                    Array.init (cgenl + m.Value.Generics) (fun i -> TypeParameter i)
                                else
                                    [||] // TODO: should this be an error? I don't think it should ever happen
                            |> Some
                        | _ -> intfGen
                    if isTakingSingleGenericArg && output <> O.JavaScript then
                        members.Add <| ClassMethod(info, mname, [], thisVar, None, getSignature baseGen false |> transformSingleGenericArgToEmpty |> addGenerics mgen)
                        members.Add <| ClassMethod(info, mname, args, thisVar, None, getSignature baseGen false |> addGenerics mgen)
                        if output = O.TypeScript then
                            let optArg = args[0].ToOptional()
                            let b = SubstituteVar(args[0], Var optArg).TransformStatement(b)
                            members.Add <| ClassMethod(info, mname, [ optArg ], thisVar, implStOpt (fun () -> b), getSignature baseGen false |> addGenerics mgen)
                    else
                        members.Add <| ClassMethod(info, mname, args, thisVar, implStOpt (fun () -> b), getSignature baseGen false |> addGenerics mgen)
                | _ -> ()       
            | M.Static (mname, fromInst, mkind) ->
                match IgnoreExprSourcePos body with
                | Function (args, thisVar, ret, b) ->
                    let info = 
                        {
                            IsStatic = true
                            IsPrivate = false // TODO
                            IsAbstract = false
                            Kind = mkind
                        }
                    if isTakingSingleGenericArg && output <> O.JavaScript then
                        members.Add <| ClassMethod(info, mname, [], thisVar, None, getSignature intfGen fromInst |> transformSingleGenericArgToEmpty |> addGenerics (cgen @ mgen))
                        members.Add <| ClassMethod(info, mname, args, thisVar, None, getSignature intfGen fromInst |> addGenerics (cgen @ mgen))
                        if output = O.TypeScript then
                            let optArg = args[0].ToOptional()
                            let b = SubstituteVar(args[0], Var optArg).TransformStatement(b)
                            members.Add <| ClassMethod(info, mname, [ optArg ], thisVar, implStOpt (fun () -> staticThisTransformer.TransformStatement b), getSignature intfGen fromInst |> addGenerics (cgen @ mgen))
                    else
                        members.Add <| ClassMethod(info, mname, args, thisVar, implStOpt (fun () -> staticThisTransformer.TransformStatement b), getSignature intfGen fromInst |> addGenerics (cgen @ mgen))
                | _ ->
                    let info = 
                        {
                            IsStatic = true
                            IsPrivate = false // TODO
                            IsOptional = false
                        }
                    members.Add <| ClassProperty(info, mname, getSignature intfGen false |> addGenerics (cgen @ mgen), implExprOpt (fun () -> body))
            | M.Func (fname, fromInst) ->
                func fromInst (currentClassAddr.Func(fname))
            | M.Remote (fname, _, _) ->
                func false (currentClassAddr.Func(fname))
            | M.GlobalFunc (addr, fromInst) ->
                func fromInst addr
            | _ -> ()

        let propInfo isStatic isPrivate isOptional =
            {
                IsStatic = isStatic
                IsPrivate = isPrivate
                IsOptional = isOptional
            }

        if c.HasWSPrototype then
            for f in c.Fields.Values |> Seq.sortBy (fun f -> f.Order) do
                let fTyp =
                    if output = O.JavaScript then TSType.Any else
                    let ft =
                        if cgenl = 0 then
                            match f.Type with
                            | TSType _ -> f.Type
                            | _ -> f.Type.SubstituteGenericsToSame (TSType TSType.Any)
                        else
                            f.Type
                    tsTypeOf gsArr ft
                match f.CompiledForm with
                | M.InstanceField name ->
                    members.Add <| ClassProperty(propInfo false false false, name, fTyp, None)
                | M.OptionalField name -> 
                    members.Add <| ClassProperty(propInfo false false true, name, fTyp, None)
                | M.StaticField name ->
                    members.Add <| ClassProperty(propInfo true false false, name, fTyp, None)
                | M.IndexedField _ ->
                    () //TODO
                | M.VarField _ -> ()

        for f in c.Fields.Values do
            match f.CompiledForm with
            | M.VarField v ->
                if output <> O.TypeScriptDeclaration then 
                    addStatement <| VarDeclaration(v, Undefined)
            | _ -> ()

        // let mem (m: Method) info gc opts intfGen body =
        for KeyValue(m, mi) in c.Methods do
            mem m mi.CompiledForm mi.Generics mi.Optimizations None mi.Expression
        
        let interfaceInfos =
            lazy
            c.Implements |> Seq.map (fun i ->
                i.Entity, (allInterfaces.[i.Entity], Array.ofList i.Generics)
            ) |> dict

        for KeyValue((i, m), mi) in c.Implementations do
            let intfGen, mParam = 
                match interfaceInfos.Value.TryGetValue i with
                | true, (intf, intfGen) ->
                    let _, _, mg = intf.Methods.[m]
                    match m.Value.Generics with
                    | 0 -> intfGen, mg
                    | mgen -> Array.append intfGen (Array.init mgen (fun i -> TypeParameter (cgenl + i))), mg
                | _ ->
                    //match baseClassInfos.Value.TryGetValue i with
                    //| true, (cls, clsGen) ->
                    //    match m.Value.Generics with
                    //    | 0 -> clsGen
                    //    | mgen -> Array.append clsGen (Array.init mgen (fun i -> TypeParameter (cgenl + i)))
                    //    , 
                    //    cls.Methods.[m].Generics
                    //| _ ->
                    //    if i = typ then
                    //        Array.init (cgenl + m.Value.Generics) (fun i -> TypeParameter i), []
                    //    else
                            [||], [] // TODO: should this be an error? I don't think it should ever happen
            mem m mi.CompiledForm mParam M.Optimizations.None (Some intfGen) mi.Expression

        let constructors = ResizeArray<string * Id option * Id list * Statement * TSType>()
        let ctorSigs = ResizeArray<Statement>()

        for KeyValue(ctor, ct) in c.Constructors do
            let getSignature isNew nameOpt =         
                if output = O.JavaScript then TSType.Any else
                let pts = typeOfParams ct.Optimizations gsArr ctor.Value.CtorParameters
                if isNew then
                    let cpts =
                        match nameOpt with
                        | Some name -> TSType.Named [ "\"" + name + "\"" ] :: pts
                        | _ -> pts
                    TSType.New(cpts, thisTSType.Value)
                else
                    TSType.Function(None, pts |> List.map (fun a -> a, false), None, thisTSType.Value)

            match withoutMacros ct.CompiledForm with
            | M.New None ->
                if ct.Expression <> Undefined then
                    match ct.Expression with
                    | Function ([], _, _, I.Empty) 
                    | Function ([], _, _, I.ExprStatement(I.Application(I.Base, [], _))) -> 
                        ()
                    | Function (args, thisVar, _, b) ->                  
                        let args = List.map (fun x -> x, Modifiers.None) args
                        members.Add (ClassConstructor (args, thisVar, implStOpt (fun () -> b), getSignature true None))
                    | _ ->
                        failwithf "Invalid form for translated constructor"

            | M.New (Some name) ->
                match ct.Expression with
                | Function (args, thisVar, _, b) ->                  
                    constructors.Add(name, thisVar, args, b, getSignature true (Some name))
                    //let info =
                    //    {
                    //        IsStatic = true
                    //        IsPrivate = false
                    //        Kind = MemberKind.Simple
                    //    }
                    //let ctorBody() =
                    //    Return (New (JSThis, [], Value (String name) :: (args |> List.map Var)))
                    //members.Add (ClassMethod(info, name, args, thisVar, implStOpt ctorBody, getSignature false None |> addGenerics cgen))
                | _ ->
                    failwithf "Invalid form for translated constructor"
            | M.Func (name, _) ->
                match ct.Expression with 
                | Function (args, thisVar, _, b) ->  
                    let f = 
                        match getOrImportAddress false (currentClassAddr.Func(name)) with
                        | Var f -> f
                        | a ->
                            failwithf "Func var lookup failed for %A, got %A while writing type %A currentScope=%A" (currentClassAddr.Func(name)) a typ (Array.ofSeq currentScope)
                            //Id.New(name, mut = false, str = true)
                    addStatement <| export false (FuncDeclaration(f, args, thisVar, implSt (fun () -> bTr().TransformStatement b), cgen))
                | _ ->
                    failwithf "Invalid form for translated constructor"
            | M.Static (name, _, kind) ->
                match ct.Expression with 
                | Function (args, thisVar, _, b) ->  
                    let info =
                        {
                            IsStatic = true
                            IsPrivate = false
                            IsAbstract = false
                            Kind = kind
                        }
                    members.Add (ClassMethod(info, name, args, thisVar, implStOpt (fun () -> b), getSignature false None |> addGenerics cgen))
                | _ ->
                    failwithf "Invalid form for translated constructor"
            | _ -> ()                            

        let baseType, bgen, bc, isObjBase =
            let tryFindClass c =
                match refMeta.Classes.TryFind c with
                | Some _ as res -> 
                    if refMeta.Interfaces.ContainsKey c then None else res
                | _ -> 
                    if current.Interfaces.ContainsKey c then None else current.Classes.TryFind c
            match c.BaseClass |> Option.bind (fun b -> tryFindClass b.Entity) with
            | Some (ba, _, _) -> 
                let bgen =
                    if output = O.JavaScript then [] else
                    c.BaseClass.Value.Generics |> List.map (tsTypeOf [||])
                Some (getOrImportAddress false ba), bgen, Some c.BaseClass.Value.Entity, c.BaseClass.Value.Entity = Definitions.Object
            | _ -> None, [], None, false

        baseClass <- bc

        if constructors.Count > 0 then
            let index = Id.New("i", mut = false)
            let maxArgs = constructors |> Seq.map (fun (_, _, a, _, _) -> List.length a) |> Seq.max
            let cArgs = List.init maxArgs (fun _ -> Id.New(mut = false, opt = true))
            let cThis = Id.NewThis()
            
            let ctorData = Dictionary()
            for (name, thisVar, args, body, tsSig) in constructors do
                let body = 
                    match thisVar with
                    | Some t -> ReplaceId(t, cThis).TransformStatement(body)
                    | _ -> body
                
                let tryPickSplit chooser list =
                    let rec p acc rest =
                        match rest with
                        | h :: t ->
                            match chooser h with
                            | None -> p (h :: acc) t
                            | res -> Some (List.rev acc), res, if List.isEmpty t then None else Some t
                        | [] ->
                           None, None, Some list 
                    p [] list

                let beforeCtor, chainedCtor, bodyRest =
                    match body with
                    | I.Block (I.ExprStatement (I.Application(I.JSThis, I.Value(String baseName) :: baseArgs, _)) :: r) ->
                        None, Some (baseName, baseArgs), Some (Block r)                                        
                    | I.ExprStatement (I.Application(I.JSThis, I.Value(String baseName) :: baseArgs, _)) ->
                        None, Some (baseName, baseArgs), None
                    | I.ExprStatement (I.Sequential(I.Application(I.JSThis, I.Value(String baseName) :: baseArgs, _) :: r)) ->
                        None, Some (baseName, baseArgs), Some (ExprStatement (Sequential r))
                    | I.ExprStatement (I.Application(I.Base, [], _)) ->
                        if Option.isSome baseType then
                            None, None, Some body
                        else
                            None, None, None
                    | I.Block list ->
                        let b, c, r = 
                            list |> tryPickSplit (
                                function
                                | I.ExprStatement (I.Application(I.JSThis, I.Value(String baseName) :: baseArgs, _)) -> Some (baseName, baseArgs)
                                | _ -> None
                            )
                        b |> Option.map CombineStatements, c, r |> Option.map CombineStatements
                    | I.ExprStatement (I.Sequential list) ->
                        let b, c, r = 
                            list |> tryPickSplit (
                                function
                                | I.Application(I.JSThis, I.Value(String baseName) :: baseArgs, _) -> Some (baseName, baseArgs)
                                | _ -> None
                            )
                        b |> Option.map (CombineExpressions >> ExprStatement), c, r |> Option.map (CombineExpressions >> ExprStatement)
                    | _ -> 
                        None, None, Some body
                ctorData.Add(name, (args, beforeCtor, chainedCtor, bodyRest, tsSig))

            // calculate order of constructors so chained constructors work,
            // adding those with no dependencies first, so we will need to reverse in the end
            let addedCtors = HashSet()
            let orderedCtorData = ResizeArray<Statement option * string * Id list * (string * Expression list) option * Statement option * TSType>()
            let mutable isLoopConstr = true
            while ctorData.Count > 0 do
                isLoopConstr <- true
                for KeyValue(name, (args, beforeCtor, chainedCtor, bodyRest, tsSig)) in ctorData |> Array.ofSeq do
                    let okToAdd =
                        match chainedCtor with
                        | None -> true
                        | Some (ccName, _) -> addedCtors.Contains ccName
                    if okToAdd then
                        addedCtors.Add name |> ignore
                        //let chainedCtorArgsNum = match chainedCtor with Some (_, a) -> a.Length | None -> 0
                        orderedCtorData.Add (beforeCtor, name, args, 
                            chainedCtor |> Option.map (fun (n, a) -> n, a), 
                            bodyRest, tsSig
                        )
                        ctorData.Remove name |> ignore
                        isLoopConstr <- false
                if isLoopConstr then failwith <| sprintf "Looping constructors found in %s" typ.Value.FullName
 
            orderedCtorData.Reverse()
            
            // signatures
            if output <> O.JavaScript then
                for (_, _, args, _, _, tsSig) in orderedCtorData do
                    let allArgs = List.map (fun x -> x, Modifiers.None) (index :: args)
                    members.Add (ClassConstructor (allArgs, Some cThis, None, tsSig))

            // implementation
            if output <> O.TypeScriptDeclaration then
                let cBody =
                    let origIndices = Dictionary()
                    [ 
                        for (beforeCtor, name, args, chainedCtor, bodyRest, _) in orderedCtorData do
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
                                        // code before calling chained constructor
                                        match beforeCtor with
                                        | Some bcOpt ->
                                            yield bcOpt
                                        | _ -> ()
                                        // redirect to chained constructor
                                        yield VarSetStatement(index, Value (String ccName))
                                        for v, vv in Seq.zip cArgs ccArgs do
                                            yield VarSetStatement(v, vv)    
                                    ]
                                yield If((Var index) ^== Value (String name), Block setters, Empty)
                            | _ -> ()
                        for (_, name, args, chainedCtor, bodyRest, _) in orderedCtorData do
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
                        for (_, name, args, _, bodyRest, _) in orderedCtorData do
                            match origIndices.TryGetValue name, bodyRest with
                            | (true, oi), Some br ->
                                yield If(Var oi, br, Empty)
                            | _ -> ()
                    ]

                let allArgs = List.map (fun x -> x, Modifiers.None) (index :: cArgs)
                members.Add (ClassConstructor (allArgs, Some cThis, implStOpt (fun () -> Block cBody), TSType.Any))
        
        let mutable isFSharpType = false

        match ct with
        | M.FSharpUnionInfo u when Option.isNone c.Type ->         
            //let tags() = u.Cases |> List.mapi (fun i c -> c.Name, MemberKind.Simple, Value (Int i)) |> Object
            //addStatement <| export false (VarDeclaration(Id.New("Tags", mut = false, str = true), implExpr tags))
            isFSharpType <- true

            if output <> O.JavaScript then
                
                let ucTypes = ResizeArray()
                let ucNames = HashSet()
                for uci, uc in u.Cases |> Seq.indexed do
                    
                    let tagMem() = 
                        ClassProperty(propInfo false false false, "$", TSType.Basic (string uci), None)
                    let ucName = 
                        if StandardLibNames.Set.Contains uc.Name then
                            Resolve.getRenamed (uc.Name + "_1") ucNames
                        else
                            Resolve.getRenamed uc.Name ucNames
                    let ucId() =
                        Id.New(ucName, str = true)  
                    match uc.Kind with
                    | M.NormalFSharpUnionCase fs ->
                        let ucmems =
                            fs |> List.mapi (fun i f ->
                                let fTyp =
                                    if output = O.JavaScript then TSType.Any else
                                    tsTypeOf gsArr f.UnionFieldType
                                ClassProperty(propInfo false false false, "$" + string i, fTyp, None)
                            )
                        addStatement <| export false ( 
                            Interface(ucId(), [], tagMem() :: ucmems, cgen)
                        )
                        ucTypes.Add(TSType.Basic ucName |> addGenerics cgen)
                    | M.ConstantFSharpUnionCase v -> 
                        ucTypes.Add(TSType.Basic v.TSType)
                    | M.SingletonFSharpUnionCase ->
                        addStatement <| export false (
                            Interface(ucId(), [], [ tagMem() ], cgen)
                        )
                        ucTypes.Add(TSType.Basic ucName |> addGenerics cgen)

                if not (TypeTranslator.CustomTranslations.ContainsKey typ) then
                    let t =
                        if c.HasWSPrototype then
                            TSType.Intersection [ thisTSTypeOrUnion.Value; TSType.Union (List.ofSeq ucTypes) ]
                        else
                            TSType.Union (List.ofSeq ucTypes)
                    addStatement <| export false (
                        Alias(classId_T, cgen, t)
                    )

        | M.FSharpRecordInfo r when Option.isNone c.Type ->     
            isFSharpType <- true

            if not c.HasWSPrototype && output <> O.JavaScript then
                let rmems =
                    r |> List.map (fun f ->
                        let fTyp =
                            if output = O.JavaScript then TSType.Any else
                            tsTypeOf gsArr f.RecordFieldType
                        ClassProperty(propInfo false false f.Optional, f.JSName, fTyp, None)
                    )
                addStatement <| export true (
                    Interface(classId, [], rmems, cgen)
                )

        | _ -> ()
        //| _ ->
        //    if c.HasWSPrototype then
        //        packageCtor addr <| ClassExpr(None, baseType, List.ofSeq members) 

        hasStaticConstructor <- 
            if output <> O.TypeScriptDeclaration then
                match c.StaticConstructor with
                | Some st -> 
                    members.Add <| ClassStatic(staticThisTransformer.TransformStatement st)
                    true
                | _ ->
                    false
            else
                false

        let packageLazyClass classDecl classExpr =
            if isSingleType then
                let withoutLazy =
                    classDecl
                    |> bTr().TransformStatement 
                    |> SubstituteVar(outerClassId, Var classId).TransformStatement
                    |> exportWithBundleSupport true typ None currentClassAddr outerClassId
                let withLazy =
                    Block [
                        VarDeclaration(outerClassId, bTr().TransformExpression (JSRuntime.Lazy classExpr))
                        exportWithBundleSupport true typ None currentClassAddr outerClassId (ExprStatement(Var outerClassId))              
                    ]
                addStatement <| LazyClass(withoutLazy, withLazy)    
            else
                addStatement <| VarDeclaration(outerClassId, bTr().TransformExpression (JSRuntime.Lazy classExpr))
                addStatement <| exportWithBundleSupport true typ None currentClassAddr outerClassId (ExprStatement(Var outerClassId))                

        let packageClass classDecl = 
            let trClassDecl =
                classDecl
                |> bTr().TransformStatement 
                |> SubstituteVar(outerClassId, Var classId).TransformStatement
            addStatement <| exportWithBundleSupport true typ None currentClassAddr outerClassId trClassDecl      

        if c.HasWSPrototype || members.Count > 0 then
            let classExpr setInstance = 
                ClassExpr(Some classId, baseType, 
                    ClassStatic (VarSetStatement(outerClassId, Cast(TSType.Any, setInstance(JSThis)))) 
                    :: List.ofSeq members)
            let implements =
                if output = O.JavaScript then [] else
                c.Implements |> List.map (tsTypeOfConcrete gsArr)
            let classDecl = Class(classId, baseType, implements, List.ofSeq members, cgen, bgen)
            match baseType with
            | Some b ->
                let needsLazy = 
                    output <> O.TypeScriptDeclaration
                    && (
                        if isSingleType then 
                            Option.isNone c.Type
                        else
                            lazyClasses.Contains typ
                    ) 
                if needsLazy then
                    packageLazyClass classDecl <| fun i ->
                        if isObjBase then
                            classExpr i
                        else
                            Sequential [
                                JSRuntime.Force(b)
                                classExpr i
                            ]
                else
                    packageClass classDecl
            | None ->
                let needsLazy = 
                    output <> O.TypeScriptDeclaration 
                    && (
                        if isSingleType then
                            c.HasWSPrototype && Option.isNone c.Type && typ <> Definitions.Object && not isFSharpType
                        else
                            lazyClasses.Contains typ
                    ) 
                if needsLazy then
                    packageLazyClass classDecl classExpr
                else
                    packageClass classDecl
    
    let packageInterface (typ: TypeDefinition) (i: M.InterfaceInfo) =
        match classRes.TryGetValue typ with 
        | false, _ -> ()
        | true, (currentClassAddr, classId, outerClassId) ->

        let className = classId.Name.Value
        let igen = List.length i.Generics
        let gen = getGenerics 0 i.Generics
        let gsArr = Array.ofList i.Generics
        let inlineMethods =
            match current.Classes.TryFind(typ) with
            | Some (_, _, Some c) -> 
                c.Methods.Keys |> Array.ofSeq
            | _ ->
                [||]
        if output <> O.JavaScript then
            let imems =
                i.Methods |> Seq.choose (fun (KeyValue (m, (n, k, gc))) ->
                    // if method has a default implementation, it might not be declared on types using it in JS
                    if inlineMethods |> Array.contains m then None else

                    let gsArr = Array.append gsArr (Array.ofList gc)
                    let args, argTypes =
                        m.Value.Parameters |> List.mapi (fun i p ->
                            let fTyp =
                                if output = O.JavaScript then TSType.Any else
                                tsTypeOf gsArr p
                            Id.New(string ('a' + char i), mut = false, str = true), fTyp
                        ) |> List.unzip
                    let signature = 
                        if output = O.JavaScript then TSType.Any else
                        TSType.Lambda(argTypes, tsTypeOf gsArr m.Value.ReturnType)
                    let info = 
                        {
                            IsStatic = false
                            IsPrivate = false
                            IsAbstract = false
                            Kind = k
                        }
                    ClassMethod(info, n, args, None, None, signature |> addGenerics (getGenerics igen gc)) |> Some   
                ) |> List.ofSeq

            let extends =
                if output = O.JavaScript then [] else
                i.Extends |> List.map (tsTypeOfConcrete gsArr)

            addStatement <| export true (
                Interface(classId, extends, imems, gen)
            )

    for typ in orderedTypes do
        match current.Classes.TryFind(typ) with
        | None -> ()
        | Some (a, ct, None) -> 
            if output <> O.JavaScript then
                let cgen = List.init typ.Value.GenericLength (fun _ -> M.GenericParam.None)
                packageClass typ a ct { M.ClassInfo.None with Generics = cgen }
        | Some (a, ct, Some c) ->
            packageClass typ a ct c

        match current.Interfaces.TryFind(typ) with
        | Some i ->
            packageInterface typ i
        | None -> ()

    if output <> O.TypeScriptDeclaration then
        match content with
        | Bundle(_, (OnLoadIfExists | ForceOnLoad), Some ep) ->
            addStatement <| ExprStatement (bodyTransformer([||]).TransformExpression(JSRuntime.OnLoad (Function([], None, None, ep))))
        | Bundle(_, (ForceImmediate | LibraryBundle), Some ep) ->
            statements.Add (bodyTransformer([||]).TransformStatement(ep))
        | Bundle(_, (ForceOnLoad | ForceImmediate), None) ->
            failwith "Missing entry point or export. Add SPAEntryPoint attribute to a static method without arguments, or JavaScriptExport on types/methods to expose them."
        | _ -> ()
        
    if statements.Count = 0 then 
        {
            Statements = []
            BundleExports = bundleExports 
            Imports = Seq.empty
            BaseClass = None
            HasStaticConstructor = false
        }
    else
        if jsUsed.Count > 0 then
            let namedImports = 
                jsUsed |> Seq.map (fun n -> n, Id.New(n, str = true)) |> List.ofSeq
            declarations.Add(Import(None, None, namedImports, ""))

        let isSPABundleType =
            match content with
            | Bundle (_, (OnLoadIfExists | ForceOnLoad | ForceImmediate), _) -> true
            | _ -> false

        for KeyValue(m, i) in imports do
            if not (currentScope.Contains(m)) then
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
                let ext =
                    match output with
                    | O.JavaScript -> ".js"
                    | _ -> ""
                let fromModule =
                    if m.Assembly = "" then
                        m.Name
                    elif isSPABundleType then
                        "./" + m.Assembly + "/" + m.Name + ext  
                    elif flattened || m.Assembly = asmName then
                        "./" + m.Name + ext  
                    else
                        "../" + m.Assembly + "/" + m.Name + ext  
                declarations.Add(Import(defaultImport, fullImport, namedImports, fromModule))
        {
            Statements = List.ofSeq (Seq.concat [ declarations; statements ])
            BundleExports = bundleExports
            Imports = imports.Keys
            BaseClass = baseClass
            HasStaticConstructor = hasStaticConstructor
        }

let makeFileName (typ: TypeDefinition) =
    typ.Value.FullName.Replace('+', '.')

let analyzeLazyClasses (pkgs: ResizeArray<string * TypeDefinition * PackageTypeResults>) =
    
    let pkgLookup = Dictionary()
    pkgs |> Seq.iteri (fun i (fn, td, pkg) ->
        let m = { Assembly = td.Value.Assembly; Name = makeFileName td }
        pkgLookup.Add(m, i) 
    )
    
    let pkgLookupByType = Dictionary()
    pkgs |> Seq.iteri (fun i (fn, td, pkg) ->
        pkgLookupByType.Add(td, i) 
    )
    
    let pkgsToProcess =  HashSet()
    pkgs |> Seq.iteri (fun i _ ->
        pkgsToProcess.Add(i) |> ignore 
    )

    let neededLazy = HashSet()
    let rec processPkg (i: int) =
        pkgsToProcess.Remove(i) |> ignore
        
        let fn, td, pkg = pkgs.[i]
        let baseNeedsLazy() =
            match pkg.BaseClass with
            | Some bc ->
                match pkgLookupByType.TryGetValue bc with
                | true, j -> 
                    if pkgsToProcess.Contains j then
                        processPkg j
                    neededLazy.Contains j
                | _ -> false
            | _ -> false
        let hasCircularDeps() =
            let deps = HashSet()
            let mutable circularDepFound = false
            let rec visitImport imp =
                match pkgLookup.TryGetValue imp with
                | true, j ->
                    if i = j then
                        circularDepFound <- true
                    else
                        if deps.Add j then
                            let _, _, pkgj = pkgs.[j]
                            for imp in pkgj.Imports do
                                if not circularDepFound then
                                    visitImport imp
                | _ -> ()
            for imp in pkg.Imports do
                if not circularDepFound then
                    visitImport imp
            circularDepFound
        let needsLazy = pkg.HasStaticConstructor || baseNeedsLazy() || hasCircularDeps()
        if needsLazy then
            neededLazy.Add(i) |> ignore
        let tr = LazyClassTransformer(needsLazy)
        let trPkg =
            { pkg with
                Statements = pkg.Statements |> List.map tr.TransformStatement
            }
        pkgs.[i] <- fn, td, trPkg
    while pkgsToProcess.Count > 0 do
        let i = (pkgsToProcess |> Seq.head)
        processPkg i

let jsInteropClasses =
    let coreTyp t =
        TypeDefinition{
            Assembly = "WebSharper.Core"
            FullName = t
        }
    [
        yield coreTyp "WebSharper.JavaScript.Optional`1"
        yield coreTyp "WebSharper.JavaScript.ItemOrArray`1"
        for i = 1 to 7 do
            yield coreTyp ("WebSharper.JavaScript.Union`" + string i)
    ]

let packageAssembly output (refMeta: M.Info) (current: M.Info) asmName flattened entryPoint entryPointStyle =
    let pkgs = ResizeArray()
    let classes = HashSet(current.Classes.Keys)
    let pkgTyp (typ: TypeDefinition) =
        let pkg = packageType output refMeta current asmName flattened (SingleType typ)
        if not (List.isEmpty pkg.Statements) then
            pkgs.Add(makeFileName typ, typ, pkg)
    for typ in jsInteropClasses do
        classes.Remove(typ) |> ignore
    for typ in current.Interfaces.Keys do
        classes.Remove(typ) |> ignore
        pkgTyp typ
    for typ in classes do
        pkgTyp typ
    if Option.isSome entryPoint then
        let epTyp = TypeDefinition { Assembly = asmName; FullName = "$EntryPoint" }     
        let pkg = packageType output refMeta current asmName flattened (Bundle ([| epTyp |], entryPointStyle, entryPoint))
        pkgs.Add("$EntryPoint", epTyp, pkg)
    if output <> O.TypeScriptDeclaration then
        analyzeLazyClasses pkgs
    pkgs |> Seq.map (fun (fn, _, pkg) -> fn, pkg.Statements) |> Array.ofSeq

let bundleAssembly output (refMeta: M.Info) (current: M.Info) asmName entryPoint entryPointStyle =
    let types =
        Seq.append current.Interfaces.Keys current.Classes.Keys
        |> Seq.distinct
        |> Array.ofSeq

    let pkg = packageType output refMeta current asmName false (Bundle (types, entryPointStyle, entryPoint))
    pkg.Statements

let getImportedModules (pkg: Statement list) =
    pkg
    |> Seq.choose (
        function
        | Import(_, _, _, m) -> Some m
        | _ -> None
    )
    |> Seq.distinct
    |> Seq.toList

let addLoadedModules (urls: string list) scriptBase skipAssemblyDir (pkg: Statement list) =
    if List.isEmpty urls then
        let start = Id.New("Start")
        
        let runtimeLoc = if skipAssemblyDir then "../"  else "./"
        
        [
            Import (None, None, ["Start", start], runtimeLoc + "WebSharper.Core.JavaScript/Runtime.js")
            yield! pkg
            ExprStatement(ApplAny(Var start, []))
        ]
    else
        let runtime = Id.New("Runtime")
        let loadScript = Id.New("LoadScript")
        let start = Id.New("Start")
        
        let runtimeLoc = if skipAssemblyDir then "../"  else "./"
        
        [
            Import (Some runtime, None, ["LoadScript", loadScript; "Start", start], runtimeLoc + "WebSharper.Core.JavaScript/Runtime.js")
            ExprStatement(ItemSet(Var runtime, Value (String "ScriptBasePath"), Value (String scriptBase)))         
            if skipAssemblyDir then 
                ExprStatement(ItemSet(Var runtime, Value (String "ScriptSkipAssemblyDir"), Value (Bool true)))
            for url in urls do 
                ExprStatement(ApplAny(Var loadScript, [ Value (String url) ]))
            yield! pkg
            ExprStatement(ApplAny(Var start, []))
        ]

let transformProgramWithJSX output pref statements =
    statements |> JavaScriptWriter.transformProgram output pref

let programToString pref (getWriter: unit -> WebSharper.Core.JavaScript.Writer.CodeWriter) program isJSX =
    let writer = getWriter()
    WebSharper.Core.JavaScript.Writer.WriteProgram pref writer program
    writer.GetCodeFile(), writer.GetMapFile(), isJSX

let packageEntryPointReexport (runtimeMeta: M.Info) =

    let allBundleContent = ResizeArray()
    let addresses = ResizeArray()

    for qi in runtimeMeta.Quotations.Values do
        allBundleContent.Add(qi.TypeDefinition, Some qi.Method)
    
    for KeyValue((td, m), _) in runtimeMeta.QuotedMethods do
        allBundleContent.Add(td, Some m)

    for wc in runtimeMeta.WebControls do
        allBundleContent.Add(wc.Key.TypeDefinition, None)

    for td, m in allBundleContent do
        match runtimeMeta.Classes.TryFind td with
        | Some (addr, _, Some cls) ->
            match m with 
            | Some m ->
                match cls.Methods.TryFind m with
                | Some mi -> 
                    match mi.CompiledForm with
                    | M.Static (name, _, _) -> addresses.Add(addr)
                    | M.Func (name, _) -> addresses.Add(addr.Func(name))
                    | M.GlobalFunc (faddr, _) -> addresses.Add(faddr) 
                    | _ -> ()
                | _ -> ()
            | _ ->
                addresses.Add(addr)
        | _ -> ()

    let addressMap = Dictionary()

    addressMap.Add("Runtime", Address.RuntimeAddr [ "default" ])

    for a in Seq.distinct addresses do
        addressMap |> Resolve.getRenamedInDict a.Address.Head a |> ignore

    let revAddressMap = addressMap |> Dict.swap

    let finalAddrMap = Dictionary()

    let rootJs =
        revAddressMap |> Seq.groupBy (fun kv -> kv.Key.Module)
        |> Seq.map (fun (m, addrs) ->
            let namedImports = ResizeArray()
            for KeyValue(a, n) in addrs do
                match a.Address |> List.rev |> List.head with
                | "default" -> 
                    let newName = 
                        match m with
                        | JavaScriptModule m -> 
                            m.Name.Replace('.', '_').Replace('`', '_')
                        | DotNetType m -> 
                            (m.Name.Split([| '/'; '.' |]) |> Array.last).Split('`') |> Array.head
                        | _ -> "x"
                    let x = Id.New newName
                    namedImports.Add("default", x)
                    finalAddrMap.Add(a, x)
                | i ->
                    let x = Id.New n
                    namedImports.Add(i, x)   
                    finalAddrMap.Add(a, x)
            let moduleName =
                match m with
                | JavaScriptModule m 
                | DotNetType m -> 
                    "../" + m.Assembly + "/" + m.Name + ".js"
                | _ -> ""
            ExportDecl (false, Import(None, None, List.ofSeq namedImports, moduleName))
        )
        |> List.ofSeq
    
    rootJs, finalAddrMap

let packageLibraryBundle (current: M.Info) (jsExport: JsExport list) output =
    let exportTypes = HashSet()
    let exportFunctions = Dictionary()
    for e in jsExport do
        match e with
        | ExportNode (M.TypeNode t | M.ConstructorNode (t, _)) -> 
            exportTypes.Add t |> ignore
        | ExportNode (M.MethodNode (t, m)) -> 
            match current.Classes.TryFind t with
            | Some (_, _, Some c) ->
                match c.Methods.TryFind m with
                | Some mi -> 
                    match mi.CompiledForm with
                    | M.Func _
                    | M.GlobalFunc _ -> 
                        let fs =
                            match exportFunctions.TryGetValue t with
                            | false, _ ->
                                let fs = ResizeArray()
                                exportFunctions.Add(t, fs)
                                fs
                            | true, fs ->
                                fs
                        fs.Add(mi.CompiledForm)
                    | _ ->
                        exportTypes.Add t |> ignore
                | _ -> ()
            | _ -> ()
        | _ -> ()
    let reexports = ResizeArray()
    for t in exportTypes do
        match current.Classes.TryFind t with
        | Some (a, _, Some c) when c.HasWSPrototype ->
            let newName = 
                match a.Module with
                | JavaScriptModule m -> 
                    m.Name.Replace('.', '_').Replace('`', '_')
                | DotNetType m -> 
                    (m.Name.Split([| '/'; '.' |]) |> Array.last).Split('`') |> Array.head
                | _ -> "x"
            let x = Id.New newName
            let moduleName =
                match a.Module with
                | JavaScriptModule m 
                | DotNetType m -> 
                    "./" + m.Name + (if output = O.JavaScript then ".js" else "")
                | _ -> ""
            reexports.Add <| ExportDecl (false, Import(None, None, ["default", x], moduleName))
        | _ -> ()
    for KeyValue(t, fs) in exportFunctions do
        match current.Classes.TryFind t with
        | Some (a, _, Some c) ->
            let moduleName =
                match a.Module with
                | JavaScriptModule m 
                | DotNetType m -> 
                    "./" + m.Name + (if output = O.JavaScript then ".js" else "")
                | _ -> ""
            let namedImports = 
                fs |> Seq.choose (fun f ->
                    match f with  
                    | M.Func (n, _) ->
                        let x = Id.New n
                        Some (n, x)
                    | _ -> None
                ) |> List.ofSeq
            let newName = 
                match a.Module with
                | JavaScriptModule m -> 
                    m.Name.Replace('.', '_').Replace('`', '_')
                | DotNetType m -> 
                    (m.Name.Split([| '/'; '.' |]) |> Array.last).Split('`') |> Array.head
                | _ -> "x"
            let x = Id.New newName
            reexports.Add <| ExportDecl (false, Import(None, None, namedImports, moduleName))
        | _ -> ()
    
    reexports |> List.ofSeq

let packageEntryPoint (runtimeMeta: M.Info) (graph: DependencyGraph.Graph) asmName output =

    let all = ResizeArray()
    let bundles = Dictionary()
    bundles.Add("all", all) 
    
    let addToBundles names item =
        all.Add(item)
        for n in names do
            match bundles.TryFind(n) with
            | None -> 
                let b = ResizeArray()
                b.Add(item)
                bundles.Add(n, b)
            | Some b ->
                b.Add(item)

    for qi in runtimeMeta.Quotations.Values do
        (qi.TypeDefinition, qi.Method) |> addToBundles qi.PreBundles
    
    for qm in runtimeMeta.QuotedMethods do
        qm.Key |> addToBundles qm.Value

    let iControlBody =
        TypeDefinition {
            Assembly = "WebSharper.StdLib"
            FullName = "WebSharper.IControlBody"
        }
    let webControl =    
        TypeDefinition {
            Assembly = "WebSharper.Web"
            FullName = "WebSharper.Web.Control"
        }
    let getBody =
        Method {
            MethodName = "get_Body"
            Parameters = []
            ReturnType = ConcreteType (NonGeneric iControlBody)
            Generics = 0
        } 
    let domNode = 
        TypeDefinition {
            Assembly = "WebSharper.JavaScript"
            FullName = "WebSharper.JavaScript.Dom.Node"
        }
    let replaceInDom =
        Method {
            MethodName = "ReplaceInDom"
            Parameters = [ NonGenericType domNode ]
            ReturnType = VoidType
            Generics = 0
        }

    for wc in runtimeMeta.WebControls do
        (wc.Key.TypeDefinition, getBody) |> addToBundles wc.Value    

    let results = ResizeArray()

    for b in bundles do
        let nodes =
            seq {
                yield M.AbstractMethodNode (iControlBody, replaceInDom)
                yield M.AbstractMethodNode (webControl, getBody)
                for td, m in b.Value do
                    if m = getBody then
                        yield M.TypeNode td
                    else
                        yield M.MethodNode (td, m)    
            }
            |> graph.GetDependencies
         
        let trimmed = trimMetadata runtimeMeta nodes 

        let types =
            Seq.append trimmed.Interfaces.Keys trimmed.Classes.Keys
            |> Seq.distinct
            |> Array.ofSeq

        let exportedTypes = Dictionary<TypeDefinition, ISet<Method>>()
        for td, m in b.Value do
            if m = getBody then    
                exportedTypes[td] <- null
            else
                match exportedTypes.TryGetValue(td) with
                | true, s -> s.Add(m) |> ignore
                | false, _ ->
                    let s = HashSet()
                    s.Add(m) |> ignore
                    exportedTypes.Add(td, s)

        let pkg = packageType output trimmed trimmed asmName false (Bundle (types, SiteletBundle exportedTypes, None)) 
        results.Add(b.Key, (pkg.Statements, pkg.BundleExports))

    results
    