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
                    let trBody = Block [ VarDeclaration(t,  JSThis); trBody ]
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
                let trBody = Block [ VarDeclaration(t,  JSThis); trBody ]
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
                let trBody = trBody |> Option.map (fun b -> Block [ VarDeclaration(t,  JSThis); b ])
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
                let trBody = trBody |> Option.map (fun b -> Block [ VarDeclaration(t,  JSThis); b ])
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

type EntryPointStyle =
    | OnLoadIfExists
    | ForceOnLoad
    | ForceImmediate

type PackageContent =
    | SingleType of TypeDefinition
    | Bundle of TypeDefinition [] * EntryPointStyle * Statement option

    with 
        member this.Types = 
            match this with
            | SingleType typ -> [| typ |]
            | Bundle (typs, _, _) -> typs

let packageType (output: O) (refMeta: M.Info) (current: M.Info) asmName (content: PackageContent) =
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

    for typ in content.Types do
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
        if not isUnionCase then
            for (classAddr, classCodeRes, isMainAddr) in classAddrs do
                if isMainAddr && output <> O.JavaScript && classAddr.Address <> [ "default" ] then
                    // add type export
                    let typeAddr = { classAddr with Address = [ "default" ] }
                    addresses.Add(typeAddr, Var classId)
                
                if isMainAddr && not (addresses.ContainsKey classAddr) then
                    addresses.Add(classAddr, Var outerClassId)
                currentScope.Add(classCodeRes) |> ignore
                if isMainAddr && not (classRes.ContainsKey typ) then
                    classRes.Add(typ, (classAddr, classId, outerClassId))
            if classAddrs.Count = 0 then
                // not represented as JS class
                classRes.Add(typ, (Address.Global(), classId, outerClassId))

    let export isDefault statement =
        match content with
        | SingleType _ -> ExportDecl(isDefault, statement)
        | Bundle _ -> 
            match statement with
            | ExprStatement(Var _) -> Empty
            | _ -> statement

    addresses.Add(Address.Lib "import", Var (Id.Import()))
    let safeObject expr = Binary(expr, BinaryOperator.``||``, Object []) 
        
    let getOrImportAddress (address: Address) =
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
                | DotNetType m ->
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
            | GlobalAccess { Module = JavaScriptModule _ | DotNetType _ } ->
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
        | DotNetType _ ->
            let a = if a.Address.IsEmpty then { a with Address = [ "default" ] } else a
            match getOrImportAddress a with
            | GlobalAccess { Module = ImportedModule i; Address = a } ->
                TSType.Imported(i, a |> List.rev)
            | Var i ->
                match classIdToOuter.Value.TryGetValue(i) with
                | true, oi ->
                    match oi.Name with
                    | Some n ->
                        TSType.Named([ n ])
                    | _ ->
                        TSType.Named t    
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
        t.ResolveModule (fun m ->
            match getOrImportAddress ({ Module = m; Address = [] }) with 
            | Var v -> Some v
            | GlobalAccess { Module = ImportedModule v } -> Some v
            | _ -> None
        )

    let bodyTransformer gsArr =
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
                | I.GlobalAccess a, I.Value (Literal.String n) when a.Address.IsEmpty ->
                    a.Sub(n) |> getOrImportAddress
                | _ ->
                    base.TransformItemGet(e, i, p)

            override this.TransformItemSet(e, i, v) =
                match e, i with
                | I.GlobalAccess a, I.Value (Literal.String n) when a.Address.IsEmpty ->
                    match a.Sub(n) |> getOrImportAddress with
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

    //let package a expr =
    //    let o, x = getFieldAddress a
    //    addStatement <| ExprStatement (ItemSet (o, x, expr))    

    let implExpr, implSt, implStOpt, implExprOpt =
        if output = O.TypeScriptDeclaration then 
            (fun _ -> Undefined), (fun _ -> Empty), (fun _ -> None), (fun _ -> None)
        else
            (fun getImpl -> getImpl()), (fun getImpl -> getImpl()), (fun getImpl -> Some (getImpl())), (fun getImpl -> Some (getImpl()))
            
    let rec withoutMacros info =
        match info with
        | M.Macro (_, _, Some fb) -> withoutMacros fb
        | _ -> info 

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
                        JSThis
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
         
        let mem (m: Method) info gc opts intfGen body =
            
            let gsArr = Array.append gsArr (Array.ofList gc)
            let bTr() = bodyTransformer(gsArr)   
            let getSignature isInstToStatic =         
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
                            match intfGen with 
                            | None -> 
                                m.Value.Parameters
                                , ret |> Option.defaultValue m.Value.ReturnType
                            | Some ig -> 
                                try
                                    m.Value.Parameters |> List.map (fun p -> p.SubstituteGenerics ig) 
                                    , m.Value.ReturnType.SubstituteGenerics ig 
                                with _ ->
                                    failwithf "failed to substitute interface generics: %A to %A" ig m
                        let pts =
                            if isInstToStatic then
                                tsTypeOf gsArr (NonGenericType typ) :: (typeOfParams opts gsArr p)
                            else typeOfParams opts gsArr p
                        TSType.Lambda(pts, tsTypeOf gsArr r)
                | _ ->
                    tsTypeOf [||] m.Value.ReturnType
            let mgen = getGenerics cgenl gc
            
            let func fromInst addr =
                let f = 
                    match getOrImportAddress addr with
                    | Var f -> 
                        f
                    | a ->
                        failwithf "Func var lookup failed for %A, got %A while writing type %A currentScope=%A" addr a typ (Array.ofSeq currentScope)
                match IgnoreExprSourcePos body with
                | Function (args, thisVar, _, b) ->
                    let f, args =
                        if output = O.JavaScript then f, args else
                        match getSignature fromInst with
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
                    addStatement <| export false (FuncDeclaration(f, args, thisVar, implSt(fun () -> bTr().TransformStatement b), cgen @ mgen))
                | e ->
                    let f = 
                        if output = O.JavaScript then f else
                        f.WithType(Some (TSType (getSignature fromInst)))
                    addStatement <| export false (VarDeclaration(f, implExpr(fun () -> bTr().TransformExpression e)))
            
            match withoutMacros info with
            | M.Instance (mname, mkind) ->
                match IgnoreExprSourcePos body with
                | Function (args, thisVar, _, b) ->
                    let info = 
                        {
                            IsStatic = false
                            IsPrivate = false // TODO
                            Kind = mkind
                        }
                    members.Add <| ClassMethod(info, mname, args, thisVar, implStOpt (fun () -> b), getSignature false |> addGenerics mgen)
                | _ -> ()       
            | M.Static (mname, fromInst, mkind) ->
                match IgnoreExprSourcePos body with
                | Function (args, thisVar, ret, b) ->
                    let info = 
                        {
                            IsStatic = true
                            IsPrivate = false // TODO
                            Kind = mkind
                        }
                    members.Add <| ClassMethod(info, mname, args, thisVar, implStOpt (fun () -> staticThisTransformer.TransformStatement b), getSignature fromInst |> addGenerics (cgen @ mgen))
                | _ ->
                    let info = 
                        {
                            IsStatic = true
                            IsPrivate = false // TODO
                            IsOptional = false
                        }
                    members.Add <| ClassProperty(info, mname, getSignature false |> addGenerics (cgen @ mgen), implExprOpt (fun () -> body))
            | M.Func (fname, fromInst) ->
                func fromInst (currentClassAddr.Func(fname))
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
                    tsTypeOf gsArr f.Type
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

        for KeyValue((i, m), mi) in c.Implementations do
            let intfGen, mParam = 
                match interfaceInfos.Value.TryGetValue i with
                | true, (intf, intfGen) ->
                    let _, _, mg = intf.Methods.[m]
                    match m.Value.Generics with
                    | 0 -> intfGen, mg
                    | mgen -> Array.append intfGen (Array.init mgen (fun i -> TypeParameter (cgenl + i))), mg
                | _ ->
                    match baseClassInfos.Value.TryGetValue i with
                    | true, (cls, clsGen) ->
                        match m.Value.Generics with
                        | 0 -> clsGen
                        | mgen -> Array.append clsGen (Array.init mgen (fun i -> TypeParameter (cgenl + i)))
                        , 
                        cls.Methods.[m].Generics
                    | _ ->
                        if i = typ then
                            Array.init (cgenl + m.Value.Generics) (fun i -> TypeParameter i), []
                        else
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
                    let info =
                        {
                            IsStatic = true
                            IsPrivate = false
                            Kind = MemberKind.Simple
                        }
                    let ctorBody() =
                        Return (New (JSThis, [], Value (String name) :: (args |> List.map Var)))
                    members.Add (ClassMethod(info, name, args, thisVar, implStOpt ctorBody, getSignature false None |> addGenerics cgen))
                | _ ->
                    failwithf "Invalid form for translated constructor"
            | M.Func (name, _) ->
                match ct.Expression with 
                | Function (args, thisVar, _, b) ->  
                    let f = 
                        match getOrImportAddress (currentClassAddr.Func(name)) with
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
                            Kind = kind
                        }
                    members.Add (ClassMethod(info, name, args, thisVar, implStOpt (fun () -> b), getSignature false None |> addGenerics cgen))
                | _ ->
                    failwithf "Invalid form for translated constructor"
            | _ -> ()                            

        let baseType, isObjBase =
            let tryFindClass c =
                match refMeta.Classes.TryFind c with
                | Some _ as res -> 
                    if refMeta.Interfaces.ContainsKey c then None else res
                | _ -> 
                    if current.Interfaces.ContainsKey c then None else current.Classes.TryFind c
            match c.BaseClass |> Option.bind (fun b -> tryFindClass b.Entity) with
            | Some (ba, _, _) -> Some (getOrImportAddress ba), c.BaseClass.Value.Entity = Definitions.Object
            | _ -> None, false

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
                // TODO what if not at start
                let chainedCtor, bodyRest =
                    match body with
                    | I.Block (I.ExprStatement (I.Application(I.JSThis, I.Value(String baseName) :: baseArgs, _)) :: r) ->
                        Some (baseName, baseArgs), Some (Block r)                                        
                    | I.ExprStatement (I.Application(I.JSThis, I.Value(String baseName) :: baseArgs, _)) ->
                        Some (baseName, baseArgs), None
                    | I.ExprStatement (I.Sequential(I.Application(I.JSThis, I.Value(String baseName) :: baseArgs, _) :: r)) ->
                        Some (baseName, baseArgs), Some (ExprStatement (Sequential r))
                    
                    | I.ExprStatement (I.Application(I.Base, [], _)) ->
                        if Option.isSome baseType then
                            None, Some body
                        else
                            None, None
                    | _ -> 
                        None, Some body
                ctorData.Add(name, (args, chainedCtor, bodyRest, tsSig))

            // calculate order of constructors so chained constructors work,
            // adding those with no dependencies first, so we will need to reverse in the end
            let addedCtors = HashSet()
            let orderedCtorData = ResizeArray<string * Id list * (string * Expression list) option * Statement option * TSType>()
            while ctorData.Count > 0 do
                for KeyValue(name, (args, chainedCtor, bodyRest, tsSig)) in ctorData |> Array.ofSeq do
                    let okToAdd =
                        match chainedCtor with
                        | None -> true
                        | Some (ccName, _) -> addedCtors.Contains ccName
                    if okToAdd then
                        addedCtors.Add name |> ignore
                        //let chainedCtorArgsNum = match chainedCtor with Some (_, a) -> a.Length | None -> 0
                        orderedCtorData.Add (name, args, 
                            chainedCtor |> Option.map (fun (n, a) -> n, a), 
                            bodyRest, tsSig
                        )
                        ctorData.Remove name |> ignore
            orderedCtorData.Reverse()
            
            if output <> O.TypeScriptDeclaration then
            
                let cBody =
                    let origIndices = Dictionary()
                    [ 
                        for (name, args, chainedCtor, bodyRest, _) in orderedCtorData do
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
                        for (name, args, chainedCtor, bodyRest, _) in orderedCtorData do
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
                        for (name, args, _, bodyRest, _) in orderedCtorData do
                            match origIndices.TryGetValue name, bodyRest with
                            | (true, oi), Some br ->
                                yield If(Var oi, br, Empty)
                            | _ -> ()
                    ]

                let allArgs = List.map (fun x -> x, Modifiers.None) (index :: cArgs)
                members.Add (ClassConstructor (allArgs, Some cThis, implStOpt (fun () -> Block cBody), TSType.Any))
        
            else
                for (_, args, _, _, tsSig) in orderedCtorData do
                    let allArgs = List.map (fun x -> x, Modifiers.None) (index :: args)
                    members.Add (ClassConstructor (allArgs, Some cThis, None, tsSig))

        let mutable isFSharpType = false

        match ct with
        | M.FSharpUnionInfo u when Option.isNone c.Type ->         
            //let tags() = u.Cases |> List.mapi (fun i c -> c.Name, MemberKind.Simple, Value (Int i)) |> Object
            //addStatement <| export false (VarDeclaration(Id.New("Tags", mut = false, str = true), implExpr tags))
            isFSharpType <- true

            if output <> O.JavaScript then
                
                let ucTypes = ResizeArray()
                for uci, uc in u.Cases |> Seq.indexed do
                    
                    let tagMem() = 
                        ClassProperty(propInfo false false false, "$", TSType.Basic (string uci), None)
                    let ucId() =
                        Id.New(uc.Name, str = true)  
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
                        ucTypes.Add(TSType.Basic uc.Name |> addGenerics cgen)
                    | M.ConstantFSharpUnionCase v -> 
                        ucTypes.Add(TSType.Basic v.TSType)
                    | M.SingletonFSharpUnionCase ->
                        addStatement <| export false (
                            Interface(ucId(), [], [ tagMem() ], cgen)
                        )
                        ucTypes.Add(TSType.Basic uc.Name |> addGenerics cgen)

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

        if output <> O.TypeScriptDeclaration then
            match c.StaticConstructor with
            | Some st -> 
                members.Add <| ClassStatic(staticThisTransformer.TransformStatement st)
            | _ -> ()

        let packageLazyClass classExpr =
            addStatement <| VarDeclaration(outerClassId, bTr().TransformExpression (JSRuntime.Lazy classExpr))
            addStatement <| export true (ExprStatement(Var outerClassId))                

        let packageClass classDecl = 
            let trClassDecl =
                classDecl
                |> bTr().TransformStatement 
                |> SubstituteVar(outerClassId, Var classId).TransformStatement
            addStatement <| export true trClassDecl      

        if c.HasWSPrototype || members.Count > 0 then
            let classExpr setInstance = 
                ClassExpr(Some classId, baseType, 
                    ClassStatic (VarSetStatement(outerClassId, setInstance(JSThis))) 
                    :: List.ofSeq members)
            let implements =
                if output = O.JavaScript then [] else
                c.Implements |> List.map (tsTypeOfConcrete gsArr)
            let classDecl() = Class(classId, baseType, implements, List.ofSeq members, cgen)
            match baseType with
            | Some b ->
                let needsLazy = (not isSingleType || Option.isNone c.Type) && output <> O.TypeScriptDeclaration
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
                let needsLazy = (not isSingleType || (c.HasWSPrototype && Option.isNone c.Type && typ <> Definitions.Object && not isFSharpType)) && output <> O.TypeScriptDeclaration
                if needsLazy then
                    packageLazyClass <| classExpr
                else
                    packageClass <| classDecl()

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

    for typ in content.Types do
        match current.Classes.TryFind(typ) with
        | None -> ()
        | Some (_, _, None) -> ()
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
        | Bundle(_, ForceImmediate, Some ep) ->
            statements.Add (bodyTransformer([||]).TransformStatement(ep))
        | Bundle(_, (ForceOnLoad | ForceImmediate), None) ->
            failwith "Missing entry point or export. Add SPAEntryPoint attribute to a static method without arguments, or JavaScriptExport on types/methods to expose them."
        | _ -> ()
        
    if statements.Count = 0 then 
        [] 
    else
        if jsUsed.Count > 0 then
            let namedImports = 
                jsUsed |> Seq.map (fun n -> n, Id.New(n, str = true)) |> List.ofSeq
            declarations.Add(Import(None, None, namedImports, ""))

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
                    if not isSingleType then
                        "./" + m.Assembly + "/" + m.Name + ext   
                    elif m.Assembly = asmName then
                        "./" + m.Name + ext
                    else
                        "../" + m.Assembly + "/" + m.Name + ext
                declarations.Add(Import(defaultImport, fullImport, namedImports, fromModule))

        List.ofSeq (Seq.concat [ declarations; statements ])

let packageAssembly output (refMeta: M.Info) (current: M.Info) asmName entryPoint entryPointStyle =
    let pkgs = ResizeArray()
    let classes = HashSet(current.Classes.Keys)
    let pkgTyp (typ: TypeDefinition) =
        let p = packageType output refMeta current asmName (SingleType typ)
        if not (List.isEmpty p) then
            pkgs.Add(typ.Value.FullName.Replace("+", "."), p)
    for typ in current.Interfaces.Keys do
        classes.Remove(typ) |> ignore
        pkgTyp typ
    for typ in classes do
        pkgTyp typ
    if Option.isSome entryPoint then
        let epTyp = TypeDefinition { Assembly = ""; FullName = "$EntryPoint" }     
        let p = packageType output refMeta current asmName (Bundle ([| epTyp |], entryPointStyle, entryPoint))
        pkgs.Add("$EntryPoint", p)
    pkgs.ToArray()

let bundleAssembly output (refMeta: M.Info) (current: M.Info) asmName entryPoint entryPointStyle =
    let types =
        Seq.append current.Interfaces.Keys current.Classes.Keys
        //current.Classes.Keys
        |> Seq.distinct
        |> Array.ofSeq

    packageType output refMeta current asmName (Bundle (types, entryPointStyle, entryPoint))

let getImportedModules (pkg: Statement list) =
    pkg
    |> Seq.choose (
        function
        | Import(_, _, _, m) -> Some m
        | _ -> None
    )
    |> Seq.distinct
    |> Seq.toList

let readMapFileSources mapFile =
    match Json.Parse mapFile with
    | Json.Object fields ->
        let getString j = match j with Json.String s -> s | _ -> failwith "string expected in map file"
        let sources = fields |> List.pick (function "sources", Json.Array s -> Some (s |> List.map getString) | _ -> None)  
        let sourcesContent = fields |> List.pick (function "sourcesContent", Json.Array s -> Some (s |> List.map getString) | _ -> None)  
        List.zip sources sourcesContent
    | _ -> failwith "map file JSON should be an object"

let addLoadedModules (urls: string list) scriptBase skipAssemblyDir (pkg: Statement list) =
    if List.isEmpty urls then
        pkg
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

let programToString output pref (getWriter: unit -> WebSharper.Core.JavaScript.Writer.CodeWriter) statements =
    let program = statements |> JavaScriptWriter.transformProgram output pref
    let writer = getWriter()
    WebSharper.Core.JavaScript.Writer.WriteProgram pref writer program
    writer.GetCodeFile(), writer.GetMapFile()
