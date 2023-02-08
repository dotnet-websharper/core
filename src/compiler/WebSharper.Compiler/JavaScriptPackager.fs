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

//let private Address a = { Module = CurrentModule; Address = Hashed a }

let packageType (output: O) (refMeta: M.Info) (current: M.Info) asmName (typs: TypeDefinition []) entryPoint entryPointStyle =
    let imports = Dictionary<string, Dictionary<string, Id>>()
    let jsUsed = HashSet<string>()
    let declarations = ResizeArray<Statement>()
    let addresses = Dictionary<Address, Expression>()
    let statements = ResizeArray<Statement>()

    let classRes = Dictionary<TypeDefinition, Address * Id>()
        
    for typ in typs do
        let className = (typ.Value.FullName.Split([|'.';'+'|]) |> Array.last).Split('`') |> Array.head
        let classId = Id.New className
        let mn = asmName + "/" + typ.Value.FullName.Replace('+', '.')
        let currentClassAdds = Address.DefaultExport mn
        addresses.Add(currentClassAdds, Var classId)
        classRes.Add(typ, (currentClassAdds, classId))

    let currentModuleName, singleClassId = 
        match typs with 
        | [| typ |] -> 
            let currentClassAdds, classId = classRes[typ]
            let mn = match currentClassAdds.Module with JavaScriptModule mn -> mn | _ -> ""
            mn, classId
        | _ -> asmName + ".js", Id.Global()

    //let g = Id.New "Global"
    //let glob = Var g
    //addresses.Add(Address.Global(), glob)
    //addresses.Add(Address.Lib "self", glob)
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
                            res |> List.fold (fun e i -> ItemGet(e, Value (String i), Pure)) (Var singleClassId)
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

    let tsTypeOfAddress (a: Address) =
        let t = a.Address.Value |> List.rev
        match a.Module with
        | StandardLibrary
        | JavaScriptFile _ -> TSType.Named t
        | JavaScriptModule m when m = currentModuleName -> TSType.Named [ singleClassId.Name.Value ]
        | JavaScriptModule _ ->
            let a = if a.Address.Value.IsEmpty then { a with Address = Hashed [ "default" ] } else a
            match getOrImportAddress a with
            | GlobalAccess { Module = ImportedModule i; Address = a } ->
                TSType.Imported(i, a.Value |> List.rev)
            | Var i ->
                TSType.Imported(i, [])
            | _ ->
                TSType.Named t
        | ImportedModule v -> TSType.Imported(v, t)

    let allClasses = MergedDictionary(refMeta.Classes, current.Classes)
    let allInterfaces =  MergedDictionary(refMeta.Interfaces, current.Interfaces)
    let lookupType (t: TypeDefinition) =
        match allInterfaces.TryFind t with
        | Some i -> TypeTranslator.Interface i
        | _ ->
        match allClasses.TryFind t with
        | Some (a, ct, c) -> TypeTranslator.Class (a, ct, c)
        | _ -> TypeTranslator.Unknown
    
    let typeTranslator = TypeTranslator.TypeTranslator(lookupType, tsTypeOfAddress) 
    
    let inline tsTypeOfDef t = typeTranslator.TSTypeOfDef t
    let inline tsTypeOfConcrete gs i = typeTranslator.TSTypeOfConcrete gs i
    let inline tsTypeOf gs t = typeTranslator.TSTypeOf gs t

    let getGenerics j (gs: list<M.GenericParam>) =
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
        t.ResolveModule (fun m ->
            match getOrImportAddress { Module = JavaScriptModule m; Address = Hashed [] } with 
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

            override this.TransformId(a) =
                a.ToTSType(tsTypeOf gsArr)

            override this.TransformApplication(a, b, c) =
                base.TransformApplication(a, b, { c with Params = c.Params |> List.map resModule })

            override this.TransformNew(a, b, c) =
                base.TransformNew(a, b |> List.map resModule, c)

            override this.TransformCast(a, b) =
                base.TransformCast(resModule a, b)

            override this.TransformFuncDeclaration(a, b, c, d, e) =
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
        let name = typ.Value.FullName
        let currentClassAdds, classId = classRes[typ]
        let className = classId.Name.Value

        let gsArr = Array.ofList c.Generics
        let bTr() = bodyTransformer(gsArr)   

        let staticThisTransformer =
            { new Transformer() with
                override this.TransformGlobalAccess a = 
                    if a = currentClassAdds then
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
                        | _ ->  failwith "Error detupling function parameter type"
                )

        let cgenl = List.length c.Generics
        let thisTSTypeDef = lazy tsTypeOf gsArr (NonGenericType typ)
        let cgen = getGenerics 0 c.Generics
        let thisTSType = lazy (thisTSTypeDef.Value |> addGenerics cgen) 

        let members = ResizeArray<Statement>()
                    
        let mem (m: Method) info gc opts intfGen body =
            
            let gsArr = Array.append gsArr (Array.ofList gc)
            let bTr() = bodyTransformer(gsArr)   
            let getSignature isInstToStatic =         
                if output = O.JavaScript then TSType.Any else
                match IgnoreExprSourcePos body with
                | Function _ ->
                    let p, r = 
                        match intfGen with 
                        | None -> m.Value.Parameters, m.Value.ReturnType
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
                    tsTypeOf gsArr m.Value.ReturnType
            let mgen = getGenerics cgenl gc
            
            let func fromInst fname =
                match IgnoreExprSourcePos body with
                | Function (args, thisVar, _, b) ->
                    let f = Id.New(fname, str = true)
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
                    addStatement <| ExportDecl (false, FuncDeclaration(f, args, thisVar, implSt(fun () -> bTr().TransformStatement b), cgen @ mgen))
                | e ->
                    addStatement <| ExportDecl (false, VarDeclaration(Id.New(fname, mut = false, str = true, typ = TSType (getSignature fromInst)), implExpr(fun () -> bTr().TransformExpression e)))
            
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
                    members.Add <| ClassMethod(info, mname, args, thisVar, implStOpt (fun () -> staticThisTransformer.TransformStatement b), getSignature fromInst |> addGenerics mgen)
                | _ ->
                    let info = 
                        {
                            IsStatic = true
                            IsPrivate = false // TODO
                            IsOptional = false
                        }
                    members.Add <| ClassProperty(info, mname, getSignature false |> addGenerics mgen, implExprOpt (fun () -> body))
            | M.Func (fname, fromInst) ->
                func fromInst fname
            | M.GlobalFunc (addr, fromInst) ->
                func fromInst addr.Address.Value.Head
            | _ -> ()

        let propInfo isStatic isPrivate isOptional =
            {
                IsStatic = isStatic
                IsPrivate = isPrivate
                IsOptional = isOptional
            }

        if c.HasWSPrototype then
            for f in c.Fields.Values |> Seq.sortBy (fun f -> f.Order) do

                match f.CompiledForm with
                | M.InstanceField name ->
                    members.Add <| ClassProperty(propInfo false false false, name, tsTypeOf gsArr f.Type, None)
                | M.OptionalField name -> 
                    members.Add <| ClassProperty(propInfo false false true, name, tsTypeOf gsArr f.Type, None)
                | M.StaticField name ->
                    members.Add <| ClassProperty(propInfo true false false, name, tsTypeOf gsArr f.Type, None)
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

        let constructors = ResizeArray<string * Id option * Id list * Statement>()
        let ctorSigs = ResizeArray<Statement>()

        for KeyValue(ctor, ct) in c.Constructors do
            let getSignature isNew =         
                if output = O.JavaScript then TSType.Any else
                let pts = typeOfParams ct.Optimizations gsArr ctor.Value.CtorParameters
                if isNew then
                    TSType.New(pts, thisTSType.Value)
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
                        members.Add (ClassConstructor (args, thisVar, implStOpt (fun () -> b), getSignature true))
                    | _ ->
                        failwithf "Invalid form for translated constructor"

            | M.New (Some name) ->
                match ct.Expression with
                | Function (args, thisVar, _, b) ->                  
                    constructors.Add(name, thisVar, args, b)
                    let info =
                        {
                            IsStatic = true
                            IsPrivate = false
                            Kind = MemberKind.Simple
                        }
                    let ctorBody =
                        Return (New (JSThis, [], Value (String name) :: (args |> List.map Var)))
                    members.Add (ClassMethod(info, name, args, thisVar, implStOpt (fun () -> ctorBody), getSignature true))
                | _ ->
                    failwithf "Invalid form for translated constructor"
            //| M.NewIndexed i ->
            //    if body <> Undefined then
            //        match body with
            //        | Function (args, _, _, b) ->  
            //            indexedCtors.Add (i, (args, b))
            //        | _ ->
            //            failwithf "Invalid form for translated constructor"
            | M.Func (name, _) ->
                match ct.Expression with 
                | Function (args, thisVar, _, b) ->  
                    addStatement <| ExportDecl(false, FuncDeclaration(Id.New(name, str = true), args, thisVar, implSt (fun () -> bTr().TransformStatement b), cgen))
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
                    members.Add (ClassMethod(info, name, args, thisVar, implStOpt (fun () -> b), getSignature false))
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
            let maxArgs = constructors |> Seq.map (fun (_, _, a, _) -> List.length a) |> Seq.max
            let cArgs = List.init maxArgs (fun _ -> Id.New(mut = false, opt = true))
            let cThis = Id.NewThis()
            
            let ctorData = Dictionary()
            for (name, thisVar, args, body) in constructors do
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
            members.Add (ClassConstructor (allArgs, Some cThis, implStOpt (fun () -> Block cBody), TSType.Any)) // TODO optimize, do not generate cBody for .d.ts  
        
        let mutable isFSharpType = false

        match ct with
        | M.FSharpUnionInfo u when Option.isNone c.Type ->         
            let tags() = u.Cases |> List.mapi (fun i c -> c.Name, MemberKind.Simple, Value (Int i)) |> Object
            addStatement <| ExportDecl(false, VarDeclaration(Id.New("Tags", mut = false, str = true), implExpr tags))
            isFSharpType <- true

            if output <> O.JavaScript then
                
                let ucTypes = ResizeArray()
                for uci, uc in u.Cases |> Seq.indexed do
                    
                    let tagMem() = 
                        ClassProperty(propInfo false false false, "$", TSType.Basic (string uci), None)
                    match uc.Kind with
                    | M.NormalFSharpUnionCase fs ->
                        let ucmems =
                            fs |> List.mapi (fun i f ->
                                ClassProperty(propInfo false false false, "$" + string i, tsTypeOf gsArr f.UnionFieldType, None)
                            )
                        addStatement <| ExportDecl(false, 
                            Interface(uc.Name, [], tagMem() :: ucmems, cgen)
                        )
                        ucTypes.Add(TSType.Basic uc.Name |> addGenerics cgen)
                    | M.ConstantFSharpUnionCase v -> 
                        ucTypes.Add(TSType.Basic v.TSType)
                    | M.SingletonFSharpUnionCase ->
                        addStatement <| ExportDecl(false, 
                            Interface(uc.Name, [], [ tagMem() ], cgen)
                        )
                        ucTypes.Add(TSType.Basic uc.Name |> addGenerics cgen)

                addStatement <| ExportDecl(false, 
                    Alias(TSType.Basic className |> addGenerics cgen, TSType.Union (List.ofSeq ucTypes))
                )

        | M.FSharpRecordInfo r when Option.isNone c.Type ->     
            isFSharpType <- true

            if not c.HasWSPrototype && output <> O.JavaScript then
                let rmems =
                    r |> List.map (fun f ->
                        ClassProperty(propInfo false false f.Optional, f.JSName, tsTypeOf gsArr f.RecordFieldType, None)
                    )
                addStatement <| ExportDecl(true, 
                    Interface(className, [], rmems, cgen)
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

        let lazyClassId = lazy Id.New("_c")

        let packageLazyClass classExpr =
            addStatement <| VarDeclaration(lazyClassId.Value, bTr().TransformExpression (JSRuntime.Lazy classExpr))
            addStatement <| ExportDecl(true, ExprStatement(Var lazyClassId.Value))                

        let packageClass classDecl = 
            addStatement <| ExportDecl(true, bTr().TransformStatement classDecl)                

        if c.HasWSPrototype || members.Count > 0 then
            let classExpr setInstance = 
                ClassExpr(Some classId, baseType, 
                    ClassStatic (VarSetStatement(lazyClassId.Value, setInstance(JSThis))) 
                    :: List.ofSeq members)
            let classDecl() = Class(classId, baseType, [], List.ofSeq members, [])
            match baseType with
            | Some b ->
                let needsLazy = Option.isNone c.Type && output <> O.TypeScriptDeclaration
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
                let needsLazy = c.HasWSPrototype && Option.isNone c.Type && typ <> Definitions.Object && not isFSharpType && output <> O.TypeScriptDeclaration
                if needsLazy then
                    packageLazyClass <| classExpr
                else
                    packageClass <| classDecl()

    let packageInterface (typ: TypeDefinition) (i: M.InterfaceInfo) =
        let _, classId = classRes[typ]
        let className = classId.Name.Value
        let igen = List.length i.Generics
        let gen = getGenerics 0 i.Generics
        let gsArr = Array.ofList i.Generics
        if output <> O.JavaScript then
            let imems =
                i.Methods |> Seq.map (fun (KeyValue (m, (n, k, gc))) ->
                    let gsArr = Array.append gsArr (Array.ofList gc)
                    let args, argTypes =
                        m.Value.Parameters |> List.mapi (fun i p ->
                            Id.New(string ('a' + char i), mut = false, str = true), tsTypeOf gsArr p
                        ) |> List.unzip
                    let signature = TSType.Lambda(argTypes, tsTypeOf gsArr m.Value.ReturnType)
                    let info = 
                        {
                            IsStatic = false
                            IsPrivate = false
                            Kind = k
                        }
                    ClassMethod(info, n, args, None, None, signature |> addGenerics (getGenerics igen gc))    
                ) |> List.ofSeq

            addStatement <| ExportDecl(true, 
                Interface(className, i.Extends |> List.map (tsTypeOfConcrete gsArr), imems, gen)
            )

        let methodNames =
            i.Methods.Values |> Seq.map (fun (i, _, _) -> i)

        let isIntf =
            let isFunctionName =
                isFunctionNameForInterface typ
            let x = Id.New("x", typ = TSType TSType.Any)
            let returnType =
                if output = O.JavaScript then 
                    None 
                else
                    Some (TSType (TSType.TypeGuard(x, TSType.Named [ className ] |> addGenerics gen)))
            let funcId = Id.New(isFunctionName, str = true, ?typ = returnType) 
            if Seq.isEmpty methodNames then
                FuncDeclaration(funcId, [x], None, implSt (fun () -> Return (Value (Bool true))), gen)
            else         
                let shortestName = methodNames |> Seq.minBy String.length
                let check = Binary(Value (String shortestName), BinaryOperator.``in``, Var x)
                FuncDeclaration(funcId, [x], None, implSt (fun () -> Return check), gen)

        statements.Add(ExportDecl (false, isIntf))

    for typ in typs do
        match current.Classes.TryFind(typ) with
        | None -> ()
        | Some (a, ct, None) -> ()
        | Some (a, ct, Some c) ->
            packageClass typ a ct c

        match current.Interfaces.TryFind(typ) with
        | Some i ->
            packageInterface typ i
        | None -> ()

    if output <> O.TypeScriptDeclaration then
        match entryPointStyle, entryPoint with
        | (OnLoadIfExists | ForceOnLoad), Some ep ->
            addStatement <| ExprStatement (bodyTransformer([||]).TransformExpression (JSRuntime.OnLoad (Function([], None, None, ep))))
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
                let ext =
                    match output with
                    | O.JavaScript -> ".js"
                    | _ -> ""
                let fromModule =
                    if m.StartsWith (asmName + "/") then
                        "./" + m[asmName.Length + 1 ..] + ext
                    else
                        "../" + m + ext
                declarations.Add(Import(defaultImport, fullImport, namedImports, fromModule))

        List.ofSeq (Seq.concat [ declarations; statements ])

let packageAssembly output (refMeta: M.Info) (current: M.Info) asmName entryPoint entryPointStyle =
    let pkgs = ResizeArray()
    let classes = HashSet(current.Classes.Keys)
    let pkgTyp (typ: TypeDefinition) =
        let p = packageType output refMeta current asmName [| typ |] None entryPointStyle
        if not (List.isEmpty p) then
            pkgs.Add(typ.Value.FullName.Replace("+", "."), p)
    for typ in current.Interfaces.Keys do
        classes.Remove(typ) |> ignore
        pkgTyp typ
    for typ in classes do
        pkgTyp typ
    if Option.isSome entryPoint then
        let epTyp = TypeDefinition { Assembly = ""; FullName = "$EntryPoint" }     
        let p = packageType output refMeta current asmName [| epTyp |] entryPoint entryPointStyle
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

let programToString output pref (getWriter: unit -> WebSharper.Core.JavaScript.Writer.CodeWriter) statements =
    let program = statements |> JavaScriptWriter.transformProgram output pref
    let writer = getWriter()
    WebSharper.Core.JavaScript.Writer.WriteProgram pref writer program
    writer.GetCodeFile(), writer.GetMapFile()
