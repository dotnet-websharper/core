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
open WebSharper.Core.DependencyGraph
open WebSharper.Core.AST
open WebSharper.Core.Metadata

module M = WebSharper.Core.Metadata
module R = WebSharper.Core.Resources
module I = IgnoreSourcePos

type StaticMembers =
    {
        Members : ResizeArray<Address * Expression * TSType>
        Namespaces : Dictionary<string, StaticMembers>
    }

    static member Empty =
        {
            Members = ResizeArray()
            Namespaces = Dictionary()
        }

type BodyTransformer(toTSType, getModule) =
    inherit Transformer()

    override this.TransformNewTuple(a, t) =
        let res = NewTuple(List.map this.TransformExpression a, [])
        match t with
        | [] -> res
        | _ -> Cast(toTSType(TupleType (t, false)), res) 

    override this.TransformCast(t, e) =
        Cast(t.ResolveModule getModule, e)

let packageAssembly (refMeta: M.Info) (current: M.Info) (resources: seq<R.IResource>) moduleName isBundle =
    let addresses = Dictionary()
    let directives = ResizeArray()
    let declarations = ResizeArray()
    let statements = ResizeArray()

    let glob = Var (Id.Global())
    addresses.Add(Address.Global(), glob)
    addresses.Add(Address.Lib "window", glob)
                                                            
    let strId name = Id.New(name, mut = false, str = true)

    let isModule = Option.isSome moduleName

    // TODO: only add what is necessary
    // TODO: set Array.prototype.GetEnumerator and String.prototype.GetEnumerator 
    let libExtensions =
        let ie t = TSType.Generic(TSType.Basic "System.Collections.Generic.IEnumerable", [ t ])
        let T = TSType.Basic "T"
        [ 
            Interface ("Error", [], [ ClassProperty (false, "inner", TSType.Basic "Error")], []) 
            Interface ("Object", [], [ ClassProperty (false, "setPrototypeOf", TSType.Any) ], [])  
            Interface ("Math", [], [ ClassProperty (false, "trunc", TSType.Any) ], [])  
            Interface ("Array", [ ie T ], [], [ T ])
            Interface ("String", [ ie (TSType.Basic "string") ], [], [])
            //Interface ("Object", [], [ ClassMethod (true, "setPrototypeOf", [strId "obj"; strId "prototype"], None, TSType.Lambda ([TSType.Object; TSType.Union [TSType.Object; TSType.Null]], TSType.Object), 0) ], 0)  
            //Interface ("Math", [], [ ClassMethod (true, "trunc", [ strId "x" ], None, TSType.Lambda([TSType.Number], TSType.Number), 0)], 0) 
        ]

    if isModule then
        declarations.Add <| Declare (Namespace ("global", libExtensions))
    else
        libExtensions |> List.iter declarations.Add 

    let importJS js =
        if isModule then
            declarations.Add <| ImportAll (None, js)
        glob

    let importTS ts =
        if isModule then
            let var = Id.New (ts |> String.filter System.Char.IsUpper)
            declarations.Add <| ImportAll (Some var, "./" + ts)
            Var var
        else
            directives.Add <| XmlComment  (sprintf "<reference path=\"%s.ts\" />" ts)
            Undefined

    match moduleName with
    | Some n ->
        directives.Add <| XmlComment (sprintf "<amd-module name=\"%s\"/>" n)
    | _ -> ()

    let rec getAddress (address: Address) =
        match addresses.TryGetValue address with
        | true, v -> v
        | _ ->
            let res =
                match address.Address.Value with
                | [] ->
                    match address.Module with
                    | StandardLibrary -> failwith "impossible, already handled"
                    | JavaScriptFile "" ->
                        glob
                    | JavaScriptFile "Runtime" ->
                        importJS "./WebSharper.Core.JavaScript/Runtime.js"
                    | JavaScriptFile js ->
                        importJS (js + ".js")
                    | WebSharperModule ts ->
                        importTS ts
                    | CurrentModule -> failwith "empty local address"
                | [ name ] ->
                    match address.Module with
                    | CurrentModule ->
                        Var (strId name)
                    | StandardLibrary ->
                        let var = Id.New name
                        declarations.Add <| VarDeclaration(var, Var (strId name)) 
                        Var var
                    | JavaScriptFile _ ->
                        match addresses.TryGetValue { address with Module = CurrentModule } with
                        | true, v -> v
                        | _ ->
                            getAddress { address with Address = Hashed [] } |> ignore
                            let var = strId name
                            if not <| StandardLibNames.Set.Contains name then
                                declarations.Add <| Declare (VarDeclaration(var, Undefined)) 
                            Var var
                    | WebSharperModule _ ->
                        let parent = getAddress { address with Address = Hashed [] }
                        if isModule then
                            ItemGet(parent, Value (String name), Pure)
                        else
                            getAddress { address with Module = CurrentModule }
                | name :: r ->
                    let parent = getAddress { address with Address = Hashed r }
                    ItemGet(parent, Value (String name), Pure)
            addresses.Add(address, res)
            res

    let getModule m =
        match importTS m with
        | Var v ->
            Some v
        | Undefined ->
            None
        | _ -> failwith "expecting a Var or Undefined for module import"
    
    for r in resources do
        match r with
        | :? R.IDownloadableResource as d ->
            for m in d.GetImports() do
                if m.EndsWith ".js" then
                    getAddress { 
                        Module = JavaScriptFile (m.Substring(0, m.Length - 3))
                        Address = Hashed [] 
                    } |> ignore
        | _ -> ()

    let mutable currentNamespace = ResizeArray() 
    let mutable currentNamespaceContents = [ statements ]

    let commonLength s1 s2 =
        Seq.zip s1 s2 |> Seq.takeWhile (fun (a, b) -> a = b) |> Seq.length

    let export x =
        if isModule || currentNamespace.Count > 0 then Export x else x

    let closeNamespace() =
        match currentNamespaceContents with
        | contents :: (parentContents :: _ as pc) ->
            let l = currentNamespace.Count - 1
            let name = currentNamespace.[l]
            currentNamespace.RemoveAt(l)
            currentNamespaceContents <- pc
            parentContents.Add(export (Namespace (name, List.ofSeq contents)))
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

    let addExport s =
        addStatement (export s) 

    let package (a: Address) expr (t: TSType) =
        let n = toNamespaceWithName a
        let i = Id.New (n, str = true)
        let exp =
            match expr with
            | Function(args, body) -> FuncDeclaration(i, args, body)
            | _ -> VarDeclaration (i, expr)
        let texp =
            match t with
            | TSType.Any -> exp
            | _ -> TypedDeclaration (exp, t)
        addExport texp

    let packageByName (a: Address) f =
        let n = toNamespaceWithName a
        addExport <| f n

    let packageCctor a expr =
        let n = toNamespaceWithName a
        match expr with
        | Function ([], body) ->
            let c = Id.New(n, str = true)
            let removeSelf = ExprStatement (VarSet (c, ItemGet(glob, Value (String "ignore"), Pure)))    
            let expr = Function([], Block [removeSelf; body])
            addExport <| VarDeclaration(c, expr)  
        | _ ->
            failwithf "Static constructor must be a function"
            
    let classes = Dictionary(current.Classes)
    let customTypes = Dictionary(current.CustomTypes)

    let rec withoutMacros info =
        match info with
        | M.Macro (_, _, Some fb) -> withoutMacros fb
        | _ -> info 

    let mappedTypes = Dictionary()

    let tsTypeOfAddress (a: Address) =
        let acc = a.Address.Value |> List.rev |> String.concat "."
        match a.Module with
        | StandardLibrary
        | JavaScriptFile _
        | CurrentModule -> TSType.Basic acc
        | WebSharperModule m ->
            match getAddress { a with Address = Hashed [] } with
            | Var v ->
                TSType.Imported(v, acc)
            | Undefined ->
                TSType.Basic acc
            | _ -> failwith "expecting a Var or Undefined for module import"

    let lookupType (t: TypeDefinition) =
        match t.Value.FullName with
        | _ ->
        let cls =
            match refMeta.Classes.TryFind t with
            | Some _ as res -> res
            | _ -> current.Classes.TryFind t
        match cls with
        | Some c -> TypeTranslator.Class c
        | _ -> 
        let intf = 
            match refMeta.Interfaces.TryFind t with
            | Some _ as res -> res
            | _ -> current.Interfaces.TryFind t
        match intf with
        | Some i -> TypeTranslator.Interface i
        | _ ->
        let cust = 
            match refMeta.CustomTypes.TryFind t with
            | Some _ as res -> res
            | _ -> current.CustomTypes.TryFind t
        match cust with
        | Some c -> TypeTranslator.CustomType c
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
                match c.Constraints with 
                | [] -> p
                | cs -> TSType.Constraint(p, cs |> List.map (tsTypeOf gsArr))
                |> Some
        ) |> List.ofSeq

    let addGenerics g t =
        match g with
        | [] -> t
        | _ -> 
            match t with
            | TSType.Any -> TSType.Any
            | _ ->
                TSType.Generic(t, g)

    let statics = StaticMembers.Empty

    let addStatic (a: Address) (e, s) =
        let rec getDict (s: StaticMembers) a =
            match a with
            | [] -> s
            | h :: t ->
                match s.Namespaces.TryGetValue(h) with
                | true, ns -> getDict ns t
                | false, _ ->
                    let ns = StaticMembers.Empty
                    s.Namespaces.Add(h, ns)
                    getDict ns t

        (getDict statics (List.rev a.Address.Value)).Members.Add(a, e, s)        

    let packageUnion (u: M.FSharpUnionInfo) (addr: Address) proto gsArr =
        toNamespace addr.Address.Value
        proto |> Option.iter (fun (baseType, impls, members, gen) ->
            let mutable numArgs = 0
            let strId x = Id.New(x, str = true)
            let specCtors =
                u.Cases |> List.mapi (fun tag uc ->
                    let fields =
                        match uc.Kind with
                        | M.NormalFSharpUnionCase fields -> fields
                        | M.SingletonFSharpUnionCase | M.ConstantFSharpUnionCase _ -> []
                    numArgs <- max numArgs (List.length fields)
                    let args =
                        (strId "$", Modifiers.None)
                        :: (fields |> List.mapi (fun i _ -> strId("$" + string i), Modifiers.None))
                    let argsType =
                        TSType.Basic (string tag)
                        :: (fields |> List.map (fun f -> tsTypeOf gsArr f.UnionFieldType))
                    let signature = TSType.New(argsType, TSType.Any)
                    ClassConstructor(args, None, signature)
                )
            let genCtor =
                let args =
                    (strId "$", Modifiers.Public)
                    :: List.init numArgs (fun i -> Id.New("$" + string i, str = true, opt = true), Modifiers.Public)
                ClassConstructor(args, Some (Statement.Block []), TSType.Any)
            addExport <| Class("$", baseType, impls, specCtors @ genCtor :: members, gen)
        )
        let gen =
            match proto with
            | Some (_, _, _, gen) -> gen
            | None -> []
        let unionNested = (addr.Address.Value |> List.rev |> String.concat ".") + "."
        let unionClass = proto |> Option.map (fun _ -> TSType.Basic (unionNested + "$") |> addGenerics gen)
        let cases = 
            u.Cases |> List.mapi (fun tag uc ->
                let case (fields: list<M.UnionCaseFieldInfo>) =
                    let tag = ClassProperty (false, "$", TSType.Basic (string tag))
                    let mem =
                        fields |> List.mapi (fun i f ->
                            ClassProperty (false, "$" + string i, tsTypeOf gsArr f.UnionFieldType)
                        )
                    addExport <| Interface(uc.Name, Option.toList unionClass, tag :: mem, gen)
                    TSType.Basic (unionNested + uc.Name) |> addGenerics gen
                match uc.Kind with
                | M.NormalFSharpUnionCase uci -> case uci
                | M.ConstantFSharpUnionCase v ->
                    match v with
                    | String s -> TSType.Basic ("'" + s + "'")
                    | Null -> TSType.Basic "null"
                    | _ -> TSType.Basic (string v.Value)
                | M.SingletonFSharpUnionCase -> case []
            )
        match addr.Address.Value with
        | n :: a ->
            if Option.isSome unionClass then toNamespace a
            addExport <| Alias ((TSType.Basic n |> addGenerics gen), TSType.Union cases)
        | _ -> failwith "empty address for union type"

    let rec packageClass (t: TypeDefinition) (c: M.ClassInfo) =

        let classAddress = 
            if c.HasWSPrototype then c.Address else None

        match c.BaseClass with
        | Some { Entity = b } ->
            match classes.TryFind b with
            | Some bc ->
                classes.Remove b |> ignore
                packageClass b bc
            | _ -> ()
        | _ -> ()

        let members = ResizeArray<Statement>()
        
        let smem (a: Address) inClass inNamespace =
            match classAddress with
            | Some ca when a.Address.Value.Tail = ca.Address.Value.Tail ->
                members.Add (inClass a.Address.Value.Head)     
            | _ -> addStatic a (inNamespace())

        let gsArr = Array.ofList c.Generics

        for f, _, t in c.Fields.Values do
            let typ = tsTypeOf gsArr t
            match f with
            | M.InstanceField n
            | M.OptionalField n ->
                members.Add (ClassProperty (false, n, typ)) 
            | M.StaticField a ->
                smem a (fun n -> ClassProperty (true, n, typ)) (fun () -> Undefined, typ)
            | _ -> ()

        match c.StaticConstructor with
        | Some(_, GlobalAccess { Module = JavaScriptFile "Runtime"; Address = a }) when a.Value = [ "ignore" ] -> ()
        | Some(ccaddr, body) ->
            //smem ccaddr 
            //    (fun n -> 
            //        match IgnoreExprSourcePos body with
            //        | Function([], b) ->
            //            ClassMethod(true, n, [], Some b, TSType.Any, 0)
            //        | _ -> failwith "static constuctor translated form must be a function"
            //    ) 
            //    (fun () -> body, TSType.Any)
            addStatic ccaddr (body, TSType.Any)
        | _ -> ()

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
        let thisTSTypeDef = tsTypeOf gsArr (NonGenericType t)

        let mem (m: Method) info gc opts intfGen body =
            let gsArr = Array.append gsArr (Array.ofList gc)
            let getSignature isInstToStatic =         
                match body with
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
                            tsTypeOf gsArr (NonGenericType t) :: (typeOfParams opts gsArr p)
                        else typeOfParams opts gsArr p
                    TSType.Lambda(pts, tsTypeOf gsArr r)
                | _ ->
                    thisTSTypeDef |> addGenerics (List.init cgenl (fun _ -> TSType.Any))

            let g = getGenerics cgenl gc
            let body = BodyTransformer(tsTypeOf gsArr, getModule).TransformExpression(body)
            let getMember isStatic n =
                match IgnoreExprSourcePos body with
                | Function (args, b) ->
                    ClassMethod(isStatic, n, args, Some b, getSignature false |> addGenerics g)
                | Undefined ->
                    let args = m.Value.Parameters |> List.map (fun _ -> Id.New(mut = false))
                    ClassMethod(isStatic, n, args, None, TSType.Any |> addGenerics g)
                | _ -> failwith "unexpected form for class member"
            match withoutMacros info with
            | M.Instance mname ->
                members.Add (getMember false mname)
            | M.Static maddr ->
                smem maddr (getMember true) (fun () -> body, getSignature false |> addGenerics g)
            | M.AsStatic maddr ->
                smem maddr (getMember true) (fun () -> body, getSignature true |> addGenerics g)
            | _ -> ()
                    
        for KeyValue(m, (info, opts, gc, body)) in c.Methods do
            mem m info gc opts None body 
        let intfGenerics =
            c.Implements |> Seq.map (fun i -> i.Entity, Array.ofList i.Generics) |> dict
        for KeyValue((i, m), (info, body)) in c.Implementations do
            let intfGen = 
                match m.Value.Generics with
                | 0 -> intfGenerics.[i]
                | mgen -> Array.append (intfGenerics.[i]) (Array.init mgen (fun i -> TypeParameter (cgenl + i)))
            mem m info [] M.Optimizations.None (Some intfGen) body

        let indexedCtors = Dictionary()
        
        let cgen = getGenerics 0 c.Generics

        for KeyValue(ctor, (info, opts, body)) in c.Constructors do
            let body = BodyTransformer(tsTypeOf gsArr, getModule).TransformExpression(body)
            let thisTSType = thisTSTypeDef |> addGenerics cgen 
            match withoutMacros info with
            | M.New ->
                if body <> Undefined then
                    match body with
                    | Function ([], I.Empty) 
                    | Function ([], I.ExprStatement(I.Application(I.Base, [], _, _))) -> 
                        ()
                    | Function (args, b) ->                  
                        let args = List.map (fun x -> x, Modifiers.None) args
                        let signature =
                            TSType.New(typeOfParams opts gsArr ctor.Value.CtorParameters, thisTSType)
                        members.Add (ClassConstructor (args, Some b, signature))
                    | _ ->
                        failwithf "Invalid form for translated constructor"
            | M.NewIndexed i ->
                if body <> Undefined then
                    match body with
                    | Function (args, b) ->  
                        let index = Id.New("i: " + string i, str = true)
                        let allArgs = List.map (fun x -> x, Modifiers.None) (index :: args)
                        let signature =
                            TSType.New(TSType.Any :: (typeOfParams opts gsArr ctor.Value.CtorParameters), thisTSType)
                        members.Add (ClassConstructor (allArgs, None, signature))
                        indexedCtors.Add (i, (args, b))
                    | _ ->
                        failwithf "Invalid form for translated constructor"
            | M.Static maddr ->
                let signature =
                    TSType.Lambda(typeOfParams opts gsArr ctor.Value.CtorParameters, thisTSType)
                
                smem maddr 
                    (fun n ->
                        match IgnoreExprSourcePos body with
                        | Function (args, b) ->
                            ClassMethod(true, n, args, Some b, signature)
                        | _ -> failwith "unexpected form for class constructor"
                    ) 
                    (fun () -> body, signature |> addGenerics cgen)
            | _ -> ()

        match classAddress with 
        | None -> ()
        | Some addr ->
            let baseType = 
                match c.BaseClass with
                | Some b when b.Entity.Value.FullName = "System.Object" -> Some (TSType.Basic "WebSharper.Obj") 
                | Some b -> Some (tsTypeOfConcrete gsArr b)
                | None -> None
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

            let impls = c.Implements |> List.map (tsTypeOfConcrete gsArr)
            let gen = getGenerics 0 c.Generics

            match current.CustomTypes.TryGetValue t with
            | true, (_, M.FSharpUnionInfo u) ->
                packageUnion u addr (Some (baseType, impls, List.ofSeq members, gen)) gsArr
            | _ ->
                packageByName addr <| fun n -> Class(n, baseType, impls, List.ofSeq members, gen)
            
        if c.IsStub then
            // import addresses for stub classes
            c.Address |> Option.iter (fun a -> getAddress { a with Module = JavaScriptFile "" } |> ignore)
    
    while classes.Count > 0 do
        let (KeyValue(t, c)) = classes |> Seq.head
        classes.Remove t |> ignore
        customTypes.Remove t |> ignore
        packageClass t c

    let rec packageUnannotatedCustomType (t: TypeDefinition) (addr: Address) (c: CustomTypeInfo) =
        match c with
        | CustomTypeInfo.FSharpRecordInfo fields ->
            let fields =
                fields |> List.map (fun f ->
                    let t = tsTypeOf [||] f.RecordFieldType
                    ClassProperty(false, f.JSName, t)
                )
            // TODO: generics?
            packageByName addr <| fun n -> Interface(n, [TSType.Basic "NOPROTO_Record"], fields, [])
        | CustomTypeInfo.FSharpUnionInfo u ->
            packageUnion u addr None [||]
        | CustomTypeInfo.FSharpUnionCaseInfo _
        | CustomTypeInfo.DelegateInfo _
        | CustomTypeInfo.EnumInfo _
        | CustomTypeInfo.StructInfo
        | CustomTypeInfo.NotCustomType -> ()

    while customTypes.Count > 0 do
        let (KeyValue(t, (a, c))) = customTypes |> Seq.head
        customTypes.Remove t |> ignore
        packageUnannotatedCustomType t { Module = CurrentModule; Address = a } c
    
    let rec packageStatics (s: StaticMembers) =
        for a, e, t in s.Members do 
            package a e t
        
        for n in s.Namespaces.Values do
            packageStatics n

    packageStatics statics

    for KeyValue(td, i) in current.Interfaces do       
        let igen = List.length i.Generics

        let gsArr = Array.ofList i.Generics

        let mem =
            i.Methods |> Seq.map (fun (KeyValue (m, (n, gc))) ->
                let gsArr = Array.append gsArr (Array.ofList gc)
                let args, argTypes =
                    m.Value.Parameters |> List.mapi (fun i p ->
                        strId (string ('a' + char i)), tsTypeOf gsArr p
                    ) |> List.unzip
                let signature = TSType.Lambda(argTypes, tsTypeOf gsArr m.Value.ReturnType)

                ClassMethod(false, n, args, None, signature |> addGenerics (getGenerics igen gc))    
            ) |> List.ofSeq

        let gen = getGenerics 0 i.Generics

        packageByName i.Address <| fun n ->
            Interface(n, i.Extends |> List.map (tsTypeOfConcrete gsArr), mem, gen)

        packageByName i.Address <| fun n ->
            let x = Id.New "x"
            let shortestName, _ = i.Methods.Values |> Seq.minBy (fst >> String.length)
            let check = Binary(Value (String shortestName), BinaryOperator.``in``, Var x)
            let signature = 
                TSType.Lambda([TSType.Any], TSType.TypeGuard(x, tsTypeOfDef td |> addGenerics gen))
                |> addGenerics gen
            TypedDeclaration(FuncDeclaration(strId ("is" + n), [x], Return check), signature) 

    toNamespace []

    if isBundle then
        match current.EntryPoint with
        | Some ep -> addStatement <| ExprStatement (JSRuntime.OnLoad (Function([], ep)))
        | _ -> failwith "Missing entry point. Add an SPAEntryPoint attribute to a static method without arguments."
    
    let globalAccessTransformer =
        { new Transformer() with
            override this.TransformGlobalAccess a =
                getAddress a
                //match addresses.TryGetValue a with
                //| true, v -> v
                //| _ -> 
                //    Var (strId (a.Address.Value |> List.rev |> String.concat "."))
        }

    let trStatements = statements |> Seq.map globalAccessTransformer.TransformStatement |> List.ofSeq

    if List.isEmpty trStatements then 
        [] 
    else
        List.ofSeq directives @ List.ofSeq declarations @ trStatements 

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
