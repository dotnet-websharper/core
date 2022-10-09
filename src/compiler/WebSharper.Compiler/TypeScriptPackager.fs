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
module WebSharper.Compiler.TypeScriptPackager

open System.Collections.Generic

open WebSharper.Core
open WebSharper.Core.DependencyGraph
open WebSharper.Core.AST
open WebSharper.Core.Metadata

module M = WebSharper.Core.Metadata
module R = WebSharper.Core.Resources
module I = IgnoreSourcePos

type EntryPointStyle =
    | OnLoadIfExists
    | ForceOnLoad
    | ForceImmediate

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

type BodyTransformer(toTSType, getAddress) =
    inherit Transformer()

    let resWSModule (t: TSType) =
        t.ResolveModule (fun m ->
            getAddress { Module = WebSharperModule m; Address = Hashed [] } |> fst 
        )

    override this.TransformId(a) =
        a.ToTSType(toTSType)

    override this.TransformNew(c, ts, args) =
        Expression.New(
            this.TransformExpression c,
            List.map resWSModule ts,
            List.map this.TransformExpression args
        )

    override this.TransformApplication(f, args, i) =
        Application(
            this.TransformExpression f,
            List.map this.TransformExpression args,
            { i with
                Params = List.map resWSModule i.Params
            }
        )

    override this.TransformNewTuple(a, t) =
        let res = NewTuple(List.map this.TransformExpression a, [])
        match t with
        | [] -> res
        | _ -> Cast(toTSType(TupleType (t, false)), res) 

    override this.TransformCast(t, e) =
        Cast(resWSModule t, this.TransformExpression e)

    override this.TransformGlobalAccess(a) =
        match getAddress a with
        | Some v, acc -> GlobalAccess { Module = ImportedModule v; Address = Hashed acc } 
        | None, acc -> GlobalAccess { Module = CurrentModule; Address = Hashed acc }

let packageAssembly (refMeta: M.Info) (current: M.Info) (resources: seq<R.IResource>) moduleName entryPoint entryPointStyle =
    let addresses = Dictionary()
    let directives = ResizeArray()
    let declarations = ResizeArray()
    let statements = ResizeArray()

    let glob = Var (Id.Global())
                                                            
    let strId name = Id.New(name, mut = false, str = true)

    let isModule = Option.isSome moduleName
    let isBundle = Option.isSome entryPoint

    // TODO: only add what is necessary for bundles
    if isBundle || current.Classes.ContainsKey Definitions.Obj then

        let libExtensions =
            let ie t = TSType.Generic(TSType.Named [ "WebSharper"; "IEnumerable" ], [ t ])
            let T = TSType.Basic "T"
            let ic t = TSType.Generic(TSType.Named [ "WebSharper"; "IComparable" ], [ t ])
            [ 
                Interface ("Error", [], [ ClassProperty (false, "inner", TSType.Basic "Error", false)], []) 
                Interface ("Object", [], [ ClassMethod (false, "setPrototypeOf", [strId "o"; strId "proto" ], None, TSType.Lambda ([TSType.Any; TSType.Object], TSType.Any)) ], [])  
                Interface ("Math", [], [ ClassMethod (false, "trunc", [ strId "x" ], None, TSType.Lambda([TSType.Number], TSType.Number))], []) 
                Interface ("Array", [ ie T ], [], [ T ])
                Interface ("String", [ ie (TSType.Basic "string") ], [], [])
                Interface ("Number", [ ic (TSType.Basic "number") ], [], [])
                Interface ("RegExp", [], 
                    [ 
                        ClassProperty (false, "flags", TSType.String, false)
                        ClassProperty (false, "sticky", TSType.Boolean, false)
                        ClassProperty (false, "unicode", TSType.Boolean, false)
                    ], []) 
            ]

        if isModule then
            declarations.Add <| Declare (Namespace ("global", libExtensions))
        else
            libExtensions |> List.iter declarations.Add 

        let rec local a =
            match a with 
            | [] -> glob
            | [ a ] -> Var (strId a)
            | h :: r -> (local r).[Value (String h)]
        let expandBuiltinType typ mem body =
            declarations.Add <| ExprStatement (ItemSet(local [ "prototype"; typ ], Value (String mem), body))
        
        expandBuiltinType "Array" "GetEnumerator" (Lambda([], None, AST.New(local [ "ItemEnumerator"; "WebSharper" ], [], [ This ])))
        expandBuiltinType "String" "GetEnumerator" (Lambda([], None, AST.New(local [ "ItemEnumerator"; "WebSharper" ], [], [ This ])))
        let x = Id.New("x", mut = false, typ = TSType (TSType.Number))
        expandBuiltinType "Number" "CompareTo" (Lambda([x], None, This ^- Var x))

    let importJS js =
        if isModule then
            declarations.Add <| ImportAll (None, js)
        None

    let importTS ts =
        if isModule then
            let var = Id.New (ts |> String.filter System.Char.IsUpper)
            declarations.Add <| ImportAll (Some var, "./" + ts)
            Some var
        else
            directives.Add <| XmlComment  (sprintf "<reference path=\"%s.ts\" />" ts)
            None
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
                    | StandardLibrary -> None
                    | JavaScriptFile "" ->
                        None
                    | JavaScriptFile "Runtime" ->
                        importJS "./WebSharper.Core.JavaScript/Runtime.js"
                    | JavaScriptFile js ->
                        importJS (js + ".js")
                    | WebSharperModule ts ->
                        if isBundle then None else importTS ts
                    | CurrentModule -> failwith "empty local address"
                    | ImportedModule v -> Some v
                    , []
                | [ name ] ->
                    match address.Module with
                    | CurrentModule ->
                        None, [ name ]
                    | StandardLibrary ->
                        let var = Id.New name
                        declarations.Add <| VarDeclaration(var, Var (strId name)) 
                        Some var, []
                    | JavaScriptFile _ ->
                        match addresses.TryGetValue { address with Module = CurrentModule } with
                        | true, v -> v
                        | _ ->
                            getAddress { address with Address = Hashed [] } |> ignore
                            let var = strId name
                            if not <| StandardLibNames.Set.Contains name then
                                declarations.Add <| Declare (VarDeclaration(var, Undefined)) 
                                if isModule then
                                    declarations.Add <| Declare (Alias(TSType.Basic name, TSType.Any)) 
                            Some var, []
                    | WebSharperModule _ ->
                        let m, a = getAddress { address with Address = Hashed [] }
                        if isModule && not isBundle then
                            m, name :: a
                        else
                            getAddress { address with Module = CurrentModule }
                    | ImportedModule _ ->
                        let m, a = getAddress { address with Address = Hashed [] }
                        m, [ name ]
                | name :: r ->
                    let m, a = getAddress { address with Address = Hashed r }
                    m, name :: a
            addresses.Add(address, res)
            res
    
    let getModule m = getAddress { Module = m; Address = Hashed [] } |> fst 

    for r in resources do
        match r with
        | :? R.IDownloadableResource as d ->
            for m in d.GetImports() do
                if m.EndsWith ".js" then
                    getModule (JavaScriptFile (m.Substring(0, m.Length - 3))) |> ignore
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
        let i = Id.New (n, str = true, typ = TSType t)
        let (|MaybeGeneric|) = function
            | TSType.Generic(t, gen) -> t, gen
            | x -> x, []
        let exp =
            match expr, t with
            | Function(args, _, body), MaybeGeneric(TSType.Function(_, targs, _, tr), gs) ->
                let args =
                    match args, targs with
                    | [_], [] -> []
                    | p -> p ||> List.map2 (fun a (t, _) -> a.WithType(Some(TSType t)))
                let i = i.WithType(Some (TSType tr))
                FuncDeclaration(i, args, body, gs)
            | Function(args, ret, body), _ ->
                FuncDeclaration(i.WithType(ret), args, body, [])
            | _ -> VarDeclaration (i, expr)
        addExport exp

    let packageByName (a: Address) f =
        let n = toNamespaceWithName a
        addExport <| f n

    let classes = Dictionary(current.Classes)

    let allClasses = MergedDictionary(refMeta.Classes, current.Classes)
    let allInterfaces =  MergedDictionary(refMeta.Interfaces, current.Interfaces)

    let rec withoutMacros info =
        match info with
        | M.Macro (_, _, Some fb) -> withoutMacros fb
        | _ -> info 

    let tsTypeOfAddress (a: Address) =
        let t = a.Address.Value |> List.rev
        match a.Module with
        | StandardLibrary
        | JavaScriptFile _
        | CurrentModule -> TSType.Named t
        | WebSharperModule _ ->
            if isBundle then TSType.Named t else
            match getModule a.Module with
            | Some v ->
                TSType.Imported(v, t)
            | None ->
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
        if Option.isSome proto || u.Cases |> List.exists (fun uc -> not uc.Kind.IsConstant) then
            toNamespace addr.Address.Value
        proto |> Option.iter (fun (baseType, impls, members, gen) ->
            let mutable numArgs = 0
            let strId x = Id.New(x, str = true)
            let specCtors =
                u.Cases |> List.mapi (fun tag uc ->
                    match uc.Kind with
                    | M.NormalFSharpUnionCase fields -> Some fields
                    | M.SingletonFSharpUnionCase -> Some []
                    | M.ConstantFSharpUnionCase _ -> None
                    |> Option.map (fun fields ->
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
                )
                |> List.choose id
            let minUnionFields = 
                u.Cases |> Seq.map (fun uc ->
                    match uc.Kind with
                    | M.NormalFSharpUnionCase fields -> List.length fields
                    | M.SingletonFSharpUnionCase
                    | M.ConstantFSharpUnionCase _ -> 0
                ) |> Seq.min
            let genCtor =
                let args =
                    (strId "$", Modifiers.Public)
                    :: List.init numArgs (fun i -> Id.New("$" + string i, str = true, opt = (i >= minUnionFields)), Modifiers.Public)
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
                    let tag = ClassProperty (false, "$", TSType.Basic (string tag), false)
                    let mem =
                        fields |> List.mapi (fun i f ->
                            ClassProperty (false, "$" + string i, tsTypeOf gsArr f.UnionFieldType, false)
                        )
                    addExport <| Interface(uc.Name, Option.toList unionClass, tag :: mem, gen)
                    TSType.Basic (unionNested + uc.Name) |> addGenerics gen
                match uc.Kind with
                | M.NormalFSharpUnionCase uci -> case uci
                | M.ConstantFSharpUnionCase v -> TSType.Basic v.TSType
                | M.SingletonFSharpUnionCase -> case []
            )
        match addr.Address.Value with
        | n :: a ->
            toNamespace a
            addExport <| Alias ((TSType.Basic n |> addGenerics gen), TSType.Union cases)
        | _ -> failwith "empty address for union type"

    let packageRecord fields addr (t: TypeDefinition) gsArr =
        let fields =
            fields |> List.map (fun f ->
                let t =
                    if f.Optional then
                        match f.RecordFieldType with
                        | ConcreteType td -> td.Generics.[0]
                        | _ -> failwith "OptionalField on a field not of type option<_>"
                    else
                        f.RecordFieldType
                    |> tsTypeOf gsArr
                ClassProperty(false, f.JSName, t, f.Optional)
            )
        packageByName addr <| fun n -> Interface(n, [], fields, getGenerics 0 (List.ofArray gsArr))

    let rec packageClass (t: TypeDefinition) (classAddress: Address) (ct: CustomTypeInfo) (c: M.ClassInfo) =

        match c.BaseClass with
        | Some { Entity = b } ->
            match classes.TryFind b with
            | Some (a, ct, Some bc) ->
                classes.Remove b |> ignore
                packageClass b a ct bc
            | _ -> ()
        | _ -> ()

        let members = ResizeArray<Statement>()
        
        let smem (a: Address) inClass inNamespace =
            match classAddress.Address.Value, a.Address.Value with
            | _::catl, ahd::atl when catl = atl ->
                members.Add (inClass ahd)
            | _ -> addStatic a (inNamespace())

        let gsArr = Array.ofList c.Generics

        for f, _, t in c.Fields.Values do
            let typ = tsTypeOf gsArr t
            match f with
            | M.InstanceField n ->
                members.Add (ClassProperty (false, n, typ, false))
            | M.OptionalField n ->
                members.Add (ClassProperty (false, n, typ, true))
            | M.StaticField a ->
                smem a (fun n -> ClassProperty (true, n, typ, false)) (fun () -> Undefined, typ)
            | _ -> ()

        match c.StaticConstructor with
        | Some(_, GlobalAccess { Module = JavaScriptFile "Runtime"; Address = a }) when a.Value = [ "ignore" ] -> ()
        | Some(ccaddr, body) ->
            let body = BodyTransformer(tsTypeOf gsArr, getAddress).TransformExpression(JSRuntime.Cctor(body))
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
        let thisTSTypeDef = lazy tsTypeOf gsArr (NonGenericType t)
        let cgen = getGenerics 0 c.Generics
        let thisTSType = lazy (thisTSTypeDef.Value |> addGenerics cgen) 

        let mem (m: Method) info gc opts intfGen body =
            let gsArr = Array.append gsArr (Array.ofList gc)
            let getSignature isInstToStatic =         
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
                            tsTypeOf gsArr (NonGenericType t) :: (typeOfParams opts gsArr p)
                        else typeOfParams opts gsArr p
                    TSType.Lambda(pts, tsTypeOf gsArr r)
                | _ ->
                    tsTypeOf gsArr m.Value.ReturnType

            let body = BodyTransformer(tsTypeOf gsArr, getAddress).TransformExpression(body)
            let mgen = getGenerics cgenl gc
            let getMember isStatic n =
                match IgnoreExprSourcePos body with
                | Function (args, _, b) ->
                    ClassMethod(isStatic, n, args, Some b, getSignature false |> addGenerics mgen)
                | Undefined ->
                    let args = m.Value.Parameters |> List.map (fun _ -> Id.New(mut = false))
                    ClassMethod(isStatic, n, args, None, TSType.Any |> addGenerics mgen)
                | _ ->
                    ClassMethod(isStatic, n, [], Some (Return errorPlaceholder), TSType.Any)
            match withoutMacros info with
            | M.Instance mname ->
                members.Add (getMember false mname)
            | M.Static maddr ->
                smem maddr (getMember true) (fun () -> body, getSignature false |> addGenerics (cgen @ mgen))
            | M.AsStatic maddr ->
                smem maddr (getMember true) (fun () -> body, getSignature true |> addGenerics (cgen @ mgen))
            | _ -> ()
                    
        for KeyValue(m, (info, opts, gc, body)) in c.Methods do
            mem m info gc opts None body 
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
        for KeyValue((i, m), (info, body)) in c.Implementations do
            let intfGen, mParam = 
                match interfaceInfos.Value.TryGetValue i with
                | true, (intf, intfGen) ->
                    match m.Value.Generics with
                    | 0 -> intfGen
                    | mgen -> Array.append intfGen (Array.init mgen (fun i -> TypeParameter (cgenl + i)))
                    , snd intf.Methods.[m]
                | _ ->
                    match baseClassInfos.Value.TryGetValue i with
                    | true, (cls, clsGen) ->
                        match m.Value.Generics with
                        | 0 -> clsGen
                        | mgen -> Array.append clsGen (Array.init mgen (fun i -> TypeParameter (cgenl + i)))
                        , 
                        let _, _, mg, _ = cls.Methods.[m]
                        mg
                    | _ ->
                        if i = t then
                            Array.init (cgenl + m.Value.Generics) (fun i -> TypeParameter i), []
                        else
                            [||], [] // TODO: should this be an error? I don't think it should ever happen
            mem m info mParam M.Optimizations.None (Some intfGen) body

        let indexedCtors = Dictionary()
        
        for KeyValue(ctor, (info, opts, body)) in c.Constructors do
            let body = BodyTransformer(tsTypeOf gsArr, getAddress).TransformExpression(body)
            match withoutMacros info with
            | M.New ->
                if body <> Undefined then
                    match body with
                    | Function ([], _, I.Empty) 
                    | Function ([], _, I.ExprStatement(I.Application(I.Base, [], _))) -> 
                        ()
                    | Function (args, _, b) ->                  
                        let args = List.map (fun x -> x, Modifiers.None) args
                        let signature =
                            TSType.New(typeOfParams opts gsArr ctor.Value.CtorParameters, thisTSType.Value)
                        members.Add (ClassConstructor (args, Some b, signature))
                    | _ ->
                        failwithf "Invalid form for translated constructor"
            | M.NewIndexed i ->
                if body <> Undefined then
                    match body with
                    | Function (args, _, b) ->  
                        let index = Id.New("i: " + string i, str = true)
                        let allArgs = List.map (fun x -> x, Modifiers.None) (index :: args)
                        let signature =
                            TSType.New(TSType.Any :: (typeOfParams opts gsArr ctor.Value.CtorParameters), thisTSType.Value)
                        members.Add (ClassConstructor (allArgs, None, signature))
                        indexedCtors.Add (i, (args, b))
                    | _ ->
                        failwithf "Invalid form for translated constructor"
            | M.Static maddr 
            | M.AsStatic maddr ->
                let signature =
                    TSType.Lambda(typeOfParams opts gsArr ctor.Value.CtorParameters, thisTSType.Value)
                
                smem maddr 
                    (fun n ->
                        match IgnoreExprSourcePos body with
                        | Function (args, _, b) ->
                            ClassMethod(true, n, args, Some b, signature)
                        | _ -> failwith "unexpected form for class constructor"
                    ) 
                    (fun () -> body, signature |> addGenerics cgen)
            | _ -> ()

        let baseType = 
            match c.BaseClass with
            | Some b when b.Entity.Value.FullName = "System.Object" -> Some (TSType.Named [ "WebSharper"; "Obj" ]) 
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

        match ct with
        | M.FSharpRecordInfo r when not c.HasWSPrototype && Option.isNone c.Type ->
            packageRecord r classAddress t gsArr
        | M.FSharpUnionInfo u when Option.isNone c.Type ->
            packageUnion u classAddress (Some (baseType, impls, List.ofSeq members, gen)) gsArr
        | _ ->
            if c.HasWSPrototype then
                packageByName classAddress <| fun n -> Class(n, baseType, impls, List.ofSeq members, gen)
            
        if c.IsStub then
            // import addresses for stub classes
            getAddress { classAddress with Module = JavaScriptFile "" } |> ignore

    let packageUnannotatedCustomType (t: TypeDefinition) (addr: Address) (c: CustomTypeInfo) =
        let numGenerics =
            try
                t.Value.FullName.Split('.', '+') |> Array.sumBy (fun n ->
                    match n.IndexOf '`' with
                    | -1 -> 0
                    | i -> int (n.Substring(i + 1))
                )
            with _ ->
                failwithf "failed to get generics count of %s" t.Value.FullName
        let gsArr = Array.init numGenerics (fun _ -> GenericParam.None)
        match c with
        | CustomTypeInfo.FSharpRecordInfo r -> packageRecord r addr t gsArr
        | CustomTypeInfo.FSharpUnionInfo u -> packageUnion u addr None gsArr
        | CustomTypeInfo.FSharpAnonRecordInfo _
        | CustomTypeInfo.FSharpUnionCaseInfo _
        | CustomTypeInfo.DelegateInfo _
        | CustomTypeInfo.EnumInfo _
        | CustomTypeInfo.StructInfo
        | CustomTypeInfo.NotCustomType -> ()
    
    while classes.Count > 0 do
        let (KeyValue(t, (a, ct, c))) = Seq.head classes
        classes.Remove t |> ignore
        match c with
        | Some c -> packageClass t a ct c
        | None -> packageUnannotatedCustomType t a ct
    
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

        let rec methodNames (i:InterfaceInfo) =
            Seq.append (i.Methods.Values |> Seq.map fst) (i.Extends |> Seq.collect (fun ie -> 
                match allInterfaces.TryFind ie.Entity with
                | Some i -> methodNames i
                | _ -> Seq.empty
            ))

        packageByName i.Address <| fun n ->
            let x = Id.New "x"
            let shortestName = methodNames i |> Seq.minBy String.length
            let check = Binary(Value (String shortestName), BinaryOperator.``in``, Var x)
            let returnType = TSType.TypeGuard(x, tsTypeOfDef td |> addGenerics gen)
            let id = Id.New("is" + n, mut = false, str = true, typ = TSType returnType)
            FuncDeclaration(id, [x], Return check, gen)

    toNamespace []

    match entryPointStyle, entryPoint with
    | (OnLoadIfExists | ForceOnLoad), Some ep ->
        statements.Add <| ExprStatement (JSRuntime.OnLoad (Function([], None, ep)))
    | ForceImmediate, Some ep ->
        statements.Add ep
    | (ForceOnLoad | ForceImmediate), None ->
        failwith "Missing entry point or export. Add SPAEntryPoint attribute to a static method without arguments, or JavaScriptExport on types/methods to expose them."
    | OnLoadIfExists, None -> ()
    
    if statements.Count = 0 then 
        [] 
    else
        List.ofSeq (Seq.concat [ directives; declarations; statements ])

let readMapFileSources mapFile =
    match Json.Parse mapFile with
    | Json.Object fields ->
        let getString j = match j with Json.String s -> s | _ -> failwith "string expected in map file"
        let sources = fields |> List.pick (function "sources", Json.Array s -> Some (s |> List.map getString) | _ -> None)  
        let sourcesContent = fields |> List.pick (function "sourcesContent", Json.Array s -> Some (s |> List.map getString) | _ -> None)  
        List.zip sources sourcesContent
    | _ -> failwith "map file JSON should be an object"

let programToString pref (getWriter: unit -> WebSharper.Core.JavaScript.Writer.CodeWriter) statements =
    let program = statements |> TypeScriptWriter.transformProgram pref
    let writer = getWriter()
    WebSharper.Core.JavaScript.Writer.WriteProgram pref writer program
    writer.GetCodeFile(), writer.GetMapFile()
