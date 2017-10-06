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
    let libExtensions =
        let t n = TSType.Basic "Error" 
        [ 
            Interface ("Error", [], [ ClassProperty (false, "inner", TSType.Basic "Error")], 0) 
            Interface ("Object", [], [ ClassProperty (false, "setPrototypeOf", TSType.Any) ], 0)  
            Interface ("Math", [], [ ClassProperty (false, "trunc", TSType.Any) ], 0)  
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

    let globalAccessTransformer =
        { new Transformer() with
            override this.TransformGlobalAccess a = getAddress a
        }

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
            | _ -> TypedDeclaration (exp, t, 0)
        addStatement <| export texp

    let packageByName (a: Address) f =
        let n = toNamespaceWithName a
        addStatement <| export (f n)

    let packageCctor a expr =
        let n = toNamespaceWithName a
        match expr with
        | Function ([], body) ->
            let c = Id.New(n, str = true)
            let removeSelf = ExprStatement (VarSet (c, ItemGet(glob, Value (String "ignore"), Pure)))    
            let expr = Function([], Block [removeSelf; body])
            addStatement <| export (VarDeclaration(c, expr))   
        | _ ->
            failwithf "Static constructor must be a function"
            
    let classes = Dictionary(current.Classes)

    let rec withoutMacros info =
        match info with
        | M.Macro (_, _, Some fb) -> withoutMacros fb
        | _ -> info 

    let tsTypeOfDef (t: TypeDefinition) =
        match t.Value.FullName with
        | _ ->
            let cls =
                match refMeta.Classes.TryFind t with
                | Some _ as res -> res
                | _ -> current.Classes.TryFind t
            match cls with
            | Some c -> 
                match c.Type with
                | Some t -> t
                | _ ->
                    match c.Address with
                    | Some a -> TSType.Basic (a.Address.Value |> List.rev |> String.concat ".")
                    | _ -> TSType.Any
            | _ -> TSType.Any

    let rec tsTypeOf (t: Type) =
        match t with 
        | ConcreteType t -> 
            match t.Generics with
            | [] -> tsTypeOfDef t.Entity
            | g -> 
                let td = tsTypeOfDef t.Entity
                match td with
                | TSType.Basic _ ->
                    TSType.Generic(td, g |> List.map tsTypeOf)
                | _ -> td
        | ArrayType (t, a) -> TSType.Generic (TSType.Array, [ tsTypeOf t ])
        | TupleType (ts, _) -> TSType.Tuple (ts |> List.map tsTypeOf)
        | FSharpFuncType (a, r) -> 
            let ta =
                match a with
                | VoidType -> []
                | _ -> [tsTypeOf a]
            TSType.Lambda(ta, tsTypeOf r)
        | ByRefType t -> TSType.Any // TODO byrefs
        | VoidType -> TSType.Void
        | TypeParameter i 
        | StaticTypeParameter i -> TSType.Param ("T" + string i)
        | LocalTypeParameter -> TSType.Any

    let getGenerics (t: TypeDefinition) =
        let tn = t.Value.FullName
        let ns =
            match tn.LastIndexOf('.') with
            | -1 -> tn
            | i -> tn.Substring(i + 1)
        ns.Split('+') |> Array.sumBy (fun n ->
            match n.LastIndexOf('`') with
            | -1 -> 0
            | i -> n.Substring(i + 1) |> int
        )

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

    let rec packageClassInstances (t: TypeDefinition) (c: M.ClassInfo) =

        let classAddress = 
            if c.HasWSPrototype then c.Address else None

        match c.BaseClass with
        | Some b ->
            match classes.TryFind b with
            | Some bc ->
                classes.Remove b |> ignore
                packageClassInstances b bc
            | _ -> ()
        | _ -> ()

        let members = ResizeArray<Statement>()
        
        let smem (a: Address) inClass inNamespace =
            match classAddress with
            | Some ca when a.Address.Value.Tail = ca.Address.Value.Tail ->
                members.Add (inClass a.Address.Value.Head)     
            | _ -> addStatic a (inNamespace())

        for f, _, t in c.Fields.Values do
            let typ = tsTypeOf t
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

        let mem (m: Method) info body =
            let signature =
                TSType.Lambda(m.Value.Parameters |> List.map tsTypeOf, tsTypeOf m.Value.ReturnType)
            let getMember isStatic n =
                match IgnoreExprSourcePos body with
                | Function (args, b) ->
                    ClassMethod(isStatic, n, args, Some b, signature, m.Value.Generics)
                | Undefined ->
                    let args = m.Value.Parameters |> List.map (fun _ -> Id.New(mut = false))
                    ClassMethod(isStatic, n, args, None, TSType.Any, m.Value.Generics)
                | _ -> failwith "unexpected form for class member"
            match withoutMacros info with
            | M.Instance mname ->
                members.Add (getMember false mname)
            | M.Static maddr ->
                smem maddr (getMember true) (fun () -> body, signature)
            | _ -> ()
                    
        for KeyValue(m, (info, _, body)) in c.Methods do
            mem m info body 
        for KeyValue((_, m), (info, body)) in c.Implementations do
            mem m info body

        let indexedCtors = Dictionary()

        for KeyValue(ctor, (info, _, body)) in c.Constructors do
            match withoutMacros info with
            | M.New ->
                if body <> Undefined then
                    match body with
                    | Function ([], I.Empty) 
                    | Function ([], I.ExprStatement(I.Application(I.Base, [], _, _))) -> 
                        ()
                    | Function (args, b) ->                  
                        let signature =
                            TSType.New(ctor.Value.CtorParameters |> List.map tsTypeOf)
                        members.Add (ClassConstructor (args, Some b, signature))
                    | _ ->
                        failwithf "Invalid form for translated constructor"
            | M.NewIndexed i ->
                if body <> Undefined then
                    match body with
                    | Function (args, b) ->  
                        let index = Id.New("i: " + string i, str = true)
                        let signature =
                            TSType.New(TSType.Any :: (ctor.Value.CtorParameters |> List.map tsTypeOf))
                        members.Add (ClassConstructor (index :: args, None, signature))
                        indexedCtors.Add (i, (args, b))
                    | _ ->
                        failwithf "Invalid form for translated constructor"
            | M.Static maddr ->
                let signature =
                    TSType.Lambda(ctor.Value.CtorParameters |> List.map tsTypeOf, tsTypeOf (NonGenericType t))
                smem maddr 
                    (fun n ->
                        match IgnoreExprSourcePos body with
                        | Function (args, b) ->
                            ClassMethod(true, n, args, Some b, signature, 0)
                        | _ -> failwith "unexpected form for class constructor"
                    ) 
                    (fun () -> body, signature)
            | _ -> ()

        match classAddress with 
        | None -> ()
        | Some addr ->
            let baseType =
                match c.BaseClass with
                | None -> None
                | Some b ->
                    if b.Value.FullName = "System.Exception" then
                        Some (Global [ "Error" ])
                    else
                        let bCls =
                            match refMeta.Classes.TryFind b with
                            | Some _ as res -> res
                            | _ -> current.Classes.TryFind b
                        match bCls |> Option.bind (fun bc -> bc.Address) with
                        | Some ba -> Some (GlobalAccess ba)
                        | _ -> None


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
                members.Add (ClassConstructor (index :: cArgs, Some cBody, TSType.Any))   

            packageByName addr <| fun n -> Class(n, baseType, [], List.ofSeq members, getGenerics t)
            
        if c.IsStub then
            // import addresses for stub classes
            c.Address |> Option.iter (fun a -> getAddress { a with Module = JavaScriptFile "" } |> ignore)
            
    while classes.Count > 0 do
        let (KeyValue(t, c)) = classes |> Seq.head
        classes.Remove t |> ignore
        packageClassInstances t c
    
    let rec packageStatics (s: StaticMembers) =
        for a, e, t in s.Members do 
            package a e t
        
        for n in s.Namespaces.Values do
            packageStatics n

    packageStatics statics

    toNamespace []

    if isBundle then
        match current.EntryPoint with
        | Some ep -> addStatement <| ExprStatement (JSRuntime.OnLoad (Function([], ep)))
        | _ -> failwith "Missing entry point. Add an SPAEntryPoint attribute to a static method without arguments."
    
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
