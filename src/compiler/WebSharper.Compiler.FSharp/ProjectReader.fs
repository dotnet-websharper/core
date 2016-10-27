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

module WebSharper.Compiler.FSharp.ProjectReader

open Microsoft.FSharp.Compiler.SourceCodeServices
open System.Collections.Generic

open WebSharper.Core
open WebSharper.Core.AST
open WebSharper.Core.Metadata
open WebSharper.Compiler
open WebSharper.Compiler.NotResolved

module A = WebSharper.Compiler.AttributeReader
module QR = WebSharper.Compiler.QuotationReader

type FSIFD = FSharpImplementationFileDeclaration
type FSMFV = FSharpMemberOrFunctionOrValue

type private StartupCode = ResizeArray<Statement> * HashSet<string> 

type private N = NotResolvedMemberKind

type private SourceMemberOrEntity =
    | SourceMember of FSMFV * list<list<FSMFV>> * FSharpExpr
    | SourceEntity of FSharpEntity * ResizeArray<SourceMemberOrEntity>
    | SourceInterface of FSharpEntity 
    | InitAction of FSharpExpr

// fixes annotation for property setters, we don't want name coflicts
let fixMemberAnnot (getAnnot: _ -> A.MemberAnnotation) (x: FSharpEntity) (m: FSMFV) (a: A.MemberAnnotation) =
    if m.IsPropertySetterMethod then
        let po = 
            x.MembersFunctionsAndValues 
            |> Seq.tryFind (fun p -> p.IsProperty && p.HasSetterMethod && p.SetterMethod = m)
        match po with
        | Some p ->
            let pa = getAnnot p
            if pa.Kind = Some A.MemberKind.Stub then
                { a with Kind = pa.Kind; Name = pa.Name }
            else  
                let name =
                    match a.Name with
                    | Some n as an when an <> pa.Name || not p.HasGetterMethod -> Some n
                    | _ -> pa.Name |> Option.map (fun n -> if p.HasGetterMethod then "set_" + n else n)
                { a with Name = name }
        | _ -> a
    else a

let private transformInterface (sr: CodeReader.SymbolReader) parentAnnot (intf: FSharpEntity) =
    let methodNames = ResizeArray()
    let annot =
       sr.AttributeReader.GetTypeAnnot(parentAnnot, intf.Attributes)
    let def =
        match annot.ProxyOf with
        | Some d -> d 
        | _ -> sr.ReadTypeDefinition intf

    for m in intf.MembersFunctionsAndValues do
        if not m.IsProperty then
            let mAnnot =
                sr.AttributeReader.GetMemberAnnot(annot, m.Attributes)
                |> fixMemberAnnot (fun a -> sr.AttributeReader.GetMemberAnnot(annot, a.Attributes)) intf m
            let md = 
                match sr.ReadMember m with
                | Member.Method (_, md) -> md
                | _ -> failwith "invalid interface member"
            methodNames.Add(md, mAnnot.Name)
    Some (def, 
        {
            StrongName = annot.Name 
            Extends = intf.DeclaredInterfaces |> Seq.map (fun i -> sr.ReadTypeDefinition i.TypeDefinition) |> List.ofSeq
            NotResolvedMethods = List.ofSeq methodNames 
        }
    )

let private isILClass (e: FSharpEntity) =
    e.IsClass || e.IsFSharpExceptionDeclaration || e.IsFSharpModule || e.IsFSharpRecord || e.IsFSharpUnion || e.IsValueType

let private isResourceType (sr: CodeReader.SymbolReader) (e: FSharpEntity) =
    e.AllInterfaces |> Seq.exists (fun i ->
        sr.ReadTypeDefinition i.TypeDefinition = Definitions.IResource
    )

let isAugmentedFSharpType (e: FSharpEntity) =
    (e.IsFSharpRecord || e.IsFSharpUnion || e.IsFSharpExceptionDeclaration)
    && not (
        e.Attributes |> Seq.exists (fun a ->
            a.AttributeType.FullName = "Microsoft.FSharp.Core.DefaultAugmentationAttribute"
            && not (snd a.ConstructorArguments.[0] :?> bool)
        )
    )

let private transformInitAction (sc: Lazy<_ * StartupCode>) (comp: Compilation) (sr: CodeReader.SymbolReader) (annot: A.TypeAnnotation) a =
    if annot.IsJavaScript then
        let _, (statements, _) = sc.Value
        let env = CodeReader.Environment.New ([], [], comp, sr)  
        statements.Add (CodeReader.transformExpression env a |> ExprStatement)

let rec private transformClass (sc: Lazy<_ * StartupCode>) (comp: Compilation) (sr: CodeReader.SymbolReader) parentAnnot (cls: FSharpEntity) members =
    let thisDef = sr.ReadTypeDefinition cls
    
    let annot = 
        sr.AttributeReader.GetTypeAnnot(parentAnnot, cls.Attributes)

    if isResourceType sr cls then
        if comp.HasGraph then
            let thisRes = comp.Graph.AddOrLookupNode(ResourceNode thisDef)
            for req in annot.Requires do
                comp.Graph.AddEdge(thisRes, ResourceNode req)
        None
    else    
    
    let def, proxied =
        match annot.ProxyOf with
        | Some p -> 
            comp.AddProxy(thisDef, p)
            if cls.Accessibility.IsPublic then
                comp.AddWarning(Some (CodeReader.getRange cls.DeclarationLocation), SourceWarning "Proxy type should not be public")
            let proxied =
                let t = Reflection.LoadTypeDefinition p
                t.GetMembers(Reflection.AllPublicMethodsFlags) |> Seq.choose Reflection.ReadMember
                |> Seq.collect (fun m ->
                    match m with
                    | Member.Override (_, me) -> [ Member.Method (true, me); m ]
                    | _ -> [ m ]
                ) |> HashSet
            p, Some proxied
        | _ -> thisDef, None

    let clsMembers = ResizeArray()
    
    let getUnresolved (mAnnot: A.MemberAnnotation) kind compiled expr = 
        {
            Kind = kind
            StrongName = mAnnot.Name
            Macros = mAnnot.Macros
            Generator = 
                match mAnnot.Kind with
                | Some (A.MemberKind.Generated (g, p)) -> Some (g, p)
                | _ -> None
            Compiled = compiled 
            Pure = mAnnot.Pure
            Body = expr
            Requires = mAnnot.Requires
        }

    let addMethod (mem: option<FSMFV * Member>) mAnnot (def: Method) kind compiled expr =
        match proxied, mem with
        | Some ms, Some (mem, memdef) ->
            if not <| ms.Contains memdef then
                let candidates =
                    let n = def.Value.MethodName
                    match memdef with
                    | Member.Method (i, _) ->
                        ms |> Seq.choose (fun m ->
                            match m with
                            | Member.Method (mi, m) -> if mi = i && m.Value.MethodName = n then Some (string m.Value) else None
                            | Member.Override (_, m) -> if i && m.Value.MethodName = n then Some (string m.Value) else None 
                            | _ -> None 
                        )    
                    | Member.Override _ ->
                        ms |> Seq.choose (fun m ->
                            match m with
                            | Member.Method (true, m) -> if m.Value.MethodName = n then Some (string m.Value) else None
                            | Member.Override (_, m) -> if m.Value.MethodName = n then Some (string m.Value) else None 
                            | _ -> None 
                        )    
                    | Member.Implementation (intf, _) ->
                        ms |> Seq.choose (fun m ->
                            match m with
                            | Member.Implementation (i, m) -> if i = intf && m.Value.MethodName = n then Some (i.Value.FullName + string m.Value) else None 
                            | _ -> None 
                        )  
                    | _ -> Seq.empty  
                    |> List.ofSeq
                if List.isEmpty candidates then
                    if not (mem.Accessibility.IsPrivate || mem.Accessibility.IsInternal) then
                        let msg = "Proxy member do not match any member names of target class."
                        comp.AddWarning(Some (CodeReader.getRange mem.DeclarationLocation), SourceWarning msg)
                else 
                    let msg = sprintf "Proxy member do not match any member signatures of target class. Current: %s, candidates: %s" (string def.Value) (String.concat ", " candidates)
                    comp.AddWarning(Some (CodeReader.getRange mem.DeclarationLocation), SourceWarning msg)
        | _ -> ()
        clsMembers.Add (NotResolvedMember.Method (def, (getUnresolved mAnnot kind compiled expr)))
        
    let addConstructor (mem: option<FSMFV * Member>) mAnnot (def: Constructor) kind compiled expr =
        match proxied, mem with
        | Some ms, Some (mem, memdef) ->
//            if mem.Accessibility.IsPublic then
                if def.Value.CtorParameters.Length > 0 && not (ms.Contains memdef) then
                    let candidates = 
                        ms |> Seq.choose (function Member.Constructor c -> Some c | _ -> None)
                        |> Seq.map (fun m -> string m.Value) |> List.ofSeq
                    if not (mem.Accessibility.IsPrivate || mem.Accessibility.IsInternal) then
                        let msg = sprintf "Proxy constructor do not match any constructor signatures of target class. Current: %s, candidates: %s" (string def.Value) (String.concat ", " candidates)
                        comp.AddWarning(Some (CodeReader.getRange mem.DeclarationLocation), SourceWarning msg)
        | _ -> ()
        clsMembers.Add (NotResolvedMember.Constructor (def, (getUnresolved mAnnot kind compiled expr)))

    let annotations = Dictionary ()
        
    let rec getAnnot x : A.MemberAnnotation =
        match annotations.TryFind (x: FSharpMemberOrFunctionOrValue) with
        | Some a -> a
        | _ -> 
            let a = 
                sr.AttributeReader.GetMemberAnnot(annot, x.Attributes)
                |> fixMemberAnnot getAnnot cls x
            annotations.Add(x, a)
            a

    let stubs = HashSet()

    for meth in cls.MembersFunctionsAndValues do
        if meth.IsProperty then () else
        let mAnnot = getAnnot meth
        
        let error m = 
            comp.AddError(Some (CodeReader.getRange meth.DeclarationLocation), SourceError m)

        match mAnnot.Kind with
        | Some A.MemberKind.Stub ->
            let memdef = sr.ReadMember meth
            match memdef with
            | Member.Method (isInstance, mdef) ->
                let expr, err = Stubs.GetMethodInline annot mAnnot isInstance mdef
                err |> Option.iter error
                stubs.Add memdef |> ignore
                addMethod (Some (meth, memdef)) mAnnot mdef N.Inline true expr
            | Member.Constructor cdef ->
                let expr, err = Stubs.GetConstructorInline annot mAnnot cdef
                err |> Option.iter error
                addConstructor (Some (meth, memdef)) mAnnot cdef N.Inline true expr
            | Member.Implementation(_, _) -> error "Implementation method can't have Stub attribute"
            | Member.Override(_, _) -> error "Override method can't have Stub attribute"
            | Member.StaticConstructor -> error "Static constructor can't have Stub attribute"
        | Some A.MemberKind.JavaScript when meth.IsDispatchSlot -> 
            let memdef = sr.ReadMember meth
            match memdef with
            | Member.Method (isInstance, mdef) ->
                if not isInstance then failwith "Abstract method should not be static" 
                addMethod (Some (meth, memdef)) mAnnot mdef N.Abstract true Undefined
            | _ -> failwith "Member kind not expected for astract method"
        | Some (A.MemberKind.Remote rp) ->
            let memdef = sr.ReadMember meth
            match memdef with
            | Member.Method (isInstance, mdef) ->
                let remotingKind =
                    match mdef.Value.ReturnType with
                    | VoidType -> RemoteSend
                    | ConcreteType { Entity = e } when e = Definitions.Async -> RemoteAsync
                    | ConcreteType { Entity = e } when e = Definitions.Task || e = Definitions.Task1 -> RemoteTask
                    | _ -> RemoteSync
                let isCsrfProtected t = true // TODO
                let rp = rp |> Option.map (fun t -> t, isCsrfProtected t)
                let handle = 
                    comp.GetRemoteHandle(
                        def.Value.FullName + "." + mdef.Value.MethodName,
                        mdef.Value.Parameters,
                        mdef.Value.ReturnType
                    )
                addMethod (Some (meth, memdef)) mAnnot mdef (N.Remote(remotingKind, handle, rp)) true Undefined
            | _ -> error "Only methods can be defined Remote"
        | _ -> ()

    let fsharpSpecific = cls.IsFSharpUnion || cls.IsFSharpRecord || cls.IsFSharpExceptionDeclaration

    let clsTparams =
        lazy 
        cls.GenericParameters |> Seq.mapi (fun i p -> p.Name, i) |> Map.ofSeq

    for m in members do
        match m with
        | SourceMember (meth, args, expr) ->        
            if meth.IsProperty || (fsharpSpecific && meth.IsCompilerGenerated) then () else

            let mAnnot = getAnnot meth
            
            let getArgsAndThis() =
                let a, t =
                    args |> List.concat
                    |> function
                    | t :: r when (t.IsMemberThisValue || t.IsConstructorThisValue) && not meth.IsExtensionMember -> r, Some t
                    | a -> a, None

                a
                |> function 
                | [ u ] when CodeReader.isUnit u.FullType -> []
                | a -> a
                , t

            let getVarsAndThis() =
                let a, t = getArgsAndThis()
                a |> List.map (fun p -> CodeReader.namedId p),
                t |> Option.map (fun p -> CodeReader.namedId p)
               
            let error m = comp.AddError(Some (CodeReader.getRange meth.DeclarationLocation), SourceError m)
            let warn m = comp.AddWarning(Some (CodeReader.getRange meth.DeclarationLocation), SourceWarning m)

            match mAnnot.Kind with
            | Some A.MemberKind.Stub
            | Some (A.MemberKind.Remote _) -> ()
            | Some kind ->
                let memdef = sr.ReadMember meth

                if stubs.Contains memdef then () else
                let getBody isInline = 
                    try
                        let fromRD = 
                            let hasRD =
                                meth.Attributes |> Seq.exists (fun a -> a.AttributeType.FullName = "Microsoft.FSharp.Core.ReflectedDefinitionAttribute")
                            if hasRD then
                                let info =
                                    match memdef with
                                    | Member.Method (_, mdef) 
                                    | Member.Override (_, mdef) 
                                    | Member.Implementation (_, mdef) ->
                                        Reflection.LoadMethod def mdef :> System.Reflection.MethodBase
                                    | Member.Constructor cdef ->
                                        Reflection.LoadConstructor def cdef :> _
                                    | Member.StaticConstructor ->
                                        (Reflection.LoadTypeDefinition def).GetConstructors(System.Reflection.BindingFlags.Static).[0] :> _
                                WebSharper.Compiler.ReflectedDefinitionReader.readReflected comp info 
                            else None
                        match fromRD with
                        | Some rd ->
                            FixThisScope().Fix(rd)
                        | _ ->
                        let a, t = getArgsAndThis()
                        let argsAndVars = 
                            [
                                match t with
                                | Some t ->
                                    yield t, (CodeReader.namedId t, CodeReader.ThisArg)
                                | _ -> ()
                                for p in a ->    
                                    p, 
                                    (CodeReader.namedId p, 
                                        if CodeReader.isByRef p.FullType then CodeReader.ByRefArg else CodeReader.LocalVar)
                            ]
                        let tparams = meth.GenericParameters |> Seq.map (fun p -> p.Name) |> List.ofSeq 
                        let env = CodeReader.Environment.New (argsAndVars, tparams, comp, sr)  
                        let res =
                            let b = CodeReader.transformExpression env expr 
                            let b = 
                                match memdef with
                                | Member.Constructor _ -> 
                                    try CodeReader.fixCtor b
                                    with e ->
                                        let tryGetExprSourcePos expr =
                                            match expr with
                                            | ExprSourcePos (p, _) -> Some p
                                            | _ -> None
                                        comp.AddError(tryGetExprSourcePos b, SourceError e.Message)
                                        WebSharper.Compiler.Translator.errorPlaceholder
                                | _ -> b
                            let b = FixThisScope().Fix(b)      
                            // TODO : startupcode only for module values
                            if List.isEmpty args then 
                                if isInline then
                                    b
                                else
                                    let scDef, (scContent, scFields) = sc.Value   
                                    let name = Resolve.getRenamed meth.CompiledName scFields
                                    scContent.Add (ExprStatement (ItemSet(Self, Value (String name), b)))
                                    Lambda([], FieldGet(None, NonGeneric scDef, name))
                            else
                                let thisVar, vars =
                                    match argsAndVars with 
                                    | (_, (t, CodeReader.ThisArg)) :: a -> Some t, a |> List.map (snd >> fst)  
                                    | a -> None, a |> List.map (snd >> fst) 
                                let defValues = 
                                    Seq.zip (Seq.concat meth.CurriedParameterGroups) vars |> Seq.choose (fun (p, i) ->
                                        if p.Attributes |> Seq.exists (fun pa -> pa.AttributeType.FullName = "System.Runtime.InteropServices.OptionalAttribute") then
                                            let d =
                                                p.Attributes |> Seq.tryPick (fun pa -> 
                                                    if pa.AttributeType.FullName = "System.Runtime.InteropServices.DefaultParameterValueAttribute" then
                                                        Some (ReadLiteral (snd pa.ConstructorArguments.[0]) |> Value) 
                                                    else None
                                                )
                                            Some (i,
                                                match d with
                                                | Some v -> v
                                                | _ -> DefaultValueOf (sr.ReadType env.TParams p.Type)
                                            )
                                        else None
                                    )
                                    |> List.ofSeq
                                let b =
                                    if List.isEmpty defValues then b else
                                        Sequential [
                                            for i, v in defValues -> VarSet(i, Conditional (Var i ^== Undefined, v, Var i))
                                            yield b
                                        ]
                                if isInline then
                                    let b = 
                                        match thisVar with
                                        | Some t -> ReplaceThisWithVar(t).TransformExpression(b)
                                        | _ -> b
                                    makeExprInline (Option.toList thisVar @ vars) b
                                else 
                                    let returnsUnit =
                                        match memdef with
                                        | Member.Method (_, mdef)  
                                        | Member.Override (_, mdef) 
                                        | Member.Implementation (_, mdef) ->
                                            mdef.Value.ReturnType = VoidType
                                        | _ -> true
                                    if returnsUnit then
                                        Function(vars, ExprStatement b)
                                    else
                                        Lambda(vars, b)
                        res
                    with e ->
                        error (sprintf "Error reading definition: %s at %s" e.Message e.StackTrace)
                        WebSharper.Compiler.Translator.errorPlaceholder

                match memdef with
                | Member.Method (_, mdef) 
                | Member.Override (_, mdef) 
                | Member.Implementation (_, mdef) ->
                    let getKind() =
                        match memdef with
                        | Member.Method (isInstance , _) ->
                            if isInstance then N.Instance else N.Static  
                        | Member.Override (t, _) -> N.Override t 
                        | Member.Implementation (t, _) -> N.Implementation t
                        | _ -> failwith "impossible"
                    
                    let addModuleValueProp kind body =
                        if List.isEmpty args && meth.EnclosingEntity.IsFSharpModule then
                            let iBody = Call(None, NonGeneric def, Generic mdef (List.init mdef.Value.Generics TypeParameter), [])
                            // TODO : check proxy targets for module values
                            addMethod None mAnnot (Method { mdef.Value with MethodName = "get_" + mdef.Value.MethodName }) N.Inline false iBody    
                            if meth.IsMutable then 
                                let setm = 
                                    let me = mdef.Value
                                    Method {
                                        MethodName = "set_" + me.MethodName   
                                        Parameters = [ me.ReturnType ]
                                        ReturnType = VoidType
                                        Generics = 0
                                    }
                                let setb =
                                    match body with
                                    | Function([], Return (FieldGet(None, {Entity = scDef; Generics = []}, name))) ->
                                        let value = CodeReader.newId()                          
                                        Function ([value], (ExprStatement <| FieldSet(None, NonGeneric scDef, name, Var value)))
                                    | _ -> 
                                        error "unexpected form in module let body"
                                        Undefined
                                        //failwith "unexpected form in module let body"
                                addMethod None { mAnnot with Name = None } setm kind false setb    
                            true
                        else false

                    let addM = addMethod (Some (meth, memdef)) mAnnot mdef

                    let jsMethod isInline =
                        let kind = if isInline then N.Inline else getKind()
                        let body = getBody isInline                        
                        if addModuleValueProp kind body then
                            addMethod None mAnnot mdef kind false body  
                        else addM kind false body

                    let checkNotAbstract() =
                        if meth.IsDispatchSlot then
                            error "Abstract methods cannot be marked with Inline, Macro or Constant attributes."
                        else
                            match memdef with
                            | Member.Override _ -> 
                                if def <> Definitions.Obj then
                                    error "Override methods cannot be marked with Inline, Macro or Constant attributes."
                            | Member.Implementation _ ->
                                error "Interface implementation methods cannot be marked with Inline, Macro or Constant attributes."
                            | _ -> ()
                    match kind with
                    | A.MemberKind.NoFallback ->
                        checkNotAbstract()
                        addM N.NoFallback true Undefined
                    | A.MemberKind.Inline js ->
                        checkNotAbstract() 
                        let vars, thisVar = getVarsAndThis()
                        try 
                            let parsed = WebSharper.Compiler.Recognize.createInline thisVar vars mAnnot.Pure js
                            if addModuleValueProp N.Inline parsed then
                                addMethod None mAnnot mdef N.Inline true parsed   
                            else addM N.Inline true parsed
                        with e ->
                            error ("Error parsing inline JavaScript: " + e.Message)
                    | A.MemberKind.Constant c ->
                        checkNotAbstract() 
                        addM N.Inline true (Value c)                        
                    | A.MemberKind.Direct js ->
                        let vars, thisVar = getVarsAndThis()
                        try
                            let parsed = WebSharper.Compiler.Recognize.parseDirect thisVar vars js
                            addM (getKind()) true parsed
                        with e ->
                            error ("Error parsing direct JavaScript: " + e.Message)
                    | A.MemberKind.JavaScript ->
                        jsMethod false
                    | A.MemberKind.InlineJavaScript ->
                        checkNotAbstract()
                        jsMethod true
                    | A.MemberKind.OptionalField ->
                        if meth.IsPropertyGetterMethod then
                            let i = JSRuntime.GetOptional (ItemGet(Hole 0, Value (String meth.CompiledName.[4..])))
                            addM N.Inline true i
                        elif meth.IsPropertySetterMethod then  
                            let i = JSRuntime.SetOptional (Hole 0) (Value (String meth.CompiledName.[4..])) (Hole 1)
                            addM N.Inline true i
                        else error "OptionalField attribute not on property"
                    | A.MemberKind.Generated _ ->
                        addM (getKind()) false Undefined
                    | A.MemberKind.AttributeConflict m -> error m
                    | A.MemberKind.Remote _ 
                    | A.MemberKind.Stub -> failwith "should be handled previously"
                    if mAnnot.IsEntryPoint then
                        let ep = ExprStatement <| Call(None, NonGeneric def, NonGeneric mdef, [])
                        if comp.HasGraph then
                            comp.Graph.AddEdge(EntryPointNode, MethodNode (def, mdef))
                        comp.SetEntryPoint(ep)
                | Member.Constructor cdef ->
                    let addC = addConstructor (Some (meth, memdef)) mAnnot cdef
                    let jsCtor isInline =   
                        if isInline then 
                            addC N.Inline false (getBody true)
                        else
                            addC N.Constructor false (getBody false)
                    match kind with
                    | A.MemberKind.NoFallback ->
                        addC N.NoFallback true Undefined
                    | A.MemberKind.Inline js ->
                        let vars, thisVar = getVarsAndThis()
                        try
                            let parsed = WebSharper.Compiler.Recognize.createInline thisVar vars mAnnot.Pure js
                            addC N.Inline true parsed 
                        with e ->
                            error ("Error parsing inline JavaScript: " + e.Message)
                    | A.MemberKind.Direct js ->
                        let vars, thisVar = getVarsAndThis()
                        try
                            let parsed = WebSharper.Compiler.Recognize.parseDirect thisVar vars js
                            addC N.Static true parsed 
                        with e ->
                            error ("Error parsing direct JavaScript: " + e.Message)
                    | A.MemberKind.JavaScript -> jsCtor false
                    | A.MemberKind.InlineJavaScript -> jsCtor true
                    | A.MemberKind.Generated _ ->
                        addC N.Static false Undefined
                    | A.MemberKind.AttributeConflict m -> error m
                    | A.MemberKind.Remote _
                    | A.MemberKind.Stub -> failwith "should be handled previously"
                    | A.MemberKind.OptionalField
                    | A.MemberKind.Constant _ -> failwith "attribute not allowed on constructors"
                | Member.StaticConstructor ->
                    clsMembers.Add (NotResolvedMember.StaticConstructor (getBody false))
            | None 
            | _ -> ()
        | SourceEntity (ent, nmembers) ->
            transformClass sc comp sr annot ent nmembers |> Option.iter comp.AddClass   
        | SourceInterface i ->
            transformInterface sr annot i |> Option.iter comp.AddInterface
        | InitAction expr ->
            transformInitAction sc comp sr annot expr    

    if not annot.IsJavaScript && clsMembers.Count = 0 && annot.Macros.IsEmpty then None else

    let ckind = 
        if cls.IsFSharpModule then NotResolvedClassKind.Static
        elif annot.IsJavaScript && isAugmentedFSharpType cls then NotResolvedClassKind.FSharpType
        else NotResolvedClassKind.Class

    let hasWSPrototype =                
        match ckind with
        | NotResolvedClassKind.Static -> false
        | NotResolvedClassKind.FSharpType -> true
        | _ ->
            clsMembers
            |> Seq.exists (
                function
                | M.Constructor (_, nr)
                | M.Method (_, nr) ->
                    match nr.Kind with
                    | N.Instance 
                    | N.Abstract
                    | N.Constructor 
                    | N.Override _
                    | N.Implementation _ -> true
                    | _ -> false
                | _ -> false
            )

    if annot.IsJavaScript || hasWSPrototype then
        if cls.IsFSharpUnion then
            let usesNull =
                cls.UnionCases.Count < 4 // see TaggingThresholdFixedConstant in visualfsharp/src/ilx/EraseUnions.fs
                && cls.Attributes |> CodeReader.hasCompilationRepresentation CompilationRepresentationFlags.UseNullAsTrueValue
                && cls.UnionCases |> Seq.exists (fun c -> c.UnionCaseFields.Count = 0)

            let mutable nullCase = usesNull 

            let cases =
                cls.UnionCases
                |> Seq.map (fun case ->
                    let cAnnot = sr.AttributeReader.GetMemberAnnot(annot, case.Attributes)
                    let kind =
                        if nullCase && case.UnionCaseFields.Count = 0 then
                            nullCase <- false
                            ConstantFSharpUnionCase Null
                        else
                        // TODO: error on null case having another constant
                        // TODO: error on same constant on multiple cases 
                        match cAnnot.Kind with
                        | Some (A.MemberKind.Constant v) -> ConstantFSharpUnionCase v
                        | _ ->
                            NormalFSharpUnionCase (
    //                            cAnnot.Name,
                                case.UnionCaseFields
                                |> Seq.map (fun f ->
                                    {
                                        Name = f.Name
                                        UnionFieldType = sr.ReadType clsTparams.Value f.FieldType
                                        DateTimeFormat = 
                                            cAnnot.DateTimeFormat 
                                            |> List.tryPick (fun (target, format) -> if target = Some f.Name then Some format else None)
                                    }
                                )
                                |> List.ofSeq
                            )
                    let staticIs =
                        not usesNull || not (
                            case.Attributes
                            |> CodeReader.hasCompilationRepresentation CompilationRepresentationFlags.Instance
                        )
                    {
                        Name = case.Name
                        JsonName = cAnnot.Name
                        Kind = kind
                        StaticIs = staticIs
                    }
                )
                |> List.ofSeq
            
            let i =
                FSharpUnionInfo {
                    Cases = cases
                    NamedUnionCases = annot.NamedUnionCases
                    HasNull = usesNull && not nullCase
                }

            comp.AddCustomType(def, i)

        if cls.IsFSharpRecord || cls.IsFSharpExceptionDeclaration then
            let cdef =
                Hashed {
                    CtorParameters =
                        cls.FSharpFields |> Seq.map (fun f -> sr.ReadType clsTparams.Value f.FieldType) |> List.ofSeq
                }
            let body =
                let vars =
                    cls.FSharpFields |> Seq.map (fun f -> Id.New(f.Name, mut = false)) |> List.ofSeq
                let fields =
                    cls.FSharpFields |> Seq.map (fun f -> 
                        let fAnnot = sr.AttributeReader.GetMemberAnnot(annot, Seq.append f.FieldAttributes f.PropertyAttributes)
                    
                        match fAnnot.Name with Some n -> n | _ -> f.Name
                        , 
                        fAnnot.Kind = Some A.MemberKind.OptionalField && CodeReader.isOption f.FieldType
                    )
                    |> List.ofSeq
                let obj = 
                    let normalFields =
                        Seq.zip (fields) vars
                        |> Seq.choose (fun ((name, opt), v) -> if opt then None else Some (name, Var v))
                        |> List.ofSeq |> Object
                    if fields |> List.exists snd then
                        let o = CodeReader.newId()
                        Let(o, normalFields, 
                            Sequential [
                                for (name, opt), v in Seq.zip fields vars do
                                    if opt then yield JSRuntime.SetOptional (Var o) (Value (String name)) (Var v)
                                yield Var o
                            ]
                        )
                    else 
                        normalFields
                Lambda (vars, CopyCtor(def, obj))

            addConstructor None A.MemberAnnotation.BasicJavaScript cdef N.Static false body

            // properties

            for f in cls.FSharpFields do
                let recTyp = Generic def (List.init cls.GenericParameters.Count TypeParameter)
                let fTyp = sr.ReadType clsTparams.Value f.FieldType
            
                let getDef =
                    Hashed {
                        MethodName = "get_" + f.Name
                        Parameters = []
                        ReturnType = fTyp
                        Generics = 0
                    }

                let getBody = FieldGet(Some (Hole 0), recTyp, f.Name)
                
                addMethod None A.MemberAnnotation.BasicInlineJavaScript getDef N.Inline false getBody

                if f.IsMutable then
                    let setDef =
                        Hashed {
                            MethodName = "set_" + f.Name
                            Parameters = [ fTyp ]
                            ReturnType = VoidType
                            Generics = 0
                        }

                    let setBody = FieldSet(Some (Hole 0), recTyp, f.Name, Hole 1)
            
                    addMethod None A.MemberAnnotation.BasicInlineJavaScript setDef N.Inline false setBody

        if cls.IsFSharpRecord then

            let i = 
                cls.FSharpFields |> Seq.map (fun f ->
                    let fAnnot = sr.AttributeReader.GetMemberAnnot(annot, Seq.append f.FieldAttributes f.PropertyAttributes)
                    let isOpt = fAnnot.Kind = Some A.MemberKind.OptionalField && CodeReader.isOption f.FieldType
                    let fTyp = sr.ReadType clsTparams.Value f.FieldType

                    {
                        Name = f.Name
                        JSName = match fAnnot.Name with Some n -> n | _ -> f.Name // TODO : set in resolver instead
                        RecordFieldType = fTyp
                        DateTimeFormat = fAnnot.DateTimeFormat |> List.tryHead |> Option.map snd
                        Optional = isOpt
                    }
                )
                |> List.ofSeq |> FSharpRecordInfo    

            comp.AddCustomType(def, i)

    for f in cls.FSharpFields do
        let fAnnot = sr.AttributeReader.GetMemberAnnot(annot, Seq.append f.FieldAttributes f.PropertyAttributes)
        let nr =
            {
                StrongName = fAnnot.Name
                IsStatic = f.IsStatic
                IsOptional = fAnnot.Kind = Some A.MemberKind.OptionalField && CodeReader.isOption f.FieldType
                FieldType = sr.ReadType clsTparams.Value f.FieldType
            }
        clsMembers.Add (NotResolvedMember.Field (f.Name, nr))    

    let strongName =
        annot.Name |> Option.map (fun n ->
            if n.StartsWith "." then n.TrimStart('.') else
            if n.Contains "." then n else 
                let origName = thisDef.Value.FullName
                origName.[.. origName.LastIndexOf '.'] + n
        )   

    Some (
        def,
        {
            StrongName = strongName
            BaseClass = cls.BaseType |> Option.bind (fun t -> t.TypeDefinition |> sr.ReadTypeDefinition |> ignoreSystemObject)
            Requires = annot.Requires
            Members = List.ofSeq clsMembers
            Kind = ckind
            IsProxy = Option.isSome annot.ProxyOf
            Macros = annot.Macros
        }
    )

let transformAssembly (comp : Compilation) assemblyName (checkResults: FSharpCheckProjectResults) =   
    comp.AssemblyName <- assemblyName
    let sr = CodeReader.SymbolReader(comp)    
    
    let asmAnnot = sr.AttributeReader.GetAssemblyAnnot(checkResults.AssemblySignature.Attributes)

    let rootTypeAnnot = asmAnnot.RootTypeAnnot

    comp.AssemblyRequires <- asmAnnot.Requires
    comp.SiteletDefinition <- asmAnnot.SiteletDefinition

    comp.CustomTypesReflector <- A.reflectCustomType
    
    let lookupAssembly =
        lazy
        Map [
            yield assemblyName, checkResults.AssemblySignature
            for r in checkResults.ProjectContext.GetReferencedAssemblies() do
                yield r.SimpleName, r.Contents 
        ]

    let lookupTypeDefinition (typ: TypeDefinition) =
        let t = typ.Value
        let path = t.FullName.Split('+')
        let mutable res =
            lookupAssembly.Value.[t.Assembly].Entities |> Seq.tryFind (fun e ->
                e.FullName = path.[0]
            )
        for i = 1 to path.Length - 1 do
            if res.IsSome then
                res <- res.Value.NestedEntities |> Seq.tryFind (fun e -> e.CompiledName = path.[i])
        res 

    let readAttribute (a: FSharpAttribute) =
        sr.ReadTypeDefinition a.AttributeType,
        a.ConstructorArguments |> Seq.map (snd >> ParameterObject.OfObj) |> Array.ofSeq

    let lookupTypeAttributes (typ: TypeDefinition) =
        lookupTypeDefinition typ |> Option.map (fun e ->
            e.Attributes |> Seq.map readAttribute |> List.ofSeq
        )

    let lookupFieldAttributes (typ: TypeDefinition) (field: string) =
        lookupTypeDefinition typ |> Option.bind (fun e -> 
            e.FSharpFields |> Seq.tryFind (fun f -> f.Name = field) |> Option.map (fun f ->
                let tparams = e.GenericParameters |> Seq.mapi (fun i p -> p.Name, i) |> Map.ofSeq
                sr.ReadType tparams f.FieldType,
                Seq.append f.FieldAttributes f.PropertyAttributes |> Seq.map readAttribute |> List.ofSeq
            ) 
        )

    comp.LookupTypeAttributes <- lookupTypeAttributes
    comp.LookupFieldAttributes <- lookupFieldAttributes 
                                             
    for file in checkResults.AssemblyContents.ImplementationFiles do
        let fileName =
            lazy
            match file.Declarations.Head with
            | FSIFD.Entity (a, _) -> a.DeclarationLocation.FileName
            | FSIFD.MemberOrFunctionOrValue (_, _, c) -> c.Range.FileName
            | FSIFD.InitAction a -> a.Range.FileName
        let sc =
            lazy
            let name = "StartupCode$" + assemblyName.Replace('.', '_') + "$" + (System.IO.Path.GetFileNameWithoutExtension fileName.Value).Replace('.', '_')
            let def =
                TypeDefinition {
                    Assembly = assemblyName
                    FullName = name
                }
            def, 
            (ResizeArray(), HashSet() : StartupCode)

        let topLevelTypes = ResizeArray<SourceMemberOrEntity>()
        let types = Dictionary<FSharpEntity, ResizeArray<SourceMemberOrEntity>>()
//        let interfaces = ResizeArray<FSharpEntity>()
        let rec getTypesWithMembers (parentMembers: ResizeArray<_>) d =
            match d with
            | FSIFD.Entity (a, b) ->
                if not a.IsFSharpAbbreviation then
                    if a.IsInterface then
                        parentMembers.Add (SourceInterface a)
                    elif isILClass a then 
                        let ms = ResizeArray()
                        parentMembers.Add (SourceEntity (a, ms))
                        try
                            types.Add (a, ms)
                        with _ ->
                            comp.AddError(Some (CodeReader.getRange a.DeclarationLocation), SourceError "Duplicate type definition")
                        b |> List.iter (getTypesWithMembers ms)
                    else
                        b |> List.iter (getTypesWithMembers parentMembers)
            | FSIFD.MemberOrFunctionOrValue (a, b, c) -> 
                types.[a.EnclosingEntity].Add(SourceMember(a, b, c))
            | FSIFD.InitAction a ->
                parentMembers.Add (InitAction a)

        file.Declarations |> Seq.iter (getTypesWithMembers topLevelTypes)

        for t in topLevelTypes do
            match t with
            | SourceMember _ -> failwith "impossible: top level member"
            | InitAction _ -> failwith "impossible: top level init action"
//                transformInitAction sc comp sr rootTypeAnnot a
            | SourceEntity (c, m) ->
                transformClass sc comp sr rootTypeAnnot c m |> Option.iter comp.AddClass
            | SourceInterface i ->
                transformInterface sr rootTypeAnnot i |> Option.iter comp.AddInterface
            
        let getStartupCodeClass (def: TypeDefinition, sc: StartupCode) =
            let statements, fields = sc            
            let cctor = Function ([], Block (List.ofSeq statements))
            let members =
                [
                    for f in fields -> 
                        NotResolvedMember.Field(f, 
                            {
                                StrongName = None
                                IsStatic = true
                                IsOptional = false
                                FieldType = VoidType // field types are only needed for adding code dependencies for activator
                            } 
                        )
                    yield NotResolvedMember.StaticConstructor cctor
                ]
            
            def,
            {
                StrongName = None
                BaseClass = None
                Requires = [] //annot.Requires
                Members = members
                Kind = NotResolvedClassKind.Static
                IsProxy = false
                Macros = []
            }
            
        if sc.IsValueCreated then
            getStartupCodeClass sc.Value |> comp.AddClass

    comp.Resolve()

    comp
