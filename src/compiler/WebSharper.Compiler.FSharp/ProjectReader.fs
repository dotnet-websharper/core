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

let transformInterface parentAnnot (intf: FSharpEntity) =
    let methodNames = ResizeArray()
    let annot =
        CodeReader.attrReader.GetTypeAnnot(parentAnnot, intf.Attributes)
    let def =
        match annot.ProxyOf with
        | Some d -> d 
        | _ -> CodeReader.getTypeDefinition intf
    for m in intf.MembersFunctionsAndValues do
        if not m.IsProperty then
            let mAnnot = CodeReader.attrReader.GetMemberAnnot(annot, m.Attributes)
            let md = 
                match CodeReader.getMember m with
                | Member.Method (_, md) -> md
                | _ -> failwith "invalid interface member"
            methodNames.Add(md, mAnnot.Name)
    Some (def, 
        {
            StrongName = annot.Name 
            Extends = intf.DeclaredInterfaces |> Seq.map (fun i -> CodeReader.getTypeDefinition i.TypeDefinition) |> List.ofSeq
            NotResolvedMethods = List.ofSeq methodNames 
        }
    )

let isILClass (e: FSharpEntity) =
    e.IsClass || e.IsFSharpExceptionDeclaration || e.IsFSharpModule || e.IsFSharpRecord || e.IsFSharpUnion || e.IsValueType

let isResourceType (e: FSharpEntity) =
    e.AllInterfaces |> Seq.exists (fun i ->
        CodeReader.getTypeDefinition i.TypeDefinition = Definitions.IResource
    )

let isAugmentedFSharpType (e: FSharpEntity) =
    (e.IsFSharpRecord || e.IsFSharpUnion || e.IsFSharpExceptionDeclaration)
    && not (
        e.Attributes |> Seq.exists (fun a ->
            a.AttributeType.FullName = "Microsoft.FSharp.Core.DefaultAugmentationAttribute"
            && not (snd a.ConstructorArguments.[0] :?> bool)
        )
    )

let basicInstanceField =
    { 
        StrongName = None
        IsStatic = false
        IsOptional = false 
    }

let rec private transformClass (sc: Lazy<_ * StartupCode>) (comp: Compilation) parentAnnot (cls: FSharpEntity) members =
    let thisDef = CodeReader.getTypeDefinition cls
    
    let annot = 
        CodeReader.attrReader.GetTypeAnnot(parentAnnot, cls.Attributes)

    if isResourceType cls then
        let thisRes = comp.Graph.AddOrLookupNode(ResourceNode thisDef)
        for req in annot.Requires do
            comp.Graph.AddEdge(thisRes, ResourceNode req)
        None
    else    
    
    let def =
        match annot.ProxyOf with
        | Some p -> 
            comp.AddProxy(thisDef, p)
            if cls.Accessibility.IsPublic then
                comp.AddWarning(Some (CodeReader.getRange cls.DeclarationLocation), SourceWarning "Proxy type should not be public")
            p
        | _ -> thisDef

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

    let addMethod mAnnot def kind compiled expr =
        clsMembers.Add (NotResolvedMember.Method (def, (getUnresolved mAnnot kind compiled expr)))
        
    let addConstructor mAnnot def kind compiled expr =
        clsMembers.Add (NotResolvedMember.Constructor (def, (getUnresolved mAnnot kind compiled expr)))

    let annotations = Dictionary ()

    let propertiesWithSetter = 
        cls.MembersFunctionsAndValues |> Seq.filter (fun x -> x.IsProperty && x.HasSetterMethod)
        |> List.ofSeq
        
    let rec getAnnot x : A.MemberAnnotation =
        match annotations.TryFind (x: FSharpMemberOrFunctionOrValue) with
        | Some a -> a
        | _ -> 
            let a = CodeReader.attrReader.GetMemberAnnot(annot, x.Attributes)
            let a =
                if x.IsPropertySetterMethod then
                    match propertiesWithSetter |> List.tryFind (fun p -> p.SetterMethod = x) with
                    | None -> a
                    | Some p ->
                        let pa = getAnnot p
                        if pa.Kind = Some A.MemberKind.Stub then
                            { a with Kind = pa.Kind; Name = pa.Name }
                        else a
                else a
            annotations.Add(x, a)
            a

    let stubs = HashSet()

    let addStub memdef mAnnot def expr = 
        stubs.Add memdef |> ignore
        addMethod mAnnot def N.Inline true expr

    for meth in cls.MembersFunctionsAndValues do
        if meth.IsProperty then () else
        let mAnnot = getAnnot meth
        
        let isStub = mAnnot.Kind = Some A.MemberKind.Stub

        let error m = 
            comp.AddError(Some (CodeReader.getRange meth.DeclarationLocation), SourceError m)

        if isStub || (meth.IsDispatchSlot && mAnnot.Kind = Some A.MemberKind.JavaScript) then
            match CodeReader.getMember meth with
            | Member.Method (isInstance, mdef) as memdef ->
                let getItem() =
                    match mAnnot.Name with
                    | Some n -> n 
                    | _ -> mdef.Value.MethodName.[4 ..]
                    |> String |> Value
                if isInstance then                                 
                    if isStub && meth.IsPropertyGetterMethod then
                        addStub memdef mAnnot mdef (ItemGet(Hole 0, (getItem())))    
                    elif isStub && meth.IsPropertySetterMethod then
                        addStub memdef mAnnot mdef (ItemSet(Hole 0, (getItem()), Hole 1))    
                    else 
                        addMethod mAnnot mdef N.Instance true Undefined
                else
                    if isStub && (meth.IsPropertyGetterMethod || meth.IsPropertySetterMethod) then
                        error "Static property can't have Stub attribute"
                    else 
                        addMethod mAnnot mdef N.Static true Undefined
            | Member.Constructor cdef -> // Todo stub constructors
                addConstructor mAnnot cdef N.Constructor true Undefined
            | _ -> error "Static method can't have Stub attribute"
        else
            match mAnnot.Kind with
            | Some (A.MemberKind.Remote rp) ->
                match CodeReader.getMember meth with
                | Member.Method (isInstance, mdef) ->
                    let remotingKind =
                        match mdef.Value.ReturnType with
                        | VoidType -> RemoteSend
                        | ConcreteType { Entity = e } when e = Definitions.Async -> RemoteAsync
                        | ConcreteType { Entity = e } when e = Definitions.Task || e = Definitions.Task1 -> RemoteTask
                        | _ -> RemoteSync
                    let isCsrfProtected t = true // TODO
                    let rp = rp |> Option.map (fun t -> t, isCsrfProtected t)
                    addMethod mAnnot mdef (N.Remote(remotingKind, comp.GetRemoteHandle(), rp)) true Undefined
                | _ -> error "Only methods can be defined Remote"
            | _ -> ()

    let fsharpSpecific = cls.IsFSharpUnion || cls.IsFSharpRecord || cls.IsFSharpExceptionDeclaration

    let clsTparams =
        cls.GenericParameters |> Seq.map (fun p -> p.Name) |> List.ofSeq    

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
                a |> List.map (fun p -> CodeReader.namedId p.CompiledName),
                t |> Option.map (fun p -> CodeReader.namedId p.CompiledName)
               
            let error m = 
                comp.AddError(Some (CodeReader.getRange meth.DeclarationLocation), SourceError m)

            match mAnnot.Kind with
            | Some A.MemberKind.Stub
            | Some (A.MemberKind.Remote _) -> ()
            | Some kind ->
                let memdef = CodeReader.getMember meth

                if stubs.Contains memdef then () else
    //            let isModuleValue = ref false
                let getBody isInline = 
                    let hasRD =
                        false
//                        meth.Attributes |> Seq.exists (fun a -> a.AttributeType.FullName = "Microsoft.FSharp.Core.ReflectedDefinitionAttribute")
                    if hasRD then
                        let info =
                            match memdef with
                            | Member.Method (_, mdef) 
                            | Member.Override (_, mdef) 
                            | Member.Implementation (_, mdef) ->
                                Reflection.LoadMethod def mdef :> System.Reflection.MethodBase
                            | Member.Constructor cdef ->
                                Reflection.LoadConstructor def cdef :> _
                            | _ -> failwith "static constructor with a reflected definition"
                        match FSharp.Quotations.Expr.TryGetReflectedDefinition(info) with
                        | Some q ->
                            comp.AddWarning(Some (CodeReader.getRange expr.Range), SourceWarning "Compiling from reflected definition")
                            QR.transformExpression (QR.Environment.New(comp)) q
                        | _ ->
                            comp.AddError(Some (CodeReader.getRange expr.Range), SourceError "Failed to get reflected definition")
                            WebSharper.Compiler.Translator.errorPlaceholder
                    else
                    let a, t = getArgsAndThis()
                    let argsAndVars = 
                        [
                            match t with
                            | Some t ->
                                yield t, (CodeReader.namedId t.CompiledName, CodeReader.ThisArg)
                            | _ -> ()
                            for p in a ->    
                                p, 
                                (CodeReader.namedId p.CompiledName, 
                                    if CodeReader.isByRef p.FullType then CodeReader.ByRefArg else CodeReader.LocalVar)
                        ]
                    let tparams = clsTparams @ (meth.GenericParameters |> Seq.map (fun p -> p.Name) |> List.ofSeq)
                    let env = CodeReader.Environment.New (argsAndVars, tparams, comp)  
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
    //                        isModuleValue := true
                            if isInline then
                                b
                            else
                                let scDef, (scContent, scFields) = sc.Value   
                                let name = Resolve.getRenamed meth.CompiledName scFields
                                scContent.Add (ExprStatement (ItemSet(Self, Value (String name), b)))
                                Lambda([], FieldGet(None, NonGeneric scDef, name))
//                                    if meth.IsMutable then
//                                        makeByref (FieldGet(None, concrete(scDef, []), name)) (fun value -> FieldSet(None, concrete(scDef, []), name, value))
//                                    else
//                                        ()
//                                )
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
                                            | _ -> DefaultValueOf (CodeReader.getType env.TParams p.Type)
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
//                                let iv = Option.toList thisVar @ vars |> List.mapi (fun i a -> a, Hole i)
//                                let ib = ReplaceThisWithHole0().TransformExpression(b)
                                let b = 
                                    match thisVar with
                                    | Some t -> ReplaceThisWithVar(t).TransformExpression(b)
                                    | _ -> b
                                makeExprInline (Option.toList thisVar @ vars) b
//                                List.foldBack (fun (v, h) body ->
//                                    Let (v, h, body)    
//                                ) (Option.toList thisVar @ vars |> List.mapi (fun i a -> a, Hole i)) (ReplaceThisWithHole0().TransformExpression(b))
                            else 
                                Function(vars, Return b)
                    res
                match memdef with
                | Member.Method (_, mdef) 
                | Member.Override (_, mdef) 
                | Member.Implementation (_, mdef) ->
                    // remove abstract slot
//                    let abstractOpt = 
//                        clsMembers |> Seq.tryFind (fun m ->
//                            match m with
//                            | NotResolvedMember.Method (d, _) when d = mdef -> true
//                            | _ -> false
//                        )
//                    abstractOpt |> Option.iter (clsMembers.Remove >> ignore)
                    let getKind() =
                        match memdef with
                        | Member.Method (isInstance , _) ->
                            if isInstance then N.Instance else N.Static  
                        | Member.Override (t, _) -> N.Override t 
                        | Member.Implementation (t, _) -> N.Implementation t
                        | _ -> failwith "impossible"
                    
                    let addModuleValueProp kind body =
                        if List.isEmpty args && meth.EnclosingEntity.IsFSharpModule then
                            addMethod { mAnnot with Name = None } (Method { mdef.Value with MethodName = "get_" + mdef.Value.MethodName }) kind false body    
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
                                        let value = CodeReader.namedId "v"                                    
                                        Function ([value], (ExprStatement <| FieldSet(None, NonGeneric scDef, name, Var value)))
                                    | _ -> failwith "unexpected form in module let body"
                                addMethod { mAnnot with Name = None } setm kind false setb    

                    let jsMethod isInline =
                        let kind = if isInline then N.Inline else getKind()
                        let body = getBody isInline
                        addMethod mAnnot mdef kind false body
                        addModuleValueProp kind body
                        // TODO: only one definition added, this is a workaround for C# compatibility
    //                    if !isModuleValue then   
//                        if List.isEmpty args && meth.EnclosingEntity.IsFSharpModule then
//                            addMethod { mAnnot with Name = None } (Method { mdef.Value with MethodName = "get_" + mdef.Value.MethodName }) kind false body    
//                            if meth.IsMutable then 
//                                let setb =
//                                    {}
//
//                                let setm = 
//                                addMethod { mAnnot with Name = None } (Method { mdef.Value with MethodName = "set_" + mdef.Value.MethodName }) kind false body    
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
                        addMethod mAnnot mdef N.NoFallback true Undefined
                    | A.MemberKind.Inline js ->
                        checkNotAbstract() 
                        let vars, thisVar = getVarsAndThis()
                        try 
                            let parsed = WebSharper.Compiler.Recognize.createInline thisVar vars js
                            addMethod mAnnot mdef N.Inline true parsed
                            // TODO: only one definition added, this is a workaround for C# compatibility
                            addModuleValueProp N.Inline parsed
                        with e ->
                            error ("Error parsing inline JavaScript: " + e.Message)
                    | A.MemberKind.Constant c ->
                        checkNotAbstract() 
                        addMethod mAnnot mdef N.Inline true (Value c)                        
                    | A.MemberKind.Direct js ->
                        let vars, thisVar = getVarsAndThis()
                        try
                            let parsed = WebSharper.Compiler.Recognize.parseDirect thisVar vars js
                            addMethod mAnnot mdef (getKind()) true parsed
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
                            addMethod mAnnot mdef N.Inline true i
                        elif meth.IsPropertySetterMethod then  
                            let i = JSRuntime.SetOptional (Hole 0) (Value (String meth.CompiledName.[4..])) (Hole 1)
                            addMethod mAnnot mdef N.Inline true i
                        else error "OptionalField attribute not on property"
                    | A.MemberKind.Generated _ ->
                        addMethod mAnnot mdef (getKind()) false Undefined
                    | A.MemberKind.AttributeConflict m -> error m
                    | A.MemberKind.Remote _ 
                    | A.MemberKind.Stub -> failwith "should be handled previously"
                    if mAnnot.IsEntryPoint then
                        let ep = ExprStatement <| Call(None, NonGeneric def, NonGeneric mdef, [])
                        comp.Graph.AddEdge(EntryPointNode, MethodNode (def, mdef))
                        comp.SetEntryPoint(ep)
                | Member.Constructor cdef ->
                    let jsCtor isInline =   
                        if isInline then 
                            addConstructor mAnnot cdef N.Inline false (getBody true)
                        else
                            addConstructor mAnnot cdef N.Constructor false (getBody false)
                    match kind with
                    | A.MemberKind.NoFallback ->
                        addConstructor mAnnot cdef N.NoFallback true Undefined
                    | A.MemberKind.Inline js ->
                        let vars, thisVar = getVarsAndThis()
                        try
                            let parsed = WebSharper.Compiler.Recognize.createInline thisVar vars js
                            addConstructor mAnnot cdef N.Inline true parsed 
                        with e ->
                            error ("Error parsing inline JavaScript: " + e.Message)
                    | A.MemberKind.Direct js ->
                        let vars, thisVar = getVarsAndThis()
                        try
                            let parsed = WebSharper.Compiler.Recognize.parseDirect thisVar vars js
                            addConstructor mAnnot cdef N.Static true parsed 
                        with e ->
                            error ("Error parsing direct JavaScript: " + e.Message)
                    | A.MemberKind.JavaScript -> jsCtor false
                    | A.MemberKind.InlineJavaScript -> jsCtor true
                    | A.MemberKind.Generated _ ->
                        addConstructor mAnnot cdef N.Static false Undefined
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
            transformClass sc comp annot ent nmembers |> Option.iter comp.AddClass   
        | SourceInterface i ->
            transformInterface annot i |> Option.iter comp.AddInterface

//    let translated = annot.IsJavaScript || clsMembers.Count > 0

    if not annot.IsJavaScript && clsMembers.Count = 0 && annot.Macros.IsEmpty then None else

    if cls.IsFSharpUnion then
        let usesNull =
            cls.UnionCases.Count < 4 // see TaggingThresholdFixedConstant in visualfsharp/src/ilx/EraseUnions.fs
            && cls.Attributes |> CodeReader.hasCompilationRepresentation CompilationRepresentationFlags.UseNullAsTrueValue
            && cls.UnionCases |> Seq.exists (fun c -> c.UnionCaseFields.Count = 0)

        let tparamsMap =
            clsTparams |> Seq.mapi (fun i p -> p, i) |> Map.ofSeq

        let mutable nullCase = usesNull 

        let cases =
            cls.UnionCases
            |> Seq.map (fun case ->
                let cAnnot = CodeReader.attrReader.GetMemberAnnot(annot, case.Attributes)
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
                                    UnionFieldType = CodeReader.getType tparamsMap f.FieldType
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
        let tparamsMap =
            clsTparams |> Seq.mapi (fun i p -> p, i) |> Map.ofSeq
        let cdef =
            Hashed {
                CtorParameters =
                    cls.FSharpFields |> Seq.map (fun f -> CodeReader.getType tparamsMap f.FieldType) |> List.ofSeq
            }
        let body =
            let vars =
                cls.FSharpFields |> Seq.map (fun f -> CodeReader.namedId f.Name) |> List.ofSeq
            let fields =
                cls.FSharpFields |> Seq.map (fun f -> 
                    let fAnnot = CodeReader.attrReader.GetMemberAnnot(annot, Seq.append f.FieldAttributes f.PropertyAttributes)
                    
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

        addConstructor A.MemberAnnotation.BasicJavaScript cdef N.Static false body

        // properties

        for f in cls.FSharpFields do
            let recTyp = Generic def (List.init cls.GenericParameters.Count TypeParameter)
            let fTyp = CodeReader.getType tparamsMap f.FieldType
            
            let getDef =
                Hashed {
                    MethodName = "get_" + f.Name
                    Parameters = []
                    ReturnType = fTyp
                    Generics = 0
                }

            let getBody = FieldGet(Some (Hole 0), recTyp, f.Name)
                
            addMethod A.MemberAnnotation.BasicInlineJavaScript getDef N.Inline false getBody

            if f.IsMutable then
                let setDef =
                    Hashed {
                        MethodName = "set_" + f.Name
                        Parameters = [ fTyp ]
                        ReturnType = VoidType
                        Generics = 0
                    }

                let setBody = FieldSet(Some (Hole 0), recTyp, f.Name, Hole 1)
            
                addMethod A.MemberAnnotation.BasicInlineJavaScript setDef N.Inline false setBody

    if cls.IsFSharpRecord then
        let tparamsMap =
            clsTparams |> Seq.mapi (fun i p -> p, i) |> Map.ofSeq

        let i = 
            cls.FSharpFields |> Seq.map (fun f ->
                let fAnnot = CodeReader.attrReader.GetMemberAnnot(annot, Seq.append f.FieldAttributes f.PropertyAttributes)
                let isOpt = fAnnot.Kind = Some A.MemberKind.OptionalField && CodeReader.isOption f.FieldType
                let fTyp = CodeReader.getType tparamsMap f.FieldType

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
        let fAnnot = CodeReader.attrReader.GetMemberAnnot(annot, Seq.append f.FieldAttributes f.PropertyAttributes)
        let nr =
            {
                StrongName = fAnnot.Name
                IsStatic = f.IsStatic
                IsOptional = fAnnot.Kind = Some A.MemberKind.OptionalField && CodeReader.isOption f.FieldType
            }
        clsMembers.Add (NotResolvedMember.Field (f.Name, nr))    

//    let notTranslated =
//        not annot.IsJavaScript 
////        && not (cls.IsFSharpExceptionDeclaration || cls.IsFSharpRecord || cls.IsFSharpUnion)
//        && clsMembers |> Seq.forall (function NotResolvedMember.Field _ -> true | _ -> false)
//
//    if notTranslated then None else

    let strongName =
        annot.Name |> Option.map (fun n ->
            if n.Contains "." then n else 
                let origName = thisDef.Value.FullName
                origName.[.. origName.LastIndexOf '.'] + n
        )   

    let ckind = 
        if cls.IsFSharpModule then NotResolvedClassKind.Static
        elif isAugmentedFSharpType cls then NotResolvedClassKind.FSharpType
        else NotResolvedClassKind.Class

    Some (
        def,
        {
            StrongName = strongName
            BaseClass = cls.BaseType |> Option.bind (fun t -> t.TypeDefinition |> CodeReader.getTypeDefinition |> ignoreSystemObject)
            Requires = annot.Requires
            Members = List.ofSeq clsMembers
            Kind = ckind
            IsProxy = Option.isSome annot.ProxyOf
            Macros = annot.Macros
        }
    )

let transformAssembly (refMeta : Info) assemblyName (checkResults: FSharpCheckProjectResults) =   
    CodeReader.thisAssemblyName <- assemblyName

    let comp = Compilation(refMeta)

    let asmAnnot = 
        CodeReader.attrReader.GetAssemblyAnnot(checkResults.AssemblySignature.Attributes)

    let rootTypeAnnot = asmAnnot.RootTypeAnnot

    comp.AssemblyName <- assemblyName
    comp.AssemblyRequires <- asmAnnot.Requires
    comp.SiteletDefinition <- asmAnnot.SiteletDefinition

    comp.CustomTypesReflector <- Some A.reflectCustomType
                                             
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
        let rec getTypesWithMembers (parentMembers: ResizeArray<_>) (sc: Lazy<_ * StartupCode>) d =
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
                        b |> List.iter (getTypesWithMembers ms sc)
                    else
                        b |> List.iter (getTypesWithMembers parentMembers sc)
            | FSIFD.MemberOrFunctionOrValue (a, b, c) -> 
                types.[a.EnclosingEntity].Add(SourceMember(a, b, c))
            | FSIFD.InitAction a ->
                ()
                // TODO : warn
//                comp.AddError(Some (ToFSharpAST.getSourcePos a), SourceError "Top level expression")
                // TODO: [<JavaScript>] on whole assembly.

        file.Declarations |> Seq.iter (getTypesWithMembers topLevelTypes sc)

        for t in topLevelTypes do
            match t with
            | SourceMember _ -> failwith "impossible: top level member"
            | SourceEntity (c, m) ->
                transformClass sc comp rootTypeAnnot c m |> Option.iter comp.AddClass
            | SourceInterface i ->
                transformInterface rootTypeAnnot i |> Option.iter comp.AddInterface
            
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
