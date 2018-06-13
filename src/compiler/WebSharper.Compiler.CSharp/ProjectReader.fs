﻿// $begin{copyright}
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

module WebSharper.Compiler.CSharp.ProjectReader

open Microsoft.CodeAnalysis
open Microsoft.CodeAnalysis.CSharp
open Microsoft.CodeAnalysis.CSharp.Syntax

open WebSharper.Core
open WebSharper.Core.AST
open WebSharper.Core.Metadata

open WebSharper.Compiler
open WebSharper.Compiler.NotResolved
open WebSharper.Compiler.CommandTools

module A = WebSharper.Compiler.AttributeReader
module R = CodeReader

type private N = NotResolvedMemberKind

type TypeWithAnnotation =
    | TypeWithAnnotation of INamedTypeSymbol * A.TypeAnnotation

let annotForTypeOrFile name (annot: A.TypeAnnotation) =
    let mutable annot = annot
    if annot.JavaScriptTypesAndFiles |> List.contains name then
        if not annot.IsForcedNotJavaScript then 
            annot <- { annot with IsJavaScript = true }
    if annot.JavaScriptExportTypesAndFiles |> List.contains name then
        annot <- { annot with IsJavaScriptExport = true }
    annot
    
let rec private getAllTypeMembers (sr: R.SymbolReader) rootAnnot (n: INamespaceSymbol) =
    let rec withNested a (t: INamedTypeSymbol) =
        let annot = sr.AttributeReader.GetTypeAnnot(a, t.GetAttributes())
        seq {
            yield TypeWithAnnotation (t, annot)
            for nt in t.GetTypeMembers() do
                yield! withNested annot nt
        }        
    Seq.append 
        (n.GetTypeMembers() |> Seq.collect (withNested rootAnnot))
        (n.GetNamespaceMembers() |> Seq.collect (getAllTypeMembers sr rootAnnot))

let private fixMemberAnnot (sr: R.SymbolReader) (annot: A.TypeAnnotation) (m: IMethodSymbol) (mAnnot: A.MemberAnnotation) =
    match mAnnot.Name with
    | Some mn as smn -> 
        match m.MethodKind with
        | MethodKind.PropertySet ->
            let p = m.AssociatedSymbol :?> IPropertySymbol
            if isNull p.GetMethod then mAnnot else
            let a = sr.AttributeReader.GetMemberAnnot(annot, p.GetAttributes())
            if a.Kind = Some A.MemberKind.Stub then
                { mAnnot with Kind = a.Kind; Name = a.Name }
            elif a.Name = smn then
                { mAnnot with Name = Some ("set_" + mn) } 
            else mAnnot
        | MethodKind.EventRemove ->
            let e = m.AssociatedSymbol
            let a = sr.AttributeReader.GetMemberAnnot(annot, e.GetAttributes())
            if a.Name = smn then
                { mAnnot with Name = Some ("remove_" + mn) } 
            else mAnnot
        | _ -> mAnnot
    | _ -> mAnnot

let private transformInterface (sr: R.SymbolReader) (annot: A.TypeAnnotation) (intf: INamedTypeSymbol) =
    if intf.TypeKind <> TypeKind.Interface || annot.IsForcedNotJavaScript then None else
    let methodNames = ResizeArray()
    let def =
        match annot.ProxyOf with
        | Some d -> d 
        | _ -> sr.ReadNamedTypeDefinition intf
    for m in intf.GetMembers().OfType<IMethodSymbol>() do
        let mAnnot =
            sr.AttributeReader.GetMemberAnnot(annot, m.GetAttributes())
            |> fixMemberAnnot sr annot m
        let md = 
            match sr.ReadMember m with
            | Member.Method (_, md) -> md
            | _ -> failwith "invalid interface member"
        methodNames.Add(md, mAnnot.Name)
    Some (def, 
        {
            StrongName = annot.Name
            Extends = intf.Interfaces |> Seq.map (fun i -> sr.ReadNamedTypeDefinition i) |> List.ofSeq
            NotResolvedMethods = List.ofSeq methodNames 
        }
    )

let initDef =
    Hashed {
        MethodName = "$init"
        Parameters = []
        ReturnType = VoidType
        Generics = 0
    }

type HasYieldVisitor() =
    inherit StatementVisitor()
    let mutable found = false

    override this.VisitYield _ = found <- true

    member this.Found = found

let hasYield st =
    let visitor = HasYieldVisitor()
    visitor.VisitStatement st
    visitor.Found

let private isResourceType (sr: R.SymbolReader) (c: INamedTypeSymbol) =
    c.AllInterfaces |> Seq.exists (fun i ->
        sr.ReadNamedTypeDefinition i = Definitions.IResource
    )

let delegateTy, delRemove =
    match <@ System.Delegate.Remove(null, null) @> with
    | FSharp.Quotations.Patterns.Call (_, mi, _) ->
        Reflection.ReadTypeDefinition mi.DeclaringType,
        Reflection.ReadMethod mi
    | _ -> failwith "Expecting a Call pattern"

let TextSpans = R.textSpans
let SaveTextSpans() = R.saveTextSpans <- true

let baseCtor thisExpr (t: Concrete<TypeDefinition>) c a =
    if t.Entity = Definitions.Obj then thisExpr
    elif (let fn = t.Entity.Value.FullName in fn = "WebSharper.ExceptionProxy" || fn = "System.Exception") then 
        match a with
        | [] -> Undefined
        | [msg] -> ItemSet(thisExpr, Value (String "message"), msg)
        | [msg; inner] -> 
            Sequential [
                ItemSet(thisExpr, Value (String "message"), msg)
                ItemSet(thisExpr, Value (String "inner"), inner)
            ]
        | _ -> failwith "Too many arguments for Error"
    else
        BaseCtor(thisExpr, t, c, a) 

let private transformClass (rcomp: CSharpCompilation) (sr: R.SymbolReader) (comp: Compilation) (annot: A.TypeAnnotation) (cls: INamedTypeSymbol) =
    let isStruct = cls.TypeKind = TypeKind.Struct
    if cls.TypeKind <> TypeKind.Class && not isStruct then None else

    let thisDef = sr.ReadNamedTypeDefinition cls

    let annot =
        cls.DeclaringSyntaxReferences |> Seq.fold (fun a r -> 
            let pos = CodeReader.getSourcePosOfSyntaxReference r
            let fileName = System.IO.Path.GetFileName pos.FileName 
            a |> annotForTypeOrFile fileName
        ) annot
        |> annotForTypeOrFile thisDef.Value.FullName

    if isResourceType sr cls then
        if comp.HasGraph then
            let thisRes = comp.Graph.AddOrLookupNode(ResourceNode (thisDef, None))
            for req, p in annot.Requires do
                comp.Graph.AddEdge(thisRes, ResourceNode (req, p |> Option.map ParameterObject.OfObj))
        None
    else    

    let inline cs model =
        CodeReader.RoslynTransformer(CodeReader.Environment.New(model, comp, sr))

    let clsMembers = ResizeArray()

    let def =
        match annot.ProxyOf with
        | Some p -> 
            comp.AddProxy(thisDef, p)
            comp.AddWarning(Some (CodeReader.getSourcePosOfSyntaxReference cls.DeclaringSyntaxReferences.[0]), SourceWarning "Proxy type should not be public")
            p
        | _ -> thisDef

    if annot.IsJavaScriptExport then
        comp.AddJavaScriptExport (ExportNode (TypeNode def))

    let members = cls.GetMembers()

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
            FuncArgs = None
            Args = []
            Warn = mAnnot.Warn
        }

    let addMethod (mAnnot: A.MemberAnnotation) mdef kind compiled expr =
        if mAnnot.IsJavaScriptExport then
            comp.AddJavaScriptExport (ExportNode (MethodNode (def, mdef)))
        clsMembers.Add (NotResolvedMember.Method (mdef, (getUnresolved mAnnot kind compiled expr)))
        
    let addConstructor (mAnnot: A.MemberAnnotation) cdef kind compiled expr =
        if mAnnot.IsJavaScriptExport then
            comp.AddJavaScriptExport (ExportNode (ConstructorNode (def, cdef)))
        clsMembers.Add (NotResolvedMember.Constructor (cdef, (getUnresolved mAnnot kind compiled expr)))

    let inits = ResizeArray()                                               
    let staticInits = ResizeArray()                                               
            
    let implicitImplementations = System.Collections.Generic.Dictionary()
    for intf in cls.Interfaces do
        for meth in intf.GetMembers().OfType<IMethodSymbol>() do
            let impl = cls.FindImplementationForInterfaceMember(meth) :?> IMethodSymbol
            if not (isNull impl) && impl.ExplicitInterfaceImplementations.Length = 0 then 
                Dict.addToMulti implicitImplementations impl (intf, meth)

    for mem in members do
        match mem with
        | :? IPropertySymbol as p ->
            if p.IsAbstract || p.IsIndexer then () else
            let pAnnot = sr.AttributeReader.GetMemberAnnot(annot, p.GetMethod.GetAttributes())
            match pAnnot.Kind with
            | Some A.MemberKind.JavaScript ->
                let decls = p.DeclaringSyntaxReferences
                if decls.Length = 0 then () else
                let syntax = decls.[0].GetSyntax()
                let model = rcomp.GetSemanticModel(syntax.SyntaxTree, false)
                let data =
                    syntax :?> PropertyDeclarationSyntax
                    |> RoslynHelpers.PropertyDeclarationData.FromNode
                let cdef = NonGeneric def
                let hasBody (a : RoslynHelpers.AccessorDeclarationData) =
                    a.Body.IsSome || a.ExpressionBody.IsSome
                match data.AccessorList with
                | None -> ()
                | Some acc when hasBody(Seq.head acc.Accessors) -> ()
                | _ ->
                let b = 
                    match data.Initializer with
                    | Some i ->
                        i |> (cs model).TransformEqualsValueClause
                    | None -> 
                        DefaultValueOf (sr.ReadType p.Type)
                match p.SetMethod with
                | null ->
                    if p.IsStatic then
                        staticInits.Add <| ItemSet(Self, Value (String ("$" + p.Name)), b )
                    else
                        inits.Add <| ItemSet(This, Value (String ("$" + p.Name)), b )
                | setMeth ->
                    let setter = sr.ReadMethod setMeth
                    if p.IsStatic then
                        staticInits.Add <| Call(None, cdef, NonGeneric setter, [ b ])
                    else
                        inits.Add <| Call(Some This, cdef, NonGeneric setter, [ b ])
            | _ -> ()
        | :? IFieldSymbol as f -> 
            let fAnnot = sr.AttributeReader.GetMemberAnnot(annot, f.GetAttributes())
            // TODO: check multiple declarations on the same line
            match fAnnot.Kind with
            | Some A.MemberKind.JavaScript ->
                let decls = f.DeclaringSyntaxReferences
                if decls.Length = 0 then () else
                let syntax = decls.[0].GetSyntax()
                let model = rcomp.GetSemanticModel(syntax.SyntaxTree, false)
                match syntax with
                | :? VariableDeclaratorSyntax as v ->
                    let x, e =
                        RoslynHelpers.VariableDeclaratorData.FromNode v
                        |> (cs model).TransformVariableDeclarator
                    
                    if f.IsStatic then 
                        staticInits.Add <| FieldSet(None, NonGeneric def, x.Name.Value, e)
                    else
                        inits.Add <| FieldSet(Some This, NonGeneric def, x.Name.Value, e)
                | _ -> 
//                    let _ = syntax :?> VariableDeclarationSyntax
                    ()
            | _ -> ()
        | _ -> ()

    let hasInit =
        if inits.Count = 0 then false else 
        Function([], ExprStatement (Sequential (inits |> List.ofSeq)))
        |> addMethod A.MemberAnnotation.BasicJavaScript initDef N.Instance false
        true

    let staticInit =
        if staticInits.Count = 0 then None else
        ExprStatement (Sequential (staticInits |> List.ofSeq)) |> Some

    let mutable hasStubMember = false

    for meth in members.OfType<IMethodSymbol>() do
        let meth = 
            match meth.PartialImplementationPart with
            | null -> meth
            | impl -> impl 
        let attrs =
            match meth.MethodKind with
            | MethodKind.PropertyGet
            | MethodKind.PropertySet
            | MethodKind.EventAdd
            | MethodKind.EventRemove ->
                Seq.append (meth.AssociatedSymbol.GetAttributes()) (meth.GetAttributes()) 
            | _ -> meth.GetAttributes() :> _
        let decls = meth.DeclaringSyntaxReferences
        let mAnnot =
            sr.AttributeReader.GetMemberAnnot(annot, attrs)
            |> fixMemberAnnot sr annot meth
        let error m = 
            let sourcePos =
                if decls.Length > 0 then
                    Some (CodeReader.getSourcePosOfSyntaxReference decls.[0])
                else None
            comp.AddError(sourcePos, SourceError m)
        
        match mAnnot.Kind with
        | Some A.MemberKind.Stub ->
            hasStubMember <- true
            match sr.ReadMember meth with
            | Member.Method (isInstance, mdef) ->
                let expr, err = Stubs.GetMethodInline annot mAnnot isInstance def mdef
                err |> Option.iter error
                addMethod mAnnot mdef N.Inline true expr
            | Member.Constructor cdef ->
                let expr = Stubs.GetConstructorInline annot mAnnot def cdef
                addConstructor mAnnot cdef N.Inline true expr
            | Member.Implementation _ -> error "Implementation method can't have Stub attribute"
            | Member.Override _ -> error "Virtual or override method can't have Stub attribute"
            | Member.StaticConstructor -> error "Static constructor can't have Stub attribute"
        | Some (A.MemberKind.Remote rp) -> 
            let memdef = sr.ReadMember meth
            match memdef with
            | Member.Method (isInstance, mdef) ->
                let remotingKind =
                    match mdef.Value.ReturnType with
                    | VoidType -> RemoteSend
                    | ConcreteType { Entity = e } when e = Definitions.Async -> RemoteAsync
                    | ConcreteType { Entity = e } when e = Definitions.Task || e = Definitions.Task1 -> RemoteTask
                    | _ -> RemoteSync // TODO: warning
                let handle = 
                    comp.GetRemoteHandle(
                        def.Value.FullName + "." + mdef.Value.MethodName,
                        mdef.Value.Parameters,
                        mdef.Value.ReturnType
                    )
                addMethod mAnnot mdef (N.Remote(remotingKind, handle, rp)) true Undefined
            | _ -> error "Only methods can be defined Remote"
        | Some kind ->
            let memdef = sr.ReadMember meth
            let getParsed() =                 
                if decls.Length > 0 then
                    try
                        let syntax = decls.[0].GetSyntax()
                        let model = rcomp.GetSemanticModel(syntax.SyntaxTree, false)
                        let fixMethod (m: CodeReader.CSharpMethod) =
                            let b1 = 
                                let defValues = 
                                    m.Parameters |> List.choose (fun p ->
                                        p.DefaultValue |> Option.map (fun v -> p.ParameterId, v)
                                    )
                                if List.isEmpty defValues then m.Body else
                                    CombineStatements [
                                        ExprStatement <| 
                                            Sequential [ for i, v in defValues -> VarSet(i, Conditional (Var i ^== Undefined, v, Var i)) ]
                                        m.Body
                                    ]    
                            let b2 =
                                let isSeq =
                                    match m.ReturnType with
                                    | ConcreteType ct ->
                                        let t = ct.Entity.Value
                                        t.Assembly = "netstandard" && t.FullName.StartsWith "System.Collections.Generic.IEnumerable"
                                    | _ -> false
                                if isSeq && hasYield b1 then
                                    let b = 
                                        b1 |> Continuation.addLastReturnIfNeeded (Value (Bool false))
                                        |> Scoping.fix
                                        |> Continuation.FreeNestedGotos().TransformStatement
                                    let labels = Continuation.CollectLabels.Collect b
                                    Continuation.GeneratorTransformer(labels).TransformMethodBody(b)
                                elif m.IsAsync then
                                    let b = 
                                        b1 |> Continuation.addLastReturnIfNeeded Undefined
                                        |> Continuation.AwaitTransformer().TransformStatement 
                                        |> BreakStatement
                                        |> Scoping.fix
                                        |> Continuation.FreeNestedGotos().TransformStatement
                                    let labels = Continuation.CollectLabels.Collect b
                                    Continuation.AsyncTransformer(labels, sr.ReadAsyncReturnKind meth).TransformMethodBody(b)
                                else b1 |> Scoping.fix |> Continuation.eliminateGotos
                            { m with Body = b2 |> FixThisScope().Fix }
                        match syntax with
                        | :? MethodDeclarationSyntax as syntax ->
                            syntax
                            |> RoslynHelpers.MethodDeclarationData.FromNode 
                            |> (cs model).TransformMethodDeclaration
                            |> fixMethod
                        | :? AccessorDeclarationSyntax as syntax ->
                            syntax
                            |> RoslynHelpers.AccessorDeclarationData.FromNode 
                            |> (cs model).TransformAccessorDeclaration
                            |> fixMethod
                        | :? ArrowExpressionClauseSyntax as syntax ->
                            syntax
                            |> RoslynHelpers.ArrowExpressionClauseData.FromNode 
                            |> (cs model).TransformArrowExpressionClauseAsMethod meth
                            |> fixMethod
                        | :? ConstructorDeclarationSyntax as syntax ->
                            let c =
                                syntax
                                |> RoslynHelpers.ConstructorDeclarationData.FromNode 
                                |> (cs model).TransformConstructorDeclaration
                            let b =
                                match c.Initializer with
                                | Some (CodeReader.BaseInitializer (bTyp, bCtor, args, reorder)) ->
                                    CombineStatements [
                                        ExprStatement (baseCtor This bTyp bCtor args |> reorder)
                                        c.Body
                                    ]
                                | Some (CodeReader.ThisInitializer (bCtor, args, reorder)) ->
                                    CombineStatements [
                                        ExprStatement (baseCtor This (NonGeneric def) bCtor args |> reorder)
                                        c.Body
                                    ]
                                | None -> c.Body
                            let b = 
                                if meth.IsStatic then
                                    match staticInit with
                                    | Some si -> 
                                        CombineStatements [ si; b ]
                                    | _ -> b
                                else
                                    match c.Initializer with
                                    | Some (CodeReader.ThisInitializer _) -> b
                                    | _ ->
                                    if hasInit then 
                                        CombineStatements [ 
                                            ExprStatement <| Call(Some This, NonGeneric def, NonGeneric initDef, [])
                                            b
                                        ]
                                    else b
                            {
                                IsStatic = meth.IsStatic
                                Parameters = c.Parameters
                                Body = b |> FixThisScope().Fix
                                IsAsync = false
                                ReturnType = Unchecked.defaultof<Type>
                            } : CodeReader.CSharpMethod
                        | :? OperatorDeclarationSyntax as syntax -> 
                            syntax
                            |> RoslynHelpers.OperatorDeclarationData.FromNode 
                            |> (cs model).TransformOperatorDeclaration
                            |> fixMethod
                        | :? ConversionOperatorDeclarationSyntax as syntax -> 
                            syntax
                            |> RoslynHelpers.ConversionOperatorDeclarationData.FromNode 
                            |> (cs model).TransformConversionOperatorDeclaration
                            |> fixMethod
                        | _ -> failwithf "Not recognized method syntax kind: %A" (syntax.Kind()) 
                    with e ->
                        comp.AddError(None, SourceError(sprintf "Error reading member '%s': %s" meth.Name e.Message))
                        {
                            IsStatic = meth.IsStatic
                            Parameters = []
                            Body = Empty
                            IsAsync = false
                            ReturnType = Unchecked.defaultof<Type>
                        } : CodeReader.CSharpMethod   
                elif meth.MethodKind = MethodKind.EventAdd then
                    let args = meth.Parameters |> Seq.map sr.ReadParameter |> List.ofSeq
                    let getEv, setEv =
                        let on = if meth.IsStatic then None else Some This
                        FieldGet(on, NonGeneric def, meth.AssociatedSymbol.Name)
                        , fun x -> FieldSet(on, NonGeneric def, meth.AssociatedSymbol.Name, x)
                    let b =
                        JSRuntime.CombineDelegates (NewArray [ getEv; Var args.[0].ParameterId ]) |> setEv    
                    {
                        IsStatic = meth.IsStatic
                        Parameters = args
                        Body = ExprStatement b
                        IsAsync = false
                        ReturnType = sr.ReadType meth.ReturnType
                    } : CodeReader.CSharpMethod   
                elif meth.MethodKind = MethodKind.EventRemove then
                    let args = meth.Parameters |> Seq.map sr.ReadParameter |> List.ofSeq
                    let getEv, setEv =
                        let on = if meth.IsStatic then None else Some This
                        FieldGet(on, NonGeneric def, meth.AssociatedSymbol.Name)
                        , fun x -> FieldSet(on, NonGeneric def, meth.AssociatedSymbol.Name, x)
                    let b =
                        Call (None, NonGeneric delegateTy, NonGeneric delRemove, [getEv; Var args.[0].ParameterId]) |> setEv
                    {
                        IsStatic = meth.IsStatic
                        Parameters = args
                        Body = ExprStatement b
                        IsAsync = false
                        ReturnType = sr.ReadType meth.ReturnType
                    } : CodeReader.CSharpMethod   
                else
                    match meth.MethodKind with
                    | MethodKind.Constructor
                    | MethodKind.StaticConstructor -> ()
                    | k -> failwithf "Unexpected method kind: %s" (System.Enum.GetName(typeof<MethodKind>, k))
                    // implicit constructor
                    let b = 
                        if meth.IsStatic then
                            match staticInit with
                            | Some si -> si
                            | _ -> Empty
                        else
                            let baseCall =
                                let bTyp = cls.BaseType
                                if isNull bTyp then Empty else
                                match sr.ReadNamedType bTyp with
                                | { Entity = td } when td = Definitions.Obj || td = Definitions.ValueType ->
                                    Empty
                                | b ->
                                    ExprStatement (baseCtor This b (ConstructorInfo.Default()) [])
                            let init =
                                if hasInit then 
                                    ExprStatement <| Call(Some This, NonGeneric def, NonGeneric initDef, [])
                                else 
                                    Empty
                            CombineStatements [
                                baseCall
                                init
                            ]
                    {
                        IsStatic = meth.IsStatic
                        Parameters = []
                        Body = b
                        IsAsync = false
                        ReturnType = Unchecked.defaultof<Type>
                    } : CodeReader.CSharpMethod
                    
            let getBody isInline =
                let parsed = getParsed() 
                let args = parsed.Parameters |> List.map (fun p -> p.ParameterId)
                if isInline then
                    let b = Function(args, parsed.Body)
                    let thisVar = if meth.IsStatic then None else Some (Id.New "$this")
                    let b = 
                        match thisVar with
                        | Some t -> ReplaceThisWithVar(t).TransformExpression(b)
                        | _ -> b
                    let allVars = Option.toList thisVar @ args
                    makeExprInline allVars (Application (b, allVars |> List.map Var, NonPure, None))
                else
                    Function(args, parsed.Body)

            let getVars() =
                // TODO: do not parse method body
                let parsed = getParsed()
                parsed.Parameters |> List.map (fun p -> p.ParameterId)

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
                let jsMethod isInline =
                    match memdef with
                    // virtual methods are split to abstract and override
                    | Member.Override (td, _) when not isInline && td = def ->
                        addMethod mAnnot mdef (N.Abstract) true Undefined
                        addMethod { mAnnot with Name = None } mdef (N.Override def) false (getBody isInline)
                    | _ ->
                        addMethod mAnnot mdef (if isInline then N.Inline else getKind()) false (getBody isInline)
                let checkNotAbstract() =
                    if meth.IsAbstract then
                        error "Abstract methods cannot be marked with Inline, Macro or Constant attributes."
                    else
                        match memdef with
                        | Member.Override (bTyp, _) -> 
                            if not (bTyp = Definitions.Obj || bTyp = Definitions.ValueType) then
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
                    try 
                        let parsed = WebSharper.Compiler.Recognize.createInline comp.MutableExternals None (getVars()) mAnnot.Pure js
                        addMethod mAnnot mdef N.Inline true parsed
                    with e ->
                        error ("Error parsing inline JavaScript: " + e.Message)
                | A.MemberKind.Constant c ->
                    checkNotAbstract() 
                    addMethod mAnnot mdef N.Inline true (Value c)                        
                | A.MemberKind.Direct js ->
                    try
                        let parsed = WebSharper.Compiler.Recognize.parseDirect comp.MutableExternals None (getVars()) js
                        addMethod mAnnot mdef (getKind()) true parsed
                    with e ->
                        error ("Error parsing direct JavaScript: " + e.Message)
                | A.MemberKind.JavaScript ->
                    jsMethod false
                | A.MemberKind.InlineJavaScript ->
                    checkNotAbstract()
                    jsMethod true
                // TODO: optional properties
                | A.MemberKind.OptionalField ->
                    let mN = mdef.Value.MethodName
                    if mN.StartsWith "get_" then
                        let i = JSRuntime.GetOptional (ItemGet(Hole 0, Value (String mN.[4..]), Pure))
                        addMethod mAnnot mdef N.Inline true i
                    elif mN.StartsWith "set_" then  
                        let i = JSRuntime.SetOptional (Hole 0) (Value (String mN.[4..])) (Hole 1)
                        addMethod mAnnot mdef N.Inline true i
                    else error "OptionalField attribute not on property"
                | A.MemberKind.Generated _ ->
                    addMethod mAnnot mdef (getKind()) false Undefined
                | A.MemberKind.AttributeConflict m -> error m
                | A.MemberKind.Remote _ 
                | A.MemberKind.Stub -> failwith "should be handled previously"
                if mAnnot.IsEntryPoint then
                    let ep = ExprStatement <| Call(None, NonGeneric def, NonGeneric mdef, [])
                    if comp.HasGraph then
                        comp.Graph.AddEdge(EntryPointNode, MethodNode (def, mdef))
                    comp.SetEntryPoint(ep)
                match implicitImplementations.TryFind meth with
                | Some impls ->
                    for intf, imeth in impls do
                        let idef = sr.ReadNamedTypeDefinition intf
                        let vars = mdef.Value.Parameters |> List.map (fun _ -> Id.New())
                        let imdef = sr.ReadMethod imeth 
                        // TODO : correct generics
                        Lambda(vars, Call(Some This, NonGeneric def, NonGeneric mdef, vars |> List.map Var))
                        |> addMethod A.MemberAnnotation.BasicJavaScript imdef (N.Implementation idef) false
                | _ -> ()
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
                    try
                        let parsed = WebSharper.Compiler.Recognize.createInline comp.MutableExternals None (getVars()) mAnnot.Pure js
                        addConstructor mAnnot cdef N.Inline true parsed 
                    with e ->
                        error ("Error parsing inline JavaScript: " + e.Message)
                | A.MemberKind.Direct js ->
                    try
                        let parsed = WebSharper.Compiler.Recognize.parseDirect comp.MutableExternals None (getVars()) js
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
        | _ -> 
            ()
    
    match staticInit with
    | Some si when not (clsMembers |> Seq.exists (function NotResolvedMember.StaticConstructor _ -> true | _ -> false)) ->
        let b = Function ([], si)
        clsMembers.Add (NotResolvedMember.StaticConstructor b)  
    | _ -> ()
    
    for f in members.OfType<IFieldSymbol>() do
        if isStruct && not f.IsReadOnly then
            comp.AddError(Some (CodeReader.getSourcePosOfSyntaxReference f.DeclaringSyntaxReferences.[0]), SourceError "Mutable structs are not supported for JavaScript translation")
        let backingForProp =
            match f.AssociatedSymbol with
            | :? IPropertySymbol as p -> Some p
            | _ -> None
        let attrs =
            match backingForProp with
            | Some p -> p.GetAttributes() 
            | _ -> f.GetAttributes() 
        let mAnnot = sr.AttributeReader.GetMemberAnnot(annot, attrs)
        let jsName =
            match backingForProp with
            | Some p -> Some ("$" + p.Name)
            | None -> mAnnot.Name                       
        let nr =
            {
                StrongName = jsName
                IsStatic = f.IsStatic
                IsOptional = mAnnot.Kind = Some A.MemberKind.OptionalField 
                IsReadonly = f.IsReadOnly
                FieldType = sr.ReadType f.Type 
            }
        clsMembers.Add (NotResolvedMember.Field (f.Name, nr))    
    
    for f in members.OfType<IEventSymbol>() do
        let mAnnot = sr.AttributeReader.GetMemberAnnot(annot, f.GetAttributes())
        let nr =
            {
                StrongName = mAnnot.Name
                IsStatic = f.IsStatic
                IsOptional = false
                IsReadonly = false
                FieldType = sr.ReadType f.Type 
            }
        clsMembers.Add (NotResolvedMember.Field (f.Name, nr))    

    if not annot.IsJavaScript && clsMembers.Count = 0 && annot.Macros.IsEmpty then None else

    if isStruct then
        comp.AddCustomType(def, StructInfo)

    let strongName =
        annot.Name |> Option.map (fun n ->
            if n.StartsWith "." then n.TrimStart('.') else
            if n.Contains "." then n else 
                let origName = thisDef.Value.FullName
                origName.[.. origName.LastIndexOf '.'] + n
        )   

    let ckind = 
        if annot.IsStub || hasStubMember
        then NotResolvedClassKind.Stub
        elif cls.IsStatic then NotResolvedClassKind.Static
        elif (annot.IsJavaScript && cls.IsAbstract) || (annot.Prototype = Some true)
        then NotResolvedClassKind.WithPrototype
        else NotResolvedClassKind.Class

    let baseCls =
        if cls.IsValueType || cls.IsStatic then
            None
        elif annot.Prototype = Some false then
            cls.BaseType |> sr.ReadNamedTypeDefinition |> ignoreSystemObject
        else
            cls.BaseType |> sr.ReadNamedTypeDefinition |> Some
    
    Some (
        def,
        {
            StrongName = strongName
            BaseClass = baseCls
            Requires = annot.Requires
            Members = List.ofSeq clsMembers
            Kind = ckind
            IsProxy = Option.isSome annot.ProxyOf
            Macros = annot.Macros
            ForceNoPrototype = (annot.Prototype = Some false)
            ForceAddress = false
        }
    )

let transformAssembly (comp : Compilation) (config: WsConfig) (rcomp: CSharpCompilation) =   
    let assembly = rcomp.Assembly

    let sr = CodeReader.SymbolReader(comp)

    let mutable asmAnnot = 
        sr.AttributeReader.GetAssemblyAnnot(assembly.GetAttributes())

    match config.JavaScriptScope with
    | JSDefault -> ()
    | JSAssembly -> asmAnnot <- { asmAnnot with IsJavaScript = true }
    | JSFilesOrTypes a -> asmAnnot <- { asmAnnot with JavaScriptTypesAndFiles = List.ofArray a @ asmAnnot.JavaScriptTypesAndFiles }

    for jsExport in config.JavaScriptExport do
        comp.AddJavaScriptExport jsExport
        match jsExport with
        | ExportCurrentAssembly -> asmAnnot <- { asmAnnot with IsJavaScript = true }
        | ExportByName n -> asmAnnot <- { asmAnnot with JavaScriptTypesAndFiles = n :: asmAnnot.JavaScriptTypesAndFiles }
        | _ -> ()

    let rootTypeAnnot = asmAnnot.RootTypeAnnot

    comp.AssemblyName <- assembly.Name
    comp.AssemblyRequires <- asmAnnot.Requires
    comp.SiteletDefinition <- asmAnnot.SiteletDefinition

    if asmAnnot.IsJavaScriptExport then
        comp.AddJavaScriptExport ExportCurrentAssembly
    for s in asmAnnot.JavaScriptExportTypesFilesAndAssemblies do
        comp.AddJavaScriptExport (ExportByName s)

    comp.CustomTypesReflector <- A.reflectCustomType

    let lookupTypeDefinition (typ: TypeDefinition) =
        rcomp.GetTypeByMetadataName(typ.Value.FullName) |> Option.ofObj

    let readAttribute (a: AttributeData) =
        let fixTypeValue (o: obj) =
            match o with
            | :? ITypeSymbol as t -> box (sr.ReadType t)
            | _ -> o
        sr.ReadNamedTypeDefinition a.AttributeClass,
        a.ConstructorArguments |> Seq.map (CodeReader.getTypedConstantValue >> fixTypeValue >> ParameterObject.OfObj) |> Array.ofSeq

    let lookupTypeAttributes (typ: TypeDefinition) =
        lookupTypeDefinition typ |> Option.map (fun s ->
            s.GetAttributes() |> Seq.map readAttribute |> List.ofSeq
        )

    let lookupFieldAttributes (typ: TypeDefinition) (field: string) =
        lookupTypeDefinition typ |> Option.bind (fun s ->
            match s.GetMembers(field).OfType<IFieldSymbol>() |> List.ofSeq with
            | [ f ] ->
                Some (f.GetAttributes() |> Seq.map readAttribute |> List.ofSeq)
            | _ -> None
        )

    let lookupMethodAttributes (typ: TypeDefinition) (meth: Method) =
        lookupTypeDefinition typ |> Option.bind (fun s -> 
            s.GetMembers(meth.Value.MethodName).OfType<IMethodSymbol>()
            |> Seq.tryFind (fun m ->
                match sr.ReadMember m with
                | Member.Method (_, m)
                | Member.Override (_, m)
                | Member.Implementation (_, m) when m = meth -> true
                | _ -> false
            )
            |> Option.map (fun m ->
                m.GetAttributes() |> Seq.map readAttribute |> List.ofSeq
            ) 
        )

    let lookupConstructorAttributes (typ: TypeDefinition) (ctor: Constructor) =
        lookupTypeDefinition typ |> Option.bind (fun s -> 
            s.GetMembers(".ctor").OfType<IMethodSymbol>()
            |> Seq.tryFind (fun m -> 
                match sr.ReadMember m with
                | Member.Constructor c when c = ctor -> true
                | _ -> false
            ) 
            |> Option.map (fun m ->
                m.GetAttributes() |> Seq.map readAttribute |> List.ofSeq
            ) 
        )

    comp.LookupTypeAttributes <- lookupTypeAttributes
    comp.LookupFieldAttributes <- lookupFieldAttributes 
    comp.LookupMethodAttributes <- lookupMethodAttributes
    comp.LookupConstructorAttributes <- lookupConstructorAttributes

    for TypeWithAnnotation(t, a) in getAllTypeMembers sr rootTypeAnnot assembly.GlobalNamespace do
        match t.TypeKind with
        | TypeKind.Interface ->
            transformInterface sr a t |> Option.iter comp.AddInterface
        | TypeKind.Struct | TypeKind.Class ->
            transformClass rcomp sr comp a t |> Option.iter comp.AddClass
        | _ -> ()
    
    comp.Resolve()

    comp

