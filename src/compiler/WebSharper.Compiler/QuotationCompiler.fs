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

// Reads F# quotations as WebSharper.Core.AST 
namespace WebSharper.Compiler

open FSharp.Quotations

open WebSharper.Core
open WebSharper.Core.AST
open WebSharper.Core.DependencyGraph
open System.Reflection
open WebSharper.Compiler.NotResolved

module M = WebSharper.Core.Metadata
module A = WebSharper.Compiler.AttributeReader
type private FST = FSharp.Reflection.FSharpType
type private N = NotResolvedMemberKind

type QuotationCompiler (meta : M.Info) =
    let comp = Compilation(meta, CustomTypesReflector = A.reflectCustomType)

    let nrInline = N.Inline false

    member this.Compilation = comp

    member this.CompileReflectedDefinitions(asm: Assembly, ?treatReflectedDefinitionAsJavaScript: bool) =
        let treatReflectedDefinitionAsJavaScript = defaultArg treatReflectedDefinitionAsJavaScript true
        comp.AssemblyName <- asm.FullName
        let asmAnnot = A.attrReader.GetAssemblyAnnot(asm.CustomAttributes)
        let rootTypeAnnot = asmAnnot.RootTypeAnnot
        for t in asm.GetTypes() do
            let annot = A.attrReader.GetTypeAnnot(rootTypeAnnot, t.CustomAttributes, t.IsInterface)
            let thisDef = Reflection.ReadTypeDefinition(t) 
            let def =
                match annot.ProxyOf with
                | Some p -> p 
                | _ -> thisDef

            let clsMembers = ResizeArray()

            let getUnresolved (m: MethodBase) (mAnnot: A.MemberAnnotation) kind compiled expr = 
                {
                    Kind = kind
                    StrongName = mAnnot.Name
                    Generics =
                        if m.IsGenericMethod then
                            [] // TODO generic constraints
                        else []
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
                    JavaScriptOptions = mAnnot.JavaScriptOptions
                }

            let mutable hasStubMember = false
            let mutable hasNonStubMember = false

            let readMember (m: MethodBase) =
                let reflected() =
                    try ReflectedDefinitionReader.readReflected comp m
                    with e -> 
                        comp.AddError(None, SourceError(sprintf "Error during reading reflected definition of %s.%s: %s" m.DeclaringType.FullName m.Name e.Message))
                        None
                let mAnnot = A.attrReader.GetMemberAnnot(annot, m.CustomAttributes)   
                
                let memdef = Reflection.ReadMember m |> Option.get
                
                let getVars() =
                    match m.GetParameters() with
                    | [| u |] when u.GetType() = typeof<unit> -> []
                    | a -> a |> Seq.map (fun p -> Id.New(p.Name)) |> List.ofSeq
   
                let getKind() =
                    match memdef with
                    | Member.Method (isInstance , _) ->
                        if isInstance then N.Instance else N.Static  
                    | Member.Override (t, _) -> N.Override t 
                    | Member.Implementation (t, _) -> N.Implementation t
                    | _ -> failwith "impossible"
                
                let getRange _ = None

                let error msg = comp.AddError(getRange m, SourceError msg)
                let warn msg = comp.AddWarning(getRange m, SourceWarning msg)

                let jsMember isInline =
                    match reflected() with
                    | Some expr ->
                        let mem = Reflection.ReadMember m |> Option.get
    
                        let nr k = getUnresolved m mAnnot (if isInline then nrInline else k) false expr

                        match mem with
                        | Member.Constructor ctor ->
                            clsMembers.Add(NotResolvedMember.Constructor(ctor, nr N.Constructor))
                        | Member.Method (inst, meth) ->
                            clsMembers.Add(NotResolvedMember.Method(meth, nr (if inst then N.Instance else N.Static)))
                        | Member.Implementation (td, impl) ->
                            clsMembers.Add(NotResolvedMember.Method(impl, nr (N.Implementation(td)))) 
                        | Member.Override (td, impl) ->
                            clsMembers.Add(NotResolvedMember.Method(impl, nr (N.Override td)))
                        | Member.StaticConstructor ->
                            clsMembers.Add(NotResolvedMember.StaticConstructor(ExprStatement expr))
                    | None -> 
                        ()

                let checkNotAbstract() =
                    if m.IsAbstract then
                        error "Abstract methods cannot be marked with Inline, Macro or Constant attributes."
                    else
                        match memdef with
                        | Member.Override (bTyp, _) -> 
                            if not (bTyp = Definitions.Obj || bTyp = Definitions.ValueType) then
                                error "Override methods cannot be marked with Inline, Macro or Constant attributes."
                        | Member.Implementation _ ->
                            error "Interface implementation methods cannot be marked with Inline, Macro or Constant attributes."
                        | _ -> ()
                
                match memdef with
                | Member.Method (_, mdef) 
                | Member.Override (_, mdef) 
                | Member.Implementation (_, mdef) ->
                    
                    let addMethod kind compiled expr =
                        clsMembers.Add (NotResolvedMember.Method (mdef, (getUnresolved m mAnnot kind compiled expr)))


                    match mAnnot.Kind with
                    | None ->
                        if treatReflectedDefinitionAsJavaScript then
                            jsMember false
                    | Some kind ->
                    match kind with
                    | A.MemberKind.Stub ->
                        match memdef with
                        | Member.Method (isInstance, mdef) ->
                            hasStubMember <- true
                            let expr, err = Stubs.GetMethodInline comp.AssemblyName annot mAnnot false isInstance def mdef
                            err |> Option.iter error
                            addMethod nrInline true expr
                        | _ -> failwith "Member kind not expected for astract method"
                    | A.MemberKind.JavaScript when m.IsAbstract -> 
                        match memdef with
                        | Member.Method (isInstance, mdef) ->
                            if not isInstance then failwith "Abstract method should not be static" 
                            addMethod N.Abstract true Undefined
                        | _ -> failwith "Member kind not expected for astract method"
                    | A.MemberKind.Remote (rp, p) ->
                        let remotingKind =
                            match mdef.Value.ReturnType with
                            | VoidType -> M.RemoteSend
                            | ConcreteType { Entity = e } when e = Definitions.Async -> M.RemoteAsync
                            | ConcreteType { Entity = e } when e = Definitions.Task || e = Definitions.Task1 -> M.RemoteTask
                            | _ -> M.RemoteSync
                        let path = comp.GetRemotePath(p, thisDef, mdef.Value.MethodName)
                        let vars = getVars()
                        let nr =
                            {
                                Kind = N.Remote (remotingKind, path, vars, rp, Some mdef.Value.ReturnType, None)
                                StrongName = mAnnot.Name
                                Generics = []
                                Macros = mAnnot.Macros
                                Generator = None
                                Compiled = true 
                                Pure = mAnnot.Pure
                                Body = Undefined
                                Requires = mAnnot.Requires
                                FuncArgs = None
                                Args = []
                                Warn = mAnnot.Warn
                                JavaScriptOptions = mAnnot.JavaScriptOptions
                            }
                        clsMembers.Add(NotResolvedMember.Method(mdef, nr))
                    | A.MemberKind.NoFallback ->
                        checkNotAbstract()
                        addMethod N.NoFallback true Undefined
                    | A.MemberKind.Inline (js, ta, dollarVars) ->
                        checkNotAbstract() 
                        let vars = getVars()
                        try 
                            let parsed = WebSharper.Compiler.Recognize.createInline comp.MutableExternals None vars mAnnot.Pure None dollarVars comp.AssemblyName js
                            List.iter warn parsed.Warnings
                            addMethod (N.Inline ta) true parsed.Expr
                        with e ->
                            error ("Error parsing inline JavaScript: " + e.Message)
                    | A.MemberKind.Constant c ->
                        checkNotAbstract() 
                        addMethod nrInline true (Value c)                        
                    | A.MemberKind.Direct (js, dollarVars) ->
                        let vars = getVars()
                        try
                            let parsed = WebSharper.Compiler.Recognize.parseDirect comp.MutableExternals None vars dollarVars comp.AssemblyName js
                            List.iter warn parsed.Warnings
                            addMethod (getKind()) true parsed.Expr
                        with e ->
                            error ("Error parsing direct JavaScript: " + e.Message)
                    | A.MemberKind.JavaScript ->
                        jsMember false
                    | A.MemberKind.InlineJavaScript ->
                        checkNotAbstract()
                        jsMember true
                    | A.MemberKind.OptionalField ->
                        if m.Name.StartsWith("get_") then
                            let i = JSRuntime.GetOptional (ItemGet(Hole 0, Value (String m.Name.[4..]), Pure))
                            addMethod nrInline true i
                        elif m.Name.StartsWith("set_") then  
                            let i = JSRuntime.SetOptional (Hole 0) (Value (String m.Name.[4..])) (Hole 1)
                            addMethod nrInline true i
                        else error "OptionalField attribute not on property"
                    | A.MemberKind.Generated _ ->
                        addMethod (getKind()) false Undefined
                    | A.MemberKind.AttributeConflict m -> error m
                | Member.Constructor cdef ->

                    let addConstructor kind compiled expr =
                        clsMembers.Add (NotResolvedMember.Constructor (cdef, (getUnresolved m mAnnot kind compiled expr)))

                    match mAnnot.Kind with
                    | None ->
                        if treatReflectedDefinitionAsJavaScript then
                            jsMember false
                    | Some kind ->
                    match kind with
                    | A.MemberKind.Stub ->
                        let expr = Stubs.GetConstructorInline comp.AssemblyName annot mAnnot def cdef
                        addConstructor nrInline true expr
                    | A.MemberKind.NoFallback ->
                        addConstructor N.NoFallback true Undefined
                    | A.MemberKind.Inline (js, ta, dollarVars) ->
                        let vars = getVars()
                        try
                            let parsed = WebSharper.Compiler.Recognize.createInline comp.MutableExternals None vars mAnnot.Pure None dollarVars comp.AssemblyName js
                            List.iter warn parsed.Warnings
                            addConstructor (N.Inline ta) true parsed.Expr
                        with e ->
                            error ("Error parsing inline JavaScript: " + e.Message)
                    | A.MemberKind.Direct (js, dollarVars) ->
                        let vars = getVars()
                        try
                            let parsed = WebSharper.Compiler.Recognize.parseDirect comp.MutableExternals None vars dollarVars comp.AssemblyName js
                            List.iter warn parsed.Warnings
                            addConstructor N.Static true parsed.Expr
                        with e ->
                            error ("Error parsing direct JavaScript: " + e.Message)
                    | A.MemberKind.JavaScript -> jsMember false
                    | A.MemberKind.InlineJavaScript -> jsMember true
                    | A.MemberKind.Generated _ ->
                        addConstructor N.Static false Undefined
                    | A.MemberKind.AttributeConflict m -> error m
                    | A.MemberKind.Remote _
                    | A.MemberKind.Stub -> failwith "should be handled previously"
                    | A.MemberKind.OptionalField
                    | A.MemberKind.Constant _ -> failwith "attribute not allowed on constructors"
                | Member.StaticConstructor ->
                    if treatReflectedDefinitionAsJavaScript then
                        jsMember false

            for m in t.GetMethods(Reflection.AllMethodsFlags) do
                readMember m

            for c in t.GetConstructors(Reflection.AllMethodsFlags) do
                readMember c

            if clsMembers.Count > 0 then
                
                let fsharpSpecificNonException =
                    FST.IsUnion(t) || FST.IsRecord(t) || t.IsValueType

                let fsharpSpecific = 
                    fsharpSpecificNonException || FST.IsExceptionRepresentation(t)

                let fsharpModule = FST.IsModule(t)

                let baseCls =
                    if fsharpSpecificNonException || fsharpModule || t.IsValueType || annot.IsStub || def.Value.FullName = "System.Object" then
                        None
                    elif annot.Prototype = Some false then
                        t.BaseType |> Option.ofObj |> Option.bind (fun bt -> Reflection.ReadType(bt) |> getConcreteType |> ignoreSystemObject)
                    else 
                        t.BaseType|> Option.ofObj |> Option.map (fun bt -> Reflection.ReadType(bt) |> getConcreteType)

                let strongName =
                    annot.Name |> Option.map (fun n ->
                        if n.StartsWith "." then n.TrimStart('.') else
                        if n.Contains "." then n else 
                            let origName = thisDef.Value.FullName
                            origName.[.. origName.LastIndexOf '.'] + n
                    )   

                let ckind = 
                    if annot.IsStub || (hasStubMember && not hasNonStubMember)
                    then NotResolvedClassKind.Stub
                    elif fsharpModule then NotResolvedClassKind.Static
                    elif (annot.IsJavaScript && (t.IsAbstract || FST.IsExceptionRepresentation(t))) || (annot.Prototype = Some true)
                    then NotResolvedClassKind.WithPrototype
                    else NotResolvedClassKind.Class               

                for i, f in t.GetFields(Reflection.AllMethodsFlags) |> Seq.indexed do
                    let fAnnot = A.attrReader.GetMemberAnnot(annot, f.CustomAttributes) 
                    let nr =
                        {
                            StrongName = fAnnot.Name
                            IsStatic = f.IsStatic
                            IsOptional = fAnnot.Kind = Some A.MemberKind.OptionalField 
                            IsReadonly = f.IsInitOnly
                            FieldType = Reflection.ReadType f.FieldType
                            Order = i 
                        }
                    clsMembers.Add(NotResolvedMember.Field(f.Name, nr))    

                comp.AddClass(
                    def,
                    {
                        StrongName = strongName
                        BaseClass = baseCls
                        Implements = []
                        Generics = []
                        Requires = annot.Requires
                        Members = List.ofSeq clsMembers
                        Kind = ckind
                        IsProxy = Option.isSome annot.ProxyOf
                        Macros = annot.Macros
                        ForceNoPrototype = (annot.Prototype = Some false) // || hasConstantCase
                        ForceAddress = false // hasSingletonCase 
                        Type = None
                        SourcePos = { FileName = ""; Start = (0,0); End = (0,0) }
                    }
                )
        
        comp.Resolve()

        Translator.DotNetToJavaScript.CompileFull comp

    member this.CompileExpression (expr: Expr, ?node) =
        let e = QuotationReader.readExpression comp expr
        Translator.DotNetToJavaScript.CompileExpression(comp, e, ?node = node)
