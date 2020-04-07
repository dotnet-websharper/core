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

    member this.Compilation = comp

    member this.CompileReflectedDefinitions(asm: Assembly) =
        for t in asm.GetTypes() do
            let typeAnnot =
                lazy A.attrReader.GetTypeAnnot(A.TypeAnnotation.Empty, t.CustomAttributes)
            let clsMembers = ResizeArray()
            // TODO JS inlines, macros, generators, stubs
            let readMember m =
                let reflected =
                    try ReflectedDefinitionReader.readReflected comp m
                    with e -> 
                        comp.AddError(None, SourceError(sprintf "Error during reading reflected definition of %s.%s: %s" m.DeclaringType.FullName m.Name e.Message))
                        None
                match reflected with
                | Some expr ->
                    let mAnnot = A.attrReader.GetMemberAnnot(typeAnnot.Value, m.CustomAttributes)   
                    let mem = Reflection.ReadMember m |> Option.get
                    
                    let nr k =
                        let kind =
                            match mAnnot.Kind with
                            | Some A.MemberKind.InlineJavaScript -> N.Inline
                            | _ -> k
                        {
                            Kind = kind
                            StrongName = mAnnot.Name
                            Macros = mAnnot.Macros
                            Generator = None
                            Compiled = false 
                            Pure = mAnnot.Pure
                            Body = expr
                            Requires = mAnnot.Requires
                            FuncArgs = None
                            Args = []
                            Warn = mAnnot.Warn
                        }

                    // TODO add Abstract methods
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
                        clsMembers.Add(NotResolvedMember.StaticConstructor(Lambda([], expr)))

                | None ->
                    ()

            for m in t.GetMethods(Reflection.AllMethodsFlags) do
                readMember m

            for c in t.GetConstructors(Reflection.AllMethodsFlags) do
                readMember c

            if clsMembers.Count > 0 then

                let annot = typeAnnot.Value

                let thisDef = Reflection.ReadTypeDefinition(t) 
                
                let def =
                    match annot.ProxyOf with
                    | Some p -> p
                    | _ -> thisDef

                let fsharpSpecificNonException =
                    FST.IsUnion(t) || FST.IsRecord(t) || t.IsValueType

                let fsharpSpecific = 
                    fsharpSpecificNonException || FST.IsExceptionRepresentation(t)

                let fsharpModule = FST.IsModule(t)

                let baseCls =
                    if fsharpSpecificNonException || fsharpModule || t.IsValueType || annot.IsStub || def.Value.FullName = "System.Object" then
                        None
                    elif annot.Prototype = Some false then
                        t.BaseType |> Option.ofObj |> Option.bind (fun bt -> Reflection.ReadTypeDefinition(bt) |> ignoreSystemObject)
                    else 
                        t.BaseType|> Option.ofObj |> Option.map (fun bt -> Reflection.ReadTypeDefinition(bt))

                let strongName =
                    annot.Name |> Option.map (fun n ->
                        if n.StartsWith "." then n.TrimStart('.') else
                        if n.Contains "." then n else 
                            let origName = thisDef.Value.FullName
                            origName.[.. origName.LastIndexOf '.'] + n
                    )   

                let ckind = 
                    if annot.IsStub //|| (hasStubMember && not hasNonStubMember)
                    then NotResolvedClassKind.Stub
                    elif fsharpModule then NotResolvedClassKind.Static
                    elif (annot.IsJavaScript && (t.IsAbstract || FST.IsExceptionRepresentation(t))) || (annot.Prototype = Some true)
                    then NotResolvedClassKind.WithPrototype
                    else NotResolvedClassKind.Class               

                for f in t.GetFields(Reflection.AllMethodsFlags) do
                    let fAnnot = A.attrReader.GetMemberAnnot(annot, f.CustomAttributes) 
                    let nr =
                        {
                            StrongName = fAnnot.Name
                            IsStatic = f.IsStatic
                            IsOptional = fAnnot.Kind = Some A.MemberKind.OptionalField 
                            IsReadonly = f.IsInitOnly
                            FieldType = Reflection.ReadType f.FieldType
                        }
                    clsMembers.Add(NotResolvedMember.Field(f.Name, nr))    

                comp.AddClass(
                    def,
                    {
                        StrongName = strongName
                        BaseClass = baseCls
                        Requires = annot.Requires
                        Members = List.ofSeq clsMembers
                        Kind = ckind
                        IsProxy = Option.isSome annot.ProxyOf
                        Macros = annot.Macros
                        ForceNoPrototype = (annot.Prototype = Some false) // || hasConstantCase
                        ForceAddress = false // hasSingletonCase 
                    }
                )
        
        comp.Resolve()

        Translator.DotNetToJavaScript.CompileFull comp

    member this.CompileExpression (expr: Expr, ?node) =
        let e = QuotationReader.readExpression comp expr
        Translator.DotNetToJavaScript.CompileExpression(comp, e, ?node = node)
