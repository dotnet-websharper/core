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
open System.Reflection

module M = WebSharper.Core.Metadata
type FST = FSharp.Reflection.FSharpType

type QuotationCompiler (?meta : M.Info) =
    let meta = defaultArg meta M.Info.Empty
    let comp = Compilation(meta)

    member this.Compilation = comp

    member this.CompileReflectedDefinitions(asm: Assembly) =
        for t in asm.GetTypes() do
            let typeAnnot =
                lazy AttributeReader.attrReader.GetTypeAnnot(AttributeReader.TypeAnnotation.Empty, t.CustomAttributes)
            let clsMembers = ResizeArray()
            let readMember m =
                match ReflectedDefinitionReader.readReflected comp m with
                | Some expr ->
                    let mAnnot = AttributeReader.attrReader.GetMemberAnnot(typeAnnot.Value, m.CustomAttributes)   
                    let mem = Reflection.ReadMember m |> Option.get
                    clsMembers.Add(NotResolvedMember.Method(mem, ))
                | None ->
                    ()
            for m in t.GetMethods() do
                readMember m

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
                    if annot.IsStub || (hasStubMember && not hasNonStubMember)
                    then NotResolvedClassKind.Stub
                    elif fsharpModule then NotResolvedClassKind.Static
                    elif (annot.IsJavaScript && (isAbstractClass cls || cls.IsFSharpExceptionDeclaration)) || (annot.Prototype = Some true)
                    then NotResolvedClassKind.WithPrototype
                    else NotResolvedClassKind.Class

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
                        ForceNoPrototype = (annot.Prototype = Some false) || hasConstantCase
                        ForceAddress = hasSingletonCase || def.Value.FullName = "System.Exception" // needed for Error inheritance
                    }
                )
                

    member this.Compile (expr: Expr) = 
        let e = QuotationReader.readExpression comp expr
        Translator.
        ()