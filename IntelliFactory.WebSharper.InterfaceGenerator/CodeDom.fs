// $begin{copyright}
// 
// This file is part of WebSharper
// 
// Copyright (c) 2008-2011 IntelliFactory
// 
// GNU Affero General Public License Usage
// WebSharper is free software: you can redistribute it and/or modify it under
// the terms of the GNU Affero General Public License, version 3, as published
// by the Free Software Foundation.
//
// WebSharper is distributed in the hope that it will be useful, but WITHOUT
// ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
// FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License
// for more details at <http://www.gnu.org/licenses/>.
//
// If you are unsure which license is appropriate for your use, please contact
// IntelliFactory at http://intellifactory.com/contact.
//
// $end{copyright}

namespace IntelliFactory.WebSharper.InterfaceGenerator

/// Defines `System.CodeDom` helpers.
module internal CodeDom =
    open System.CodeDom
    open System.CodeDom.Compiler
    open System.IO
    open System.Text.RegularExpressions

    /// Looks up a namespace.
    let TryFindNamespace (p: CodeCompileUnit)
                         (name: string) : option<CodeNamespace> =
        let ns = Seq.cast p.Namespaces : seq<CodeNamespace>
        ns |> Seq.tryFind (fun n -> n.Name = name)

    /// Gets or creates a `CodeNamespace`.
    let GetNamespace (p: CodeCompileUnit) (name: string) : CodeNamespace =
        match TryFindNamespace p name with
        | None ->
            let ns = new CodeNamespace(name)
            p.Namespaces.Add ns |> ignore
            ns
        | Some x ->
            x

    /// Looks up a `CodeTypeDeclaration`.
    let TryFindType (p: CodeCompileUnit) (ns: string)
                    (name: string) : option<CodeTypeDeclaration> =
        match TryFindNamespace p ns with
        | None ->

            None
        | Some ns ->
            let ts = Seq.cast ns.Types : seq<CodeTypeDeclaration>
            ts |> Seq.tryFind (fun t -> t.Name = name)

    /// Gets or creates a `CodeTypeDeclaration`.
    let GetType (p: CodeCompileUnit) 
                (ns: string)
                (name: string) : CodeTypeDeclaration =
        match TryFindType p ns name with
        | None ->
            let ns = GetNamespace p ns
            let t = new CodeTypeDeclaration(name)
            ns.Types.Add t |> ignore
            t
        | Some x ->
            x

    /// Generates code from a `CodeAttributeDeclaration`.
    let GenerateCodeFromAttributeDeclaration 
            (provider: CodeDomProvider)
            (attr: CodeAttributeDeclaration) =
        let t = new CodeMemberMethod()
        t.CustomAttributes.Add attr |> ignore
        use buffer = new StringWriter()
        provider.GenerateCodeFromMember(t, buffer, CodeGeneratorOptions ())
        use reader = new StringReader(buffer.ToString().Trim())
        reader.ReadLine()

    /// Adds attribute annotations to property getters and setters.
    let AnnotateProperty (provider: CodeDomProvider)
                         (indentation: int)
                         (getter: list<CodeAttributeDeclaration>)
                         (setter: list<CodeAttributeDeclaration>) 
                         (prop: CodeMemberProperty) =
        use buffer = new StringWriter()
        use writer = new IndentedTextWriter(buffer, Indent = indentation)
        let opts = CodeGeneratorOptions ()
        provider.GenerateCodeFromMember(prop, writer, opts)
        let generate etter =
            use buffer = new StringWriter()
            use writer = new IndentedTextWriter(buffer, Indent = indentation)
            for x in etter do
                GenerateCodeFromAttributeDeclaration provider x
                |> writer.WriteLine
            buffer.ToString().Trim()
        let code =
            Regex.Replace(buffer.ToString(), @"(\s+)(get|set)",
                new MatchEvaluator(fun m ->
                    let x =
                        if m.Groups.[2].Value = "get" then getter else setter
                        |> generate
                    m.Groups.[1].Value + x + m.Groups.[0].Value
                ))
        new CodeSnippetTypeMember(code)
