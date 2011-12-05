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

namespace System

namespace IntelliFactory.WebSharper.InterfaceGenerator

/// Compiles `Library` values to `System.CodeDom` values.
module Compiler =
    open System.CodeDom
    open System.Collections.Generic
    open System.IO
    open System.Text.RegularExpressions
    open Microsoft.CSharp

    type private T = Type.Type

    module Code = CodeModel

    type internal InlineMode =
        | Getter = 0uy
        | Setter = 1uy
        | Method = 2uy

    let private GetMethodBaseInline (td: Code.TypeDeclaration) 
                                    (t: T)
                                    (m: Code.MethodBase) =
        if m.Inline.IsSome then m.Inline.Value else
            let name =
                match m.Name with
                | ""                   -> "new " + td.Name
                | name when m.IsStatic -> td.Name + "." + name
                | name                 -> name
            match t with
            | Type.FunctionType f ->
                let args =
                    seq {
                        for i in 0 .. f.Parameters.Length - 1 ->
                            "$" + string ((if m.IsStatic then 0 else 1) + i)
                    }
                    |> String.concat ","
                let arity = f.Parameters.Length
                match f.ParamArray with
                | Some v ->
                    if m.IsStatic then
                        let tpl = "{0}.apply(undefined,[{1}].concat(${2}))"
                        System.String.Format(tpl, name, args, arity)
                    else
                        let tpl = "$this.{0}.apply($this,[{1}].concat(${2}))"
                        System.String.Format(tpl, name, args, arity + 1)
                | None ->
                    let tpl =
                        if m.IsStatic
                        then "{0}({1})"
                        else "$this.{0}({1})"
                    System.String.Format(tpl, name, args)
            | _ ->
                if m.IsStatic then m.Name + "()" else "$this." + m.Name + "()"

    let private GetPropertyGetterInline (td: Code.TypeDeclaration) 
                                        (p: Code.Property) =
        if p.GetterInline.IsSome then p.GetterInline.Value else
            let pfx = if p.IsStatic then td.Name else "$this"
            System.String.Format("{0}.{1}", pfx, p.Name)

    let private GetPropertySetterInline (td: Code.TypeDeclaration) 
                                        (p: Code.Property) =
        if p.SetterInline.IsSome then p.SetterInline.Value else
            let pfx = if p.IsStatic then td.Name else "$this"
            System.String.Format("void ({0}.{1} = $value)", pfx, p.Name)

    let private GetSourceName (entity: Code.Entity) =
        if entity.SourceName.IsSome then entity.SourceName.Value else
            let name = entity.Name
            let name =
                match name.LastIndexOf '.' with
                | -1 -> name
                | n -> name.Substring(n + 1)
            let name =
                if name.StartsWith "new " then
                    name.Substring 4
                else
                    name
            name.Substring(0, 1).ToUpper() + name.Substring 1

    type private Context =
        {
            CodeDomProvider      : Compiler.CodeDomProvider
            CodeGeneratorOptions : Compiler.CodeGeneratorOptions
            Resolve              : Type.Id -> option<string>
            Indentation          : int
        }

        member this.Indent =
            { this with Indentation = this.Indentation + 1 }

    /// Interprets a canonical type as a `CodeTypeReference`.
    let rec private TranslateType (ctx: Context)
                                  (t: T) : CodeTypeReference =
        let flat (n: string) =
            new CodeTypeReference(n)
        let from t = TranslateType ctx t
        let gen (n: string) ts =
            let r = flat n
            r.TypeArguments.AddRange (Seq.toArray ts)
            r
        let tuple = gen "System.Tuple"
        let func dom ran = gen "Microsoft.FSharp.Core.FSharpFunc" [dom; ran]
        let arr rank (x: CodeTypeReference) =
            new CodeTypeReference(ArrayRank = rank, ArrayElementType = x)
        match t with
        | Type.ArrayType (rank, t) ->
            arr rank (from t)
        | Type.DeclaredType id ->
            match ctx.Resolve id with
            | Some name -> flat name
            | _         -> flat typeof<obj>.FullName
        | Type.FunctionType f->
            match f.ParamArray, f.This with
            | None, None ->
                let domain =
                    match f.Parameters with
                    | []       -> flat typeof<unit>.FullName
                    | [(_, t)] -> from t
                    | ts       -> tuple [for (_, t) in ts -> from t]
                func domain (from f.ReturnType)
            | None, Some this when f.Parameters.IsEmpty ->
                match f.ReturnType with
                | Type.Unit ->
                    gen "System.Action" [from this]
                | Type.NonUnit ->
                    gen "System.Converter" [from this; from f.ReturnType]
            | None, Some this ->
                match f.ReturnType with
                | Type.Unit ->
                    this :: List.map snd f.Parameters @ [Type.Unit]
                | Type.NonUnit ->
                    this :: List.map snd f.Parameters @ [f.ReturnType]
                |> List.map from
                |> gen "IntelliFactory.WebSharper.Pervasives+Func"
            | _ ->
                // ParamArray not supported yet:
                CodeTypeReference typeof<obj>
        | Type.GenericType id ->
            CodeTypeReference (CodeTypeParameter id)
        | Type.SpecializedType (x, xs) ->
            let a = from x
            for y in xs do
                a.TypeArguments.Add (from y) |> ignore
            a
        | Type.SystemType t ->
            flat t.FullName
        | Type.TupleType xs ->
            tuple (List.map from (List.rev xs))
        | Type.UnionType _ ->
            CodeTypeReference (typeof<obj>)

    let private NotImplemented : CodeStatement =
        let exn = typeof<System.NotImplementedException>
        new CodeThrowExceptionStatement(
            new CodeObjectCreateExpression(
                new CodeTypeReference(exn), [||]
            )
        ) :> _

    let private MakeInlineAttribute (code: string) =
        CodeAttributeDeclaration(
            typeof<IntelliFactory.WebSharper.Core.Attributes.InlineAttribute>.FullName,
            CodeAttributeArgument (CodePrimitiveExpression code))

    let private CompileParameters
            (ctx: Context)
            (f: Type.Function)
            (out: CodeParameterDeclarationExpressionCollection) =
        for (n, t) in f.Parameters do
            let t = TranslateType ctx t
            new CodeParameterDeclarationExpression(t, n)
            |> out.Add
            |> ignore
        if f.ParamArray.IsSome then
            let t = TranslateType ctx (Type.ArrayOf f.ParamArray.Value)
            let code =
                new CodeParameterDeclarationExpression(t, "parameters")
            new CodeAttributeDeclaration(
                new CodeTypeReference(
                    typeof<System.ParamArrayAttribute>))
            |> code.CustomAttributes.Add
            |> ignore
            out.Add code |> ignore

    let private MakeAttributes (x: Code.Entity) =
        seq {
            match x with
            | :? Code.Member as x ->
                if x.IsStatic then
                    yield MemberAttributes.Static
            | _ ->
                ()
            match x.AccessModifier with
            | Code.AccessModifier.Private ->
                yield MemberAttributes.Private
            | Code.AccessModifier.Protected ->
                yield MemberAttributes.Family
            | Code.AccessModifier.Internal ->
                yield MemberAttributes.Assembly
            | Code.AccessModifier.Public ->
                yield MemberAttributes.Public
            | _ ->
                ()
        }
        |> Seq.reduce ( ||| )

    let private CompileMethod (ctx: Context)
                              (ctd: CodeTypeDeclaration)
                              (td:  Code.TypeDeclaration)
                              (x:   Code.Method) =
        let overloads =
            x.Type
            |> Type.Normalize
            |> Type.DistinctOverloads
        for t in overloads do
            match t with
            | Type.FunctionType f ->
                let cmm = 
                    CodeMemberMethod (
                        Name = GetSourceName x,
                        Attributes = MakeAttributes x
                    )
                for g in x.Generics do
                    CodeTypeParameter g
                    |> cmm.TypeParameters.Add
                    |> ignore
                ctd.Members.Add cmm |> ignore
                CompileParameters ctx f cmm.Parameters
                match f.ReturnType with
                | Type.Unit -> 
                    ()
                | Type.NonUnit -> 
                    cmm.ReturnType <- TranslateType ctx f.ReturnType
                if not ctd.IsInterface then
                    cmm.Statements.Add NotImplemented |> ignore
                    let i = GetMethodBaseInline td t x
                    MakeInlineAttribute i
                    |> cmm.CustomAttributes.Add
                    |> ignore
            | _ ->
                ()

    let private CompileConstructor (ctx: Context)
                                   (ctd: CodeTypeDeclaration)
                                   (td:  Code.TypeDeclaration)
                                   (x:   Code.Constructor) =
        let overloads =
            x.Type
            |> Type.Normalize
            |> Type.DistinctOverloads
        for t in overloads do
            match t with
            | Type.FunctionType f ->
                let cc = CodeConstructor (Attributes = MakeAttributes x)
                let i = GetMethodBaseInline td t x
                MakeInlineAttribute i
                |> cc.CustomAttributes.Add
                |> ignore
                CompileParameters ctx f cc.Parameters
                ctd.Members.Add cc |> ignore
            | _ ->
                ()

    let private CompileProperty (ctx: Context)
                                (ctd: CodeTypeDeclaration)
                                (td:  Code.TypeDeclaration)
                                (p:   Code.Property) =
        let ty =
            match Type.Normalize p.Type with
            | [t] -> t
            | _   -> T<obj>
            |> TranslateType ctx
        if ctd.IsInterface then
            let cmp =
                CodeMemberProperty (
                    Name = GetSourceName p,
                    Type = ty
                )
            if p.HasGetter then
                cmp.HasGet <- true
            if p.HasSetter then
                cmp.HasSet <- true
            ctd.Members.Add cmp
            |> ignore
        else
            let cmp =
                CodeMemberProperty (
                    Name = GetSourceName p,
                    Attributes = MakeAttributes p,
                    Type = ty
                )
            if p.HasGetter then
                cmp.HasGet <- true
                cmp.GetStatements.Add NotImplemented |> ignore
            if p.HasSetter then
                cmp.HasSet <- true
                cmp.SetStatements.Add NotImplemented |> ignore
            cmp
            |> CodeDom.AnnotateProperty
                ctx.CodeDomProvider
                ctx.Indentation
                [ if p.HasGetter then
                    yield GetPropertyGetterInline td p
                          |> MakeInlineAttribute
                ]
                [ if p.HasSetter then
                    yield GetPropertySetterInline td p
                          |> MakeInlineAttribute
                ]
            |> ctd.Members.Add
            |> ignore

    let private TranslateTypeDeclaration (inter: bool) (ctx: Context) (d: Code.TypeDeclaration) =
        let ctx = ctx.Indent
        let ctd = 
            CodeTypeDeclaration(GetSourceName d,
                IsInterface = inter,
                Attributes  = MakeAttributes d)
        for g in d.Generics do
            CodeTypeParameter g
            |> ctd.TypeParameters.Add
            |> ignore
        for x in d.Methods do
            CompileMethod ctx ctd d x
        for x in d.Properties do
            CompileProperty ctx ctd d x
        (ctx, ctd)
        
    let private TranslateInterface (ctx: Context) (i: Code.Interface) =
        let (ctx, ctd) = TranslateTypeDeclaration true ctx i
        for x in i.BaseInterfaces do
            TranslateType ctx x
            |> ctd.BaseTypes.Add
            |> ignore
        ctd

    let rec private TranslateClass (ctx: Context) (c: Code.Class) =
        let (ctx, ctd) = TranslateTypeDeclaration false ctx c
        let bases =
            [ if c.BaseClass.IsSome then
                yield c.BaseClass.Value
              for x in c.ImplementedInterfaces do
                yield x
            ]
        for t in bases do
            TranslateType ctx t
            |> ctd.BaseTypes.Add
            |> ignore
        for x in c.Constructors do
            CompileConstructor ctx ctd c x
        let hasZeroConstructor =
            Seq.cast<CodeTypeMember> ctd.Members
            |> Seq.exists (function
                | :? CodeConstructor as c   -> c.Parameters.Count = 0
                | _                         -> false)
        if not hasZeroConstructor then
            let cc = new CodeConstructor()
            cc.Attributes <- MemberAttributes.Assembly
            ctd.Members.Add(cc) |> ignore
        for nt in c.NestedClasses do
            TranslateClass ctx nt
            |> ctd.Members.Add
            |> ignore
        for ni in c.NestedInterfaces do
            TranslateInterface ctx ni
            |> ctd.Members.Add
            |> ignore
        ctd

    let private CompileNamespace (ctx: Context) 
                                 (ccu: CodeCompileUnit)
                                 (ns: Code.Namespace) =
        let cn = new CodeNamespace(ns.Name)
        ccu.Namespaces.Add cn |> ignore
        for c in ns.Classes do
            TranslateClass ctx.Indent c
            |> cn.Types.Add
            |> ignore
        for i in ns.Interfaces do
            TranslateInterface ctx.Indent i
            |> cn.Types.Add
            |> ignore

    let private MakeContext (provider: Compiler.CodeDomProvider)
                            (options: Compiler.CodeGeneratorOptions)
                            (namespaces: list<Code.Namespace>) =
        let getIds (namespaces: list<Code.Namespace>) =
            let getInterface (prefix: string) (i: Code.Interface) = 
                (prefix + GetSourceName i, i.Id)
            let rec getClass (prefix: string) (c: Code.Class) = [
                let n = GetSourceName c
                yield (prefix + n, c.Id)
                for nc in c.NestedClasses do
                    yield! getClass (prefix + n + "+") nc
                for ni in c.NestedInterfaces do
                    yield getInterface (prefix + n + "+") ni
            ]
            [ for ns in namespaces do
                for i in ns.Interfaces do
                    yield getInterface (ns.Name + ".") i
                for t in ns.Classes do
                    yield! getClass (ns.Name + ".") t ]
        let ids = new Dictionary<Type.Id,string>()
        for (n, id) in getIds namespaces do
            ids.[id] <- n
        let resolve (dict: Dictionary<_,_>) x =
            if dict.ContainsKey x then Some dict.[x] else None
        {
            Indentation = 0
            Resolve = resolve ids
            CodeDomProvider = provider
            CodeGeneratorOptions = options
        }

    /// Compiles a program.
    let Compile (writer: TextWriter) (assembly: Code.Assembly) =
        use provider = new CSharpCodeProvider()
        try
            let opts    = new Compiler.CodeGeneratorOptions()
            let ccu     = new CodeCompileUnit()
            let ctx     = MakeContext provider opts assembly.Namespaces
            for ns in assembly.Namespaces do
                CompileNamespace ctx ccu ns
            provider.GenerateCodeFromCompileUnit(ccu, writer, opts)
        with exn ->
            writer.Write exn
