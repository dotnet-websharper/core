// $begin{copyright}
//
// This file is part of WebSharper
//
// Copyright (c) 2008-2013 IntelliFactory
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

namespace IntelliFactory.WebSharper

module internal TypeScriptExporter =
    open System
    open System.IO
    open IntelliFactory.WebSharper.Compiler
    module CM = Metadata
    module T = TypeScriptGenerator
    module P = IntelliFactory.JavaScript.Packager
    module V = Validator

    type Context =
        {
            CM : Metadata.T
            Builder : T.AddressBuilder
        }

    let emptyDefs =
        T.Definitions.Merge []

    let rec convertAddress ctx (addr: P.Address) =
        match addr with
        | P.Global x -> ctx.Builder.Root(x)
        | P.Local (x, y) ->
            let x = convertAddress ctx x
            ctx.Builder.Nested(x, y)

    let rec getContract ctx (tR: TypeReference) =
        match tR.Shape with
        | TypeShape.ArrayType (1, r) ->
            getContract ctx r
            |> T.Contract.Array
        | TypeShape.ArrayType _ ->
            T.Contract.Any
        | TypeShape.GenericInstanceType ts ->
            match ctx.CM.DataType(Adapter.AdaptTypeDefinition(tR)) with
            | Some (CM.DataTypeKind.Class addr)
            | Some (CM.DataTypeKind.Exception addr) ->
                let gs =
                    if tR.HasGenericParameters
                    then [for i in 1 .. tR.GenericArity -> "T" + string i]
                    else []
                let a = convertAddress ctx addr
                let decl = T.Declaration.Create(a, gs)
                T.Contract.Named(decl, [for t in ts -> getContract ctx t])
            | _ ->
                T.Contract.Any
        | TypeShape.GenericParameter (_, _) ->
            T.Contract.Any
        | TypeShape.OtherType ->
            if tR.IsPrimitive then
                if tR.Name = "Boolean" then
                    T.Contract.Boolean
                else
                    T.Contract.Number
            else
                match tR.FullName with
                | "System.String" -> T.Contract.String
                | "System.Void"
                | "Microsoft.FSharp.Core.Unit" -> T.Contract.Void
                | _ -> T.Contract.Any

    let exportMethod ctx (m: Validator.Method) =
        match m.Kind with
        | V.MethodKind.InlineMethod _
        | V.MethodKind.MacroMethod _
        | V.MethodKind.RemoteMethod _ -> None
        | V.MethodKind.JavaScriptMethod _ ->
            let addr = convertAddress ctx m.Slot.Address.Address
            let mutable s = T.Signature.Create()
            for p in m.Definition.Parameters do
                s <- s.WithArgument(p.Name, getContract ctx p.ParameterType)
            match m.Definition.ReturnType with
            | None -> ()
            | Some rT ->
                s <- s.WithReturn(getContract ctx rT)
            let contract =
                T.Interface.Create [
                    T.Member.Call(s)
                ]
            T.Definitions.Var(addr, T.Contract.Anonymous contract)
            |> Some
        | V.MethodKind.StubMethod _ ->
            None

    let rec exportType ctx (t: Validator.Type) : seq<T.Definitions> =
        match t.Kind with
        | V.TypeKind.Class _
        | V.TypeKind.Exception _
        | V.TypeKind.Interface _ ->
            Seq.empty
        | V.TypeKind.Module nestedTypes ->
            seq  {
                for t in nestedTypes do
                    yield! exportType ctx t
                for m in t.Methods do
                    match exportMethod ctx m with
                    | None -> ()
                    | Some r -> yield r
            }
        | V.TypeKind.Record _
        | V.TypeKind.Resource _
        | V.TypeKind.Union _ ->
            Seq.empty

    let ExportDeclarations cm (v: Validator.Assembly) =
        let ctx =
            {
                Builder = T.AddressBuilder.Create()
                CM = cm
            }
        let defs =
            seq {
                for t in v.Types do
                    yield! exportType ctx t
            }
            |> T.Definitions.Merge
        let text =
            use w = new StringWriter()
            defs.Write(w)
            w.ToString()
        text


