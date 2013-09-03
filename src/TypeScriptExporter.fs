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
    open System.Collections.Generic
    open System.IO
    open System.Text.RegularExpressions
    open IntelliFactory.WebSharper.Compiler
    module CM = Metadata
    module T = TypeScriptGenerator
    module P = IntelliFactory.JavaScript.Packager
    module Re = Reflector
    module V = Validator

    type Context =
        {
            Builder : T.AddressBuilder
            CM : Metadata.T
            GenericDeclaration : option<T.Declaration>
            GenericSignature : option<T.Signature>
            GetContractImplementation : Context -> TypeReference -> T.Contract
        }

        static member Create(cm, impl) =
            {
                Builder = T.AddressBuilder.Create()
                CM = cm
                GenericDeclaration = None
                GenericSignature = None
                GetContractImplementation = impl
            }

    let emptyDefs = T.Definitions.Merge []
    let pat = Regex(@"[^\w$]")

    let cleanName (s: string) =
        pat.Replace(s, "_")

    let rec convertAddress ctx (addr: P.Address) =
        match addr with
        | P.Global x -> ctx.Builder.Root(cleanName x)
        | P.Local (x, y) ->
            let x = convertAddress ctx x
            ctx.Builder.Nested(x, cleanName y)

    let getContract ctx tR =
        ctx.GetContractImplementation ctx tR

    type FType =
        {
            FArgs : list<TypeReference>
            FRet : TypeReference
        }

    let (|UnitType|_|) (tR: TypeReference) =
        if tR.Namespace = "Microsoft.FSharp.Core" && tR.Name = "Unit"
            then Some () else None

    let (|TupleType|_|) (tR: TypeReference) =
        match tR.Namespace, tR.Name with
        | "System", n when n.StartsWith("Tuple") ->
            let ps =
                match tR.Shape with
                | TypeShape.GenericInstanceType args -> args
                | _ -> []
            Some ps
        | _ -> None

    let (|FuncType|_|) ts (tR: TypeReference) =
        match tR.Namespace, tR.Name, ts with
        | "Microsoft.FSharp.Core", "FSharpFunc`2", [d; r] ->
            let fType args r = { FArgs = args; FRet = r }
            match d with
            | UnitType -> fType [] r
            | TupleType ts -> fType ts r
            | _ -> fType [d] r
            |> Some
        | _ -> None

    let getFuncContract ctx fT =
        let call =
            fT.FArgs
            |> Seq.mapi (fun i t ->
                ("x" + string i, getContract ctx t))
            |> Seq.fold (fun (s: T.Signature) (n, t) ->
                s.WithArgument(n, t))
                (T.Signature.Create [])
            |> fun s -> s.WithReturn(getContract ctx fT.FRet)
            |> T.Member.Call
        T.Interface.Create [call]
        |> T.Contract.Anonymous

    let makeGenerics prefix count =
        [ for i in 1 .. count -> prefix + string i ]

    let getNamedContract ctx (tR: TypeReference) ts =
        match ctx.CM.DataType(Adapter.AdaptTypeDefinition(tR)) with
        | Some (CM.DataTypeKind.Class addr)
        | Some (CM.DataTypeKind.Exception addr)
        | Some (CM.DataTypeKind.Record (addr, _)) ->
            let addr = convertAddress ctx addr
            let decl = T.Declaration.Create(addr, makeGenerics "T" tR.GenericArity)
            T.Contract.Named(decl, ts)
        | Some (CM.DataTypeKind.Object fields) -> T.Contract.Any
        | None -> T.Contract.Any

    let rec getContractImpl ctx (tR: TypeReference) =
        let inline getContract ctx tR = getContractImpl ctx tR
        match tR.Shape with
        | TypeShape.ArrayType (1, r) ->
            getContractImpl ctx r
            |> T.Contract.Array
        | TypeShape.ArrayType _ ->
            T.Contract.Any
        | TypeShape.GenericInstanceType ts ->
            match tR with
            | FuncType ts fT ->
                getFuncContract ctx fT
            | _ ->
                getNamedContract ctx tR [for t in ts -> getContract ctx t]
        | TypeShape.GenericParameter (OwnerMethod _, pos) ->
            match ctx.GenericSignature with
            | None -> T.Contract.Any
            | Some m -> T.Contract.Generic(m, pos)
        | TypeShape.GenericParameter (OwnerType _, pos) ->
            match ctx.GenericDeclaration with
            | None -> T.Contract.Any
            | Some d -> T.Contract.Generic(d, pos)
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
                | _ -> getNamedContract ctx tR []

    let getSignature ctx (m: MethodReference) =
        let s = T.Signature.Create(makeGenerics "M" m.GenericArity)
        let ctx = { ctx with GenericSignature = Some s }
        let s =
            (s, m.Parameters)
            ||> Seq.fold (fun s p -> s.WithArgument(p.Name, getContract ctx p.ParameterType))
        match m.ReturnType with
        | None -> s
        | Some rT -> s.WithReturn(getContract ctx rT)

    let convMethod ctx (m: V.Method) =
        match m.Kind with
        | V.JavaScriptMethod _ ->
            Some (getSignature ctx m.Definition)
        | _ -> None

    let exportStaticMethod ctx (m: V.Method) =
        let addr = convertAddress ctx m.Name
        match convMethod ctx m with
        | None -> []
        | Some s ->
            let c =
                [T.Member.Call s]
                |> T.Interface.Create
                |> T.Contract.Anonymous
            [T.Definitions.Var(addr, c)]

    let exportStaticMethods ctx (t: V.Type) =
        seq {
            for m in t.Methods do
                match m.Scope with
                | MemberScope.Static ->
                    yield! exportStaticMethod ctx m
                | MemberScope.Instance -> ()
        }

    let exportStaticProperties ctx (t: V.Type) =
        seq {
            for p in t.Properties do
                match p.Scope with
                | MemberScope.Static ->
                    match p.Kind with
                    | V.BasicProperty (m1, m2) ->
                        let p m =
                            match m with
                            | None -> []
                            | Some m -> exportStaticMethod ctx m
                        yield! p m1
                        yield! p m2
                    | _ -> ()
                | _ -> ()
        }

    let exportNamedContract ctx (t: V.Type) =
        let addr = convertAddress ctx t.Name
        let decl = T.Declaration.Create(addr, makeGenerics "T" t.ReflectorType.Definition.GenericArity)
        let ctx = { ctx with GenericDeclaration = Some decl }
        let emitMethod m =
            match m with
            | None -> []
            | Some m ->
                match convMethod ctx m with
                | None -> []
                | Some s -> [T.Member.Method(m.Name.LocalName, s)]
        let iF =
            [
                for m in t.Methods do
                    match m.Scope with
                    | MemberScope.Instance ->
                        yield! emitMethod (Some m)
                    | MemberScope.Static -> ()
                for p in t.Properties do
                    match p.Scope with
                    | MemberScope.Instance ->
                        match p.Kind with
                        | V.PropertyKind.BasicProperty (gM, sM) ->
                            yield! emitMethod gM
                            yield! emitMethod sM
                        | _ -> ()
                    | _ -> ()
            ]
            |> T.Interface.Create
        T.Definitions.Define(decl, iF)

    let rec exportType ctx (t: V.Type) =
        seq {
            yield! exportStaticMethods ctx t
            yield! exportStaticProperties ctx t
            match t.Kind with
            | V.TypeKind.Class (slot, baseType, ctorList, nestedTypes) ->
                match t.Status with
                | V.Status.Compiled ->
                    for t in nestedTypes do
                        yield! exportType ctx t
                    yield exportNamedContract ctx t
                | V.Status.Ignored -> ()
            | V.TypeKind.Exception _
            | V.TypeKind.Interface _ ->
                ()
            | V.TypeKind.Module nestedTypes ->
                for t in nestedTypes do
                    yield! exportType ctx t
            | V.TypeKind.Record _ ->
                yield exportNamedContract ctx t
            | V.TypeKind.Resource _ -> ()
            | V.TypeKind.Union _ ->
                yield exportNamedContract ctx t
        }

    let ExportDeclarations (cm: CM.T) (v: V.Assembly) =
        let ctx = Context.Create(cm, getContractImpl)
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


