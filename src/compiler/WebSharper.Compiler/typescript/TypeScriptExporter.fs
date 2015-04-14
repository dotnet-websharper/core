// $begin{copyright}
//
// This file is part of WebSharper
//
// Copyright (c) 2008-2014 IntelliFactory
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

namespace WebSharper

module internal TypeScriptExporter =
    open System
    open System.Collections.Generic
    open System.IO
    open System.Text.RegularExpressions
    open WebSharper.Compiler
    module CM = Metadata
    module T = TypeScriptGenerator
    module P = WebSharper.Core.JavaScript.Packager
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

    let convertAddress ctx mem addr =
        let rec convertAddress ctx addr =
            match addr with
            | P.Global x -> ctx.Builder.Root(cleanName x)
            | P.Local (x, y) ->
                let x = convertAddress ctx x
                ctx.Builder.Nested(x, cleanName y)
        try
            convertAddress ctx addr
        with
        | QualifiedNames.InvalidIdentifier id ->
            failwithf "Invalid identifier: %s in the compiled name for %s" id mem

    let getContract ctx tR =
        ctx.GetContractImplementation ctx tR

    let makeGenerics prefix count =
        [ for i in 1 .. count -> prefix + string i ]

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
            | _ -> fType [d] r
            |> Some
        | _ -> None

    let getFuncContract ctx fT =
        match fT.FArgs with
        | [] ->
            T.Contract.Function([], None, getContract ctx fT.FRet)
        | [x] ->
            T.Contract.Function(["x", getContract ctx x], None, getContract ctx fT.FRet)
        | _ -> failwith "TODO: add more function conventions"

    let getNamedContract ctx (tR: TypeReference) ts =
        match ctx.CM.DataType(Adapter.AdaptTypeDefinition(tR)) with
        | Some (CM.DataTypeKind.Class (addr, _, _))
        | Some (CM.DataTypeKind.Exception addr)
        | Some (CM.DataTypeKind.Interface addr)
        | Some (CM.DataTypeKind.Record (addr, _)) ->
            let addr = convertAddress ctx tR.FullName addr
            let decl = T.Declaration.Create(addr, makeGenerics "T" tR.GenericArity)
            T.Contract.Named(decl, ts)
        | Some (CM.DataTypeKind.Object fields) -> T.Contract.Any
        | None -> T.Contract.Any

    let rec getContractImpl ctx (tR: TypeReference) =
        match tR.Shape with
        | TypeShape.ArrayType (1, r) ->
            getContract ctx r
            |> T.Contract.Array
        | TypeShape.ArrayType (2, r) ->
            getContract ctx r
            |> T.Contract.Array
            |> T.Contract.Array
        | TypeShape.ArrayType _ ->
            T.Contract.Any
        | TypeShape.GenericInstanceType ts ->
            match tR with
            | FuncType ts fT ->
                getFuncContract ctx fT
            | TupleType ts ->
                T.Contract.Tuple [for t in ts -> getContract ctx t]
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

    let getSignature ctx staticGenerics (m: MethodReference) =
        let mutable s = T.Signature.Create(staticGenerics @ makeGenerics "M" m.GenericArity)
        let ctx = { ctx with GenericSignature = Some s }
        let mutable i = 0
        for p in m.Parameters do
            let name =
                match p.Name with
                | "" | null -> "x" + string i
                | n -> n
            s <- s.WithArgument(name, getContract ctx p.ParameterType)
            i <- i + 1
        match m.ReturnType with
        | None -> s
        | Some rT -> s.WithReturn(getContract ctx rT)

    let convMethod ctx isInterfaceMethod staticGenerics (m: V.Method) =
        let isExported =
            isInterfaceMethod ||
            match m.Kind with
            | V.JavaScriptMethod _
            | V.CoreMethod _
            | V.SyntaxMethod _ -> true
            | _ -> false
        if isExported then
            Some (getSignature ctx staticGenerics m.Definition)
        else
            None

    let exportStaticMethod ctx tgen (m: V.Method) =
        let addr = convertAddress ctx (string m.Reference) m.Name
        match convMethod ctx false tgen m with
        | None -> []
        | Some s ->
            let c =
                [T.Member.Call s]
                |> T.Interface.Create
                |> T.Contract.Anonymous
            [T.Definitions.Var(addr, c)]

    let exportStaticMethods ctx tgen (t: V.Type) =
        seq {
            for m in t.Methods do
                match m.Scope with
                | MemberScope.Static ->
                    yield! exportStaticMethod ctx tgen m
                | MemberScope.Instance -> ()
        }

    let exportStaticProperties ctx tgen (t: V.Type) =
        seq {
            for p in t.Properties do
                match p.Scope with
                | MemberScope.Static ->
                    match p.Kind with
                    | V.BasicProperty (m1, m2) ->
                        let p m =
                            match m with
                            | None -> []
                            | Some m -> exportStaticMethod ctx tgen m
                        yield! p m1
                        yield! p m2
                    | V.JavaScriptModuleProperty _ ->
                        let addr = convertAddress ctx (string p.Reference) p.Slot.Address.Address
                        let s =
                            T.Signature.Create().WithReturn(getContract ctx p.PropertyType)
                        let con =
                            [ T.Member.Call s ]
                            |> T.Interface.Create
                        yield T.Definitions.Var(addr, T.Contract.Anonymous con)
                    | _ -> ()
                | _ -> ()
        }

    let exportNamedContract ctx tgen (t: V.Type) isIF ext intf =
        let addr = convertAddress ctx (string t.Reference.FullName) t.Name
        let decl = T.Declaration.Create(addr, tgen)
        let ctx = { ctx with GenericDeclaration = Some decl }
        let emitMethod m =
            match convMethod ctx isIF [] m with
            | None -> []
            | Some s -> [T.Member.Method(m.Name.LocalName, s)]
        let emitMethodOpt m =
            match m with
            | None -> []
            | Some m -> emitMethod m
        let members =
            [
                for m in t.Methods do
                    match m.Scope with
                    | MemberScope.Instance ->
                        if not (m.Definition.IsGetter || m.Definition.IsSetter) then
                            yield! emitMethod m
                    | MemberScope.Static -> ()
                for p in t.Properties do
                    match p.Scope with
                    | MemberScope.Instance ->
                        match p.Kind with
                        | V.PropertyKind.BasicProperty (gM, sM) ->
                            yield! emitMethodOpt gM
                            yield! emitMethodOpt sM
                        | _ -> ()
                    | _ -> ()
                match t.Kind with
                | V.TypeKind.Record props ->
                    for p in props do
                        yield T.Member.Property(p.JavaScriptName, getContract ctx p.PropertyType, p.OptionalField)
                | V.TypeKind.Union _ ->
                    yield T.Member.Property("$", T.Contract.Number)
                | _ -> ()
            ]
        if isIF then
            let ext = intf |> Seq.map (getContract ctx) 
            T.Definitions.Define(decl, T.Interface.Create (members, ext = ext))
        else 
            let ext = ext |> Option.map (getContract ctx) 
            let impl = intf |> Seq.map (getContract ctx) |> Seq.filter ((<>) T.Contract.Any)
            T.Definitions.Define(decl, T.Class.Create (members, ?ext = ext, impl = impl))

    let exportUnionCase ctx tgen (t: V.Type) ci (uc: V.UnionCase) =
        let baseAddr = convertAddress ctx (string t.Reference.FullName) t.Name
        let addr = baseAddr.Builder.Nested(baseAddr, sprintf "_%d_%s" ci uc.Reference.Name)
        let baseDecl = T.Declaration.Create(baseAddr, tgen)    
        let decl = T.Declaration.Create(addr, tgen)     
        let ctx = { ctx with GenericDeclaration = Some decl }
        let members =
            uc.Definition.Parameters |> List.mapi (fun i p ->
                T.Member.Property("$" + string i, getContract ctx p.ParameterType)
            )
        let baseGen = tgen |> List.mapi (fun i _ -> T.Contract.Generic(decl, i))
        T.Definitions.Define(decl, T.Class.Create(members, ext = T.Contract.Named(baseDecl, baseGen)))

    let rec exportType ctx (t: V.Type) =
        seq {
            match t.Kind with
            | V.TypeKind.Class { Nested = nT }
            | V.TypeKind.Module nT ->
                for t in nT do
                    yield! exportType ctx t
            | _ -> ()
            if t.ReflectorType.Definition.IsPublic || t.Proxy.IsSome then
                let tgen = makeGenerics "T" t.ReflectorType.Definition.GenericArity
                yield! exportStaticMethods ctx tgen t
                yield! exportStaticProperties ctx tgen t
                match t.Kind with
                | V.TypeKind.Class c ->
                    yield exportNamedContract ctx tgen t false t.ReflectorType.Definition.BaseType t.ReflectorType.Definition.Interfaces
                | V.TypeKind.Exception ->
                    yield exportNamedContract ctx tgen t false None []
                | V.TypeKind.Interface ->
                    yield exportNamedContract ctx tgen t true None t.ReflectorType.Definition.Interfaces
                | V.TypeKind.Module _ -> ()
                | V.TypeKind.Record props ->
                    yield exportNamedContract ctx tgen t true None t.ReflectorType.Definition.Interfaces
                | V.TypeKind.Resource _ -> ()
                | V.TypeKind.Union ucs ->
                    yield exportNamedContract ctx tgen t false None t.ReflectorType.Definition.Interfaces
                    yield! ucs |> List.mapi (exportUnionCase ctx tgen t)   
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


