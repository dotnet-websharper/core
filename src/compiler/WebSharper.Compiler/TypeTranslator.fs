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

// Main translation module from .NET forms to JavaScript
module WebSharper.Compiler.TypeTranslator

open WebSharper.Core
open WebSharper.Core.AST
open WebSharper.Core.Metadata

type LookupTypeResult =
    | Class of ClassInfo
    | Interface of InterfaceInfo
    | CustomType of PlainAddress * CustomTypeInfo
    | Unknown

type TypeTranslator(lookupType: TypeDefinition -> LookupTypeResult, ?tsTypeOfAddress) =
    let defaultTsTypeOfAddress (a: Address) =
        let t = a.Address.Value |> List.rev
        match a.Module with
        | StandardLibrary
        | JavaScriptFile _
        | CurrentModule -> TSType.Named t
        | WebSharperModule m -> TSType.Importing (m, t)
        | ImportedModule m -> TSType.Imported(m, t) 

    let tsTypeOfAddress = defaultArg tsTypeOfAddress defaultTsTypeOfAddress

    let mappedTypes = Dictionary()

    member this.TSTypeOfDef(t: TypeDefinition) =
        match mappedTypes.TryGetValue t with
        | true, tt -> tt
        | _ ->
            let res =
                match lookupType t with
                | Class c ->
                    match c.Type with
                    | Some t -> t
                    | _ ->
                        match c.Address with
                        | Some a -> tsTypeOfAddress a
                        | _ -> TSType.Any
                | Interface i ->
                    tsTypeOfAddress i.Address
                | CustomType (_, DelegateInfo i) -> 
                    TSType.Lambda(i.DelegateArgs |> List.map (this.TSTypeOf [||]), this.TSTypeOf [||] i.ReturnType)
                | CustomType (_, EnumInfo t) -> 
                    this.TSTypeOfDef t
                | CustomType(Hashed (_ :: _) as a, (FSharpRecordInfo _ | FSharpUnionInfo _ | FSharpUnionCaseInfo _)) ->
                    tsTypeOfAddress { Module = CurrentModule; Address = a }  
                | _ -> TSType.Any
            mappedTypes.Add(t, res)
            res

    member this.TSTypeOfGenParam i (p: GenericParam) =
        match p.Type with
        | Some t -> t
        | _ -> TSType.Param i

    member this.TSTypeOfConcrete (gs: GenericParam[]) (t: Concrete<TypeDefinition>) =
        let e = t.Entity
        let tn = e.Value.FullName
        if tn = "WebSharper.JavaScript.Optional`1" then
            match t.Generics with
            | [] -> TSType.Basic "undefined"
            | _ -> this.TSTypeOf gs t.Generics.Head
        elif tn.StartsWith "WebSharper.JavaScript.Union`" then
            TSType.Union (t.Generics |> List.map (this.TSTypeOf gs))
        elif tn = "WebSharper.JavaScript.Object`1" then
            TSType.ObjectOf(this.TSTypeOf gs t.Generics.Head)
        else
        let td = this.TSTypeOfDef e
        match t.Generics with
        | [] -> td
        | g -> 
            match td with
            | TSType.Importing _
            | TSType.Imported _
            | TSType.Named _ ->
                TSType.Generic(td, g |> List.map (this.TSTypeOf gs))
            | TSType.Lambda _ ->
                td.SubstituteGenerics (g |> Seq.map (this.TSTypeOf gs) |> Array.ofSeq)
            | _ -> td

    member this.TSTypeOf (gs: GenericParam[]) (t: Type) =
        match t with 
        | ConcreteType t -> this.TSTypeOfConcrete gs t
        | ArrayType (t, a) -> 
            match a with
            | 1 -> TSType.ArrayOf (this.TSTypeOf gs t)
            | 2 -> TSType.ArrayOf (TSType.ArrayOf (this.TSTypeOf gs t))
            | _ -> failwith "only 1 and 2-dim arrays are supported"
        | TupleType (ts, _) -> TSType.Tuple (ts |> List.map (this.TSTypeOf gs))
        | FSharpFuncType (a, r) -> 
            let ta =
                match a with
                | VoidType -> []
                | _ -> [this.TSTypeOf gs a]
            TSType.Lambda(ta, this.TSTypeOf gs r)
        | ByRefType t -> TSType.Any // TODO byrefs
        | VoidType -> TSType.Void
        | TypeParameter i 
        | StaticTypeParameter i -> 
            if i >= gs.Length then TSType.Param i else this.TSTypeOfGenParam i gs.[i]
        | LocalTypeParameter -> TSType.Any
