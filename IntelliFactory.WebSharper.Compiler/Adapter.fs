// $begin{copyright}
//
// This file is part of WebSharper
//
// Copyright (c) 2008-2012 IntelliFactory
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

module IntelliFactory.WebSharper.Compiler.Adapter

module R = IntelliFactory.WebSharper.Core.Reflection

open Mono.Cecil

let rec AdaptTypeDefinition (tR: Mono.Cecil.TypeReference) =
    match tR.DeclaringType with
    | null ->
        let assemblyName =
            match tR.Scope.MetadataScopeType with
            | MetadataScopeType.AssemblyNameReference ->
                let anr = tR.Scope :?> AssemblyNameReference
                anr.FullName
            | _ ->
                tR.Module.Assembly.FullName
            |> R.AssemblyName.Parse
        R.TypeDefinition.Create assemblyName tR.Namespace tR.Name
    | dT ->
        R.TypeDefinition.CreateNested (AdaptTypeDefinition dT) tR.Name

let AdaptType (t: TypeReference) =
    let rec (!) (p: TypeReference) =
        if p.IsGenericParameter then
            let p : GenericParameter = downcast p
            let rec count z (tR: TypeReference) =
                match tR with
                | null -> z
                | tR ->
                    let j =
                        if tR.HasGenericParameters
                        then tR.GenericParameters.Count 
                        else 0
                    count (z + j) tR.DeclaringType
            let k =
                match p.Owner with
                | :? Mono.Cecil.MethodReference as mR ->
                    count 0 mR.DeclaringType + p.Position
                | :? Mono.Cecil.TypeReference as tR ->
                    count 0 tR.DeclaringType + p.Position
                | _ ->
                    failwith "Problems with resolving generics."
            R.Type.Generic k
        elif p.IsArray then
            let p : ArrayType = downcast p
            R.Type.Array (!p.ElementType, p.Rank)
        elif p.IsGenericInstance then
            let p : GenericInstanceType = downcast p
            let d = AdaptTypeDefinition p
            R.Type.Concrete (d, [for x in p.GenericArguments -> !x])
        else
            let d = AdaptTypeDefinition p
            R.Type.Concrete (d, [])
    !t

let AdaptParameters (ps: seq<ParameterDefinition>) =
    [for p in ps -> AdaptType p.ParameterType]

let AdaptConstructor (c: MethodReference) =
    R.Constructor.Create
        (AdaptTypeDefinition c.DeclaringType)
        (AdaptParameters c.Parameters)

let AdaptMethod (m: MethodReference) =
    R.Method.Create
        (AdaptTypeDefinition m.DeclaringType)
        m.Name
        m.GenericParameters.Count
        (AdaptParameters m.Parameters)
        (AdaptType m.ReturnType)

let AdaptProperty (p: PropertyReference) =
    R.Property.Create
        (AdaptTypeDefinition p.DeclaringType)
        p.Name
        (AdaptType p.PropertyType)
        (AdaptParameters p.Parameters)

let AdaptUnionCase t name =
    R.UnionCase.Create
        (AdaptTypeDefinition t)
        name
