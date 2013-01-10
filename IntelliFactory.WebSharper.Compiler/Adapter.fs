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

let rec AdaptTypeDefinition (tR: TypeReference) =
    match tR.DeclaringType with
    | None ->
        let assemblyName =
            tR.AssemblyName
            |> R.AssemblyName.Parse
        R.TypeDefinition.Create assemblyName tR.Namespace tR.Name
    | Some dT ->
        R.TypeDefinition.CreateNested (AdaptTypeDefinition dT) tR.Name

let AdaptType (t: TypeReference) =
    let rec (!) (p: TypeReference) =
        match p.Shape with
        | TypeShape.GenericParameter (owner, position) ->
            let rec count z (tR: option<TypeReference>) =
                match tR with
                | None -> z
                | Some tR ->
                    let j = tR.GenericArity
                    count (z + j) tR.DeclaringType
            let k =
                match owner with
                | OwnerMethod mR -> count 0 (Some mR.DeclaringType) + position
                | OwnerType tR -> count 0 tR.DeclaringType + position
            R.Type.Generic k
        | TypeShape.ArrayType (rank, elT) ->
            R.Type.Array (!elT, rank)
        | TypeShape.GenericInstanceType args ->
            let d = AdaptTypeDefinition p
            R.Type.Concrete (d, [for x in args -> !x])
        | TypeShape.OtherType ->
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
        m.GenericArity
        (AdaptParameters m.Parameters)
        (match m.ReturnType with
            | None -> R.Type.FromType typeof<System.Void>
            | Some t -> AdaptType t)

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
