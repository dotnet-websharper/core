// $begin{copyright}
//
// This file is part of WebSharper
//
// Copyright (c) 2008-2015 IntelliFactory
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

module WebSharper.Compiler.Adapter

module R = WebSharper.Core.Reflection

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
