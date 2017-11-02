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

module WebSharper.InterfaceGenerator.TypeScriptImport

open WebSharper

module J = WebSharper.Core.Json 

type OutputParameter =
    {
      name: string
      ``type``: OutputType
    }

and [<NamedUnionCases "kind">] OutputType =
    | [<Name "simple">] OutputSimpleType of ``type``: string
    | [<Name "array">] OutputArrayType of elementType: OutputType
    | [<Name "tuple">] OutputTupleType of elementTypes: OutputType[]
    | [<Name "function">] OutputFunctionType of parameters: OutputParameter[] * returnType: OutputType
    | [<Name "new">] OutputNewType of parameters: OutputParameter[] * returnType: OutputType
    | [<Name "union">] OutputUnionType of types: OutputType[]
    | [<Name "intersection">] OutputIntersectionType of types: OutputType[]
    | [<Name "object">] OutputTypeLiteral of members: OutputTypeElement[]
    | [<Name "typeref">] OutputTypeReference of ``type``: string * arguments: OutputType[]
    | [<Name "typeparamref">] OutputTypeParamReference of ``type``: string
    | [<Name "predicate">] OutputTypePredicate of ``type``: OutputType * parameter: string
    | [<Name "index">] OutputIndexType of index: OutputType * ``type``: OutputType
    | [<Name "keyof">] OutputKeyOfType of ``type``: OutputType
    | [<Name "mapped">] OutputMappedType of ``type``: OutputType

and OutputTypeParameter =
    {
      name: string
      ``constraint``: OutputType
    }

and [<NamedUnionCases "kind">] OutputTypeElement =
    | [<Name "method">] OutputMethod of name: string * parameters: OutputParameter[] * typeParameters: OutputTypeParameter[] * ``type``: OutputType
    | [<Name "property">] OutputProperty of name: string * ``type``: OutputType
    | [<Name "new">] OutputNew of parameters: OutputParameter[] * ``type``: OutputType
    | [<Name "call">] OutputCall of parameters: OutputParameter[] * typeParameters: OutputTypeParameter[] * ``type``: OutputType
    | [<Name "index">] OutputIndex of parameters: OutputParameter[] * ``type``: OutputType

and [<NamedUnionCases "kind">] OutputStatement =
    | [<Name "vars">] OutputVariableStatement of declarations:OutputParameter[]
    | [<Name "function">] OutputFunction of parameters: OutputParameter[] * returnType: OutputType
    | [<Name "new">] OutputNew of parameters: OutputParameter[] * returnType: OutputType
    | [<Name "typealias">] OutputTypeAlias of name: string * typeParameters: OutputTypeParameter[] * ``type``: OutputType
    | [<Name "class">] OutputClassDeclaration of name: string * typeParameters: OutputTypeParameter[]  * members: OutputTypeElement[] * extends: OutputType * implements: OutputType[]
    | [<Name "interface">] OutputInterfaceDeclaration of name: string * typeParameters: OutputTypeParameter[]  * members: OutputTypeElement[] * extends: OutputType[]
    | [<Name "module">] OutputModuleDeclaration of name: string * members: OutputStatement[]
