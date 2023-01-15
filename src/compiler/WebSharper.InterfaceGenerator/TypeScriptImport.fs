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

type TSParameter =
    {
        Name: string
        Type: TSType
    }

and [<NamedUnionCases "Kind">] TSType =
    | [<Name "simple">] TSSimpleType of Type: string
    | [<Name "literal">] TSLiteralType of Value: string
    | [<Name "array">] TSArrayType of ElementType: TSType
    | [<Name "tuple">] TSTupleType of ElementTypes: TSType[]
    | [<Name "function">] TSFunctionType of Parameters: TSParameter[] * ReturnType: TSType
    | [<Name "new">] TSNewType of Parameters: TSParameter[] * ReturnType: TSType
    | [<Name "union">] TSUnionType of Types: TSType[]
    | [<Name "intersection">] TSIntersectionType of Types: TSType[]
    | [<Name "conditional">] TSConditionalType of CheckType: TSType * ExtendsType: TSType * TrueType: TSType * FalseType: TSType
    | [<Name "object">] TSTypeLiteral of Members: TSTypeElement[]
    | [<Name "typeref">] TSTypeReference of Type: string * Arguments: TSType[]
    | [<Name "typeparamref">] TSTypeParamReference of Type: string
    | [<Name "predicate">] TSTypePredicate of Type: TSType * Parameter: string
    | [<Name "index">] TSIndexType of Index: TSType * Type: TSType
    | [<Name "keyof">] TSKeyOfType of Type: TSType
    | [<Name "mapped">] TSMappedType of Type: TSType
    | [<Name "query">] TSQueryType of Expression: string

and TSTypeParameter =
    {
        Name: string
        Constraint: TSType
    }

and [<NamedUnionCases "Kind">] TSTypeElement =
    | [<Name "method">] TSMethod of Name: string * Parameters: TSParameter[] * TypeParameters: TSTypeParameter[] * Type: TSType
    | [<Name "property">] TSProperty of Name: string * Type: TSType
    | [<Name "new">] TSNew of Parameters: TSParameter[] * Type: TSType
    | [<Name "call">] TSCall of Parameters: TSParameter[] * TypeParameters: TSTypeParameter[] * Type: TSType
    | [<Name "get">] TSGet of Name: string * Type: TSType
    | [<Name "set">] TSSet of Name: string * Type: TSType
    | [<Name "index">] TSIndex of Parameters: TSParameter[] * Type: TSType

and [<NamedUnionCases "Kind">] TSStatement =
    | [<Name "vars">] TSVariableStatement of Declarations: TSParameter[]
    | [<Name "function">] TSFunction of Parameters: TSParameter[] * ReturnType: TSType
    | [<Name "new">] TSNew of Parameters: TSParameter[] * ReturnType: TSType
    | [<Name "typealias">] TSTypeAlias of Name: string * TypeParameters: TSTypeParameter[] * Type: TSType
    | [<Name "class">] TSClassDeclaration of Name: string * TypeParameters: TSTypeParameter[] * Members: TSTypeElement[] * Extends: TSType * Implements: TSType[]
    | [<Name "interface">] TSInterfaceDeclaration of Name: string * TypeParameters: TSTypeParameter[] * Members: TSTypeElement[] * Extends: TSType[]
    | [<Name "module">] TSModuleDeclaration of Name: string * Members: TSStatement[]

and TSFile =
    {
        Name: string
        Statements: TSStatement[]
    }
