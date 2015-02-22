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

/// Validates reflected assemblies to verify that custom attributes
/// are used correctly. Performs analysis useful for other compiler
/// stages.
module internal WebSharper.Compiler.Validator

module I = WebSharper.Compiler.Inlining
module M = WebSharper.Core.Macros
module Me = WebSharper.Core.Metadata
module R = WebSharper.Core.Reflection
module Re = WebSharper.Compiler.Reflector
module P = WebSharper.Core.JavaScript.Packager
module Q = WebSharper.Core.Quotations

type RecordField = string
type Requirement = R.TypeDefinition

type Status =
    | Compiled
    | Ignored

type Name = P.Address

type ConstructorKind =
    | InlineConstructor of Inlining.Inline
    | JavaScriptConstructor of Q.Expression
    | MacroConstructor of R.Type * M.Macro
    | StubConstructor of Name

type Constructor =
    {
        Currying : list<int>
        Kind : ConstructorKind
        Location : Location
        Name : Name
        Reference : R.Constructor
        Requirements : list<Requirement>
        Slot : Re.MemberSlot
    }

type RemotingKind =
    | RemoteAsync
    | RemoteSend
    | RemoteSync

type MethodKind =
    | InlineMethod of Inlining.Inline
    | JavaScriptMethod of Q.Expression
    | MacroMethod of R.Type * M.Macro
    | RemoteMethod of RemotingKind * ref<option<Me.MethodHandle>>
    | StubMethod

type Method =
    {
        Currying : list<int>
        Definition : MethodDefinition
        Kind : MethodKind
        Location : Location
        Name : Name
        Reference : R.Method
        Requirements : list<Requirement>
        Scope : MemberScope
        Slot : Re.MemberSlot
    }

type PropertyKind =
    | BasicProperty of option<Method> * option<Method>
    | OptionalProperty
    | FieldProperty of int
    | InlineModuleProperty of Inlining.Inline
    | InterfaceProperty
    | JavaScriptModuleProperty of Q.Expression
    | StubProperty

type RecordProperty =
    {
        JavaScriptName : string
        OriginalName : string
        PropertyType : TypeReference
        OptionalField : bool
    }

type Property =
    {
        Kind : PropertyKind
        Location : Location
        Name : Name
        PropertyType : TypeReference
        Reference : R.Property
        Scope : MemberScope
        Slot : Re.MemberSlot
    }

and TypeKind =
    | Class of ClassKind
    | Exception
    | Interface
    | Module of list<Type>
    | Record of list<RecordProperty>
    | Resource
    | Union of list<UnionCase>

    member Nested : list<Type>

and ClassKind =
    {
        Slot : Re.ClassSlot
        BaseClass : option<R.Type>
        Constructors : list<Constructor>
        Nested : list<Type>
        Fields : list<string>
        FieldRenames : list<string * string * bool>   
    }

and Type =
    {
        Kind : TypeKind
        Location : Location
        Methods : list<Method>
        Name : Name
        Properties : list<Property>
        Proxy : option<R.TypeDefinition>
        Reference : R.TypeDefinition
        ReflectorType : Re.Type
        Requirements : list<Requirement>
        Status : Status
    }

    member Nested : list<Type>

and UnionCase =
    {
        Kind : UnionCaseKind
        Location : Location
        Reference : R.UnionCase
        Requirements : list<Requirement>
    }

and UnionCaseKind =
    | BasicUnionCase
    | ConstantUnionCase of Value

type Assembly =
    {
        Name : R.AssemblyName
        Location : Location
        Mode : Me.AssemblyMode
        RemotingProvider : option<R.TypeDefinition>
        Requirements : list<Requirement>
        Types : list<Type>
    }

val Validate : Logger -> I.Pool -> Re.Pool -> (R.TypeDefinition -> list<string>) -> Re.Assembly -> Assembly
