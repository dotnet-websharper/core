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

/// Adapts Mono.Cecil representations to QuotationReader representations.
module internal WebSharper.Compiler.Adapter

module R  = WebSharper.Core.Reflection
module QR = WebSharper.Core.Quotations

/// Adapts a type definition (a class or a module).
val AdaptTypeDefinition : TypeReference -> R.TypeDefinition

/// Adapts a type.
val AdaptType : TypeReference -> R.Type

/// Adapts a constructor.
val AdaptConstructor : MethodReference -> R.Constructor

/// Adapts a method.
val AdaptMethod : MethodReference -> R.Method

/// Adapts a property.
val AdaptProperty : PropertyReference -> R.Property

/// Adapts a union case.
val AdaptUnionCase : TypeReference -> Name -> R.UnionCase
