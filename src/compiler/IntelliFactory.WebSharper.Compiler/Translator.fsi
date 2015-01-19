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

/// Defines the translation of F# quotations to JavaScript core expressions.
module internal IntelliFactory.WebSharper.Compiler.Translator

module Q = IntelliFactory.WebSharper.Core.Quotations
module C = IntelliFactory.JavaScript.Core
module I = IntelliFactory.WebSharper.Compiler.Inlining
module R = IntelliFactory.WebSharper.Compiler.Reflector

/// Thrown when quotation invariants are broken.
exception InvalidQuotation

/// Translates a quotation to JavaScript.
val Translate :
    Logger -> I.Pool -> R.Pool -> remotingProviderQualifiedName:list<string> ->
    Metadata.T -> Location ->
    Q.Expression -> C.Expression
