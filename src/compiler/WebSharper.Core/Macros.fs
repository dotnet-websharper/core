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

/// Provides supporting types for implementing custom compilation
/// rules using MacroAttribute.
namespace WebSharper.Core

open WebSharper.Core.AST
//module D = CommonAST.DotNet
//module J = CommonAST.JavaScript
//module Q = WebSharper.Core.Quotations
//module R = WebSharper.Core.Reflection
module S = WebSharper.Core.JavaScript.Syntax

type MacroResult =
    | MacroOk of Expression
    | MacroWarning of string * MacroResult
    | MacroError of string
    | MacroFallback
    | MacroNeedsResolvedTypeArg

    static member Map f m =
        match m with
        | MacroWarning (w, m) -> MacroWarning (w, MacroResult.Map f m)
        | MacroOk e -> MacroOk (f e)
        | m -> m

    static member Bind f m =
        match m with
        | MacroWarning (w, m) -> MacroWarning (w, MacroResult.Bind f m)
        | MacroOk e -> f e
        | m -> m

/// Represents method bodies, at either JavaScript core or syntax level.
type GeneratorResult =
    | GeneratedQuotation of Microsoft.FSharp.Quotations.Expr
    | GeneratedAST of Expression
//    | GeneratedString of string
//    | GeneratedJavaScript of S.Expression
    | GeneratorError of string
    | GeneratorWarning of string * GeneratorResult

/// An abstract base class for macro definitions used with MacroAttribute.
[<AbstractClass>]
type Macro() =
    abstract member TranslateCall: thisArg: option<Expression> * targetType: Concrete<TypeDefinition> * methodDef: Concrete<Method> * arguments: list<Expression> * parameter: option<obj> -> MacroResult
    override this.TranslateCall(_,_,_,_,_) = failwithf "TranslateCall not implemented for macro %s" (this.GetType().FullName)
    
    abstract member TranslateCtor: targetType: Concrete<TypeDefinition> * ctorDef: Constructor * arguments: list<Expression> * parameter: option<obj> -> MacroResult
    override this.TranslateCtor(_,_,_,_) = failwithf "TranslateCall not implemented for macro %s" (this.GetType().FullName)

type GeneratedMember =
    | GeneratedMethod of TypeDefinition * Method
    | GeneratedConstructor of TypeDefinition * Constructor
    | GeneratedImplementation of TypeDefinition * TypeDefinition * Method

/// An abstract base class for code generation used with GeneratedAttribute.
[<AbstractClass>]
type Generator() =
//    abstract member Generate : arguments: list<Id> * parameter: option<obj> -> GeneratorResult
    abstract member Generate : mem:GeneratedMember *  parameter: option<obj> -> GeneratorResult