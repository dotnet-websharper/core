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
open System.Collections.Generic

/// Input for a TranslateCall method on a macro
type MacroCall =
    {
         This : option<Expression>
         DefiningType : Concrete<TypeDefinition>
         Method : Concrete<Method>
         Arguments: list<Expression>
         Parameter: option<obj>
         IsInline: bool
         Compilation: Metadata.ICompilation
         BoundVars: IReadOnlyDictionary<Id, Expression>
    }

/// Input for a TranslateCtor method on a macro
type MacroCtor =
    {
         DefiningType : Concrete<TypeDefinition>
         Constructor : Constructor
         Arguments: list<Expression>
         Parameter: option<obj>
         IsInline: bool
         Compilation: Metadata.ICompilation
         BoundVars: IReadOnlyDictionary<Id, Expression>
    }

/// The return type of macro methods 
type MacroResult =
    /// An expression to inline at call point for macroed member.
    /// Can contain JavaScript and .NET level AST nodes.
    | MacroOk of Expression
    /// Add a source warning at the call point for macroed member.
    | MacroWarning of string * MacroResult
    /// Add a source error at the call point for macroed member.
    | MacroError of string
    /// Add code dependencies to the member containing the call for the macroed member.
    | MacroDependencies of list<Metadata.Node> * MacroResult
    /// Revert to next in chain tranlation strategy for the call.
    | MacroFallback
    /// Report that the macro needs concrete type information.
    /// Delays compilation of inlined calls until type resolution. 
    | MacroNeedsResolvedTypeArg of Type
    /// Report that the macro has made use of the expression of an outside let binding
    /// which now can be removed
    | MacroUsedBoundVar of Id * MacroResult

    static member Map f m =
        match m with
        | MacroWarning (w, m) -> MacroWarning (w, MacroResult.Map f m)
        | MacroDependencies (d, m) -> MacroDependencies (d, MacroResult.Map f m)
        | MacroOk e -> MacroOk (f e)
        | m -> m

    static member Bind f m =
        match m with
        | MacroWarning (w, m) -> MacroWarning (w, MacroResult.Bind f m)
        | MacroDependencies (d, m) -> MacroDependencies (d, MacroResult.Bind f m)
        | MacroOk e -> f e
        | m -> m

/// Represents method bodies, at either JavaScript core or syntax level.
type GeneratorResult =
    | GeneratedQuotation of Microsoft.FSharp.Quotations.Expr
    | GeneratedAST of Expression
    | GeneratedString of string
    | GeneratedJavaScript of JavaScript.Syntax.Expression
    | GeneratorError of string
    | GeneratorWarning of string * GeneratorResult

/// An abstract base class for macro definitions used with MacroAttribute.
[<AbstractClass>]
type Macro() =    
    /// This method is invoked every time a call to a method annotated with this macro type is being translated.
    abstract TranslateCall : MacroCall -> MacroResult
    default this.TranslateCall(call: MacroCall) = failwithf "TranslateCall not implemented for macro %s" (this.GetType().FullName)

    /// This method is invoked every time a call to a constructor annotated with this macro type is being translated.
    abstract TranslateCtor : MacroCtor -> MacroResult
    default this.TranslateCtor(ctor: MacroCtor) = failwithf "TranslateCtor not implemented for macro %s" (this.GetType().FullName)

    abstract Close : Metadata.ICompilation -> unit
    default this.Close _ = ()

/// The return type of Generate method of a generator 
type GeneratedMember =
    | GeneratedMethod of TypeDefinition * Method
    | GeneratedConstructor of TypeDefinition * Constructor
    | GeneratedImplementation of TypeDefinition * TypeDefinition * Method

/// Input for a Generate method on a generator
type Generated =
    {
        Member : GeneratedMember
        Parameter : option<obj>
        Compilation : Metadata.ICompilation
    }

/// An abstract base class for code generation used with GeneratedAttribute.
[<AbstractClass>]
type Generator() =
    abstract Generate : gen:Generated -> GeneratorResult
