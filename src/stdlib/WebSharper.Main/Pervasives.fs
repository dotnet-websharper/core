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

/// Defines operators and functions that are automatically available whenever
/// `WebSharper` is open.
[<AutoOpen>]
module WebSharper.Pervasives

module A = WebSharper.Core.Attributes

/// Marks union cases or properties that should be compiled to constants.
type ConstantAttribute = A.ConstantAttribute

/// Marks methods and constructors for direct compilation to a JavaScript function.
/// Direct members work by expanding JavaScript code templates
/// with placeholders of the form such as $0, $x, $this or $value
/// into the body of a JavaScript function. See also InlineAttribute.
type DirectAttribute = A.DirectAttribute

/// Marks methods and constructors for inline compilation to JavaScript.
/// Inline members work by expanding JavaScript code templates
/// with placeholders of the form such as $0, $x, $this or $value
/// directly at the place of invocation. See also DirectAttribute.
type InlineAttribute = A.InlineAttribute

/// Marks methods, properties and constructors for compilation to JavaScript.
type JavaScriptAttribute = A.JavaScriptAttribute

/// Annotates methods with custom compilation rules. The supplied type
/// should implement Macros.IMacroDefinition and a default constructor.
type MacroAttribute = A.MacroAttribute

/// Provides a runtime name for members when it differs from the F# name.
/// The constructor accepts either an explicit array of parts,
/// or a single string, in which case it is assumed to be dot-separated.
type NameAttribute = A.NameAttribute

/// Declares a type to be a proxy for another type, identified directly or
/// by using an assembly-qualified name.
type ProxyAttribute = A.ProxyAttribute

/// Marks a server-side function to be invokable remotely from the client-side.
type RemoteAttribute = A.RemoteAttribute

/// Indicates the client-side remoting provider that should be used
/// by remote function calls in this assembly. The type passed to the
/// constructor must have three static methods as described by the
/// interface Remoting.IRemotingProvider.
type RemotingProviderAttribute = A.RemotingProviderAttribute

/// Annotates members with dependencies. The type passed to the constructor
/// must implement Resources.IResourceDefinition and a default constructor.
type RequireAttribute = A.RequireAttribute

/// Marks a server-side function to be invokable remotely from the client-side.
type RpcAttribute = A.RemoteAttribute

/// Marks members that should be compiled by-name.
type StubAttribute = A.StubAttribute

/// Adds automatic inlines to a property so that a missing JavaScript field
/// is converted to None, otherwise Some fieldValue.
type OptionalFieldAttribute = A.OptionalFieldAttribute

/// Declares that when de/serializing this union type for external use, its cases
/// must be tagged by their CompiledName rather than an integer.
/// `discriminatorName` is the name of the field in which this tag is stored.
type NamedUnionCasesAttribute = A.NamedUnionCasesAttribute

/// Re-exports Remoting.IRpcHandlerFactory.
type IRpcHandlerFactory =
    WebSharper.Core.Remoting.IHandlerFactory

/// Re-exports Remoting.SetHandlerFactory.
let SetRpcHandlerFactory (factory: IRpcHandlerFactory) =
    WebSharper.Core.Remoting.SetHandlerFactory factory
