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
/// `IntelliFactory.WebSharper` is open.
[<AutoOpen>]
module IntelliFactory.WebSharper.Pervasives

module A = IntelliFactory.WebSharper.Core.Attributes

type ConstantAttribute = A.ConstantAttribute
type DirectAttribute = A.DirectAttribute
type InlineAttribute = A.InlineAttribute
type JavaScriptAttribute = A.JavaScriptAttribute
type MacroAttribute = A.MacroAttribute
type NameAttribute = A.NameAttribute
type ProxyAttribute = A.ProxyAttribute
type RemoteAttribute = A.RemoteAttribute
type RequireAttribute = A.RequireAttribute
type RpcAttribute = A.RemoteAttribute
type StubAttribute = A.StubAttribute

/// Re-exports Remoting.IRpcHandlerFactory.
type IRpcHandlerFactory =
    IntelliFactory.WebSharper.Core.Remoting.IHandlerFactory

/// Re-exports Remoting.SetHandlerFactory.
let SetRpcHandlerFactory (factory: IRpcHandlerFactory) =
    IntelliFactory.WebSharper.Core.Remoting.SetHandlerFactory factory
