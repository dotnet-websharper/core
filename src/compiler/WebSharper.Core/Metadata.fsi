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

/// Provides types for the assembly and project runtime metadata.
module WebSharper.Core.Metadata

type private D<'T1,'T2> = System.Collections.Generic.Dictionary<'T1,'T2>

module P = WebSharper.Core.JavaScript.Packager
module R = WebSharper.Core.Resources
module Re = WebSharper.Core.Reflection

/// Represents the compilation mode of an assembly.
type AssemblyMode =
    | CompiledAssembly
    | IgnoredAssembly

/// Represents nodes in the resource graph.
type Node =
    | AssemblyNode of Re.AssemblyName * AssemblyMode
    | ConstructorNode of Re.Constructor
    | MethodNode of Re.Method
    | ResourceNode of Re.TypeDefinition
    | TypeNode of Re.TypeDefinition

/// Represents resources.
type Resource =
    | AssemblyResource of Re.AssemblyName
    | UserResource of Re.TypeDefinition

/// A serializable method representation.
[<Sealed>]
type MethodHandle =

    /// Constructs a serialized representation.
    member Pack : unit -> string

    /// Reads the serialized representation.
    static member Unpack : string -> MethodHandle

/// Represents assembly metadata.
[<Sealed>]
type AssemblyInfo =

    /// Enumerates the requirements.
    member Requirements : seq<Node * list<Resource>>

    /// Creates a new AssemblyInfo.
    static member Create : Re.AssemblyName -> AssemblyInfo

    /// Tests whether the assembly contains any remote methods.
    member HasRemoteMethods : bool

    /// Adds a method marked as remote.
    member AddRemoteMethod : Re.Method -> MethodHandle

    /// Adds a record definition.
    member AddRecord : Re.TypeDefinition -> list<string*string> -> unit

    /// Adds a compiled JavaScript type.
    member AddCompiledType : Re.TypeDefinition -> P.Address -> unit

    /// Adds a requirement.
    member AddRequirement : Node -> list<Resource> -> unit

    /// Tries to load assembly metadata from a given path.
    static member Load : string -> option<AssemblyInfo>

    /// Tries to load assembly metadata from a reflected assembly.
    static member LoadReflected : System.Reflection.Assembly ->
        option<AssemblyInfo>

    /// Reads the metadata from a compressed stream.
    static member FromStream : System.IO.Stream -> AssemblyInfo

    /// Writes the metadata to a compressed stream.
    member ToStream : System.IO.Stream -> unit

    /// The name of the embedded resource storing the metadata.
    static member EmbeddedResourceName : string

/// Represents metadata for a set of assemblies.
[<Sealed>]
type Info =

    /// Constructs a new assembly set.
    static member Create : seq<AssemblyInfo> -> Info

    /// Computes the JavaScript name for a type, respecting the proxy graph.
    member GetAddress : Re.TypeDefinition -> option<P.Address>

    /// Computes the JavaScript name for a record field.
    member GetFieldName : Re.TypeDefinition -> string -> string

    /// Tries to load a method by its handle.
    member GetRemoteMethod : MethodHandle -> option<Re.Method>

    /// Resolves resource and assembly requirements for a given set of types.
    member GetDependencies : seq<Node> -> list<R.IResource>


