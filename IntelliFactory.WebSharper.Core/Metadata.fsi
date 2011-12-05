// $begin{copyright}
// 
// This file is part of WebSharper
// 
// Copyright (c) 2008-2011 IntelliFactory
// 
// GNU Affero General Public License Usage
// WebSharper is free software: you can redistribute it and/or modify it under
// the terms of the GNU Affero General Public License, version 3, as published
// by the Free Software Foundation.
//
// WebSharper is distributed in the hope that it will be useful, but WITHOUT
// ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
// FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License
// for more details at <http://www.gnu.org/licenses/>.
//
// If you are unsure which license is appropriate for your use, please contact
// IntelliFactory at http://intellifactory.com/contact.
//
// $end{copyright}

/// Provides types for the assembly and project runtime metadata.
module IntelliFactory.WebSharper.Core.Metadata

type private D<'T1,'T2> = System.Collections.Generic.Dictionary<'T1,'T2>

module P = IntelliFactory.JavaScript.Packager
module R = IntelliFactory.WebSharper.Core.Resources
module Re = IntelliFactory.WebSharper.Core.Reflection

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


