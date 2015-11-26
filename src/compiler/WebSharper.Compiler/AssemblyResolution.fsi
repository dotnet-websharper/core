// Copyright 2013-2015 IntelliFactory
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

namespace WebSharper.Compiler

open System
open System.Reflection

/// An utility for resolving assemblies from non-standard contexts.
[<Sealed>]
type AssemblyResolver =

    /// Installs the resolver into the `AppDomain`.
    member Install : unit -> unit

    /// Uninstalls the resolver from the `AppDomain`.
    member Remove : unit -> unit

    /// Wraps an action in `Install/Remove` on the `AppDomain`.
    member Wrap : (unit -> 'T) -> 'T

    /// Resolves an assembly.
    member Resolve : AssemblyName  -> option<Assembly>

    /// Resolves a full path to the assembly.
    member ResolvePath : AssemblyName -> option<string>

    /// Adds search directories.
    member SearchDirectories : searchPaths: seq<string> -> AssemblyResolver

    /// Adds search paths.
    member SearchPaths : searchPaths: seq<string> -> AssemblyResolver

    /// Sets the base directory. Assemblies from this directory
    /// are loaded by name rather than via a byte array.
    member WithBaseDirectory : string -> AssemblyResolver

    /// Creates a new resovler for a given domain or the current domain.
    static member Create : ?domain: AppDomain -> AssemblyResolver
