// $begin{copyright}
//
// This file is part of WebSharper
//
// Copyright (c) 2008-2012 IntelliFactory
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

/// Exposes the compiler front-end for programmatic use.
module IntelliFactory.WebSharper.Compiler.FrontEnd

/// Represents key pairs.
type Key = System.Reflection.StrongNameKeyPair

/// Represents file-system paths.
type Path = string

/// Represents raw symbols data as found in .mdb and .pdb files.
type Symbols =
    | Mdb of byte []
    | Pdb of byte []

/// Represents assemblies.
[<Sealed>]
type Assembly =

    /// Returns the raw assembly data.
    member RawBytes : option<Key> -> byte []

    /// Returns the associated symbols, if any.
    member Symbols : option<Symbols>

    /// Writes the assembly to the given path.
    member Write : option<Key> -> Path -> unit

    /// Reads the embedded JavaScript.
    member ReadableJavaScript : option<string>

    /// Reads the embedded JavaScript.
    member CompressedJavaScript : option<string>

/// Loads assemblies.
[<Sealed>]
type Loader =

    /// Creates a new loader. Accepts a set of search paths.
    static member Create : Set<Path> -> (string -> unit) -> Loader

    /// Loads an assembly from raw data.
    member LoadRaw : byte [] -> option<Symbols> -> Assembly

    /// Loads an assembly from a given path.
    member LoadFile : Path -> Assembly

/// Represents compilation options.
type Options =
    {
        ErrorLimit : int
        KeyPair : option<System.Reflection.StrongNameKeyPair>
        References : list<Assembly>
    }

    /// The defaults.
    static member Default : Options

/// Compiles an assembly.
val Compile : Options -> (Message -> unit) -> (Assembly -> bool)
