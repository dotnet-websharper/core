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

/// Implements generic URL encoding and decoding for algebraic
/// datatypes. To be on the safe side, the encoded values use only
/// unreserved URL characters (including alphanumeric, tilde, dash,
/// underscore and dot characters). Tilde serves as a special character
/// starting escape sequences. The encoding also uses the forward slash
/// to separate logical components.
module IntelliFactory.WebSharper.Sitelets.UrlEncoding

/// Thrown when a formatter cannot be derived for a certain type.
exception NoFormatError of System.Type

/// Represents an URL encoding for a given type.
[<Sealed>]
type Format<'T> =

    /// Parses a string. Fails if the string cannot be parsed.
    member Read : string -> option<'T>

    /// Formats a value. Fails if it cannot be represented.
    member Show : 'T -> option<string>

/// Represents cached formatter collection.
[<Sealed>]
type Factory =

    /// Derives an encoding for the given type.
    member GetFormatFor : System.Type -> Format<obj>

    /// Derives an encoding for the given type passed as a type parameter.
    member GetFormat<'T> : unit -> Format<'T>

    /// Creates a new factory.
    static member Create : unit -> Factory

/// Derives a format using a new temporary factory.
val GetFormat<'T> : unit -> Format<'T>

/// Derives a format for a given type using a new temporary factory.
val GetFormatFor : System.Type -> Format<obj>

