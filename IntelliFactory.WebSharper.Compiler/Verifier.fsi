// $begin{copyright}
//
// This file is part of WebSharper
//
// Copyright (c) 2008-2013 IntelliFactory
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

/// Verifies correct usage conventions.
module internal IntelliFactory.WebSharper.Compiler.Verifier

/// Describes a verification result.
type Status =
    | Correct
    | Incorrect of string

/// Provides verification methods.
[<Sealed>]
type State =

    /// Checks a Remote method declaration.
    member VerifyRemoteMethod : MethodDefinition -> Status

    /// Checks a Web.Control declaration.
    member VerifyWebControl : TypeDefinition -> Status

/// Creates a new object for verification.
val Create : unit -> State
