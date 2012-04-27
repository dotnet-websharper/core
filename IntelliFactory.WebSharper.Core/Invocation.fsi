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

/// Implements dynamic method invocation that is faster than
/// methodInfo.Invoke for methods with a small number of arguments (0-3).
/// Falls back on methodInfo.Invoke for larger numbers of arguments.
/// The implementation does not use Reflection.Emit.
module IntelliFactory.WebSharper.Core.Invocation

/// Compiles a method to a fast invoke function.
val Compile : System.Reflection.MethodInfo -> (obj -> obj [] -> option<obj>)
