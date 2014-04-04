// $begin{copyright}
//
// This file is part of WebSharper
//
// Copyright (c) 2008-2014 IntelliFactory
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

namespace IntelliFactory.WebSharper.Core

open System
open System.Reflection

/// Splits invocation of methods represented as `System.Reflection.MethodInfo`
/// into the static and dynamic phase, doing the binding work at the static phase.
/// A pre-bound method can be 100-1000x faster to invoke compared to using `.Invoke`
/// every time, which has to repeatedly do method binding.
module internal FastInvoke =

    /// Represents a method that can be quickly invoked dynamically.
    [<AbstractClass>]
    type FastMethod =

        /// Invokes with 0 arguments.
        abstract Invoke0 : unit -> obj

        /// Invokes with 1 argument.
        abstract Invoke1 : obj -> obj

        /// Invokes with 2 arguments.
        abstract Invoke2 : obj * obj -> obj

        /// Invokes with 3 arguments.
        abstract Invoke3 : obj * obj * obj -> obj

        /// Invokes with 4 arguments.
        abstract Invoke4 : obj * obj * obj * obj -> obj

        /// Invokes with 5 arguments.
        abstract Invoke5 : obj * obj * obj * obj * obj -> obj

        /// Invokes with 6 arguments.
        abstract Invoke6 : obj * obj * obj * obj * obj * obj -> obj

        /// Invokes with 7 arguments.
        abstract Invoke7 : obj * obj * obj * obj * obj * obj * obj -> obj

        /// Invokes with an arbitrary number of arguments.
        abstract InvokeN : [<ParamArray>] args: obj [] -> obj

    /// Compiles a method to a fast invoke function.
    val Compile : MethodInfo -> FastMethod
