// $begin{copyright}
//
// This file is part of WebSharper
//
// Copyright (c) 2008-2016 IntelliFactory
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

namespace WebSharper.Core

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
