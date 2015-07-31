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

namespace WebSharper.Compiler

/// Some algorithms used in WebSharper, expressed in a general form.
module internal Algorithms =
    open System
    open System.Collections
    open System.Collections.Generic

    /// Topological sort.
    [<Sealed>]
    type TopSort =

        /// Given a directed graph defined by a set of root nodes,
        /// a predecessor function and a notion of node identity,
        /// computes a topological sort of the nodes reachable from
        /// this set.
        static member Do<'T> :
            rootNodes: seq<'T>
            * getPredecessors: ('T -> seq<'T>)
            * nodeIdentity: IEqualityComparer<'T> ->
            seq<'T>

        /// A variant of the algorithm using built-in equality.
        static member Do<'T when 'T : equality> :
            rootNodes: seq<'T>
            * getPredecessors: ('T -> seq<'T>) ->
            seq<'T>

