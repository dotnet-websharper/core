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

module Algorithms =
    open System
    open System.Collections
    open System.Collections.Generic

    [<Sealed>]
    type TopSort =

        static member Do(roots: seq<'T>, pred: 'T -> seq<'T>, nodeIdentity: IEqualityComparer<'T>) =
            seq {
                let visited = HashSet(nodeIdentity)
                let rec visit node =
                    seq {
                        if visited.Add(node) then
                            for pN in pred node do
                                yield! visit pN
                            yield node
                    }
                for node in roots do
                    yield! visit node
            }

        static member Do(roots: seq<'T>, pred: 'T -> seq<'T>) =
            seq {
                let visited = HashSet()
                let rec visit node =
                    seq {
                        if visited.Add(node) then
                            for pN in pred node do
                                yield! visit pN
                            yield node
                    }
                for node in roots do
                    yield! visit node
            }
