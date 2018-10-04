// $begin{copyright}
//
// This file is part of WebSharper
//
// Copyright (c) 2008-2018 IntelliFactory
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

module WebSharper.Tests.Lazy

open WebSharper
open WebSharper.Testing

[<JavaScript>]
let Tests =
    TestCategory "Lazy" {

        Test "Basics" {
            let k = lazy 1
            isFalse k.IsValueCreated
            equal k.Value 1
            isTrue k.IsValueCreated
            let b = Lazy.CreateFromValue 1
            isTrue b.IsValueCreated
            equal b.Value 1
            let r = ref 0
            let c = Lazy.Create (fun () -> incr r; 1)
            isFalse c.IsValueCreated
            equal !r 0
            equal c.Value 1
            equal !r 1
            isTrue c.IsValueCreated
        }

        Test "Constructor" {
            let l2 = System.Lazy<int>(fun () -> 1 + 1)
            isFalse l2.IsValueCreated
            equal l2.Value 2
        }
    }
