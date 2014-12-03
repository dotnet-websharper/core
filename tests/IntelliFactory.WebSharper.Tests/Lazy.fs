// $begin{copyright}
//
// This file is part of WebSharper
//
// Copyright (c) 2008-2014 IntelliFactory
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

module IntelliFactory.WebSharper.Tests.Lazy

open IntelliFactory.WebSharper
open IntelliFactory.WebSharper.Testing

[<JavaScript>]
let Tests =
    Section "Lazy"

    Test "Basics" {
        let k = lazy 1
        k.IsValueCreated =? false
        k.Value =? 1
        k.IsValueCreated =? true
        let b = Lazy.CreateFromValue 1
        b.IsValueCreated =? true
        b.Value =? 1
        let r = ref 0
        let c = Lazy.Create (fun () -> incr r; 1)
        c.IsValueCreated =? false
        !r =? 0
        c.Value =? 1
        !r =? 1
        c.IsValueCreated =? true
    }
