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
