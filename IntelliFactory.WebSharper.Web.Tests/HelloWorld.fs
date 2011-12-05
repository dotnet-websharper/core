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

namespace IntelliFactory.WebSharper.Web.Tests

module A = IntelliFactory.WebSharper.Core.Attributes
module H = IntelliFactory.WebSharper.Html.Default
module J = IntelliFactory.WebSharper.JavaScript

type HelloWorld() =
    inherit IntelliFactory.WebSharper.Web.Control()

    [<A.JavaScript>]
    override this.Body =
        let o = obj ()
        J.Set o "a" 1
        J.Set o "b" 2
        let k = J.Get<int> "b" o
        let t = string k
        H.Div [H.Text t] :> _
