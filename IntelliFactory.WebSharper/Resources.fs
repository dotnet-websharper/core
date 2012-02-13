
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

/// Re-exports functionality from IntelliFactory.WebSharper.Core.Resources.
module IntelliFactory.WebSharper.Resources

module R = IntelliFactory.WebSharper.Core.Resources

/// Re-exports BaseResource.
type BaseResource = R.BaseResource

/// Re-exports Context.
type Context = R.Context

/// Re-exports Runtime.
type Runtime = R.Runtime

/// Re-exports IResource.
type IResource = R.IResource

[<assembly: System.Web.UI.WebResource("Json.js", "text/javascript")>]
[<assembly: System.Web.UI.WebResource("Json.min.js", "text/javascript")>]
do ()
