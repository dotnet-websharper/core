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

namespace IntelliFactory.WebSharper.Web

module A = IntelliFactory.WebSharper.Sitelets.Html.Attributes
module H = IntelliFactory.WebSharper.Sitelets.Html.Html
module T = IntelliFactory.WebSharper.Sitelets.Html.Tags

/// A base class for defining custom ASP.NET controls. Inherit from this class,
/// override the Body property and use the new class as a Server ASP.NET
/// control in your application.
[<AbstractClass>]
type Control() =
    inherit System.Web.UI.Control()

    static let gen = System.Random()
    let mutable isR = true
    let mutable id = System.String.Format("ws{0:x}", gen.Next().ToString())

    override this.ID
        with get () = id
        and set x = id <- x; isR <- false

    override this.OnLoad _ =
        this.ID <-
            ScriptManager.Find(base.Page).Register
                (if isR then None else Some id) this

    interface IntelliFactory.WebSharper.Sitelets.Html.Html.INode with
        member this.Node =
            let el = T.Div [A.Id this.ID]
            let el = el |> H.Annotate this
            H.ContentNode el

    abstract member Body : IntelliFactory.WebSharper.Pagelets.Html.IPagelet
    default this.Body = Unchecked.defaultof<_>

    interface IntelliFactory.WebSharper.Pagelets.Html.Activator.IControl with
        member this.Body = this.Body
        member this.Id = this.ID

    override this.Render writer =
        writer.WriteLine("<div id='{0}'></div>", this.ID)
