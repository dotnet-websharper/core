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

namespace WebSharper.Web

open WebSharper

module A = WebSharper.Html.Server.Attr
module H = WebSharper.Html.Server.Html
module T = WebSharper.Html.Server.Tags
module M = WebSharper.Core.Metadata
module R = WebSharper.Core.Reflection
module P = WebSharper.Core.JavaScript.Packager

/// A base class for defining custom ASP.NET controls. Inherit from this class,
/// override the Body property and use the new class as a Server ASP.NET
/// control in your application.
[<AbstractClass>]
type Control() =
    inherit System.Web.UI.Control()

    static let gen = System.Random()
    [<System.NonSerialized>]
    let mutable isR = true
    [<System.NonSerialized>]
    let mutable id = System.String.Format("ws{0:x}", gen.Next().ToString())

    override this.ID
        with get () = id
        and set x = id <- x; isR <- false

    override this.OnLoad _ =
        this.ID <-
            ScriptManager.Find(base.Page).Register
                (if isR then None else Some id) this

    interface Html.Server.Html.INode with
        member this.Node =
            let el = T.Div [A.Id this.ID]
            let el = el |> H.Annotate this
            H.ContentNode el

    abstract member Body : Html.Client.IControlBody
    default this.Body = Unchecked.defaultof<_>

    interface Html.Client.IControl with
        member this.Body = this.Body
        member this.Id = this.ID
        member this.Requires meta =
            let t = this.GetType()
            let t = if t.IsGenericType then t.GetGenericTypeDefinition() else t
            [M.TypeNode (R.TypeDefinition.FromType t)] :> seq<_>

    override this.Render writer =
        writer.WriteLine("<div id='{0}'></div>", this.ID)

open Microsoft.FSharp.Quotations
open Microsoft.FSharp.Quotations.Patterns

/// Implements a web control based on a quotation-wrapped top-level body.
/// Use the function ClientSide to create an InlineControl.
type InlineControl<'T when 'T :> Html.Client.IControlBody>(elt: Expr<'T>) =
    inherit Control()

    let mutable body = ""

    [<System.NonSerialized>]
    let elt = elt

    [<System.NonSerialized>]
    let props =
        match elt :> Expr with
        | PropertyGet(None, p, []) ->
            let rp = R.Property.Parse p
            rp.DeclaringType, rp.Name, Seq.singleton (M.TypeNode rp.DeclaringType)
        | Call(None, m, []) ->
            let rm = R.Method.Parse m
            rm.DeclaringType, rm.Name, Seq.singleton (M.MethodNode rm)
        | e -> failwithf "Wrong format for InlineControl: expected global variable access, got: %A" e

    [<JavaScript>]
    override this.Body = JavaScript.JS.Eval(body) :?> _

    interface Html.Client.IControl with
        [<JavaScript>]
        member this.Body = this.Body
        member this.Id = this.ID
        member this.Requires meta =
            let declType, name, deps = props
            body <-
                match meta.GetAddress declType with
                | None -> failwith "Couldn't find address for method"
                | Some a ->
                    let rec mk acc (a: P.Address) =
                        let n = a.LocalName.Replace(@"\", @"\\").Replace("'", @"\'")
                        match a.Parent with
                        | None -> n + "['" + acc
                        | Some p -> mk (n + "']['" + acc) p
                    mk (name + "']()") a
            deps

namespace WebSharper

[<AutoOpen>]
module WebExtensions =

    open Microsoft.FSharp.Quotations
    open WebSharper.Html.Client

    /// Embed the given client-side control body in a server-side control.
    /// The client-side control body must be either a module-bound or static value,
    /// or a call to a module-bound function or static method with argument ().
    let ClientSide (e: Expr<#IControlBody>) =
        new WebSharper.Web.InlineControl<_>(e)