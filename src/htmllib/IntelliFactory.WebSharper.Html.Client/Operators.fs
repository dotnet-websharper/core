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

namespace IntelliFactory.WebSharper.Html.Client

open IntelliFactory.WebSharper
open IntelliFactory.WebSharper.JavaScript
open IntelliFactory.WebSharper.Html.Client.Interfaces

[<AutoOpen>]
module Operators =

    [<JavaScript>]
    [<Name "add">]
    let ( -< ) (el: Element) (inner: seq<#Pagelet>) =
        for pl in inner do
            el.Append (pl :> Pagelet)
        el

    [<Inline>]
    [<JavaScript>]
    let ( -- ) (el: Element) (inner: #Pagelet) =
        el -< [inner]

    [<Inline "$f.apply($o)">]
    let private Apply f o = ()

    /// Destructively adds an function to be invoked after the render is called on
    /// the pagelet.
    [<JavaScript>]
    let OnAfterRender<'T when 'T :> Pagelet> (f: 'T -> unit) (w: 'T) : unit =
        let r = w?Render
        w?Render <- fun () ->
            Apply r w
            f w

    /// Destructively adds an function to be invoked before the render is called on
    /// the pagelet.
    [<JavaScript>]
    let OnBeforeRender<'T when 'T :> Pagelet> (f: 'T -> unit) (w: 'T) : unit =
        let r = w?Render
        w?Render <- fun () ->
            f w
            Apply r w

    // Generates a new unique id.
    [<JavaScript>]
    [<Inline>]
    let NewId () = Utils.NewId ()

    /// Looks up dom element by id
    [<Inline "document.getElementById($name)">]
    let ById (name: string) : Dom.Element = Unchecked.defaultof<_>()



