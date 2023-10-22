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

namespace WebSharper

open WebSharper
open WebSharper.JavaScript
module M = WebSharper.Core.Metadata
module J = WebSharper.Core.Json

type ClientCode =
    | ClientRequire of M.Node
    | ClientJsonData of J.Value
    | ClientImport of WebSharper.Core.AST.Address 
    | ClientApply of ClientCode * seq<ClientCode>
    | ClientReplaceInDom of string * ClientCode
    | ClientReplaceInDomWithBody of string * ClientCode
    | ClientAddEventListener of string * string * ClientCode
    | ClientDOMElement of string
    | ClientInitialize of string * ClientCode

/// Use to get a new unique id for an element on a page.
type IUniqueIdSource =
    abstract member NewId : unit -> string

/// An interface that has to be implemented by controls
/// that depend on resources.
type IRequiresResources =
    abstract member Requires : M.Info * J.Provider * IUniqueIdSource -> seq<ClientCode>

/// HTML content that can be used as the Body of a web Control.
/// Can be zero, one or many DOM nodes.
type IControlBody =
    /// Replace the given node with the HTML content.
    /// The node is guaranteed to be present in the DOM.
    /// Called exactly once on startup on an IControl's Body.
    [<JavaScript; Name "ReplaceInDom">]
    abstract ReplaceInDom : Dom.Node -> unit

/// An interface that has to be implemented by controls that
/// are subject to activation, ie. server-side controls that
/// contain client-side elements.
type IControl =
    inherit IRequiresResources
    [<JavaScript; Name "Body">]
    abstract member Body : IControlBody

/// An interface that has to be implemented by controls that
/// are subject to activation but are not attached to a
/// specific DOM element.
type IInitializer =
    inherit IRequiresResources

    /// Called during the preparation phase of initialization.
    /// This is guaranteed to run before any IInitializer's Initialize
    /// and before any IControl's Body.
    /// The order between the PreInitialize of two IInitializers is unspecified.
    [<JavaScript; Name "$preinit">]
    abstract member PreInitialize : id: string -> unit

    /// Called during the main phase of initialization.
    /// The order between the Initialize of two IInitializers is unspecified.
    [<JavaScript; Name "$init">]
    abstract member Initialize : id: string -> unit

    /// Called during the final phase of initialization.
    /// This is guaranteed to run after any IInitializer's Initialize
    /// and after any IControl's Body.
    /// The order between the PostInitialize of two IInitializers is unspecified.
    [<JavaScript; Name "$postinit">]
    abstract member PostInitialize : id: string -> unit

[<AutoOpen>]
module HtmlContentExtensions =

    [<JavaScript>]
    type private SingleNode(node: Dom.Node) =
        interface IControlBody with
            member this.ReplaceInDom(old) =
                node.ParentNode.ReplaceChild(node, old) |> ignore

    type IControlBody with
        /// Create HTML content comprised of a single DOM node.
        [<JavaScript>]
        static member SingleNode (node: Dom.Node) =
            new SingleNode(node) :> IControlBody
