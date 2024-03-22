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

namespace WebSharper.Web

open WebSharper.Core.Resources
open WebSharper
module M = WebSharper.Core.Metadata

type INode =
    inherit IRequiresResources

    abstract member Write : Web.Context * HtmlTextWriter -> unit

    abstract member IsAttribute : bool

type BundleNode(bundle: string, ?node: INode) =
    
    interface INode with

        member this.Requires (i, p, s) = 
            match node with 
            | Some n ->
                Seq.append (Seq.singleton (ClientCode.ClientBundle bundle)) (n.Requires(i, p, s))
            | _ ->
                Seq.singleton (ClientCode.ClientBundle bundle)

        member this.Write (c, w) =
            match node with
            | Some n ->
                n.Write(c, w)
            | _ ->
                ()

        member this.IsAttribute = 
            match node with
            | Some n ->
                n.IsAttribute
            | _ ->
                false
