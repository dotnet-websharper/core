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

namespace WebSharper.Tests.Website

open WebSharper
open WebSharper.Sitelets

type WebsiteEntryPoint() =
    abstract Sitelet : Sitelet<Content.FullAction>
    default this.Sitelet = Content.Main true

    abstract Actions : list<Content.FullAction>
    default this.Actions = [Content.Site Actions.Home; Content.Site Actions.Tests]

    interface IWebsite<Content.FullAction> with
        member this.Sitelet = this.Sitelet
        member this.Actions = this.Actions

[<assembly: Website(typeof<WebsiteEntryPoint>)>]
[<assembly: Require(typeof<WebSharper.Tests.TwitterBootstrap>)>]
do ()
