// $begin{copyright}
//
// This file is part of WebSharper
//
// Copyright (c) 2008-2016 IntelliFactory
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

module SPA =
    type EndPoint

namespace WebSharper

open System
open System.Runtime.CompilerServices
open System.Threading.Tasks
open WebSharper.Sitelets

[<Class>]
type Application =
    /// Create a multi-page application.
    static member MultiPage : (Context<'EndPoint> -> 'EndPoint -> Async<Content<'EndPoint>>) -> Sitelet<'EndPoint>

    /// Create a single-page HTML application.
    static member SinglePage : (Context<SPA.EndPoint> -> Async<Content<SPA.EndPoint>>) -> Sitelet<SPA.EndPoint>

    /// Create a single-page application that returns text.
    // static member Text : (Context<SPA.EndPoint> -> string) -> Sitelet<SPA.EndPoint>

    /// Create a multi-page application.
    static member MultiPage : Func<Context<'EndPoint>, 'EndPoint, Task<Content<'EndPoint>>> -> Sitelet<'EndPoint>

    /// Create a single-page HTML application.
    static member SinglePage : Func<Context<SPA.EndPoint>, Task<Content<SPA.EndPoint>>> -> Sitelet<SPA.EndPoint>

    /// Create a single-page application that returns text.
    static member Text : Func<Context<SPA.EndPoint>, string> -> Sitelet<SPA.EndPoint>