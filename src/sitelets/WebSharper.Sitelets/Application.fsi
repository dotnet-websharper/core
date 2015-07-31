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

module WebSharper.Application

open WebSharper.Sitelets

/// Create a multi-page application.
val MultiPage : (Context<'EndPoint> -> 'EndPoint -> Async<Content<'EndPoint>>) -> Sitelet<'EndPoint>

module SPA =
    type EndPoint

/// Create a single-page HTML application.
val SinglePage : (Context<SPA.EndPoint> -> Async<Content<SPA.EndPoint>>) -> Sitelet<SPA.EndPoint>

/// Create a single-page application that returns text.
val Text : (Context<SPA.EndPoint> -> string) -> Sitelet<SPA.EndPoint>
