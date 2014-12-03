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

/// Declares resource (JS/CSS) dependencies.
module Website.Dependencies

open IntelliFactory.WebSharper

/// Declare how to load Twitter Bootstrap resources from the CDN.
/// Declare that Twitter Bootstrap depends on jQuery (so that jQuery is included first).
[<Require(typeof<JQuery.Resources.JQuery>)>]
[<Sealed>]
type TwitterBootstrap() =
    inherit Resources.BaseResource("//netdna.bootstrapcdn.com/twitter-bootstrap/2.3.1/",
        "js/bootstrap.min.js", "css/bootstrap-combined.min.css")

[<Require(typeof<JQuery.Resources.JQuery>)>]
[<Sealed>]
type QUnit() =
    inherit Resources.BaseResource("//code.jquery.com/qunit/",
        "qunit-1.12.0.js", "qunit-1.12.0.css")

[<assembly: Require(typeof<TwitterBootstrap>)>]
[<assembly: Require(typeof<QUnit>)>]
do ()
