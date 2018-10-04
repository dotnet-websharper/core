
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

/// Re-exports functionality from WebSharper.Core.Resources.
module WebSharper.Resources

open WebSharper
module R = WebSharper.Core.Resources

/// Re-exports BaseResource.
type BaseResource = R.BaseResource

/// Re-exports Context.
type Context = R.Context

/// Re-exports Runtime.
type Runtime = R.Runtime

/// Re-exports IResource.
type IResource = R.IResource

[<assembly: WebResource("Json.js", "text/javascript")>]
[<assembly: WebResource("Json.min.js", "text/javascript")>]
[<assembly: WebResource("AnimFrame.js", "text/javascript")>]
[<assembly: WebResource("AnimFrame.min.js", "text/javascript")>]
do ()
