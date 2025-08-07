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

open System

/// Marks an embedded file name to be unpacked for web projects.
/// Mime type application/javascript or text/javascript will be unpacked to Scripts folder,
/// otherwise into Contents folder.
[<Sealed; AttributeUsage(AttributeTargets.Assembly, AllowMultiple = true)>]
[<Obsolete("Use an extra.files file instead to define your copied/embedded files.")>]
type WebResourceAttribute(filename: string, mime: string) =
    inherit Attribute()

[<assembly: WebResource("Runtime.js", "text/javascript")>]
[<assembly: WebResource("Runtime.ts", "text/javascript")>]
do ()
