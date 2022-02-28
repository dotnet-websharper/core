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

namespace WebSharper.Constants

[<AutoOpen>]
module EmbeddedResources =

    [<Literal>]
    let EMBEDDED_METADATA = "WebSharper.meta"

    [<Literal>]
    let EMBEDDED_RUNTIME_METADATA = "WebSharper.runtime.meta"

    [<Literal>]
    let EMBEDDED_JS = "WebSharper.js"

    [<Literal>]
    let EMBEDDED_MAP = "WebSharper.map"

    [<Literal>]
    let EMBEDDED_MINJS = "WebSharper.min.js"

    [<Literal>]
    let EMBEDDED_MINMAP = "WebSharper.min.map"

    [<Literal>]
    let EMBEDDED_DTS = "WebSharper.d.ts"

[<AutoOpen>]
module RuntimeSettings =
    
    [<Literal>]
    let RUNTIMESETTING_SHARED_METADATA = "WebSharperSharedMetadata"

    [<Literal>]
    let RUNTIMESETTING_OLD_CDNFORMAT_PREFIX = "WebSharper.CdnFormat."

    [<Literal>]
    let RUNTIMESETTING_CDNFORMAT_PREFIX = "CdnFormat."

    [<Literal>]
    let RUNTIMESETTING_OLD_STDLIB_USECDN = "WebSharper.StdlibUseCdn"

    [<Literal>]
    let RUNTIMESETTING_STDLIB_USECDN = "UseStdlibCdn"

    [<Literal>]
    let RUNTIMESETTING_OLD_STDLIB_CDNFORMAT = "WebSharper.StdlibCdnFormat"

    [<Literal>]
    let RUNTIMESETTING_STDLIB_CDNFORMAT = "StdlibCdnFormat"

    [<Literal>]
    let RUNTIMESETTING_USEDOWNLOADEDRESOURCES = "UseDownloadedResources"

    [<Literal>]
    let RUNTIMESETTING_USEMINIFIEDSCRIPTS = "UseMinifiedScripts"
