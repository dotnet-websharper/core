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

/// Implements offline sitelet HTML generation.
module internal WebSharper.Sitelets.Offline.Output

open WebSharper.Compiler
open WebSharper.Sitelets
module H = WebSharper.Compiler.HtmlCommand

/// The output mode, Debug or Release.
type Mode = H.Mode

/// Configuration options.
type Config =
    {
        Actions : list<obj>
        Options : H.Config
        Sitelet : Sitelet<obj>
        UnpackSourceMap : bool
        UnpackTypeScript : bool
        Metadata: WebSharper.Core.Metadata.Info
    }

/// Writes a site given the configuration options.
val WriteSite : AssemblyResolver -> config: Config -> Async<unit>
