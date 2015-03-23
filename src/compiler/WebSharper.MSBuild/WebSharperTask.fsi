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

namespace WebSharper.MSBuild

open Microsoft.Build.Framework
open Microsoft.Build.Utilities

/// Implements MSBuild logic used in `WebSharper.targets`.
[<Sealed>]
type WebSharperTask =
    inherit AppDomainIsolatedTask

    new : unit -> WebSharperTask

    /// Used to specify which "method" to call.
    member Command : string with get, set

    /// MSBuild $(Configuration) property.
    member Configuration : string with get, set

    /// Path to the XML documentation file that the Interface Generator should generate.
    member DocumentationFile : string with get, set

    /// Propagating EmbeddedResource group from MSBuild.
    member EmbeddedResources : ITaskItem [] with get, set

    /// Item input for item commands.
    member ItemInput : ITaskItem [] with get, set

    /// Item output for item commands.
    member ItemOutput : ITaskItem [] with get, set

    /// Path to an `.snk` strong name key file, if any.
    member KeyOriginatorFile : string with get, set

    /// MSBuild $(MSBuildProjectDirectory) property.
    member MSBuildProjectDirectory : string with get, set

    /// The project $(Name) property, typically set in F# projects.
    member Name : string with get, set

    /// Copy-local file list.
    member ReferenceCopyLocalPaths : ITaskItem [] with get, set

    /// Path to the directory containing Web.config, used by the "Unpack" command,
    /// and also to detect Web projects.
    member WebProjectOutputDir : string with get, set

    /// Output directory for the Bundle task.
    member WebSharperBundleOutputDir : string with get, set

    /// Used as root for HTML project output.
    member WebSharperHtmlDirectory : string with get, set

    /// Specifies which project type is being built.
    member WebSharperProject : string with get, set

    /// Specifies if source map is generated and source files are included
    /// in the assembly as resources.
    member WebSharperSourceMap : string with get, set

