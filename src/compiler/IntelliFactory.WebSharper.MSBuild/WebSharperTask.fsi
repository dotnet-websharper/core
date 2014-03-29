// $begin{copyright}
//
// This file is part of WebSharper
//
// Copyright (c) 2008-2014 IntelliFactory
//
// GNU Affero General Public License Usage
// WebSharper is free software: you can redistribute it and/or modify it under
// the terms of the GNU Affero General Public License, version 3, as published
// by the Free Software Foundation.
//
// WebSharper is distributed in the hope that it will be useful, but WITHOUT
// ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
// FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License
// for more details at <http://www.gnu.org/licenses/>.
//
// If you are unsure which license is appropriate for your use, please contact
// IntelliFactory at http://intellifactory.com/contact.
//
// $end{copyright}

namespace IntelliFactory.WebSharper.MSBuild

open Microsoft.Build.Framework
open Microsoft.Build.Utilities

/// Implements MSBuild logic used in WebSharper.targets
[<Sealed>]
type WebSharperTask =
    inherit Task

    new : unit -> WebSharperTask

    /// Used to specify which "method" to call.
    member Command : string with get, set

    /// Item input for item commands.
    member ItemInput : ITaskItem [] with get, set

    /// Item output for item commands.
    member ItemOutput : ITaskItem [] with get, set

    /// Path to an `.snk` strong name key file, if any.
    member KeyOriginatorFile : string with get, set

    /// The project name, typically set in F# projects.
    member Name : string with get, set

    /// Path to the directory containing Web.config, used by the "Unpack" command,
    /// and also to detect Web projects.
    member WebProjectOutputDir : string with get, set

    /// Output directory for the Bundle task.
    member WebSharperBundleOutputDir : string with get, set

    /// Flag used to skip auto-referencing assemblies.
    member WebSharperExplicitRefs : string with get, set

    /// Specifies which project type is being built.
    member WebSharperProject : string with get, set

