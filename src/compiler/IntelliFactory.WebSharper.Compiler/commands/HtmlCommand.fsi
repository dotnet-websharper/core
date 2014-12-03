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

namespace IntelliFactory.WebSharper.Compiler

/// Performs generation of static HTML from a set of WebSharper assemblies
/// that contain offline sitelet declarations.
module HtmlCommand =

    /// In debug mode, generated scripts are not compressed.
    type Mode =
        | Debug
        | Release

    /// Options for the command.
    type Config =
        {
            /// Path to the main assembly defining the sitelet.
            MainAssemblyPath : string

            /// Debug or Release mode.
            Mode : Mode

            /// Output directory to place files under.
            OutputDirectory : string

            /// Project directory where root files are located.
            ProjectDirectory : string

            /// Paths to reference assemblies.
            ReferenceAssemblyPaths : list<string>
        }

        /// Configures with default options.
        static member Create : mainAssemblyPath: string -> Config

    /// The command instance.
    val Instance : Commands.ICommand<Config>

    /// The implementation of the command is deferred until
    /// the Sitelts assembly, this interface works as plumbing for
    /// the forward-declaration.
    type IHtmlCommand =

        /// Executes the command.
        abstract Execute : Commands.Environment * Config -> Commands.Result
