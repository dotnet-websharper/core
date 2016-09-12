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

namespace WebSharper.Compiler

/// Performs bundling of JavaScript and CSS resources,
/// producing linked artifacts for a set of WebSharper assemblies.
module BundleCommand =

    /// Configuration for the bundle command.
    type Config =
        {
            /// Paths to WebSharper assemblies to consider.
            AssemblyPaths : list<string>

            /// Path to the application configuration file to use.
            AppConfigFile : option<string>

            /// File name used for generated files.
            FileName : string

            /// Output directory where generated files are placed.
            OutputDirectory : string

            SourceMap : bool
        }

        static member Create : unit -> Config

    /// The command instance.
    val Instance : Commands.ICommand<Config>
