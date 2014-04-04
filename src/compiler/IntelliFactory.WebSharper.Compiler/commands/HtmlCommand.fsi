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
