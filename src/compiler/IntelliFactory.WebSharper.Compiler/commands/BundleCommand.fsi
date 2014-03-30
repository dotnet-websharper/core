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

/// Performs bundling of JavaScript and CSS resources,
/// producing linked artifacts for a set of WebSharper assemblies.
module BundleCommand =

    /// Configuration for the bundle command.
    type Config =
        {
            /// Defaults to `true`, runs the command in a fresh AppDomain.
            AppDomainIndirection : bool

            /// Paths to WebSharper assemblies to consider.
            AssemblyPaths : list<string>

            /// File name used for generated files.
            FileName : string

            /// Output directory where generated files are placed.
            OutputDirectory : string
        }

        static member Create : unit -> Config

    /// The command instance.
    val Instance : Commands.ICommand<Config>
