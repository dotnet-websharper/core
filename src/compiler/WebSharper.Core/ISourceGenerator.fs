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

/// Defines custom attributes used by WebSharper projects.
namespace WebSharper

type GenerateCall =
    {
        /// Full path to the file the generator is invoked for.
        FilePath: string
        /// Project file full path.
        ProjectFilePath: string
        /// Print to the standard output.
        Print: string -> unit
        /// Print to the error output.
        PrintError: string -> unit
    }

/// Use with assembly-level FSharpSourceGenerator attribute
type ISourceGenerator =
    /// Gets an object of contextual information and actions.
    /// Must return an array of file paths, which can be newly created.
    abstract Generate: GenerateCall -> string[] 