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

module WebSharper.Compiler.Extra

/// Copies files specified in extra.files to the html directory of a mobile application
/// takes in the directory in which extra.files is contained and the destination.
val CopyFiles : dir: string -> dest: string -> unit

/// Finds all files in a directory that match the given patterns.
val FindAllFiles: dir: string -> paths: string list -> string list

/// Copies files specified in extra.files to the output directory.
/// Returns extra resources to be potentially embedded in the assembly.
val ProcessFiles : dir: string -> plainDest: string option -> asmDest: string option -> string[]