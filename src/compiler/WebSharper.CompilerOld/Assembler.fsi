// $begin{copyright}
//
// This file is part of WebSharper
//
// Copyright (c) 2008-2015 IntelliFactory
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

/// Assembles reflected code into JavaScript packages.
module internal WebSharper.Compiler.Assembler

module P = WebSharper.Core.JavaScript.Packager

/// Assembles the reflected code.
val Assemble : Logger -> Inlining.Pool -> Reflector.Pool ->
               Metadata.T -> Validator.Assembly -> unit
