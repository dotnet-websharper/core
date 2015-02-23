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

/// Provides a writer for JavaScript syntax.
module WebSharper.Core.JavaScript.Writer

type CodeWriter =
    new : ?assemblyName : string -> CodeWriter 
    member GetMapFile : unit -> string option
    member GetCodeFile : unit -> string
    member GetSourceFiles : unit -> string[]

/// Writes a JavaScript expression to a writer.
val WriteExpression : Preferences -> CodeWriter -> Syntax.Expression -> unit

/// Writes a JavaScript program to a writer.
val WriteProgram : Preferences -> CodeWriter -> Syntax.Program -> unit

/// Writes a JavaScript expression to a string.
val ExpressionToString : Preferences -> Syntax.Expression -> string

/// Writes a JavaScript program to a string.
val ProgramToString : Preferences -> Syntax.Program -> string
