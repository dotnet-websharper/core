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

/// Implements offline sitelet HTML generation.
namespace WebSharper.Sitelets.Offline

[<AutoOpen>]
module internal PathUtility =

    [<Sealed>]
    type Path

    [<Sealed>]
    type Pattern

    val CreatePath : list<string> -> Path
    val ParsePath : string -> Path
    val ParsePattern : string -> Pattern
    val RootPath : Path
    val ShowPath : Path -> string
    val ShowPattern : Pattern -> string
    val SubPath : Path -> string -> Path
    val ToAbsolute : baseDir: string -> Path -> string
    val TryParsePath : string -> option<Path>
    val TryParsePattern : string -> option<Pattern>
    val (|RootPattern|SubPattern|) : Pattern -> Choice<unit,Pattern*string>
    val (|RootPath|SubPath|) : Path -> Choice<unit,Path*string>
