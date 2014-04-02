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

/// Implements offline sitelet HTML generation.
namespace IntelliFactory.WebSharper.Sitelets.Offline

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
