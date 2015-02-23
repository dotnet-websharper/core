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

/// Provides utilities for working with JavaScript identifier names.
module WebSharper.Core.JavaScript.Identifier

/// Checks if a string is a reserved word in JavaScript.
val IsReserved : string -> bool

/// Checks if a string is a valid JavaScript identifier name.
val IsValid : string -> bool

/// Replaces bad characters by underscore to make an identifier valid.
val MakeValid : string -> string

/// Constructs a compact numeric identifier formatter.
val internal MakeFormatter : unit -> (int -> string)
