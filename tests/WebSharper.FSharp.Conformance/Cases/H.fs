// $begin{copyright}
//
// This file is part of WebSharper
//
// Copyright (c) 2008-2026 IntelliFactory
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
namespace Conformance

open WebSharper

/// Helpers shared by the cases: `C "area.name" (fun () -> ...)` is one case.
[<JavaScript>]
module H =
    let C (name: string) (f: unit -> string) = name, f
    let J (xs: seq<string>) = "[" + String.concat "," xs + "]"
    let Ji (xs: seq<int>) = J (Seq.map string xs)
    let B (b: bool) = if b then "true" else "false"
