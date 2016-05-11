// $begin{copyright}
//
// This file is part of WebSharper
//
// Copyright (c) 2008-2016 IntelliFactory
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

namespace WebSharper

open WebSharper.JavaScript

[<Proxy(typeof<option<_>>)>]
[<Name "WebSharper.Option.T">]
[<CompilationRepresentation (CompilationRepresentationFlags.UseNullAsTrueValue)>]
[<DefaultAugmentation(false)>]
[<RequireQualifiedAccess>]
type private OptionProxy<'T> =
    | None
    | Some of 'T

    [<CompilationRepresentation (CompilationRepresentationFlags.Instance)>]
    member this.Value with [<Inline "$this.$0">] get () = X<'T>

    [<Inline "$x != null">]
    static member get_IsSome(x: option<'T>) = false

    [<Inline "$x == null">]
    static member get_IsNone(x: option<'T>) = false

    [<Inline; JavaScript>]  
    static member Some(v: 'T) = As<'T option> (Some v)  
  
    [<Inline; JavaScript>]  
    static member get_None<'T>() = As<'T option> None

    [<Inline "$x ? 1 : 0">]
    static member GetTag(x: option<'T>) = 0
