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

[<WebSharper.Proxy "Microsoft.FSharp.Linq.NullableModule, FSharp.Core">]
module private WebSharper.NullableModuleProxy 

open System
open WebSharper.JavaScript

type N<'T when 'T: (new: unit -> 'T) and 'T: struct and 'T :> ValueType> = Nullable<'T> 

module M = WebSharper.Core.Macros

[<Macro(typeof<M.Char>)>]
let ToChar (x: N<'T>) = X<N<char>>

[<Macro(typeof<M.Conversion>)>]
let ToDouble (x: N<'T>) = X<N<double>>

[<Macro(typeof<M.Conversion>)>]
let ToFloat (x: N<'T>) = X<N<float>>

[<Macro(typeof<M.Conversion>)>]
let ToInt (x: N<'T>) = X<N<int>>

[<Macro(typeof<M.Conversion>)>]
let ToSingle (x: N<'T>) = X<N<single>>

[<Macro(typeof<M.Conversion>)>]
let ToInt32 (x: N<'T>) = X<N<int32>>

[<Inline "$x">]
let ToEnum<'T when 'T: (new: unit -> 'T) and 'T: struct and 'T :> ValueType> (x: N<int>) = X<N<'T>>

[<Macro(typeof<M.Conversion>)>]
let ToInt64 (x: N<'T>) = X<N<int64>>
