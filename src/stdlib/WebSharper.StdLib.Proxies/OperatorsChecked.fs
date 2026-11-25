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

/// F# checked arithmetic and conversions (open Checked, Checked.(+), Checked.int): .NET throws
/// OverflowException where the unchecked operators wrap around.
[<WebSharper.Name "Checked">]
[<WebSharper.Proxy "Microsoft.FSharp.Core.Operators+Checked, FSharp.Core">]
module private WebSharper.CheckedProxy

open WebSharper.JavaScript

module M = WebSharper.Core.Macros

[<Macro(typeof<M.CheckedArith>)>]
let ( + ) (a: 'T1) (b: 'T2) = X<'T3>

[<Macro(typeof<M.CheckedArith>)>]
let ( - ) (a: 'T1) (b: 'T2) = X<'T3>

[<Macro(typeof<M.CheckedArith>)>]
let ( * ) (a: 'T1) (b: 'T2) = X<'T3>

[<Macro(typeof<M.CheckedArith>)>]
let ( ~- ) (a: 'T) = X<'T>

[<Macro(typeof<M.CheckedConversion>)>]
let ToByte (x: 'T) = X<byte>

[<Macro(typeof<M.CheckedConversion>)>]
let ToSByte (x: 'T) = X<sbyte>

[<Macro(typeof<M.CheckedConversion>)>]
let ToInt16 (x: 'T) = X<int16>

[<Macro(typeof<M.CheckedConversion>)>]
let ToUInt16 (x: 'T) = X<uint16>

[<Macro(typeof<M.CheckedConversion>)>]
let ToInt (x: 'T) = X<int>

[<Macro(typeof<M.CheckedConversion>)>]
let ToInt32 (x: 'T) = X<int32>

[<Macro(typeof<M.CheckedConversion>)>]
let ToUInt32 (x: 'T) = X<uint32>

[<Macro(typeof<M.CheckedConversion>)>]
let ToInt64 (x: 'T) = X<int64>

[<Macro(typeof<M.CheckedConversion>)>]
let ToUInt64 (x: 'T) = X<uint64>

[<Macro(typeof<M.CheckedConversion>)>]
let ToChar (x: 'T) = X<char>
