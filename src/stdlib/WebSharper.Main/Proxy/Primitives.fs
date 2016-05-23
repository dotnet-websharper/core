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

module M = WebSharper.Macro

[<Macro(typeof<M.NumericMacro>)>]
[<Proxy(typeof<System.Byte>)>]
type internal NB = class end

[<Macro(typeof<M.NumericMacro>)>]
[<Proxy(typeof<System.SByte>)>]
type internal NSB = class end

[<Macro(typeof<M.NumericMacro>)>]
[<Proxy(typeof<System.Int16>)>]
type internal NI16 = class end

[<Macro(typeof<M.NumericMacro>)>]
[<Proxy(typeof<System.Int32>)>]
type internal NI32 = class end

[<Macro(typeof<M.NumericMacro>)>]
[<Proxy(typeof<System.UInt16>)>]
type internal NUI16 = class end

[<Macro(typeof<M.NumericMacro>)>]
[<Proxy(typeof<System.UInt32>)>]
type internal NUI32 = class end

[<Macro(typeof<M.NumericMacro>)>]
[<Proxy(typeof<System.Int64>)>]
type internal NI64 = class end

[<Macro(typeof<M.NumericMacro>)>]
[<Proxy(typeof<System.Single>)>]
type internal NS = class end

[<Macro(typeof<M.NumericMacro>)>]
[<Proxy(typeof<System.Double>)>]
type internal ND = class end
