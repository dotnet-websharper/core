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

namespace IntelliFactory.WebSharper

module M = Macro

[<Proxy(typeof<PrintfFormat<_,_,_,_,_>>)>]
type PrintfFormat = 
    [<Macro(typeof<M.PrintF>)>]
    new (value: string) = {}

[<Name "Printf">]
[<Proxy
    "Microsoft.FSharp.Core.PrintfModule, \
     FSharp.Core, Culture=neutral, \
     PublicKeyToken=b03f5f7f11d50a3a">]
module private PrintfProxy =
    [<Inline "$f($k)">]
    let PrintFormatToStringThen (k: string -> _) (f: Printf.StringFormat<_, _>) = X

    [<JavaScript; Inline>]
    let PrintFormatLine (f: Printf.StringFormat<_>) = PrintFormatToStringThen JavaScript.Log f  

    [<JavaScript; Inline>]
    let PrintFormatToStringThenFail (f: Printf.StringFormat<_>) = PrintFormatToStringThen failwith f
