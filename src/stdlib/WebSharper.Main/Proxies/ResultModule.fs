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

#if JAVASCRIPT
[<WebSharper.NameAttribute "Result">]
[<WebSharper.Proxy
    "Microsoft.FSharp.Core.ResultModule, \
     FSharp.Core, Culture=neutral, \
     PublicKeyToken=b03f5f7f11d50a3a">]
module private WebSharper.ResultModuleProxy
    
let Bind f r =
    match r with
    | Ok x -> f x
    | Error e -> Error e
        
let Map f r =
    match r with
    | Ok x -> Ok (f x)
    | Error e -> Error e
        
let MapError f r =
    match r with
    | Ok x -> Ok x
    | Error e -> Error (f e)    
#else
namespace WebSharper
#endif