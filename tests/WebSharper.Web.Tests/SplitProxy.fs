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

module WebSharper.Web.Tests.SplitProxy

open WebSharper
open WebSharper.JavaScript.Interop

[<Proxy "WebSharper.Collections.Tests.SplitProxy+ClassInfoMergeTestType, WebSharper.Collections.Tests">]
module private TestTypeProxy =
    [<Name "WebSharper.Collections.Tests.SplitProxy.ClassInfoMergeTestType.Member2">]
    let [<Inline "'member2'">] Member2 () = X<string>
