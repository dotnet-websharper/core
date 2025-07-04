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

module WebSharper.Module.Tests.Main

open WebSharper
open WebSharper.Testing

[<assembly: JavaScript "WebSharper.Tests.Basis+TestOptionals">] // test for JavaScript "TypeName"
[<assembly: WebResource("sayHiNonModule.js", "text/javascript")>]
[<assembly: WebResource("sayHi.js", "text/javascript")>]
do()

[<JavaScript>]
let RunTests () =
    Runner.RunTests [|
        StubInterfaces.Tests
        Import.Tests
    |]
