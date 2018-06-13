﻿// $begin{copyright}
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
module WebSharper.SPA.Tests.JavaScriptExportTest2

open WebSharper
open WebSharper.JavaScript

type IncludedFromAssemblyLevel1() =
    member this.Helo() =
        Console.Log "Hello from JavaScriptExportTest2.IncludedFromAssemblyLevel1"

type IncludedFromAssemblyLevel2() =
    member this.Helo() =
        Console.Log "Hello from JavaScriptExportTest2.IncludedFromAssemblyLevel2"

[<JavaScriptExport>]
let testThatThisIsIncluded2 () =
    Console.Log "Hello from JavaScriptExportTest2"