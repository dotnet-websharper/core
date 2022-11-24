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

module WebSharper.Module.Tests.Import

open WebSharper
open WebSharper.JavaScript
open WebSharper.Testing

[<JavaScript>]
let mA, mB = 1, 2

//[<Inline>]
//let importTestJsAll : obj = JS.ImportAll "/modules/test.js"

//[<Inline>]
//let importTestJs : obj = JS.Import("testExport", "/modules/test.js")

//[<Inline>]
//let importTestJsDefault : obj = JS.ImportDefault "/modules/test.js"

//[<Inline "import('/modules/test.js')">]
//let importInline = X<obj>

[<JavaScript>]
let Tests =
    TestCategory "Import" {

        Test "JS.Import" {
            let sayHi = JS.Import<string -> string>("sayHi", "WebSharper.Module.Tests/sayHi.js")
            equal (sayHi "World") "Hello, World!"
        }

        Test "JS.ImportDynamic" {
            let! sayHiModule = JS.ImportDynamic("./sayHi.js") |> Promise.AsAsync
            let sayHi = As<string -> string>(sayHiModule?sayHi)
            equal (sayHi "World") "Hello, World!"
        }

        //Test "import" {
        //    Console.Log("import all", importTestJsAll)
        //    Console.Log("import all 2", importTestJsAll)
        //    Console.Log("import", importTestJs)
        //    Console.Log("import default", importTestJsDefault)
        //    Console.Log("import inline", importInline)
        //    expect 0
        //}
    }
