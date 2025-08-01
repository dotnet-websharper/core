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

[<Import "./lib/sayHi.js">]
type MyClassNoStub() =
    member this.sayHiInst(user: string) = X<string>
    static member sayHiStatic(user: string) = X<string>

[<Stub; Import "./lib/sayHi.js">]
type MyClassStub() =
    member this.sayHiInst(user: string) = X<string>
    static member sayHiStatic(user: string) = X<string>

[<Import "./lib/sayHi.js">]
type MyClassInline [<Inline "new $import()">] () =
    [<Inline "$this.sayHiInst($user)">]
    member this.sayHiInst(user: string) = X<string>
    [<Inline "$import.sayHiStatic($user)">]
    static member sayHiStatic(user: string) = X<string>

[<Import ("sayHi", "./lib/sayHi.js")>]
let sayHiFunc (str: string) = X<string> 

type SayHiNonModule() =
    inherit Core.Resources.BaseResource("lib/sayHiNonModule.js")

[<Require(typeof<SayHiNonModule>)>]
[<Inline "sayHiNonModule($str)">]
let sayHiNonModuleFunc (str: string) = X<string> 

[<Import "is-sorted">]
let isSorted (array: 'T[]) = X<bool> 

[<JavaScript>]
module JSXTest =
    let html() =
        let x = "hello"
        let kecske x = JS.Html $"<div>{x?children}</div>" 
        JS.Html $"<h1>{x}</h1><{kecske}>{x}</{kecske}>"

[<JavaScript>]
module NPMTest =
    let npmImportTest() =
        WebSharper.InterfaceGenerator.Tests.NPMTest.SayHiStatic "World"

[<JavaScript>]
let Tests =
    TestCategory "Import" {

        Test "JS.Import" {
            let sayHi = JS.Import<string -> string>("sayHi", "./lib/sayHi.js")
            equal (sayHi "World") "Hello, World!"
        }

        Test "JS.Import default" {
            let sayHiClass = JS.Import<obj>("default", "./lib/sayHi.js")
            let sayHi = sayHiClass?sayHiStatic : string -> string
            equal (sayHi "World") "Hello, World!"
        }

        Test "JS.ImportDefault" {
            let sayHiClass = JS.ImportDefault<obj>("./lib/sayHi.js")
            let sayHi = sayHiClass?sayHiStatic : string -> string
            equal (sayHi "World") "Hello, World!"
        }

        Test "JS.Import *" {
            let sayHiModule = JS.Import<obj>("*", "./lib/sayHi.js")
            let sayHi = sayHiModule?sayHi : string -> string
            equal (sayHi "World") "Hello, World!"
        }

        Test "JS.ImportAll" {
            let sayHiModule = JS.ImportAll<obj>("./lib/sayHi.js")
            let sayHi = sayHiModule?sayHi : string -> string
            equal (sayHi "World") "Hello, World!"
        }

        Test "JS.ImportDynamic" {
            let! sayHiModule = JS.ImportDynamic("../WebSharper.Module.Tests/lib/sayHi.js") |> Promise.AsAsync
            let sayHi = As<string -> string>(sayHiModule?sayHi)
            equal (sayHi "World") "Hello, World!"
        }

        Test "Import attribute without Stub" {
            equal (MyClassNoStub.sayHiStatic "World") "Hello, World!"
            let c = MyClassNoStub() 
            equal (c.sayHiInst "World") "Hello, World!"
        }

        Test "Import attribute with Stub" {
            equal (MyClassStub.sayHiStatic "World") "Hello, World!"
            let c = MyClassStub() 
            equal (c.sayHiInst "World") "Hello, World!"
        }

        Test "Import attribute with Inline" {
            equal (MyClassInline.sayHiStatic "World") "Hello, World!"
            let c = MyClassInline() 
            equal (c.sayHiInst "World") "Hello, World!"
        }

        Test "Import attribute on function" {
            equal (sayHiFunc "World") "Hello, World!"
        }

        Test "Non-module resource defined in F#" {
            equal (sayHiNonModuleFunc "World") "Hello, World!"
        }

        Test "WIG Import" {
            equal (WebSharper.InterfaceGenerator.Tests.WIGtest4.SayHiFunc "World") "Hello, World!"
            equal (WebSharper.InterfaceGenerator.Tests.WIGtest4.SayHiStatic "World") "Hello, World!"
            let c = WebSharper.InterfaceGenerator.Tests.WIGtest4()
            equal (c.SayHiInst "World") "Hello, World!"
        }

        Test "npm import" {
            isTrue (isSorted([| 1; 2; 3 |]))
            isFalse (isSorted([| 1; 3; 2 |]))
        }
    }
