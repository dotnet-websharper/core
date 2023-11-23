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

[<Stub; Import "./sayHi.js">]
type MyClassStub() =
    member this.sayHiInst(user: string) = X<string>
    static member sayHiStatic(user: string) = X<string>

[<Import "./sayHi.js">]
type MyClassInline [<Inline "new $import()">] () =
    [<Inline "$this.sayHiInst($user)">]
    member this.sayHiInst(user: string) = X<string>
    [<Inline "$import.sayHiStatic($user)">]
    static member sayHiStatic(user: string) = X<string>

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
            let sayHi = JS.Import<string -> string>("sayHi", "./sayHi.js")
            equal (sayHi "World") "Hello, World!"
        }

        Test "JS.Import default" {
            let sayHiClass = JS.Import<obj>("default", "./sayHi.js")
            let sayHi = sayHiClass?sayHiStatic : string -> string
            equal (sayHi "World") "Hello, World!"
        }

        Test "JS.ImportDefault" {
            let sayHiClass = JS.ImportDefault<obj>("./sayHi.js")
            let sayHi = sayHiClass?sayHiStatic : string -> string
            equal (sayHi "World") "Hello, World!"
        }

        Test "JS.Import *" {
            let sayHiModule = JS.Import<obj>("*", "./sayHi.js")
            let sayHi = sayHiModule?sayHi : string -> string
            equal (sayHi "World") "Hello, World!"
        }

        Test "JS.ImportAll" {
            let sayHiModule = JS.ImportAll<obj>("./sayHi.js")
            let sayHi = sayHiModule?sayHi : string -> string
            equal (sayHi "World") "Hello, World!"
        }

        Test "JS.ImportDynamic" {
            let! sayHiModule = JS.ImportDynamic("./sayHi.js") |> Promise.AsAsync
            let sayHi = As<string -> string>(sayHiModule?sayHi)
            equal (sayHi "World") "Hello, World!"
        }

        Skip "Import attribute with Stub" {
            equal (MyClassStub.sayHiStatic "World") "Hello, World!"
            let c = MyClassStub() 
            equal (c.sayHiInst "World") "Hello, World!"
        }

        Test "Import attribute with Inline" {
            equal (MyClassInline.sayHiStatic "World") "Hello, World!"
            let c = MyClassInline() 
            equal (c.sayHiInst "World") "Hello, World!"
        }

        Test "WIG Import" {
            equal (WebSharper.InterfaceGenerator.Tests.WIGtest4.SayHiFunc "World") "Hello, World!"
            equal (WebSharper.InterfaceGenerator.Tests.WIGtest4.SayHiStatic "World") "Hello, World!"
            let c = WebSharper.InterfaceGenerator.Tests.WIGtest4()
            equal (c.SayHiInst "World") "Hello, World!"
        }
    }
