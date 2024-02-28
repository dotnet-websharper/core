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

module WebSharper.Module.Tests.StubInterfaces

open WebSharper
open WebSharper.JavaScript
open WebSharper.Testing

type GlobalMacro() =
    inherit Core.Macro()

    override this.TranslateCall(c) =
        Core.AST.Expression.GlobalAccess
            {
                Module = Core.AST.Module.StandardLibrary
                Address = [c.Method.Entity.Value.MethodName]
            } |> Core.MacroOk

type ITest =
    abstract member Something: int -> int

[<JavaScript>]
type TestImpl() =
    member this.Something x = x + 1

type ITest2 =
    abstract member Something: int -> int
    abstract member Something: string -> string

[<JavaScript>]
type NonInlinedTest() =
    member this.NotInlined x = x + 1

type ITest3 =
    [<Inline "$0 + $1">]
    abstract member Something: int -> int

    [<Inline "$this + $x">]
    abstract member Something2: x:int -> int

    abstract member NotInlined: x:int -> int

type MyConsole =
    abstract log : string -> unit
    abstract member error : string -> unit

[<Macro(typeof<GlobalMacro>)>]
let console : MyConsole = As<MyConsole> null

[<JavaScript>]
let Tests =
    TestCategory "Stub Interfaces" {
        Test "Config setting" {
            let o = TestImpl()
            equal (o.Something(3)) 4 // ensure member is not DCE-d
            equal ((As<ITest> o).Something(3)) 4
            equal (o?Something(3)) 4
        }

        Test "Name collision ok" {
            let o = TestImpl()
            equal ((As<ITest2> o).Something(3)) 4
            equal ((As<ITest2> o).Something("1")) "11"
        }

        Test "Interface with inline" {
            equal ((As<ITest3> 1).Something(3)) 4
            equal ((As<ITest3> 1).Something2(3)) 4
            let n = NonInlinedTest()
            equal (n.NotInlined(3)) 4 // ensure member is not DCE-d
            equal ((As<ITest3> n).NotInlined(3)) 4
        }

        Test "External implementation stub interfaces" {
            console.log("hello")
            console.error("hola")
            equal 1 1
        }
    }