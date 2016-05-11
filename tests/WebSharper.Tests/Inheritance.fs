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

module WebSharper.Tests.Inheritance

open WebSharper
open WebSharper.JavaScript
open WebSharper.Testing

[<JavaScript>]
type ClassA<'U>() =
    [<Name "x">]
    abstract X : int
    override this.X = 1

    member this.Y = 1

    abstract Z : 'U -> 'U
    override this.Z x = JS.Undefined

[<JavaScript>]
type ClassB<'T, 'U>() =
    inherit ClassA<'U>()

    override this.X = 2

    member this.Y = 2

    member this.BaseX = base.X

    member this.BaseY = base.Y

    override this.Z x = x 

[<JavaScript>]
let mutable ThenTest = 0

[<JavaScript>]
type ClassC =
    val x : int
    val y : int

    new() = { x = 1; y = 1 } 
    
    new(a) = ClassC() then ThenTest <- 1

[<JavaScript>]
let Tests =
    TestCategory "Inheritance" {
        Test "Overriding" {
            equal (ClassB().X) 2   
            equal ((ClassB() :> ClassA<_>).X) 2   
        }

        Test "Naming" {
            equal (ClassB()?x()) 2    
        }

        Test "Hiding" {
            equal (ClassB().Y) 2   
            equal ((ClassB() :> ClassA<_>).Y) 1   
        }

        Test "Base call" {
            equal (ClassB().BaseX) 1    
            equal (ClassB().BaseY) 1    
        }

        Test "Generic" {
            equal (ClassB().Z "hi") "hi"
        }

        Test "No implicit constructor" {
            let a = ClassC()
            equal a.x 1
            equal a.y 1
            let b = ClassC(2)
            equal b.x 1
            equal b.y 1
            equal ThenTest 1 
        }
    }
