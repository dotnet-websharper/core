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

module WebSharper.Tests.Reflected

open WebSharper
open WebSharper.Testing

[<JavaScript; ReflectedDefinition>]
let add0 () = 1 + 1

[<JavaScript; ReflectedDefinition>]
let add1 (x: int) = x + 1

[<JavaScript; ReflectedDefinition>]
let add2 (x: int, y: int) = x + y

[<JavaScript; ReflectedDefinition>]
let add3 (x: int) (y: int) = x + y

[<JavaScript; ReflectedDefinition>]
let add4 (x: int, y: int) (z: int) = x + y + z

[<JavaScript; ReflectedDefinition>]
type Inst(a: int) =
    member this.Add0 () = a + 1
    member this.Add1 (x: int) = a + x
    member this.Add2 (x: int, y: int) = a + x + y
    member this.Add3 (x: int) (y: int) = a + x + y
    member this.Add4 (x: int, y: int) (z: int) = a + x + y + z

[<JavaScript>]
let Tests =

    TestCategory "ReflectedDefinition" {

        Test "Zero argument" {
            equal (add0()) 2
        } 

        Test "One argument" {
            equal (add1 1) 2
        } 

        Test "Tupled argument" {
            equal (add2 (1, 2)) 3
        } 

        Test "Curried argument" {
            equal (add3 1 2) 3
        } 

        Test "Tupled and curried argument" {
            equal (add4 (1, 2) 3) 6
        } 

        Test "Instance methods" {
            let i = Inst(1)
            equal (i.Add0()) 2
            equal (i.Add1 1) 2
            equal (i.Add2 (1, 2)) 4
            equal (i.Add3 1 2) 4
            equal (i.Add4 (1, 2) 3) 7
        }
    }
