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

module WebSharper.Tests.AddressOf

open System.Runtime.InteropServices

open WebSharper
open WebSharper.JavaScript
open WebSharper.Testing

[<JavaScript>]
type AddressOfTests() =

    static member AddOne(x: byref<int>) = 
        x <- x + 1

    static member OutOne([<Out>] x: byref<int>) = 
        x <- 1

    static member AddTwo(x: byref<int>) = 
        AddressOfTests.AddOne(&x)
        AddressOfTests.AddOne(&x)

    static member ByRefReturn (x, a: byref<int>, b: byref<int>) =
        if x then &a else &b

[<JavaScript>]
type MutableRecord = 
    {
        mutable Value: int
    }

[<JavaScript>]
module M =
    let mutable moduleValue = 0

[<JavaScript>]
let Tests =
    TestCategory "AddressOf" {
    
        Test "Operator" {
            let res =
                let mutable x = 1
                AddressOfTests.AddOne(&x)
                x
            equal res 2

            let res2 = 
                let mutable x = 1
                let xr = &x
                xr <- 2 // assign to byref
                xr + 1 // dereference

            equal res2 3
        }
    
        Test "Passing ref" {
            let x = ref 1
            AddressOfTests.AddOne(x)
            equal !x 2
        }

        Test "Passing byref" {
            let res =
                let mutable x = 1
                AddressOfTests.AddTwo(&x)
                x
            equal res 3
        }

        Test "Record field" {
            let x = { Value = 1 }
            AddressOfTests.AddOne(&x.Value)
            equal x.Value 2
        }

        Test "Module let" {
            equal M.moduleValue 0
            M.moduleValue <- 1
            equal M.moduleValue 1
            AddressOfTests.AddOne(&M.moduleValue)
            equal M.moduleValue 2
        }

        Test "Array element" {
            let arr = [| 1; 2 |]
            AddressOfTests.AddOne(&arr.[1])
            equal arr [| 1; 3 |] 
        }

        Test "Out" {
            let res =
                let mutable x = 1
                AddressOfTests.OutOne(&x)
                x
            equal res 1

            equal (AddressOfTests.OutOne()) 1
        }      

        Test "Byref returns" {
            let res =
                let mutable a = 0
                let mutable b = 0
                let ar = AddressOfTests.ByRefReturn(true, &a, &b) 
                let br = AddressOfTests.ByRefReturn(false, &a, &b) 
                ar <- 1
                br <- 2
                a, b

            equal res (1, 2)
        }
    }
