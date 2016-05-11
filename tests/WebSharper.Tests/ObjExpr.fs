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

module WebSharper.Tests.ObjExpr

open WebSharper
open WebSharper.JavaScript
open WebSharper.Testing

[<JavaScript>]
type TestObj() as self =
    let mutable v = 0
    do self.SetDefault()  

    abstract SetDefault : unit -> unit
    override this.SetDefault() = v <- 1

    abstract SetValueModified : int -> unit
    override this.SetValueModified x = v <- x

    member this.Value 
        with get () = v
        and set x = v <- x

[<JavaScript>]
let Tests =
    TestCategory "ObjExpr" {
        Test "IDisposable" {
            let d = ref false
            do
                use x =
                    { new System.IDisposable with
                        member this.Dispose() = d := true
                    }
                ()
            isTrue !d
        }

        Test "Custom class" {
            equal (TestObj().Value) 1
            
            let o =
                { new TestObj() with
                    override this.SetDefault() = this.Value <- 2
                    override this.SetValueModified x = this.Value <- x + 1
                }    
            equal o.Value 2
            o.SetValueModified 3
            equal o.Value 4                   
        }
    }