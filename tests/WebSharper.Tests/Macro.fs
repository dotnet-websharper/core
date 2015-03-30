// $begin{copyright}
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

module WebSharper.Tests.Macro

open WebSharper
open WebSharper.Testing
open WebSharper.JavaScript
//
//open WebSharper.Core.Macros
//module C = WebSharper.Core.JavaScript.Core
//
//[<Sealed>]
//type HelloWorldCoreMacro() =
//    interface IMacroDefinition with
//        member this.Macro =
//            let b = 
//                C.Lambda (None, [], C.Binary(!~(C.String "Hello "), C.BinaryOperator.``+``, !~(C.String "world!")))
//            {
//                Body         = Some (CoreBody b)
//                Expand       = id
//                Requirements = []
//            }

//[<Macro(typeof<HelloWorldCoreMacro>)>]
//[<Macro(typeof<Assert.HelloWorldCoreMacro>)>]
[<JavaScript>]
let helloWorld() = "Hello world!"

[<JavaScript>]
let Tests =

    Section "Macro"

    Test "Core macro" {
        helloWorld() =? "Hello world!"    
    }
    

