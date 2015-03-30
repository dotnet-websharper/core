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

namespace WebSharper.Tests

open WebSharper.Core.Macros
module C = WebSharper.Core.JavaScript.Core
module S = WebSharper.Core.JavaScript.Syntax

[<Sealed>]
type HelloQuotationGenerator() =
    interface IGenerator with
        member this.Body =
            <@@ fun w -> "Hello " + w + "!" @@>
            |> QuotationBody

[<Sealed>]
type HelloCoreGenerator() =
    interface IGenerator with
        member this.Body =
            let w = C.Id "w"
            C.Lambda (None, [w], !~(C.String "Hello ") + C.Var w + !~(C.String "!"))
            |> CoreBody

[<Sealed>]
type HelloJSGenerator() =
    interface IGenerator with
        member this.Body =
            S.Lambda (None, ["w"], [ S.Action (S.Return (Some (!~(S.String "Hello ") + S.Var "w" + !~(S.String "!")))) ])
            |> SyntaxBody

module Macro =

    open WebSharper
    open WebSharper.Testing
    open WebSharper.JavaScript

    [<Generated(typeof<HelloQuotationGenerator>)>]
    let helloQuotation (w: string) = X<string>

    [<Generated(typeof<HelloCoreGenerator>)>]
    let helloCore (w: string) = X<string>

    [<Generated(typeof<HelloJSGenerator>)>]
    let helloJS (w: string) = X<string>

    [<JavaScript>]
    let Tests =

        Section "Macro"

        Test "Generated" {
            helloQuotation "world" =? "Hello world!"    
            helloCore "world" =? "Hello world!"    
            helloJS "world" =? "Hello world!"    
        }
    

