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

module Q = WebSharper.Core.Quotations

[<Sealed>]
type NameOfMacro() =
    interface IMacro with
        member this.Translate(q, _) =
            match q with
            | Q.CallModule (c, []) ->
                match c.Generics with
                | [t] -> !~(C.String t.FullName) 
                | _ -> failwith "NameOfMacro error"
            | _ -> failwith "NameOfMacro error"

[<Sealed>]
type AddMacro() =
    interface IMacro with
        member this.Translate(q, tr) =
            match q with
            | Q.CallModule (_, [a; b]) ->
                match a, b with
                | Q.Value (Q.Int ai), Q.Value (Q.Int bi) ->
                    !~ (C.Integer (int64 (ai + bi)))
                | _ -> tr q
            | _ -> failwith "AddMacro error"

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

    [<Macro(typeof<NameOfMacro>)>]
    let nameof<'a> = X<string>

    [<Macro(typeof<AddMacro>)>]
    let add a b = a + b 

    [<JavaScript>]
    let Tests =

        Section "Metaprogramming"

        Test "Generated" {
            Equal (helloQuotation "world") "Hello world!"
            Equal (helloCore "world") "Hello world!"
            Equal (helloJS "world") "Hello world!"    
        }

        Test "Macro" {
            Equal nameof<string> "System.String"
            Equal (add 1 2) 3
        }
    


