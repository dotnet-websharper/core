// $begin{copyright}
//
// This file is part of WebSharper
//
// Copyright (c) 2008-2015 IntelliFactory
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

open WebSharper.Core
open WebSharper.Core.AST
module S = WebSharper.Core.JavaScript.Syntax

[<Sealed>]
type HelloQuotationGenerator() =
    inherit Generator()
    override this.Generate (_,_) =
        <@@ fun w -> "Hello " + w + "!" @@>
        |> GeneratedQuotation

[<Sealed>]
type HelloASTGenerator() =
    inherit Generator()
    override this.Generate (_,_) =
        let w = Id.New "w"
        let (+) a b = Binary(a, BinaryOperator.``+``, b)
        Lambda ([w], !~(String "Hello ") + Var w + !~(String "!"))
        |> GeneratedAST

//[<Sealed>]
//type HelloJSGenerator() =
//    inherit Generator()
//    member this.Body =
//        S.Lambda (None, ["w"], [ S.Action (S.Return (Some (!~(S.String "Hello ") + S.Var "w" + !~(S.String "!")))) ])
//        |> GeneratedJavaScript

//module Q = WebSharper.Core.Quotations

[<Sealed>]
type NameOfMacro() =
    inherit Macro()
    override this.TranslateCall(_, _, c, _, _) =
        match c.Generics with
        | [t] -> !~(String t.TypeDefinition.Value.FullName) |> MacroOk
        | _ -> MacroError "NameOfMacro error"

[<Sealed>]
type AddMacro() =
    inherit Macro()
    override this.TranslateCall(_, _, _, args, _) =
        match args with
        | [a; b] ->
            match ignoreExprSourcePos a, ignoreExprSourcePos b with
            | Value (Int ai), Value (Int bi) ->
                !~ (Int64 (int64 (ai + bi))) |> MacroOk
            | _ -> MacroFallback
        | _ -> MacroError "AddMacro error"
            
module Macro =

    open WebSharper
    open WebSharper.Testing
    open WebSharper.JavaScript

    [<Generated(typeof<HelloQuotationGenerator>)>]
    let helloQuotation (w: string) = X<string>

    [<Generated(typeof<HelloASTGenerator>)>]
    let helloAST (w: string) = X<string>

//    [<Generated(typeof<HelloJSGenerator>)>]
//    let helloJS (w: string) = X<string>

    [<Macro(typeof<NameOfMacro>)>]
    let nameof<'a> = X<string>

    [<Macro(typeof<AddMacro>)>]
    let add a b = a + b 
    [<JavaScript>]
    let Tests =
        TestCategory "Metaprogramming" {
                    
            Test "Generated" {
                equal (helloQuotation "world") "Hello world!"
                equal (helloAST "world") "Hello world!"
//                equal (helloJS "world") "Hello world!"    
            }

            Test "Macro" {
                equal nameof<string> "System.String"
                equal (add 1 2) 3
            }
        }
