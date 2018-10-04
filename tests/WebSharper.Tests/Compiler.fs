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

module WebSharper.Tests.Compiler

#if NET461

open WebSharper
open WebSharper.JavaScript
open WebSharper.Testing

module Server =
    open WebSharper.Core.AST
    open IgnoreSourcePos
    open FSharp.Quotations
    module P = Patterns

    let getCompiled a =
        match a with
        | P.Call(_, mi, _) ->
            let typ = Reflection.ReadTypeDefinition mi.DeclaringType
            let meth = Reflection.ReadMethod mi
            let _, _, expr = WebSharper.Web.Shared.Metadata.Classes.[typ].Methods.[meth]
            expr, meth.Value.MethodName
        | _ -> failwith "expected a Call pattern"

    type RuntimeCleaner() =
        inherit Transformer()
    
        override this.TransformExpression (a) =
            base.TransformExpression(WebSharper.Compiler.Optimizations.cleanRuntime true a)

    let private runtimeCleaner = RuntimeCleaner()

    let testWithMatch f p =
        let expr, name = getCompiled f
        let res =
            if p expr then "" 
            else
                let opt = runtimeCleaner.TransformExpression expr
                if opt <> expr then
                    "Not fully optimized: " + Debug.PrintExpression expr + " => " + Debug.PrintExpression opt
                else
                    "Unexpected optimized form: " + Debug.PrintExpression expr
        res, name

    [<Remote>]
    let OptimizationTests() =
        async.Return [|
            
            testWithMatch <@ Optimizations.TupledArgWithGlobal() @> <| function
            | Function (_, Return (Application (GlobalAccess _, [ GlobalAccess _ ], _, _))) -> true
            | _ -> false
        
            testWithMatch <@ Optimizations.TupledArgWithLocal() @> <| function
            | Function (_, Return (Application (GlobalAccess _, [ Function ([ _; _], _) ], _, _))) -> true
            | _ -> false
        
            testWithMatch <@ Optimizations.TupledArgWithLocalAlias() @> <| function
            | Function (_, Return (Application (GlobalAccess _, [ Function ([ _; _], _) ], _, _))) -> true
            | _ -> false
        
            testWithMatch <@ Optimizations.CurriedArgWithGlobal() @> <| function
            | Function (_, Return (Application (GlobalAccess _, [ GlobalAccess _ ], _, _))) -> true
            | _ -> false

            testWithMatch <@ Optimizations.CurriedArgWithLocal() @> <| function
            | Function (_, Return (Application (GlobalAccess _, [ Function ([ _; _], _) ], _, _))) -> true
            | _ -> false

            testWithMatch <@ Optimizations.CollectJSObject() @> <| function
            | Function (_, Return (Object [ "a", Value (Int 1); "b", Sequential [_; Value (Int 2)]; "c", Sequential [_; Value (Int 3)]])) -> true
            | _ -> false

            testWithMatch <@ Optimizations.InlineValues() @> <| function
            | Function (_, ExprStatement (Application(_, [Value (String "a"); Value (String "b")], NonPure, None) )) -> true
            | _ -> false

            testWithMatch <@ Optimizations.InlineValues2() @> <| function
            | Function (_, ExprStatement (Application(_, [Sequential [_; Value (String "a")]; Sequential [_; Value (String "b")]], NonPure, None) )) -> true
            | _ -> false

            testWithMatch <@ Optimizations.InlineValues3() @> <| function
            | Function (_, Return (NewArray [Sequential [_; Value (String "a")]; Sequential [_; Value (String "b")]])) -> true
            | _ -> false
        
        |]

    let funcWithJSAttr ([<JavaScript>] x : Quotations.Expr<unit>) = ()

    let moduleVal = 1

    let callFuncWithJSAttr =
        let x = 2
        if IsClient then
            funcWithJSAttr <@ JavaScript.Console.Log("Hello from callFuncWithJSAttr") @>
            funcWithJSAttr <@ JavaScript.Console.Log("Hello from callFuncWithJSAttr with arg", x) @>

[<JavaScript>]
let Tests =
    TestCategory "Compiler" {
        Test "Optimizations" {
            let! res = Server.OptimizationTests()
            forEach res (fun (r, msg) ->
                Do { equalMsg r "" msg }
            )
        }
    }

#endif
