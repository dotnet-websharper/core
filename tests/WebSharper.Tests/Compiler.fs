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

module WebSharper.Tests.Compiler

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
            WebSharper.Web.Shared.Metadata.Classes.[typ].Methods.[meth]
        | _ -> failwith "expected a Call pattern"

    [<Remote>]
    let TupledArgWithGlobal() =
        let _, _, expr = getCompiled <@ Optimizations.TupledArgWithGlobal() @>
        match expr with 
        | Function (_, Return (Application (GlobalAccess _, [ GlobalAccess _ ], _, _))) -> 
            ""
        | _ -> "unexpected optimized form: " + Debug.PrintExpression expr
        |> async.Return 

    [<Remote>]
    let TupledArgWithLocal() =
        let _, _, expr = getCompiled <@ Optimizations.TupledArgWithLocal() @>
        match expr with 
        | Function (_, Return (Application (GlobalAccess _, [ Function ([ _; _], _) ], _, _))) -> 
            ""
        | _ -> "unexpected optimized form: " + Debug.PrintExpression expr
        |> async.Return 

    [<Remote>]
    let CurriedArgWithGlobal() =
        let _, _, expr = getCompiled <@ Optimizations.CurriedArgWithGlobal() @>
        match expr with 
        | Function (_, Return (Application (GlobalAccess _, [ GlobalAccess _ ], _, _))) -> 
            ""
        | _ -> "unexpected optimized form: " + Debug.PrintExpression expr
        |> async.Return 

    [<Remote>]
    let CurriedArgWithLocal() =
        let _, _, expr = getCompiled <@ Optimizations.CurriedArgWithLocal() @>
        match expr with 
        | Function (_, Return (Application (GlobalAccess _, [ Function ([ _; _], _) ], _, _))) -> 
            ""
        | _ -> "unexpected optimized form: " + Debug.PrintExpression expr
        |> async.Return 

[<JavaScript>]
let Tests =
    TestCategory "Compiler" {
        Test "Optimizations.TupledArgWithGlobal" {
            let! res = Server.TupledArgWithGlobal()
            equal res ""
        }

        Test "Optimizations.TupledArgWithLocal" {
            let! res = Server.TupledArgWithLocal()
            equal res ""
        }

        Test "Optimizations.CurriedArgWithGlobal" {
            let! res = Server.CurriedArgWithGlobal()
            equal res ""
        }

        Test "Optimizations.CurriedArgWithLocal" {
            let! res = Server.CurriedArgWithLocal()
            equal res ""
        }
    }
