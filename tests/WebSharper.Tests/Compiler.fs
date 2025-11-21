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
        let ctx = WebSharper.Web.Remoting.GetContext()
        match a with
        | P.Call(_, mi, _) ->
            let typ = Reflection.ReadTypeDefinition mi.DeclaringType
            let meth = Reflection.ReadMethod mi
            match ctx.Metadata.Classes.[typ] with
            | _, _, Some cls ->
                let expr = cls.Methods.[meth].Expression
                expr, meth.Value.MethodName
            | _ -> failwith "failed to look up class info"
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
        let (|MayCastAny|) = function Cast(TSType.Any, x) | x -> x
        async.Return [|
            
            testWithMatch <@ Optimizations.TupledArgWithGlobal() @> <| function
            | Function (_, _, _, Return (Application (GlobalAccess _, [ GlobalAccess _ ], _))) -> true
            | _ -> false
        
            testWithMatch <@ Optimizations.TupledArgWithLocal() @> <| function
            | Function (_, _, _, Return (Application (GlobalAccess _, [ Function ([ _; _], _, _, _) ], _))) -> true
            | _ -> false
        
            testWithMatch <@ Optimizations.CurriedArgWithGlobal() @> <| function
            | Function (_, _, _, Return (Application (GlobalAccess _, [ GlobalAccess _ ], _))) -> true
            | _ -> false

            testWithMatch <@ Optimizations.CurriedArgWithLocal() @> <| function
            | Function (_, _, _, Return (Application (GlobalAccess _, [ Function ([ _; _], _, _, _) ], _))) -> true
            | _ -> false

            testWithMatch <@ Optimizations.CollectJSObject() @> <| function
            | Function (_, _, _, Return (Object ["a", MemberKind.Simple, MayCastAny (Value (Int 1));
                                                 "b", MemberKind.Simple, MayCastAny (Sequential [_; Value (Int 2)]);
                                                 "c", MemberKind.Simple, MayCastAny (Sequential [_; Value (Int 3)]);
                                                ])) -> true
            | _ -> false

            testWithMatch <@ Optimizations.InlineValues() @> <| function
            | Function (_, _, _, ExprStatement (Application(_, [MayCastAny(Value (String "a"));
                                                             MayCastAny(Value (String "b"));
                                                            ], { Purity = NonPure; KnownLength = None }) )) -> true
            | _ -> false

            testWithMatch <@ Optimizations.InlineValues2() @> <| function
            | Function (_, _, _, ExprStatement (Application(_, [MayCastAny(Sequential [_; Value (String "a")]);
                                                             MayCastAny(Sequential [_; Value (String "b")]);
                                                            ], { Purity = NonPure; KnownLength = None }) )) -> true
            | _ -> false

            testWithMatch <@ Optimizations.InlineValues3() @> <| function
            | Function (_, _, _, Return (NewArray [Sequential [_; Value (String "a")]; Sequential [_; Value (String "b")]])) -> true
            | _ -> false
        
        |]

[<JavaScript>]
type MyFeatureClient =
    static let mutable initialized = false
    
    static member Initialize() = initialized <- true
    
    static member IsInitialized() = initialized

type MyFeature() =
    
    member this.InitializeMethod =
        let clientType = typeof<MyFeatureClient>
        let initializeMethod = clientType.GetMethod("Initialize")
        WebSharper.Core.AST.Reflection.ReadTypeDefinition clientType,
        WebSharper.Core.AST.Reflection.ReadMethod initializeMethod

    interface IBundleExports with
        member this.Exports(_) =
            [| this.InitializeMethod |]

    interface IRequiresResources with
        member this.Requires(meta, json, getId) =
            let clientType, initializeMethod = this.InitializeMethod
            match meta.Classes.TryGetValue(clientType) with
            | true, (cAddr, _, Some cls) ->
                match cls.Methods.TryGetValue(initializeMethod) with
                | true, mtd ->
                    match mtd.CompiledForm with
                    | WebSharper.Core.Metadata.CompiledMember.Static (name, _, _) ->
                        let initialize = ClientCode.ClientImport (cAddr.Static(name))
                        [ ClientCode.ClientApply(initialize, []) ]
                    | _ -> failwith "MyFeature: Expected function compiled form"
                | _ -> failwith "MyFeature: Failed to look up method"
            | _ -> failwith "MyFeature: Failed to look up class"

    interface Web.INode with
        member this.Write(_, _) = ()
        member this.IsAttribute = false

[<JavaScript>]
let Tests =
    TestCategory "Compiler" {
        Test "Optimizations" {
            let! res = Server.OptimizationTests()
            forEach res (fun (r, msg) ->
                Do { equalMsg r "" msg }
            )
        }

        Test "RequireFeature" {
            isTrue (MyFeatureClient.IsInitialized())
        }
    }

[<RequireFeature(typeof<MyFeature>)>]
let RequireMyFeature() = ()
