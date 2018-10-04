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

// Reads F# reflected definition as WebSharper.Core.AST 
module WebSharper.Compiler.ReflectedDefinitionReader

open System.Reflection
open FSharp.Quotations
open WebSharper.Core.AST

module A = WebSharper.Compiler.AttributeReader
module QR = WebSharper.Compiler.QuotationReader

let readReflected (comp: Compilation) (m: MethodBase) =
    match Expr.TryGetReflectedDefinition m with
    | None -> None
    | Some q -> 
    let attrs = m.GetCustomAttributesData()
    let compArgCounts =
        attrs |> Seq.tryPick (fun a ->
            if a.Constructor.DeclaringType = typeof<CompilationArgumentCountsAttribute> then
                a.ConstructorArguments.[0].Value :?> seq<CustomAttributeTypedArgument>
                |> Seq.map (fun v -> v.Value :?> int) |> List.ofSeq |> Some
            else None  
        )
    let currying =
        match compArgCounts with
        | None ->
            let k = m.GetParameters().Length
            // -1 marks 'this' argument
            if m.IsStatic then 
                if k = 0 then [1] else [k]
            elif k = 0 then [-1; 1] else [-1; k]
        | Some x ->
            if m.IsStatic then x else -1 :: x   
    
    let env = QR.Environment.New(comp)
    
    let rec decurry args curr expr =
        match curr with
        | -1 :: restCurr ->
            match expr with
            | Patterns.Lambda (arg, body) ->
                let i = Id.New(arg.Name, false)
                env.AddVar(i, arg, QR.ThisArg)
                decurry [] restCurr body
            | _ ->
                failwithf "Expecting a lambda while decurrying 'this' argument of a ReflectedDefinition quotation: %A" expr
        | 1 :: restCurr ->
            match expr with
            | Patterns.Lambda (arg, body) ->
                let i = Id.New(arg.Name, false)
                env.AddVar(i, arg, QR.LocalVar)
                decurry (args @ [i]) restCurr body
            | _ ->
                failwithf "Expecting a lambda while decurrying an argument of a ReflectedDefinition quotation: %A" expr
        | k :: restCurr ->
            match expr with
            | Patterns.Lambda (arg, body) ->
                let rec detuple tup j q =
                    match q with
                    | Patterns.Let (vn, Patterns.TupleGet (Patterns.Var tuple, n), q)
                        when tuple = arg && int n = j ->
                        if j + 1 = k then
                            vn :: tup, q
                        else
                            detuple (vn :: tup) (j + 1) q
                    | _ ->
                        failwithf "Expecting a tuple get while detupling arguments a ReflectedDefinition quotation: %A" q
                
                let tupleVars, detupledBody = detuple [] 0 body
                let tupleArgs = 
                    tupleVars |> List.map (fun v ->
                        let a = Id.New(v.Name, false)
                        env.AddVar(a, v, QR.LocalVar)
                        a
                    ) 
                decurry (args @ tupleArgs) restCurr detupledBody
            | _ ->
                failwithf "Expecting a lambda while detupling arguments a ReflectedDefinition quotation: %A" expr
        | _ ->
            Lambda(args, QR.transformExpression env expr)

    Some (decurry [] currying q)
