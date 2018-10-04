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

module WebSharper.Compiler.Optimizations

open System.Collections.Generic

open WebSharper.Core
open WebSharper.Core.AST
open WebSharper.Compiler

module I = WebSharper.Core.JavaScript.Identifier

open IgnoreSourcePos

//let (|GlobalAccess|_|) e =
//    let rec ga acc e =
//        match e with
//        | GlobalAccess a -> Some (Address (acc @ a.Value))
//        | ItemGet(m, Value (String f), _) -> ga (f :: acc) m
//        | _ -> None
//    match e with 
//    | GlobalAccess a -> Some a
//    | _ -> ga [] e

let (|Runtime|_|) e = 
    match e with 
    | GlobalAccess a ->
        match a.Value with
        | [ r; "Runtime"; "IntelliFactory" ] -> Some r
        | _ -> None
    | _ -> None

let (|Global|_|) e = 
    match e with 
    | GlobalAccess a ->
        match a.Value with
        | [ r ] -> Some r
        | _ -> None
    | _ -> None
    
let (|AppItem|_|) e =
    match e with
    | Application (ItemGet (obj, Value (String item), _), args, _, _) -> Some (obj, item, args)
    | _ -> None

let (|AppRuntime|_|) e =
    match e with
    | Application (Runtime rtFunc, args, _, _) -> Some (rtFunc, args)
    | _ -> None

let (|GetPrototypeConstuctor|_|) e =
    match e with
    | ItemGet(ItemGet(GlobalAccess m, Value (String "prototype"), _), Value (String "constructor"), _) -> Some m
    | _ -> None

let AppItem (obj, item, args) =
    Application(ItemGet(obj, Value (String item), Pure), args, NonPure, None)

let func vars body isReturn =
    if isReturn then Lambda(vars, body) else Function(vars, ExprStatement body)

let thisFunc this vars body isReturn =
    func vars (FixThisScope().Fix(SubstituteVar(this, This).TransformExpression(body))) isReturn

let globalArray = Address [ "Array" ]

let cleanRuntime force expr =
//    let tr = Transform clean
    match expr with
    | Application (Global "id", [ x ], _, _) -> 
        x
    | Application (Global "ignore", [ x ], _, _) -> 
        Unary(UnaryOperator.``void``, x)
    | Application (AppRuntime ("Bind", [f; obj]), args, _, _) -> 
        AppItem(f, "call", obj :: args)
    | Application(Application(AppRuntime("Curried2", [ f ]), [ a ], _, _), [ b ], isPure, _) ->
        Application(f, [ a; b ], isPure, Some 2)
    | Application(Application(Application(AppRuntime("Curried3", [ f ]), [ a ], _, _), [ b ], _, _), [ c ], isPure, _) ->
        Application(f, [ a; b; c ], isPure, Some 3)

    | AppItem(NewArray arr, "concat", [ NewArray rest ]) ->
        NewArray (arr @ rest)    
    | AppRuntime(rtFunc, xs) ->
        match rtFunc, xs with
        | "Apply", [AppRuntime ("Bind", [f; obj]); ignoredObj; args] ->
            AppItem(f, "apply", [obj; Sequential [ignoredObj; args]])
        
        //used by functions with rest argument
        | "Apply", [GlobalAccess mf; Value Null ] ->
            Application (GlobalAccess mf, [], NonPure, None)
        | "Apply", [GlobalAccess mf; Value Null; NewArray arr ] ->
            Application (GlobalAccess mf, arr, NonPure, None)
        | "Apply", [GlobalAccess mf; Value Null; AppItem(NewArray arr, "concat", [ NewArray rest ]) ] ->
            Application (GlobalAccess mf, arr @ rest, NonPure, None)

        | "Apply", [GlobalAccess mf; GlobalAccess m ] when mf.Value.Tail = m.Value ->
            Application (GlobalAccess mf, [], NonPure, None)
        | "Apply", [GlobalAccess mf; GlobalAccess m; NewArray arr ] when mf.Value.Tail = m.Value ->
            Application (GlobalAccess mf, arr, NonPure, None)
        | "Apply", [GlobalAccess mf; GlobalAccess m; AppItem(NewArray arr, "concat", [ NewArray rest ]) ] when mf.Value = m.Value ->
            Application (GlobalAccess mf, arr @ rest, NonPure, None)
        
        | "Apply", [GetPrototypeConstuctor m1; GlobalAccess m2 ] when m1 = m2 ->
            if m1 = globalArray then NewArray []
            else New(GlobalAccess m1, [])
        | "Apply", [GetPrototypeConstuctor m1; GlobalAccess m2; NewArray arr ] when m1 = m2 ->
            if m1 = globalArray then NewArray arr
            else New(GlobalAccess m1, arr)
        | "Apply", [GetPrototypeConstuctor m1; GlobalAccess m2; AppItem(NewArray arr, "concat", [ NewArray rest ]) ] when m1 = m2 ->
            if m1 = globalArray then NewArray (arr @ rest)
            else New(GlobalAccess m1, arr @ rest)

        | "Apply", [ Application(Runtime "Curried", [f; Value (Int l)], isPure, _); ignoredObj; NewArray args ] 
            when args.Length = l && isPureExpr ignoredObj ->
                Application(f, args, isPure, Some l)
        | "Apply", [f; obj; args] when force ->
            AppItem(f, "apply", [ obj; args ])
        | "Apply", [f; obj] when force ->
            AppItem(f, "apply", [ obj ])
        | "CreateFuncWithArgs", [ TupledLambda (vars, body, isReturn) as f ] ->
            func vars body isReturn |> WithSourcePosOfExpr f
        | "CreateFuncWithArgs", _ ->
#if DEBUG
            printfn "non-optimized CreateFuncWithArgs: %A" (Debug.PrintExpression expr)
#endif
            expr
        | "CreateFuncWithOnlyThis", [ Lambda ([obj], body, isReturn) as f ] ->
            thisFunc obj [] body isReturn |> WithSourcePosOfExpr f
        | "CreateFuncWithThis", [ Lambda ([obj], Lambda (args, body, isReturn), true) as f ] ->
            thisFunc obj args body isReturn |> WithSourcePosOfExpr f   
        | "CreateFuncWithThis", [ AppRuntime ("Curried", [Lambda([obj; arg], body, isReturn) as f]) ] ->
            thisFunc obj [arg] body isReturn |> WithSourcePosOfExpr f   
        | "CreateFuncWithThisArgs", [ Lambda ([obj], TupledLambda (vars, body, isReturn), true) as f ] ->
            thisFunc obj vars body isReturn |> WithSourcePosOfExpr f
        | "CreateFuncWithThisArgs", [ AppRuntime ("Curried", [Lambda([obj; arg], body, isReturn) as f]) ] ->
            match func [arg] body isReturn with
            | TupledLambda(vars, body, _) ->
                thisFunc obj vars body isReturn |> WithSourcePosOfExpr f
            | _ ->
                thisFunc obj [arg] body isReturn |> WithSourcePosOfExpr f
        | "CreateFuncWithRest", [ Value (Int length); TupledLambda (vars, body, isReturn) as f ] ->
            match List.rev vars with
            | rest :: fixRev ->
                let fix = List.rev fixRev
                if containsVar rest body then
                    func fix (Let (rest, sliceFromArguments [ length ], body)) isReturn |> WithSourcePosOfExpr f
                else
                    func fix body isReturn |> WithSourcePosOfExpr f
            | _ -> expr
        | "SetOptional", [obj; field; optValue] ->
            match optValue with
            | Object ["$", Value (Int 0)] ->
                MutatingUnary(MutatingUnaryOperator.delete, ItemGet(obj, field, NonPure)) |> WithSourcePosOfExpr expr
            | Object ["$", Value (Int 1); "$0", value] ->
                ItemSet (obj, field, value) |> WithSourcePosOfExpr expr
            | _ -> expr     
        | "SetOrDelete", [obj; field; value] ->
            if isTrivialValue value then
                match value with
                | Undefined ->
                    MutatingUnary(MutatingUnaryOperator.delete, ItemGet(obj, field, NonPure)) |> WithSourcePosOfExpr expr
                | _ ->
                    ItemSet (obj, field, value) |> WithSourcePosOfExpr expr
            else expr     
        | "NewObject", [NewArray keyValuePairs] ->
            let withConstantKey =
                keyValuePairs |> List.choose (function 
                    | NewArray [Value (String k); v] -> Some (k, v) 
                    | _ -> None)
            if withConstantKey.Length = keyValuePairs.Length then
                Object (withConstantKey |> List.map (fun (k, v) -> k, v)) |> WithSourcePosOfExpr expr
            else expr
        | "DeleteEmptyFields", [Object fs; NewArray names] ->
            let toDelete = HashSet (names |> Seq.choose (function Value (String n) -> Some n | _ -> None))
            if names.Length = toDelete.Count then
                let remaining = ResizeArray()
                let rec alwaysHasValue e =
                    match e with
                    | Arguments        
                    | Value _
                    | Function _            
                    | New _               
                    | NewArray _          
                    | Object _ -> true        
                    | Let (_, _, b)       
                    | LetRec (_, b) -> alwaysHasValue b     
                    | Sequential b -> alwaysHasValue (List.last b)     
                    | _ -> false    
                for (n, v) in fs do
                    if toDelete.Contains n then
                        if v = Undefined then 
                            toDelete.Remove n |> ignore
                        else
                            if alwaysHasValue v then
                                toDelete.Remove n |> ignore
                            remaining.Add (n, v)
                    else remaining.Add (n, v)
                let obj = Object (List.ofSeq remaining)
                if toDelete.Count = 0 then
                    obj
                else                  
                    JSRuntime.DeleteEmptyFields obj [for f in toDelete -> !~(String f)]
            else expr
        | _ -> expr
    | Let (var, value, body) ->
        //transform function if it is always used as JavaScript interop
        let transformIfAlwaysInterop rtFunc getJsFunc =
            let (|WithInterop|_|) e =
                match e with
                | Application (Runtime f, [ Var v ], _, _) when f = rtFunc && v = var -> Some ()
                | _ -> None
            let rec isWithInterop e =
                match e with
                | WithInterop -> Some true
                | Var v when v = var -> Some false
                | _ -> None
            if ForAllSubExpr(isWithInterop).Check(body) then
                Let(var, getJsFunc() |> WithSourcePosOfExpr value, 
                    body |> BottomUp (function WithInterop -> Var var | e -> e))
            else expr
        match value with
        | TupledLambda (vars, lBody, isReturn) ->
            transformIfAlwaysInterop "CreateFuncWithArgs" (fun () -> func vars lBody isReturn)
        | Lambda ([obj], Lambda (args, lBody, isReturn), true) ->
            transformIfAlwaysInterop "CreateFuncWithThis" (fun () -> thisFunc obj args lBody isReturn)
        | Lambda ([obj], lBody, isReturn) ->
            transformIfAlwaysInterop "CreateFuncWithOnlyThis" (fun () -> thisFunc obj [] lBody isReturn)
        | Lambda ([obj], TupledLambda (vars, lBody, isReturn), true) ->
            transformIfAlwaysInterop "CreateFuncWithThisArgs" (fun () -> thisFunc obj vars lBody isReturn)
        | _ ->
            expr
    | ItemGet (Object fs, Value (String fieldName), _) when not (I.IsObjectMember fieldName) ->
        let mutable nonPureBefore = []
        let mutable nonPureAfter = []
        let mutable fieldValue = None
        for n, v in fs do
            if n = fieldName then
                fieldValue <- Some v
            else 
                if not (isPureExpr v) then
                    match fieldValue with
                    | None -> nonPureBefore <- v :: nonPureBefore
                    | _ -> nonPureAfter <- v :: nonPureAfter
        let fieldValue = defaultArg fieldValue Undefined
        let result =
            Sequential (List.rev (fieldValue :: nonPureBefore))
        if List.isEmpty nonPureAfter then
            result 
        else 
            let resVar = Id.New (fieldName, false)
            Let (resVar, result, 
                Sequential (List.rev (Var resVar :: nonPureAfter))
            )
    | ItemGet (NewArray fs, Value (Int index), _) ->
        let mutable nonPureBefore = []
        let mutable nonPureAfter = []
        let mutable fieldValue = None
        let mutable i = 0
        for v in fs do
            if i = int index then
                fieldValue <- Some v
            else 
                if not (isPureExpr v) then
                    match fieldValue with
                    | None -> nonPureBefore <- v :: nonPureBefore
                    | _ -> nonPureAfter <- v :: nonPureAfter
            i <- i + 1
        let fieldValue = defaultArg fieldValue Undefined
        let result =
            Sequential (List.rev (fieldValue :: nonPureBefore))
        if List.isEmpty nonPureAfter then
            result 
        else 
            let resVar = Id.New ("item" + string index, false)
            Let (resVar, result, 
                Sequential (List.rev (Var resVar :: nonPureAfter))
            )
    // created by FSharpRef if using record constructor
    | Object [ "0", x ] ->
        NewArray [ x ]
    | _ -> expr
