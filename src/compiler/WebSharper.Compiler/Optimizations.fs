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

module WebSharper.Compiler.Optimizations

open System.Collections.Generic

open WebSharper.Core
open WebSharper.Core.AST
open WebSharper.Compiler

module I = WebSharper.Core.JavaScript.Identifier

open IgnoreSourcePos

let (|Runtime|_|) e =
    match e with
    | GlobalAccess { Module = JavaScriptFile "Runtime"; Address = Hashed [ a; "WSRuntime" ] } -> Some a
    | _ -> None

let (|Global|_|) e = 
    match e with 
    | GlobalAccess { Module = JavaScriptFile "Runtime"; Address = Hashed [ r ] } -> Some r
    | _ -> None
    
let (|AppItem|_|) e =
    match e with
    | Application (ItemGet (obj, Value (String item), _), args, _) -> Some (obj, item, args)
    | _ -> None

let (|AppRuntime|_|) e =
    match e with
    | Application (Runtime rtFunc, args, _) -> Some (rtFunc, args)
    | _ -> None

let (|GetPrototypeConstuctor|_|) e =
    match e with
    | ItemGet(ItemGet(GlobalAccess m, Value (String "prototype"), _), Value (String "constructor"), _) -> Some m
    | _ -> None

let AppItem (obj, item, args) =
    ApplAny(ItemGet(obj, Value (String item), Pure), args)

let func vars ret body isReturn =
    if isReturn then Lambda(vars, ret, body) else Function(vars, true, ret, ExprStatement body)

let thisFunc (this: Id) vars ret body isReturn =
    func vars ret (FixThisScope(this.VarType).Fix(SubstituteVar(this, This).TransformExpression(body))) isReturn

let globalArray = Address.Lib "Array"

let cleanRuntime force expr =
//    let tr = Transform clean
    match expr with
    | Application (Global "id", [ x ], _) -> 
        x
    | Application (Global "ignore", [ x ], _) -> 
        Unary(UnaryOperator.``void``, x)
    | Application (AppRuntime ("Bind", [f; obj]), args, _) -> 
        AppItem(f, "call", obj :: args)
    //| Application(Application(AppRuntime("Curried2", [ f ]), [ a ], _), [ b ], info) ->
    //    Application(f, [ a; b ], { info with KnownLength = Some 2 })
    //| Application(Application(Application(AppRuntime("Curried3", [ f ]), [ a ], _), [ b ], _), [ c ], info) ->
    //    Application(f, [ a; b; c ], { info with KnownLength = Some 3 })

    | AppItem(NewArray arr, "concat", [ NewArray rest ]) ->
        NewArray (arr @ rest)    
    | AppRuntime(rtFunc, xs) ->
        match rtFunc, xs with
        | "Apply", [AppRuntime ("Bind", [f; obj]); ignoredObj; args] ->
            AppItem(f, "apply", [obj; Sequential [ignoredObj; args]])
        
        //used by functions with rest argument
        | "Apply", [GlobalAccess mf; Value Null ] ->
            ApplAny (GlobalAccess mf, [])
        | "Apply", [GlobalAccess mf; Value Null; NewArray arr ] ->
            ApplAny (GlobalAccess mf, arr)
        | "Apply", [GlobalAccess mf; Value Null; AppItem(NewArray arr, "concat", [ NewArray rest ]) ] ->
            ApplAny (GlobalAccess mf, arr @ rest)

        | "Apply", [GlobalAccess mf; GlobalAccess m ] when mf.Module = m.Module && mf.Address.Value.Tail = m.Address.Value ->
            ApplAny (GlobalAccess mf, [])
        | "Apply", [GlobalAccess mf; GlobalAccess m; NewArray arr ] when mf.Module = m.Module && mf.Address.Value.Tail = m.Address.Value ->
            ApplAny (GlobalAccess mf, arr)
        | "Apply", [GlobalAccess mf; GlobalAccess m; AppItem(NewArray arr, "concat", [ NewArray rest ]) ] when mf.Module = m.Module && mf.Address.Value = m.Address.Value ->
            ApplAny (GlobalAccess mf, arr @ rest)
        
        | "Apply", [GetPrototypeConstuctor m1; GlobalAccess m2 ] when m1 = m2 ->
            if m1 = globalArray then NewArray []
            else New(GlobalAccess m1, [], [])
        | "Apply", [GetPrototypeConstuctor m1; GlobalAccess m2; NewArray arr ] when m1 = m2 ->
            if m1 = globalArray then NewArray arr
            else New(GlobalAccess m1, [], arr)
        | "Apply", [GetPrototypeConstuctor m1; GlobalAccess m2; AppItem(NewArray arr, "concat", [ NewArray rest ]) ] when m1 = m2 ->
            if m1 = globalArray then NewArray (arr @ rest)
            else New(GlobalAccess m1, [], arr @ rest)

        | "Apply", [ Application(Runtime "Curried", [f; Value (Int l)], info); ignoredObj; NewArray args ] 
            when args.Length = l && isPureExpr ignoredObj ->
                Application(f, args, { info with KnownLength = Some l })
        | "Apply", [f; obj; args] when force ->
            AppItem(f, "apply", [ obj; args ])
        | "Apply", [f; obj] when force ->
            AppItem(f, "apply", [ obj ])
        | "CreateFuncWithArgs", [ TupledLambda (vars, ret, body, isReturn) as f ] ->
            func vars ret body isReturn |> WithSourcePosOfExpr f
        | "CreateFuncWithArgs", _ ->
#if DEBUG
            printfn "non-optimized CreateFuncWithArgs: %A" (Debug.PrintExpression expr)
#endif
            expr
        | "CreateFuncWithOnlyThis", [ Lambda ([obj], ret, body, isReturn) as f ] ->
            thisFunc obj [] ret body isReturn |> WithSourcePosOfExpr f
        | "CreateFuncWithThis", [ Lambda ([obj], _, Lambda (args, ret, body, isReturn), true) as f ] ->
            thisFunc obj args ret body isReturn |> WithSourcePosOfExpr f   
        | "CreateFuncWithThis", [ AppRuntime ("Curried", [Lambda([obj; arg], ret, body, isReturn) as f]) ] ->
            thisFunc obj [arg] ret body isReturn |> WithSourcePosOfExpr f   
        | "CreateFuncWithThisArgs", [ Lambda ([obj], _, TupledLambda (vars, ret, body, isReturn), true) as f ] ->
            thisFunc obj vars ret body isReturn |> WithSourcePosOfExpr f
        | "CreateFuncWithThisArgs", [ AppRuntime ("Curried", [Lambda([obj; arg], ret, body, isReturn) as f]) ] ->
            match func [arg] ret body isReturn with
            | TupledLambda(vars, ret, body, _) ->
                thisFunc obj vars ret body isReturn |> WithSourcePosOfExpr f
            | _ ->
                thisFunc obj [arg] ret body isReturn |> WithSourcePosOfExpr f
        | "CreateFuncWithRest", [ Value (Int length); TupledLambda (vars, ret, body, isReturn) as f ] ->
            match List.rev vars with
            | rest :: fixRev ->
                let fix = List.rev fixRev
                if containsVar rest body then
                    func fix ret (Let (rest, sliceFromArguments [ length ], body)) isReturn |> WithSourcePosOfExpr f
                else
                    func fix ret body isReturn |> WithSourcePosOfExpr f
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
    // printf and string interpolation translation cleanup
    | Application(Global "String", [Value Null], _) -> !~(String "null")
    | Application(Global "String", [Value (String _) as s], _) -> s
    | Application(GlobalAccess a, [Value Null], _) when a.Address.Value = [ "toSafe"; "Utils"; "WebSharper" ] -> !~(String "")
    | Application(GlobalAccess a, [Value (String _) as s], _) when a.Address.Value = [ "toSafe"; "Utils"; "WebSharper" ] -> s
    | Binary(Value (String s1), BinaryOperator.``+``, Value (String s2)) -> !~(String (s1 + s2))
    | Let (var, value, body) ->
        //transform function if it is always used as JavaScript interop
        let transformIfAlwaysInterop rtFunc getJsFunc =
            let (|WithInterop|_|) e =
                match e with
                | Application (Runtime f, [ Var v ], _) when f = rtFunc && v = var -> Some ()
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
        | TupledLambda (vars, ret, lBody, isReturn) ->
            transformIfAlwaysInterop "CreateFuncWithArgs" (fun () -> func vars ret lBody isReturn)
        | Lambda ([obj], _, Lambda (args, ret, lBody, isReturn), true) ->
            transformIfAlwaysInterop "CreateFuncWithThis" (fun () -> thisFunc obj args ret lBody isReturn)
        | Lambda ([obj], ret, lBody, isReturn) ->
            transformIfAlwaysInterop "CreateFuncWithOnlyThis" (fun () -> thisFunc obj [] ret lBody isReturn)
        | Lambda ([obj], _, TupledLambda (vars, ret, lBody, isReturn), true) ->
            transformIfAlwaysInterop "CreateFuncWithThisArgs" (fun () -> thisFunc obj vars ret lBody isReturn)
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
