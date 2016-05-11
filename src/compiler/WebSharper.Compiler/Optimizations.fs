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

module internal WebSharper.Compiler.Optimizations

open System.Collections.Generic

open WebSharper.Core
open WebSharper.Core.AST

open IgnoreSourcePos

type ForAllSubExpr(checker) =
    inherit Visitor()
    let mutable ok = true

    override this.VisitExpression(e) =
        if not (checker e) then
            ok <- false
        elif ok then base.VisitExpression e

    member this.Check(e) = 
        ok <- true
        this.VisitExpression(e)
        ok

type BottomUpTransformer(tr) =
    inherit Transformer()

    override this.TransformExpression(e) =
        base.TransformExpression(e) |> tr

let BottomUp tr expr =
    BottomUpTransformer(tr).TransformExpression(expr)  

let containsVar v expr =
    CountVarOccurence(v).Get(expr) > 0

let sliceFromArguments slice =
    Application (Global [ "Array"; "prototype"; "slice"; "call" ], 
        Arguments :: [ for a in slice -> !~ (Int a) ])

let (|TupledLambda|_|) expr =
    match expr with
    | Function ([tupledArg], Return b) ->
        // when the tuple itself is bound to a name, there will be an extra let expression
        let tupledArg, b =
            match b with
            | Let (newTA, Var t, b) when t = tupledArg -> 
                newTA, SubstituteVar(tupledArg, Var newTA).TransformExpression b
            | _ -> tupledArg, b
        let rec loop acc = function
            | Let (v, ItemGet(Var t, Value (Int i)), body) when t = tupledArg ->
                loop ((int i, v) :: acc) body
            | body -> 
                if List.isEmpty acc then None else
                let m = Map.ofList acc
                Some (
                    [ for i in 0 .. (acc |> Seq.map fst |> Seq.max) -> 
                        match m |> Map.tryFind i with
                        | None -> Id.New()
                        | Some v -> v 
                    ], body)
        match loop [] b with
        | None -> None
        | Some (vars, body) -> 
            if containsVar tupledArg body then
                let (|TupleGet|_|) e =
                    match e with 
                    | ItemGet(Var t, Value (Int i)) when t = tupledArg ->
                        Some (int i)
                    | _ -> None 
                let maxTupleGet = ref (vars.Length - 1)
                let checkTupleGet e =
                    match e with 
                    | TupleGet i -> 
                        if i > !maxTupleGet then maxTupleGet := i
                        true
                    | Var t when t = tupledArg -> false
                    | _ -> true
                let alwaysTupleGet e =
                    ForAllSubExpr(checkTupleGet).Check(e)
                if alwaysTupleGet body then
                    let vars = 
                        if List.length vars > !maxTupleGet then vars
                        else vars @ [ for k in List.length vars .. !maxTupleGet -> Id.New() ]
                    Some (vars, body |> BottomUp (function TupleGet i -> Var vars.[i] | e -> e))
                else 
                    // if we would use the arguments object for anything else than getting
                    // a tuple item, convert it to an array
                    Some (vars, Let (tupledArg, sliceFromArguments [], body))
            else
                Some (vars, body)
    | _ -> None

let ThisLambda (this, args, body) =
    Lambda(args, FixThisScope().Fix(SubstituteVar(this, This).TransformExpression(body)))

let (|Runtime|_|) e = 
    match e with 
    | GlobalAccess a ->
        match a.Value with
        | [ r; "Runtime"; "IntelliFactory" ] -> Some r
        | _ -> None
    | _ -> None
    
let (|AppItem|_|) e =
    match e with
    | Application (ItemGet (obj, Value (String item)), args) -> Some (obj, item, args)
    | _ -> None

let (|Lambda|_|) e = 
    match e with
    | Function(args, Return body) -> Some (args, body)
    | _ -> None

let AppItem (obj, item, args) =
    Application(ItemGet(obj, Value (String item)), args)

let cleanRuntime expr =
//    let tr = Transform clean
    match expr with
    | Application (Application (Runtime "Bind", [f; obj]), args) -> 
        AppItem(f, "call", obj :: args)
    | AppItem(Application (Runtime "Bind", [f; obj]), "apply", [args]) ->
        AppItem(f, "apply", [obj; args])
    | Application(Runtime rtFunc, xs) ->
        match rtFunc, xs with
        | "Apply", [ Application(Runtime "Curried", [Lambda(vars, _) as f]); NewArray args ] 
            when vars.Length = args.Length ->
            Application(f, args)   
        | "CreateFuncWithArgs", [ TupledLambda (vars, body) as f ] ->
            Lambda(vars, body) |> WithSourcePosOfExpr f
        | "CreateFuncWithOnlyThis", [ Lambda ([obj], body) as f ] ->
            ThisLambda (obj, [], body) |> WithSourcePosOfExpr f
        | "CreateFuncWithThis", [ Lambda ([obj], Lambda (args, body)) as f ] ->
            ThisLambda (obj, args, body) |> WithSourcePosOfExpr f   
        | "CreateFuncWithThisArgs", [ Lambda ([obj], TupledLambda (vars, body)) as f ] ->
            ThisLambda (obj, vars, body) |> WithSourcePosOfExpr f
        | "CreateFuncWithRest", [ Value (Int length); TupledLambda (vars, body) as f ] ->
            let rest :: fixRev = List.rev vars
            let fix = List.rev fixRev
            if containsVar rest body then
                Lambda (fix, Let (rest, sliceFromArguments [ length ], body)) |> WithSourcePosOfExpr f
            else
                Lambda (fix, body) |> WithSourcePosOfExpr f
        | "SetOptional", [obj; field; optValue] ->
            match optValue with
            | Object ["$", Value (Int 0)] ->
                MutatingUnary(MutatingUnaryOperator.delete, ItemGet(obj, field)) |> WithSourcePosOfExpr expr
            | Object ["$", Value (Int 1); "$0", value] ->
                ItemSet (obj, field, value) |> WithSourcePosOfExpr expr
            | _ -> expr     
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
                    | Lambda _            
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
                    let names = NewArray [for f in toDelete -> !~(String f)]
                    Application (JSRuntime.DeleteEmptyFields, [obj; names; Value Null])
            else expr
        | _ -> expr
    | Let (var, value, body) ->
        //transform function if it is always used as JavaScript interop
        let transformIfAlwaysInterop rtFunc getJsFunc =
            let (|WithInterop|_|) e =
                match e with
                | Application (Runtime f, [ Var v ]) when f = rtFunc && v = var -> Some ()
                | _ -> None
            let rec isWithInterop e =
                match e with
                | WithInterop -> true
                | Var v when v = var -> false
                | _ -> true
            if ForAllSubExpr(isWithInterop).Check(body) then
                Let(var, getJsFunc() |> WithSourcePosOfExpr value, 
                    body |> BottomUp (function WithInterop -> Var var | e -> e))
            else expr
        match value with
        | TupledLambda (vars, lBody) ->
            transformIfAlwaysInterop "CreateFuncWithArgs" (fun () -> Lambda (vars, lBody))
        | Lambda ([obj], Lambda (args, lBody)) ->
            transformIfAlwaysInterop "CreateFuncWithThis" (fun () -> ThisLambda (obj, args, lBody))
        | Lambda ([obj], lBody) ->
            transformIfAlwaysInterop "CreateFuncWithOnlyThis" (fun () -> ThisLambda (obj, [], lBody))
        | Lambda ([obj], TupledLambda (vars, lBody)) ->
            transformIfAlwaysInterop "CreateFuncWithThisArgs" (fun () -> ThisLambda (obj, vars, lBody))
        | _ ->
            expr
    //used by functions with rest argument
    | Application (ItemGet(NewArray arr, Value (String "concat")), [ NewArray rest ]) ->
        NewArray (arr @ rest)    
    | ItemGet (Object fs, Value (String fieldName)) ->
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
            let resVar = Id.New fieldName
            Let (resVar, result, 
                Sequential (List.rev (Var resVar :: nonPureAfter))
            )
    | ItemGet (NewArray fs, Value (Int index)) ->
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
            let resVar = Id.New ("item" + string index)
            Let (resVar, result, 
                Sequential (List.rev (Var resVar :: nonPureAfter))
            )
    | _ -> expr
