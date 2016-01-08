module WebSharper.Compiler.Recognize

open WebSharper.Core
open WebSharper.Core.AST

module S = WebSharper.Core.JavaScript.Syntax
type SB = WebSharper.Core.JavaScript.Syntax.BinaryOperator

type Environment =
    {
        Vars : list<IDictionary<string, Expression>>
        Labels : Map<string, Id>
        This : option<Id>
    }
//    static member New(vars) =
//        {
//            Vars =
//                vars |> Seq.mapi (fun i n ->                    
//                    let h = Hole i
//                    [ 
//                        "$" + string i, h
//                        "$" + n, h  
//                    ] 
//                ) |> Seq.concat |> Map.ofSeq
//            Labels = Map.empty
//            This = None
//        }

    static member New(thisArg, isDirect, args) =
        // TODO : add arguments to scope
        let mainScope =
            Option.toList thisArg @ args
            //args
            |> Seq.mapi (fun i (a: Id) ->                    
                let isThis = Some a = thisArg
                let v =
                    if isDirect && isThis then This else Var a
                [ 
                    yield "$" + string i, v
                    yield "$" + a.Name.Value, v
                    if isThis then yield "$this", v
                ] 
            ) |> Seq.concat |> dict
        {
            Vars = [ Dictionary(); mainScope ]
            Labels = Map.empty
            This = None
        }

    member this.WithNewScope (vars) =
        { this with Vars = (Dictionary(dict vars) :> _) :: this.Vars }

    member this.NewVar(name) =
        let v = Id.New name
        match this.Vars with
        | [] -> failwith "no scope"
        | h :: t ->
            h.Add(name, Var v)
            v
            //{ this with Vars = (h |> Map.add name (Var v)) :: t }, v

    member this.TryFindVar(name) =
        let rec findIn scope =
            match scope with
            | [] -> None
            | (h : IDictionary<_,_>) :: t ->
                match h.TryFind name with
                | Some _ as res -> res
                | _ -> findIn t
        findIn this.Vars

exception RecognitionError

let rec transformExpression (env: Environment) (expr: S.Expression) =
    let inline trE e = transformExpression env e
    match expr with
    | S.Application (a, b) ->
        Application (trE a, b |> List.map trE) 
    | S.Binary (a, b, c) ->
        match b with    
        | SB.``!=``     -> Binary(trE a, BinaryOperator.``!=``, trE c)
        | SB.``!==``    -> Binary(trE a, BinaryOperator.``!==``, trE c)
        | SB.``%``      -> Binary(trE a, BinaryOperator.``%``, trE c)
        | SB.``%=``     -> MutatingBinary(trE a, MutatingBinaryOperator.``%=``, trE c)
        | SB.``&``      -> Binary(trE a, BinaryOperator.``&``, trE c)
        | SB.``&&``     -> Binary(trE a, BinaryOperator.``&&``, trE c)
        | SB.``&=``     -> MutatingBinary(trE a, MutatingBinaryOperator.``&=``, trE c)
        | SB.``*``      -> Binary(trE a, BinaryOperator.``*``, trE c)
        | SB.``*=``     -> MutatingBinary(trE a, MutatingBinaryOperator.``*=``, trE c)
        | SB.``+``      -> Binary(trE a, BinaryOperator.``+``, trE c)
        | SB.``+=``     -> MutatingBinary(trE a, MutatingBinaryOperator.``+=``, trE c)
        | SB.``,``      -> Sequential [trE a; trE c]
        | SB.``-``      -> Binary(trE a, BinaryOperator.``-``, trE c)
        | SB.``-=``     -> MutatingBinary(trE a, MutatingBinaryOperator.``-=``, trE c)
        | SB.``.``      -> ItemGet(trE a, trE c)
        | SB.``/``      -> Binary(trE a, BinaryOperator.``/``, trE c)
        | SB.``/=``     -> MutatingBinary(trE a, MutatingBinaryOperator.``/=``, trE c)
        | SB.``<``      -> Binary(trE a, BinaryOperator.``<``, trE c)
        | SB.``<<``     -> Binary(trE a, BinaryOperator.``<<``, trE c)
        | SB.``<<=``    -> MutatingBinary(trE a, MutatingBinaryOperator.``<<=``, trE c)
        | SB.``<=``     -> Binary(trE a, BinaryOperator.``<=``, trE c)
        | SB.``=``      ->
            match ignoreExprSourcePos (trE a) with
            | ItemGet (d, e) -> ItemSet(d, e, trE c)
            | Var d -> VarSet(d, trE c)
            | a -> failwith "TODO: recognize '='"
        | SB.``==``     -> Binary(trE a, BinaryOperator.``==``, trE c)
        | SB.``===``    -> Binary(trE a, BinaryOperator.``===``, trE c)
        | SB.``>``      -> Binary(trE a, BinaryOperator.``>``, trE c)
        | SB.``>>``     -> Binary(trE a, BinaryOperator.``>>``, trE c)
        | SB.``>=``     -> Binary(trE a, BinaryOperator.``>=``, trE c)
        | SB.``>>=``    -> MutatingBinary(trE a, MutatingBinaryOperator.``>>=``, trE c)
        | SB.``>>>``    -> Binary(trE a, BinaryOperator.``>>>``, trE c)
        | SB.``>>>=``   -> MutatingBinary(trE a, MutatingBinaryOperator.``>>>=``, trE c)
        | SB.``^``      -> Binary(trE a, BinaryOperator.``^``, trE c)
        | SB.``^=``     -> MutatingBinary(trE a, MutatingBinaryOperator.``^=``, trE c)
        | SB.``in``     -> Binary(trE a, BinaryOperator.``in``, trE c)
        | SB.instanceof -> Binary(trE a, BinaryOperator.instanceof, trE c)
        | SB.``|``      -> Binary(trE a, BinaryOperator.``|``, trE c)
        | SB.``|=``     -> MutatingBinary(trE a, MutatingBinaryOperator.``|=``, trE c)
        | SB.``||``     -> Binary(trE a, BinaryOperator.``||``, trE c)
    | S.Conditional (a, b, c) ->
        Conditional (trE a, trE b, trE c)
    | S.Constant a -> 
        let rN n =
            match System.Int64.FromString n with
            | Some i -> Int64 i
            | None ->
                match System.Double.FromString n with
                | Some i -> Double i
                | None -> raise RecognitionError
        match a with
        | S.False -> Bool false
        | S.Null -> Null
        | S.Number n -> rN n
        | S.String x -> String x
        | S.True -> Bool true
        |> Value
    | S.Lambda (a, b, c) ->
       match a with
       | None ->
            let vars = b |> List.map (fun v -> Id.New v)
            let env = env.WithNewScope(Seq.zip b (vars |> Seq.map Var))
            let body =
                c
                |> List.map (function
                    | S.Action s -> s
                    | _ -> raise RecognitionError)
                |> S.Block
            Function (
                vars,
                transformStatement env body
            )
       | _ -> failwith "TODO" 
    | S.New (a, b) -> New(trE a, List.map trE b)
    | S.NewArray a -> NewArray (a |> List.map (function Some i -> trE i | _ -> Undefined))
    | S.NewObject a -> Object(a |> List.map (fun (b, c) -> b, trE c))
    | S.NewRegex a -> 
        let closingSlash = a.LastIndexOf '/'
        let flags = a.[closingSlash + 1 ..] |> Seq.map (string >> String >> Value) |> List.ofSeq
        New (globalAccess ["RegExp"], Value (String a.[1 .. closingSlash - 1]) :: flags)
    | S.Postfix (a, b) ->
        match b with
        | S.PostfixOperator.``++`` -> MutatingUnary(MutatingUnaryOperator.``()++``, trE a)
        | S.PostfixOperator.``--`` -> MutatingUnary(MutatingUnaryOperator.``()--``, trE a)
    | S.This -> This
    | S.Unary (a, b) ->
        match a with
        | S.UnaryOperator.``!`` -> Unary(UnaryOperator.``!``, trE b)
        | S.UnaryOperator.``+`` -> Unary(UnaryOperator.``+``, trE b)
        | S.UnaryOperator.``++`` -> MutatingUnary(MutatingUnaryOperator.``++()``, trE b)
        | S.UnaryOperator.``-`` -> Unary(UnaryOperator.``-``, trE b)
        | S.UnaryOperator.``--`` -> MutatingUnary(MutatingUnaryOperator.``--()``, trE b)
        | S.UnaryOperator.delete -> MutatingUnary(MutatingUnaryOperator.delete, trE b)
        | S.UnaryOperator.typeof -> Unary(UnaryOperator.typeof, trE b)
        | S.UnaryOperator.``void`` -> Unary(UnaryOperator.``void``, trE b)
        | S.UnaryOperator.``~`` -> Unary(UnaryOperator.``~``, trE b)
    | S.Var a ->
        match a with
        | "$global" -> globalAccess []
        | "$wsruntime" -> globalAccess ["IntelliFactory"; "Runtime"]
        | _ ->
        match env.TryFindVar a with
        | Some e -> e
        | None -> globalAccess [a]
    | e ->     
        failwithf "Failed to recognize: %A" e
//    | S.Postfix (a, b) ->
//        match b with
//        | S.PostfixOperator.``++`` -> 
                                                                       
and transformStatement (env: Environment) (statement: S.Statement) =
    let inline trE e = transformExpression env e
    let inline trS s = transformStatement env s
    match statement with
    | S.Block a ->
        Block (a |> List.map trS)
    | S.Break a   -> Break (a |> Option.map (fun l -> env.Labels.[l]))
    | S.Continue a -> Continue (a |> Option.map (fun l -> env.Labels.[l]))
    | S.Debugger -> failwith "TODO"
    | S.Do (a, b) -> DoWhile (trS a, trE b)
    | S.Empty -> Empty
    | S.For (a, b, c, d) -> 
        For (Option.map trE a, Option.map trE b, Option.map trE c, trS d) // TODO: var declarations (?)
    | S.ForIn (a, b, c) -> failwith "TODO" //
    | S.ForVarIn (a, b, c, d) -> 
        match b with
        | Some b -> failwith "TODO"
        | _ ->
        let v = env.NewVar a
        Statements [
            VarDeclaration (v, Undefined)
            ForIn(v, trE c, trS d)
        ]
    | S.ForVars (a, b, c, d) -> 
        let decls =
            a |> List.map (fun (i, v) ->
                let id = env.NewVar i
                VarDeclaration(id, match v with Some v -> trE v | _ -> Undefined)
            )
        Statements (
            decls @
            [
                For (None, Option.map trE b, Option.map trE c, trS d)
            ]    
        )
    | S.If (a, b, c) -> If (trE a, trS b, trS c)
    | S.Ignore a -> ExprStatement (trE a)    
    | S.Labelled (a, b)  -> failwith "TODO"
    | S.Return a -> Return (match a with Some v -> trE v | _ -> Undefined)
    | S.Switch (a, b)    -> failwith "TODO"
    | S.Throw a -> Throw (trE a)
    | S.TryFinally (a, b) -> TryFinally (trS a, trS b)
    | S.TryWith (a, b, c, d)   -> failwith "TODO"
    | S.Vars a ->
        match a with
        | [var, value] -> 
            VarDeclaration (env.NewVar var, match value with Some v -> trE v | None -> Undefined)
        | _ -> 
            Statements (a |> List.map (fun (var, value) -> VarDeclaration (env.NewVar var, match value with Some v -> trE v | None -> Undefined)))
    | S.While (a, b) -> While (trE a, trS b)
    | S.With (a, b) -> failwith "TODO"

let createInline thisArg args inlineString =        
    let s = 
        inlineString 
        |> WebSharper.Core.JavaScript.Parser.Source.FromString
    let b =
        try
            s
            |> WebSharper.Core.JavaScript.Parser.ParseExpression 
            |> transformExpression (Environment.New(thisArg, false, args))
        with _ ->
            s
            |> WebSharper.Core.JavaScript.Parser.ParseProgram
            |> List.map (function S.Action a -> a | _ -> failwith "TODO: function declarations in Inline" )
            |> S.Block
            |> transformStatement (Environment.New(thisArg, false, args))
            |> StatementExpr
    makeExprInline (Option.toList thisArg @ args) b

let parseDirect thisArg args jsString =
    let s = 
        jsString 
        |> WebSharper.Core.JavaScript.Parser.Source.FromString
    let body =
        try
            s
            |> WebSharper.Core.JavaScript.Parser.ParseExpression 
            |> transformExpression (Environment.New(thisArg, true, args))
            |> Return 
        with _ ->
            s
            |> WebSharper.Core.JavaScript.Parser.ParseProgram
            |> List.map (
                function 
                | S.Action a -> a 
                | S.Function (id, args, body) -> S.Vars [ id, Some (S.Lambda(None, args, body)) ]
            )
            |> S.Block
            |> transformStatement (Environment.New(thisArg, true, args))
    Function(args, body)