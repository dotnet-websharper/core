module WebSharper.Compiler.Common.Recognize

open WebSharper.Core
open WebSharper.Core.AST

module S = WebSharper.Core.JavaScript.Syntax
type SB = WebSharper.Core.JavaScript.Syntax.BinaryOperator

type Environment =
    {
        Vars : Map<string, Expression>
        Labels : Map<string, Id>
        This : option<Id>
    }
    static member New(vars) =
        {
            Vars =
                vars |> Seq.mapi (fun i n ->                    
                    let h = Hole i
                    [ 
                        "$" + string i, h
                        "$" + n, h  
                    ] 
                ) |> Seq.concat |> Map.ofSeq
            Labels = Map.empty
            This = None
        }

    static member NewDirect(args) =
        {
            Vars =
                args |> Seq.mapi (fun i (a: Id) ->                    
                    [ 
                        "$" + string i, Var a
                        "$" + a.Name.Value, Var a  
                    ] 
                ) |> Seq.concat |> Map.ofSeq
            Labels = Map.empty
            This = None
        }

    member this.WithVar(name) =
        let v = Id name
        { this with Vars = this.Vars |> Map.add name (Var v) }, v

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
            | a -> failwith "Not implemented yet"
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
//            let this = Id()
            let vars = b |> List.map (fun v -> Id v)
            let env =
                { env with
                    Vars = (env.Vars, Seq.zip b vars) ||> Seq.fold (fun env (n, v) -> Map.add n (Var v) env)
                }
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
    | S.NewRegex a -> New (globalAccess ["Regex"], [Value (String a)])
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
        match env.Vars.TryFind a with
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
    | S.Block a -> Block (a |> List.map trS)
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
        let env, v = env.WithVar a
        ForIn(v, trE c, transformStatement env d)
    | S.ForVars (a, b, c, d) -> 
        let decls, env =
            List.mapFold (fun (e: Environment) (i, v) ->
                let e, id = e.WithVar i
                VarDeclaration(id, match v with Some v -> trE v | _ -> Undefined), e
            ) env a
        let inline trE e = transformExpression env e
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
        | [var, value] -> VarDeclaration (Id var, match value with Some v -> trE v | None -> Undefined)
        | _ -> Statements (a |> List.map (fun (var, value) -> VarDeclaration (Id var, match value with Some v -> trE v | None -> Undefined)))
    | S.While (a, b) -> While (trE a, trS b)
    | S.With (a, b) -> failwith "TODO"

let createInline vars inlineString =        
    let s = 
        inlineString 
        |> WebSharper.Core.JavaScript.Parser.Source.FromString
    try
        s
        |> WebSharper.Core.JavaScript.Parser.ParseExpression 
        |> transformExpression (Environment.New(vars)) 
    with _ ->
        s
        |> WebSharper.Core.JavaScript.Parser.ParseProgram
        |> List.map (function S.Action a -> a | _ -> failwith "TODO: function declarations in Imnline" )
        |> S.Block
        |> transformStatement (Environment.New(vars))
        |> StatementExpr

let parseDirect args jsString =
    let s = 
        jsString 
        |> WebSharper.Core.JavaScript.Parser.Source.FromString
    try
        s
        |> WebSharper.Core.JavaScript.Parser.ParseExpression 
        |> transformExpression (Environment.NewDirect(args))
        |> Return 
    with _ ->
        s
        |> WebSharper.Core.JavaScript.Parser.ParseProgram
        |> List.map (function S.Action a -> a | _ -> failwith "TODO: function declarations in Direct" )
        |> S.Block
        |> transformStatement (Environment.NewDirect(args))
