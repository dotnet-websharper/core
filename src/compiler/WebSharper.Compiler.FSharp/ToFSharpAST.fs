module internal WebSharper.Compiler.FSharp.ToFSharpAST

//open System.Runtime.CompilerServices

open Microsoft.FSharp.Compiler.SourceCodeServices
 
open WebSharper.Core.AST
            
type Environment =
    {
        Vars : System.Collections.Generic.Dictionary<FSharpMemberOrFunctionOrValue, Id>
        TParams : Map<string, int>
        MatchVars : option<Id * Id>
    }
    static member Empty =
        { 
            Vars = System.Collections.Generic.Dictionary() 
            TParams = Map.empty
            MatchVars = None
        }

    static member New(vars, tparams) = 
        let d = System.Collections.Generic.Dictionary()
        for k, v in vars do d.Add(k, v)
        { 
            Vars = d 
            TParams = tparams |> Seq.mapi (fun i p -> p, i) |> Map.ofSeq
            MatchVars = None
        }

    member this.WithTParams tparams =
        if List.isEmpty tparams then this else
        { this with 
            TParams = 
                ((this.TParams, this.TParams.Count), tparams) 
                ||> List.fold (fun (m, i) p -> m |> Map.add p i, i + 1) 
                |> fst
        }

    member this.WithVar (i: Id, v: FSharpMemberOrFunctionOrValue) =
        if this.Vars.ContainsKey v then 
            ()// TODO: investigate
        else    
            this.Vars.Add(v, i)
        this

    member this.LookupVar (v: FSharpMemberOrFunctionOrValue) =
        this.Vars.[v]

let getTypeDefinition (td: FSharpEntity) =
    if td.IsArrayType then
        Hashed {
            Assembly = "mscorlib"
            FullName = "System.Array`1"
        }
    else
    let rec getOrigDef (td: FSharpEntity) =
        if td.IsFSharpAbbreviation then getOrigDef td.AbbreviatedType.TypeDefinition else td 
    let td = getOrigDef td
    if td.FullName = "Microsoft.FSharp.Core.LanguagePrimitives.IntrinsicFunctions" then
        () 
    Hashed {
        Assembly = td.Assembly.SimpleName //td.Assembly.FileName |> Option.toObj
        FullName = try td.QualifiedName.Split([|','|]).[0] with _ -> td.FullName
    }

let funcDef =
    Hashed {
        Assembly = "FSharp.Core"
        FullName = "Microsoft.FSharp.Core.FSharpFunc"
    }

let rec getType (tparams: Map<string, int>) (t: FSharpType) =
    if t.IsGenericParameter then
        GenericType tparams.[t.GenericParameter.Name]
    else
    let rec getOrigDef (t: FSharpType) =
        if t.IsAbbreviation then getOrigDef t.AbbreviatedType else t
    let t = getOrigDef t
    if t.IsTupleType then
        let tupleTypeDef i =
            Hashed {
                Assembly = "mscorlib"
                FullName = "System.Tuple`" + string i
            }
        let rec getTupleTD (args: _[]) =
            match args.Length with
            | i when i < 8 ->
                concreteType (tupleTypeDef i, args |> List.ofArray)
            | i ->
                concreteType (tupleTypeDef i, Seq.append (args.[.. 6]) (Seq.singleton (getTupleTD args.[7 ..])) |> List.ofSeq)    
        t.GenericArguments |> Seq.map (getType tparams) |> Array.ofSeq |> getTupleTD
    elif t.IsFunctionType then
        concreteType(funcDef, t.GenericArguments |> Seq.map (getType tparams) |> List.ofSeq)
    else
        concreteType (getTypeDefinition t.TypeDefinition, 
            t.GenericArguments |> Seq.map (getType tparams) |> List.ofSeq)

let removeUnitParam (ps: list<Type>) =
    match ps with 
    | [ ConcreteType { Entity = t } ] when t.Value.FullName.StartsWith "Microsoft.FSharp.Core.Unit" -> []
    | _ -> ps

let getMember (x : FSharpMemberOrFunctionOrValue) : Member =
    let name = x.CompiledName
    let tparams = 
        lazy
            Seq.append x.EnclosingEntity.GenericParameters x.GenericParameters
            |> Seq.mapi (fun i p -> p.Name, i) |> Seq.distinctBy fst |> Map.ofSeq
    let getParams() =
        // temporary hack
        try
            x.CurriedParameterGroups |> Seq.concat |> Seq.map (fun p -> getType tparams.Value p.Type) |> List.ofSeq |> removeUnitParam  
        with _ -> [] 
    match name with 
    | ".ctor" ->
        Member.Constructor <| Hashed {
            CtorParameters = getParams()
        }  
    | ".cctor" -> StaticConstructor
    | _ ->
        Method <| Hashed {
        MethodName = name
        Parameters = getParams()
        ReturnType = 
            try getType tparams.Value x.ReturnParameter.Type
            with _ -> 
                // hack
                GenericType 0
        Generics   = tparams.Value.Count - x.EnclosingEntity.GenericParameters.Count
    }

//let getMethod (x : FSharpMemberOrFunctionOrValue) =
//    let tparams = 
//        Seq.append x.EnclosingEntity.GenericParameters x.GenericParameters
//        |> Seq.mapi (fun i p -> p.Name, i) |> Map.ofSeq
//    Hashed {
//        MethodName = x.CompiledName
////        DefinedBy = getTypeDefinition x.EnclosingEntity
//        Parameters = x.CurriedParameterGroups |> Seq.concat |> Seq.map (fun p -> getType tparams p.Type) |> List.ofSeq
//        ReturnType = getType tparams x.ReturnParameter.Type
//        Generics = x.GenericParameters.Count
//    }

let rec transformExpression (env: Environment) (expr: FSharpExpr) =
    let inline tr x = transformExpression env x
    match expr with
    | BasicPatterns.Value(value) ->
        if value.IsModuleValueOrMember then
            ItemGet (Value (String "TODO: module access"), Value (String value.CompiledName))
        else
            env.LookupVar value |> Var
    | BasicPatterns.Lambda(arg, body) ->
        let i = Id(arg.DisplayName)
        Function([i], Return (body |> transformExpression (env.WithVar(i, arg).WithTParams(arg.GenericParameters |> Seq.map (fun p -> p.Name) |> List.ofSeq))))
    | BasicPatterns.Application(func, types, args) ->
        Seq.fold (fun f a -> Application(f, [tr a])) (tr func) args
    | BasicPatterns.Let((id, value), body) ->
        let i = Id(id.DisplayName)
        Let (i, tr value, body |> transformExpression (env.WithVar(i, id)))
    | BasicPatterns.LetRec(defs, body) ->
        let mutable env = env
        let ids = defs |> List.map (fun (id, _) ->
            let i = Id(id.DisplayName)
            env <- env.WithVar(i, id)    
            i      
        )
        LetRec (
            Seq.zip ids defs 
            |> Seq.map (fun (i, (_, value)) -> i, value |> transformExpression env) |> List.ofSeq, 
            body |> transformExpression env
        )
    | BasicPatterns.Call(this, meth, typeGenerics, methodGenerics, arguments) ->
        let td = getTypeDefinition meth.EnclosingEntity
        //let tparams = meth.GenericParameters |> Seq.mapi (fun i p -> p.Name, i) |> Map.ofSeq
 
        let t = concrete (td, typeGenerics |> List.map (getType env.TParams))
        let args = List.map tr arguments
        match getMember meth with
        | Method m -> 
            let mt = concrete (m, methodGenerics |> List.map (getType env.TParams))
            Call (Option.map tr this, t, mt, args)
        | Constructor c -> Ctor (t, c, args)
        | StaticConstructor -> CCtor t 
    | BasicPatterns.Sequential(first, second) ->
        match tr first, tr second with
        | Sequential f, Sequential s -> Sequential (f @ s)
        | f, Sequential s -> Sequential (f :: s)
        | Sequential f, s -> Sequential (f @ [s])
        | f, s -> Sequential [f; s]
    | BasicPatterns.Const (value, _) ->
        match value with
        | x when obj.ReferenceEquals(x, null) -> Null      
        | :? bool   as x -> Bool   x
        | :? byte   as x -> Byte   x
        | :? char   as x -> Char   x
        | :? double as x -> Double x
        | :? int    as x -> Int    x
        | :? int16  as x -> Int16  x
        | :? int64  as x -> Int64  x
        | :? sbyte  as x -> SByte  x
        | :? single as x -> Single x
        | :? string as x -> String x
        | :? uint16 as x -> UInt16 x
        | :? uint32 as x -> UInt32 x
        | :? uint64 as x -> UInt64 x
        | _ -> failwith "F# constant value not recognized: %A" value
        |> Value
    | BasicPatterns.IfThenElse (cond, then_, else_) ->
        Conditional(tr cond, tr then_, tr else_)    
    | BasicPatterns.NewObject (constructor_, typeGenerics, arguments) -> 
        let td = getTypeDefinition constructor_.EnclosingEntity
//        let tparams = constructor_.GenericParameters |> Seq.mapi (fun i p -> p.Name, i) |> Map.ofSeq
        let t = concrete (td, typeGenerics |> List.map (getType env.TParams))
        let args = List.map tr arguments
        match getMember constructor_ with
        | Constructor c -> Ctor (t, c, args)
        | _ -> failwith "constructor call expected"
    | BasicPatterns.TryFinally (body, final) ->
        let res = Id ()
        Sequential [
            StatementExpr (TryFinally(ExprStatement(NewVar(res, tr body)), ExprStatement (tr final)))
            Var res
        ]
    | BasicPatterns.TryWith (body, var, filter, e, with_) ->
        let err = Id e.DisplayName
        let res = Id ()
        Sequential [
            StatementExpr (
                TryWith(ExprStatement(NewVar(res, tr body)), 
                    Some err, 
                    (ExprStatement (transformExpression (env.WithVar(err, e)) with_))))
            Var res
        ]
    | BasicPatterns.NewArray (_, items) ->
        NewArray (items |> List.map tr)              
    | BasicPatterns.NewTuple (_, items) ->
        NewArray (items |> List.map tr)              
    // _ -> failwith "TODO"
    | BasicPatterns.WhileLoop (cond, body) ->
        StatementExpr(While(tr cond, ExprStatement (tr body)))
    | BasicPatterns.ValueSet (var, value) ->
        VarSet(env.LookupVar(var), tr value) 
    | BasicPatterns.TupleGet (_, i, tuple) ->
        ItemGet(tr tuple, Value (Int i))   
    | BasicPatterns.FastIntegerForLoop (start, end_, body, up) ->
        let i = Id "i"
        let j = Id "j"
        For (
            Some (Sequential [NewVar(i, tr start); NewVar (j, tr end_)]), 
            Some (if up then Binary(Var i, BinaryOperator.``<=``, Var j) else Binary(Var i, BinaryOperator.``>=``, Var j)), 
            Some (if up then MutatingUnary(MutatingUnaryOperator.``()++``, Var i)  else MutatingUnary(MutatingUnaryOperator.``()--``, Var i)), 
            ExprStatement (tr body)
        ) |> StatementExpr
    | BasicPatterns.TypeTest (typ, expr) ->
        TypeCheck (tr expr, getType Map.empty typ)
    | BasicPatterns.Coerce (typ, expr) ->
        tr expr // TODO: type check when possible
    | BasicPatterns.NewUnionCase (typ, case, exprs) ->
        let i = typ.TypeDefinition.UnionCases |> Seq.findIndex (fun c -> c.CompiledName = case.CompiledName)
        Object (
            ("$", Value (Int i)) ::
            (exprs |> List.mapi (fun j e -> "$" + string j, tr e)) 
        )
    | BasicPatterns.UnionCaseGet (expr, typ, case, field) ->
        let i = case.UnionCaseFields |> Seq.findIndex (fun f -> f.FullName = field.FullName)
        ItemGet(tr expr, Value (Int i))   
    | BasicPatterns.UnionCaseSet (expr, typ, case, field, value) ->
        let i = case.UnionCaseFields |> Seq.findIndex (fun f -> f.FullName = field.FullName)
        ItemSet(tr expr, Value (Int i), tr value)   
    | BasicPatterns.UnionCaseTest (expr, typ, case) ->
        let i = typ.TypeDefinition.UnionCases |> Seq.findIndex (fun c -> c.CompiledName = case.CompiledName)
        Binary(ItemGet(tr expr, Value (String "$")), BinaryOperator.``==``, Value (Int i))
    | BasicPatterns.UnionCaseTag (expr, typ) ->
        ItemGet(tr expr, Value (String "$"))
    | BasicPatterns.NewRecord (typ, items) ->
        Seq.zip 
            (typ.TypeDefinition.FSharpFields |> Seq.map (fun f -> f.Name))
            (items |> Seq.map tr)
        |> List.ofSeq |> Object
    | BasicPatterns.DecisionTree (matchValue, cases) ->
        let i = Id "matchIndex"
        let c = Id "matchCaptures"
//        let value = tr matchValue
//        let caseExprs = cases |> List.map (snd >> tr) 
        let r = Id "matchResult"
        let captures = cases |> Seq.collect fst |> List.ofSeq
        let mutable env = { env with MatchVars = Some (i, c) }
        let mutable captVars = []
        for v in captures do
            let i = Id v.LogicalName
            env <- env.WithVar(i, v)
            captVars <- i :: captVars
        let env = env
        let inline tr x = transformExpression env x
        Sequential (
            (captVars |> List.map (fun v -> NewVar(v, Undefined)))
            @ [
                NewVar(i, Undefined)
                NewVar(c, Undefined)
                NewVar(r, Undefined)
                tr matchValue
                StatementExpr(
                    Switch(
                        Var i, 
                        cases |> List.mapi (fun j (_, e) -> Some (Value (Int j)), ExprStatement(VarSet(r, tr e)))
                    )
                )
                Var r
            ]
        )
    | BasicPatterns.DecisionTreeSuccess (index, results) ->
        let i, c = env.MatchVars.Value
        Sequential [
            yield VarSet (i, Value (Int index))
            match results |> List.map tr with
            | [] -> ()
            | matchCaptures -> yield VarSet (c, NewArray matchCaptures) 
        ]
    | BasicPatterns.ThisValue (typ) ->
        This
    | BasicPatterns.FSharpFieldGet (thisOpt, typ, field) ->
        match thisOpt with
        | Some this ->
            ItemGet(tr this, Value (String field.Name))  // TODO : field renames 
        | _ -> failwith "TODO"
    | BasicPatterns.FSharpFieldSet (thisOpt, typ, field, value) ->
        match thisOpt with
        | Some this ->
            ItemSet(tr this, Value (String field.Name), tr value)  // TODO : field renames 
        | _ -> failwith "TODO"
    | BasicPatterns.AddressOf expr ->
        let value = Id "v"
        match tr expr with
        | Var v as e ->
            Object [
                "get", (Function ([], Return e))
                "set", (Function ([value], ExprStatement (VarSet(v, Var value))))
            ]
        | ItemGet(o, i) as e ->
            Object [
                "get", (Function ([], Return e))
                "set", (Function ([value], ExprStatement (ItemSet(o, i, Var value))))
            ]
    | BasicPatterns.AddressSet (addr, value) ->
        Application(ItemGet(tr addr, Value (String "get")), [tr value])    
    | BasicPatterns.ObjectExpr (typ, expr, overrides, interfaces) ->
        failwith "TODO"
    | BasicPatterns.DefaultValue typ ->
        Undefined
//        match typ.TypeDefinition with
//        | "System.Int32" -> Value (Int 0) // TODO more
//        | _ -> Value Null
    | BasicPatterns.NewDelegate (typ, arg) ->
        tr arg     
        //failwith "TODO"
    | BasicPatterns.TypeLambda          _ -> failwith "TODO"
    | BasicPatterns.Quote               _ -> failwith "TODO"
    | BasicPatterns.BaseValue           _ -> failwith "TODO"
    | BasicPatterns.ILAsm               _ -> failwith "TODO"
    | BasicPatterns.ILFieldGet          _ -> failwith "TODO"
    | BasicPatterns.ILFieldSet          _ -> failwith "TODO"
    | BasicPatterns.TraitCall           _ -> failwith "TODO"
    | _ -> failwith "transformExpression: Expression not recognized"
