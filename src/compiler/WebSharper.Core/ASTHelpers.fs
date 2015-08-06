[<AutoOpen>]
module WebSharper.Core.AST.ASTHelpers

let concrete (e, g) =
    {
        Entity = e
        Generics = g
    }

let concreteType (td, g) = ConcreteType (concrete (td, g))

let ignoreExprSourcePos expr =
    match expr with
    | ExprSourcePos (_, e) -> e
    | _ -> expr

let (|IgnoreExprSourcePos|) expr = ignoreExprSourcePos expr

let withSourcePosOfExpr sourceExpr expr =
    match sourceExpr with      
    | ExprSourcePos (pos, _) -> ExprSourcePos (pos, expr)
    | _ -> expr

let ignoreStatementSourcePos expr =
    match expr with
    | StatementSourcePos (_, e) -> e
    | _ -> expr

let (|IgnoreStatementSourcePos|) expr = ignoreStatementSourcePos expr

let withSourcePosOfStatement sourceStatement statement =
    match sourceStatement with      
    | ExprSourcePos (pos, _) -> ExprSourcePos (pos, statement)
    | _ -> statement
    
let combineStatements statements =
    match statements with
    | [] -> Empty
    | [s] -> s
    | _ -> Block statements

let getConstrantValue (value: obj) =
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
 

let rec breakExpr expr =
    let inline br x = breakExpr x
    let brL l =
        let bb = l |> List.map br
        if bb |> List.forall Option.isNone then None
        else
            let rec bL br (accSt, accE) ol bl =
                match ol, bl with
                | [], _ -> accSt, accE
                | a :: oRest, None :: bRest ->
                    if br then
                        let aV = Id()
                        bL true (VarDeclaration (aV, a) :: accSt, Var aV :: accE) oRest bRest
                    else
                        bL false (accSt, a :: accE) oRest bRest
                | _ :: oRest, Some (aSt, aE) :: bRest ->
                    bL true (aSt @ accSt, aE :: accE) oRest bRest
            bL false ([], []) (List.rev l) (List.rev bb) |> Some
    
    let comb2 f a b =
        match brL [a; b] with
        | Some (st, [aE; bE]) -> Some (st, f (aE, bE))
        | None -> None
        | _ -> failwith "impossible"
    let comb3 f a b c =
        match brL [a; b; c] with
        | Some (st, [aE; bE; cE]) -> Some (st, f (aE, bE, cE))
        | None -> None
        | _ -> failwith "impossible"
    
    match expr with
    | Undefined
    | This
    | Var _
    | Value _ -> None 
    | Application (a, b) -> 
        brL (a :: b)
        |> Option.map (fun (st, aE :: bE) -> st, Application (aE, bE))
//    | Access (a, b) ->
//        breakList [a; b]
//        |> Option.map (fun (st, [aE; bE]) -> st, Access (aE, bE))
    | Function _ -> None
    | VarSet (a, b) ->
        br b
        |> Option.map (fun (bSt, bE) -> bSt, VarSet (a, bE))
    | Sequential a ->
        brL a
        |> Option.map (fun (st, l) -> st, Sequential l)
    | NewArray a ->
        brL a
        |> Option.map (fun (st, l) -> st, NewArray l)
    | Conditional (a, b, c) ->
        comb3 Conditional a b c
    | ItemGet (a, b) ->
        comb2 ItemGet a b
    | ItemSet (a, b, c) ->
        comb3 ItemSet a b c
    | Binary (a, b, c) ->
        comb2 (fun (aE, cE) -> Binary(aE, b, cE)) a c
    | MutatingBinary (a, b, c) -> 
        comb2 (fun (aE, cE) -> MutatingBinary(aE, b, cE)) a c
    | Unary (a, b) ->
        br b
        |> Option.map (fun (bSt, bE) -> bSt, Unary (a, bE))
    | MutatingUnary (a, b) ->
        br b
        |> Option.map (fun (bSt, bE) -> bSt, MutatingUnary (a, bE))
    | ExprSourcePos (a, b) -> 
        br b 
        |> Option.map (fun (st, bB) -> st, ExprSourcePos(a, bB))
    | StatementExpr st ->
        Some ([ st ], Undefined)
    | Call(a, b, c, d) ->
        brL (Option.toList a @ d)
        |> Option.map (fun (st, l) ->
            st,
            if Option.isSome a then
                Call (Some l.Head, b, c, l.Tail)
            else Call (None, b, c, l)
        )
    | Ctor(a, b, c) ->
        brL c
        |> Option.map (fun (st, l) -> st, Ctor(a, b, l))
    | CCtor(_) -> None
    | FieldGet(_, _, _) -> failwith "Not implemented yet"
    | FieldSet(_, _, _, _) -> failwith "Not implemented yet"
    | Let(a, b, c) ->
        match br b, br c with
        | None, None -> None
        | Some (bSt, bE), None ->
            Some (bSt, Let(a, bE, b))
        | None, Some (cSt, cE) ->
            Some (VarDeclaration (a, b) :: cSt, cE)
        | Some (aSt, aE), Some (bSt, bE) ->
            Some (aSt @ (VarDeclaration(a, aE) :: bSt), bE)
    | NewVar(a, b) ->
        br b
        |> Option.map (fun (bSt, bE) -> bSt, NewVar (a, bE))
    | Object a ->
        let names, values = List.unzip a
        brL values
        |> Option.map (fun (st, l) -> st, Object (List.zip names l)) 
    | Coalesce(_, _, _) -> failwith "Not implemented yet"
    | TypeCheck(a, b) ->
        br a
        |> Option.map (fun (aSt, aE) -> aSt, TypeCheck (aE, b))
    | LetRec(a, b) ->
        failwith "Not implemented yet"
//        None // TODO
    | Await(_) -> failwith "Not implemented yet"
    | New(a, b) -> 
        brL (a :: b)
        |> Option.map (fun (st, aE :: bE) -> st, New (aE, bE))
    | NamedParameter(_, _) -> failwith "Not implemented yet"
    | RefOrOutParameter(_) -> failwith "Not implemented yet"
    | GlobalAccess _ -> None
    | Hole _ -> None

let rec private breakSt statement =
    let inline brE x = breakExpr x
    let inline brS x = breakSt x
    let inline combine x = x |> List.ofSeq |> combineStatements
    let comb2 f a b =
        match brS a, brS b with
        | Some aB, Some bB -> f (combine aB) (combine bB) |> Seq.singleton |> Some
        | Some aB, None -> f (combine aB) b |> Seq.singleton |> Some
        | None, Some bB -> f a (combine bB) |> Seq.singleton |> Some
        | _ -> None
    match statement with
    | Empty
    | Break _ 
    | Continue _ -> None
    | ExprStatement a ->
        brE a |> Option.map (fun (st, aB) -> Seq.append st (Seq.singleton (ExprStatement aB)))
    | Return a ->
        brE a |> Option.map (fun (st, aB) -> Seq.append st (Seq.singleton (Return aB)))
    | Block a ->
        let aB = a |> List.map brS 
        if aB |> List.forall Option.isNone then None
        else
            Seq.map2 (fun o b ->
                match b with
                | Some bs -> bs
                | _ -> Seq.singleton o
            ) a aB |> Seq.concat |> List.ofSeq |> Block |> Seq.singleton |> Some
    | Labeled (a, b) ->
        brS b
        |> Option.map (fun bB -> Labeled (a, combine bB) |> Seq.singleton)
    | VarDeclaration (a, b) ->
        brE b
        |> Option.map (fun (st, bB) -> Seq.append st (Seq.singleton (VarDeclaration (a, bB))))
    | While (a, b) ->
        match brE a with
        | Some (st, aB) ->
            let ok = Id()
            [
                VarDeclaration (ok, Value (Bool true))
                While (Var ok, 
                    Block (
                        st @ [ 
                            If (VarSet (ok, aB), 
                                match brS b with
                                | None -> b
                                | Some bB -> combine bB
                            , Empty)  
                        ]
                    )
                ) 
            ] |> Seq.ofList |> Some
        | None -> 
            brS b |> Option.map (fun bB -> While (a, combine bB) |> Seq.singleton)
    | StatementSourcePos (a, b) ->
        brS b |> Option.map (fun bB -> StatementSourcePos (a, combine bB) |> Seq.singleton)
    | DoWhile(a, b) -> 
        match brE b with
        | Some (st, bB) ->
            DoWhile (
                match brS a with
                | None -> combine (Seq.append [a] st)
                | Some aB -> combine (Seq.append aB st)
                , bB    
            ) |> Seq.singleton |> Some
        | None ->
            brS a |> Option.map (fun aB -> DoWhile (combine aB, b) |> Seq.singleton)
    | For(a, b, c, d) ->       
        let withoutInit =
            While (
                match b with Some b -> b | _ -> Value (Bool true) 
                ,
                match c with Some c -> Statements [d; ExprStatement c] | _ -> d
            )
        match a with
        | Some a -> Statements [ExprStatement a; withoutInit]
        | _ -> withoutInit
        |> brS
//        let aB = Option.map brE a
//        let bB = Option.map brE b
//        let cB = Option.map brE c
//        let dB = brS d
//        match aB, bB, cB, dB with
//        | None, None, None, None -> None
//        | _ ->
//            None
        // TODO
        //failwith "Not implemented yet"
    | Switch(a, b) -> 
        let brA = brE a
        let brCases = b |> List.map (fun (c, d) -> Option.bind brE c, brS d)
        if Option.isNone brA && brCases |> List.forall (fun (c, d) -> Option.isNone c && Option.isNone d) then 
            None
        else
//        None // TODO
            if brCases |> List.exists (fst >> Option.isSome) then
                failwith "Not implemented yet"
            else
                let cases = List.map2 (fun (c, d) (_, dB) -> c, match dB with Some dB -> combine dB | _ -> d) b brCases
                match brA with
                | Some (st, aE) ->
                    Seq.append st (Seq.singleton (Switch(aE, cases)))
                | _ ->
                    Seq.singleton (Switch(a, cases))
                |> Some 
    | If(a, b, c) ->
        match brE a, brS b, brS c with
        | None, None, None -> None
        | Some (st, aB), bB, cB ->
            Some (Seq.append st (Seq.singleton (If (aB, (match bB with Some bB -> combine bB | _ -> b), (match bB with Some bB -> combine bB | _ -> b)))))
        | None, bB, cB ->
            Some (Seq.singleton (If (a, (match bB with Some bB -> combine bB | _ -> b), (match bB with Some bB -> combine bB | _ -> b))))     
        //failwith "Not implemented yet"
    | Throw(a) -> 
        brE a
        |> Option.map (fun (st, aB) -> Seq.append st (Seq.singleton (Throw aB)))
    | TryWith (a, b, c) ->
        comb2 (fun ar cr -> TryWith (ar, b, cr)) a c
    | TryFinally (a, b) ->           
        comb2 (fun ar br -> TryFinally (ar, br)) a b
    | Statements a ->
        Some (Seq.ofList a)
    | ForIn(a, b, c) -> 
        match brE b, brS c with
        | None, None -> None
        | bB, cB ->
            let c = match cB with Some cB -> combine cB | _ -> c
            match bB with
            | None -> Seq.singleton (ForIn (a, b, c))
            | Some (st, bB) -> Seq.append st (Seq.singleton (ForIn (a, bB, c)))
            |> Some
    | Yield(a) -> 
        brE a |> Option.map (fun (st, aB) -> Seq.append st (Seq.singleton (Yield aB))) // TODO yield breakup
    | Goto(a) -> failwith "Not implemented yet"
    | CSharpSwitch(_, _) -> failwith "Not implemented yet"
    | GotoCase(_) -> failwith "Not implemented yet"

let breakStatement statement =
    match breakSt statement with
    | Some s -> Block (List.ofSeq s)
    | None -> statement 

type Substitution(args) =
    inherit Transformer()
    
    let args = Array.ofList args

    override this.TransformHole i = args.[i]
   
open WebSharper.Core

let globalAccess a = GlobalAccess (Hashed (List.rev a))
  
let runtimeCtor = GlobalAccess (Hashed ["Ctor"; "Runtime"]) 
let runtimeCctor = GlobalAccess (Hashed ["Cctor"; "Runtime"])

module Resolve =
    let newName (name: string) =
        match name.LastIndexOf '$' with
        | -1 -> name + "$1"
        | i -> 
            match System.Int32.TryParse name.[i + 1 ..] with
            | true, n -> name.[.. i - 1] + "$" + string (n + 1)
            | _ -> name + "$1"

    open System.Collections.Generic

    type Package =
        | Module of Dictionary<string, option<Package>>

    let rec getModule existing name (d: IDictionary<string, _>) =
        match d.TryGetValue name with
        | true, Some (Module m) when existing -> name, m
        | true, _ -> getModule existing (newName name) d  
        | _ -> 
            let m = Dictionary()
            d.Add(name, Some (Module m))
            name, m

    let classAddress root address =
        match List.rev address with
        | c :: rest ->
            let restAddr, restD = 
                ([], root) |> List.foldBack (fun name (a, d) -> 
                    let n, o = getModule true name d
                    n :: a, o
                ) rest
            let n, o = getModule false c restD 
            Hashed (n :: restAddr), o
        | _ ->
            failwith "empty address"
    
    let rec addRenamed name (s: ISet<string>) =
        if s.Add name then name else addRenamed (newName name) s

    let rec addChild name child (d: IDictionary<string, _>) =
        match d.TryGetValue name with
        | true, _ -> addChild (newName name) child d
        | _ -> 
            d.Add(name, child)
            name 
