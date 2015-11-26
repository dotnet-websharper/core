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
 
type IdReplace(fromId, toId) =
    inherit Transformer()
    
    override this.TransformId i =
        if i = fromId then toId else i

type BrokenExpr =
    {
        VarDeft : list<Id>
        Statements : list<Statement>
        Result : Expression
    }

let hasNoStatements (be: BrokenExpr) = List.isEmpty be.Statements

let rec breakExpr expr : BrokenExpr =
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
                        match a with
                        | Undefined | Value _ 
                        | Var _ ->
                            bL true (accSt, a :: accE) oRest bRest
                        | _ ->
                            let aV = Id.New ()
                            bL true (VarDeclaration (aV, a) :: accSt, Var aV :: accE) oRest bRest
                    else
                        bL false (accSt, a :: accE) oRest bRest
                | _ :: oRest, Some (aSt, aE) :: bRest ->
                    match aE with
                    | Undefined | Value _ 
                    | Var _ ->
                        bL true (aSt @ accSt, aE :: accE) oRest bRest
                    | _ ->
                        let aV = Id.New()
                        bL true (aSt @ VarDeclaration (aV, aE) :: accSt, Var aV :: accE) oRest bRest
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
    // generated for disposing iterators
    | Application (ItemGet(Let (x, Var y, Var x2), i), b) when x = x2 ->
        Some ([], Application(ItemGet(Var y, i), b))   
    | Application (ItemGet(a, b), c) ->
        brL (a :: b :: c)
        |> Option.map (fun (st, aE :: bE :: cE) -> st, Application (ItemGet(aE, bE), cE))
    | Application (a, b) -> 
//        match a with
//        | ItemGet(x, Value (String "System-IDisposable-Dispose")) ->
//            let brA = br a
//            ()
//        | _ -> ()

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
        |> Option.map (fun (st, l) -> st, 
            match List.rev l with
            | [] -> failwith "empty Sequential"
            | [ e ] -> e
            | h :: t -> h :: (t |> List.filter (function Undefined | Value _ | Var _ -> false | _ -> true)) |> List.rev |> Sequential
        )
    | NewArray a ->
        brL a
        |> Option.map (fun (st, l) -> st, NewArray l)
    | Conditional (a, b, c) ->
        match br a, br b, br c with   
        | None, None, None -> None
        | Some (aSt, aE), None, None ->
            Some (aSt, Conditional (aE, b, c))
        | aBr, bBr, cBr ->
            let res = Id.New ()
            let setRes x brX =
                match brX with
                | None -> ExprStatement(VarSet(res, x))
                | Some (xSt, xE) -> Block (xSt @ [ ExprStatement(VarSet(res, xE)) ]) 
            Some (
                [
                    yield VarDeclaration(res, Undefined)
                    match aBr with
                    | None ->
                        yield If (a, setRes b bBr, setRes c cBr)
                    | Some (aSt, aE) ->
                        yield! aSt
                        yield If (aE, setRes b bBr, setRes c cBr)
                ], Var res        
            )
    | ItemGet (a, b) ->
        comb2 ItemGet a b
    | ItemSet (a, b, c) ->
        comb3 ItemSet a b c
    | Binary (a, b, c) ->
        match b with
        | BinaryOperator.``&&`` ->
            Conditional (a, c, Value (Bool false)) |> br
        | BinaryOperator.``||`` ->
            Conditional (a, Value (Bool true), c) |> br
        | _ ->
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
    | NewObject (a, b) ->
        br b |> Option.map (fun (bSt, bE) -> bSt, NewObject (a, bE))
    | Self -> None
    | FieldGet(_, _, _) -> failwith "Not implemented yet"
    | FieldSet(_, _, _, _) -> failwith "Not implemented yet"
//    | Let(a, Var b, c) ->
//        let ctr = IdReplace(a, b).TransformExpression(c)
//        match br ctr with
//        | Some _ as bC -> bC
//        | _ -> Some ([], ctr) 
    | Let(a, b, c) ->
        match br b, br c with
        | None, None -> 
//            None
            Some ([ VarDeclaration(a, b) ], c)
        | Some (bSt, bE), None ->
            Some (bSt @ [ VarDeclaration(a, bE) ], c)
//            Some (bSt, Let(a, bE, c))
        | None, Some (cSt, cE) ->
            Some (VarDeclaration (a, b) :: cSt, cE)
        | Some (bSt, bE), Some (cSt, cE) ->
            Some (bSt @ (VarDeclaration(a, bE) :: cSt), cE)
    | NewVar(a, b) ->
//        br b
//        |> Option.map (fun (bSt, bE) -> bSt, NewVar (a, bE))
        match br b with
        | None ->
            Some ([ VarDeclaration(a, b) ], Var a)   
        | Some (bSt, bE) ->
            Some (bSt @ [ VarDeclaration(a, bE) ], Var a)   

    | WithVars (a, b) ->
        match br b with
        | None ->
            Some (a |> List.map (fun v -> VarDeclaration(v, Undefined)), b)
        | Some (bSt, bE) ->
            Some ((a |> List.map (fun v -> VarDeclaration(v, Undefined))) @ bSt, bE)
    | Object a ->
        let names, values = List.unzip a
        brL values
        |> Option.map (fun (st, l) -> st, Object (List.zip names l)) 
    | Coalesce(_, _, _) -> failwith "Not implemented yet"
    | TypeCheck(a, b) ->
        br a
        |> Option.map (fun (aSt, aE) -> aSt, TypeCheck (aE, b))
    | LetRec _ -> //(a, b) ->
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

and private breakSt statement : seq<Statement> =
    let inline brE x = breakExpr x
    let inline brS x = breakSt x
    let inline combine x = x |> List.ofSeq |> combineStatements
    let inline combineOpt x xB = match xB with Some xB -> combine xB | _ -> x
//    let comb2 f a b =
//        match brS a, brS b with
//        | Some aB, Some bB -> f (combine aB) (combine bB) |> Seq.singleton |> Some
//        | Some aB, None -> f (combine aB) b |> Seq.singleton |> Some
//        | None, Some bB -> f a (combine bB) |> Seq.singleton |> Some
//        | _ -> None
    match statement with
    | Empty
    | Break _ 
    | Continue _ -> Seq.singleton statement
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
            let ok = Id.New "ok"
            st @ [
                VarDeclaration (ok, aB)
                While (Var ok, 
                    Block [
                        yield combineOpt b (brS b) 
                        for s in st do
                            match s with
                            | VarDeclaration (v, x) -> yield ExprStatement(VarSet(v, x))
                            | _ -> yield s 
                        yield ExprStatement(VarSet(ok, aB))
                    ]
                ) 
            ] |> Seq.ofList |> Some
//            let ok = Id.New "ok"
//            [
//                VarDeclaration (ok, Value (Bool true))
//                While (Var ok, 
//                    Block (
//                        st @ [ 
//                            If (VarSet (ok, aB), 
//                                match brS b with
//                                | None -> b
//                                | Some bB -> combine bB
//                            , Empty)  
//                        ]
//                    )
//                ) 
//            ] |> Seq.ofList |> Some
        | None -> 
            brS b |> Option.map (fun bB -> While (a, combine bB) |> Seq.singleton)
    | StatementSourcePos (a, b) ->
        brS b |> Option.map (fun bB -> StatementSourcePos (a, combine bB) |> Seq.singleton)
    | DoWhile(a, b) -> 
        failwith "TODO"
        // this is wrong because of VarDeclarations repeated
//        match brE b with
//        | Some (st, bB) ->
//            DoWhile (
//                match brS a with
//                | None -> combine (Seq.append [a] st)
//                | Some aB -> combine (Seq.append aB st)
//                , bB    
//            ) |> Seq.singleton |> Some
//        | None ->
//            brS a |> Option.map (fun aB -> DoWhile (combine aB, b) |> Seq.singleton)
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
                let cases = List.map2 (fun (c, d) (_, dB) -> c, combineOpt d dB) b brCases
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
            Some (Seq.append st (Seq.singleton (If (aB, combineOpt b bB, combineOpt c cB))))
        | None, bB, cB ->
            Some (Seq.singleton (If (a, combineOpt b bB, combineOpt c cB)))     
        //failwith "Not implemented yet"
    | Throw(a) -> 
        brE a
        |> Option.map (fun (st, aB) -> Seq.append st (Seq.singleton (Throw aB)))
    | TryWith (a, b, c) ->
        match brS a, brS c with
        | None, None -> None
        | aB, cB ->
            Some (Seq.singleton (TryWith (combineOpt a aB, b, combineOpt c cB))) 
        //comb2 (fun ar cr -> TryWith (ar, b, cr)) a c
    | TryFinally (a, b) ->           
        match brS a, brS b with
        | None, None -> None
        | aB, bB ->
            Some (Seq.singleton (TryFinally (combineOpt a aB, combineOpt b bB))) 
        //comb2 (fun ar br -> TryFinally (ar, br)) a b
    | Statements a ->
        let aB = a |> List.map brS 
        if aB |> List.forall Option.isNone then None
        else
            Seq.map2 (fun o b ->
                match b with
                | Some bs -> bs
                | _ -> Seq.singleton o
            ) a aB |> Seq.concat |> List.ofSeq |> Statements |> Seq.singleton |> Some
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
      
open WebSharper.Core

type Substitution(args, ?thisObj) =
    inherit Transformer()
    
//    let mutable replaceThis = true
    let args = 
        Array.ofList (Option.toList thisObj @ if List.isEmpty args then [ Undefined ] else args)
    let refresh = System.Collections.Generic.Dictionary()

    override this.TransformHole i = args.[i]

//    override this.TransformThis () = 
//        if replaceThis then thisObj.Value else This

    override this.TransformFunction(args, body) =
//        replaceThis <- false
        let res = base.TransformFunction(args, body)
//        replaceThis <- true
        res

    override this.TransformId i =
        match refresh.TryFind i with
        | Some n -> n
        | _ ->
            let n = Id.New (?name = i.Name)
            refresh.Add(i, n)
            n
   
type FixThisScope() =
    inherit Transformer()
    let mutable scope = 0
    let mutable thisVar = None

    override this.TransformFunction(args, body) =
        scope <- scope + 1
        let res = base.TransformFunction(args, body)
        scope <- scope - 1
        res
     
    member this.Fix(expr) =
        let b = this.TransformExpression(expr)
        match thisVar with
        | Some t -> Let (t, This, b)
        | _ -> b
        
    override this.TransformThis () =
        if scope > 0 then
            match thisVar with
            | Some t -> Var t
            | None ->
                let t = Id.New "this"
                thisVar <- Some t
                Var t
        else This

type ReplaceThisWithHole0() =
    inherit Transformer()

    override this.TransformThis () = Hole 0

open WebSharper.Core

let globalAccess a = GlobalAccess (Address (List.rev a))
  
let consAddress n (a: Address) = Address(n :: a.Value)

let private runtime = ["Runtime"; "IntelliFactory"]
let private runtimeFunc f = GlobalAccess (Address (f :: runtime))
let runtimeClass =  runtimeFunc "Class"
let runtimeCtor =   runtimeFunc "Ctor"
let runtimeCctor =  runtimeFunc "Cctor"
let runtimeGetOptional = runtimeFunc "GetOptional"
let runtimeSetOptional = runtimeFunc "SetOptional"

//let runtimeDefine = runtimeFunc "Define"

let sysObjDef =
    TypeDefinition {
        Assembly = "mscorlib"
        FullName = "System.Object"    
    }

let ignoreSystemObject td =
    if td = sysObjDef then None else Some td

module Resolve =
    open System.Collections.Generic

    let newName (name: string) =
        match name.LastIndexOf '$' with
        | -1 -> name + "$1"
        | i -> 
            match System.Int32.TryParse name.[i + 1 ..] with
            | true, n -> name.[.. i - 1] + "$" + string (n + 1)
            | _ -> name + "$1"

    type private ResolveNode =
        | Module
        | Class
        | Member
    
    type Resolver() =
        let statics = Dictionary<Address, ResolveNode>()
        let prototypes = Dictionary<TypeDefinition, HashSet<string>>()

        let rec getSubAddress (root: list<string>) (name: string) node =
            let tryAddr = Address (name :: root)
            match statics.TryFind tryAddr, node with
            | Some _, Member
            | Some Member, _ 
            | Some Class, Class -> getSubAddress root (newName name) node
            | Some (Class | Module), Module -> tryAddr
            | _ -> 
                statics.[tryAddr] <- node
                tryAddr

        let getExactSubAddress (root: list<string>) (name: string) node =
            let tryAddr = Address (name :: root)
            match statics.TryFind tryAddr, node with
            | Some (Class | Module), Module -> true
            | Some Module, Class
            | None, _ -> 
                statics.[tryAddr] <- node
                true
            | _ -> false

        let rec getFullAddress (address: list<string>) node =
            match address with
            | [] -> failwith "Empty address"
            | [ x ] -> getSubAddress [] x node
            | h :: r -> getSubAddress ((getFullAddress r Module).Value) h node

        let rec getExactFullAddress (address: list<string>) node =
            match address with
            | [] -> failwith "Empty address"
            | [ x ] -> getExactSubAddress [] x node
            | h :: r -> 
                getExactFullAddress r Module && getExactSubAddress r h node

        member this.LookupPrototype typ =
            match prototypes.TryFind typ with
            | Some p -> p
            | _ ->
                let p = HashSet()
                prototypes.Add(typ, p)
                p

        member this.ExactClassAddress(addr: list<string>, hasPrototype) =
            getExactFullAddress addr (if hasPrototype then Class else Module)
            && if hasPrototype then getExactSubAddress addr "prototype" Member else true 
        
        member this.ClassAddress(addr: list<string>, hasPrototype) =
            let res = getFullAddress addr (if hasPrototype then Class else Module)
            if hasPrototype then
                getExactSubAddress addr "prototype" Member |> ignore    
            res
                    
        member this.ExactStaticAddress addr =
            getExactFullAddress addr Member 

        member this.StaticAddress addr =
            getFullAddress addr Member 
                  


//    type ResolvedNames = 
//        {
//            Statics : Dictionary<Address, bool> 
//            Prototypes : Dictionary<TypeDefinition, HashSet<string>>
//        }
//
//        static member Empty = 
//            {
//                Statics = Dictionary()
//                Prototypes = Dictionary()
//            }
//
//        member this.LookupPrototype typ =
//            match this.Prototypes.TryFind typ with
//            | Some p -> p
//            | _ ->
//                let p = HashSet()
//                this.Prototypes.Add(typ, p)
//                p

//    let rec getSubAddress (root: list<string>) (name: string) isLeaf (d: Dictionary<Address, bool>) =
//        let tryAddr = Address (name :: root)
//        match d.TryGetValue tryAddr with
//        | true, true -> getSubAddress root (newName name) isLeaf d
//        | true, false when not isLeaf -> tryAddr
//        | _ -> 
//            d.Add(tryAddr, isLeaf)
//            tryAddr
//
//    let getExactSubAddress (root: list<string>) (name: string) isLeaf (d: Dictionary<Address, bool>) =
//        let tryAddr = Address (name :: root)
//        match d.TryGetValue tryAddr with
//        | true, true -> failwith "Name resolving error"
//        | true, false when not isLeaf -> tryAddr
//        | _ -> 
//            d.Add(tryAddr, isLeaf)
//            tryAddr
//
//    let rec getFullAddress (address: list<string>) isLeaf (d: Dictionary<Address, bool>) =
//        match address with
//        | [] -> failwith "empty address"
//        | [ x ] -> getSubAddress [] x isLeaf d
//        | h :: r -> getSubAddress ((getFullAddress r false d).Value) h isLeaf d
//    
//    let getPrototype (address: list<string>) (d: Dictionary<Address, bool>) =
//        getExactSubAddress address "prototype" false d
//
//    let getCctor (address: list<string>) (d: Dictionary<Address, bool>) =
//        getExactSubAddress address "$cctor" true d

//    let rec getExactFullAddress (address: list<string>) isLeaf (d: Dictionary<Address, bool>) =
//        match address with
//        | [] -> failwith "empty address"
//        | [ x ] -> getExactSubAddress [] x isLeaf d
//        | h :: r -> getExactSubAddress ((getExactFullAddress r false d).Value) h isLeaf d
   
    let rec getRenamed name (s: HashSet<string>) =
        if s.Add name then name else getRenamed (newName name) s
   
     
//    type Package =
//        | Module of Dictionary<string, option<Package>>
//
//    let rec getModule name (d: IDictionary<string, _>) =
//        match d.TryGetValue name with
//        | true, Some (Module m) -> name, m
//        | true, _ -> getModule (newName name) d  
//        | _ -> 
//            let m = Dictionary()
//            d.Add(name, Some (Module m))
//            name, m
//
//    let classAddress root address =
//        match List.rev address with
//        | c :: rest ->
//            let restAddr, restD = 
//                ([], root) |> List.foldBack (fun name (a, d) -> 
//                    let n, o = getModule name d
//                    n :: a, o
//                ) rest
//            let n, o = getModule c restD 
//            Address (n :: restAddr), o
//        | _ ->
//            failwith "empty address"
//
//    let rec getRenamed name (s: ISet<string>) =
//        if s.Add name then name else getRenamed (newName name) s
//
//    let rec addChild name child (d: IDictionary<string, _>) =
//        match d.TryGetValue name with
//        | true, _ -> addChild (newName name) child d
//        | _ -> 
//            d.Add(name, child)
//            name 
//
//    let addStatic root address =
//        match List.rev address with
//        | c :: rest ->
//            let restAddr, restD = 
//                ([], root) |> List.foldBack (fun name (a, d) -> 
//                    let n, o = getModule nam- d
//                    n :: a, o
//                ) rest
//            let n = addChild c None restD
//            Address (n :: restAddr)
//        | _ ->
//            failwith "empty address"
//
//    let addPrototype (d: IDictionary<_, _>) =
//        let m = Dictionary() 
//        d.Add("prototype", Some (Module m))
//        m
    