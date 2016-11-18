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

// Various helpers for compilation
[<AutoOpen>]
module WebSharper.Compiler.CompilationHelpers

open WebSharper.Core
open WebSharper.Core.AST

module I = IgnoreSourcePos

/// Change every occurence of one Id to another
type ReplaceId(fromId, toId) =
    inherit Transformer()
    
    override this.TransformId i =
        if i = fromId then toId else i

/// Determine if expression has no side effect
let rec isPureExpr expr =
    match expr with
    | Undefined
    | This
    | Base
    | Var _
    | Value _
    | Function _ 
    | GlobalAccess _
    | Self
        -> true
    | Sequential a 
    | NewArray a 
        -> List.forall isPureExpr a 
    | Conditional (a, b, c) 
        -> isPureExpr a && isPureExpr b && isPureExpr c
    | ItemGet(a, b)
    | Binary (a, _, b)
    | Let (_, a, b)
    | Coalesce(a, _, b) 
        -> isPureExpr a && isPureExpr b 
    | Unary (_, a) 
    | ExprSourcePos (_, a)
    | TypeCheck(a, _)
        -> isPureExpr a     
    | Object a 
        -> List.forall (snd >> isPureExpr) a 
    | LetRec (a, b) 
        -> List.forall (snd >> isPureExpr) a && isPureExpr b
    | Application(a, b, true, _) ->
        isPureExpr a && List.forall isPureExpr b    
    | _ -> false

let isPureFunction expr =
    match IgnoreExprSourcePos expr with
    | Function (_, (I.Return body | I.ExprStatement body)) -> isPureExpr body
    | Function (_, (I.Empty | I.Block [])) -> true
    | _ -> false

let rec isTrivialValue expr =
    match expr with
    | Undefined
    | Value _
    | GlobalAccess _
        -> true
    | Var v  ->
        not v.IsMutable
    | ExprSourcePos (_, a) ->
        isTrivialValue a
    | _ -> false

/// Determine if expression has no side effect and value does not depend on execution order
let rec isStronglyPureExpr expr =
    match expr with
    | Undefined
    | This
    | Base
    | Value _
    | Function _ 
    | GlobalAccess _
    | Self
        -> true
    | Var v ->
        not v.IsMutable
    | Sequential a 
        -> 
        match List.rev a with
        | [] -> true
        | h :: t -> isStronglyPureExpr h && List.forall isPureExpr t
    | NewArray a 
        -> List.forall isStronglyPureExpr a 
    | Conditional (a, b, c) 
        -> isStronglyPureExpr a && isStronglyPureExpr b && isStronglyPureExpr c
    | ItemGet(a, b)
    | Binary (a, _, b)
    | Let (_, a, b)
    | Coalesce(a, _, b) 
        -> isStronglyPureExpr a && isStronglyPureExpr b 
    | Unary (op, a)
        ->
        match op with
        | UnaryOperator.``void`` -> isPureExpr a
        | _ -> isStronglyPureExpr a 
    | ExprSourcePos (_, a)
    | TypeCheck(a, _)
        -> isStronglyPureExpr a     
    | Object a 
        -> List.forall (snd >> isStronglyPureExpr) a 
    | LetRec (a, b) 
        -> List.forall (snd >> isStronglyPureExpr) a && isStronglyPureExpr b
    | Application(a, b, true, _) ->
        isStronglyPureExpr a && List.forall isStronglyPureExpr b    
    | _ -> false

/// Checks if a specific Id is mutated or accessed within a function body
/// (captured) inside an Expression
type private NotMutatedOrCaptured(v) =
    inherit Visitor()

    let mutable scope = 0
    let mutable ok = true

    override this.VisitVarSet (a, b) =
        if a = v then ok <- false
        else this.VisitExpression b

    override this.VisitMutatingUnary (_, a) =
        match IgnoreExprSourcePos a with
        | Var av when av = v ->
            ok <- false
        | _ ->
            this.VisitExpression a

    override this.VisitMutatingBinary (a, _, b) =
        match IgnoreExprSourcePos a with
        | Var av when av = v ->
            ok <- false
        | _ ->
            this.VisitExpression a
            this.VisitExpression b

    override this.VisitFunction(a, b) =
        scope <- scope + 1
        base.VisitFunction(a, b)
        scope <- scope - 1

    override this.VisitExpression(a) =
        if ok then base.VisitExpression(a)

    override this.VisitId a =
        if a = v && scope > 0 then ok <- false

    member this.Check(a) =
        this.VisitExpression(a)
        ok

let notMutatedOrCaptured (v: Id) expr =
    NotMutatedOrCaptured(v).Check(expr)   

/// Optimization for inlining: if arguments are always accessed in
/// the same order as they are provided, and there are no side effects
/// between then, then extra Let forms and variables for them are not needed
let varEvalOrder (vars : Id list) expr =
    let watchedVars = System.Collections.Generic.HashSet vars
    let mutable vars = vars
    let mutable ok = true 

    let fail () =
        vars <- []
        ok <- false
    
    let stop () =
        if List.isEmpty vars |> not then 
            fail()  
        
    let rec eval e =
        if ok then
            match e with
            | Undefined
            | This
            | Base
            | Value _
            | Self
                -> ()
            | Sequential a
            | NewArray a ->
                List.iter eval a
            | Conditional (a, b, c) ->
                eval a
                let aVars = vars
                eval b
                if ok then
                    let bVars = vars
                    vars <- aVars
                    eval c
                    if ok && (bVars <> vars) then fail()
            | ItemGet(a, b) 
            | ItemGetNonPure(a, b)
            | Binary (a, _, b)
            | Let (_, a, b)
                ->
                eval a
                eval b
            | Unary (_, a) 
            | ExprSourcePos (_, a)
            | TypeCheck(a, _)
                -> eval a
            | Object a 
                -> List.iter (snd >> eval) a 
            | Var a ->
                if watchedVars.Contains a then
                    match vars with
                    | [] -> fail()
                    | hv :: tv ->
                        if a = hv then
                            vars <- tv
                        else fail() 
            | VarSet(a, b) ->
                if watchedVars.Contains a then
                    fail()
                else 
                    eval b
            | ItemSet(a, b, c) ->
                eval a
                eval b
                eval c
                stop()
            | MutatingUnary (_, a) ->
                eval a
                stop()                 
            | MutatingBinary(a, _, c) ->
                match IgnoreExprSourcePos a with
                | Var v
                | ItemGet(I.Var v, _)
                | ItemGetNonPure(I.Var v, _)
                    -> if watchedVars.Contains v then fail()
                | _ -> ()   
                eval a
                eval c
                stop()                 
            | Application(a, b, c, _) ->
                eval a
                List.iter eval b
                if not c then stop()
            | Call(a, _, _, b) ->
                Option.iter eval a
                List.iter eval b
                stop()
            | Ctor(_, _, a) -> 
                List.iter eval a
                stop()
            | FieldGet(a, _, _) ->
                Option.iter eval a
            | FieldSet(a, _, _, b) ->   
                Option.iter eval a
                eval b
                stop()
            // TODO: more
            | StatementExpr (a, _) ->
                evalSt a
            | _ -> fail()
    
    and evalSt s =
        match s with
        | ExprStatement a
        | VarDeclaration(_, a) -> eval a
        | StatementSourcePos (_, a) -> evalSt a
        | Block a -> List.iter evalSt a
        | If (a, b, c) -> Conditional (a, IgnoredStatementExpr b, IgnoredStatementExpr c) |> eval
        | _ -> fail()
               
    eval expr
    ok && List.isEmpty vars   

/// Counts the number of occurrences of a single Id within an
/// expression or statement. Useful for Let optimization.
type CountVarOccurence(v) =
    inherit Visitor()

    let mutable occ = 0

    override this.VisitId(a) =
        if a = v then 
            occ <- occ + 1

    member this.Get(e) =
        this.VisitExpression(e) 
        occ

    member this.GetForStatement(s) =
        this.VisitStatement(s) 
        occ

type VarsNotUsed(vs) =
    inherit Visitor()

    let mutable ok = true

    override this.VisitId(a) =
        if vs |> List.contains a then 
            ok <- false

    member this.Get(e) =
        this.VisitExpression(e) 
        ok

/// Substitutes every access to an Id to a given expression
type SubstituteVar(v, e) =
    inherit Transformer()

    override this.TransformVar(a) = if a = v then e else Var a

/// Substitutes every access to specific variables to an expression,
/// as described by the input dictionary.
type SubstituteVars(sub : System.Collections.Generic.IDictionary<Id, Expression>) =
    inherit Transformer()

    override this.TransformVar(a) = 
        match sub.TryGetValue(a) with
        | true, e -> e
        | _ -> Var a 
      
type ContinueTransformer(e) =
    inherit StatementTransformer()

    override this.TransformContinue(a) =
        Block [
            ExprStatement e
            Continue a
        ]

type RemoveSourcePositions() =
    inherit Transformer()

    override this.TransformExprSourcePos(_, e) =
        this.TransformExpression e

    override this.TransformStatementSourcePos(_, s) =
        this.TransformStatement s

let removeSourcePos = RemoveSourcePositions()

type Substitution(args, ?thisObj) =
    inherit Transformer()
    
    let args = 
        Array.ofList (Option.toList thisObj @ if List.isEmpty args then [ Value Null ] else args)
    let refresh = System.Collections.Generic.Dictionary()

    override this.TransformHole i = 
        if i <= args.Length - 1 then args.[i] else Undefined

    override this.TransformFunction(args, body) =
        let res = base.TransformFunction(args, body)
        res

    override this.TransformId i =
        match refresh.TryFind i with
        | Some n -> n
        | _ ->
            let n = i.Clone()
            refresh.Add(i, n)
            n
   
type FixThisScope() =
    inherit Transformer()
    let mutable scope = 0
    let mutable thisVar = None
    let mutable thisArgs = System.Collections.Generic.Dictionary<Id, int * bool ref>()

    override this.TransformFunction(args, body) =
        scope <- scope + 1
        let res = base.TransformFunction(args, body)
        scope <- scope - 1
        res
     
    override this.TransformFuncWithThis (thisArg, args, body) =
        scope <- scope + 1
        let used = ref false
        thisArgs.Add(thisArg, (scope, used))
        let trBody = this.TransformStatement body
        scope <- scope - 1
        if !used then
            Function(args, CombineStatements [ VarDeclaration(thisArg, This); trBody ])
        else
            Function(args, trBody)
    
    member this.Fix(expr) =
        let b = this.TransformExpression(expr)
        match thisVar with
        | Some t -> Let (t, This, b)
        | _ -> b

    member this.Fix(statement) =
        let b = this.TransformStatement(statement)
        match thisVar with
        | Some t -> CombineStatements [ VarDeclaration(t, This); b ]
        | _ -> b
                
    override this.TransformThis () =
        if scope > 0 then
            match thisVar with
            | Some t -> Var t
            | None ->
                let t = Id.New ("$this", mut = false)
                thisVar <- Some t
                Var t
        else This

    override this.TransformVar v =
        match thisArgs.TryFind v with
        | Some (funcScope, used) ->
            if scope > funcScope then
                used := true
                Var v
            else This
        | _ -> Var v

type ReplaceThisWithVar(v) =
    inherit Transformer()

    override this.TransformThis () = Var v
    override this.TransformBase () = failwith "Base call is not allowed inside inlined member"

let makeExprInline (vars: Id list) expr =
    if varEvalOrder vars expr then
        SubstituteVars(vars |> Seq.mapi (fun i a -> a, Hole i) |> dict).TransformExpression(expr)
    else
        List.foldBack (fun (v, h) body ->
            Let (v, h, body)    
        ) (vars |> List.mapi (fun i a -> a, Hole i)) expr

module JSRuntime =
    let private runtime = ["Runtime"; "IntelliFactory"]
    let private runtimeFunc f p args = Application(GlobalAccess (Address (f :: runtime)), args, p, Some (List.length args))
    let private runtimeFuncI f p i args = Application(GlobalAccess (Address (f :: runtime)), args, p, Some i)
    let Class members basePrototype statics = runtimeFunc "Class" true [members; basePrototype; statics]
    let Ctor ctor typeFunction = runtimeFunc "Ctor" true [ctor; typeFunction]
    let Cctor cctor = runtimeFunc "Cctor" true [cctor]
    let GetOptional value = runtimeFunc "GetOptional" true [value]
    let SetOptional obj field value = runtimeFunc "SetOptional" false [obj; field; value]
    let DeleteEmptyFields obj fields = runtimeFunc "DeleteEmptyFields" false [obj; NewArray fields] 
    let CombineDelegates dels = runtimeFunc "CombineDelegates" true [dels]  
    let BindDelegate func obj = runtimeFunc "BindDelegate" true [func; obj]    
    let DelegateEqual d1 d2 = runtimeFunc "DelegateEqual" true [d1; d2]
    let Curried f n = runtimeFuncI "Curried" true 3 [f; Value (Int n)]
    let Curried2 f = runtimeFuncI "Curried2" true 1 [f]
    let Curried3 f = runtimeFuncI "Curried3" true 1 [f]
    let Apply f args = runtimeFunc "Apply" false [f; NewArray args]
    let Apply2 f a b = runtimeFunc "Apply2" false [f; a; b]
    let Apply3 f a b c = runtimeFunc "Apply3" false [f; a; b; c]

module Definitions =
    let Obj =
        TypeDefinition {
            Assembly = "mscorlib"
            FullName = "System.Object"    
        }

    let Dynamic =
        TypeDefinition {
            Assembly = ""
            FullName = "dynamic"
        }

    let IResource =
        TypeDefinition {
            Assembly = "WebSharper.Core"
            FullName = "WebSharper.Core.Resources+IResource"    
        }

    let Async =
        TypeDefinition {
            Assembly = "FSharp.Core"
            FullName = "Microsoft.FSharp.Control.FSharpAsync`1"
        }
        
    let Task =
        TypeDefinition {
            Assembly = "mscorlib"
            FullName = "System.Threading.Tasks.Task"
        }

    let Task1 =
        TypeDefinition {
            Assembly = "mscorlib"
            FullName = "System.Threading.Tasks.Task`1"
        }

    let IRemotingProvider =
        TypeDefinition {
            Assembly = "WebSharper.Main"
            FullName = "WebSharper.Remoting+IRemotingProvider"
        } 

    let String =
        TypeDefinition {
            Assembly = "mscorlib"
            FullName = "System.String"
        }

    let Int =
        TypeDefinition {
            Assembly = "mscorlib"
            FullName = "System.Int32"
        }

    let Bool =
        TypeDefinition {
            Assembly = "mscorlib"
            FullName = "System.Boolean"
        }
    
let ignoreSystemObject td =
    if td = Definitions.Obj then None else Some td

module Resolve =
    open System.Collections.Generic

    let newName (name: string) =
        match name.LastIndexOf '$' with
        | -1 -> name + "$1"
        | i -> 
            match System.Int32.TryParse (name.Substring(i + 1)) with
            | true, n -> name.Substring(0, i) + "$" + string (n + 1)
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
                     
    let rec getRenamed name (s: HashSet<string>) =
        if s.Add name then name else getRenamed (newName name) s
 
open WebSharper.Core.Metadata 
open System.Collections.Generic

let getAllAddresses (meta: Info) =
    let r = Resolve.Resolver()
    for KeyValue(typ, cls) in meta.Classes do
        if typ.Value.FullName.StartsWith "Generated$" then () else
        let pr = if cls.HasWSPrototype then Some (r.LookupPrototype typ) else None 
        let rec addMember (m: CompiledMember) =
            match m with
            | Instance n -> pr |> Option.iter (fun p -> p.Add n |> ignore)
            | Static a 
            | Constructor a -> r.ExactStaticAddress a.Value |> ignore
            | Macro (_, _, Some m) -> addMember m
            | _ -> ()
        for m, _, _ in cls.Constructors.Values do addMember m
        for f in cls.Fields.Values do
            match f with
            | InstanceField n 
            | OptionalField n -> pr |> Option.iter (fun p -> p.Add n |> ignore)
            | StaticField a -> r.ExactStaticAddress a.Value |> ignore
            | IndexedField _ -> ()
        for m, _ in cls.Implementations.Values do addMember m
        for m, _, _ in cls.Methods.Values do addMember m
        match cls.StaticConstructor with
        | Some (a, _) -> r.ExactStaticAddress a.Value |> ignore  
        | _ -> ()
    // inheritance 
    let inheriting = HashSet()
    let rec inheritMembers typ (cls: ClassInfo) =
        if inheriting.Add typ then
            match cls.BaseClass with
            | None -> ()
            | Some b ->
                // assembly containing base class may not be referenced
                match meta.Classes.TryFind b with
                | None -> ()
                | Some bCls ->
                    inheritMembers b bCls              
                    (r.LookupPrototype typ).UnionWith(r.LookupPrototype b) 
    for KeyValue(typ, cls) in meta.Classes do
        inheritMembers typ cls        
    r

type Refresher() =
    inherit Transformer()
    
    let refresh = System.Collections.Generic.Dictionary()

    override this.TransformId i =
        match refresh.TryFind i with
        | Some n -> n
        | _ ->
            let n = i.Clone()
            refresh.Add(i, n)
            n

let refreshAllIds (i: Info) =
    let r = Refresher()

    let rec refreshNotInline (i, p, e) =
        match i with
        | Inline
        | NotCompiledInline -> i, p, e
        | Macro (_, _, Some f) -> refreshNotInline (f, p, e)
        | _ -> i, p, r.TransformExpression e

    { i with
        Classes =
            i.Classes |> Dict.map (fun c ->
                { c with
                    Constructors = 
                        c.Constructors |> Dict.map refreshNotInline
                    StaticConstructor = 
                        c.StaticConstructor |> Option.map (fun (x, b) -> x, r.TransformExpression b) 
                    Methods = 
                        c.Methods |> Dict.map refreshNotInline
                    Implementations = 
                        c.Implementations |> Dict.map (fun (x, b) -> x, r.TransformExpression b) 
                }
            )
        EntryPoint = i.EntryPoint |> Option.map r.TransformStatement 
    }

type MaybeBuilder() =
    member this.Bind(x, f) = 
        match x with
        | None -> None
        | Some a -> f a

    member this.Return(x) = Some x
    member this.ReturnFrom(x) = x
    member this.Zero() = None
   
let maybe = new MaybeBuilder()

let trimMetadata (meta: Info) (nodes : seq<Node>) =
    let classes = Dictionary<_,_>() 
    let getOrAddClass td =
        try
            match classes.TryGetValue td with
            | true, cls -> cls
            | _ ->
                let cls = 
                    { meta.Classes.[td] with
                        Constructors = Dictionary<_,_>()
                        Methods = Dictionary<_,_>()
                        Implementations = Dictionary<_,_>()
                    }
                classes.Add(td, cls)
                cls    
        with _ -> failwithf "%A" td.Value
    for n in nodes do
        match n with
        | MethodNode (td, m) -> 
            (getOrAddClass td).Methods.Add(m, meta.Classes.[td].Methods.[m])
        | ConstructorNode (td, c) -> 
            (getOrAddClass td).Constructors.Add(c, meta.Classes.[td].Constructors.[c])
        | ImplementationNode (td, i, m) ->
            (getOrAddClass td).Implementations.Add((i, m), meta.Classes.[td].Implementations.[i, m])
        | TypeNode td ->
            if meta.Classes.ContainsKey td then 
                getOrAddClass td |> ignore 
        | _ -> ()
    { meta with Classes = classes}

let removeSourcePositionFromMetadata (meta: Info) =
    { meta with 
        Classes = 
            meta.Classes |> Dict.map (fun c ->
                { c with 
                    Constructors = c.Constructors |> Dict.map (fun (i, p, e) -> i, p, removeSourcePos.TransformExpression e)    
                    StaticConstructor = c.StaticConstructor |> Option.map (fun (a, e) -> a, removeSourcePos.TransformExpression e)
                    Methods = c.Methods |> Dict.map (fun (i, p, e) -> i, p, removeSourcePos.TransformExpression e)
                    Implementations = c.Implementations |> Dict.map (fun (i, e) -> i, removeSourcePos.TransformExpression e)
                }
            )
        EntryPoint = meta.EntryPoint |> Option.map removeSourcePos.TransformStatement
    }

type TransformSourcePositions(asmName) =
    inherit Transformer()
    
    let fileMap = Dictionary()

    let fileNames = HashSet()

    let trFileName fn =
        match fileMap.TryFind fn with
        | Some res -> res
        | None ->
            let name = Resolve.getRenamed (Path.GetFileNameWithoutExtension(fn)) fileNames
            let res = asmName + "/" + name + Path.GetExtension(fn)
            fileMap.Add(fn, res)
            res

    member this.FileMap = fileMap |> Seq.map (fun (KeyValue pair) -> pair) |> Array.ofSeq

    override this.TransformExprSourcePos(p, e) =
        ExprSourcePos (
            { p with FileName = trFileName p.FileName },
            this.TransformExpression e
        )

    override this.TransformStatementSourcePos(p, s) =
        StatementSourcePos (
            { p with FileName = trFileName p.FileName },
            this.TransformStatement s
        )

let transformAllSourcePositionsInMetadata asmName (meta: Info) =
    let tr = TransformSourcePositions(asmName)
    { meta with 
        Classes = 
            meta.Classes |> Dict.map (fun c ->
                { c with 
                    Constructors = c.Constructors |> Dict.map (fun (i, p, e) -> i, p, tr.TransformExpression e)    
                    StaticConstructor = c.StaticConstructor |> Option.map (fun (a, e) -> a, tr.TransformExpression e)
                    Methods = c.Methods |> Dict.map (fun (i, p, e) -> i, p, tr.TransformExpression e)
                    Implementations = c.Implementations |> Dict.map (fun (i, e) -> i, tr.TransformExpression e)
                }
            )
        EntryPoint = meta.EntryPoint |> Option.map tr.TransformStatement
    },
    tr.FileMap

type Capturing(?var) =
    inherit Transformer()

    let defined = HashSet()
    let mutable capture = false
    let mutable captVal = None
    let mutable scope = 0

    override this.TransformNewVar(var, value) =
        if scope = 0 then
            defined.Add var |> ignore
        NewVar(var, this.TransformExpression value)

    override this.TransformVarDeclaration(var, value) =
        if scope = 0 then
            defined.Add var |> ignore
        VarDeclaration(var, this.TransformExpression value)

    override this.TransformLet(var, value, body) =
        if scope = 0 then
            defined.Add var |> ignore
        Let(var, this.TransformExpression value, this.TransformExpression body)

    override this.TransformLetRec(defs, body) = 
        if scope = 0 then
            for var, _ in defs do
                defined.Add var |> ignore
        LetRec (defs |> List.map (fun (a, b) -> a, this.TransformExpression b), body |> this.TransformExpression)
    
    override this.TransformId i =
        if scope > 0 then
            match var with
            | Some v when v = i ->
                capture <- true
                match captVal with
                | Some c -> c
                | _ ->
                    let c = i.Clone()
                    captVal <- Some c
                    c
            | _ ->
                if defined.Contains i then 
                    capture <- true
                i
        else i

    override this.TransformFunction (args, body) =
        scope <- scope + 1
        let res = Function (args, this.TransformStatement body)
        scope <- scope - 1
        res

    member this.CaptureValueIfNeeded expr =
        let res = this.TransformExpression expr  
        if capture then
            match captVal with
            | None -> Application (Function ([], Return res), [], false, Some 0)
            | Some c -> Application (Function ([c], Return res), [Var var.Value], false, Some 1)        
        else expr

type NeedsScoping() =
    inherit Visitor()

    let defined = HashSet()
    let mutable needed = false
    let mutable scope = 0

    override this.VisitNewVar(var, value) =
        if scope = 0 then
            defined.Add var |> ignore
        this.VisitExpression value

    override this.VisitVarDeclaration(var, value) =
        if scope = 0 then
            defined.Add var |> ignore
        this.VisitExpression value

    override this.VisitLet(var, value, body) =
        if scope = 0 then
            defined.Add var |> ignore
        this.VisitExpression value
        this.VisitExpression body

    override this.VisitLetRec(defs, body) = 
        if scope = 0 then
            for var, _ in defs do
                defined.Add var |> ignore
        for _, value in defs do
            this.VisitExpression value
        this.VisitExpression body         
    
    override this.VisitId i =
        if scope > 0 && defined.Contains i then 
            needed <- true

    override this.VisitFunction (args, body) =
        scope <- scope + 1
        this.VisitStatement body
        scope <- scope - 1

    member this.Check(args, expr) =
        for a in args do
            defined.Add a |> ignore
        this.VisitExpression expr  
        needed

let needsScoping args body =
    NeedsScoping().Check(args, body)    
    
type HasNoThisVisitor() =
    inherit Visitor()

    let mutable ok = true

    override this.VisitThis() = ok <- false

    member this.Check(e) =
        this.VisitExpression e
        ok