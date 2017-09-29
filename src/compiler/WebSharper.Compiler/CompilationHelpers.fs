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
open System.Collections.Generic

module I = IgnoreSourcePos

let rec removePureParts expr =
    match expr with
    | Undefined
    | This
    | Base
    | Var _
    | Value _
    | Function _ 
    | GlobalAccess _
    | Self
        -> Undefined
    | Sequential a
    | NewArray a 
        -> CombineExpressions (a |> List.map removePureParts) 
    | Conditional (a, b, c) 
        -> Conditional (a, removePureParts b, removePureParts c) 
    | ItemGet(a, b, (NoSideEffect | Pure))
    | Binary (a, _, b)
        -> CombineExpressions [removePureParts a; removePureParts b]
    | Let (v, a, b)
        -> Let (v, a, removePureParts b)
    | Unary (_, a) 
    | TypeCheck(a, _)
        -> removePureParts a     
    | ExprSourcePos (p, a)
        -> ExprSourcePos (p, removePureParts a)     
    | Object a 
        -> a |> List.map (snd >> removePureParts) |> CombineExpressions
    | LetRec (a, b) 
        -> LetRec (a, removePureParts b)
    | Application(a, b, (NoSideEffect | Pure), _) ->
        CombineExpressions ((a :: b) |> List.map removePureParts)
    | _ -> expr

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
    | ItemGet(a, b, (NoSideEffect | Pure))
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
    | Application(a, b, (NoSideEffect | Pure), _) ->
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
    | ItemGet(a, b, Pure)
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
    | Application(a, b, Pure, _) ->
        isStronglyPureExpr a && List.forall isStronglyPureExpr b    
    | _ -> false

let getFunctionPurity expr =
    match IgnoreExprSourcePos expr with
    | Function (_, (I.Return body | I.ExprStatement body)) -> 
        if isStronglyPureExpr body then
            Pure
        elif isPureExpr body then
            NoSideEffect
        else 
            NonPure
    | Function (_, (I.Empty | I.Block [])) -> Pure
    | _ -> NonPure

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

type VarsNotUsed(vs : HashSet<Id>) =
    inherit Visitor()

    let mutable ok = true

    new (vs: seq<Id>) = VarsNotUsed(HashSet vs)
    
    override this.VisitId(a) =
        if vs.Contains a then 
            ok <- false

    override this.VisitExpression(e) =
        if ok then
            base.VisitExpression(e) 

    member this.Get(e) =
        if vs.Count = 0 then true else
        ok <- true
        this.VisitExpression(e) 
        ok

    member this.GetSt(s) =
        if vs.Count = 0 then true else
        ok <- true
        this.VisitStatement(s) 
        ok

/// Optimization for inlining: if arguments are always accessed in
/// the same order as they are provided, and there are no side effects
/// between then, then extra Let forms and variables for them are not needed
let varEvalOrder (vars : Id list) expr =
    let watchedVars = HashSet vars
    let varsNotUsed = VarsNotUsed watchedVars
        
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
            | GlobalAccess _
            | OverrideName _
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
            | ItemGet(a, b, (NoSideEffect | Pure))
            | Binary (a, _, b)
            | Let (_, a, b)
                ->
                eval a
                eval b
            | LetRec (a, b) ->
                List.iter (snd >> eval) a
                eval b
            | ItemGet(a, b, NonPure) ->
                eval a
                eval b
                stop()
            | Unary (_, a) 
            | ExprSourcePos (_, a)
            | TypeCheck(a, _)
            | NewVar(_, a)
            | UnionCaseGet (a, _, _, _)
            | UnionCaseTag (a, _)
            | UnionCaseTest (a, _, _)
            | Cast (_, a)
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
                | ItemGet(I.Var v, _, _)
                    -> if watchedVars.Contains v then fail()
                | _ -> ()   
                eval a
                eval c
                stop()                 
            | Application(a, b, c, _) ->
                eval a
                List.iter eval b
                if c = NonPure then stop()
            | New(a, b) ->
                eval a
                List.iter eval b
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
            | Function(_, a)
            | FuncWithThis(_, _, a) ->
                if not <| varsNotUsed.GetSt(a) then fail()
            | StatementExpr (a, _) ->
                evalSt a
            | Arguments
            | Await _
            | ChainedCtor _
            | Ctor _
            | CallNeedingMoreArgs _
            | Cctor _
            | Coalesce _
            | ComplexElement _
            | CopyCtor _
            | CurriedApplication _
            | Hole _
            | MatchSuccess _
            | NamedParameter _ 
            | NewDelegate _
            | NewRecord _
            | NewUnionCase _
            | OptimizedFSharpArg _
            | RefOrOutParameter _
            | TraitCall _
                -> fail()
            | _ ->
                failwith "unexpected form"
    
    and evalSt s =
        if ok then
            match s with
            | Empty 
                -> ()
            | ExprStatement a
            | VarDeclaration(_, a) -> eval a
            | Labeled (_, a)
            | StatementSourcePos (_, a) -> evalSt a
            | Block a -> List.iter evalSt a
            | If (a, b, c) -> Conditional (a, IgnoredStatementExpr b, IgnoredStatementExpr c) |> eval
            | Throw (a)
            | Return (a) ->
                eval a
                stop()
            | FuncDeclaration(_, _, a) -> 
                if not <| varsNotUsed.GetSt(a) then fail()
            | TryFinally (a, b) ->
                evalSt a
                evalSt b
            | TryWith (a, _, b) ->
                evalSt a
                stop()
                evalSt b
            | Break _ 
            | CSharpSwitch _
            | Continuation _
            | Continue _
            | DoNotReturn
            | DoWhile _
            | For _
            | ForIn _
            | Goto _
            | GotoCase _
            | Switch _
            | While _
            | Yield _
                -> fail()      
            | _ ->
                failwith "unexpected form"
               
    eval expr
    ok && List.isEmpty vars   

let sameVars vars args =
    args |> List.forall (function I.Var _ -> true | _ -> false)
    && vars = (args |> List.map (function I.Var v -> v | _ -> failwith "impossible")) 

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
    let mutable chainedCtor = None
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
    
    override this.TransformChainedCtor(a, b, c, d, e) =
        let cc = Id.New()
        chainedCtor <- Some cc 
        Sequential [ ChainedCtor(a, this.TransformExpression b, c, d, e |> List.map this.TransformExpression); Var cc ] 

    member this.Fix(expr) =
        let b = this.TransformExpression(expr)
        match thisVar, chainedCtor with
        | Some t, Some cc -> SubstituteVar(cc, NewVar(t, This)).TransformExpression(b) 
        | Some t, _ -> Let (t, This, b)
        | _, Some cc -> SubstituteVar(cc, Undefined).TransformExpression(b) 
        | _ -> b

    member this.Fix(statement) =
        let b = this.TransformStatement(statement)
        match thisVar, chainedCtor with
        | Some t, Some cc -> SubstituteVar(cc, NewVar(t, This)).TransformStatement(b) 
        | Some t, _ -> CombineStatements [ VarDeclaration(t, This); b ]
        | _, Some cc -> SubstituteVar(cc, Undefined).TransformStatement(b) 
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

let CurrentGlobal a = GlobalAccess { Module = CurrentModule; Address = Hashed (List.rev a) }

module JSRuntime =
    let private runtimeFunc f p args = Application(GlobalAccess (Address.Runtime f), args, p, Some (List.length args))
    let private runtimeFuncI f p i args = Application(GlobalAccess (Address.Runtime f), args, p, Some i)
    let Create obj props = runtimeFunc "Create" Pure [obj; props]
    let Class members basePrototype statics = runtimeFunc "Class" Pure [members; basePrototype; statics]
    let Ctor ctor typeFunction = runtimeFunc "Ctor" Pure [ctor; typeFunction]
    let Clone obj = runtimeFunc "Clone" Pure [obj]
    let GetOptional value = runtimeFunc "GetOptional" Pure [value]
    let SetOptional obj field value = runtimeFunc "SetOptional" NonPure [obj; field; value]
    let DeleteEmptyFields obj fields = runtimeFunc "DeleteEmptyFields" NonPure [obj; NewArray fields] 
    let CombineDelegates dels = runtimeFunc "CombineDelegates" Pure [dels]  
    let BindDelegate func obj = runtimeFunc "BindDelegate" Pure [func; obj]    
    let DelegateEqual d1 d2 = runtimeFunc "DelegateEqual" Pure [d1; d2]
    let Curried f n = runtimeFuncI "Curried" Pure 3 [f; Value (Int n)]
    let Curried2 f = runtimeFuncI "Curried2" Pure 1 [f]
    let Curried3 f = runtimeFuncI "Curried3" Pure 1 [f]
    let CurriedA f n arr = runtimeFuncI "Curried" Pure 3 [f; Value (Int n); arr]
    let Apply f obj args = runtimeFunc "Apply" Pure [f; obj; NewArray args]
    let OnLoad f = runtimeFunc "OnLoad" NonPure [f]

module Definitions =
    open WebSharper.InterfaceGenerator.Type

    let Obj =
        TypeDefinition {
            Assembly = "mscorlib"
            FullName = "System.Object"    
        }

    let ValueType =
        TypeDefinition {
            Assembly = "mscorlib"
            FullName = "System.ValueType"    
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

    // Private static field for single-case unions.
    let SingletonUnionCase name =
        Method {
            MethodName = "_unique_" + name
            Parameters = []
            ReturnType = VoidType 
            Generics = 0
        }

    let StringFormat1 =
        Method {
            MethodName = "Format"
            Parameters = [ NonGenericType String; NonGenericType Obj ]
            ReturnType = NonGenericType String
            Generics = 0
        }
    
let ignoreSystemObject td =
    if td = Definitions.Obj || td = Definitions.ValueType then None else Some td

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
        let statics = Dictionary<Hashed<list<string>>, ResolveNode>()
        let prototypes = Dictionary<TypeDefinition, HashSet<string>>()

        let rec getSubAddress (root: list<string>) (name: string) node =
            let name = name.Replace('.', '_')
            let tryAddr = Hashed (name :: root)
            match statics.TryFind tryAddr, node with
            | Some _, Member
            | Some Member, _ 
            | Some Class, Class -> getSubAddress root (newName name) node
            | Some (Class | Module), Module -> tryAddr
            | _ -> 
                statics.[tryAddr] <- node
                tryAddr

        let getExactSubAddress (root: list<string>) (name: string) node =
            let tryAddr = Hashed (name :: root)
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
        with _ ->
            eprintfn "WebSharper error: Assembly needed for bundling but is not referenced: %s" td.Value.Assembly
            ClassInfo.None
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

let private exposeAddress asmName (a: Address) =
    match a.Module with
    | CurrentModule ->
        { a with Module = WebSharperModule asmName }
    | _ -> a

type RemoveSourcePositionsAndUpdateModule (asmName) =
    inherit RemoveSourcePositions ()

    override this.TransformGlobalAccess(a) =
        GlobalAccess <| exposeAddress asmName a

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

type TransformSourcePositionsAndUpdateModule(asmName) =
    inherit TransformSourcePositions(asmName)

    override this.TransformGlobalAccess(a) =
        GlobalAccess <| exposeAddress asmName a

let rec private exposeCompiledMember asmName m = 
    match m with
    | Static a -> Static <| exposeAddress asmName a
    | Macro (td, p, Some r) ->
        Macro (td, p, Some (exposeCompiledMember asmName r))
    | _ -> m

let private exposeCompiledField asmName f =
    match f with
    | StaticField a -> StaticField <| exposeAddress asmName a
    | _ -> f

let transformAllSourcePositionsInMetadata asmName isRemove (meta: Info) =
    let tr, sp = 
        if isRemove then
            RemoveSourcePositionsAndUpdateModule(asmName) :> Transformer, None 
        else
            let tr = TransformSourcePositionsAndUpdateModule(asmName)
            tr :> _, Some tr
    { meta with 
        Classes = 
            meta.Classes |> Dict.map (fun c ->
                { c with 
                    Address = c.Address |> Option.map (exposeAddress asmName)
                    Constructors = c.Constructors |> Dict.map (fun (i, p, e) -> exposeCompiledMember asmName i, p, tr.TransformExpression e)    
                    Fields = c.Fields |> Dict.map (fun (i, p, t) -> exposeCompiledField asmName i, p, t)
                    StaticConstructor = c.StaticConstructor |> Option.map (fun (a, e) -> exposeAddress asmName a, tr.TransformExpression e)
                    Methods = c.Methods |> Dict.map (fun (i, p, e) -> exposeCompiledMember asmName i, p, tr.TransformExpression e)
                    Implementations = c.Implementations |> Dict.map (fun (i, e) -> exposeCompiledMember asmName i, tr.TransformExpression e)
                }
            )
        EntryPoint = meta.EntryPoint |> Option.map tr.TransformStatement
    },
    match sp with
    | None -> [||]
    | Some t -> t.FileMap

let private localizeAddress (a: Address) =
    match a.Module with
    | WebSharperModule _ ->
        { a with Module = CurrentModule }
    | _ -> a

let rec private localizeCompiledMember m = 
    match m with
    | Static a -> Static <| localizeAddress a
    | Macro (td, p, Some r) ->
        Macro (td, p, Some (localizeCompiledMember r))
    | _ -> m

let private localizeCompiledField f =
    match f with
    | StaticField a -> StaticField <| localizeAddress a
    | _ -> f

type UpdateModuleToLocal() =
    inherit Transformer()

    override this.TransformGlobalAccess(a) =
        GlobalAccess <| localizeAddress a

let transformToLocalAddressInMetadata (meta: Info) =
    let tr = UpdateModuleToLocal() 
    { meta with 
        Classes = 
            meta.Classes |> Dict.map (fun c ->
                { c with 
                    Address = c.Address |> Option.map localizeAddress
                    Constructors = c.Constructors |> Dict.map (fun (i, p, e) -> localizeCompiledMember i, p, tr.TransformExpression e)    
                    Fields = c.Fields |> Dict.map (fun (i, p, t) -> localizeCompiledField i, p, t)
                    StaticConstructor = c.StaticConstructor |> Option.map (fun (a, e) -> localizeAddress a, tr.TransformExpression e)
                    Methods = c.Methods |> Dict.map (fun (i, p, e) -> localizeCompiledMember i, p, tr.TransformExpression e)
                    Implementations = c.Implementations |> Dict.map (fun (i, e) -> localizeCompiledMember i, tr.TransformExpression e)
                }
            )
        EntryPoint = meta.EntryPoint |> Option.map tr.TransformStatement
    }

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
            | None -> Application (Function ([], Return res), [], NonPure, None)
            | Some c -> Application (Function ([c], Return res), [Var var.Value], NonPure, None)        
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
    
    override this.VisitId i =
        if scope > 0 && defined.Contains i then 
            needed <- true

    override this.VisitFunction (args, body) =
        scope <- scope + 1
        this.VisitStatement body
        scope <- scope - 1

    override this.VisitExpression expr =
        if not needed then
            base.VisitExpression expr

    member this.Check(args: seq<Id>, values, expr) =
        for a, v in Seq.zip args values do
            if a.IsMutable && not (isTrivialValue v) then
                defined.Add a |> ignore
        this.VisitExpression expr  
        needed

let needsScoping args values body =
    NeedsScoping().Check(args, values, body)    
    
type HasNoThisVisitor() =
    inherit Visitor()

    let mutable ok = true

    override this.VisitThis() = ok <- false

    member this.Check(e) =
        this.VisitExpression e
        ok

/// A placeholder expression when encountering a translation error
/// so that collection of all errors can occur.
let errorPlaceholder = 
    Cast(Var TSType.Any, Value (String "$$ERROR$$"))

/// A transformer that tracks current source position
type TransformerWithSourcePos(comp: Metadata.ICompilation) =
    inherit Transformer()

    let mutable currentSourcePos = None

    member this.CurrentSourcePos = currentSourcePos

    member this.Error msg =
        comp.AddError(currentSourcePos, msg)
        errorPlaceholder

    member this.Warning msg =
        comp.AddWarning(currentSourcePos, msg)

    override this.TransformExprSourcePos (pos, expr) =
        let p = currentSourcePos 
        currentSourcePos <- Some pos
        let res = this.TransformExpression expr
        currentSourcePos <- p
        ExprSourcePos(pos, res)

    override this.TransformStatementSourcePos (pos, statement) =
        let p = currentSourcePos 
        currentSourcePos <- Some pos
        let res = this.TransformStatement statement
        currentSourcePos <- p
        StatementSourcePos(pos, res)

open IgnoreSourcePos

let containsVar v expr =
    CountVarOccurence(v).Get(expr) > 0

/// Checks if a predicate is true for all sub-expressions.
/// `checker` can return `None` for continued search
/// `Some true` to ignore sub-expressions of current node and
/// `Some false` to fail the check.
type ForAllSubExpr(checker) =
    inherit Visitor()
    let mutable ok = true

    override this.VisitExpression(e) =
        if ok then
            match checker e with
            | None -> base.VisitExpression e
            | Some true -> ()
            | Some false -> ok <- false

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

let callArraySlice =
    (Global ["Array"]).[Value (String "prototype")].[Value (String "slice")].[Value (String "call")]   

let sliceFromArguments slice =
    Application (callArraySlice, Arguments :: [ for a in slice -> !~ (Int a) ], Pure, None)

let (|Lambda|_|) e = 
    match e with
    | Function(args, Return body) -> Some (args, body, true)
    | Function(args, ExprStatement body) -> Some (args, body, false)
    | _ -> None

let (|SimpleFunction|_|) expr =
    // TODO : have typed versions in Runtime.ts
    //match IgnoreExprSourcePos expr with
    //| Function (_, I.Empty) ->
    //    Some <| Global [ "ignore" ]
    //| Function (x :: _, I.Return (I.Var y)) when x = y ->
    //    Some <| Global [ "id" ]
    //| Function (x :: _, I.Return (I.ItemGet(I.Var y, I.Value (Int 0), _))) when x = y ->
    //    Some <| Global [ "fst" ]
    //| Function (x :: _, I.Return (I.ItemGet(I.Var y, I.Value (Int 1), _))) when x = y ->
    //    Some <| Global [ "snd" ]
    //| Function (x :: _, I.Return (I.ItemGet(I.Var y, I.Value (Int 2), _))) when x = y ->
    //    Some <| Global [ "trd" ]
    //| _ -> None
    None

let (|AlwaysTupleGet|_|) tupledArg length expr =
    let (|TupleGet|_|) e =
        match e with 
        | ItemGet(Var t, Value (Int i), _) when t = tupledArg ->
            Some (int i)
        | _ -> None 
    let maxTupleGet = ref (length - 1)
    let checkTupleGet e =
        match e with 
        | TupleGet i -> 
            if i > !maxTupleGet then maxTupleGet := i
            Some true
        | Var t when t = tupledArg -> Some false
        | _ -> None
    if ForAllSubExpr(checkTupleGet).Check(expr) then
        Some (!maxTupleGet, (|TupleGet|_|))
    else
        None

let (|TupledLambda|_|) expr =
    match expr with
    | Lambda ([tupledArg], b, isReturn) ->
        // when the tuple itself is bound to a name, there will be an extra let expression
        let tupledArg, b =
            match b with
            | Let (newTA, Var t, b) when t = tupledArg -> 
                newTA, SubstituteVar(tupledArg, Var newTA).TransformExpression b
            | _ -> tupledArg, b
        let rec loop acc = function
            | Let (v, ItemGet(Var t, Value (Int i), _), body) when t = tupledArg ->
                loop ((int i, v) :: acc) body
            | body -> 
                if List.isEmpty acc then [], body else
                let m = Map.ofList acc
                [ for i in 0 .. (acc |> Seq.map fst |> Seq.max) -> 
                    match m |> Map.tryFind i with
                    | None -> Id.New(mut = false)
                    | Some v -> v 
                ], body
        let vars, body = loop [] b
        if containsVar tupledArg body then
            match body with
            | AlwaysTupleGet tupledArg vars.Length (maxTupleGet, (|TupleGet|_|)) ->
                let vars = 
                    if List.length vars > maxTupleGet then vars
                    else vars @ [ for k in List.length vars .. maxTupleGet -> Id.New(mut = false) ]
                Some (vars, body |> BottomUp (function TupleGet i -> Var vars.[i] | e -> e), isReturn)
            | _ ->                                                        
                // if we would use the arguments object for anything else than getting
                // a tuple item, convert it to an array
                if List.isEmpty vars then None else
                Some (vars, Let (tupledArg, sliceFromArguments [], body), isReturn)
        else
            if List.isEmpty vars then None else
            Some (vars, body, isReturn)
    | _ -> None

let (|CurriedLambda|_|) expr =
    let rec curr args expr =
        match expr with
        | Lambda ([], b, true) ->
            let a = Id.New(mut = false)
            curr (a :: args) b
        | Lambda ([a], b, true) ->
            curr (a.ToNonOptional() :: args) b
        | Lambda ([], b, false) ->
            if not (List.isEmpty args) then
                let a = Id.New(mut = false)
                Some (List.rev (a :: args), b, false) 
            else None
        | Lambda ([a], b, false) ->
            if not (List.isEmpty args) then
                Some (List.rev (a.ToNonOptional() :: args), b, false) 
            else None
        | _ -> 
            if List.length args > 1 then
                Some (List.rev args, expr, true)
            else None
    curr [] expr

let (|CurriedFunction|_|) expr =
    let rec curr args expr =
        match expr with
        | Lambda ([], b, true) ->
            let a = Id.New(mut = false)
            curr (a :: args) b
        | Lambda ([a], b, true) ->
            curr (a.ToNonOptional() :: args) b
        | Lambda ([], b, false) ->
            if not (List.isEmpty args) then
                let a = Id.New(mut = false)
                Some (List.rev (a :: args), ExprStatement b) 
            else None
        | Lambda ([a], b, false) ->
            if not (List.isEmpty args) then
                Some (List.rev (a.ToNonOptional() :: args), ExprStatement b) 
            else None
        | Function ([], b) ->
            if not (List.isEmpty args) then
                let a = Id.New(mut = false)
                Some (List.rev (a :: args), b) 
            else None
        | Function ([a], b) ->
            if not (List.isEmpty args) then
                Some (List.rev (a.ToNonOptional() :: args), b) 
            else None
        | _ -> 
            if List.length args > 1 then
                Some (List.rev args, Return expr)
            else None
    curr [] expr

let (|CurriedApplicationSeparate|_|) expr =
    let rec appl args expr =
        match expr with
        | Application(func, [], p, Some _) ->
            appl (Value Null :: args) func 
        | Application(func, [a], p, Some _) ->
            appl (a :: args) func 
        | CurriedApplication(func, a) ->
            appl (a @ args) func
        | _ ->
            if args.Length > 1 then
                Some (expr, args)
            else None
    appl [] expr

type OptimizeLocalTupledFunc(var, tupling) =
    inherit Transformer()

    override this.TransformVar(v) =
        if v = var then
            let t = Id.New(mut = false)
            Lambda([t], Application(Var v, List.init tupling (fun i -> (Var t).[Value (Int i)]), NonPure, Some tupling))
        else Var v  

    override this.TransformApplication(func, args, isPure, length) =
        match func with
        | I.Var v when v = var ->                    
            match args with
            | [ I.NewArray ts ] when ts.Length = tupling ->
                Application (func, ts |> List.map this.TransformExpression, isPure, Some tupling)
            | [ t ] ->
                Application((Var v).[Value (String "apply")], [ Value Null; this.TransformExpression t ], isPure, None)               
            | _ -> failwith "unexpected tupled FSharpFunc applied with multiple arguments"
        | _ -> base.TransformApplication(func, args, isPure, length)

let curriedApplication func args =
    let func, args =
        match func with
        | CurriedApplicationSeparate (f, fa) -> f, fa @ args
        | _ -> func, args
    match List.length args with
    | 0 -> func
    | 1 -> Application (func, args, NonPure, Some 1)
    | _ -> CurriedApplication(func, args)

type OptimizeLocalCurriedFunc(var, currying) =
    inherit Transformer()

    override this.TransformVar(v) =
        if v = var then
            let ids = List.init currying (fun _ -> Id.New(mut = false))
            CurriedLambda(ids, Application(Var v, ids |> List.map Var, NonPure, Some currying))    
        else Var v  

    override this.TransformCurriedApplication(func, args) =
        match func with
        | Var v when v = var ->
            if args.Length >= currying then
                let cargs, moreArgs = args |> List.splitAt currying
                let f = Application(func, cargs |> List.map this.TransformExpression, NonPure, Some currying)  
                curriedApplication f (moreArgs |> List.map this.TransformExpression)
            else
                base.TransformCurriedApplication(func, args)             
        | _ -> base.TransformCurriedApplication(func, args)

#if DEBUG
let mutable logTransformations = false
#endif
