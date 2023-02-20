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
    | Base
    | Var _
    | Value _
    | Function _ 
    | GlobalAccess _
    | JSThis
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
        -> a |> List.map (fun (_, _, v) -> removePureParts v) |> CombineExpressions
    | LetRec (a, b) 
        -> LetRec (a, removePureParts b)
    | Application(a, b, { Purity = NoSideEffect | Pure }) ->
        CombineExpressions ((a :: b) |> List.map removePureParts)
    | _ -> expr

/// Determine if expression has no side effect
let rec isPureExpr expr =
    match expr with
    | Undefined
    | Base
    | Var _
    | Value _
    | Function _ 
    | GlobalAccess _
    | JSThis
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
    | Coerce (a, _, _)
        -> isPureExpr a     
    | Object a 
        -> List.forall (fun (_, _, v) -> isPureExpr v) a 
    | LetRec (a, b) 
        -> List.forall (snd >> isPureExpr) a && isPureExpr b
    | Application(a, b, { Purity = NoSideEffect | Pure }) ->
        isPureExpr a && List.forall isPureExpr b    
    | _ -> false

let isPureFunction expr =
    match IgnoreExprSourcePos expr with
    | Function (_, _, _, (I.Return body | I.ExprStatement body)) -> isPureExpr body
    | Function (_, _, _, (I.Empty | I.Block [])) -> true
    | _ -> false

let isTrivialFunction (args: Id list) body =
    let rec check expr =
        match IgnoreExprSourcePos expr with
        | Var v -> args |> List.contains v
        | NewTuple (e, _) -> e |> List.forall check
        | _ -> false
    check body

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
    //| Function (args, _, _, (I.Empty | I.Block [])) -> true
    //| Function (args, _, _, (I.Return body)) ->
    //    isTrivialValue body || isTrivialFunction args body
    | _ -> false

/// Determine if expression has no side effect and value does not depend on execution order
let rec isStronglyPureExpr expr =
    match expr with
    | Undefined
    | Base
    | Value _
    | Function _
    | GlobalAccess _
    | JSThis
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
    | Coerce (a, _, _)
        -> isStronglyPureExpr a     
    | Object a 
        -> List.forall (fun (_, _, v) -> isStronglyPureExpr v) a 
    | LetRec (a, b) 
        -> List.forall (snd >> isStronglyPureExpr) a && isStronglyPureExpr b
    | Application(a, b, { Purity = Pure }) ->
        isStronglyPureExpr a && List.forall isStronglyPureExpr b    
    | _ -> false

let getFunctionPurity expr =
    match IgnoreExprSourcePos expr with
    | Function (_, _, _, (I.Return body | I.ExprStatement body)) -> 
        if isStronglyPureExpr body then
            Pure
        elif isPureExpr body then
            NoSideEffect
        else 
            NonPure
    | Function (_, _, _, (I.Empty | I.Block [])) -> Pure
    | _ -> NonPure

/// Checks if a specific Id is mutated or accessed within a function body
/// (captured) inside an Expression
type private NotMutatedOrCaptured(v, ?allowApplication) =
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

    override this.VisitFuncDeclaration(i, p, t, b, g) =
        scope <- scope + 1
        base.VisitFuncDeclaration(i, p, t, b, g)
        scope <- scope - 1

    override this.VisitFunction(a, i, t, b) =
        scope <- scope + 1
        base.VisitFunction(a, i, t, b)
        scope <- scope - 1

    override this.VisitExpression(a) =
        if ok then base.VisitExpression(a)

    override this.VisitId a =
        if a = v && scope > 0 then ok <- false

    override this.VisitApplication(a, b, c) =
        if defaultArg allowApplication false then
            match a with
            | I.Var av when av = v ->
                b |> List.iter this.VisitExpression
            | _ ->
                base.VisitApplication(a, b, c)
        else
            base.VisitApplication(a, b, c)

    member this.Check(a) =
        this.VisitExpression(a)
        ok

let notMutatedOrCaptured (v: Id)  expr =
    NotMutatedOrCaptured(v).Check(expr)   

let notMutatedOrCapturedExceptAppl (v: Id)  expr =
    NotMutatedOrCaptured(v, true).Check(expr)   

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

type CollectUnusedVars(vs : HashSet<Id>) =
    inherit Visitor()

    new (vs: seq<Id>) = CollectUnusedVars(HashSet vs)
    
    override this.VisitId(a) =
        vs.Remove(a) |> ignore

    member this.Get(e) =
        if vs.Count > 0 then
            this.VisitExpression(e) 
        vs

    member this.GetSt(s) =
        if vs.Count > 0 then
            this.VisitStatement(s) 
        vs

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
            | Base
            | Value _
            | JSThis
            | GlobalAccess _
                -> ()
            | Sequential a
            | NewTuple (a, _) ->
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
            | Coerce (a, _, _)
                -> eval a
            | Object a 
                -> List.iter (fun (_, _, v) -> eval v) a 
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
                    stop()
            | GlobalAccessSet (_, a) ->
                eval a
                stop()
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
            | Application(a, b, c) ->
                eval a
                List.iter eval b
                if c.Purity = NonPure then stop()
            | New(a, _, b) ->
                eval a
                List.iter eval b
                stop()
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
            | Function(_, _, _, a) ->
                if not <| varsNotUsed.GetSt(a) then fail()
            | StatementExpr (a, _) ->
                evalSt a
            | Await _
            | ChainedCtor _
            | Ctor _
            | CallNeedingMoreArgs _
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
            | ObjectExpr _
            | ClassExpr _
                -> fail()
    
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
            | FuncDeclaration(_, _, _, a, _) -> 
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
            | Alias _
            | Class _
            | ClassConstructor _
            | ClassMethod _
            | ClassProperty _
            | ClassStatic _
            | Declare _
            | ExportDecl _
            | Import _
            | Interface _
            | XmlComment _
                -> fail()      
               
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

    member this.RefreshVar(i: Id) =
        let n = i.Clone()
        refresh.Add(i, n)
        n

    override this.TransformFunction(args, isArr, typ, body) =
        Function(args |> List.map this.RefreshVar, isArr, typ, this.TransformStatement body)

    override this.TransformVarDeclaration(i, v) =
        VarDeclaration(this.RefreshVar i, this.TransformExpression v)

    override this.TransformLet(i, v, b) =
        Let(this.RefreshVar i, this.TransformExpression v, this.TransformExpression b)

    override this.TransformLetRec(vs, b) =
        LetRec(vs |> List.map (fun (i, v) -> this.RefreshVar i, this.TransformExpression v), this.TransformExpression b)

    override this.TransformNewVar(i, v) =
        NewVar(this.RefreshVar i, this.TransformExpression v)

    override this.TransformFuncDeclaration(i, a, t, b, ty) =
        FuncDeclaration(this.RefreshVar i, a, t, this.TransformStatement b, ty)

    override this.TransformId i =
        match refresh.TryFind i with
        | Some n -> n
        | _ -> i

type TransformBaseCall(f) =
    inherit Transformer()

    override this.TransformApplication(a, b, c) =
        match a with
        | Base ->
            f b
        | _ ->
            base.TransformApplication(a, b, c)

let funcFromLambda(funcId, thisVar, isRec, args, body, ts) =
    if Option.isNone thisVar && not isRec then
        VarDeclaration(funcId, Function(args, None, None, body))
    else
        FuncDeclaration(funcId, args, thisVar, body, ts)
        
let makeExprInline (vars: Id list) expr =
    if varEvalOrder vars expr then
        SubstituteVars(vars |> Seq.mapi (fun i a -> a, Hole i) |> dict).TransformExpression(expr)
    else
        List.foldBack (fun (v, h) body ->
            Let (v, h, body)    
        ) (vars |> List.mapi (fun i a -> a, Hole i)) expr

let isFunctionNameForInterface (t: TypeDefinition) =
    "is" + (t.Value.FullName.Split([| '.'; '+' |]) |> Array.last).Split('`')[0]

module Definitions =
    open WebSharper.InterfaceGenerator.Type

    // Private static field for single-case unions.
    let SingletonUnionCase name typ =
        Method {
            MethodName = "_unique_" + name
            Parameters = []
            ReturnType = typ
            Generics = 0
        }

    let StringFormat1 =
        Method {
            MethodName = "Format"
            Parameters = [ NonGenericType Definitions.String; NonGenericType Definitions.Obj ]
            ReturnType = NonGenericType Definitions.String
            Generics = 0
        }
    
let rec (|IsClientCall|_|) (e: Expression) =
    match e with
    | I.Call (None, td, m, []) ->
        if td.Entity.Value.FullName = "WebSharper.Pervasives" then
            let mName = m.Entity.Value.MethodName 
            if mName = "IsClient" || mName = "get_IsClient" then
                Some true
            else None
        else None
    | I.Call (None, td, m, [ a ]) ->
        let tName = td.Entity.Value.FullName
        if tName = "Microsoft.FSharp.Core.Operators" && m.Entity.Value.MethodName = "Not" 
            || tName = "System.Boolean" && m.Entity.Value.MethodName = "op_LogicalNot"
        then
            match a with 
            | IsClientCall c -> Some (not c)
            | _ -> None
        else None
    | I.Unary(UnaryOperator.Not, a ) ->
        match a with 
        | IsClientCall c -> Some (not c)
        | _ -> None
    | _ -> None

//let ignoreSystemObject td =
//    if td = Definitions.Obj || td = Definitions.ValueType then None else Some td

let getConcreteType t =
    match t with
    | ConcreteType ct -> ct
    | t -> failwithf "invalid base type or interface form: %O" t

let ignoreSystemObject t =
    let td = t.Entity
    if td = Definitions.Obj || td = Definitions.ValueType then None else Some t

open WebSharper.Core.Metadata

module Resolve =
    open System.Collections.Generic

    let newName (name: string) =
        match name.LastIndexOf '_' with
        | -1 -> name + "_1"
        | i -> 
            match System.Int32.TryParse (name.Substring(i + 1)) with
            | true, n -> name.Substring(0, i) + "_" + string (n + 1)
            | _ -> 
                if name.EndsWith "_" then name + "1" else name + "_1"
    
    type Class = 
        {
            InstanceMembers: HashSet<string * MemberKind>
            StaticMembers: HashSet<string * MemberKind>
            Functions: HashSet<string>
            SubClasses: ResizeArray<Class>
        } with 
        static member New() =
            {
                InstanceMembers = HashSet()
                StaticMembers = HashSet()
                Functions = HashSet()
                SubClasses = ResizeArray()
            }

    let rec addInstanceMemberToClass (c: Class) name =
        [
            yield c.InstanceMembers.Add name 
            for s in c.SubClasses do
                yield addInstanceMemberToClass s name
        ]
        |> List.forall id
    
    let addStaticMemberToClass (c: Class) name =
        c.StaticMembers.Add name 

    let addFunctionToClass (c: Class) name =
        c.Functions.Add name 

    type Resolver() =
        let classes = Dictionary<TypeDefinition, Class>()

        //let rec getSubAddress (root: list<string>) (name: string) node =
        //    let name = name.Replace('.', '_')
        //    let tryAddr = Hashed (name :: root)
        //    match statics.TryFind tryAddr, node with
        //    | Some _, Member
        //    | Some Member, _ 
        //    | Some Class, Class -> getSubAddress root (newName name) node
        //    | Some (Class | Module), Module -> tryAddr
        //    | _ -> 
        //        statics.[tryAddr] <- node
        //        tryAddr

        //let getExactSubAddress (root: list<string>) (name: string) node =
        //    let tryAddr = Hashed (name :: root)
        //    match statics.TryFind tryAddr, node with
        //    | Some (Class | Module), Module -> true
        //    | Some Module, Class
        //    | None, _ -> 
        //        statics.[tryAddr] <- node
        //        true
        //    | _ -> false

        //let rec getFullAddress (address: list<string>) node =
        //    match address with
        //    | [] -> failwith "Empty address"
        //    | [ x ] -> getSubAddress [] x node
        //    | h :: r -> getSubAddress ((getFullAddress r Module).Value) h node

        //let rec getExactFullAddress (address: list<string>) node =
        //    match address with
        //    | [] -> failwith "Empty address"
        //    | [ x ] -> getExactSubAddress [] x node
        //    | h :: r -> 
        //        getExactFullAddress r Module && getExactSubAddress r h node

        member this.AddClass (typ, bTyp) =
            let c = Class.New()
            match bTyp with
            | Some bTyp ->
                let bc = classes.[bTyp]
                bc.SubClasses.Add c
            | None -> ()
            try
                classes.Add(typ, c)
            with _ ->
                failwithf "Failed to add prototype for %A" typ

        member this.HasClass typ =
            classes.ContainsKey typ
        
        member this.LookupClass typ =
            classes.[typ]

        //member this.ExactClassAddress(addr: list<string>, hasPrototype) =
        //    getExactFullAddress addr (if hasPrototype then Class else Module)
        //    && if hasPrototype then getExactSubAddress addr "prototype" Member else true 

        //member this.ClassAddress(typ: TypeDefinitionInfo, hasPrototype) =
        //    let removeGen (n: string) =
        //        match n.LastIndexOf '`' with
        //        | -1 -> n
        //        | i -> n.[.. i - 1]
        //    let addr = typ.FullName.Split('.', '+') |> List.ofArray |> List.map removeGen |> List.rev 
        //    let res = getFullAddress addr (if hasPrototype then Class else Module)
        //    if hasPrototype then
        //        getExactSubAddress addr "prototype" Member |> ignore    
        //    res

        //member this.ExactStaticAddress addr =
        //    getExactFullAddress addr Member 

        //member this.StaticAddress addr =
        //    getFullAddress addr Member 
                     
    let rec getRenamed name (s: HashSet<string>) =
        if s.Add name then name else getRenamed (newName name) s

    let rec getRenamedWithKind name kind (s: HashSet<string * MemberKind>) =
        let hasConflict = 
            match kind with
            | MemberKind.Simple ->
                s.Contains (name, MemberKind.Simple) 
                || s.Contains (name, MemberKind.Getter) 
                || s.Contains (name, MemberKind.Setter)
            | _ ->
                s.Contains (name, MemberKind.Simple) 
                || s.Contains (name, kind) 
        if hasConflict then 
            getRenamedWithKind (newName name) kind s
        else
            s.Add (name, kind) |> ignore
            name

    let rec getRenamedInstanceMemberForClass name kind c =
        let rec isNameOk (c: Class) =
            let hasConflict = 
                let s = c.InstanceMembers
                match kind with
                | MemberKind.Simple ->
                    s.Contains (name, MemberKind.Simple) 
                    || s.Contains (name, MemberKind.Getter) 
                    || s.Contains (name, MemberKind.Setter)
                | _ ->
                    s.Contains (name, MemberKind.Simple) 
                    || s.Contains (name, kind) 
            seq {
                yield not hasConflict
                for sc in c.SubClasses do 
                    yield isNameOk sc
            }
            |> Seq.forall id
        if isNameOk c then
            addInstanceMemberToClass c (name, kind) |> ignore
            name
        else
            getRenamedInstanceMemberForClass (newName name) kind c

    let rec getRenamedStaticMemberForClass name kind c =
        let hasConflict = 
            let s = c.StaticMembers
            match kind with
            | MemberKind.Simple ->
                s.Contains (name, MemberKind.Simple) 
                || s.Contains (name, MemberKind.Getter) 
                || s.Contains (name, MemberKind.Setter)
            | _ ->
                s.Contains (name, MemberKind.Simple) 
                || s.Contains (name, kind) 
        if not hasConflict then
            addStaticMemberToClass c (name, kind) |> ignore
            name
        else
            getRenamedStaticMemberForClass (newName name) kind c

    let rec getRenamedFunctionForClass name c =
        if not (c.Functions.Contains name) then
            addFunctionToClass c name |> ignore
            name
        else
            getRenamedFunctionForClass (newName name) c
       
    let rec getRenamedInDict name v (s: Dictionary<string, _>) =
        if not (s.ContainsKey name) then
            s.Add(name, v) 
            name
        else
            getRenamedInDict (newName name) v s

    let addInherits (r: Resolver) (classes: IDictionary<TypeDefinition, ClassInfo>) =
        let rec inheritMembers typ (cls: ClassInfo) =
            if not (r.HasClass typ) then
                match cls.BaseClass with
                | None ->
                    r.AddClass(typ, None)    
                | Some b ->
                    // assembly containing base class may not be referenced
                    match classes.TryFind b.Entity with
                    | None ->
                        r.AddClass(typ, None)
                    | Some bCls ->
                        inheritMembers b.Entity bCls  
                        r.AddClass(typ, Some b.Entity)
        for KeyValue(typ, cls) in classes do
            inheritMembers typ cls        

let getAllAddresses (meta: Info) =
    let r = Resolve.Resolver()
    let classes =
        meta.Classes |> Dict.choose (fun (_, _, cls) -> cls)
    Resolve.addInherits r classes
    // add members
    for KeyValue(typ, cls) in classes do
        if typ.Value.FullName.StartsWith "Generated$" then () else
        let pr = if cls.HasWSPrototype then Some (r.LookupClass typ) else None 
        let rec addMember (m: CompiledMember) =
            match m with
            | Instance (n, k) -> pr |> Option.iter (fun p -> Resolve.addInstanceMemberToClass p (n, k) |> ignore)            
            | Macro (_, _, Some m) -> addMember m
            | _ -> ()
        for m in cls.Constructors.Values do addMember m.CompiledForm
        for f in cls.Fields.Values do
            match f.CompiledForm with
            | InstanceField n 
            | OptionalField n -> pr |> Option.iter (fun p -> Resolve.addInstanceMemberToClass p (n, MemberKind.Simple) |> ignore)
            | IndexedField _ -> ()
            | _ -> ()
        for m in cls.Implementations.Values do addMember m.CompiledForm
        for m in cls.Methods.Values do addMember m.CompiledForm
    r
 
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

    let rec refreshNotInline i e =
        match i with
        | Inline _ -> e
        | Macro (_, _, Some f) -> refreshNotInline f e
        | _ -> r.TransformExpression e

    i.MapClasses((fun c ->
        { c with
            Constructors = 
                c.Constructors |> Dict.map (fun c -> { c with Expression = refreshNotInline c.CompiledForm c.Expression })
            StaticConstructor = 
                c.StaticConstructor |> Option.map (fun b -> r.TransformStatement b) 
            Methods = 
                c.Methods |> Dict.map (fun m -> { m with Expression = refreshNotInline m.CompiledForm m.Expression })
            Implementations = 
                c.Implementations |> Dict.map (fun i -> { i with Expression = r.TransformExpression i.Expression })
        }), r.TransformStatement)

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
    let interfaces = Dictionary<_,_>() 
    let rec getOrAddClass td =
        match classes.TryGetValue td with
        | true, (_, _, x) -> x
        | false, _ ->
            match meta.Classes.TryGetValue td with
            | true, (a, ct, Some cls) ->
                cls.BaseClass |> Option.iter (fun c -> getOrAddClass c.Entity |> ignore)
                let methods = cls.Methods |> Dict.filter (fun _ m ->
                    // keep abstract members
                    match m.CompiledForm, m.Expression with
                    | CompiledMember.Instance _, IgnoreExprSourcePos Undefined -> true
                    | _ -> false
                )
                let cls = 
                    { cls with
                        Constructors = Dictionary<_,_>()
                        Methods = methods
                        Implementations = Dictionary<_,_>()
                    }
                classes.Add(td, (a, ct, Some cls))
                Some cls
            | true, (_, _, None as actNone) ->
                classes.Add(td, actNone)
                None
            | _ ->
                eprintfn "WebSharper warning: Assembly needed for bundling but is not referenced: %s (missing type: %s)"
                    td.Value.Assembly td.Value.FullName
                None
    let moveToDict (fromDic: IDictionary<_,_>) (toDic: IDictionary<_,_>) key =
        match fromDic.TryGetValue(key) with
        | true, value -> toDic.[key] <- value
        | false, _ ->
            eprintfn "WebSharper warning: Member implementation not found during bundling for %A" key
    for n in nodes do
        match n with
        | AbstractMethodNode (td, m)
        | MethodNode (td, m) -> 
            getOrAddClass td |> Option.iter (fun cls -> m |> moveToDict (meta.ClassInfo(td).Methods) cls.Methods)
        | ConstructorNode (td, c) -> 
            getOrAddClass td |> Option.iter (fun cls -> c |> moveToDict (meta.ClassInfo(td).Constructors) cls.Constructors)
        | ImplementationNode (td, i, m) ->
            try
                //if td = Definitions.Obj then () else
                getOrAddClass td |> Option.iter (fun cls -> (i, m) |> moveToDict (meta.ClassInfo(td).Implementations) cls.Implementations)
            with _ ->
                failwithf "implementation node not found %A" n
        | TypeNode td ->
            if meta.Classes.ContainsKey td then 
                getOrAddClass td |> ignore 
            if meta.Interfaces.ContainsKey td then
                interfaces[td] <- meta.Interfaces[td]
        | _ -> ()
    let classes =
        classes |> Dict.map (
            function 
            | a, cti, Some ({ Implements = _ :: _ } as cls) ->
                let clsTrimmedImplements =
                    { cls with
                        Implements = cls.Implements |> List.filter (fun i -> interfaces.ContainsKey(i.Entity))
                    }
                a, cti, Some clsTrimmedImplements
            | ci -> ci
        )

    { meta with 
        Classes = classes
        Interfaces = interfaces
    }

//let private exposeAddress asmName (a: Address) =
//    match a.Module with
//    | CurrentModule ->
//        { a with Module = WebSharperModule asmName }
//    | _ -> a

//type RemoveSourcePositionsAndUpdateModule (asmName) =
//    inherit RemoveSourcePositions ()

//    override this.TransformGlobalAccess(a) =
//        GlobalAccess <| exposeAddress asmName a

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

//type TransformSourcePositionsAndUpdateModule(asmName) =
//    inherit TransformSourcePositions(asmName)

//    override this.TransformGlobalAccess(a) =
//        GlobalAccess <| a

//let rec private exposeCompiledMember asmName m = 
//    match m with
//    | Static (a, k) -> Static (exposeAddress asmName a, k)
//    | Macro (td, p, Some r) ->
//        Macro (td, p, Some (exposeCompiledMember asmName r))
//    | _ -> m

//let private exposeCompiledField asmName f =
//    match f with
//    | StaticField a -> StaticField <| exposeAddress asmName a
//    | _ -> f

let transformAllSourcePositionsInMetadata asmName isRemove (meta: Info) =
    let tr, sp = 
        if isRemove then
            RemoveSourcePositions() :> Transformer, None 
        else
            let tr = TransformSourcePositions(asmName)
            tr :> _, Some tr
    { meta with 
        Classes = 
            meta.Classes |> Dict.map (fun (a, ct, c) ->
                a, ct,
                c |> Option.map (fun c ->
                    { c with 
                        Constructors = c.Constructors |> Dict.map (fun c -> { c with Expression = tr.TransformExpression c.Expression })    
                        StaticConstructor = c.StaticConstructor |> Option.map (fun s -> tr.TransformStatement s)
                        Methods = c.Methods |> Dict.map (fun m -> { m with Expression = tr.TransformExpression m.Expression })
                        Implementations = c.Implementations |> Dict.map (fun i -> { i with Expression = tr.TransformExpression i.Expression })
                    }
                )
            )
    },
    match sp with
    | None -> [||]
    | Some t -> t.FileMap

//let private localizeAddress (a: Address) =
//    match a.Module with
//    | WebSharperModule _ ->
//        { a with Module = CurrentModule }
//    | _ -> a

//let rec private localizeCompiledMember m = 
//    match m with
//    | Static (a, k) -> Static (localizeAddress a, k)
//    | Macro (td, p, Some r) ->
//        Macro (td, p, Some (localizeCompiledMember r))
//    | _ -> m

//let private localizeCompiledField f =
//    match f with
//    | StaticField a -> StaticField <| localizeAddress a
//    | _ -> f

//type UpdateModuleToLocal() =
//    inherit Transformer()

//    override this.TransformGlobalAccess(a) =
//        GlobalAccess <| localizeAddress a

//let transformToLocalAddressInMetadata (meta: Info) =
//    let tr = UpdateModuleToLocal() 
//    { meta with 
//        Classes = 
//            meta.Classes |> Dict.map (fun (a, ct, c) ->
//                localizeAddress a, ct,
//                c |> Option.map (fun c ->
//                    { c with 
//                        Constructors = c.Constructors |> Dict.map (fun (i, p, e) -> localizeCompiledMember i, p, tr.TransformExpression e)    
//                        Fields = c.Fields |> Dict.map (fun (i, p, t) -> localizeCompiledField i, p, t)
//                        StaticConstructor = c.StaticConstructor |> Option.map (fun s -> tr.TransformStatement s)
//                        Methods = c.Methods |> Dict.map (fun (i, p, c, e) -> localizeCompiledMember i, p, c, tr.TransformExpression e)
//                        Implementations = c.Implementations |> Dict.map (fun (i, e) -> localizeCompiledMember i, tr.TransformExpression e)
//                    }
//                )
//            )
//    }

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

    override this.TransformFunction (args, isArr, typ, body) =
        scope <- scope + 1
        let res = Function (args, isArr, typ, this.TransformStatement body)
        scope <- scope - 1
        res

    member this.CaptureValueIfNeeded expr =
        let res = this.TransformExpression expr  
        if capture then
            match captVal with
            | None -> Appl (Function ([], None, None, Return res), [], NonPure, None)
            | Some c -> Appl (Function ([c], None, None, Return res), [Var var.Value], NonPure, None)        
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

    override this.VisitFunction (args, thisVar, typ, body) =
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
    
//type HasNoThisVisitor() =
//    inherit Visitor()

//    let mutable ok = true

//    override this.VisitThis() = 
//        ok <- false

//    override this.VisitFunction (_, isArrow, _, _) = ()

//    override this.VisitFuncDeclaration (_, _, _, _) = ()
    
//    member this.Check(e) =
//        this.VisitStatement e
//        ok

/// A placeholder expression when encountering a translation error
/// so that collection of all errors can occur.
let errorPlaceholder = 
    Cast(TSType.Any, Value (String "$$ERROR$$"))

/// A transformer that tracks current source position
type TransformerWithSourcePos(comp: Metadata.ICompilation) =
    inherit Transformer()

    let mutable currentSourcePos = None

    member this.CurrentSourcePos = currentSourcePos
    
    member val FailOnError = false with get, set

    member this.Error msg =
        // for trait calls, if the target class is macroed, we run in FailOnError=true mode to catch errors and discard trial
        if this.FailOnError then
            failwithf "Experimental compilation failed"
        else
            comp.AddError(currentSourcePos, msg)
            errorPlaceholder

    member this.Warning msg =
        if not this.FailOnError then
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

let (|Lambda|_|) e = 
    match e with
    | Function(args, None, typ, Return body) -> Some (args, typ, body, true)
    | Function(args, None, typ, ExprStatement body) -> Some (args, typ, body, false)
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

let rec (|NewVars|_|) expr =
    let (|SingleNewVar|_|) e =
        match e with
        | NewVar(i, Undefined) -> Some (i, None)
        | NewVar(i, v) -> Some (i, Some v)
        | _ -> None
    match expr with
    | SingleNewVar r -> Some [ r ]
    | Sequential s ->
        let m = s |> List.map (|SingleNewVar|_|)
        if m |> List.forall Option.isSome then
            Some (m |> List.map Option.get)
        else 
            None
    | _ -> None

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
    | Lambda ([tupledArg], ret, b, isReturn) when tupledArg.IsTuple ->
        // when the tuple itself is bound to a name, there will be an extra let expression
        let tupledArg, b =
            match b with
            | Let (newTA, Var t, b) when t = tupledArg -> 
                newTA, SubstituteVar(tupledArg, Var newTA).TransformExpression b
            | _ -> tupledArg, b
        let rec loop acc = function
            | Let (v, ItemGet(Var t, Value (Int i), _), body) when t = tupledArg ->
                loop ((i, v) :: acc) body
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
                Some (vars, ret, body |> BottomUp (function TupleGet i -> Var vars.[i] | e -> e), isReturn)
            | _ ->                                                        
                // if we would use the arguments object for anything else than getting
                // a tuple item, convert it to an array
                //if List.isEmpty vars then None else
                //Some (vars, ret, Let (tupledArg, sliceFromArguments [], body), isReturn)
                
                // we don't want arguments hack any more, does not work inside => functions
                None
        else
            if List.isEmpty vars then None else
            Some (vars, ret, body, isReturn)
    | _ -> None

let (|CurriedLambda|_|) expr =
    let rec curr args ret expr =
        match expr with
        | Lambda ([], ret, b, true) ->
            let a = Id.New(mut = false)
            curr (a :: args) ret b
        | Lambda ([a], ret, b, true) ->
            curr (a.ToNonOptional() :: args) ret b
        | Lambda ([], ret, b, false) ->
            if not (List.isEmpty args) then
                let a = Id.New(mut = false)
                Some (List.rev (a :: args), ret, b, false) 
            else None
        | Lambda ([a], ret, b, false) ->
            if not (List.isEmpty args) then
                Some (List.rev (a.ToNonOptional() :: args), ret, b, false) 
            else None
        | _ -> 
            if List.length args > 1 then
                Some (List.rev args, ret, expr, true)
            else None
    curr [] None expr

let (|CurriedFunction|_|) expr =
    let rec curr args ret expr =
        match expr with
        | Lambda ([], ret, b, true) ->
            let a = Id.New(mut = false)
            curr (a :: args) ret b
        | Lambda ([a], ret, b, true) ->
            curr (a.ToNonOptional() :: args) ret b
        | Lambda ([], ret, b, false) ->
            if not (List.isEmpty args) then
                let a = Id.New(mut = false)
                Some (List.rev (a :: args), ret, ExprStatement b) 
            else None
        | Lambda ([a], ret, b, false) ->
            if not (List.isEmpty args) then
                Some (List.rev (a.ToNonOptional() :: args), ret, ExprStatement b) 
            else None
        | Function ([], None, ret, b) ->
            if not (List.isEmpty args) then
                let a = Id.New(mut = false)
                Some (List.rev (a :: args), ret, b) 
            else None
        | Function ([a], None, ret, b) ->
            if not (List.isEmpty args) then
                Some (List.rev (a.ToNonOptional() :: args), ret, b) 
            else None
        | _ -> 
            if List.length args > 1 then
                Some (List.rev args, ret, Return expr)
            else None
    curr [] None expr

let (|CurriedApplicationSeparate|_|) expr =
    let rec appl args expr =
        match expr with
        | Application(func, [], { Purity = p; KnownLength = Some _ }) ->
            appl ((true, Value Null) :: args) func 
        | Application(func, [a], { Purity = p; KnownLength = Some _ }) ->
            // TODO : what if a has type unit but has side effect?
            appl ((false, a) :: args) func 
        | CurriedApplication(func, a) ->
            appl (a @ args) func
        | _ ->
            if args.Length > 1 then
                Some (expr, args)
            else None
    appl [] expr

type OptimizeLocalTupledFunc(var: Id , tupling) =
    inherit Transformer()

    let tupleAndRetType = 
        match var.VarType with
        | Some (FSharpFuncType (ts, ret)) ->
           Some (ts, ret)
        | _ -> None

    override this.TransformVar(v) =
        if v = var then
            let t = Id.New(mut = false, ?typ = Option.map fst tupleAndRetType)
            Lambda([t], Option.map snd tupleAndRetType, Appl(Var v, List.init tupling (fun i -> ItemGet(Var t, Value (Int i), Pure)), NonPure, Some tupling))
        else Var v  

    override this.TransformApplication(func, args, info) =
        match func with
        | I.Var v when v = var ->                    
            match args with
            | [ I.NewArray ts ] when ts.Length = tupling ->
                Application (func, ts |> List.map this.TransformExpression, { info with KnownLength = Some tupling })
            | [ t ] ->
                Application ((Var v).[Value (String "apply")], [ Value Null; this.TransformExpression t ], { info with KnownLength = None })               
            | _ -> failwith "unexpected tupled FSharpFunc applied with multiple arguments"
        | _ -> base.TransformApplication(func, args, info)

let applyUnitArg func a =
    match IgnoreExprSourcePos a with
    | Undefined | Value Null ->
        Appl (func, [], NonPure, Some 0)
    | _ ->
        // if argument expression is not trivial, it might have a side effect which should
        // be ran before the application but after evaluating the function
        let x = Id.New(mut = false)
        Let (x, func, Sequential [a; Appl (Var x, [], NonPure, Some 0)])

let applyFSharpArg func (isUnit, a) =
    if isUnit then
        applyUnitArg func a
    else
        Appl (func, [ a ], NonPure, Some 1)

let curriedApplication func (args: (bool * Expression) list) =
    let func, args =
        match func with
        | CurriedApplicationSeparate (f, fa) -> f, fa @ args
        | _ -> func, args
    match args with
    | [] -> func
    | [ a ] -> applyFSharpArg func a
    | _ -> CurriedApplication(func, args)

type OptimizeLocalCurriedFunc(var: Id, currying) =
    inherit Transformer()

    let types =
        let rec getTypes acc i t =
            if i = 0 then List.rev acc, t else
            match t with
            | FSharpFuncType (a, r) -> getTypes (a :: acc) (i - 1) r
            | TypeHelpers.OptimizedClosures3 (a1, a2, r) -> getTypes (a2 :: a1 :: acc) (i - 2) r
            | TypeHelpers.OptimizedClosures4 (a1, a2, a3, r) -> getTypes (a3 :: a2 :: a1 :: acc) (i - 3) r
            | TypeHelpers.OptimizedClosures5 (a1, a2, a3, a4, r) -> getTypes (a4 :: a3 :: a2 :: a1 :: acc) (i - 4) r
            | TypeHelpers.OptimizedClosures6 (a1, a2, a3, a4, a5, r) -> getTypes (a5 :: a4 :: a3 :: a2 :: a1 :: acc) (i - 5) r
            | _ -> failwithf "Trying to optimize currification of a non-function type: %A for var %s" t (var.Name |> Option.defaultValue "noname")
        var.VarType |> Option.map (getTypes [] currying)

    override this.TransformVar(v) =
        if v = var then
            let ids, retType =
                match types with
                | Some (argTypes, retType) -> argTypes |> List.map (fun t -> Id.New(mut = false, typ = t)), Some retType
                | None -> List.init currying (fun i -> Id.New(mut = false)), None
            CurriedLambda(ids, retType, Appl(Var v, ids |> List.map Var, NonPure, Some currying))
        else Var v  

    override this.TransformCurriedApplication(func, args) =
        match func with
        | Var v when v = var ->
            if args.Length >= currying then
                let cargs, moreArgs = args |> List.splitAt currying
                let f = Appl(func, cargs |> List.map (fun (u, a) -> this.TransformExpression a), NonPure, Some currying)  
                curriedApplication f (moreArgs |> List.map (fun (u, a) -> u, this.TransformExpression a))
            else
                base.TransformCurriedApplication(func, args)             
        | _ -> base.TransformCurriedApplication(func, args)

#if DEBUG
let mutable logTransformations = false
#endif
