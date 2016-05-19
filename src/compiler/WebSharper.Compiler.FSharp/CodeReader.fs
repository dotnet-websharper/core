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

module internal WebSharper.Compiler.FSharp.CodeReader

open Microsoft.FSharp.Compiler.SourceCodeServices
 
open WebSharper.Core
open WebSharper.Core.AST
open WebSharper.Compiler

type VarKind =
    | LocalVar 
    | ByRefArg
    | ThisArg
         
type Environment =
    {
        ScopeIds : list<FSharpMemberOrFunctionOrValue * Id * VarKind>
        TParams : Map<string, int>
        Exception : option<Id>
        MatchCaptures : option<Id>
        Compilation : Compilation
    }
    static member New(vars, tparams, comp) = 
        { 
            ScopeIds = vars |> Seq.map (fun (i, (v, k)) -> i, v, k) |> List.ofSeq 
            TParams = tparams |> Seq.mapi (fun i p -> p, i) |> Map.ofSeq
            Exception = None
            MatchCaptures = None
            Compilation = comp
        }

    member this.WithTParams tparams =
        if List.isEmpty tparams then this else
        { this with 
            TParams = 
                ((this.TParams, this.TParams.Count), tparams) 
                ||> List.fold (fun (m, i) p -> m |> Map.add p i, i + 1) 
                |> fst
        }

    member this.WithVar(i: Id, v: FSharpMemberOrFunctionOrValue, ?k) =
        { this with ScopeIds = (v, i, defaultArg k LocalVar) :: this.ScopeIds }

    member this.WithException (i: Id, v: FSharpMemberOrFunctionOrValue) =
        { this with 
            ScopeIds = (v, i, LocalVar) :: this.ScopeIds
            Exception = Some i }

    member this.LookupVar (v: FSharpMemberOrFunctionOrValue) =
        match this.ScopeIds |> List.tryPick (fun (sv, i, k) -> if sv = v then Some (i, k) else None) with
        | Some var -> var
        | _ -> failwithf "Variable lookup failed: %s" v.DisplayName

let rec getOrigDef (td: FSharpEntity) =
    if td.IsFSharpAbbreviation then getOrigDef td.AbbreviatedType.TypeDefinition else td 

let mutable thisAssemblyName = "CurrentAssembly"

let getSimpleName (a: FSharpAssembly) =
    match a.FileName with
    | None -> thisAssemblyName
    | _ -> a.SimpleName

module M = WebSharper.Core.Metadata

let getTypeDefinition (td: FSharpEntity) =
    if td.IsArrayType then
        TypeDefinition {
            Assembly = "mscorlib"
            FullName = "System.Array`1"
        }
    else
    let td = getOrigDef td
    let res =
        {
            Assembly = getSimpleName td.Assembly 
            FullName = if td.IsProvidedAndErased then td.LogicalName else td.QualifiedName.Split([|','|]).[0] 
        }
    // TODO: more measure types
    match res.Assembly with
    | "FSharp.Core" ->
        match res.FullName with
        | "Microsoft.FSharp.Core.byte`1" -> 
            { Assembly = "mscorlib"; FullName = "System.Byte" }   
        | "Microsoft.FSharp.Core.syte`1" -> 
            { Assembly = "mscorlib"; FullName = "System.SByte" }   
        | "Microsoft.FSharp.Core.int16`1" -> 
            { Assembly = "mscorlib"; FullName = "System.Int16" }   
        | "Microsoft.FSharp.Core.int`1" -> 
            { Assembly = "mscorlib"; FullName = "System.Int32" }   
        | "Microsoft.FSharp.Core.uint16`1" ->
            { Assembly = "mscorlib"; FullName = "System.UInt16" }   
        | "Microsoft.FSharp.Core.uint32`1" -> 
            { Assembly = "mscorlib"; FullName = "System.UInt32" }   
        | "Microsoft.FSharp.Core.decimal`1" -> 
            { Assembly = "mscorlib"; FullName = "System.Decimal" }   
        | "Microsoft.FSharp.Core.int64`1" -> 
            { Assembly = "mscorlib"; FullName = "System.Int64" }   
        | "Microsoft.FSharp.Core.uint64`1" -> 
            { Assembly = "mscorlib"; FullName = "System.UInt64" }   
        | "Microsoft.FSharp.Core.float32`1" ->
            { Assembly = "mscorlib"; FullName = "System.Single" }   
        | "Microsoft.FSharp.Core.float`1" ->
            { Assembly = "mscorlib"; FullName = "System.Double" }   
        | _ -> res
    | _ -> res
    |> fun x -> TypeDefinition x

module A = WebSharper.Compiler.AttributeReader

type FSharpAttributeReader() =
    inherit A.AttributeReader<FSharpAttribute>()
    override this.GetAssemblyName attr = getSimpleName attr.AttributeType.Assembly
    override this.GetName attr = attr.AttributeType.LogicalName
    override this.GetCtorArgs attr = attr.ConstructorArguments |> Seq.map snd |> Array.ofSeq          
    override this.GetTypeDef o = getTypeDefinition (o :?> FSharpType).TypeDefinition

let attrReader = FSharpAttributeReader()

let rec getOrigType (t: FSharpType) =
    if t.IsAbbreviation then getOrigType t.AbbreviatedType else t

let isUnit (t: FSharpType) =
    if t.IsGenericParameter then
        false
    else
    let t = getOrigType t
    if t.IsTupleType || t.IsFunctionType then false else
    let td = t.TypeDefinition
    if td.IsArrayType || td.IsByRef then false
    elif td.IsProvidedAndErased then false
    else td.FullName = "Microsoft.FSharp.Core.Unit" || td.FullName = "System.Void"

let isOption (t: FSharpType) =
    let td = (getOrigType t).TypeDefinition
    not td.IsProvidedAndErased && td.FullName.StartsWith "Microsoft.FSharp.Core.FSharpOption`1"

let isByRef (t: FSharpType) =
    if t.IsGenericParameter then
        false
    else
    let t = getOrigType t
    if t.IsTupleType || t.IsFunctionType then false else
    t.TypeDefinition.IsByRef

exception ParseError of message: string with
    override this.Message = this.message 

let parsefailf x =
    Printf.kprintf (fun s -> raise <| ParseError s) x

let rec getTypeR markStaticTP (tparams: Map<string, int>) (t: FSharpType) =
    if t.IsGenericParameter then
        
        match tparams.TryFind t.GenericParameter.Name with
        | Some i -> 
            if markStaticTP && t.GenericParameter.IsSolveAtCompileTime then StaticTypeParameter i else TypeParameter i
        | _ ->
            parsefailf "Failed to resolve generic parameter: %s, found: %s" 
                t.GenericParameter.Name (tparams |> Map.toSeq |> Seq.map fst |> String.concat ", ")
    else
    let t = getOrigType t
    let getFunc() =
        match t.GenericArguments |> Seq.map (getTypeR markStaticTP tparams) |> List.ofSeq with
        | [a; r] -> FSharpFuncType(a, r)
        | _ -> failwith "impossible: FSharpFunc must have 2 type parameters"
    if t.IsTupleType then
        t.GenericArguments |> Seq.map (getTypeR markStaticTP tparams) |> List.ofSeq |> TupleType
    elif t.IsFunctionType then
        getFunc()
    else
    let td = t.TypeDefinition
    if td.IsArrayType then
        ArrayType(getTypeR markStaticTP tparams t.GenericArguments.[0], td.DisplayName.Length - 1)
    elif td.IsByRef then
        ByRefType(getTypeR markStaticTP tparams t.GenericArguments.[0])
    else
        let fn = 
            if td.IsProvidedAndErased then td.LogicalName else td.FullName
        if fn.StartsWith "System.Tuple" then
            t.GenericArguments |> Seq.map (getTypeR markStaticTP tparams) |> List.ofSeq |> TupleType
        elif fn = "Microsoft.FSharp.Core.FSharpFunc`2" then
            getFunc()
        elif fn = "Microsoft.FSharp.Core.Unit" || fn = "System.Void" then
            VoidType
        else
            let td = getTypeDefinition td
            // erase Measure parameters
            match td.Value.FullName with
            | "System.Byte" 
            | "System.SByte"            
            | "System.Int16" 
            | "System.Int32" 
            | "System.UInt16" 
            | "System.UInt32" 
            | "System.Decimal"
            | "System.Int64" 
            | "System.UInt64" 
            | "System.Single" 
            | "System.Double" -> NonGenericType td
            | _ ->
                GenericType td (t.GenericArguments |> Seq.map (getTypeR markStaticTP tparams) |> List.ofSeq)

let getType tparams t = getTypeR false tparams t

let removeUnitParam (ps: list<Type>) =
    match ps with 
    | [ VoidType ] -> []
    | _ -> ps

let hasCompilationRepresentation (cr: CompilationRepresentationFlags) attrs =
    attrs |> Seq.exists (fun (a: FSharpAttribute) ->
        a.AttributeType.FullName = "Microsoft.FSharp.Core.CompilationRepresentationAttribute"
        && obj.Equals(snd a.ConstructorArguments.[0], int cr)
    )

let getAbstractSlot tparams (x: FSharpAbstractSignature) : Method =
    let tparams =
        Seq.append 
            (Map.toSeq tparams)
            (x.DeclaringTypeGenericParameters |> Seq.mapi (fun i p -> p.Name, i)) 
        |> Map.ofSeq
    Method {
        MethodName = x.Name
        Parameters = x.AbstractArguments |> Seq.concat |> Seq.map (fun p -> getType tparams p.Type) |> List.ofSeq |> removeUnitParam
        ReturnType = getType tparams x.AbstractReturnType
        Generics   = x.MethodGenericParameters.Count
    } 

let getMember (x : FSharpMemberOrFunctionOrValue) : Member =
    let name = x.CompiledName

    if name = ".cctor" then Member.StaticConstructor else

    let tparams = 
        Seq.append x.EnclosingEntity.GenericParameters x.GenericParameters
        |> Seq.distinctBy (fun p -> p.Name)
        |> Seq.mapi (fun i p -> p.Name, i) |> Map.ofSeq

    let isInstance = x.IsInstanceMember

    let compiledAsStatic =
        isInstance && (
            x.IsExtensionMember || (
                not (x.Attributes |> hasCompilationRepresentation CompilationRepresentationFlags.Instance) &&
                x.EnclosingEntity.Attributes |> hasCompilationRepresentation CompilationRepresentationFlags.UseNullAsTrueValue
            ) 
        )    

    let getPars() =
        let ps =  
            x.CurriedParameterGroups |> Seq.concat |> Seq.map (fun p -> getType tparams p.Type) |> List.ofSeq |> removeUnitParam  
        if compiledAsStatic then
            GenericType (getTypeDefinition x.LogicalEnclosingEntity) 
                (List.init x.LogicalEnclosingEntity.GenericParameters.Count (fun i -> TypeParameter i)) :: ps
        else ps

    if name = ".ctor" then
        Member.Constructor <| Constructor {
            CtorParameters = getPars()
        }  
    else
        if x.IsOverrideOrExplicitInterfaceImplementation then
            let s = x.ImplementedAbstractSignatures |> Seq.head

            let iTparams = 
                Seq.append s.DeclaringTypeGenericParameters s.MethodGenericParameters
                |> Seq.distinctBy (fun p -> p.Name)
                |> Seq.mapi (fun i p -> p.Name, i) |> Map.ofSeq
            
            let i = getTypeDefinition s.DeclaringType.TypeDefinition

            let meth = getAbstractSlot iTparams s

            if x.IsExplicitInterfaceImplementation then
                Member.Implementation(i, meth)    
            else
                Member.Override(i, meth)    
        else 
        
            Member.Method(
                isInstance && not compiledAsStatic,
                Method {
                    MethodName = name
                    Parameters = getPars()
                    ReturnType = getType tparams x.ReturnParameter.Type
                    Generics   = tparams.Count - x.EnclosingEntity.GenericParameters.Count
                } 
            )

let getAndRegisterTypeDefinition (comp: Compilation) (td: FSharpEntity) =
    let res = getTypeDefinition td

    if td.IsDelegate then 
        if not (comp.HasCustomTypeInfo res) then
            let tparams = 
                td.GenericParameters
                |> Seq.mapi (fun i p -> p.Name, i) |> Map.ofSeq
            let info =
                try 
                    let sign = td.FSharpDelegateSignature
                    M.DelegateInfo {
                        DelegateArgs =
                            sign.DelegateArguments |> Seq.map (snd >> getType tparams) |> List.ofSeq
                        ReturnType = getType tparams sign.DelegateReturnType
                    }
                with _ ->
                    let inv = td.MembersFunctionsAndValues |> Seq.find(fun m -> m.CompiledName = "Invoke")
                    M.DelegateInfo {
                        DelegateArgs =
                            inv.CurriedParameterGroups |> Seq.concat |> Seq.map (fun p -> getType tparams p.Type) |> List.ofSeq
                        ReturnType = getType tparams inv.ReturnParameter.Type
                    }
            comp.AddCustomType(res, info)
    res

type Capturing(var) =
    inherit Transformer()

    let mutable captVal = None
    let mutable scope = 0

    override this.TransformId i =
        if scope > 0 && i = var then
            match captVal with
            | Some c -> c
            | _ ->
                let c = Id.New(?name = var.Name, mut = var.IsMutable)
                captVal <- Some c
                c
        else i

    override this.TransformFunction (args, body) =
        scope <- scope + 1
        let res = base.TransformFunction (args, body)
        scope <- scope - 1
        res

    member this.CaptureValueIfNeeded expr =
        let res = this.TransformExpression expr  
        match captVal with
        | None -> res
        | Some c ->
            Application (Function ([c], Return res), [Var var])        

let getRange (range: Microsoft.FSharp.Compiler.Range.range) =
    {   
        FileName = range.FileName
        Start = range.StartLine, range.StartColumn + 1
        End = range.EndLine, range.EndColumn + 1
    }

let getSourcePos (x: FSharpExpr) =
    getRange x.Range

let withSourcePos (x: FSharpExpr) (expr: Expression) =
    ExprSourcePos (getSourcePos x, expr)

type FixCtorTransformer(?thisExpr) =
    inherit Transformer()

    let mutable firstOcc = true

    let thisExpr = defaultArg thisExpr This

    override this.TransformSequential (es) =
        match es with
        | h :: t -> Sequential (this.TransformExpression h :: t)
        | _ -> Undefined

    override this.TransformLet(a, b, c) =
        Let(a, b, this.TransformExpression c)

    override this.TransformConditional(a, b, c) =
        Conditional(a, this.TransformExpression b, this.TransformExpression c)   
        
    override this.TransformLetRec(a, b) =
        LetRec(a, this.TransformExpression b) 

    override this.TransformStatementExpr(a, b) = StatementExpr (a, b)

    override this.TransformCtor (t, c, a) =
        if not firstOcc then Ctor (t, c, a) else
        firstOcc <- false
        if t.Entity = Definitions.Obj then thisExpr
        elif (let fn = t.Entity.Value.FullName in fn = "WebSharper.ExceptionProxy" || fn = "System.Exception") then 
            match a with
            | [] -> Undefined
            | [msg] -> ItemSet(thisExpr, Value (String "message"), msg)
            | _ -> failwith "Too many arguments for Error"
        else
            BaseCtor(thisExpr, t, c, a) 

let fixCtor expr =
    FixCtorTransformer().TransformExpression(expr)

let fsharpListDef =
    TypeDefinition {
        Assembly = "FSharp.Core"
        FullName = "Microsoft.FSharp.Collections.FSharpList`1"  
    }

let emptyListDef =
    Method {
        MethodName = "get_Empty"
        Parameters = []
        ReturnType = GenericType fsharpListDef [ TypeParameter 0 ]
        Generics = 0      
    }

let listModuleDef =
    TypeDefinition {
        Assembly = "FSharp.Core"
        FullName = "Microsoft.FSharp.Collections.ListModule"
    }

let listOfArrayDef =
    Method {
        MethodName = "OfArray"
        Parameters = [ ArrayType (TypeParameter 0, 1) ]
        ReturnType = GenericType fsharpListDef [ TypeParameter 0 ]
        Generics = 1      
    }

let newId() = Id.New(mut = false)
let namedId n =
    match n with
    | "tupledArg" -> Id.New("a", false)
    | "( builder@ )" -> newId()
    | _ -> if n.StartsWith "_arg" then newId() else Id.New(n, false) 

let rec transformExpression (env: Environment) (expr: FSharpExpr) =
    let inline tr x = transformExpression env x
    try
        match expr with
        | BasicPatterns.Value(var) ->
            if isUnit var.FullType then
                Undefined
            else
                let v, k = env.LookupVar var
                match k with
                | LocalVar -> Var v  
                | ByRefArg -> GetRef (Var v)
                | ThisArg -> This
        | BasicPatterns.Lambda _ ->
            let rec loop acc = function
                | BasicPatterns.Lambda (var, body) -> loop (var :: acc) body
                | body -> (List.rev acc, body)
            match loop [] expr with
            | [arg], body ->
                let lArg, env =
                    if isUnit arg.FullType then [], env
                    else 
                        let v = namedId arg.DisplayName
                        [ v ], env.WithVar(v, arg)
                let env = env.WithTParams(arg.GenericParameters |> Seq.map (fun p -> p.Name) |> List.ofSeq)
                Lambda(lArg, (body |> transformExpression env))
            | args, body ->
                let vars, env =
                    (env, args) ||> List.mapFold (fun env arg ->
                        if isUnit arg.FullType then namedId "_", env
                        else 
                            let v = namedId arg.DisplayName
                            v, env.WithVar(v, arg)
                    ) 
                Application(JSRuntime.Curried, [ Lambda(vars, (body |> transformExpression env)) ])
        | BasicPatterns.Application(func, types, args) ->
            match IgnoreExprSourcePos (tr func) with
            | CallNeedingMoreArgs(thisObj, td, m, ca) ->
                Call(thisObj, td, m, ca @ (args |> List.map tr))
            | trFunc ->
                match args with
                | [a] ->
                    let trA = tr a
                    match IgnoreExprSourcePos trA with
                    | Undefined | Value Null -> Application (trFunc, [])
                    | _ -> Application (trFunc, [trA])  
                | _ ->
                    Application(JSRuntime.Apply, [trFunc; NewArray (args |> List.map tr) ])
        | BasicPatterns.Let((id, value), body) ->
            let i = Id.New(id.DisplayName, id.IsMutable)
            let trValue = tr value
            let env = env.WithVar(i, id, if isByRef id.FullType then ByRefArg else LocalVar)
            let inline tr x = transformExpression env x
            if id.IsMutable then
                Sequential [ NewVar(i, trValue); tr body ]
            else
                Let (i, trValue, tr body)
        | BasicPatterns.LetRec(defs, body) ->
            let mutable env = env
            let ids = defs |> List.map (fun (id, _) ->
                let i = Id.New(id.DisplayName, id.IsMutable)
                env <- env.WithVar(i, id, if isByRef id.FullType then ByRefArg else LocalVar)
                i
            )
            let inline tr x = transformExpression env x
            LetRec (
                Seq.zip ids defs 
                |> Seq.map (fun (i, (_, v)) -> i, tr v) |> List.ofSeq, 
                tr body
            )
        | BasicPatterns.Call(this, meth, typeGenerics, methodGenerics, arguments) ->
            let td = getAndRegisterTypeDefinition env.Compilation meth.EnclosingEntity
            if td.Value.FullName = "Microsoft.FSharp.Core.Operators" && meth.CompiledName = "Reraise" then
                IgnoredStatementExpr (Throw (Var env.Exception.Value))    
            else
                let t = Generic td (typeGenerics |> List.map (getType env.TParams))
                let args = 
                    arguments |> List.map (fun a ->
                        let ta = tr a
                        if isByRef a.Type then
                            match IgnoreExprSourcePos ta with
                            | Application(ItemGet (r, Value (String "get")), []) -> r
                            | _ -> ta
                        else ta
                    )
                let args =
                    match args with
                    | [ Undefined | Value Null ] -> []
                    | _ -> args
                match getMember meth with
                | Member.Method (isInstance, m) -> 
                    let mt = Generic m (methodGenerics |> List.map (getType env.TParams))
                    if isInstance then
                        Call (Option.map tr this, t, mt, args)
                    else 
                        if meth.IsInstanceMember && not meth.IsExtensionMember then
                            CallNeedingMoreArgs (None, t, mt, Option.toList (Option.map tr this) @ args)
                        else 
                            Call (None, t, mt, Option.toList (Option.map tr this) @ args)
                | Member.Implementation (i, m) ->
                    let t = Generic i (typeGenerics |> List.map (getType env.TParams))
                    let mt = Generic m (methodGenerics |> List.map (getType env.TParams))
                    Call (Option.map tr this, t, mt, args)
                | Member.Override (_, m) ->
                    let mt = Generic m (methodGenerics |> List.map (getType env.TParams))
                    Call (Option.map tr this, t, mt, args)
                | Member.Constructor c -> Ctor (t, c, args)
                | Member.StaticConstructor -> parsefailf "Invalid: direct call to static constructor" //CCtor t 
        | BasicPatterns.Sequential _ ->
            let rec getSeq acc expr =
                match expr with            
                | BasicPatterns.Sequential (f, s) ->
                    getSeq (f :: acc) s   
                | _ -> expr :: acc
            getSeq [] expr |> List.rev |> List.map tr |> Sequential
        | BasicPatterns.Const (value, _) ->
            Value(ReadLiteral value)
        | BasicPatterns.IfThenElse (cond, then_, else_) ->
            Conditional(tr cond, tr then_, tr else_)    
        | BasicPatterns.NewObject (ctor, typeGenerics, arguments) -> 
            let td = getAndRegisterTypeDefinition env.Compilation ctor.EnclosingEntity
            let t = Generic td (typeGenerics |> List.map (getType env.TParams))
            let args = List.map tr arguments
            let args =
                match args with
                | [ Undefined | Value Null ] -> []
                | _ -> args
            match getMember ctor with
            | Member.Constructor c -> Ctor (t, c, args)
            | _ -> parsefailf "Expected a constructor call"
        | BasicPatterns.TryFinally (body, final) ->
            let res = newId()
            StatementExpr (TryFinally(VarSetStatement(res, tr body), ExprStatement (tr final)), Some res)
        | BasicPatterns.TryWith (body, var, filter, e, catch) -> // TODO: var, filter?
            let err = namedId e.DisplayName
            let res = newId()
            StatementExpr (
                TryWith(VarSetStatement(res, tr body), 
                    Some err, 
                    (VarSetStatement(res, transformExpression (env.WithException(err, e)) catch)))
                , Some res)
        | BasicPatterns.NewArray (_, items) ->
            NewArray (items |> List.map tr)              
        | BasicPatterns.NewTuple (_, items) ->
            NewArray (items |> List.map tr)              
        | BasicPatterns.WhileLoop (cond, body) ->
            IgnoredStatementExpr(While(tr cond, ExprStatement (tr body)))
        | BasicPatterns.ValueSet (var, value) ->
            if var.IsModuleValueOrMember then
                let td = getAndRegisterTypeDefinition env.Compilation var.EnclosingEntity
                match getMember var with
                | Member.Method (_, m) ->
                    let me = m.Value
                    let setm =
                        NonGeneric <| Method {
                            MethodName = "set_" + me.MethodName   
                            Parameters = [ me.ReturnType ]
                            ReturnType = VoidType
                            Generics = 0
                        }
                    Call (None, NonGeneric td, setm, [tr value])
                | _ -> parsefailf "Module member is not a method"
            else
                let v, k = env.LookupVar var
                match k with
                | LocalVar -> Void(VarSet(v, tr value)) 
                | ByRefArg -> SetRef (Var v) (tr value)
                | ThisArg -> failwith "'this' parameter cannot be set"
        | BasicPatterns.TupleGet (_, i, tuple) ->
            ItemGet(tr tuple, Value (Int i))   
        | BasicPatterns.FastIntegerForLoop (start, end_, body, up) ->
            let j = newId()
            let i, trBody =
                match IgnoreExprSourcePos (tr body) with
                | Function ([i], Return b) -> i, b
                | _ -> parsefailf "Unexpected form of consumeExpr in FastIntegerForLoop pattern"     
            For (
                Some (Sequential [NewVar(i, tr start); NewVar (j, tr end_)]), 
                Some (if up then Binary(Var i, BinaryOperator.``<=``, Var j) else Binary(Var i, BinaryOperator.``>=``, Var j)), 
                Some (if up then MutatingUnary(MutatingUnaryOperator.``()++``, Var i)  else MutatingUnary(MutatingUnaryOperator.``()--``, Var i)), 
                ExprStatement (Capturing(i).CaptureValueIfNeeded(trBody))
            ) |> IgnoredStatementExpr
        | BasicPatterns.TypeTest (typ, expr) ->
            TypeCheck (tr expr, getType env.TParams typ)
        | BasicPatterns.Coerce (typ, expr) ->
            tr expr // TODO: type check when possible
        | BasicPatterns.NewUnionCase (typ, case, exprs) ->
            let t =
                match getType env.TParams typ with
                | ConcreteType ct -> ct
                | _ -> parsefailf "Expected a union type"
            if t.Entity = fsharpListDef then
                let rec getItems acc e =
                    match e with 
                    | BasicPatterns.NewUnionCase (_, _, [h; t]) ->
                        getItems (h :: acc) t 
                    | BasicPatterns.NewUnionCase (_, _, []) ->
                        Some (List.rev acc)
                    | _ -> None
                match exprs with
                | [] ->
                    NewUnionCase(t, case.CompiledName, [])
                | [h; r] -> 
                    match getItems [ h ] r with
                    | Some fromArr -> 
                        Call(None, NonGeneric listModuleDef, NonGeneric listOfArrayDef, [ NewArray (fromArr |> List.map tr) ])    
                    | None ->
                        NewUnionCase(t, case.CompiledName, exprs |> List.map tr) 
                | _ -> parsefailf "Invalid number of union fields for FSharpList"
            else
                NewUnionCase(t, case.CompiledName, exprs |> List.map tr)
        | BasicPatterns.UnionCaseGet (expr, typ, case, field) ->
            let i = case.UnionCaseFields |> Seq.findIndex (fun f -> f = field)
            ItemGet(tr expr, Value (String ("$" + string i)))   
        | BasicPatterns.UnionCaseTest (expr, typ, case) ->
            let t =
                match getType env.TParams typ with
                | ConcreteType ct -> ct
                | _ -> parsefailf "Expected a union type"
            UnionCaseTest(tr expr, t, case.CompiledName)
        | BasicPatterns.UnionCaseTag (expr, typ) ->
            let t =
                match getType env.TParams typ with
                | ConcreteType ct -> ct
                | _ -> parsefailf "Expected a union type"
            UnionCaseTag(tr expr, t)
        | BasicPatterns.NewRecord (typ, items) ->
            let td = typ.TypeDefinition 
            let t =
                match getType env.TParams typ with
                | ConcreteType ct -> ct
                | _ -> parsefailf "Expected a named type"
            if td.IsFSharpRecord || td.IsFSharpExceptionDeclaration then
                NewRecord (t, List.map tr items)
            else
                let fields = td.FSharpFields
                if items.Length <> fields.Count then
                    parsefailf "Number of val fields and explicit constructor arguments do not match"    
                else                            
                    Sequential [
                        for a, f in Seq.zip items fields ->
                            FieldSet(Some This, t, f.Name, tr a)
                    ]
        | BasicPatterns.DecisionTree (matchValue, cases) ->
            let c = newId()
            let r = newId()
            let env = { env with MatchCaptures = Some c }
            Sequential [
                NewVar(c, Undefined)
                StatementExpr(
                    Switch(
                        transformExpression env matchValue, 
                        cases |> List.mapi (fun j (ci, e) -> 
                            Some (Value (Int j)), 
                            Block [
                                let mutable env = env 
                                yield! ci |> Seq.mapi (fun captIndex cv ->
                                    let i = namedId cv.DisplayName
                                    env <- env.WithVar (i, cv)
                                    (VarDeclaration(i, ItemGet(Var c, Value (Int captIndex))))
                                )
                                yield VarSetStatement(r, transformExpression env e) 
                                yield Break None 
                            ]
                        )
                    )
                    , Some r
                )
            ]
        | BasicPatterns.DecisionTreeSuccess (index, results) ->
            let c = env.MatchCaptures.Value
            Sequential [
                match results |> List.map tr with
                | [] -> ()
                | matchCaptures -> yield VarSet (c, NewArray matchCaptures) 
                yield Value (Int index)
            ]
        | BasicPatterns.ThisValue (typ) ->
            This
        | BasicPatterns.FSharpFieldGet (thisOpt, typ, field) ->
            let t = 
                match getType env.TParams typ with
                | ConcreteType ct -> ct
                | _ -> parsefailf "Expected a record type"
            FieldGet(thisOpt |> Option.map tr, t, field.Name)
        | BasicPatterns.FSharpFieldSet (thisOpt, typ, field, value) ->
            let t = 
                match getType env.TParams typ with
                | ConcreteType ct -> ct
                | _ -> parsefailf "Expected a record type"
            FieldSet(thisOpt |> Option.map tr, t, field.Name, tr value)
        | BasicPatterns.AddressOf expr ->
            let e = IgnoreExprSourcePos (tr expr)
            match e with
            | Var v ->
                MakeRef e (fun value -> VarSet(v, value))
            | ItemGet(o, i) ->
                let ov = newId()
                let iv = newId()
                Let (ov, o, Let(iv, i, MakeRef (ItemGet(Var ov, Var iv)) (fun value -> ItemSet(Var ov, Var iv, value))))
            | FieldGet(o, t, f) ->
                match o with
                | Some o ->
                    let ov = newId()
                    Let (ov, o, MakeRef (FieldGet(Some (Var ov), t, f)) (fun value -> FieldSet(Some (Var ov), t, f, value)))     
                | _ ->
                    MakeRef e (fun value -> FieldSet(None, t, f, value))  
            | Application(ItemGet (r, Value (String "get")), []) ->
                MakeRef e (fun value -> Application(ItemGet (r, Value (String "set")), [value]))        
            | Call(None, td, m, []) ->
                let me = m.Entity.Value
                let setm =
                    NonGeneric <| Method {
                        MethodName = "set_" + me.MethodName   
                        Parameters = [ me.ReturnType ]
                        ReturnType = VoidType
                        Generics = 0
                    }
                MakeRef e (fun value -> Call(None, td, setm, [value]))    
            | _ -> failwithf "AddressOf error, unexpected form: %+A" e 
        | BasicPatterns.AddressSet (addr, value) ->
            match addr with
            | BasicPatterns.Value(var) ->
                let v, _ = env.LookupVar var
                SetRef (Var v) (tr value)
            | _ -> failwith "AddressSet not on a Value"
        | BasicPatterns.ObjectExpr (typ, expr, overrides, interfaces) ->
            let o = newId()
//            let objTParams =
//                (getOrigDef typ.TypeDefinition).GenericParameters
//                |> Seq.mapi (fun i p -> p.Name, i)
//                |> Map.ofSeq 
            Sequential [
                yield NewVar(o, CopyCtor(getAndRegisterTypeDefinition env.Compilation typ.TypeDefinition, Object []))
                for ovr in Seq.append overrides (interfaces |> Seq.collect snd) do
                    let i = getAndRegisterTypeDefinition env.Compilation ovr.Signature.DeclaringType.TypeDefinition
                    let s = getAbstractSlot env.TParams ovr.Signature
                    let mutable env = env
                    let thisVar, vars =
                        match ovr.CurriedParameterGroups with
                        | [t] :: a ->
                            let thisVar = namedId t.DisplayName
                            env <- env.WithVar(thisVar, t)
                            thisVar,
                            a |> Seq.concat |> Seq.map (fun v ->
                                let vv = namedId v.DisplayName
                                env <- env.WithVar(vv, v)
                                vv
                            ) |> List.ofSeq 
                        | _ ->
                            failwith "Wrong `this` argument in object expression override"
                    let b = FuncWithThis (thisVar, vars, Return (transformExpression env ovr.Body)) 
                    yield ItemSet(Var o, OverrideName(i, s), b)
                yield FixCtorTransformer(Var o).TransformExpression(tr expr)
                yield Var o
            ]
        | BasicPatterns.DefaultValue typ ->
            Value Null
//            getDefaultOf (getType env.TParams typ) // this would need Unchecked.defaulof macro with type parameter constraint `: null`
        | BasicPatterns.NewDelegate (typ, arg) ->
            // TODO : loop for exact length of delegate type
            let rec loop acc = function
                | BasicPatterns.Lambda (var, body) -> loop (var :: acc) body
                | body -> (List.rev acc, body)

            match loop [] arg with
            | ([], BasicPatterns.Application (f, _, [BasicPatterns.Const (null, _)])) ->
                tr f
            | vars, body ->
                let mutable env = env
                let args = 
                    vars |> List.map (fun v -> 
                        let vv = namedId v.DisplayName
                        env <- env.WithVar(vv, v)
                        vv
                    )
                Lambda (args, transformExpression env body)
            | _ -> failwith "Failed to translate delegate creation"
        | BasicPatterns.TypeLambda (gen, expr) -> tr expr
        | BasicPatterns.Quote expr -> tr expr
        | BasicPatterns.BaseValue _ -> Base
        | BasicPatterns.ILAsm("[I_ldelema (NormalAddress,false,ILArrayShape [(Some 0, null)],TypeVar 0us)]", _, [ arr; i ]) ->
            let arrId = newId()
            let iId = newId()
            Let (arrId, tr arr, Let(iId, tr i, MakeRef (ItemGet(Var arrId, Var iId)) (fun value -> ItemSet(Var arrId, Var iId, value))))
        | BasicPatterns.ILAsm (s, _, _) ->
             parsefailf "Unrecognized ILAsm: %s" s
        | BasicPatterns.ILFieldGet _ -> parsefailf "F# pattern not handled: ILFieldGet"
        | BasicPatterns.ILFieldSet _ -> parsefailf "F# pattern not handled: ILFieldSet"
        | BasicPatterns.TraitCall(sourceTypes, traitName, typeArgs, typeInstantiation, argExprs) ->
            if sourceTypes.Length <> 1 then parsefailf "TODO: TraitCall with multiple source types" 
            match argExprs with
            | t :: a -> 
                let meth =
                    Method {
                        MethodName = traitName
                        Parameters = typeInstantiation |> List.map (getTypeR true env.TParams)
                        ReturnType = getTypeR true env.TParams expr.Type
                        Generics   = 0
                    } 
                TraitCall(tr t, getType env.TParams sourceTypes.[0], NonGeneric meth, a |> List.map tr)  
            | _ ->
                failwith "Impossible: TraitCall must have a this argument"
        | BasicPatterns.UnionCaseSet _ ->
            parsefailf "UnionCaseSet pattern is only allowed in FSharp.Core"
        | _ -> parsefailf "F# expression not recognized"
    with e ->
        let msg =
            match e with
            | ParseError m -> m
            | _ -> "Error while reading F# code: " + e.Message + " " + e.StackTrace
        env.Compilation.AddError(Some (getSourcePos expr), WebSharper.Compiler.SourceError msg)
        WebSharper.Compiler.Translator.errorPlaceholder        
    |> withSourcePos expr
