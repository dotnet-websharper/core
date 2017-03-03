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
    | FuncArg
    | ByRefArg
    | ThisArg
         
let rec getOrigDef (td: FSharpEntity) =
    if td.IsFSharpAbbreviation then getOrigDef td.AbbreviatedType.TypeDefinition else td 

module M = WebSharper.Core.Metadata
module A = WebSharper.Compiler.AttributeReader
module I = IgnoreSourcePos
module P = BasicPatterns

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
    let t = getOrigType t
    t.HasTypeDefinition &&
        let td = t.TypeDefinition
        not td.IsProvidedAndErased && td.TryFullName = Some "Microsoft.FSharp.Core.FSharpOption`1"

let rec isSeq (t: FSharpType) = 
    let t = getOrigType t
    (
        t.HasTypeDefinition &&
            let td = t.TypeDefinition
            not td.IsProvidedAndErased && td.TryFullName = Some "System.Collections.Generic.IEnumerable`1"
    ) || (
        t.IsGenericParameter && 
            t.GenericParameter.Constraints
            |> Seq.exists (fun c -> c.IsCoercesToConstraint && isSeq c.CoercesToTarget)
    )

let isByRef (t: FSharpType) =
    if t.IsGenericParameter then
        false
    else
    let t = getOrigType t
    if t.IsTupleType || t.IsFunctionType then false else
    t.HasTypeDefinition && t.TypeDefinition.IsByRef

let getFuncArg t =
    let rec get acc (t: FSharpType) =
        if t.IsFunctionType then
            let a = t.GenericArguments.[0] 
            let r = t.GenericArguments.[1] 
            let i = if a.IsTupleType then a.GenericArguments.Count else 1
            get (i :: acc) r 
        else
            match acc with
            | [] | [1] -> NotOptimizedFuncArg
            | [n] -> TupledFuncArg n
            | _ -> CurriedFuncArg (List.length acc)
    get [] t    

exception ParseError of message: string with
    override this.Message = this.message 

let parsefailf x =
    Printf.kprintf (fun s -> raise <| ParseError s) x

let removeUnitParam (ps: list<Type>) =
    match ps with 
    | [ VoidType ] -> []
    | _ -> ps

let hasCompilationRepresentation (cr: CompilationRepresentationFlags) attrs =
    attrs |> Seq.exists (fun (a: FSharpAttribute) ->
        a.AttributeType.FullName = "Microsoft.FSharp.Core.CompilationRepresentationAttribute"
        && obj.Equals(snd a.ConstructorArguments.[0], int cr)
    )

let getRange (range: Microsoft.FSharp.Compiler.Range.range) =
    {   
        FileName = range.FileName
        Start = range.StartLine, range.StartColumn + 1
        End = range.EndLine, range.EndColumn + 1
    }

let getSourcePos (x: FSharpExpr) =
    getRange x.Range

let withSourcePos (x: FSharpExpr) (expr: Expression) =
    ExprSourcePos (getSourcePos x, IgnoreExprSourcePos expr)

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
            | [msg; inner] -> 
                Sequential [
                    ItemSet(thisExpr, Value (String "message"), msg)
                    ItemSet(thisExpr, Value (String "inner"), inner)
                ]
            | _ -> failwith "Too many arguments for Error"
        else
            BaseCtor(thisExpr, t, c, a) 

let fixCtor expr =
    FixCtorTransformer().TransformExpression(expr)

module Definitions =
    let List =
        TypeDefinition {
            Assembly = "FSharp.Core"
            FullName = "Microsoft.FSharp.Collections.FSharpList`1"  
        }

    let ListEmpty =
        Method {
            MethodName = "get_Empty"
            Parameters = []
            ReturnType = GenericType List [ TypeParameter 0 ]
            Generics = 0      
        }

    let ListModule =
        TypeDefinition {
            Assembly = "FSharp.Core"
            FullName = "Microsoft.FSharp.Collections.ListModule"
        }

    let ListOfArray =
        Method {
            MethodName = "OfArray"
            Parameters = [ ArrayType (TypeParameter 0, 1) ]
            ReturnType = GenericType List [ TypeParameter 0 ]
            Generics = 1      
        }

    let Array =
        TypeDefinition {
            Assembly = "mscorlib"
            FullName = "System.Array"
        }

    let ArrayLength =
        Method {
            MethodName = "get_Length"
            Parameters = []
            ReturnType = NonGenericType Definitions.Int
            Generics = 0        
        }

    let Operators =
        TypeDefinition {
            Assembly = "FSharp.Core"
            FullName = "Microsoft.FSharp.Core.Operators"
        }
    

let newId() = Id.New(mut = false)
let namedId (i: FSharpMemberOrFunctionOrValue) =
    if i.IsCompilerGenerated then
        let n = i.DisplayName.TrimStart('(', ' ', '_', '@')
        if n.Length > 0 then
            Id.New(n.Substring(0, 1), i.IsMutable)
        else
            Id.New(mut = i.IsMutable)
    elif i.IsActivePattern then
        Id.New(i.DisplayName.Split('|').[1], i.IsMutable)
    else
        let n = i.DisplayName
        if n = "( builder@ )" then Id.New("b", i.IsMutable)
        else Id.New(n, i.IsMutable) 

type MatchValueVisitor(okToInline: int[]) =
    inherit Visitor()

    override this.VisitMatchSuccess(i, _) =
        let ok = okToInline.[i] 
        if ok >= 0 then
            okToInline.[i] <- okToInline.[i] + 1               

type MatchValueTransformer(c, twoCase) =
    inherit Transformer()

    override this.TransformMatchSuccess(index, results) =
        let resVal = 
            if twoCase then
                Value (Bool (index = 0))
            else
                Value (Int index)
        match results with
        | [] -> resVal
        | [ capt ] -> Sequential [ VarSet (c, capt); resVal ] 
        | _ -> Sequential [ VarSet (c, NewArray results); resVal ] 

type InlineMatchValueTransformer(cases : (Id list * Expression) list) =
    inherit Transformer()

    let cases = Array.ofList cases

    override this.TransformMatchSuccess(index, results) =
        let captures, body = cases.[index]    
        body |> List.foldBack (fun (c, r) body -> Let (c, r, body)) (List.zip captures results)

let removeListOfArray (argType: FSharpType) (expr: Expression) =
    if isSeq argType then
        match IgnoreExprSourcePos expr with
        | Call (None, td, meth, [ NewArray _ as arr ]) 
            when td.Entity = Definitions.ListModule && meth.Entity = Definitions.ListOfArray  ->
                arr
        | NewUnionCase(td, "Empty", []) when td.Entity = Definitions.List ->
            NewArray []
        | _ -> expr
    else expr

type SymbolReader(comp : WebSharper.Compiler.Compilation) as self =

    let attrReader =
        { new A.AttributeReader<FSharpAttribute>() with
            override this.GetAssemblyName attr = self.ReadSimpleName attr.AttributeType.Assembly
            override this.GetName attr = attr.AttributeType.LogicalName
            override this.GetCtorArgs attr = attr.ConstructorArguments |> Seq.map snd |> Array.ofSeq          
            override this.GetTypeDef o = self.ReadTypeDefinition (o :?> FSharpType).TypeDefinition
        }

    member this.ReadSimpleName (a: FSharpAssembly) =
        match a.FileName with
        | None -> comp.AssemblyName
        | _ -> a.SimpleName

    member this.ReadTypeDefinition (td: FSharpEntity) =
        if td.IsArrayType then
            TypeDefinition {
                Assembly = "mscorlib"
                FullName = "System.Array`1"
            }
        else
        let td = getOrigDef td
        let res =
            {
                Assembly = this.ReadSimpleName td.Assembly 
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

    member this.ReadTypeSt markStaticTP (tparams: Map<string, int>) (t: FSharpType) =
        if t.IsGenericParameter then
            match tparams.TryFind t.GenericParameter.Name with
            | Some i -> 
                if markStaticTP && t.GenericParameter.IsSolveAtCompileTime then StaticTypeParameter i else TypeParameter i
            | _ ->
                LocalTypeParameter
        else
        let t = getOrigType t
        let getFunc() =
            match t.GenericArguments |> Seq.map (this.ReadTypeSt markStaticTP tparams) |> List.ofSeq with
            | [a; r] -> FSharpFuncType(a, r)
            | _ -> failwith "impossible: FSharpFunc must have 2 type parameters"
        if t.IsTupleType then
            t.GenericArguments |> Seq.map (this.ReadTypeSt markStaticTP tparams) |> List.ofSeq |> TupleType
        elif t.IsFunctionType then
            getFunc()
        else
        let td = t.TypeDefinition
        if td.IsArrayType then
            ArrayType(this.ReadTypeSt markStaticTP tparams t.GenericArguments.[0], td.ArrayRank)
        elif td.IsByRef then
            ByRefType(this.ReadTypeSt markStaticTP tparams t.GenericArguments.[0])
        else
            let fn = 
                if td.IsProvidedAndErased then td.LogicalName else td.FullName
            if fn.StartsWith "System.Tuple" then
                t.GenericArguments |> Seq.map (this.ReadTypeSt markStaticTP tparams) |> List.ofSeq |> TupleType
            elif fn = "Microsoft.FSharp.Core.FSharpFunc`2" then
                getFunc()
            elif fn = "Microsoft.FSharp.Core.Unit" || fn = "System.Void" then
                VoidType
            else
                let td = this.ReadTypeDefinition td
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
                    GenericType td (t.GenericArguments |> Seq.map (this.ReadTypeSt markStaticTP tparams) |> List.ofSeq)

    member this.ReadType tparams t = this.ReadTypeSt false tparams t

    member this.ReadAbstractSlot tparams (x: FSharpAbstractSignature) : Method =
        let tparams =
            Seq.append 
                (Map.toSeq tparams)
                (x.DeclaringTypeGenericParameters |> Seq.mapi (fun i p -> p.Name, i)) 
            |> Map.ofSeq
        Method {
            MethodName = x.Name
            Parameters = x.AbstractArguments |> Seq.concat |> Seq.map (fun p -> this.ReadType tparams p.Type) |> List.ofSeq |> removeUnitParam
            ReturnType = this.ReadType tparams x.AbstractReturnType
            Generics   = x.MethodGenericParameters.Count
        } 

    member this.ReadMember (x : FSharpMemberOrFunctionOrValue) : Member =
        let name = x.CompiledName

        if name = ".cctor" then Member.StaticConstructor else

        let tparams = 
            Seq.append x.EnclosingEntity.GenericParameters x.GenericParameters
            |> Seq.distinctBy (fun p -> p.Name)
            |> Seq.mapi (fun i p -> p.Name, i) |> Map.ofSeq

        let getPars() =
            let ps =  
                x.CurriedParameterGroups |> Seq.concat |> Seq.map (fun p -> this.ReadType tparams p.Type) |> List.ofSeq |> removeUnitParam  
            if x.IsInstanceMember && not x.IsInstanceMemberInCompiledCode then
                GenericType (this.ReadTypeDefinition x.LogicalEnclosingEntity) 
                    (List.init x.LogicalEnclosingEntity.GenericParameters.Count (fun i -> TypeParameter i)) :: ps
            else ps

        if name = ".ctor" || name = "CtorProxy" then
            Member.Constructor <| Constructor {
                CtorParameters = getPars()
            }  
        else
            if x.IsOverrideOrExplicitInterfaceImplementation then
                let s = 
                    match x.ImplementedAbstractSignatures |> Seq.tryHead with
                    | Some s -> s
                    | _ -> failwithf "Failed to read abstract signature of %s" x.FullName

                let iTparams = 
                    Seq.append s.DeclaringTypeGenericParameters s.MethodGenericParameters
                    |> Seq.distinctBy (fun p -> p.Name)
                    |> Seq.mapi (fun i p -> p.Name, i) |> Map.ofSeq
            
                let i = this.ReadTypeDefinition s.DeclaringType.TypeDefinition

                let meth = this.ReadAbstractSlot iTparams s

                if x.IsExplicitInterfaceImplementation then
                    Member.Implementation(i, meth)    
                else
                    Member.Override(i, meth)    
            else 
        
                Member.Method(
                    x.IsInstanceMemberInCompiledCode,
                    Method {
                        MethodName = name
                        Parameters = getPars()
                        ReturnType = this.ReadType tparams x.ReturnParameter.Type
                        Generics   = tparams.Count - x.EnclosingEntity.GenericParameters.Count
                    } 
                )

    member this.ReadAndRegisterTypeDefinition (comp: Compilation) (td: FSharpEntity) =
        let res = this.ReadTypeDefinition td

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
                                sign.DelegateArguments |> Seq.map (snd >> this.ReadType tparams) |> List.ofSeq
                            ReturnType = this.ReadType tparams sign.DelegateReturnType
                        }
                    with _ ->
                        let inv = td.MembersFunctionsAndValues |> Seq.find(fun m -> m.CompiledName = "Invoke")
                        M.DelegateInfo {
                            DelegateArgs =
                                inv.CurriedParameterGroups |> Seq.concat |> Seq.map (fun p -> this.ReadType tparams p.Type) |> List.ofSeq
                            ReturnType = this.ReadType tparams inv.ReturnParameter.Type
                        }
                comp.AddCustomType(res, info)
        res
    
    member this.AttributeReader = attrReader

type Environment =
    {
        ScopeIds : list<FSharpMemberOrFunctionOrValue * Id * VarKind>
        TParams : Map<string, int>
        Exception : option<Id>
        Compilation : Compilation
        SymbolReader : SymbolReader
    }
    static member New(vars, tparams, comp, sr) = 
//        let tparams = Array.ofSeq tparams
//        if tparams |> Array.distinct |> Array.length <> tparams.Length then
//            failwithf "Repeating type parameter names: %A" tparams
        { 
            ScopeIds = vars |> Seq.map (fun (i, (v, k)) -> i, v, k) |> List.ofSeq 
            TParams = tparams |> Seq.mapi (fun i p -> p, i) |> Map.ofSeq
            Exception = None
            Compilation = comp
            SymbolReader = sr 
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
    
let rec (|CompGenClosure|_|) (expr: FSharpExpr) =
    match expr with 
    | P.Let((clo1, value), P.Lambda (x1, (P.Application(P.Value clo2, _, [P.Value x2]) | CompGenClosure(P.Application(P.Value clo2, _, [P.Value x2]))))) 
        when clo1.IsCompilerGenerated && clo1 = clo2 && x1 = x2 ->
            Some value
    | _ -> None

let (|CompGenLambda|_|) n (expr: FSharpExpr) =
    let rec get acc n expr =
        if n = 0 then Some (List.rev acc, expr) else
        match expr with    
        | P.Lambda(id, body) when id.IsCompilerGenerated ->
            get (id :: acc) (n - 1) body
        | _ -> None 
    get [] n expr

let rec transformExpression (env: Environment) (expr: FSharpExpr) =
    let inline tr x = transformExpression env x
    let sr = env.SymbolReader
    try
        match expr with
        | P.Value(var) ->                
            if isUnit var.FullType then
                Undefined
            else
                let v, k = env.LookupVar var
                match k with
                | LocalVar -> Var v  
                | FuncArg -> Var v
                | ByRefArg -> GetRef (Var v)
                | ThisArg -> This
        | P.Lambda _ ->
            let rec loop acc = function
                | P.Lambda (var, body) -> loop (var :: acc) body
                | body -> (List.rev acc, body)
            
            let lam vars body isUnitReturn =
                if isUnitReturn then
                    Function(vars, ExprStatement body)
                else
                    Lambda(vars, body)   
            match loop [] expr with
            | [arg], body ->
                if isUnit arg.FullType then 
                    lam [] (tr body) (isUnit body.Type)
                else 
                    let v = namedId arg
                    let env = env.WithVar(v, arg)
                    lam [v] (body |> transformExpression env) (isUnit body.Type)
            | args, body ->
                let vars, env =
                    (env, args) ||> List.mapFold (fun env arg ->
                        let v = namedId arg
                        v, env.WithVar(v, arg)
                    ) 
                let trBody = body |> transformExpression env
                trBody |> List.foldBack (fun v e -> lam [v] e (obj.ReferenceEquals(trBody, isUnit) && isUnit body.Type)) vars
        | P.Application(func, types, args) ->
            let compGenCurriedAppl (env: Environment) ids body =
                let vars, env =
                    (env, ids) ||> List.mapFold (fun env arg ->
                        let v = namedId arg
                        v, env.WithVar(v, arg)
                    ) 
                let inline tr x = transformExpression env x
                List.foldBack2 (fun i v b -> Let(i, tr v, b)) vars args (tr body)
            match func with
            | P.Let((o, objectArg), CompGenLambda args.Length (ids, body)) ->
                let ov = namedId o
                let trObjectArg = tr objectArg
                Let(ov, tr objectArg, compGenCurriedAppl (env.WithVar(ov, o)) ids body)    
            | CompGenLambda args.Length (ids, body) ->
                compGenCurriedAppl env ids body   
            | _ ->
            match IgnoreExprSourcePos (tr func) with
            | CallNeedingMoreArgs(thisObj, td, m, ca) ->
                Call(thisObj, td, m, ca @ (args |> List.map tr))
            | trFunc ->
                match args with
                | [a] when isUnit a.Type ->
                    let trA = tr a |> removeListOfArray a.Type
                    match IgnoreExprSourcePos trA with
                    | Undefined | Value Null -> Application (trFunc, [], false, Some 0)
                    | _ -> Sequential [ trA; Application (trFunc, [], false, Some 0) ]
                | _ ->
                    let trArgs = args |> List.map (fun a -> tr a |> removeListOfArray a.Type)
                    curriedApplication trFunc trArgs
        // eliminating unneeded compiler-generated closures
        | CompGenClosure value ->
            tr value
        | P.Let((id, value), body) ->
            let i = namedId id
            let trValue = tr value
            let env = env.WithVar(i, id, if isByRef id.FullType then ByRefArg else LocalVar)
            let inline tr x = transformExpression env x
            if id.IsMutable then
                Sequential [ NewVar(i, trValue); tr body ]
            else
                Let (i, trValue, tr body)
        | P.LetRec(defs, body) ->
            let mutable env = env
            let ids = defs |> List.map (fun (id, _) ->
                let i = namedId id
                env <- env.WithVar(i, id, if isByRef id.FullType then ByRefArg else LocalVar)
                i
            )
            let inline tr x = transformExpression env x
            LetRec (
                Seq.zip ids defs 
                |> Seq.map (fun (i, (_, v)) -> i, tr v) |> List.ofSeq, 
                tr body
            )
        | P.Call(this, meth, typeGenerics, methodGenerics, arguments) ->
            let td = sr.ReadAndRegisterTypeDefinition env.Compilation meth.EnclosingEntity
            if td.Value.FullName = "Microsoft.FSharp.Core.Operators" && meth.CompiledName = "Reraise" then
                IgnoredStatementExpr (Throw (Var env.Exception.Value))    
            else
                let t = Generic td (typeGenerics |> List.map (sr.ReadType env.TParams))
                let args = 
                    arguments |> List.map (fun a ->
                        let ta = tr a
                        if isByRef a.Type then
                            match IgnoreExprSourcePos ta with
                            | Application(ItemGet (r, Value (String "get")), [], _, _) -> r
                            | _ -> ta
                        else ta |> removeListOfArray a.Type
                    )
                let args, before =
                    match args with
                    | [ a ] when isUnit arguments.[0].Type ->
                        match IgnoreExprSourcePos a with
                        | Undefined | Value Null -> [], None
                        | _ -> [], Some a     
                    | _ -> args, None
                let call =
                    match sr.ReadMember meth with
                    | Member.Method (isInstance, m) -> 
                        let mt = Generic m (methodGenerics |> List.map (sr.ReadType env.TParams))
                        if isInstance then
                            Call (Option.map tr this, t, mt, args)
                        else 
                            if meth.IsInstanceMember && not meth.IsExtensionMember then
                                CallNeedingMoreArgs (None, t, mt, Option.toList (Option.map tr this) @ args)
                            else 
                                Call (None, t, mt, Option.toList (Option.map tr this) @ args)
                    | Member.Implementation (i, m) ->
                        let t = Generic i (typeGenerics |> List.map (sr.ReadType env.TParams))
                        let mt = Generic m (methodGenerics |> List.map (sr.ReadType env.TParams))
                        Call (Option.map tr this, t, mt, args)
                    | Member.Override (_, m) ->
                        let mt = Generic m (methodGenerics |> List.map (sr.ReadType env.TParams))
                        Call (Option.map tr this, t, mt, args)
                    | Member.Constructor c -> Ctor (t, c, args)
                    | Member.StaticConstructor -> parsefailf "Invalid: direct call to static constructor" 
                match before with
                | None -> call
                | Some a -> Sequential [a; call]
        | P.Sequential _ ->
            let rec getSeq acc expr =
                match expr with            
                | P.Sequential (f, s) ->
                    getSeq (f :: acc) s   
                | _ -> expr :: acc
            getSeq [] expr |> List.rev |> List.map tr |> Sequential
        | P.Const (value, _) ->
            Value(ReadLiteral value)
        | P.IfThenElse (cond, then_, else_) ->
            Conditional(tr cond, tr then_, tr else_)    
        | P.NewObject (ctor, typeGenerics, arguments) -> 
            let td = sr.ReadAndRegisterTypeDefinition env.Compilation ctor.EnclosingEntity
            let t = Generic td (typeGenerics |> List.map (sr.ReadType env.TParams))
            let args = List.map tr arguments
            let args, before =
                match args with
                | [ a ] when isUnit arguments.[0].Type ->
                    match IgnoreExprSourcePos a with
                    | Undefined | Value Null -> [], None
                    | _ -> [], Some a     
                | _ -> args, None
            let call =
                match sr.ReadMember ctor with
                | Member.Constructor c -> Ctor (t, c, args)
                | _ -> parsefailf "Expected a constructor call"
            match before with
            | None -> call
            | Some a -> Sequential [a; call]
        | P.TryFinally (body, final) ->
            let res = newId()
            StatementExpr (TryFinally(VarSetStatement(res, tr body), ExprStatement (tr final)), Some res)
        | P.TryWith (body, var, filter, e, catch) -> // TODO: var, filter?
            let err = namedId e
            let res = newId()
            StatementExpr (
                TryWith(VarSetStatement(res, tr body), 
                    Some err, 
                    (VarSetStatement(res, transformExpression (env.WithException(err, e)) catch)))
                , Some res)
        | P.NewArray (_, items) ->
            NewArray (items |> List.map tr)              
        | P.NewTuple (_, items) ->
            NewArray (items |> List.map tr)              
        | P.WhileLoop (cond, body) ->
            IgnoredStatementExpr(While(tr cond, ExprStatement (Capturing().CaptureValueIfNeeded(tr body))))
        | P.ValueSet (var, value) ->
            if var.IsModuleValueOrMember then
                let td = sr.ReadAndRegisterTypeDefinition env.Compilation var.EnclosingEntity
                match sr.ReadMember var with
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
                | FuncArg -> failwith "function argument cannot be set"
                | ByRefArg -> SetRef (Var v) (tr value)
                | ThisArg -> failwith "'this' parameter cannot be set"
        | P.TupleGet (_, i, tuple) ->
            ItemGet(tr tuple, Value (Int i))   
        | P.FastIntegerForLoop (start, end_, body, up) ->
            let j = newId()
            let i, trBody =
                match IgnoreExprSourcePos (tr body) with
                | Function ([i], ExprStatement b) -> i, b
                | _ -> parsefailf "Unexpected form of consumeExpr in FastIntegerForLoop pattern"     
            For (
                Some (Sequential [NewVar(i, tr start); NewVar (j, tr end_)]), 
                Some (if up then Binary(Var i, BinaryOperator.``<=``, Var j) else Binary(Var i, BinaryOperator.``>=``, Var j)), 
                Some (if up then MutatingUnary(MutatingUnaryOperator.``()++``, Var i)  else MutatingUnary(MutatingUnaryOperator.``()--``, Var i)), 
                ExprStatement (Capturing(i).CaptureValueIfNeeded(trBody))
            ) |> IgnoredStatementExpr
        | P.TypeTest (typ, expr) ->
            TypeCheck (tr expr, sr.ReadType env.TParams typ)
        | P.Coerce (typ, expr) ->
            tr expr // TODO: type check when possible
        | P.NewUnionCase (typ, case, exprs) ->
            let t =
                match sr.ReadType env.TParams typ with
                | ConcreteType ct -> ct
                | _ -> parsefailf "Expected a union type"
            if t.Entity = Definitions.List then
                let rec getItems acc e =
                    match e with 
                    | P.NewUnionCase (_, _, [h; t]) ->
                        getItems (h :: acc) t 
                    | P.NewUnionCase (_, _, []) ->
                        if acc.Length > 1 then Some (List.rev acc) else None
                    | _ -> None
                match exprs with
                | [] ->
                    NewUnionCase(t, case.CompiledName, [])
                | [h; r] -> 
                    match getItems [ h ] r with
                    | Some fromArr -> 
                        Call(None, NonGeneric Definitions.ListModule, NonGeneric Definitions.ListOfArray, [ NewArray (fromArr |> List.map tr) ])    
                    | None ->
                        NewUnionCase(t, case.CompiledName, exprs |> List.map tr) 
                | _ -> parsefailf "Invalid number of union fields for FSharpList"
            else
                NewUnionCase(t, case.CompiledName, exprs |> List.map tr)
        | P.UnionCaseGet (expr, typ, case, field) ->
            let t =
                match sr.ReadType env.TParams typ with
                | ConcreteType ct -> ct
                | _ -> parsefailf "Expected a union type"
            UnionCaseGet(tr expr, t, case.CompiledName, field.Name)
        | P.UnionCaseTest (expr, typ, case) ->
            let t =
                match sr.ReadType env.TParams typ with
                | ConcreteType ct -> ct
                | _ -> parsefailf "Expected a union type"
            UnionCaseTest(tr expr, t, case.CompiledName)
        | P.UnionCaseTag (expr, typ) ->
            let t =
                match sr.ReadType env.TParams typ with
                | ConcreteType ct -> ct
                | _ -> parsefailf "Expected a union type"
            UnionCaseTag(tr expr, t)
        | P.NewRecord (typ, items) ->
            let td = typ.TypeDefinition 
            let t =
                match sr.ReadType env.TParams typ with
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
        | P.DecisionTree (matchValue, cases) ->
            let trMatchVal = transformExpression env matchValue
            let simpleMatchCases = System.Collections.Generic.Dictionary()
            let rec isSimpleMatch mv expr =
                match IgnoreExprSourcePos expr with 
                | Conditional(I.Call(None, td, m, [I.Var c; I.Value v]), I.MatchSuccess (succ, []), elseExpr)
                    when td.Entity.Value.FullName = "Microsoft.FSharp.Core.Operators" && m.Entity.Value.MethodName = "op_Equality" ->
                        Dict.addToMulti simpleMatchCases succ (Some v)
                        match mv with
                        | Some mc -> if mc = c then isSimpleMatch mv elseExpr else None
                        | None -> isSimpleMatch (Some c) elseExpr       
                | MatchSuccess(succ, []) ->
                    Dict.addToMulti simpleMatchCases succ None
                    mv
                | _ -> None
            let simpleMatchValue = 
                match trMatchVal with
                | I.Let (mv, mvalue, expr) -> 
                    isSimpleMatch (Some mv) expr |> Option.map (fun _ -> mvalue)
                | _ -> 
                    isSimpleMatch None trMatchVal |> Option.map Var

            let trCases =
                cases |> List.map (fun (ci, e) ->
                    let mutable env = env 
                    let captures =
                        ci |> List.map (fun cv ->
                            let i = namedId cv
                            env <- env.WithVar (i, cv)
                            i
                        )
                    captures,
                    transformExpression env e
                )
            let okToInline = Array.create cases.Length 0  
            trCases |> List.iteri (fun i (_, e) -> if isTrivialValue e then okToInline.[i] <- -1)
            MatchValueVisitor(okToInline).VisitExpression(trMatchVal)

            if okToInline |> Array.forall (fun i -> i <= 1) then
                InlineMatchValueTransformer(trCases).TransformExpression(trMatchVal)    
            else
                let c = newId()
                let mv = MatchValueTransformer(c, trCases.Length = 2).TransformExpression(trMatchVal)
                let cs = 
                    trCases |> List.map (fun (captures, body) ->
                        match captures with
                        | [] -> body
                        | [capt] ->
                            ReplaceId(capt, c).TransformExpression(body)
                        | _ ->
                            body |> List.foldBack (fun (i, id) body -> 
                                Let(id, ItemGet(Var c, Value (Int i)), body)) (List.indexed captures)
                    )
                
                Sequential [
                    NewVar(c, Undefined)
                    (
                        match cs with
                        | [c1] ->
                            Sequential [mv; c1]
                        | [c1; c2] ->
                            Conditional(mv, c1, c2)
                        | _ ->
                            let res = newId()
                            match simpleMatchValue with
                            | None ->
                                let css =
                                    cs |> List.mapi (fun j e ->
                                        Some (Value (Int j)),
                                        Block [
                                            VarSetStatement(res, e) 
                                            Break None 
                                        ]
                                    )
                                StatementExpr(Switch(mv, css), Some res)
                            | Some smv ->
                                let css =
                                    cs |> List.mapi (fun j e ->
                                        simpleMatchCases.[j] |> List.map (Option.map Value),
                                        Block [
                                            VarSetStatement(res, e) 
                                            Break None 
                                        ]
                                    )
                                StatementExpr(CSharpSwitch(smv, css), Some res)    
                    )
                ]

        | P.DecisionTreeSuccess (index, results) ->
            MatchSuccess (index, results |> List.map tr)
        | P.ThisValue (typ) ->
            This
        | P.FSharpFieldGet (thisOpt, typ, field) ->
            let t = 
                match sr.ReadType env.TParams typ with
                | ConcreteType ct -> ct
                | _ -> parsefailf "Expected a record type"
            FieldGet(thisOpt |> Option.map tr, t, field.Name)
        | P.FSharpFieldSet (thisOpt, typ, field, value) ->
            let t = 
                match sr.ReadType env.TParams typ with
                | ConcreteType ct -> ct
                | _ -> parsefailf "Expected a record type"
            FieldSet(thisOpt |> Option.map tr, t, field.Name, tr value)
        | P.AddressOf expr ->
            let isStructUnionGet =
                let t = expr.Type
                t.HasTypeDefinition && (
                    let td = t.TypeDefinition
                    td.IsFSharpUnion && td.IsValueType
                )    
            if isStructUnionGet then
                tr expr     
            else
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
            | Application(ItemGet (r, Value (String "get")), [], _, _) ->
                r   
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
        | P.AddressSet (addr, value) ->
            match addr with
            | P.Value(var) ->
                let v, _ = env.LookupVar var
                SetRef (Var v) (tr value)
            | _ -> failwith "AddressSet not on a Value"
        | P.ObjectExpr (typ, expr, overrides, interfaces) ->
            let o = newId()
            let r = newId()
            let plainObj =
                Let (o, Object [],
                    Sequential [
                        for ovr in Seq.append overrides (interfaces |> Seq.collect snd) do
                            let i = sr.ReadAndRegisterTypeDefinition env.Compilation ovr.Signature.DeclaringType.TypeDefinition
                            let s = sr.ReadAbstractSlot env.TParams ovr.Signature
                            let mutable env = env
                            let thisVar, vars =
                                match ovr.CurriedParameterGroups with
                                | [t] :: a ->
                                    let thisVar = namedId t
                                    env <- env.WithVar(thisVar, t)
                                    thisVar,
                                    a |> Seq.concat |> Seq.map (fun v ->
                                        let vv = namedId v
                                        env <- env.WithVar(vv, v)
                                        vv
                                    ) |> List.ofSeq 
                                | _ ->
                                    failwith "Wrong `this` argument in object expression override"
                            let b = FuncWithThis (thisVar, vars, Return (transformExpression env ovr.Body)) 
                            yield ItemSet(Var o, OverrideName(i, s), b)
                        yield Var o
                    ]
                )
            Let(r, CopyCtor(sr.ReadAndRegisterTypeDefinition env.Compilation typ.TypeDefinition, plainObj),
                Sequential [
                    yield FixCtorTransformer(Var r).TransformExpression(tr expr)
                    yield Var r
                ]
            )
        | P.DefaultValue typ ->
            if typ.IsGenericParameter && typ.GenericParameter.Constraints |> Seq.exists (fun c -> c.IsReferenceTypeConstraint || c.IsSupportsNullConstraint) then
                Value Null
            else
                DefaultValueOf (sr.ReadType env.TParams typ)
        | P.NewDelegate (typ, arg) ->
            // TODO : loop for exact length of delegate type
            let rec loop acc = function
                | P.Lambda (var, body) -> loop (var :: acc) body
                | body -> (List.rev acc, body)

            match loop [] arg with
            | ([], P.Application (f, _, [P.Const (null, _)])) ->
                tr f
            | vars, body ->
                let mutable env = env
                let args = 
                    vars |> List.map (fun v -> 
                        let vv = namedId v
                        env <- env.WithVar(vv, v)
                        vv
                    )
                Lambda (args, transformExpression env body)
            | _ -> failwith "Failed to translate delegate creation"
        | P.TypeLambda (gen, expr) ->
            tr expr
        | P.Quote expr -> tr expr
        | P.BaseValue _ -> Base
        | P.ILAsm("[I_ldelema (NormalAddress,false,ILArrayShape [(Some 0, null)],TypeVar 0us)]", _, [ arr; i ]) ->
            let arrId = newId()
            let iId = newId()
            Let (arrId, tr arr, Let(iId, tr i, MakeRef (ItemGet(Var arrId, Var iId)) (fun value -> ItemSet(Var arrId, Var iId, value))))
        | P.ILAsm ("[I_ldarg 0us]", [], []) ->
            This
        | P.ILAsm ("[AI_ldnull; AI_cgt_un]", [], [ arr ]) ->
            tr arr  
        | P.ILAsm ("[I_ldlen; AI_conv DT_I4]", [], [ arr ]) ->
            Call(Some (tr arr), NonGeneric Definitions.Array, NonGeneric Definitions.ArrayLength, [])
        | P.ILAsm (s, _, _) ->
            parsefailf "Unrecognized ILAsm: %s" s
        | P.ILFieldGet _ -> parsefailf "F# pattern not handled: ILFieldGet"
        | P.ILFieldSet _ -> parsefailf "F# pattern not handled: ILFieldSet"
        | P.TraitCall(sourceTypes, traitName, memberFlags, typeArgs, typeInstantiation, argExprs) ->
            if sourceTypes.Length <> 1 then parsefailf "TODO: TraitCall with multiple source types" 
            match argExprs with
            | t :: a -> 
                let meth =
                    Method {
                        MethodName = traitName
                        Parameters = typeInstantiation |> List.map (sr.ReadTypeSt true env.TParams)
                        ReturnType = sr.ReadTypeSt true env.TParams expr.Type
                        Generics   = 0
                    } 
                TraitCall(tr t, sr.ReadType env.TParams sourceTypes.[0], NonGeneric meth, a |> List.map tr)  
            | _ ->
                failwith "Impossible: TraitCall must have a this argument"
        | P.UnionCaseSet _ ->
            parsefailf "UnionCaseSet pattern is only allowed in FSharp.Core"
        | _ -> parsefailf "F# expression not recognized"
    with e ->
        let msg =
            match e with
            | ParseError m -> m
            | _ -> "Error while reading F# code: " + e.Message + " " + e.StackTrace
        env.Compilation.AddError(Some (getSourcePos expr), WebSharper.Compiler.SourceError msg)
        errorPlaceholder        
    |> withSourcePos expr
