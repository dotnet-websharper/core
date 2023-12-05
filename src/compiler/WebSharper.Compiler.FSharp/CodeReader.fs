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

module internal rec WebSharper.Compiler.FSharp.CodeReader

open FSharp.Compiler.Symbols
 
open WebSharper.Core
open WebSharper.Core.AST
open WebSharper.Compiler
open System.Collections.Generic

module M = WebSharper.Core.Metadata
module A = WebSharper.Compiler.AttributeReader
module I = IgnoreSourcePos
module P = FSharpExprPatterns

type VarKind =
    | LocalVar 
    | FuncArg
    | ByRefArg
    | ThisArg
         
let rec getOrigDef (td: FSharpEntity) =
    if td.IsFSharpAbbreviation then getOrigDef td.AbbreviatedType.TypeDefinition else td 

let rec getOrigType (t: FSharpType) =
    if t.IsAbbreviation then getOrigType t.AbbreviatedType else t

let isByRefDef (td: FSharpEntity) =
    // workaround for FCS bug 
    td.IsByRef || td.CompiledName = "byref`2"

let isUnit (t: FSharpType) =
    let t = getOrigType t
    if not t.HasTypeDefinition then false else
    let td = t.TypeDefinition
    if td.IsArrayType || isByRefDef td then false
    elif td.IsProvidedAndErased then false
    else td.FullName = "Microsoft.FSharp.Core.Unit" || td.FullName = "System.Void"

let isOption (t: FSharpType) =
    let t = getOrigType t
    t.HasTypeDefinition &&
        let td = t.TypeDefinition
        not td.IsProvidedAndErased &&
        match td.TryFullName with
        | Some "Microsoft.FSharp.Core.FSharpOption`1"
        | Some "Microsoft.FSharp.Core.FSharpValueOption`1" -> true
        | _ -> false

let rec isSeq (t: FSharpType) = 
    let t = getOrigType t
    (
        t.HasTypeDefinition &&
            let td = t.TypeDefinition
            not td.IsProvidedAndErased &&
            td.TryFullName = Some "System.Collections.Generic.IEnumerable`1"
    ) || (
        t.IsGenericParameter && 
            t.GenericParameter.Constraints
            |> Seq.exists (fun c -> c.IsCoercesToConstraint && isSeq c.CoercesToTarget)
    )

let isByRef (t: FSharpType) =
    let t = getOrigType t
    if t.IsGenericParameter then
        false
    else
    if t.IsTupleType || t.IsFunctionType then false else
    t.HasTypeDefinition && isByRefDef t.TypeDefinition

let getFuncArg t =
    let rec get acc (t: FSharpType) =
        if t.IsFunctionType then
            let a = t.GenericArguments.[0] 
            let r = t.GenericArguments.[1] 
            let i = if a.IsTupleType then a.GenericArguments.Count else 1
            get (i :: acc) r 
        else
            match acc with
            | [] | [1] -> 
                if isByRef t then
                    match t.TypeDefinition.LogicalName with
                    | "byref`2" ->
                        match t.GenericArguments.[1].TypeDefinition.LogicalName with
                        | "Out" -> OutRefArg
                        | "In" -> InRefArg
                        | _ -> NotOptimizedFuncArg
                    | "outref`1" -> OutRefArg
                    | "inref`1" -> InRefArg
                    | _ ->
                        NotOptimizedFuncArg
                else
                    NotOptimizedFuncArg
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

let getRange (range: FSharp.Compiler.Text.Range) =
    {   
        FileName = range.FileName
        Start = range.StartLine, range.StartColumn + 1
        End = range.EndLine, range.EndColumn + 1
    }

let getSourcePos (x: FSharpExpr) =
    getRange x.Range

let withSourcePos (x: FSharpExpr) (expr: Expression) =
    ExprSourcePos (getSourcePos x, IgnoreExprSourcePos expr)

let getDeclaringEntity (x : FSharpMemberOrFunctionOrValue) =
    match x.DeclaringEntity with
    | Some e -> e
    | None -> failwithf "Enclosing entity not found for %s" x.FullName
  
let (|This|_|) v expr =
    match expr, v with
    | I.Var t, Some vv when t = vv -> Some()
    | _ -> None

type FixCtorTransformer(typ, btyp, thisVar) =
    inherit Transformer()

    let mutable addedChainedCtor = false
    let mutable cgenFieldNames = []

    override this.TransformSequential (es) =
        match es with
        // handle class self alias, simplify to call to this.
        | I.FieldSet(Some (This thisVar), _, f, I.NewRecord(_, [DefaultValueOf _])) 
            :: I.Let (self, I.FieldGet _, o)
            :: I.FieldSet(Some (I.FieldGet(Some (This thisVar), _, _)), _, _, (This thisVar))
            :: I.FieldSet(Some (This thisVar), _, i, I.Value (Int 1))
            :: t ->
                cgenFieldNames <- [ f; i ]
                printfn "self identifier found"
                Sequential (SubstituteVar(self, Var thisVar.Value).TransformExpression(this.TransformExpression(o)) :: t)  
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
        if addedChainedCtor then Ctor (t, c, a) else
        addedChainedCtor <- true
        let isBase = t.Entity <> typ
        let tn = typ.Value.FullName
        if (not isBase || Option.isSome btyp) && not (tn = "System.Object" || tn = "System.Exception") then
            if t.Entity.Value.FullName = "System.Exception" then 
                let args, inner =
                    match a with
                    | [] -> [], None
                    | [_] -> a, None
                    | [msg; inner] -> [msg], Some inner 
                    | _ -> failwith "Too many arguments for Error"
                Sequential [
                    yield Appl(Base, args, NonPure, None)
                    match inner with
                    | Some i ->
                        yield ItemSet(JSThis, Value (String "inner"), i)
                    | None -> ()
                ]
            else
                ChainedCtor(isBase, t, c, a) 
        else JSThis

    member this.Fix(expr) = 
        let res = this.TransformExpression(expr)
        match btyp with
        | Some b when not addedChainedCtor -> 
            Sequential [ ChainedCtor(true, b, ConstructorInfo.Default(), []); res ]
        | _ -> res
        , cgenFieldNames

let fixCtor thisTyp baseTyp thisVar expr =
    FixCtorTransformer(thisTyp, baseTyp, thisVar).Fix(expr)

module Definitions =
    let List =
        TypeDefinition {
            Assembly = "FSharp.Core"
            FullName = "Microsoft.FSharp.Collections.FSharpList`1"  
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
            Assembly = "netstandard"
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

    let IntrinsicFunctions =
        TypeDefinition {
            Assembly = "FSharp.Core"
            FullName = "Microsoft.FSharp.Core.LanguagePrimitives+IntrinsicFunctions"
        }

    let CheckThis =
        Method {
            MethodName = "CheckThis"
            Parameters = [ TypeParameter 0 ]
            ReturnType = TypeParameter 0
            Generics = 1      
        }
    
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

let rec IgnoreCoerce expr =
    match expr with
    | ExprSourcePos(_, e) -> IgnoreCoerce e
    | Coerce(e, _, _) -> IgnoreCoerce e
    | _ -> expr

let removeListOfArray (argType: FSharpType) (expr: Expression) =
    if isSeq argType then
        match IgnoreCoerce expr with
        | Call (None, td, meth, [ NewArray _ as arr ]) 
            when td.Entity = Definitions.ListModule && meth.Entity = Definitions.ListOfArray  ->
                arr
        | NewUnionCase(td, "Empty", []) when td.Entity = Definitions.List ->
            NewArray []
        | _ -> expr
    else expr

type SymbolReader(comp : WebSharper.Compiler.Compilation) as self =

    let mutable anonRecords = null : IDictionary<string[], string>
    
    let readSimpleName (a: FSharpAssembly) typeFullName =
        if Option.isNone a.FileName then // currently compiled assembly
            comp.AssemblyName
        else
            match AssemblyConventions.StandardAssemblyNameForTypeNamed typeFullName with
            | Some n -> n
            | None -> a.SimpleName

    let attrReader =
        { new A.AttributeReader<FSharpAttribute>() with
            override this.GetAssemblyName attr =
                readSimpleName attr.AttributeType.Assembly (attr.AttributeType.QualifiedName.Split([|','|]).[0])
            override this.GetName attr = attr.AttributeType.LogicalName
            override this.GetCtorArgs attr = attr.ConstructorArguments |> Seq.map snd |> Array.ofSeq
            override this.GetNamedArgs attr = attr.NamedArguments |> Seq.map (fun (_, n, _, v) -> n, v) |> Array.ofSeq
            override this.GetTypeDef o = (self.ReadType Map.empty (o :?> FSharpType) : Type).TypeDefinition
        }

    member this.ResolveAnonRecord (d: FSharpAnonRecordTypeDetails) =
        let asmName = readSimpleName d.Assembly d.CompiledName
        
        let fallback() =
            TypeDefinition {
                Assembly = asmName
                FullName = d.CompiledName
            }
            
        // anon records have a timestamp in their compiled name, use reflection to find name in dll if possible
        // only for currently compiled assembly
        if Option.isNone d.Assembly.FileName && Option.isNone comp.ProxyTargetName then
            if isNull anonRecords then
                let asmOpt = 
                    try Some (Reflection.LoadAssembly comp.AssemblyName)
                    with _ -> None
                match asmOpt with
                | Some asm ->
                    anonRecords <-
                        asm.DefinedTypes 
                        |> Seq.filter (fun t -> t.Name.StartsWith("<>f__AnonymousType")) 
                        |> Seq.map (fun t ->
                            let sortedFieldNames = 
                                t.DeclaredProperties
                                |> Seq.sortBy(fun p ->
                                    let a = p.GetCustomAttributes(typeof<CompilationMappingAttribute>, false)[0] :?> CompilationMappingAttribute
                                    a.SequenceNumber
                                )
                                |> Seq.map(fun p -> p.Name)
                                |> Array.ofSeq
                            sortedFieldNames, t.Name
                        )
                        |> dict
                | _ -> 
                    anonRecords <- dict []
            match anonRecords.TryFind(d.SortedFieldNames) with
            | Some rname ->
                TypeDefinition {
                    Assembly = asmName
                    FullName = rname
                }
            | None ->
                // fallback for bundle-only projects (no compiled dll)
                fallback()
        else
            // for referenced libs and proxied lib
            fallback()

    member this.ReadTypeDefinition (td: FSharpEntity) =
        if td.IsArrayType then
            TypeDefinition {
                Assembly = "netstandard"
                FullName = "System.Array`1"
            }
        else
        let td = getOrigDef td
        let fullName =
            if td.IsProvidedAndErased then td.LogicalName else
            td.QualifiedName.Split([|','|]).[0] 
        let res =
            {
                Assembly = comp.FindProxiedAssembly(readSimpleName td.Assembly fullName)
                FullName = fullName
            }
        // TODO: more measure types
        match res.Assembly with
        | "FSharp.Core" ->
            match res.FullName with
            | "Microsoft.FSharp.Core.byte`1" -> 
                { Assembly = "netstandard"; FullName = "System.Byte" }   
            | "Microsoft.FSharp.Core.syte`1" -> 
                { Assembly = "netstandard"; FullName = "System.SByte" }   
            | "Microsoft.FSharp.Core.int16`1" -> 
                { Assembly = "netstandard"; FullName = "System.Int16" }   
            | "Microsoft.FSharp.Core.int`1" -> 
                { Assembly = "netstandard"; FullName = "System.Int32" }   
            | "Microsoft.FSharp.Core.uint16`1" ->
                { Assembly = "netstandard"; FullName = "System.UInt16" }   
            | "Microsoft.FSharp.Core.uint32`1" -> 
                { Assembly = "netstandard"; FullName = "System.UInt32" }   
            | "Microsoft.FSharp.Core.decimal`1" -> 
                { Assembly = "netstandard"; FullName = "System.Decimal" }   
            | "Microsoft.FSharp.Core.int64`1" -> 
                { Assembly = "netstandard"; FullName = "System.Int64" }   
            | "Microsoft.FSharp.Core.uint64`1" -> 
                { Assembly = "netstandard"; FullName = "System.UInt64" }   
            | "Microsoft.FSharp.Core.float32`1" ->
                { Assembly = "netstandard"; FullName = "System.Single" }   
            | "Microsoft.FSharp.Core.float`1" ->
                { Assembly = "netstandard"; FullName = "System.Double" }   
            | _ -> res
        | _ -> res
        |> fun x -> TypeDefinition x |> comp.FindProxied

    member this.ReadTypeSt markStaticTP (tparams: Map<string, int>) (t: FSharpType) =
        let t = getOrigType t
        if t.IsGenericParameter then
                match tparams.TryFind t.GenericParameter.Name with
                | Some i -> 
                    if markStaticTP && t.GenericParameter.IsSolveAtCompileTime then StaticTypeParameter i else TypeParameter i
                | _ ->
                    LocalTypeParameter
        else
        let getFunc() =
            match t.GenericArguments |> Seq.map (this.ReadTypeSt markStaticTP tparams) |> List.ofSeq with
            | [a; r] -> FSharpFuncType(a, r)
            | _ -> failwith "impossible: FSharpFunc must have 2 type parameters"
        let getTupleType isStruct =
            let tts = t.GenericArguments |> Seq.map (this.ReadTypeSt markStaticTP tparams) |> List.ofSeq
            TupleType(tts, isStruct)
        if t.IsStructTupleType then
            getTupleType true
        elif t.IsTupleType then
            getTupleType false
        elif t.IsFunctionType then
            getFunc()
        elif t.IsAnonRecordType then
            let d = t.AnonRecordTypeDetails
            let def = this.ResolveAnonRecord(d)
            if not (comp.HasCustomTypeInfo def) then
                let info =
                    d.SortedFieldNames
                    |> List.ofArray
                    |> M.FSharpAnonRecordInfo
                comp.AddCustomType(def, info)
            GenericType def (t.GenericArguments |> Seq.map (this.ReadTypeSt markStaticTP tparams) |> List.ofSeq)
        else
        // measure type parameters do not have a TypeDefinition
        // reusing LocalTypeParameter case as it is also fully erased
        if not t.HasTypeDefinition then LocalTypeParameter else
        let td = t.TypeDefinition
        if td.IsArrayType then
            ArrayType(this.ReadTypeSt markStaticTP tparams t.GenericArguments.[0], td.ArrayRank)
        elif isByRefDef td then
            ByRefType(this.ReadTypeSt markStaticTP tparams t.GenericArguments.[0])
        else
            let fn = 
                if td.IsProvidedAndErased then td.LogicalName else
                try
                    td.FullName
                with _ -> failwithf "No FullName: LogicalName '%s', CompiledName '%s'" td.LogicalName td.CompiledName 
            if fn.StartsWith "System.Tuple" then
                getTupleType false
            elif fn.StartsWith "System.ValueTuple" then
                getTupleType true
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

    member this.ReadType tparams t = (this.ReadTypeSt false tparams t).Normalize()

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

    member this.ReadMember (x : FSharpMemberOrFunctionOrValue, ?cls: FSharpEntity) : Member =
        let name = x.CompiledName

        if name = ".cctor" then Member.StaticConstructor else

        let declEnt = cls |> Option.defaultWith (fun () -> getDeclaringEntity x)

        let tparams = 
            Seq.append declEnt.GenericParameters x.GenericParameters
            |> Seq.distinctBy (fun p -> p.Name)
            |> Seq.mapi (fun i p -> p.Name, i) |> Map.ofSeq

        let getPars() =
            let ps =  
                x.CurriedParameterGroups |> Seq.concat |> Seq.map (fun p -> this.ReadType tparams p.Type) |> List.ofSeq |> removeUnitParam  
            if x.IsInstanceMember && not x.IsInstanceMemberInCompiledCode then
                let extOn = x.ApparentEnclosingEntity
                GenericType (this.ReadTypeDefinition extOn) 
                    (List.init extOn.GenericParameters.Count (fun i -> TypeParameter i)) :: ps
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
                        Generics   = tparams.Count - declEnt.GenericParameters.Count
                    } 
                )
        |> comp.ResolveProxySignature

    member this.ReadAndRegisterTypeDefinition (comp: Compilation) (td: FSharpEntity) =
        let res = this.ReadTypeDefinition td

        if td.IsDelegate then 
            if not (comp.HasCustomTypeInfo res) then
                let tparams = 
                    td.GenericParameters
                    |> Seq.mapi (fun i p -> p.Name, i) |> Map.ofSeq
                let info =
                    // todo: Optional and DefaultValue attributes on F# delegate arguments
                    try 
                        let sign = td.FSharpDelegateSignature
                        M.DelegateInfo {
                            DelegateArgs =
                                sign.DelegateArguments |> Seq.map (fun (_, a) -> this.ReadType tparams a, None) |> List.ofSeq
                            ReturnType = this.ReadType tparams sign.DelegateReturnType
                        }
                    with _ ->
                        let inv = td.MembersFunctionsAndValues |> Seq.find(fun m -> m.CompiledName = "Invoke")
                        M.DelegateInfo {
                            DelegateArgs =
                                inv.CurriedParameterGroups |> Seq.concat |> Seq.map (fun p -> this.ReadType tparams p.Type, None) |> List.ofSeq
                            ReturnType = this.ReadType tparams inv.ReturnParameter.Type
                        }
                comp.AddCustomType(res, info)
        res
    
    member this.AttributeReader = attrReader

type Environment =
    {
        ScopeIds : list<FSharpMemberOrFunctionOrValue * Id * VarKind>
        mutable This : option<Id>
        TParams : Map<string, int>
        FreeVars : ResizeArray<FSharpMemberOrFunctionOrValue * Id * VarKind>
        Exception : option<Id>
        Compilation : Compilation
        SymbolReader : SymbolReader
        RecMembers : Dictionary<FSharpMemberOrFunctionOrValue, Id * FSharpExpr>
        mutable RecMemberUsed : option<Id * FSharpExpr>
    }
    static member New(vars, isCtor, tparams, comp, sr, rm) = 
//        let tparams = Array.ofSeq tparams
//        if tparams |> Array.distinct |> Array.length <> tparams.Length then
//            failwithf "Repeating type parameter names: %A" tparams
        { 
            ScopeIds = vars |> Seq.map (fun (i, (v, k)) -> i, v, k) |> List.ofSeq 
            This = 
                vars 
                |> Seq.tryPick (function (_, (i, ThisArg)) -> Some i | _ -> None) 
                |> Option.orElseWith (fun () -> if isCtor then Some (Id.NewThis()) else None)
            TParams = tparams |> Seq.mapi (fun i p -> p, i) |> Map.ofSeq
            FreeVars = ResizeArray()
            Exception = None
            Compilation = comp
            SymbolReader = sr 
            RecMembers = rm
            RecMemberUsed = None
        }

    member this.WithVar(i: Id, v: FSharpMemberOrFunctionOrValue, ?k) =
        { this with ScopeIds = (v, i, defaultArg k LocalVar) :: this.ScopeIds }

    member this.WithThis(i: Id) =
        { this with This = Some i }

    member this.WithException (i: Id, v: FSharpMemberOrFunctionOrValue) =
        { this with 
            ScopeIds = (v, i, LocalVar) :: this.ScopeIds
            Exception = Some i }

    member this.LookupVar (v: FSharpMemberOrFunctionOrValue) =
        let isMatch (sv, i, k) = if sv = v then Some (i, k) else None
        match this.ScopeIds |> List.tryPick isMatch with
        | Some var -> var
        | None ->
            match this.FreeVars |> Seq.tryPick isMatch with
            | Some var -> var
            | None ->
                match this.RecMembers.TryGetValue v with
                | true, ((id, _) as rm) -> 
                    this.RecMemberUsed <- Some rm
                    id, VarKind.LocalVar
                | _ ->
                    let id = namedId (Some this) false v
                    let kind = VarKind.FuncArg
                    this.FreeVars.Add((v, id, kind))
                    id, kind

    member this.ThisVar =
        match this.This with
        | Some t -> Var t
        | None -> failwith "'this' identifier not found"
            
let newId() = Id.New(mut = false)
let namedId (env: option<Environment>) isOpt (i: FSharpMemberOrFunctionOrValue) =
    let typ = env |> Option.bind (fun env -> i.FullTypeSafe |> Option.map (env.SymbolReader.ReadType env.TParams))
    if i.IsCompilerGenerated then
        let n = i.DisplayName.TrimStart('(', ' ', '_', '@')
        if n.Length > 0 then
            Id.New(n.Substring(0, 1), i.IsMutable, opt = isOpt, ?typ = typ)
        else
            Id.New(mut = i.IsMutable, opt = isOpt, ?typ = typ)
    elif i.IsActivePattern then
        Id.New(i.DisplayName.Split('|').[1], i.IsMutable, ?typ = typ)
    else
        let n = i.DisplayName
        if n = "( builder@ )" then Id.New("b", i.IsMutable, opt = isOpt, ?typ = typ)
        else Id.New(n, i.IsMutable, opt = isOpt, ?typ = typ) 

let rec (|CompGenClosure|_|) (expr: FSharpExpr) =
    match expr with 
    | P.Let((clo1, value, _), P.Lambda (x1, (P.Application(P.Value clo2, _, [P.Value x2]) | CompGenClosure(P.Application(P.Value clo2, _, [P.Value x2]))))) 
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
            let t = var.FullType
            if isUnit t then
                Undefined
            else
                let v, k = env.LookupVar var
                match k with
                | LocalVar 
                | FuncArg 
                | ThisArg -> Var v
                | ByRefArg -> 
                    let t = getOrigType expr.Type
                    if t.HasTypeDefinition && isByRefDef t.TypeDefinition then Var v else GetRef (Var v)
        | P.Lambda _ ->
            let rec loop acc = function
                | P.Lambda (var, body) -> loop (var :: acc) body
                | body -> (List.rev acc, body)
            
            let lam vars typ body isUnitReturn =
                if isUnitReturn then
                    Function(vars, None, None, ExprStatement body)
                else
                    Lambda(vars, typ, body)   
            match loop [] expr with
            | [arg], body ->
                if isUnit arg.FullType then 
                    lam [] (Some (sr.ReadType env.TParams body.Type)) (tr body) (isUnit body.Type)
                else 
                    let t = arg.FullType
                    let v = namedId (Some env) (isUnit t || t.IsGenericParameter) arg
                    let env = env.WithVar(v, arg)
                    lam [v] (Some (sr.ReadType env.TParams body.Type)) (body |> transformExpression env) (isUnit body.Type)
            | args, body ->
                let vars, env =
                    (env, args) ||> List.mapFold (fun env arg ->
                        let t = arg.FullType
                        let v = namedId (Some env) (isUnit t || t.IsGenericParameter) arg
                        v, env.WithVar(v, arg)
                    ) 
                let trBody = body |> transformExpression env
                trBody |> List.foldBack (fun v e ->
                    let typ =
                        if obj.ReferenceEquals(trBody, e)
                        then Some (sr.ReadType env.TParams body.Type)
                        else None
                    lam [v] typ e false
                ) vars
        | P.Application(func, types, args) ->
            let compGenCurriedAppl (env: Environment) ids body =
                let vars, env =
                    (env, ids) ||> List.mapFold (fun env arg ->
                        let v = namedId (Some env) false arg
                        v, env.WithVar(v, arg)
                    ) 
                let inline tr x = transformExpression env x
                let trArg x = tr x |> removeListOfArray x.Type
                List.foldBack2 (fun i v b -> Let(i, trArg v, b)) vars args (tr body)
            let trArg x = tr x |> removeListOfArray x.Type
            match func with
            | P.Let((o, objectArg, _), CompGenLambda args.Length (ids, body)) ->
                let ov = namedId (Some env) false o
                Let(ov, tr objectArg, compGenCurriedAppl (env.WithVar(ov, o)) ids body)    
            | CompGenLambda args.Length (ids, body) ->
                compGenCurriedAppl env ids body   
            | _ ->
            match IgnoreExprSourcePos (tr func) with
            | CallNeedingMoreArgs(thisObj, td, m, ca) ->
                Call(thisObj, td, m, ca @ (args |> List.map trArg))
            | trFunc ->
                match args with
                | [a] when isUnit a.Type ->
                    applyUnitArg trFunc (trArg a)
                | _ ->
                    let trArgs = args |> List.map (fun a -> isUnit a.Type, trArg a)
                    curriedApplication trFunc trArgs
        // eliminating unneeded compiler-generated closures
        | CompGenClosure value ->
            tr value
        | P.Let((id, value, _), body) ->
            let i = namedId (Some env) false id
            let trValue = tr value
            let env = env.WithVar(i, id, if isByRef id.FullType then ByRefArg else LocalVar)
            let inline tr x = transformExpression env x
            if id.IsMutable then
                Sequential [ NewVar(i, trValue); tr body ]
            else
                Let (i, trValue, tr body)
        | P.LetRec(defs, body) ->
            let mutable env = env
            let ids = defs |> List.map (fun (id, _, _) ->
                let i = namedId (Some env) false id
                env <- env.WithVar(i, id, if isByRef id.FullType then ByRefArg else LocalVar)
                i
            )
            let inline tr x = transformExpression env x
            LetRec (
                Seq.zip ids defs 
                |> Seq.map (fun (i, (_, v, _)) -> i, tr v) |> List.ofSeq, 
                tr body
            )
        | P.Call(this, meth, typeGenerics, methodGenerics, arguments) ->
            let td = sr.ReadAndRegisterTypeDefinition env.Compilation (getDeclaringEntity meth)
            if td.Value.FullName = "Microsoft.FSharp.Core.Operators" && meth.CompiledName = "Reraise" then
                IgnoredStatementExpr (Throw (Var env.Exception.Value))    
            else
                let t = Generic td (typeGenerics |> List.map (sr.ReadType env.TParams))
                let args = 
                    arguments |> List.map (fun a ->
                        let ta = tr a
                        if isByRef a.Type then
                            match IgnoreExprSourcePos ta with
                            | Application(ItemGet (r, Value (String "get"), _), [], _) -> r
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
                        if td = Definitions.IntrinsicFunctions && m = Definitions.CheckThis then
                            env.ThisVar
                        else
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
            let trCond = tr cond
            match trCond with
            | IsClientCall b ->
                if b then tr then_ else tr else_
            | _ ->
                Conditional(trCond, tr then_, tr else_)    
        | P.NewObject (ctor, typeGenerics, arguments) -> 
            let td = sr.ReadAndRegisterTypeDefinition env.Compilation (getDeclaringEntity ctor)
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
        | P.TryFinally (body, final, _, _) ->
            let res = newId()
            StatementExpr (TryFinally(VarSetStatement(res, tr body), ExprStatement (tr final)), Some res)
        | P.TryWith (body, var, filter, e, catch, _, _) -> // TODO: var, filter?
            let err = namedId (Some env) false e
            let res = newId()
            StatementExpr (
                TryWith(VarSetStatement(res, tr body), 
                    Some err, 
                    (VarSetStatement(res, transformExpression (env.WithException(err, e)) catch)))
                , Some res)
        | P.NewArray (_, items) ->
            NewArray (items |> List.map tr)              
        | P.NewTuple (typ, items) ->
            match sr.ReadType env.TParams typ with
            | TupleType (ts, _) ->
                NewTuple ((items |> List.map tr), ts)    
            | _ -> failwith "Expecting a tuple type for NewTuple"
        | P.WhileLoop (cond, body, _) ->
            IgnoredStatementExpr(While(tr cond, ExprStatement (Capturing().CaptureValueIfNeeded(tr body))))
        | P.ValueSet (var, value) ->
            if var.IsModuleValueOrMember then
                let td = sr.ReadAndRegisterTypeDefinition env.Compilation (getDeclaringEntity var)
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
            ItemGet(tr tuple, Value (Int i), Pure)   
        | P.FastIntegerForLoop (start, end_, body, up, _, _) ->
            let j = newId()
            let i, trBody =
                match IgnoreExprSourcePos (tr body) with
                | Function ([i], _, _, ExprStatement b) -> i, b
                | _ -> parsefailf "Unexpected form of consumeExpr in FastIntegerForLoop pattern"    
            let i = i.ToMutable()
            For (
                Some (Sequential [NewVar(i, tr start); NewVar (j, tr end_)]), 
                Some (if up then Binary(Var i, BinaryOperator.``<=``, Var j) else Binary(Var i, BinaryOperator.``>=``, Var j)), 
                Some (if up then MutatingUnary(MutatingUnaryOperator.``()++``, Var i)  else MutatingUnary(MutatingUnaryOperator.``()--``, Var i)), 
                ExprStatement (Capturing(i).CaptureValueIfNeeded(trBody))
            ) |> IgnoredStatementExpr
        | P.TypeTest (typ, expr) ->
            TypeCheck (tr expr, sr.ReadType env.TParams typ)
        | P.Coerce (typ, expr) ->
            Coerce (tr expr, sr.ReadType env.TParams expr.Type, sr.ReadType env.TParams typ)  
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
                        Some (List.rev acc)
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
                            FieldSet(Some env.ThisVar, t, f.Name, tr a)
                    ]
        | P.NewAnonRecord (typ, items) ->
            let t =
                match sr.ReadType env.TParams typ with
                | ConcreteType ct -> ct
                | _ -> parsefailf "Expected a named type"
            NewRecord (t, List.map tr items)
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
                            let i = namedId (Some env) false cv
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
                                Let(id, ItemGet(Var c, Value (Int i), Pure), body)) (List.indexed captures)
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
            env.ThisVar
        | P.FSharpFieldGet (thisOpt, typ, field) ->
            let t = 
                match sr.ReadType env.TParams typ with
                | ConcreteType ct -> ct
                | _ -> parsefailf "Expected a named type in FSharpFieldGet"
            FieldGet(thisOpt |> Option.map tr, t, field.Name)
        | P.FSharpFieldSet (thisOpt, typ, field, value) ->
            let t = 
                match sr.ReadType env.TParams typ with
                | ConcreteType ct -> ct
                | _ -> parsefailf "Expected a named type in FSharpFieldSet"
            FieldSet(thisOpt |> Option.map tr, t, field.Name, tr value)
        | P.ILFieldGet (thisOpt, typ, field) -> 
            let t = 
                match sr.ReadType env.TParams typ with
                | ConcreteType ct -> ct
                | _ -> parsefailf "Expected a named type in ILFieldGet"
            FieldGet(thisOpt |> Option.map tr, t, field)
        | P.ILFieldSet (thisOpt, typ, field, value) ->
            let t = 
                match sr.ReadType env.TParams typ with
                | ConcreteType ct -> ct
                | _ -> parsefailf "Expected a named type in ILFieldSet"
            FieldSet(thisOpt |> Option.map tr, t, field, tr value)
        | P.AnonRecordGet (expr, typ, index) ->
            let t =
                match sr.ReadType env.TParams typ with
                | ConcreteType ct -> ct
                | _ -> parsefailf "Expected a named type"
            FieldGet (Some (tr expr), t, typ.AnonRecordTypeDetails.SortedFieldNames.[index])
        | P.AddressOf expr ->
            let isStructUnionOrTupleGet =
                let t = getOrigType expr.Type
                t.IsStructTupleType 
                || t.IsAnonRecordType
                || (
                    t.HasTypeDefinition && (
                        let td = t.TypeDefinition
                        td.IsFSharpUnion && td.IsValueType
                    )    
                )
            if isStructUnionOrTupleGet then
                tr expr     
            else
            let e = IgnoreExprSourcePos (tr expr)
            match e with
            | Var v ->
                MakeRef e (fun value -> VarSet(v, value)) v.VarType
            | ItemGet(o, i, p) ->
                let ov = newId()
                let iv = newId()
                Let (ov, o, Let(iv, i, MakeRef (ItemGet(Var ov, Var iv, p)) (fun value -> ItemSet(Var ov, Var iv, value)) None))
            | FieldGet(o, t, f) ->
                match o with
                | Some o ->
                    let ov = newId()
                    Let (ov, o, MakeRef (FieldGet(Some (Var ov), t, f)) (fun value -> FieldSet(Some (Var ov), t, f, value)) None)     
                | _ ->
                    MakeRef e (fun value -> FieldSet(None, t, f, value)) None
            | Application(ItemGet (r, Value (String "get"), _), [], _) ->
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
                MakeRef e (fun value -> Call(None, td, setm, [value])) None  
            | _ -> failwithf "AddressOf error, unexpected form: %+A" e 
        | P.AddressSet (addr, value) ->
            match addr with
            | P.Value(var) ->
                let v, _ = env.LookupVar var
                SetRef (Var v) (tr value)
            | _ -> failwith "AddressSet not on a Value"
        | P.ObjectExpr (typ, expr, overrides, interfaces) ->
            let typ' = sr.ReadType env.TParams typ
            let ctor =
                match tr expr with
                | Undefined -> None
                | e -> Some e
            let overrides =
                [
                    for ovr in Seq.append overrides (interfaces |> Seq.collect snd) do
                        let i = sr.ReadAndRegisterTypeDefinition env.Compilation ovr.Signature.DeclaringType.TypeDefinition
                        let s = sr.ReadAbstractSlot env.TParams ovr.Signature
                        let mutable env = env
                        let thisVar, vars =
                            match ovr.CurriedParameterGroups with
                            | [t] :: args ->
                                let thisVar = namedId (Some env) false t
                                env <- env.WithVar(thisVar, t).WithThis(thisVar)
                                let args = 
                                    match args with
                                    | [[ a ]] when isUnit a.FullType -> [[]]
                                    | _ -> args
                                thisVar,
                                args |> Seq.concat |> Seq.map (fun v ->
                                    let vv = namedId (Some env) false v
                                    env <- env.WithVar(vv, v)
                                    vv
                                ) |> List.ofSeq 
                            | _ ->
                                failwith "Wrong `this` argument in object expression override"
                        let b = Function (vars, Some thisVar, Some typ', Return (transformExpression env ovr.Body)) 
                        yield i, s, b
                ]
            ObjectExpr(typ', ctor, overrides)
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
            | [ v ], body when isUnit v.FullType  ->
                Lambda ([], None, transformExpression env body)
            | vars, body ->
                let mutable env = env
                let args = 
                    vars |> List.map (fun v -> 
                        let vv = namedId (Some env) false v
                        env <- env.WithVar(vv, v)
                        vv
                    )
                Lambda (args, Some (sr.ReadType env.TParams body.Type), transformExpression env body)
            | _ -> failwith "Failed to translate delegate creation"
        | P.TypeLambda (gen, expr) ->
            tr expr
        | P.Quote expr -> tr expr
        | P.BaseValue _ -> Base
        | P.ILAsm
            (
                ("[I_ldelema (NormalAddress,false,ILArrayShape [(Some 0, None)],TypeVar 0us)]" 
                    | "[I_ldelema (NormalAddress,false,ILArrayShape [(Some 0, None)],!0)]"
                    | "[I_ldelema (NormalAddress, false, ILArrayShape [(Some 0, None)], !0)]"), _, [ arr; i ]
            ) ->
            let arrId = newId()
            let iId = newId()
            Let (arrId, tr arr, Let(iId, tr i, MakeRef (ItemGet(Var arrId, Var iId, NoSideEffect)) (fun value -> ItemSet(Var arrId, Var iId, value)) None))
        | P.ILAsm ("[I_ldarg 0us]", [], []) ->
            env.ThisVar
        | P.ILAsm ("[AI_ldnull; AI_cgt_un]", [], [ arr ]) ->
            tr arr  
        | P.ILAsm ("[I_ldlen; AI_conv DT_I4]", [], [ arr ]) ->
            Call(Some (tr arr), NonGeneric Definitions.Array, NonGeneric Definitions.ArrayLength, [])
        | P.ILAsm (s, _, _) ->
            parsefailf "Unrecognized ILAsm: %s" s
        | P.TraitCall(sourceTypes, traitName, memberFlags, typeArgs, typeInstantiation, argExprs) ->
            let isInstance = memberFlags.IsInstance
            let meth =
                Method {
                    MethodName = traitName
                    Parameters = argExprs |> List.skip (if isInstance then 1 else 0) |> List.map (fun e -> e.Type |> sr.ReadTypeSt true env.TParams)
                    ReturnType = sr.ReadTypeSt true env.TParams expr.Type
                    Generics   = 0
                } 
            let s = sourceTypes |> Seq.map (sr.ReadType env.TParams) |> List.ofSeq
            let m = Generic meth (typeInstantiation @ typeArgs |> List.map (sr.ReadType env.TParams))
            if isInstance then 
                match argExprs |> List.map tr with
                | t :: a ->
                    TraitCall(Some t, s, m, a)
                | _ ->
                    failwith "No this value found for instance trait call"
            else
                TraitCall(None, s, m, argExprs |> List.map tr)  
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

type FSharp.Compiler.Text.Range with
    member this.AsSourcePos =
        {
            FileName = System.IO.Path.GetFileName(this.FileName)
            Start = this.StartLine, this.StartColumn
            End = this.EndLine, this.EndColumn
        }

// Searches for calls within server-side code to method with JavaScript-enabled parameters.
// These quotations or auto-quoted expressions passed are then translated by WebSharper.
let scanExpression (env: Environment) (containingMethodName: string) (expr: FSharpExpr) =
    let vars = Dictionary<FSharpMemberOrFunctionOrValue, FSharpExpr>()
    let quotations = ResizeArray()
    let quotedMethods = ResizeArray()
    let rec scan (expr: FSharpExpr) =
        let default'() =
            List.iter scan expr.ImmediateSubExpressions
        try
            let storeExprTranslation (mem: FSharpMemberOrFunctionOrValue) (indexes: int[]) (arguments: FSharpExpr list) =
                let pars = mem.CurriedParameterGroups |> Seq.concat |> Array.ofSeq
                indexes |> Array.iter (fun i ->
                    let arg = arguments[i]
                    let p = pars[i]
                    let e, withValue =
                        match arg with
                        | P.Quote e -> Some e, false
                        | P.Call(None, wv, _, _, [_; P.Quote e]) 
                            when wv.FullName = "Microsoft.FSharp.Quotations.WithValue" -> Some e, true
                        | P.Value v ->
                            match vars.TryGetValue v with
                            | true, e -> Some e, false
                            | false, _ -> None, false
                        | _ -> None, false
                    let expectWithValue =
                        pars[i].Attributes |> Seq.exists (fun a -> 
                            a.AttributeType.FullName = "Microsoft.FSharp.Core.ReflectedDefinitionAttribute"
                            && a.ConstructorArguments |> Seq.exists (fun (_, v) -> v = true)
                        )
                    match e with
                    | Some e ->
                        let pos = e.Range.AsSourcePos
                        if expectWithValue && not withValue then
                            env.Compilation.AddWarning(Some pos, SourceWarning "Auto-quoted argument expected to have access to server-side value. Use `( )` instead of `<@ @>`.")   
                        let e = transformExpression env e
                        let argTypes = [ for (v, _, _) in env.FreeVars -> env.SymbolReader.ReadType Map.empty v.FullType ]
                        let retTy = env.SymbolReader.ReadType Map.empty mem.ReturnParameter.Type
                        let qm =
                            Hashed {
                                MethodInfo.Generics = 0
                                MethodInfo.MethodName = sprintf "%s$%i$%i" containingMethodName (fst pos.Start) (snd pos.Start)
                                MethodInfo.Parameters = argTypes
                                MethodInfo.ReturnType = retTy
                            }
                        let argNames = [ for (v, id, _) in env.FreeVars -> v.LogicalName ]
                        let f = Lambda([ for (_, id, _) in env.FreeVars -> id ], None, e)
                        // emptying FreeVars so that env can be reused for reading multiple quotation arguments
                        env.FreeVars.Clear()
                        // if the quotation is a single static call, the runtime fallback will be able to 
                        // handle it without introducing a pre-compiled function for it
                        let isTrivial =
                            match e with 
                            | I.Call(None, _, _, args) ->
                                args |> List.forall (function I.Var _ | I.Value _ -> true | _ -> false)
                            | _ -> false
                        if not isTrivial then
                            quotations.Add(pos, qm, argNames, f) 
                        else
                            match e with 
                            | I.Call(None, td, m, _) ->
                                quotedMethods.Add(td, m) 
                            | _ -> ()
                    | None -> scan arg
                )
            
            match expr with
            | P.Let ((id, (P.Quote value), _), body) ->
                // I'd rather pass around a Map than do this dictionary mutation,
                // but the type FSharpMemberOrFunctionOrValue isn't comparable :(
                vars.[id] <- value
                scan body
                vars.Remove(id) |> ignore
            | P.Call(this, meth, typeGenerics, methodGenerics, arguments) ->
                let typ = env.SymbolReader.ReadTypeDefinition(getDeclaringEntity meth)
                match env.SymbolReader.ReadMember(meth) with
                | Member.Method(_, m) ->
                    match env.Compilation.TryLookupQuotedArgMethod(typ, m) with
                    | Some indexes ->
                        Option.iter scan this
                        arguments |> List.iteri (fun i a -> 
                            if indexes |> Array.contains i |> not then
                                scan a
                        )
                        storeExprTranslation meth indexes arguments
                    | _ -> default'()
                | _ -> default'()
            | P.NewObject(ctor, typeList, arguments) ->
                let typ = env.SymbolReader.ReadTypeDefinition(getDeclaringEntity ctor)
                match env.SymbolReader.ReadMember(ctor) with
                | Member.Constructor(con) ->
                    match env.Compilation.TryLookupQuotedConstArgMethod(typ, con) with
                    | Some indexes ->
                        arguments |> List.iteri (fun i a -> 
                            if indexes |> Array.contains i |> not then
                                scan a
                        )
                        storeExprTranslation ctor indexes arguments
                    | _ -> default'()
                | _ -> default'()
            | _ -> default'()
        with _ -> 
            // some TP-s can create code that FCS fails to expose, ignore that
            // see https://github.com/dotnet-websharper/core/issues/904
            ()
    scan expr
    quotations :> _ seq, quotedMethods :> _ seq
