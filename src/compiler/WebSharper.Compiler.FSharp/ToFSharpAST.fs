module internal WebSharper.Compiler.FSharp.ToFSharpAST

//open System.Runtime.CompilerServices

open Microsoft.FSharp.Compiler.SourceCodeServices
 
open WebSharper.Core
open WebSharper.Core.AST
       
type Environment =
    {
        Vars : System.Collections.Generic.Dictionary<FSharpMemberOrFunctionOrValue, Id>
        TParams : Map<string, int>
        Exception : option<Id>
        MatchVars : option<Id * Id>
        Compilation : Metadata.Compilation
    }
    static member New(vars, tparams, comp) = 
        let d = System.Collections.Generic.Dictionary()
        for k, v in vars do d.Add(k, v)
        { 
            Vars = d 
            TParams = tparams |> Seq.mapi (fun i p -> p, i) |> Map.ofSeq
            Exception = None
            MatchVars = None
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

    member this.AddVar (i: Id, v: FSharpMemberOrFunctionOrValue) =
        if this.Vars.ContainsKey v then 
//            failwith "problem"
            ()// TODO: investigate
        else    
            this.Vars.Add(v, i)

    member this.WithException (i: Id, v: FSharpMemberOrFunctionOrValue) =
        this.AddVar(i, v)
        { this with Exception = Some i }

    member this.LookupVar (v: FSharpMemberOrFunctionOrValue) =
        this.Vars.[v]

let rec getOrigDef (td: FSharpEntity) =
    if td.IsFSharpAbbreviation then getOrigDef td.AbbreviatedType.TypeDefinition else td 

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
            Assembly = td.Assembly.SimpleName //td.Assembly.FileName |> Option.toObj
            FullName = 
    //            try 
                    let res = td.QualifiedName.Split([|','|]).[0] 
    //                if not (res.Contains("+")) && res <> td.FullName then
    //                    failwith "WTF"
                    res
    //            with _ -> 
    //                td.FullName
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
    override this.GetAssemblyName attr = attr.AttributeType.Assembly.SimpleName
    override this.GetName attr = attr.AttributeType.LogicalName
    override this.GetCtorArgs attr = attr.ConstructorArguments |> Seq.map snd |> Array.ofSeq          
    override this.GetTypeDef o = getTypeDefinition (o :?> FSharpType).TypeDefinition

let attrReader = FSharpAttributeReader()

let rec getOrigType (t: FSharpType) =
    if t.IsAbbreviation then getOrigType t.AbbreviatedType else t

//let funcDef =
//    Hashed {
//        Assembly = "FSharp.Core"
//        FullName = "Microsoft.FSharp.Core.FSharpFunc`2"
//    }

let isUnit (t: FSharpType) =
    if t.IsGenericParameter then
        false
    else
    let t = getOrigType t
    if t.IsTupleType || t.IsFunctionType then false else
    let td = t.TypeDefinition
    if td.IsArrayType then false
    else td.FullName = "Microsoft.FSharp.Core.Unit" || td.FullName = "System.Void"

let rec getType (tparams: Map<string, int>) (t: FSharpType) =
    if t.IsGenericParameter then
        GenericType tparams.[t.GenericParameter.Name]
    else
    let t = getOrigType t
    if t.IsTupleType then
//        let tupleTypeDef i =
//            Hashed {
//                Assembly = "mscorlib"
//                FullName = "System.Tuple`" + string i
//            }
//        let rec getTupleTD (args: _[]) =
//            match args.Length with
//            | i when i < 8 ->
//                concreteType (tupleTypeDef i, args |> List.ofArray)
//            | i ->
//                concreteType (tupleTypeDef i, Seq.append (args.[.. 6]) (Seq.singleton (getTupleTD args.[7 ..])) |> List.ofSeq)    
        t.GenericArguments |> Seq.map (getType tparams) |> List.ofSeq |> TupleType
    elif t.IsFunctionType then
//        concreteType(funcDef, t.GenericArguments |> Seq.map (getType tparams) |> List.ofSeq)
        let [a; r] = t.GenericArguments |> Seq.map (getType tparams) |> List.ofSeq
        FSharpFuncType(a, r)
    else
    let td = t.TypeDefinition
    if td.IsArrayType then
        ArrayType(getType tparams t.GenericArguments.[0], 1) // TODO: multi dimensional arrays   
    else
//        let td = getTypeDefinition td
        if td.FullName.StartsWith "System.Tuple" then
            t.GenericArguments |> Seq.map (getType tparams) |> List.ofSeq |> TupleType
        elif td.FullName = "Microsoft.FSharp.Core.FSharpFunc`2" then
            let [a; r] = t.GenericArguments |> Seq.map (getType tparams) |> List.ofSeq
            FSharpFuncType(a, r)
        elif td.FullName = "Microsoft.FSharp.Core.Unit" || td.FullName = "System.Void" then
            VoidType
        else
            let td = getTypeDefinition td
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
            | "System.Double" -> concreteType (td, [])  
            | _ ->
                concreteType (td, t.GenericArguments |> Seq.map (getType tparams) |> List.ofSeq)

//            concreteType (getTypeDefinition td, 
//                t.GenericArguments |> Seq.choose (fun t -> 
//                    if t.HasTypeDefinition && t.TypeDefinition.IsMeasure then None else Some (getType tparams t)
//                ) |> List.ofSeq
//            )

let removeUnitParam (ps: list<Type>) =
    match ps with 
    | [ VoidType ] -> []
    //| [ ConcreteType { Entity = t } ] when t.Value.FullName.StartsWith "Microsoft.FSharp.Core.Unit" -> []
    | _ -> ps

//let isUnionUsingNull (x: FSharpEntity) =
//    x.IsFSharpUnion && (
//        x.Attributes |> Seq.exists (fun a ->
//            a.AttributeType.FullName = "Microsoft.FSharp.Core.CompilationRepresentationAttribute"
//            && obj.Equals(snd a.ConstructorArguments.[0], int CompilationRepresentationFlags.UseNullAsTrueValue) //
//        )
//    )  
//    <CompilationRepresentation(CompilationRepresentationFlags.Instance)

let hasCompilationRepresentation (cr: CompilationRepresentationFlags) attrs =
    attrs |> Seq.exists (fun (a: FSharpAttribute) ->
        a.AttributeType.FullName = "Microsoft.FSharp.Core.CompilationRepresentationAttribute"
        && obj.Equals(snd a.ConstructorArguments.[0], int cr)
    )

let makeByref getVal setVal =
    let value = Id.New "v"
    Object [
        "get", (Function ([], Return getVal))
        "set", (Function ([value], ExprStatement (setVal (Var value))))
    ]

let getByref r =
    Application(ItemGet (r, Value (String "get")), [])   

let setByref r v =
    Application(ItemGet (r, Value (String "set")), [v])   

let getMember (x : FSharpMemberOrFunctionOrValue) : Member =
    let name = x.CompiledName

//    if name = "get_IsNone" then
//        let u = isUnionUsingNull x.EnclosingEntity
//        ()
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
            concreteType (getTypeDefinition x.LogicalEnclosingEntity, 
                List.init x.LogicalEnclosingEntity.GenericParameters.Count (fun i -> GenericType i)
            ) :: ps
        else ps

//    let getremap() =
//        let s = x.AbstractSlotSignature
//        let otparams = 
//            Seq.append s.ClassGenericParameters s.MethodGenericParameters
//            |> Seq.distinctBy (fun p -> p.Name)
//            |> Seq.mapi (fun i p -> p.Name, i) |> Map.ofSeq
//
//        x.
//
//        let m =
//            Seq.append tyPars mPars |> Seq.mapi (fun i t -> 
//                match getType otparams t with
//                | GenericType j -> Some (j, i)
//                | _ -> None
//            ) |> Seq.choose id |> Map.ofSeq
//        let fe (y: FSharpDelegateSignature) =
//            y.
//
//        let rec remap t =
//            match t with
//            | ConcreteType { Entity = td; Generics = ts } -> ConcreteType { Entity = td; Generics = List.map remap ts }
//            | GenericType i -> GenericType m.[i]
//            | ArrayType (t, r) -> ArrayType (remap t, r)
//            | TupleType ts -> TupleType (List.map remap ts)
//            | FSharpFuncType (a, r) -> FSharpFuncType (remap a, remap r)
//            | _ -> t
//        remap
////        ps |> List.map remap

    if name = ".ctor" then
        Member.Constructor <| Constructor {
            CtorParameters = getPars()
        }  
    else
//        let name =
//            match name.LastIndexOf '-' with
//            | -1 -> name
//            | i -> name.[i + 1 ..]
        if x.IsOverrideOrExplicitInterfaceImplementation then
            // TODO: multiple abstract slots implemented
            let s = x.ImplementedAbstractSignatures |> Seq.head

            let iTparams = 
                Seq.append s.DeclaringTypeGenericParameters s.MethodGenericParameters
                |> Seq.distinctBy (fun p -> p.Name)
                |> Seq.mapi (fun i p -> p.Name, i) |> Map.ofSeq
            
            let i = getTypeDefinition s.DeclaringType.TypeDefinition

            let meth =
                Method {
                    MethodName = s.Name
                    Parameters = s.AbstractArguments |> Seq.concat |> Seq.map (fun p -> getType iTparams p.Type) |> List.ofSeq |> removeUnitParam
                    ReturnType = getType iTparams s.AbstractReturnType
                    Generics   = s.MethodGenericParameters.Count
                } 
            if x.IsExplicitInterfaceImplementation then
                Member.Implementation(i, meth)    
            else
                Member.Override(i, meth)    

//            Some (getTypeDefinition iTyp.TypeDefinition), x.IsExplicitInterfaceImplementation, remap tyPars mPars                 
        else 
        
            Member.Method(
                isInstance && not compiledAsStatic,
                Method {
                    MethodName = name
                    Parameters = getPars()
                    ReturnType = getType tparams x.ReturnParameter.Type
        //                    try getType tparams x.ReturnParameter.Type
        //                    with _ -> 
        //                        // TODO: why
        //                        GenericType 0
                    Generics   = tparams.Count - x.EnclosingEntity.GenericParameters.Count
                } 
            )

//        match iTyp with
//        | None ->
//            Member.Method(isInstance && not compiledAsStatic, meth)
//        | Some iTyp ->
//            if isIntf then
//                Member.Implementation (iTyp, meth)
//            else
//                Member.Override (iTyp, meth) 

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

type Capturing(var) =
    inherit Transformer()

    let mutable captVal = None
    let mutable scope = 0

    override this.TransformId i =
        if scope > 0 && i = var then
            match captVal with
            | Some c -> c
            | _ ->
                let c = Id.New(?name = var.Name)
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

let getSourcePos (x: FSharpExpr) =
    let range = x.Range
    {   
        FileName = range.FileName
        Start = range.StartLine, range.StartColumn + 1
        End = range.EndLine, range.EndColumn + 1
    }

let withSourcePos (x: FSharpExpr) (expr: Expression) =
    ExprSourcePos (getSourcePos x, expr)

let rec transformExpression (env: Environment) (expr: FSharpExpr) =
    let inline tr x = transformExpression env x
    try
        match expr with
        | BasicPatterns.Value(var) ->
            if var.IsModuleValueOrMember then
                let td = getTypeDefinition var.EnclosingEntity
                match getMember var with
                | Member.Method (_, m) -> // TODO: instance methods represented as static
                    let ids = List.init var.CurriedParameterGroups.Count (fun _ -> Id.New())  
                    // TODO : generics
                    // TODO : this is probably wrong, also have to detuple within parameter groups
                    let body = Call (None, concrete (td, []), concrete (m, []), ids |> List.map Var)
                    let res = List.foldBack (fun i b -> Function ([i], Return b)) ids body
                    if var.IsMutable then getByref res else res
                // TODO : constructor as a value
                | _ -> failwith "Module member is not a method"
    //            Call(None, concrete (td, []), "get_" + var.CompiledName, [])                      // TODO setter
    //            FieldGet (None, concrete (td, []), var.CompiledName) 
    //            ItemGet (Value (String "TODO: module access"), Value (String var.CompiledName))
            elif var.IsMemberThisValue || var.IsConstructorThisValue then 
                This 
            else
                if isUnit var.FullType then
                    Undefined
                else
                    env.LookupVar var |> Var
        | BasicPatterns.Lambda(arg, body) ->
            let lArg =
                if isUnit arg.FullType then
                    []
                else
                    let i = Id.New(arg.DisplayName)  
                    env.AddVar(i, arg)
                    [i]
            Function(lArg, Return (body |> transformExpression (env.WithTParams(arg.GenericParameters |> Seq.map (fun p -> p.Name) |> List.ofSeq))))
        | BasicPatterns.Application(func, types, args) ->
            let args =
                match types with
                | [t] when isUnit t -> []
                | _ -> args
            match ignoreExprSourcePos (tr func) with
            | CallNeedingMoreArgs(thisObj, td, m, ca) ->
                Call(thisObj, td, m, ca @ (args |> List.map tr))
            | trFunc ->
                Seq.fold (fun f a -> Application(f, [tr a])) trFunc args
        | BasicPatterns.Let((id, value), body) ->
            let i = Id.New(id.DisplayName)
            env.AddVar(i, id)
            if id.IsMutable then
                Sequential [ NewVar(i, tr value); tr body ]
            else
                Let (i, tr value, tr body)
        | BasicPatterns.LetRec(defs, body) ->
            let ids = defs |> List.map (fun (id, _) ->
                let i = Id.New(id.DisplayName)
            
                env.AddVar(i, id)    
                i      
            )
            Sequential [ 
                for i in ids -> NewVar(i, Undefined)
                for i, (_, v) in Seq.zip ids defs -> VarSet(i, tr v)
                yield tr body
            ]

    //        LetRec (
    //            Seq.zip ids defs 
    //            |> Seq.map (fun (i, (_, value)) -> i, value |> transformExpression !env) |> List.ofSeq, 
    //            body |> transformExpression !env
    //        )
        | BasicPatterns.Call(this, meth, typeGenerics, methodGenerics, arguments) ->
            let td = getTypeDefinition meth.EnclosingEntity
            //let tparams = meth.GenericParameters |> Seq.mapi (fun i p -> p.Name, i) |> Map.ofSeq        
            if td.Value.FullName = "Microsoft.FSharp.Core.Operators" && meth.CompiledName = "Reraise" then
                StatementExpr (Throw (Var env.Exception.Value))    
            else
                let t = concrete (td, typeGenerics |> List.map (getType env.TParams))
                let args = List.map tr arguments
                let args =
                    match args with
                    | [ Undefined | Value Null ] -> []
                    | _ -> args
                match getMember meth with
                | Member.Method (isInstance, m) -> 
                    let mt = concrete (m, methodGenerics |> List.map (getType env.TParams))
    //                if not mt.Generics.IsEmpty && mt.Generics.Head.AssemblyQualifiedName.Contains "float`1" then
    //                    failwith "WTF"
    //                if m.Value.MethodName = "get_IsNone" then
    //                    ()
                    if isInstance then
                        Call (Option.map tr this, t, mt, args)
                    else 
                        if meth.IsInstanceMember && not meth.IsExtensionMember then
                            CallNeedingMoreArgs (None, t, mt, Option.toList (Option.map tr this) @ args)
                        else 
                            Call (None, t, mt, Option.toList (Option.map tr this) @ args)
                | Member.Implementation (i, m) ->
                    let t = concrete (i, typeGenerics |> List.map (getType env.TParams))
                    let mt = concrete (m, methodGenerics |> List.map (getType env.TParams))
                    Call (Option.map tr this, t, mt, args)
    //                CallInterface (tr this.Value, t, mt, args)            
                | Member.Override (_, m) ->
                    let mt = concrete (m, methodGenerics |> List.map (getType env.TParams))
                    Call (Option.map tr this, t, mt, args)
                | Member.Constructor c -> Ctor (t, c, args)
                | Member.StaticConstructor -> failwith "Invalid: direct call to static constructor" //CCtor t 
        | BasicPatterns.Sequential _ ->
            let rec getSeq acc expr =
                match expr with            
                | BasicPatterns.Sequential (f, s) ->
                    getSeq (f :: acc) s   
                | _ -> expr :: acc
            getSeq [] expr |> List.rev |> List.map tr |> Sequential
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
        | BasicPatterns.NewObject (ctor, typeGenerics, arguments) -> 
            let td = getTypeDefinition ctor.EnclosingEntity
    //        let tparams = constructor_.GenericParameters |> Seq.mapi (fun i p -> p.Name, i) |> Map.ofSeq
            let t = concrete (td, typeGenerics |> List.map (getType env.TParams))
            let args = List.map tr arguments
            match getMember ctor with
            | Member.Constructor c -> Ctor (t, c, args)
            | _ -> failwith "Expected a constructor call"
        | BasicPatterns.TryFinally (body, final) ->
            let res = Id.New ()
            Sequential [
                StatementExpr (TryFinally(ExprStatement(NewVar(res, tr body)), ExprStatement (tr final)))
                Var res
            ]
        | BasicPatterns.TryWith (body, var, filter, e, with_) ->
            let err = Id.New e.DisplayName
            let res = Id.New ()
            Sequential [
                StatementExpr (
                    TryWith(ExprStatement(NewVar(res, tr body)), 
                        Some err, 
                        (ExprStatement (transformExpression (env.WithException(err, e)) with_))))
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
            if var.IsModuleValueOrMember then
                let td = getTypeDefinition var.EnclosingEntity
                match getMember var with
                | Member.Method (_, m) -> // TODO: instance methods represented as static
                    let ids = List.init var.CurriedParameterGroups.Count (fun _ -> Id.New())  
                    // TODO : generics
                    // TODO : this is probably wrong, also have to detuple within parameter groups
                    let body = Call (None, concrete (td, []), concrete (m, []), ids |> List.map Var)
                    setByref (List.foldBack (fun i b -> Function ([i], Return b)) ids body) (tr value)
                | _ -> failwith "Module member is not a method"
            else
                VarSet(env.LookupVar(var), tr value) 
        | BasicPatterns.TupleGet (_, i, tuple) ->
            ItemGet(tr tuple, Value (Int i))   
        | BasicPatterns.FastIntegerForLoop (start, end_, body, up) ->
    //        let i = Id.New "i"
            let j = Id.New "j"
            let i, trBody =
                match ignoreExprSourcePos (tr body) with
                | Function ([i], Return b) -> i, b
                | _ -> failwith "Unexpected form of consumeExpr in FastIntegerForLoop pattern"     
            For (
                Some (Sequential [NewVar(i, tr start); NewVar (j, tr end_)]), 
                Some (if up then Binary(Var i, BinaryOperator.``<=``, Var j) else Binary(Var i, BinaryOperator.``>=``, Var j)), 
                Some (if up then MutatingUnary(MutatingUnaryOperator.``()++``, Var i)  else MutatingUnary(MutatingUnaryOperator.``()--``, Var i)), 
                ExprStatement (Capturing(i).CaptureValueIfNeeded(trBody))
            ) |> StatementExpr
        | BasicPatterns.TypeTest (typ, expr) ->
            TypeCheck (tr expr, getType Map.empty typ)
        | BasicPatterns.Coerce (typ, expr) ->
            tr expr // TODO: type check when possible
        | BasicPatterns.NewUnionCase (typ, case, exprs) ->
            let annot = attrReader.GetMemberAnnot(A.TypeAnnotation.Empty, case.Attributes) 
            match annot.Kind with
            | Some (A.MemberKind.Constant c) -> c
            | _ ->
            let i = typ.TypeDefinition.UnionCases |> Seq.findIndex (fun c -> c.CompiledName = case.CompiledName)
            let t =
                match getType env.TParams typ with
                | ConcreteType ct -> ct
                | _ -> failwith "Expected a union type"
            NewObject(
                t,
                Object (
                    ("$", Value (Int i)) ::
                    (exprs |> List.mapi (fun j e -> "$" + string j, tr e)) 
                )
            )
        | BasicPatterns.UnionCaseGet (expr, typ, case, field) ->
            let i = case.UnionCaseFields |> Seq.findIndex (fun f -> f = field)
            ItemGet(tr expr, Value (String ("$" + string i)))   
        | BasicPatterns.UnionCaseTest (expr, typ, case) ->
            let annot = attrReader.GetMemberAnnot(A.TypeAnnotation.Empty, case.Attributes) 
            match annot.Kind with
            | Some (A.MemberKind.Constant c) -> Binary (tr expr, BinaryOperator.``==``, c)
            | _ ->
            let i = typ.TypeDefinition.UnionCases |> Seq.findIndex (fun c -> c.CompiledName = case.CompiledName)
            Binary(ItemGet(tr expr, Value (String "$")), BinaryOperator.``==``, Value (Int i))
        | BasicPatterns.UnionCaseTag (expr, typ) ->
            ItemGet(tr expr, Value (String "$"))
        | BasicPatterns.NewRecord (typ, items) ->
            let t =
                match getType env.TParams typ with
                | ConcreteType ct -> ct
                | _ -> failwith "Expected a record type"
            NewObject (
                t,
                Seq.zip 
                    // TODO : optional fields
                    (typ.TypeDefinition.FSharpFields |> Seq.map (fun f -> f.Name))
                    (items |> Seq.map tr)
                |> List.ofSeq |> Object
            )
        | BasicPatterns.DecisionTree (matchValue, cases) ->
            let i = Id.New "matchIndex"
            let c = Id.New "matchCaptures"
    //        let value = tr matchValue
    //        let caseExprs = cases |> List.map (snd >> tr) 
            let r = Id.New "matchResult"
            let captures = cases |> Seq.collect fst |> List.ofSeq
            let mutable env = { env with MatchVars = Some (i, c) }
    //        let mutable captVars = []
    //        for v in captures do
    //            let i = Id.New v.LogicalName
    //            env <- env.WithVar(i, v)
    //            captVars <- i :: captVars
            let env = env
            let inline tr x = transformExpression env x
            Sequential [
    //            (captVars |> List.map (fun v -> NewVar(v, Undefined)))
                NewVar(i, Undefined)
                NewVar(c, Undefined)
                NewVar(r, Undefined)
                tr matchValue
                StatementExpr(
                    Switch(
                        Var i, 
                        cases |> List.mapi (fun j (ci, e) -> 
                            Some (Value (Int j)), 
                            Block [ 
                                let captIndex = ref 0
                                for cv in ci do
                                    let i = Id.New cv.DisplayName
                                    env.AddVar (i, cv)
                                    yield (VarDeclaration(i, ItemGet(Var c, Value (Int !captIndex))))
                                    incr captIndex
                                yield ExprStatement(VarSet(r, tr e)) 
                                yield Break None 
                            ]
                        )
                    )
                )
                Var r
            ]
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
            let t = 
                match getType env.TParams typ with
                | ConcreteType ct -> ct
                | _ -> failwith "Expected a record type"
    //        let t = concrete (getTypeDefinition typ.TypeDefinition, typ.GenericArguments |> Seq.map (getType env.TParams) |> List.ofSeq)
            FieldGet(thisOpt |> Option.map tr, t, field.Name)
    //        match thisOpt with
    //        | Some this ->
    //            ItemGet(tr this, Value (String field.Name))  // TODO : field renames 
    //        | _ -> failwith "TODO"
        | BasicPatterns.FSharpFieldSet (thisOpt, typ, field, value) ->
            let t = 
                match getType env.TParams typ with
                | ConcreteType ct -> ct
                | _ -> failwith "Expected a record type"
    //        let t = concrete (getTypeDefinition typ.TypeDefinition, typ.GenericArguments |> Seq.map (getType env.TParams) |> List.ofSeq)
            FieldSet(thisOpt |> Option.map tr, t, field.Name, tr value)
    //        match thisOpt with
    //        | Some this ->
    //            ItemSet(tr this, Value (String field.Name), tr value)  // TODO : field renames 
    //        | _ -> failwith "TODO"
        | BasicPatterns.AddressOf expr ->
            match ignoreExprSourcePos (tr expr) with
            | Var v as e ->
                makeByref e (fun value -> VarSet(v, value))
            | ItemGet(o, i) as e ->
                makeByref e (fun value -> ItemSet(o, i, value))
        | BasicPatterns.AddressSet (addr, value) ->
            Application(ItemGet(tr addr, Value (String "get")), [tr value])    
        | BasicPatterns.ObjectExpr (typ, expr, overrides, interfaces) ->
            failwith "TODO: object expressions"
        | BasicPatterns.DefaultValue typ ->
            Value Null
        | BasicPatterns.NewDelegate (typ, arg) ->
    //        NewArray [ tr arg ]    
            match ignoreExprSourcePos (tr arg) with
            // TODO: can arguments have useful info?
            | Application (func, _) -> func
            // TODO: find if this can happen
            | res -> res
        | BasicPatterns.TypeLambda          _ -> failwith "F# pattern not handled: TypeLambda"
        | BasicPatterns.Quote               _ -> failwith "F# pattern not handled: Quote"
        | BasicPatterns.BaseValue           _ -> failwith "F# pattern not handled: BaseValue"
        | BasicPatterns.ILAsm               _ -> failwith "F# pattern not handled: ILAsm"
        | BasicPatterns.ILFieldGet          _ -> failwith "F# pattern not handled: ILFieldGet"
        | BasicPatterns.ILFieldSet          _ -> failwith "F# pattern not handled: ILFieldSet"
        | BasicPatterns.TraitCall           _ -> failwith "F# pattern not handled: TraitCall"
        | BasicPatterns.UnionCaseSet _ ->
            failwith "UnionCaseSet pattern is only allowed in FSharp.Core"
        | _ -> failwith "F# expression not recognized"
    with e ->
        let msg =
            match e with
            | Failure m -> m
            | _ -> "Error while reading F# code: " + e.Message
        env.Compilation.AddError(Some (getSourcePos expr), Metadata.SourceError msg)
        WebSharper.Compiler.ToJavaScript.errorPlaceholder        
    |> withSourcePos expr
