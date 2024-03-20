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

module internal WebSharper.Compiler.CSharp.CodeReader

open Microsoft.CodeAnalysis
open Microsoft.CodeAnalysis.CSharp
open Microsoft.CodeAnalysis.CSharp.Syntax
 
open WebSharper.Core
open WebSharper.Core.AST
open WebSharper.Compiler.CompilationHelpers
open WebSharper.Compiler.Breaker

module A = WebSharper.Compiler.AttributeReader
            
open WebSharper.Compiler.CSharp.RoslynHelpers

open System.Collections.Generic

type CSharpParameter =
    {
        ParameterId : Id
        Symbol : IParameterSymbol
        DefaultValue : option<Expression>
        Type : Type
        RefOrOut : bool
    }

    static member New name =    
        {
            ParameterId = Id.New()
            Symbol = null
            DefaultValue = None
            Type = NonGenericType Definitions.Obj
            RefOrOut = false
        }

    static member New (name, typ) =    
        {
            ParameterId = Id.New(name)
            Symbol = null
            DefaultValue = None
            Type = typ
            RefOrOut = false
        }

module Seq =
    open System.Linq

    let inline equals (s1: _ seq) (s2: _ seq) = s1.SequenceEqual(s2)

type CSharpMethod =
    {
        IsStatic : bool
        Parameters : list<CSharpParameter>
        This : option<Id>
        Body : Statement
        IsAsync : bool
        ReturnType : Type
    }  

type CSharpConstructorInitializer =
    | ThisInitializer of Constructor * list<Expression> * (Expression -> Expression)
    | BaseInitializer of Concrete<TypeDefinition> * Constructor * list<Expression> * (Expression -> Expression)

type CSharpConstructor =
    {
        Parameters : list<CSharpParameter>
        Body : Statement
        Initializer : option<CSharpConstructorInitializer>
    }

type CSharpSwitchLabel =
    | DefaultLabel 
    | ConstantLabel of Expression
    | PatternLabel of (Id -> Expression)

type CSharpRecordDataMember =
    | CSharpRecordProperty of IPropertySymbol * Method
    | CSharpRecordField of IFieldSymbol * Type 

type CSharpRecord =
    {
        PositionalFields : list<CSharpParameter * Method>
        OtherFields : list<CSharpRecordDataMember>
        BaseCall : Concrete<TypeDefinition> * Constructor * list<Expression> * (Expression -> Expression)
    }

type RemoveBreaksTransformer() =
    inherit StatementTransformer()

    override this.TransformStatement s =
        match s with
        | Break None -> Empty
        | For _
        | ForIn _
        | While _
        | DoWhile _
        | Switch _ ->
            s
        | _ -> base.TransformStatement s

let removeBreaksTr = RemoveBreaksTransformer()

let textSpans =
    System.Runtime.CompilerServices.ConditionalWeakTable()

let mutable saveTextSpans = false

let getSourcePosOfSpan (ts: Text.TextSpan) (span: FileLinePositionSpan) =
    let res =
        {   
            FileName = span.Path
            Start = 
                let pos = span.StartLinePosition
                pos.Line + 1, pos.Character + 1
            End = 
                let pos = span.EndLinePosition
                pos.Line + 1, pos.Character + 1
        }
    if saveTextSpans then textSpans.Add(res, ref ts)
    res

let getSourcePos (x: CSharpSyntaxNode) =
    let ts = x.Span
    getSourcePosOfSpan ts (x.SyntaxTree.GetLineSpan ts)

let getSourcePosOfSyntaxReference (x: SyntaxReference) =
    let ts = x.Span
    getSourcePosOfSpan ts (x.SyntaxTree.GetLineSpan ts)

let withExprSourcePos (x: CSharpSyntaxNode) expr =
    ExprSourcePos (getSourcePos x, IgnoreExprSourcePos expr)

let withStatementSourcePos (x: CSharpSyntaxNode) statement =
    StatementSourcePos (getSourcePos x, IgnoreStatementSourcePos statement)

exception TransformError of obj * message: string with
    override this.Message = this.message    

let inline err node message = raise (TransformError (node, message)) 
let inline errf node x = Printf.kprintf (err node) x

let inline TODO x = 
    let xn = x.GetType().Name
    failwithf "Syntax currently not supported for client side: %s" (xn.[.. xn.Length - 5])  

let inline NotSupported x = 
    failwithf "Syntax not supported for client side: %s" x

module M = Metadata

let rec getTypedConstantValue (c: TypedConstant) =
    match c.Kind with
    | TypedConstantKind.Array ->
        c.Values |> Seq.map getTypedConstantValue |> Array.ofSeq |> box
    | _ -> c.Value 

let numericTypes =
    HashSet [
        "System.Byte" 
        "System.SByte"
        "System.Int16"
        "System.Int32"
        "System.UInt16" 
        "System.UInt32" 
        "System.Single" 
        "System.Double" 
    ]

let setterOf (getter : Concrete<Method>) =
    let m = getter.Entity.Value
    if not (m.MethodName.StartsWith "get_") then
        failwithf "Invalid getter method: %s" m.MethodName
    { getter with
        Entity = 
            Method {
                MethodName = "set" + m.MethodName.[3 ..]
                ReturnType = VoidType
                Parameters = m.Parameters @ [ m.ReturnType ]
                Generics = m.Generics
            }
    }        

let dynTyp = ConcreteType (NonGeneric Definitions.Dynamic)     

type SymbolReader(comp : WebSharper.Compiler.Compilation) as self =
    let getTypeFullName (t: ITypeSymbol) =
        let rec getNamespaceOrTypeAddress acc (symbol: INamespaceOrTypeSymbol) =
            match symbol.ContainingNamespace with
            | null -> acc |> String.concat "."
            | ns -> getNamespaceOrTypeAddress (symbol.MetadataName :: acc) ns   

        let rec getTypeAddress acc (symbol: ITypeSymbol) =
            match symbol.ContainingType with
            | null -> 
                let ns = getNamespaceOrTypeAddress [] symbol
                if List.isEmpty acc then ns else
                    ns :: acc |> String.concat "+" 
            | t -> getTypeAddress (symbol.MetadataName :: acc) t
        getTypeAddress [] t

    let getContainingAssemblyName (t: ITypeSymbol) =
        match t.ContainingAssembly with
        | null -> comp.AssemblyName
        | a ->
            match getTypeFullName t with
            | "" -> a.Name
            | t ->
                match AssemblyConventions.StandardAssemblyNameForTypeNamed t with
                | Some n -> n
                | None -> a.Name

    let getMeth (x: IMethodSymbol) =
        Hashed {
            MethodName = x.Name
            Parameters = self.ReadParameterTypes x
            ReturnType = x.ReturnType |> self.ReadType
            Generics = x.Arity
        }

    let attrReader =
        { new A.AttributeReader<Microsoft.CodeAnalysis.AttributeData>() with
            override this.GetAssemblyName attr = attr.AttributeClass |> getContainingAssemblyName
            override this.GetName attr = attr.AttributeClass.Name
            override this.GetCtorArgs attr = attr.ConstructorArguments |> Seq.map getTypedConstantValue |> Array.ofSeq          
            override this.GetNamedArgs attr = attr.NamedArguments |> Seq.map (fun (KeyValue(n, v)) -> n, getTypedConstantValue v) |> Array.ofSeq
            override this.GetTypeDef o = self.ReadNamedTypeDefinition (o :?> INamedTypeSymbol)
            override this.GetAttributeRedirections attr = attr.AttributeClass.GetAttributes() |> Array.ofSeq
        }

    member this.RegisterCustomType def (td: INamedTypeSymbol) =
        if not (comp.HasCustomTypeInfo def) then
            let inv = td.DelegateInvokeMethod
            if not (isNull inv) then
                let info =
                    M.DelegateInfo {
                        DelegateArgs =                                
                            inv.Parameters |> Seq.map (fun p -> 
                                this.ReadType p.Type, 
                                if p.IsOptional then Some (ReadLiteral p.ExplicitDefaultValue) else None
                            ) |> List.ofSeq
                        ReturnType = this.ReadType inv.ReturnType
                    }
                comp.AddCustomType(def, info)

    member this.ReadNamedTypeDefinition (x: INamedTypeSymbol) =
        let rec getNamespaceOrTypeAddress acc (symbol: INamespaceOrTypeSymbol) =
            match symbol.ContainingNamespace with
            | null -> acc |> String.concat "."
            | ns -> getNamespaceOrTypeAddress (symbol.MetadataName :: acc) ns   

        let rec getTypeAddress acc (symbol: INamedTypeSymbol) =
            match symbol.ContainingType with
            | null -> 
                let ns = getNamespaceOrTypeAddress [] symbol
                if List.isEmpty acc then ns else
                    ns :: acc |> String.concat "+" 
            | t -> getTypeAddress (symbol.MetadataName :: acc) t           

        let res =
            Hashed {
                Assembly = comp.FindProxiedAssembly(getContainingAssemblyName x)
                FullName = getTypeFullName x
            }

        this.RegisterCustomType res x

        res

    member this.ReadNamedType (x: INamedTypeSymbol) = 
        if isNull x then
            // TODO: handle dynamic by cheking symbol.ContainingSymbol before calling sr.ReadNamedType
            NonGeneric Definitions.Dynamic     
        else          
            //let x = if x.IsTupleType then x.TupleUnderlyingType else x
            let ta = this.GetTypeArguments x
            let td = this.ReadNamedTypeDefinition x
            Generic td ta
    
    member this.GetTypeArguments (x: INamedTypeSymbol) =
        let rec get (x: INamedTypeSymbol) =
            match x.ContainingType with
            | null -> x.TypeArguments |> Seq.map this.ReadType
            | ct -> Seq.append (get ct) (x.TypeArguments |> Seq.map this.ReadType)
        get x |> List.ofSeq

    member this.RecognizeNamedType (x: INamedTypeSymbol) =
        if x.IsTupleType then
            TupleType (x.TupleElements |> Seq.map (fun f -> this.ReadType f.Type) |> List.ofSeq, true)  
        else
        let ta = this.GetTypeArguments x
        let td = this.ReadNamedTypeDefinition x
        let tName = td.Value.FullName
        if tName.StartsWith "System.Tuple" then
            if tName.EndsWith "8" then
                match ta.[7] with
                | TupleType (rest, _) -> TupleType (ta.[.. 6] @ rest, false)
                | _ -> failwith "invalid big tuple type" 
            else TupleType (ta, false)
        elif tName.StartsWith "System.ValueTuple" then
            let te = x.TupleElements |> Seq.map (fun f -> this.ReadType f.Type) |> List.ofSeq
            if tName.EndsWith "8" then
                match te.[7] with
                | TupleType (rest, _) -> TupleType (te.[.. 6] @ rest, true)
                | _ -> failwith "invalid big tuple type" 
            else TupleType (te, true)
        elif tName = "Microsoft.FSharp.Core.FSharpFunc`2" then
            match ta with
            | [a; r] -> FSharpFuncType(a, r)
            | _ -> failwith "impossible"
        elif tName = "Microsoft.FSharp.Core.Unit" || tName = "System.Void" then
            VoidType
        else
            GenericType td ta

    member this.ReadType (x: ITypeSymbol) : Type =
        if isNull x then VoidType else
        match x.TypeKind with
        | TypeKind.Array ->
            let t = x :?> IArrayTypeSymbol
            ArrayType (this.ReadType t.ElementType, t.Rank)
        | TypeKind.Class
        | TypeKind.Struct
        | TypeKind.Error
        | TypeKind.Enum
        | TypeKind.Delegate
        | TypeKind.Interface ->
            let t = x :?> INamedTypeSymbol 
            this.RecognizeNamedType t
        | TypeKind.Dynamic ->
            dynTyp
        | TypeKind.TypeParameter ->
            let t = x :?> ITypeParameterSymbol 
            let t = match t.ReducedFrom with null -> t | t -> t
            match t.TypeParameterKind with
            | TypeParameterKind.Method ->
                TypeParameter (t.Ordinal + t.DeclaringMethod.ContainingType.TypeParameters.Length)
            | TypeParameterKind.Type ->
                TypeParameter t.Ordinal
            | _ -> errf x "Unexpected TypeParameterKind"
        | _ ->
            errf x "transformType: typekind %O not suppported" x.TypeKind

    member this.ReadAsyncReturnKind (x: IMethodSymbol) =
        if x.ReturnsVoid then
            Continuation.ReturnsVoid
        elif (x.ReturnType :?> INamedTypeSymbol).IsGenericType then
            Continuation.ReturnsResultTask
        else Continuation.ReturnsTask

    member this.ReadMethod (x: IMethodSymbol) =
        let x  = x.OriginalDefinition
        Hashed {
            MethodName = x.Name
            Parameters = this.ReadParameterTypes x
            ReturnType = x.ReturnType |> this.ReadType
            Generics = x.Arity
        }
        |> comp.ResolveProxySignature

    member this.ReadGenericMethod (x: IMethodSymbol) =
        let ma = x.TypeArguments |> Seq.map this.ReadType |> List.ofSeq
        Generic (this.ReadMethod x) ma

    member this.ReadConstructor (x: IMethodSymbol) =
        let x  = x.OriginalDefinition
        Hashed {
            CtorParameters = this.ReadParameterTypes x
        }
        |> comp.ResolveProxySignature

    member this.ReadMember (x: IMethodSymbol) =
        let name = x.Name
        match name with
        | ".ctor" ->
            Member.Constructor <| Hashed {
                CtorParameters = this.ReadParameterTypes x
            }
        | ".cctor" -> Member.StaticConstructor
        | _ ->
            let getMeth (x: IMethodSymbol) =
                Hashed {
                    MethodName = x.Name
                    Parameters = this.ReadParameterTypes x
                    ReturnType = x.ReturnType |> this.ReadType
                    Generics = x.Arity
                }
            if x.IsOverride then
                let rec getOrig (x: IMethodSymbol) =
                    let o = x.OverriddenMethod.OriginalDefinition
                    if o.IsOverride && not (isNull o.OverriddenMethod) then
                        getOrig o
                    else
                        o
                let o = getOrig x
                Member.Override(this.ReadNamedTypeDefinition o.ContainingType, getMeth o)
            elif x.IsVirtual then
                let o = x.OriginalDefinition                                        
                Member.Override(this.ReadNamedTypeDefinition o.ContainingType, getMeth o)
            elif x.ExplicitInterfaceImplementations.Length > 0 then
                let o = x.ExplicitInterfaceImplementations.[0].OriginalDefinition
                Member.Implementation(this.ReadNamedTypeDefinition o.ContainingType, getMeth o)
            else
                let o = x.OriginalDefinition
                Member.Method (not o.IsStatic, getMeth o)
        |> comp.ResolveProxySignature

    member this.ReadMemberImpl (x: IMethodSymbol) =
        getMeth x.OriginalDefinition    

    member this.ReadParameter (x: IParameterSymbol) : CSharpParameter =
        let typ = this.ReadType x.Type
        let defValue = 
            if x.HasExplicitDefaultValue then 
                Some (ReadLiteral x.ExplicitDefaultValue |> Value) 
            elif x.IsOptional then
                Some (DefaultValueOf typ)
            elif x.IsParams then
                Some (NewArray [])
            else None

        let id = Id.New(x.Name, opt = Option.isSome defValue)
        {
            ParameterId = id
            Symbol = x
            DefaultValue = defValue
            Type = typ
            RefOrOut = x.RefKind <> RefKind.None
        }

    member this.ReadParameters (x: IMethodSymbol) =
        x.Parameters |> Seq.map this.ReadParameter |> List.ofSeq   

    member this.ReadParameterType (x: IParameterSymbol) =
        if x.RefKind = RefKind.None then
            this.ReadType x.Type 
        else ByRefType (this.ReadType x.Type)   

    member this.ReadParameterTypes (x: IMethodSymbol) =
        x.Parameters |> Seq.map this.ReadParameterType |> List.ofSeq   
  
    member this.AttributeReader = attrReader

type Environment =
    {
        SemanticModel : SemanticModel 
        Vars : IDictionary<ILocalSymbol, Id>
        This : option<Id>
        Parameters : IDictionary<IParameterSymbol, Id * bool>
        Labels : IDictionary<ILabelSymbol, Id>
        LocalFunctions : IDictionary<IMethodSymbol, Id>
        Caught : option<Id>
        Conditional : option<Id>
        Initializing : option<Id>
        Compilation : WebSharper.Compiler.Compilation
        SymbolReader : SymbolReader
        RangeVars : IDictionary<IRangeVariableSymbol, Id * option<int>>
    }
    static member New(model, comp, sr, thisVar) = 
        { 
            SemanticModel = model
            Vars = Dictionary()
            This = thisVar
            Parameters = Dictionary()
            Labels = Dictionary()
            LocalFunctions = Dictionary()
            Caught = None
            Conditional = None
            Initializing = None
            Compilation = comp
            SymbolReader = sr
            RangeVars = Dictionary()
        }      

    member this.GetLabelId(symbol) =
        match this.Labels.TryGetValue(symbol) with
        | true, id -> id
        | _ ->
            let id = Id.New(symbol.Name)           
            this.Labels.Add(symbol, id)
            id

    member this.GetLocalFunctionId(symbol) =
        match this.LocalFunctions.TryGetValue(symbol) with
        | true, id -> id
        | _ ->
            let id = Id.New(symbol.Name, mut = false)           
            this.LocalFunctions.Add(symbol, id)
            id

    member this.WithCaught(c) =
        { this with Caught = Some c }

    member this.WithConditional(c) =
        { this with Conditional = Some c }

    member this.WithInitializing(i) =
        { this with Initializing = Some i }

    member this.WithNotInitializing() =
        { this with Initializing = None }

module Definitions =
    let Decimal =
        TypeDefinition {
            Assembly = "netstandard"
            FullName = "System.Decimal"    
        }

    let DecimalCtorIntArray =
        Constructor {
            CtorParameters = [ ArrayType(NonGenericType Definitions.Int, 1) ]
        }

    let DefaultRecordBaseCall =
        NonGeneric Definitions.Obj,
        ConstructorInfo.Default(),
        ([] : list<Expression>),
        (id : Expression -> Expression)

type RoslynTransformer(env: Environment) = 
    let getConstantValueOfExpression x =
        let l =
            env.SemanticModel.GetConstantValue(x).Value
            |> ReadLiteral 
        match l with
        | Decimal x ->
            Ctor(NonGeneric Definitions.Decimal, Definitions.DecimalCtorIntArray,
                (System.Decimal.GetBits(x) |> Seq.map (Int >> Value) |> List.ofSeq))
        | _ -> Value l

    let sr = env.SymbolReader

    let fixNonTrailingNamedArguments (argumentList : list<int option * Expression>) =
        let isNotNamed (iOpt, _) = Option.isNone iOpt 
        let lastNonNamedIndex = 
            argumentList |> List.tryFindIndexBack isNotNamed
        match lastNonNamedIndex with
        | None -> argumentList 
        | Some l -> 
            if argumentList |> List.forall isNotNamed then
                argumentList
            else
                argumentList |> List.mapi (fun i a ->
                    if i < l then None, snd a else a
                )
    
    let fixParamArray (symbol: IMethodSymbol) (args: ArgumentListData) argumentList  =
        let ps = symbol.Parameters
        let paramCount = ps.Length
        let fixSingleArg pa =
            let rec arrayDepth (t: ITypeSymbol) =
                if t.TypeKind = TypeKind.Array then
                    let t = t :?> IArrayTypeSymbol
                    1 + arrayDepth t.ElementType
                else 0                
            let paTy = 
                env.SemanticModel.GetTypeInfo((args.Arguments |> Seq.last).Expression.Node).Type
            let paTyArrayDepth =
                if paTy = null then 0 else arrayDepth paTy
            if arrayDepth (ps.[paramCount - 1].Type) = paTyArrayDepth then pa else NewArray [ pa ] 

        if paramCount > 0 && ps.[paramCount - 1].IsParams then                
            if List.length argumentList >= paramCount - 1 
                && argumentList |> Seq.forall (fst >> Option.isNone) then
                let normal, toArr = argumentList |> List.map snd |> List.splitAt (paramCount - 1)                
                match toArr with
                | [ pa ] -> normal @ [ fixSingleArg pa ]
                | _ ->
                    normal @ [ NewArray toArr ]  
                |> List.map (fun a -> None, a)  
            else
            let psInd = Some (paramCount - 1)
            if argumentList |> Seq.map fst |> Seq.contains psInd then
                argumentList |> List.map (fun (i, v) ->
                    i, if i = psInd then fixSingleArg v else v 
                )
            else argumentList
        else argumentList

    let readReorderedParams args =
        if args |> List.forall (fst >> Option.isNone) then [], args |> List.map snd else
        let needsParamReorder =
            args |> List.fold (fun bo (i, _) ->
                bo |> Option.bind (fun b -> 
                    match i with
                    | None -> Some 0
                    | Some i -> if i < b then None else Some i    
                )   
            ) (Some 0)
            |> Option.isNone
        let lastIndex = args |> Seq.choose fst |> Seq.max 
        let argsWithIndexes =
            args |> List.mapi (fun i (j, e) -> i, defaultArg j i, e, Id.New())
        if needsParamReorder then
            let byOrdinal = argsWithIndexes |> Seq.map (fun (_, i, _, v) -> i, v) |> Map.ofSeq
            let inOrder = argsWithIndexes |> List.sortBy (fun (_, i, _, _) -> i) |> List.map (fun (_, _, e, v) -> v, e)
            let fargs = 
                List.init (lastIndex + 1) (fun i ->
                    match byOrdinal |> Map.tryFind i with
                    | Some v -> Var v
                    | _ -> Undefined
                ) 
            inOrder, fargs
        else 
            let byOrdinal = argsWithIndexes |> Seq.map (fun (_, i, e, _) -> i, e) |> Map.ofSeq
            let fargs =
                List.init (lastIndex + 1) (fun i ->
                    match byOrdinal |> Map.tryFind i with
                    | Some e -> e
                    | _ -> Undefined
                ) 
            [], fargs
        
    let createRef (e: Expression) =
        match IgnoreExprSourcePos e with
        | Var v ->
            MakeRef e (fun value -> VarSet(v, value)) v.VarType
        | NewVar (v, Undefined) -> // out var
            Sequential [ e; MakeRef e (fun value -> VarSet(v, value)) v.VarType ]
        | ItemGet(o, i, _) ->
            let ov = Id.New ()
            let iv = Id.New ()
            Let (ov, o, Let(iv, i, MakeRef (ItemGet(Var ov, Var iv, NoSideEffect)) (fun value -> ItemSet(Var ov, Var iv, value)) None))
        | FieldGet(o, t, f) ->
            match o with
            | Some o ->
                let ov = Id.New ()
                Let (ov, o, MakeRef (FieldGet(Some (Var ov), t, f)) (fun value -> FieldSet(Some (Var ov), t, f, value)) (Some (ConcreteType t)))     
            | _ ->
                MakeRef e (fun value -> FieldSet(None, t, f, value)) (Some (ConcreteType t)) 
        | Application(ItemGet (r, Value (String "get"), _), [], _) ->
            r
        | Call (thisOpt, typ, getter, args) ->
            MakeRef e (fun value -> (Call (thisOpt, typ, setterOf getter, args @ [value]))) (Some getter.Entity.Value.ReturnType)
        | e -> failwithf "ref argument or expression has unexpected form: %s" (Debug.PrintExpression e)     

    let setterOf (getter : Concrete<Method>) =
        let m = getter.Entity.Value
        if not (m.MethodName.StartsWith "get_") then
            failwithf "Invalid getter method: %s" m.MethodName
        { getter with
            Entity = 
                Method {
                    MethodName = "set" + m.MethodName.[3 ..]
                    ReturnType = VoidType
                    Parameters = m.Parameters @ [ m.ReturnType ]
                    Generics = m.Generics
                }
        }                

    let jsConcat expr args =
        Appl(ItemGet(expr, Value (String "concat"), Pure), args, Pure, None)

    let queryCall (symbol: IMethodSymbol) args =
        let qtyp = sr.ReadNamedType symbol.ContainingType
        let ma = symbol.TypeArguments |> Seq.map (sr.ReadType) |> List.ofSeq
        let meth = Generic (sr.ReadMethod symbol.ReducedFrom) ma
        Call (None, qtyp, meth, args)

    let getTypeAndMethod (symbol: IMethodSymbol) =
        let symbol =
            match symbol.OverriddenMethod with
            | null -> symbol
            | symbol -> symbol
        let eSymbol =
            match symbol.ReducedFrom with
            | null -> symbol
            | symbol -> symbol
        let typ = sr.ReadNamedType eSymbol.ContainingType
        let ma = 
            (symbol.TypeArguments, eSymbol.TypeParameters) 
            ||> Seq.map2 (fun a p -> if a.Equals p then dynTyp else sr.ReadType a) 
            |> List.ofSeq
        let meth = Generic (sr.ReadMethod eSymbol) ma
        typ, meth

    let call (symbol: IMethodSymbol) thisOpt args =
        let typ, meth = getTypeAndMethod symbol
        Call(thisOpt, typ, meth, args)

    let trLocalId (s: ILocalSymbol) = 
        Id.New(s.Name, not s.IsConst, typ = sr.ReadType(s.Type))

    member this.TransformIdentifierName (x: IdentifierNameData) : Expression =
        let symbol = env.SemanticModel.GetSymbolInfo(x.Node).Symbol
        let getTarget() =
            if symbol.IsStatic then None else
            match env.Initializing with
            | Some i -> Var i
            | _ -> Var env.This.Value
            |> Some
        match symbol with
        | :? ILocalSymbol as s -> 
            if s.IsRef then
                GetRef (Var env.Vars.[s])     
            else
                Var env.Vars.[s]
        | :? IParameterSymbol as p -> 
            match env.Parameters.[p] with
            | v, false -> Var v
            | v, true -> GetRef (Var v) 
        | :? IRangeVariableSymbol as v ->
            match env.RangeVars.[v] with
            | v, None -> Var v
            | v, Some i -> ItemGet(Var v, Value (Int i), NoSideEffect)
        | :? IFieldSymbol as f -> FieldGet((getTarget()), sr.ReadNamedType f.ContainingType, f.Name) 
        | :? IEventSymbol as e -> FieldGet((getTarget()), sr.ReadNamedType e.ContainingType, e.Name)
        | :? IPropertySymbol as p ->
            call p.GetMethod (getTarget()) [] // TODO: indexed properties?
        | :? IMethodSymbol as m -> 
            let conv = env.SemanticModel.GetConversion(x.Node)
            if not conv.Exists || conv.IsIdentity then Var env.This.Value
            elif conv.IsMethodGroup then
                let typ, meth = getTypeAndMethod m
                NewDelegate(Some (Var env.This.Value), typ, meth)
            else failwithf "TransformIdentifierName: unhandled IMethodSymbol conversion: %A" conv 
        | :? IDiscardSymbol ->
            Undefined
        | null -> 
            err x.Node (sprintf "TransformIdentifierName: Symbol is null for identifier %s" (x.Node.ToFullString()))
        | _ -> 
            err x.Node (sprintf "TransformIdentifierName: Local variable not found, symbol type: %s" 
                (symbol.GetType().FullName))
                                                                                                                                                                                                                                                                                                                                                                                 
    member this.TransformExpression (x: ExpressionData) : Expression =
        try
            let expr =
                match x with
                | ExpressionData.Type                              x -> this.TransformType x
                | ExpressionData.InstanceExpression                x -> this.TransformInstanceExpression x
                | ExpressionData.AnonymousFunctionExpression       x -> this.TransformAnonymousFunctionExpression x
                | ExpressionData.ParenthesizedExpression           x -> this.TransformParenthesizedExpression x
                | ExpressionData.TupleExpression                   x -> this.TransformTupleExpression x
                | ExpressionData.PrefixUnaryExpression             x -> this.TransformPrefixUnaryExpression x
                | ExpressionData.AwaitExpression                   x -> this.TransformAwaitExpression x
                | ExpressionData.PostfixUnaryExpression            x -> this.TransformPostfixUnaryExpression x
                | ExpressionData.MemberAccessExpression            x -> this.TransformMemberAccessExpression x
                | ExpressionData.ConditionalAccessExpression       x -> this.TransformConditionalAccessExpression x
                | ExpressionData.MemberBindingExpression           x -> this.TransformMemberBindingExpression x
                | ExpressionData.ElementBindingExpression          x -> this.TransformElementBindingExpression x
                | ExpressionData.ImplicitElementAccess             x -> this.TransformImplicitElementAccess x
                | ExpressionData.BinaryExpression                  x -> this.TransformBinaryExpression x
                | ExpressionData.AssignmentExpression              x -> this.TransformAssignmentExpression x
                | ExpressionData.ConditionalExpression             x -> this.TransformConditionalExpression x
                | ExpressionData.LiteralExpression                 x -> this.TransformLiteralExpression x
                | ExpressionData.MakeRefExpression                 _ -> NotSupported "__makeref"
                | ExpressionData.RefTypeExpression                 _ -> NotSupported "__reftype"
                | ExpressionData.RefValueExpression                _ -> NotSupported "__refvalue"
                | ExpressionData.CheckedExpression                 x -> this.TransformCheckedExpression x
                | ExpressionData.DefaultExpression                 x -> this.TransformDefaultExpression x
                | ExpressionData.TypeOfExpression                  _ -> NotSupported "typeof"
                | ExpressionData.SizeOfExpression                  _ -> NotSupported "sizeof"
                | ExpressionData.InvocationExpression              x -> this.TransformInvocationExpression x
                | ExpressionData.ElementAccessExpression           x -> this.TransformElementAccessExpression x
                | ExpressionData.DeclarationExpression             x -> this.TransformDeclarationExpression x
                | ExpressionData.CastExpression                    x -> this.TransformCastExpression x
                | ExpressionData.RefExpression                     x -> this.TransformRefExpression x
                | ExpressionData.InitializerExpression             x -> this.TransformInitializerExpression x
                | ExpressionData.BaseObjectCreationExpression      x -> this.TransformBaseObjectCreationExpression x
                | ExpressionData.AnonymousObjectCreationExpression x -> this.TransformAnonymousObjectCreationExpression x
                | ExpressionData.ArrayCreationExpression           x -> this.TransformArrayCreationExpression x
                | ExpressionData.ImplicitArrayCreationExpression   x -> this.TransformImplicitArrayCreationExpression x
                | ExpressionData.ImplicitStackAllocArrayCreationExpression _
                | ExpressionData.StackAllocArrayCreationExpression _ -> NotSupported "stackalloc"
                | ExpressionData.QueryExpression                   x -> this.TransformQueryExpression x
                | ExpressionData.OmittedArraySizeExpression        _ -> failwith "not a general expression: OmittedArraySizeExpression"
                | ExpressionData.InterpolatedStringExpression      x -> this.TransformInterpolatedStringExpression x      
                | ExpressionData.IsPatternExpression               x -> this.TransformIsPatternExpression x
                | ExpressionData.ThrowExpression                   x -> this.TransformThrowExpression x      
                | ExpressionData.RangeExpression                   x -> this.TransformRangeExpression x      
                | ExpressionData.SwitchExpression                  x -> this.TransformSwitchExpression x      
                | ExpressionData.WithExpression                    x -> this.TransformWithExpression x      
            let conversion = env.SemanticModel.GetConversion(x.Node)
            if conversion.IsUserDefined then
                let symbol = conversion.MethodSymbol
                let typ = sr.ReadNamedType symbol.ContainingType
                let ma = symbol.TypeArguments |> Seq.map (sr.ReadType) |> List.ofSeq
                let meth = Generic (sr.ReadMethod symbol) ma
                Call(None, typ, meth, [ expr ])
            elif conversion.IsNumeric then
                let typInfo = env.SemanticModel.GetTypeInfo(x.Node)
                let fromTyp = typInfo.Type :?> INamedTypeSymbol |> sr.ReadNamedTypeDefinition
                let toTyp = typInfo.ConvertedType :?> INamedTypeSymbol |>  sr.ReadNamedTypeDefinition
                //let fromTyp = sr.ReadNamedTypeDefinition (conversion.MethodSymbol.Parameters.[0].Type :?> INamedTypeSymbol)
                //let toTyp = sr.ReadNamedTypeDefinition (conversion.MethodSymbol.ReturnType :?> INamedTypeSymbol)
                NumericConversion fromTyp toTyp expr
            //elif conversion.IsImplicit then
            //    let typInfo = env.SemanticModel.GetTypeInfo(x.Node)
            //    let fromTyp = typInfo.Type |> sr.ReadType
            //    let toTyp = typInfo.ConvertedType |> sr.ReadType
            //    Coerce(expr, fromTyp, toTyp)
            else expr
        with e ->
            env.Compilation.AddError(Some (getSourcePos x.Node), WebSharper.Compiler.SourceError("Error while reading C# code: " + e.Message + " at " + e.StackTrace))
            errorPlaceholder       

    member this.TransformType (x: TypeData) : _ =
        match x with
        | TypeData.Name                x -> this.TransformName x
        | TypeData.PredefinedType      x -> TODO x //this.TransformPredefinedType x
        | TypeData.ArrayType           x -> TODO x //this.TransformArrayType x
        | TypeData.PointerType         x -> TODO x //this.TransformPointerType x
        | TypeData.FunctionPointerType x -> TODO x //this.TransformFunctionPointerType x
        | TypeData.NullableType        x -> TODO x //this.TransformNullableType x
        | TypeData.TupleType           x -> TODO x //this.TransformTupleType x
        | TypeData.OmittedTypeArgument x -> TODO x //this.TransformOmittedTypeArgument x
        | TypeData.RefType             x -> TODO x //this.TransformRefType x

    member this.TransformName (x: NameData) : Expression =
        match x with
        | NameData.SimpleName         x -> this.TransformSimpleName x
        | NameData.QualifiedName      x -> TODO x //this.TransformQualifiedName x
        | NameData.AliasQualifiedName x -> TODO x //this.TransformAliasQualifiedName x

    member this.TransformSimpleName (x: SimpleNameData) : Expression =
        match x with
        | SimpleNameData.IdentifierName x -> this.TransformIdentifierName x |> withExprSourcePos x.Node
        | SimpleNameData.GenericName    x -> TODO x //this.TransformGenericName x

    member this.TransformInvocationExpression (x: InvocationExpressionData) : Expression =
        let symbol = env.SemanticModel.GetSymbolInfo(x.Node).Symbol :?> IMethodSymbol
        if isNull symbol then
            // for dynamic
            Appl(x.Expression |> this.TransformExpression, x.ArgumentList |> this.TransformArgumentList |> List.map snd, NonPure, None)
        else
        let eSymbol, isExtensionMethod =
            match symbol.ReducedFrom with
            | null -> symbol, false
            | symbol -> symbol, true
        let typ, meth = getTypeAndMethod symbol
        let argumentList = x.ArgumentList |> this.TransformArgumentList
        let argumentListWithParamsFix() = fixParamArray eSymbol x.ArgumentList argumentList
        let argumentListWithThis =
            if symbol.MethodKind = MethodKind.LocalFunction then
                argumentListWithParamsFix()    
            elif isExtensionMethod then
                fixParamArray eSymbol x.ArgumentList ((None, (x.Expression |> this.TransformExpression)) :: argumentList)
                |> List.map (fun (i, e) -> i |> Option.map ((+) 1), e)    
            elif not symbol.IsStatic then
                (None, (x.Expression |> this.TransformExpression)) :: (argumentListWithParamsFix() 
                |> List.map (fun (i, e) -> i |> Option.map ((+) 1), e))   
            else argumentListWithParamsFix() 

        let tempVars, args = readReorderedParams argumentListWithThis

        if symbol.MethodKind = MethodKind.LocalFunction then
            let f = env.GetLocalFunctionId(symbol)
            Appl(Var f, args, NonPure, Some args.Length)
        elif isExtensionMethod || symbol.IsStatic then
            Call(None, typ, meth, args)
        else        
            Call(Some (List.head args), typ, meth, List.tail args)
        |> List.foldBack (fun (v, e) b -> Let (v, e, b)) tempVars
        |> withExprSourcePos x.Node

    member this.TransformElementAccess (node: ExpressionSyntax, expr, args) =
        // TODO : static indexer?
        let expression = 
            match expr with
            | Some e -> this.TransformExpression e
            | None ->
                match env.Initializing with
                | Some i -> Var i
                | None -> Var env.Conditional.Value
        let argumentList = args |> this.TransformBracketedArgumentList
        let symbol = env.SemanticModel.GetSymbolInfo(node).Symbol :?> IPropertySymbol
        // TODO: investigate that symbol is null only when it is an array
        if symbol = null then
            // TODO: make setter possible for 2-dim arrays
            List.fold (fun e a -> ItemGet(e, a, NoSideEffect)) expression argumentList
        else
            call symbol.GetMethod (Some expression) argumentList

    member this.TransformElementAccessExpression (x: ElementAccessExpressionData) : _ =
        this.TransformElementAccess(x.Node, Some x.Expression, x.ArgumentList)

    member this.TransformImplicitElementAccess (x: ImplicitElementAccessData) : _ =
        this.TransformElementAccess(x.Node, None, x.ArgumentList)

    member this.TransformArgument (x: ArgumentData) : option<int> * Expression =
        let namedParamOrdinal = 
            x.NameColon |> Option.bind (fun nc ->
                match env.SemanticModel.GetSymbolInfo(nc.Name.Node).Symbol with
                | :? IParameterSymbol as symbol -> Some symbol.Ordinal
                | _ -> None
            )
        let expression = x.Expression |> this.TransformExpression
        let value =
            match x.RefKindKeyword with
            | Some ArgumentRefKindKeyword.RefKeyword 
            | Some ArgumentRefKindKeyword.OutKeyword
            | Some ArgumentRefKindKeyword.InKeyword ->
                createRef expression
            | None ->
                // TODO: copy struct values, if support for mutable structs on the client-side is added
                //if env.SemanticModel.GetTypeInfo(x.Node).Type.IsValueType then ...
                expression
        namedParamOrdinal, value

    member this.TransformArgumentList (x: ArgumentListData) =
        x.Arguments |> Seq.map this.TransformArgument |> List.ofSeq |> fixNonTrailingNamedArguments

    member this.TransformBracketedArgumentList (x: BracketedArgumentListData) =
        x.Arguments |> Seq.map this.TransformArgument |> Seq.map snd |> List.ofSeq
 
    member this.TransformLiteralExpression (x: LiteralExpressionData) =
        getConstantValueOfExpression x.Node

    member this.TransformStatement (x: StatementData) : Statement =
        try
            match x with
            | StatementData.CommonForEachStatement    x -> this.TransformCommonForEachStatement x
            | StatementData.Block                     x -> this.TransformBlock x
            | StatementData.LocalFunctionStatement    x -> this.TransformLocalFunctionStatement x
            | StatementData.LocalDeclarationStatement x -> this.TransformLocalDeclarationStatement x
            | StatementData.ExpressionStatement       x -> this.TransformExpressionStatement x
            | StatementData.EmptyStatement            x -> this.TransformEmptyStatement x
            | StatementData.LabeledStatement          x -> this.TransformLabeledStatement x
            | StatementData.GotoStatement             x -> this.TransformGotoStatement x
            | StatementData.BreakStatement            x -> this.TransformBreakStatement x
            | StatementData.ContinueStatement         x -> this.TransformContinueStatement x
            | StatementData.ReturnStatement           x -> this.TransformReturnStatement x
            | StatementData.ThrowStatement            x -> this.TransformThrowStatement x
            | StatementData.YieldStatement            x -> this.TransformYieldStatement x
            | StatementData.WhileStatement            x -> this.TransformWhileStatement x
            | StatementData.DoStatement               x -> this.TransformDoStatement x
            | StatementData.ForStatement              x -> this.TransformForStatement x
            | StatementData.UsingStatement            x -> this.TransformUsingStatement x
            | StatementData.FixedStatement            _ -> NotSupported "fixed"
            | StatementData.CheckedStatement          x -> this.TransformCheckedStatement x
            | StatementData.UnsafeStatement           _ -> NotSupported "unsafe"
            | StatementData.LockStatement             _ -> NotSupported "lock"
            | StatementData.IfStatement               x -> this.TransformIfStatement x
            | StatementData.SwitchStatement           x -> this.TransformSwitchStatement x
            | StatementData.TryStatement              x -> this.TransformTryStatement x             
        with e ->
            env.Compilation.AddError(Some (getSourcePos x.Node), WebSharper.Compiler.SourceError("Error while reading C# code: " + e.Message + " at " + e.StackTrace))
            ExprStatement errorPlaceholder        

    member this.TransformBlock (x: BlockData) : Statement =
        x.Statements |> Seq.map (this.TransformStatement) |> List.ofSeq |> Block
        |> withStatementSourcePos x.Node  

    member this.TransformEmptyStatement (x: EmptyStatementData) : Statement =
        Empty

    member this.TransformLabeledStatement (x: LabeledStatementData) : Statement =
        let symbol = env.SemanticModel.GetDeclaredSymbol(x.Node)
        let label = env.GetLabelId(symbol)
        let statement = x.Statement |> this.TransformStatement
        Labeled (label, statement)
        |> withStatementSourcePos x.Node

    member this.TransformEqualsValueClause (x: EqualsValueClauseData) : Expression =
        x.Value |> this.TransformExpression

    member this.TransformNameColon (x: NameColonData) : Expression =
        x.Name |> this.TransformIdentifierName

    member this.TransformNameEquals (x: NameEqualsData) : Expression =
        x.Name |> this.TransformIdentifierName

    member this.TransformVariableDeclaration (x: VariableDeclarationData) : list<Id * Expression> =
        x.Variables |> Seq.map (this.TransformVariableDeclarator) |> List.ofSeq

    member this.TransformLocalFunctionStatement (x: LocalFunctionStatementData) : _ =
        let symbol = env.SemanticModel.GetDeclaredSymbol(x.Node) :?> IMethodSymbol
        let parameterList = sr.ReadParameters symbol
        for p in parameterList do
            env.Parameters.Add(p.Symbol, (p.ParameterId, p.RefOrOut))
        let body = x.Body |> Option.map this.TransformBlock
        let expressionBody = x.ExpressionBody |> Option.map this.TransformArrowExpressionClause
        let id = env.GetLocalFunctionId(symbol)
 
        let b =
            match expressionBody with
            | Some b -> Return b
            | _ -> body.Value
        let b =
            if symbol.IsAsync then
                let b = 
                    b |> Continuation.addLastReturnIfNeeded Undefined
                    |> Continuation.AwaitTransformer().TransformStatement 
                    |> BreakStatement
                    |> Continuation.FreeNestedGotos().TransformStatement
                let labels = Continuation.CollectLabels.Collect b
                Continuation.AsyncTransformer(labels, sr.ReadAsyncReturnKind symbol).TransformMethodBody(b)
            else
                b
 
        // TODO : optional?
        // TODO : generics?
        funcFromLambda(id, None, true, parameterList |> List.map (fun p -> p.ParameterId), b, [])
        |> withStatementSourcePos x.Node

    member this.TransformLocalDeclarationStatement (x: LocalDeclarationStatementData) : Statement =
        x.Declaration |> this.TransformVariableDeclaration |> List.map VarDeclaration |> Block 
        |> withStatementSourcePos x.Node

    member this.TransformSingleVariableDesignation (x: SingleVariableDesignationData) : _ =
        let symbol = env.SemanticModel.GetDeclaredSymbol(x.Node)
        let id = 
            match symbol with
            | :? ILocalSymbol as s ->
                let id = Id.New(s.Name, not s.IsConst)
                env.Vars.Add(s, id)
                id
            | _ -> failwithf "this.TransformSingleVariableDesignation: invalid symbol type %A" symbol
        NewVar(id, Undefined)
        |> withExprSourcePos x.Node

    member this.TransformDiscardDesignation (x: DiscardDesignationData) : _ =
        Undefined

    member this.TransformParenthesizedVariableDesignation (x: ParenthesizedVariableDesignationData) : _ =
        let variables = x.Variables |> Seq.map this.TransformVariableDesignation |> List.ofSeq
        NewArray variables
        |> withExprSourcePos x.Node

    member this.TransformVariableDesignation (x: VariableDesignationData) : _ =
        match x with
        | VariableDesignationData.SingleVariableDesignation        x -> this.TransformSingleVariableDesignation x
        | VariableDesignationData.DiscardDesignation               x -> this.TransformDiscardDesignation x
        | VariableDesignationData.ParenthesizedVariableDesignation x -> this.TransformParenthesizedVariableDesignation x

    member this.PatternSet (e, value, rTypOpt: ITypeSymbol option) =
        let v = Id.New ("$m", mut = false)
        let rec trDesignation v e =
            match IgnoreExprSourcePos e with
            | Var x ->
                VarSet(x, v) 
            | NewVar(nv, _) ->
                NewVar(nv, v) 
            | NewArray ds ->
                let dvalue =
                    match rTypOpt with
                    | None -> v // parenthesized variable designation
                    | Some rTyp ->
                        let t = sr.ReadNamedType (rTyp :?> INamedTypeSymbol)
                        if rTyp.IsTupleType || t.Entity.Value.FullName.StartsWith "System.Tuple" then
                            v 
                        else
                            // workaround until https://github.com/dotnet/roslyn/pull/16541 is available
                            let len = ds.Length
                            //env.SemanticModel.LookupSymbols(value.
                            let decM =
                                rTyp.GetMembers("Deconstruct") |> Seq.tryPick (function
                                    | :? IMethodSymbol as m ->
                                        let md = sr.ReadMethod m
                                        if md.Value.Parameters.Length = len then
                                            Some md
                                        else None
                                    | _ -> None
                                )
                            match decM with
                            | Some decM ->
                                let vars = List.init len (fun _ -> Id.New(mut = false))
                                Sequential [
                                    for v in vars do yield NewVar(v, Undefined)
                                    yield Call(Some v, t, NonGeneric decM, vars |> List.map (fun i -> MakeRef (Var i) (fun e -> VarSet(i, e)) i.VarType))
                                    yield NewArray (vars |> List.map Var)
                                ]
                            | _ -> failwith "Failed to find deconstructor, extension methods are not supported yet"

                let tvalue = Id.New ("$m", mut = false)                
                Let (tvalue, dvalue,
                    Sequential(
                        ds |> List.mapi (fun i di -> 
                            trDesignation (Var tvalue).[Value (Int i)] di      
                        )
                    )
                )
            | Undefined -> Undefined
            | Call(t, td, g, []) ->
                Call(t, td, setterOf g, [ v ])
            | _ -> failwithf "Unexpected form in variable designation: %s" (Debug.PrintExpression e)
            |> WithSourcePosOfExpr e
        Let(v, value, trDesignation (Var v) e)

    member this.TransformDeclarationExpression (x: DeclarationExpressionData) : _ =
        x.Designation |> this.TransformVariableDesignation

    member this.TransformExpressionStatement (x: ExpressionStatementData) : Statement =
        x.Expression |> this.TransformExpression |> ExprStatement

    member this.TransformVariableDeclarator (x: VariableDeclaratorData) : Id * Expression =    
        let symbol = env.SemanticModel.GetDeclaredSymbol(x.Node)
        let id, ftyp = 
            match symbol with
            | :? ILocalSymbol as s ->
                let id = trLocalId s
                env.Vars.Add(s, id)
                id, None
            | :? IFieldSymbol as s ->
                Id.New(s.Name), Some s.Type
            | _ -> failwithf "this.TransformVariableDeclarator: invalid symbol type %A" symbol
        let initializer = 
            match x.Initializer with
            | Some i -> i |> this.TransformEqualsValueClause
            | _ -> 
            match ftyp with
            | Some t -> DefaultValueOf (sr.ReadType t)
            | _ -> Undefined
        id, initializer

    member this.TransformSwitchStatement (x: SwitchStatementData) : Statement =
        let expression = x.Expression |> this.TransformExpression
        let sections = x.Sections |> Seq.map (this.TransformSwitchSection) |> List.ofSeq
        let hasPattern = 
            sections |> List.exists (fst >> List.exists (function PatternLabel _ -> true | _ -> false))
        if hasPattern then
            let sv = Id.New("$sv", mut = false)
            let defaultSection, nonDefaultSections =
                sections |> List.partition (fst >> List.exists (function DefaultLabel -> true | _ -> false)) 
            
            let defaultBody =
                match defaultSection with
                | [] -> Empty
                | (_, s) :: _ -> removeBreaksTr.TransformStatement s

            Let (sv, expression, 
                let nestedIfs =
                    (nonDefaultSections, defaultBody) 
                    ||> List.foldBack (fun (labels, body) rest ->
                        let condition =
                            labels |> List.map (function
                                | DefaultLabel -> failwith "impossible"
                                | ConstantLabel c -> Var sv ^== c
                                | PatternLabel p -> p sv
                            )
                            |> List.reduce (^||)
                        If (condition, removeBreaksTr.TransformStatement body, rest)
                    )
                StatementExpr(nestedIfs, None)
            )
            |> ExprStatement
        else
            let sections = 
                sections |> List.map (fun (labels, body) ->
                    labels |> List.map (function
                        | DefaultLabel -> None
                        | ConstantLabel c -> Some c
                        | PatternLabel _ -> failwith "impossible"
                    ), body
                )

            CSharpSwitch (expression, sections)
            |> withStatementSourcePos x.Node

    member this.TransformSwitchSection (x: SwitchSectionData) : list<CSharpSwitchLabel> * Statement =
        let labels = x.Labels |> Seq.map (this.TransformSwitchLabel) |> List.ofSeq
        let statements = x.Statements |> Seq.map (this.TransformStatement) |> List.ofSeq |> CombineStatements
        labels, statements

    member this.TransformSwitchLabel (x: SwitchLabelData) : CSharpSwitchLabel =
        match x with
        | SwitchLabelData.CasePatternSwitchLabel x -> this.TransformCasePatternSwitchLabel x
        | SwitchLabelData.CaseSwitchLabel        x -> this.TransformCaseSwitchLabel x
        | SwitchLabelData.DefaultSwitchLabel     x -> DefaultLabel

    member this.TransformWhenClause (x: WhenClauseData) : _ =
        x.Condition |> this.TransformExpression

    member this.TransformCasePatternSwitchLabel (x: CasePatternSwitchLabelData) : CSharpSwitchLabel =
        let pattern = x.Pattern |> this.TransformPattern
        let whenClause = x.WhenClause |> Option.map this.TransformWhenClause
        match whenClause with
        | None -> PatternLabel pattern
        | Some w -> PatternLabel (fun i -> pattern i ^&& w)

    member this.TransformCaseSwitchLabel (x: CaseSwitchLabelData) : CSharpSwitchLabel =
        x.Value |> this.TransformExpression
        |> withExprSourcePos x.Node
        |> ConstantLabel

    member this.TransformGotoStatement (x: GotoStatementData) : Statement =
        match x.Kind with
        | GotoStatementKind.GotoStatement -> 
            let expr = x.Expression.Value.Node :?> IdentifierNameSyntax
            let symbol = env.SemanticModel.GetSymbolInfo(expr).Symbol :?> ILabelSymbol
            let label = env.GetLabelId(symbol)
            Goto label
        | GotoStatementKind.GotoCaseStatement ->
            let expression = x.Expression |> Option.map (this.TransformExpression)
            GotoCase expression
        | GotoStatementKind.GotoDefaultStatement -> 
            GotoCase None
        |> withStatementSourcePos x.Node

    member this.TransformBreakStatement (x: BreakStatementData) : Statement  =
        Break None
        |> withStatementSourcePos x.Node

    member this.TransformIfStatement (x: IfStatementData) =
        let condition = x.Condition |> this.TransformExpression
        match condition with
        | IsClientCall b ->
            if b then
                x.Statement |> this.TransformStatement
            else
                match x.Else with
                | Some e -> e |> this.TransformElseClause
                | _ -> Empty
        | _ ->
            let statement = x.Statement |> this.TransformStatement
            let else_ = 
                match x.Else with
                | Some e -> e |> this.TransformElseClause
                | _ -> Empty
            If (condition, statement, else_)
            |> withStatementSourcePos x.Node

    member this.TransformElseClause (x: ElseClauseData) : Statement =
        x.Statement |> this.TransformStatement

    member this.TransformAssignmentExpression (x: AssignmentExpressionData) : Expression =
        let withResultValue right makeExpr =
            let isValueNeeded = not (x.Node.Parent :? ExpressionStatementSyntax)
            if isValueNeeded then
                let rv = Id.New () // right side value stored for chaining
                Sequential [ makeExpr (NewVar (rv, right)); Var rv ]
            else
                makeExpr right   
        let leftSymbol = env.SemanticModel.GetSymbolInfo(x.Left.Node).Symbol
        let trR =
            if Option.isSome env.Initializing then 
                RoslynTransformer(env.WithNotInitializing())
            else this
        match leftSymbol with
        | :? IPropertySymbol as leftSymbol ->
            if leftSymbol.IsReadOnly then
                // init property
                let td = leftSymbol.ContainingType |> sr.ReadNamedTypeDefinition
                let right = x.Right |> trR.TransformExpression
                withResultValue right <| fun rv -> FieldSet(Some (Var env.This.Value), NonGeneric td, leftSymbol.Name, rv)
            else
                let typ, setter = getTypeAndMethod leftSymbol.SetMethod
                //if leftSymbol.IsIndexer // TODO property indexers
                match x.Kind with
                | AssignmentExpressionKind.SimpleAssignmentExpression ->
                    let right = x.Right |> trR.TransformExpression
                    if leftSymbol.IsStatic then
                        withResultValue right <| fun rv -> Call(None, typ, setter, [rv])
                    else
                        let left = x.Left |> this.TransformExpression
                        // eliminate getter
                        match IgnoreExprSourcePos left with
                        | Call (Some v, _, _, args) ->
                            withResultValue right <| fun rv -> Call (Some v, typ, setter, args @ [rv])
                        | _ -> failwithf "this.TransformAssignmentExpression: getter expression not recognized for creating assigment"
                | _ ->
                    let left = x.Left |> this.TransformExpression
                    let right = x.Right |> trR.TransformExpression
                    let symbol = env.SemanticModel.GetSymbolInfo(x.Node).Symbol :?> IMethodSymbol
                    let opTyp, operator = getTypeAndMethod symbol
                    if leftSymbol.IsStatic then
                        withResultValue (Call(None, opTyp, operator, [left; right])) <| fun rv -> 
                            Call(None, typ, setter, [rv]) 
                    else
                        let left = x.Left |> this.TransformExpression
                        // eliminate getter
                        match IgnoreExprSourcePos left with
                        | Call (Some v, _, getter, []) ->
                            let m = Id.New ()
                            let leftWithM = Call (Some (Var m), typ, getter, [])
                            withResultValue (Call(None, opTyp, operator, [leftWithM; right])) <| fun rv ->
                                Let (m, v, Call (Some (Var m), typ, setter, [rv])) 
                        | _ -> failwith "this.TransformAssignmentExpression: getter expression not recognized for creating compound assigment"
        | _ ->
        let left = x.Left |> this.TransformExpression
        let right = x.Right |> trR.TransformExpression
        match x.Kind with
        | AssignmentExpressionKind.SimpleAssignmentExpression ->
            match IgnoreExprSourcePos left with
            | Var id -> VarSet(id, right)
            | FieldGet (obj, ty, f) -> FieldSet (obj, ty, f, right)
            | ItemGet(obj, i, _) -> ItemSet (obj, i, right)
            | Application(ItemGet (r, Value (String "get"), _), [], _) ->
                // when right is also a ref, this is a ref local set
                match r, right with
                | Var id, Object [ 
                    "get", MemberKind.Simple, (Function ([], None, _, Return getVal)); 
                    "set", MemberKind.Simple, (Function ([_], None, _, ExprStatement _)) ] ->
                        VarSet(id, right)
                | _ ->
                    withResultValue right <| SetRef r
            | Call (thisOpt, typ, getter, args) ->
                withResultValue right <| fun rv -> Call (thisOpt, typ, setterOf getter, args @ [rv])
            | e -> 
                let rTyp = env.SemanticModel.GetTypeInfo(x.Right.Node).Type
                this.PatternSet(e, right, Some rTyp)
        | _ ->
            let symbol = env.SemanticModel.GetSymbolInfo(x.Node).Symbol :?> IMethodSymbol
            let opTyp, operator = getTypeAndMethod symbol
            let e = IgnoreExprSourcePos left
            let direct = 
                if numericTypes.Contains opTyp.Entity.Value.FullName then
                    match e with
                    | Var _ 
                    | ItemGet _ 
                    | FieldGet _ ->
                        let op =
                            match x.Kind with
                            | AssignmentExpressionKind.SimpleAssignmentExpression ->
                                failwith "impossible"    
                            | AssignmentExpressionKind.AddAssignmentExpression ->
                                MutatingBinaryOperator.``+=``
                            | AssignmentExpressionKind.SubtractAssignmentExpression ->  
                                MutatingBinaryOperator.``-=``
                            | AssignmentExpressionKind.MultiplyAssignmentExpression ->   
                                MutatingBinaryOperator.``*=``
                            | AssignmentExpressionKind.DivideAssignmentExpression ->    
                                MutatingBinaryOperator.``/=``
                            | AssignmentExpressionKind.ModuloAssignmentExpression ->     
                                MutatingBinaryOperator.``%=``
                            | AssignmentExpressionKind.AndAssignmentExpression ->        
                                MutatingBinaryOperator.``&=``
                            | AssignmentExpressionKind.ExclusiveOrAssignmentExpression ->
                                MutatingBinaryOperator.``^=``
                            | AssignmentExpressionKind.OrAssignmentExpression ->         
                                MutatingBinaryOperator.``|=``
                            | AssignmentExpressionKind.LeftShiftAssignmentExpression ->  
                                MutatingBinaryOperator.``<<=``
                            | AssignmentExpressionKind.RightShiftAssignmentExpression -> 
                                MutatingBinaryOperator.``>>=``
                            | AssignmentExpressionKind.CoalesceAssignmentExpression ->
                                MutatingBinaryOperator.``??=``
                        MutatingBinary(left, op, right) |> Some
                    | _ -> None
                else None
            match direct with
            | Some d -> d
            | _ ->
            match IgnoreExprSourcePos left with
            | Var id -> VarSet(id, Call(None, opTyp, operator, [left; right]))
            | FieldGet (obj, ty, f) -> 
                if leftSymbol :? IEventSymbol then
                    Call(obj, opTyp, operator, [right])
                else
                match obj with
                | None ->
                    FieldSet (None, ty, f, Call(None, opTyp, operator, [left; right]))
                | Some obj ->
                    let m = Id.New ()
                    let leftWithM = FieldGet (Some (Var m), ty, f)
                    Let (m, obj, FieldSet (Some (Var m), ty, f, Call(None, opTyp, operator, [leftWithM; right]))) 
            | ItemGet(obj, i, _) ->
                let m = Id.New ()
                let j = Id.New ()
                let leftWithM = ItemGet (Var m, Var j, NoSideEffect)
                Let (m, obj, Let (j, i, ItemSet(Var m, Var j, Call(None, opTyp, operator, [leftWithM; right]))))
            | Application(ItemGet (r, Value (String "get"), _), [], _) ->
                withResultValue (Call(None, opTyp, operator, [left; right])) <| SetRef r
            | Call (thisOpt, typ, getter, args) ->
                withResultValue (Call(None, opTyp, operator, [left; right])) <| fun rv ->
                    Call (thisOpt, typ, setterOf getter, args @ [rv])
            | e -> failwithf "AssignmentExpression left side not handled: %s" (Debug.PrintExpression e)
            
        |> withExprSourcePos x.Node

    member this.TransformParenthesizedExpression (x: ParenthesizedExpressionData) : Expression =
        x.Expression |> this.TransformExpression

    member this.TransformTupleExpression (x: TupleExpressionData) : _ =
        let arguments = x.Arguments |> Seq.map this.TransformArgument |> List.ofSeq
        NewArray (arguments |> List.map snd)

    member this.TransformBinaryExpression (x: BinaryExpressionData) : Expression =
        let left = x.Left |> this.TransformExpression
        match x.Kind with
        | BinaryExpressionKind.IsExpression -> 
            let rightType = env.SemanticModel.GetTypeInfo(x.Right.Node).ConvertedType |> sr.ReadType
            TypeCheck (left, rightType)
        | BinaryExpressionKind.AsExpression -> 
            let rightType = env.SemanticModel.GetTypeInfo(x.Right.Node).ConvertedType |> sr.ReadType
            let asVar = Id.New ()
            Let (asVar, left, Conditional (TypeCheck (Var asVar, rightType), Var asVar, Value Null))
        | _ ->
        let right = x.Right |> this.TransformExpression
        match x.Kind with
        | BinaryExpressionKind.LogicalOrExpression ->
            Conditional(left, Value (Bool true), right)
        | BinaryExpressionKind.LogicalAndExpression ->
            Conditional(left, right, Value (Bool false))
        | BinaryExpressionKind.CoalesceExpression ->
            let leftType = env.SemanticModel.GetTypeInfo(x.Left.Node).ConvertedType |> sr.ReadType
            Coalesce(left, leftType, right)
        | _ -> 
            let leftTypeSymbol = env.SemanticModel.GetTypeInfo(x.Left.Node).Type
            let rightTypeSymbol = env.SemanticModel.GetTypeInfo(x.Right.Node).Type
            let tupleOp =
                if leftTypeSymbol.IsTupleType && rightTypeSymbol.IsTupleType then
                    match x.Kind with
                    | BinaryExpressionKind.EqualsExpression ->
                        Some (Macros.UncheckedEquals left right)   
                    | BinaryExpressionKind.NotEqualsExpression ->
                        Some (Unary(UnaryOperator.``!``, Macros.UncheckedEquals left right))                       
                    | _ -> None
                else None
            match tupleOp with
            | Some res -> res
            | _ ->
                let symbol = env.SemanticModel.GetSymbolInfo(x.Node).Symbol :?> IMethodSymbol
                let typ, meth = getTypeAndMethod symbol
                if List.isEmpty meth.Generics then
                    // add fake generics type information to resolve nullable operations
                    let leftType = leftTypeSymbol |> sr.ReadType
                    let rightType = rightTypeSymbol |> sr.ReadType
                    let meth =
                        { meth with Generics = [leftType; rightType] }
                    Call(None, typ, meth, [left; right])
                else
                    Call(None, typ, meth, [left; right])
        |> withExprSourcePos x.Node

    member this.TransformConditionalExpression (x: ConditionalExpressionData) : Expression =
        let condition = x.Condition |> this.TransformExpression
        match condition with
        | IsClientCall b ->
            if b then x.WhenTrue |> this.TransformExpression
            else x.WhenFalse |> this.TransformExpression
        | _ ->
            let whenTrue = x.WhenTrue |> this.TransformExpression
            let whenFalse = x.WhenFalse |> this.TransformExpression
            Conditional(condition, whenTrue, whenFalse)
            |> withExprSourcePos x.Node

    member this.TransformMethodDeclarationBase (symbol: IMethodSymbol, parameterList, stBody, exprBody) : CSharpMethod =
        let returnType = sr.ReadType symbol.ReturnType
        for p in parameterList do
            env.Parameters.Add(p.Symbol, (p.ParameterId, p.RefOrOut))
        let body = 
            match stBody |> Option.map this.TransformBlock with
            | Some b -> b
            | _ -> 
            match exprBody |> Option.map this.TransformArrowExpressionClause with
            | Some v -> Block [ Return v ]
            | _ -> Empty

        {
            IsStatic = symbol.IsStatic
            Parameters = parameterList
            This = if symbol.IsStatic then None else env.This 
            Body = body
            IsAsync = symbol.IsAsync
            ReturnType = returnType
        }

    member this.TransformMethodDeclaration (x: MethodDeclarationData) : CSharpMethod =
        let symbol = env.SemanticModel.GetDeclaredSymbol(x.Node)
        //let parameterList = sr.ReadParameters symbol
        let parameterList = x.ParameterList |> this.TransformParameterList
        this.TransformMethodDeclarationBase(symbol, parameterList, x.Body, x.ExpressionBody)

    member this.TransformOperatorDeclaration (x: OperatorDeclarationData) : CSharpMethod =
        let symbol = env.SemanticModel.GetDeclaredSymbol(x.Node)
        //let parameterList = sr.ReadParameters symbol
        let parameterList = x.ParameterList |> this.TransformParameterList
        this.TransformMethodDeclarationBase(symbol, parameterList, x.Body, x.ExpressionBody)

    member this.TransformConversionOperatorDeclaration (x: ConversionOperatorDeclarationData) : CSharpMethod =
        let symbol = env.SemanticModel.GetDeclaredSymbol(x.Node)
        //let parameterList = sr.ReadParameters symbol
        let parameterList = x.ParameterList |> this.TransformParameterList
        this.TransformMethodDeclarationBase(symbol, parameterList, x.Body, x.ExpressionBody)

    member this.TransformRecordDeclaration (x: RecordDeclarationData) : _ =
        //let attributeLists = x.AttributeLists |> Seq.map this.TransformAttributeList |> List.ofSeq
        //let typeParameterList = x.TypeParameterList |> Option.map this.TransformTypeParameterList
        let parameterList = x.ParameterList |> Option.map this.TransformParameterList

        match parameterList with
        | Some pl ->
            for p in pl do
                env.Parameters.Add(p.Symbol, (p.ParameterId, p.RefOrOut))
        | None -> ()

        let baseList = x.BaseList |> Option.map this.TransformBaseList
        //let constraintClauses = x.ConstraintClauses |> Seq.map this.TransformTypeParameterConstraintClause |> List.ofSeq
        //let members = x.Members |> Seq.map this.TransformMemberDeclaration |> List.ofSeq

        let positionalFields =
            match parameterList with
            | None -> []
            | Some pl ->
                pl |> List.map (fun p ->
                    let pDef =
                        Hashed {
                            MethodName = "get_" + p.Symbol.Name
                            Parameters = []
                            ReturnType = p.Type
                            Generics = 0
                        }
                    p, pDef
                )   

        let otherFields =
            x.Members |> Seq.collect (fun m ->
                match m with
                | MemberDeclarationData.BaseFieldDeclaration (BaseFieldDeclarationData.FieldDeclaration fDecl) ->
                    fDecl.Declaration.Variables |> Seq.map (fun v ->
                        let vSymbol = env.SemanticModel.GetDeclaredSymbol(v.Node) :?> IFieldSymbol
                        CSharpRecordField (vSymbol, sr.ReadType vSymbol.Type)
                    )
                | MemberDeclarationData.BasePropertyDeclaration (BasePropertyDeclarationData.PropertyDeclaration pDecl) ->
                    let symbol = env.SemanticModel.GetDeclaredSymbol(pDecl.Node)
                    let pDef =
                        Hashed {
                            MethodName = "get_" + symbol.Name
                            Parameters = []
                            ReturnType = sr.ReadType symbol.Type
                            Generics = 0
                        }
                    Seq.singleton (CSharpRecordProperty (symbol, pDef))
                | _ -> 
                    Seq.empty
            )
            |> List.ofSeq

        let baseCall =
            baseList |> Option.bind (
                List.tryPick (
                    function
                    | (typ: Type), Some (bCtor, args, reorder) -> 
                        Some (NonGeneric typ.TypeDefinition, bCtor, args, reorder)
                    | _ -> None
                )
            )
            |> Option.defaultValue Definitions.DefaultRecordBaseCall

        {
            PositionalFields = positionalFields
            OtherFields = otherFields
            BaseCall = baseCall
        }

    member this.TransformParameterList (x: ParameterListData) : list<CSharpParameter> =
        x.Parameters |> Seq.map this.TransformParameter |> List.ofSeq

    member this.TransformParameter (symbol: IParameterSymbol) : CSharpParameter =
        let typ = sr.ReadType symbol.Type
        let defValue = 
            if symbol.HasExplicitDefaultValue then None
            else
                Some (Value (ReadLiteral symbol.ExplicitDefaultValue))
        let id = Id.New(symbol.Name, opt = Option.isSome defValue)
        {
            ParameterId = id
            Symbol = symbol
            DefaultValue = defValue
            Type = typ
            RefOrOut = symbol.RefKind <> RefKind.None
        }

    member this.TransformParameter (x: ParameterData) : CSharpParameter =
        let symbol = env.SemanticModel.GetDeclaredSymbol(x.Node)
        let typ = sr.ReadType symbol.Type
        let defValue = 
            match x.Default with
            | Some defExpr -> Some (this.TransformEqualsValueClause defExpr)
            | None ->
                if symbol.IsOptional then
                    Some (DefaultValueOf typ)
                elif symbol.IsParams then
                    Some (NewArray [])
                else None
        match x.Identifier with
        | ParameterIdentifier.IdentifierToken t -> 
            let id = Id.New(t, opt = Option.isSome defValue)
            {
                ParameterId = id
                Symbol = symbol
                DefaultValue = defValue
                Type = typ
                RefOrOut = symbol.RefKind <> RefKind.None
            }
        | ParameterIdentifier.ArgListKeyword -> NotSupported "__arglist"

    member this.TransformBaseList (x: BaseListData) : _ =
        let types = x.Types |> Seq.map this.TransformBaseType |> List.ofSeq
        types

    member this.TransformBaseType (x: BaseTypeData) : _ =
        match x with
        | BaseTypeData.SimpleBaseType             x -> this.TransformSimpleBaseType x
        | BaseTypeData.PrimaryConstructorBaseType x -> this.TransformPrimaryConstructorBaseType x

    member this.TransformSimpleBaseType (x: SimpleBaseTypeData) : _ =
        let typ = env.SemanticModel.GetTypeInfo(x.Type.Node).ConvertedType |> sr.ReadType
        typ, None

    member this.TransformPrimaryConstructorBaseType (x: PrimaryConstructorBaseTypeData) : _ =
        let symbol = env.SemanticModel.GetSymbolInfo(x.Node).Symbol :?> IMethodSymbol
        let typ = env.SemanticModel.GetTypeInfo(x.Type.Node).ConvertedType |> sr.ReadType
        let argumentList = x.ArgumentList |> this.TransformArgumentList
        let argumentListWithParamsFix = fixParamArray symbol x.ArgumentList argumentList
        let tempVars, args = readReorderedParams argumentListWithParamsFix
        let initTempVars = List.foldBack (fun (v, e) b -> Let (v, e, b)) tempVars
        let bCtor = sr.ReadConstructor symbol

        typ, Some (bCtor, args, initTempVars)

    member this.TransformArrowExpressionClause (x: ArrowExpressionClauseData) : Expression =
        x.Expression |> this.TransformExpression

    member this.TransformArrowExpressionClauseAsMethod (meth: IMethodSymbol) (x: ArrowExpressionClauseData) : CSharpMethod =
        let parameterList = sr.ReadParameters meth
        for p in parameterList do
            env.Parameters.Add(p.Symbol, (p.ParameterId, p.RefOrOut))        
        let body = x.Expression |> this.TransformExpression
        let rTyp = sr.ReadType meth.ReturnType
        {
            IsStatic = meth.IsStatic
            Parameters = parameterList
            This = if meth.IsStatic then None else env.This 
            Body = if rTyp = VoidType then ExprStatement body else Return body
            IsAsync = meth.IsAsync
            ReturnType = sr.ReadType meth.ReturnType
        }

    member this.TransformReturnStatement (x: ReturnStatementData) : Statement =
        defaultArg (x.Expression |> Option.map (this.TransformExpression)) Undefined |> Return

    member this.TransformBaseObjectCreationExpression (x: BaseObjectCreationExpressionData) : _ =
        match x with
        | BaseObjectCreationExpressionData.ImplicitObjectCreationExpression x -> this.TransformImplicitObjectCreationExpression x
        | BaseObjectCreationExpressionData.ObjectCreationExpression         x -> this.TransformObjectCreationExpression x

    member this.TransformObjectCreationExpression (x: ObjectCreationExpressionData) : _ =
        let isDelegate = env.SemanticModel.GetTypeInfo(x.Node).Type.TypeKind = TypeKind.Delegate
        if isDelegate then
            let argumentList = defaultArg (x.ArgumentList |> Option.map (this.TransformArgumentList)) []
            match argumentList with
            | [_, lam] -> lam
            | _ -> failwith "Delegate constructor must have a single argument"
        else
        let symbol = env.SemanticModel.GetSymbolInfo(x.Node).Symbol :?> IMethodSymbol
        let typ = sr.ReadNamedType symbol.ContainingType
        let argumentList = defaultArg (x.ArgumentList |> Option.map (this.TransformArgumentList)) []
        let argumentListWithParamsFix = 
            match x.ArgumentList with
            | Some a -> fixParamArray symbol a argumentList
            | _-> argumentList
        let tempVars, args = readReorderedParams argumentListWithParamsFix
        let ctor =
            Ctor (typ, sr.ReadConstructor symbol, args)
            |> List.foldBack (fun (v, e) b -> Let (v, e, b)) tempVars
        match x.Initializer with
        | Some init ->
            let o = Id.New ()          
            let initializer = init |> RoslynTransformer(env.WithInitializing(o)).TransformInitializerExpression
            Let(o, ctor, Sequential [initializer; Var o])
        | None -> ctor
        |> withExprSourcePos x.Node

    member this.TransformImplicitObjectCreationExpression (x: ImplicitObjectCreationExpressionData) : _ =
        let isDelegate = env.SemanticModel.GetTypeInfo(x.Node).Type.TypeKind = TypeKind.Delegate
        if isDelegate then
            let argumentList = this.TransformArgumentList x.ArgumentList
            match argumentList with
            | [_, lam] -> lam
            | _ -> failwith "Delegate constructor must have a single argument"
        else
        let symbol = env.SemanticModel.GetSymbolInfo(x.Node).Symbol :?> IMethodSymbol
        let typ = sr.ReadNamedType symbol.ContainingType
        let argumentList = this.TransformArgumentList x.ArgumentList
        let argumentListWithParamsFix = 
            fixParamArray symbol x.ArgumentList argumentList
        let tempVars, args = readReorderedParams argumentListWithParamsFix
        let ctor =
            Ctor (typ, sr.ReadConstructor symbol, args)
            |> List.foldBack (fun (v, e) b -> Let (v, e, b)) tempVars
        match x.Initializer with
        | Some init ->
            let o = Id.New ()          
            let initializer = init |> RoslynTransformer(env.WithInitializing(o)).TransformInitializerExpression
            Let(o, ctor, Sequential [initializer; Var o])
        | None -> ctor
        |> withExprSourcePos x.Node

    member this.TransformInitializerExpression (x: InitializerExpressionData) : _ =
        match x.Kind with
        | InitializerExpressionKind.ObjectInitializerExpression
        | InitializerExpressionKind.WithInitializerExpression -> 
            x.Expressions |> Seq.map this.TransformExpression |> List.ofSeq |> Sequential
        | InitializerExpressionKind.CollectionInitializerExpression -> 
            x.Expressions |> Seq.map (fun e -> 
                let item = this.TransformExpression e
                let addSymbol = 
                    let s = env.SemanticModel.GetCollectionInitializerSymbolInfo(e.Node).Symbol :?> IMethodSymbol
                    // workaround for implicit object creation expression's collection initializer 
                    if isNull s then
                        let iocType = env.SemanticModel.GetTypeInfo(x.Node.Parent).Type
                        let addMethods = iocType.GetMembers("Add").OfType<IMethodSymbol>() |> Array.ofSeq
                        if addMethods.Length = 1 then
                            addMethods.[0]
                        else
                            failwith "Collection initializer for implicit object creation only supported with a non-overloaded Add method"
                    else
                        s
                let cTyp, addM = getTypeAndMethod addSymbol
                match IgnoreExprSourcePos item with
                | ComplexElement cItem ->
                    Call(Some (Var env.Initializing.Value), cTyp, addM, cItem)
                | _ -> Call(Some (Var env.Initializing.Value), cTyp, addM, [item])
            ) |> List.ofSeq |> Sequential
        | InitializerExpressionKind.ArrayInitializerExpression ->
            // TODO: 2-dimensional
            x.Expressions |> Seq.map this.TransformExpression |> List.ofSeq |> NewArray
        | InitializerExpressionKind.ComplexElementInitializerExpression -> 
            x.Expressions |> Seq.map this.TransformExpression |> List.ofSeq |> ComplexElement
        |> withExprSourcePos x.Node

    member this.TransformAnonymousObjectCreationExpression (x: AnonymousObjectCreationExpressionData) : _ =
        x.Initializers |> Seq.map this.TransformAnonymousObjectMemberDeclarator |> List.ofSeq
        |> Object

    member this.TransformAnonymousObjectMemberDeclarator (x: AnonymousObjectMemberDeclaratorData) : string * MemberKind * Expression =
        let expression = x.Expression |> this.TransformExpression
        let identifierName =
            match x.NameEquals with
            | Some n -> n.Name.Node.Identifier.Text
            | None -> (x.Expression.Node :?> IdentifierNameSyntax).Identifier.Text
        identifierName, MemberKind.Simple, expression

    member this.TransformConstructorDeclaration (x: ConstructorDeclarationData) : _ =
        let parameterList = x.ParameterList |> this.TransformParameterList
        for p in parameterList do
            env.Parameters.Add(p.Symbol, (p.ParameterId, p.RefOrOut))
        let initializer = 
            match x.Initializer with
            | Some i -> Some <| this.TransformConstructorInitializer i
            | _ -> 
                let symbol = env.SemanticModel.GetDeclaredSymbol(x.Node)
                if symbol.IsStatic then None else
                let bTyp = symbol.ContainingType.BaseType
                if isNull bTyp then None else
                match sr.ReadNamedType bTyp with
                | { Entity = td } when td = Definitions.Obj || td = Definitions.ValueType ->
                    None
                | b ->
                    Some <| BaseInitializer(b, ConstructorInfo.Default(), [], id)
        let body = 
            match x.ExpressionBody |> Option.map this.TransformArrowExpressionClause with
            | Some e -> Some (ExprStatement e)
            | _ ->
                x.Body |> Option.map (this.TransformBlock)
        {
            Parameters = parameterList
            Body = defaultArg body Empty
            Initializer = initializer
        }

    member this.TransformConstructorInitializer (x: ConstructorInitializerData) : _ =
        let symbol = env.SemanticModel.GetSymbolInfo(x.Node).Symbol :?> IMethodSymbol
        let argumentList = x.ArgumentList |> this.TransformArgumentList
        let argumentListWithParamsFix = fixParamArray symbol x.ArgumentList argumentList
        let tempVars, args = readReorderedParams argumentListWithParamsFix
        let initTempVars = List.foldBack (fun (v, e) b -> Let (v, e, b)) tempVars
        match x.Kind with
        | ConstructorInitializerKind.BaseConstructorInitializer -> 
            BaseInitializer (sr.ReadNamedType symbol.ContainingType, sr.ReadConstructor symbol, args, initTempVars)
        | ConstructorInitializerKind.ThisConstructorInitializer -> 
            ThisInitializer (sr.ReadConstructor symbol, args, initTempVars)

    member this.TransformAccessorDeclaration (x: AccessorDeclarationData) : _ =
        let symbol = env.SemanticModel.GetDeclaredSymbol(x.Node)
        let returnType = sr.ReadType symbol.ReturnType
        let parameterList = sr.ReadParameters symbol
        for p in parameterList do
            env.Parameters.Add(p.Symbol, (p.ParameterId, p.RefOrOut))
        let body = 
            match x.ExpressionBody |> Option.map this.TransformArrowExpressionClause with
            | Some e -> 
                match x.Kind with 
                | AccessorDeclarationKind.GetAccessorDeclaration ->
                    Some (Return e)
                | _ ->
                    Some (ExprStatement e)
            | _ ->
                x.Body |> Option.map (this.TransformBlock)
        match x.Kind with
        | AccessorDeclarationKind.GetAccessorDeclaration      
        | AccessorDeclarationKind.SetAccessorDeclaration 
        | AccessorDeclarationKind.InitAccessorDeclaration -> 
            let body =
                match body with
                | Some b -> b
                | _ ->
                    let pr = symbol.AssociatedSymbol :?> IPropertySymbol
                    let typ = sr.ReadNamedType symbol.ContainingType
                    let backingfield = 
                        symbol.ContainingType.GetMembers().OfType<IFieldSymbol>()
                        |> Seq.find (fun bf -> bf.AssociatedSymbol = symbol.AssociatedSymbol)
                    if pr.IsStatic then 
                        match x.Kind with 
                        | AccessorDeclarationKind.GetAccessorDeclaration ->     
                            Return <| FieldGet(None, typ, backingfield.Name)
                        | AccessorDeclarationKind.SetAccessorDeclaration 
                        | AccessorDeclarationKind.InitAccessorDeclaration -> 
                            let v = parameterList.Head.ParameterId
                            ExprStatement <| FieldSet(None, typ, backingfield.Name, Var v)
                        | _ -> failwith "impossible"
                    else
                        match x.Kind with 
                        | AccessorDeclarationKind.GetAccessorDeclaration ->     
                            Return <| FieldGet(Some (Var env.This.Value), typ, backingfield.Name)
                        | AccessorDeclarationKind.SetAccessorDeclaration 
                        | AccessorDeclarationKind.InitAccessorDeclaration -> 
                            let v = parameterList.Head.ParameterId
                            ExprStatement <| FieldSet(Some (Var env.This.Value), typ, backingfield.Name, Var v)
                        | _ -> failwith "impossible"
            {
                IsStatic = symbol.IsStatic
                Parameters = parameterList
                This = if symbol.IsStatic then None else env.This 
                Body = body
                IsAsync = symbol.IsAsync
                ReturnType = returnType
            }
        | AccessorDeclarationKind.AddAccessorDeclaration
        | AccessorDeclarationKind.RemoveAccessorDeclaration ->
            {
                IsStatic = symbol.IsStatic
                Parameters = parameterList
                This = if symbol.IsStatic then None else env.This 
                Body = body.Value
                IsAsync = symbol.IsAsync
                ReturnType = returnType
            }
        | AccessorDeclarationKind.UnknownAccessorDeclaration -> TODO x

    member this.TransformWhileStatement (x: WhileStatementData) : _ =
        let condition = x.Condition |> this.TransformExpression
        let statement = x.Statement |> this.TransformStatement
        While(condition, statement)
        |> withStatementSourcePos x.Node

    member this.TransformDoStatement (x: DoStatementData) : _ =
        let statement = x.Statement |> this.TransformStatement
        let condition = x.Condition |> this.TransformExpression
        DoWhile(statement, condition)
        |> withStatementSourcePos x.Node

    member this.TransformForStatement (x: ForStatementData) : _ =
        let declaration = x.Declaration |> Option.map (this.TransformVariableDeclaration)
        let initializers = x.Initializers |> Seq.map (this.TransformExpression) |> List.ofSeq
        let condition = x.Condition |> Option.map (this.TransformExpression)
        let incrementors = x.Incrementors |> Seq.map (this.TransformExpression) |> List.ofSeq
        let statement = x.Statement |> this.TransformStatement
        let init = 
            match initializers with
            | [] -> None 
            | [i] -> Some i 
            | init -> Some (Sequential init)
        let incr =
            match incrementors with
            | [] -> None 
            | [i] -> Some i 
            | init -> Some (Sequential init)
        let loop = For(init, condition, incr, statement)
        match declaration with
        | Some d ->
            Block ((d |> List.map VarDeclaration) @ [ loop ])
        | None -> 
            loop
        |> withStatementSourcePos x.Node

    member this.TransformIncrOrDecr (node : ExpressionSyntax, operand, isPostfix) =
        let symbol = env.SemanticModel.GetSymbolInfo(node).Symbol :?> IMethodSymbol
        let typ, meth = getTypeAndMethod symbol
        let e = IgnoreExprSourcePos operand
        let direct =
            let getOp() =
                if meth.Entity.Value.MethodName = "op_Increment" then
                    if isPostfix then 
                        MutatingUnaryOperator.``()++``
                    else
                        MutatingUnaryOperator.``++()``
                else
                    if isPostfix then 
                        MutatingUnaryOperator.``()--``
                    else
                        MutatingUnaryOperator.``--()``
            if numericTypes.Contains typ.Entity.Value.FullName then
                match e with
                | Var _ 
                | ItemGet _ 
                | FieldGet _ ->
                    MutatingUnary(getOp(), operand) |> Some
                | _ -> None
            else None
        match direct with
        | Some d -> d |> withExprSourcePos node
        | _ ->
        let callOp v = Call(None, typ, meth, [ v ])
        let withResultValue alwaysSaveValue right makeExpr =
            let isValueNeeded = not (node.Parent :? ExpressionStatementSyntax)
            let rv = Id.New () // right side value stored for chaining
            if isValueNeeded then
                if isPostfix then
                    Sequential [ makeExpr (callOp (NewVar (rv, right))); Var rv ]
                elif alwaysSaveValue then
                    Sequential [ makeExpr (NewVar (rv, callOp right)); Var rv ]
                else
                    makeExpr (callOp right)
            else
                makeExpr (callOp right)   
        match e with
        | Var v ->
            withResultValue false operand <| fun rv -> VarSet(v, rv)
        | ItemGet(o, i, _) ->
            let ov = Id.New ()
            let iv = Id.New ()
            withResultValue false (ItemGet(Var ov, Var iv, NoSideEffect)) <| fun rv -> 
                ItemSet(Var ov, Var iv, rv)
        | FieldGet(o, t, f) ->
            match o with
            | Some o ->
                let ov = Id.New ()
                withResultValue false (FieldGet(Some (Var ov), t, f)) <| fun rv ->
                    Let (ov, o, FieldSet(Some (Var ov), t, f, rv))
            | _ ->
                withResultValue false operand <| fun rv -> FieldSet(None, t, f, rv)
        | Application(ItemGet (r, Value (String "get"), _), [], _) ->
            withResultValue true operand <| SetRef r
        | Call (thisOpt, typ, getter, args) ->
            withResultValue true operand <| fun rv ->
                Call (thisOpt, typ, setterOf getter, args @ [rv])
        | _ -> failwithf "ref argument has unexpected form: %+A" e     
        |> withExprSourcePos node
                                                   
    member this.TransformPostfixUnaryExpression (x: PostfixUnaryExpressionData) : _ =
        let operand = x.Operand |> this.TransformExpression
        match x.Kind with
        | PostfixUnaryExpressionKind.PostIncrementExpression 
        | PostfixUnaryExpressionKind.PostDecrementExpression ->
            this.TransformIncrOrDecr(x.Node, operand, true)
        | PostfixUnaryExpressionKind.SuppressNullableWarningExpression ->
            operand

    member this.TransformPrefixUnaryExpression (x: PrefixUnaryExpressionData) : _ =
        let operand = x.Operand |> this.TransformExpression
        match x.Kind with
        | PrefixUnaryExpressionKind.PreIncrementExpression 
        | PrefixUnaryExpressionKind.PreDecrementExpression ->
            this.TransformIncrOrDecr(x.Node, operand, false)
        | _ ->
            let symbol = env.SemanticModel.GetSymbolInfo(x.Node).Symbol :?> IMethodSymbol
            let typ = sr.ReadNamedType symbol.ContainingType
            let ma = symbol.TypeArguments |> Seq.map (sr.ReadType) |> List.ofSeq
            let meth = Generic (sr.ReadMethod symbol) ma
            Call(None, typ, meth, [ operand ])
            |> withExprSourcePos x.Node
    
    member this.TransformUsingStatement (x: UsingStatementData) : _ =
        let declaration = x.Declaration |> Option.map (this.TransformVariableDeclaration)
        let expression = x.Expression |> Option.map (this.TransformExpression)
        let statement = x.Statement |> this.TransformStatement
        
        let disp e =
            Call(
                Some e, 
                NonGeneric (TypeDefinition { Assembly = "netstandard"; FullName = "System.IDisposable" }),
                NonGeneric (Method { MethodName = "Dispose"; Parameters = []; ReturnType = VoidType; Generics = 0 }),
                []            
            )

        match declaration with
        | Some d ->
            Block [
                for ve in d -> VarDeclaration ve
                yield TryFinally(statement, ExprStatement <| Sequential (d |> List.map (fun (v, _) -> disp (Var v))))
            ]
        | _ ->
            let v = Id.New("$using")
            Block [
                VarDeclaration (v, expression.Value)
                TryFinally(statement, ExprStatement <| disp (Var v))
            ]

    member this.TransformTryStatement (x: TryStatementData) : _ =
        let block = x.Block |> this.TransformBlock
        let err = Id.New "err"
        let catches = x.Catches |> Seq.map (RoslynTransformer(env.WithCaught(err)).TransformCatchClause) |> List.ofSeq
        let body =
            if List.isEmpty catches then None else
            if catches |> List.exists (fst >> Option.isSome) then
                Some err,
                Empty |> List.foldBack (fun catch else_ -> 
                    match catch with
                    | Some cond, cbody -> If (cond, cbody, else_)
                    | None, cBody -> cBody
                ) catches
            else
                None,
                List.head catches |> snd
            |> Some

        let finally_ = x.Finally |> Option.map (this.TransformFinallyClause)
        let tryWith =
            match body with
            | Some (err, b) ->
                TryWith(block, err, b)
            | None -> block
        match finally_ with
        | Some f -> TryFinally(tryWith, f)
        | None -> tryWith  
        |> withStatementSourcePos x.Node

    member this.TransformCatchClause (x: CatchClauseData) : _ =
        let declaration = x.Declaration |> Option.map (this.TransformCatchDeclaration)
        match declaration with
        | Some (symbolOpt, typ) ->
            let err = env.Caught.Value    
            symbolOpt |> Option.iter (fun symbol -> env.Vars.Add(symbol, err))
            let filter = x.Filter |> Option.map (this.TransformCatchFilterClause)
            let typeCheck = TypeCheck(Var err, typ) 
            let cond = 
                match filter with
                | Some f -> Conditional(typeCheck, f, Value (Bool false))
                | None -> typeCheck
            let block = x.Block |> this.TransformBlock
            Some cond, block
        | None ->
            let filter = x.Filter |> Option.map (this.TransformCatchFilterClause)
            let block = x.Block |> this.TransformBlock
            filter, block        

    member this.TransformCatchDeclaration (x: CatchDeclarationData) : _ =
        let symbol = env.SemanticModel.GetDeclaredSymbol(x.Node)
        let typ = env.SemanticModel.GetTypeInfo(x.Type.Node).Type
        Option.ofObj symbol, sr.ReadType typ

    member this.TransformCatchFilterClause (x: CatchFilterClauseData) : _ =
        x.FilterExpression |> this.TransformExpression

    member this.TransformFinallyClause (x: FinallyClauseData) : _ =
        x.Block |> this.TransformBlock

    member this.TransformForEach (varSet, expression, statement, info: ForEachStatementInfo) = 
        let call e m =
            let typ, meth = getTypeAndMethod m
            Call(Some e, typ, meth, [])

        let en = Id.New ()
        Block [
            VarDeclaration (en, call expression info.GetEnumeratorMethod)
            TryFinally(
                While(
                    call (Var en) info.MoveNextMethod, 
                    Block [
                        ExprStatement <| varSet (call (Var en) info.CurrentProperty.GetMethod)
                        statement
                    ]
                ), 
                ExprStatement <| call (Var en) info.DisposeMethod
            )
        ]        

    member this.TransformForEachStatement (x: ForEachStatementData) : _ =
        let info = env.SemanticModel.GetForEachStatementInfo(x.Node)
        let expression = x.Expression |> this.TransformExpression
        let symbol = env.SemanticModel.GetDeclaredSymbol(x.Node)
        let v = Id.New symbol.Name
        env.Vars.Add(symbol, v)
        let statement = x.Statement |> this.TransformStatement

        this.TransformForEach ((fun c -> NewVar(v, c)), expression, statement, info)
        |> withStatementSourcePos x.Node

    member this.TransformForEachVariableStatement (x: ForEachVariableStatementData) : _ =
        let info = env.SemanticModel.GetForEachStatementInfo(x.Node)
        let variable = x.Variable |> this.TransformExpression
        let expression = x.Expression |> this.TransformExpression
        let statement = x.Statement |> this.TransformStatement
        let varSet c = this.PatternSet(variable, c, Some info.ElementType)
        this.TransformForEach (varSet, expression, statement, info)
        |> withStatementSourcePos x.Node

    member this.TransformCommonForEachStatement (x: CommonForEachStatementData) : _ =
        match x with
        | CommonForEachStatementData.ForEachStatement         x -> this.TransformForEachStatement x
        | CommonForEachStatementData.ForEachVariableStatement x -> this.TransformForEachVariableStatement x

    member this.TransformAnonymousFunctionExpression (x: AnonymousFunctionExpressionData) : _ =
        match x with
        | AnonymousFunctionExpressionData.LambdaExpression          x -> this.TransformLambdaExpression x
        | AnonymousFunctionExpressionData.AnonymousMethodExpression x -> this.TransformAnonymousMethodExpression x

    member this.TransformLambdaExpression (x: LambdaExpressionData) : _ =
        match x with
        | LambdaExpressionData.SimpleLambdaExpression        x -> this.TransformSimpleLambdaExpression x
        | LambdaExpressionData.ParenthesizedLambdaExpression x -> this.TransformParenthesizedLambdaExpression x

    member this.TransformAnonymousMethodExpression (x: AnonymousMethodExpressionData) : _ =
        let parameterList = x.ParameterList |> Option.map this.TransformParameterList
        let ids =
            parameterList |> Option.defaultValue [] |> List.map (fun p -> 
                let id = Id.New p.ParameterId.Name.Value
                env.Parameters.Add(p.Symbol, (id, false))
                id
            )    
        let block = x.Block |> this.TransformBlock
        let expressionBody = x.ExpressionBody |> Option.map this.TransformExpression
        let body =
            match expressionBody with
            | Some b -> Return b
            | _ -> block
        let symbol = env.SemanticModel.GetSymbolInfo(x.Node).Symbol :?> IMethodSymbol
        let ret = Some (sr.ReadType symbol.ReturnType)
        if symbol.IsAsync then
            let b = 
                body |> Continuation.addLastReturnIfNeeded Undefined
                |> Continuation.AwaitTransformer().TransformStatement 
                |> BreakStatement
                |> Continuation.FreeNestedGotos().TransformStatement
            let labels = Continuation.CollectLabels.Collect b
            Function(ids, None, ret, Continuation.AsyncTransformer(labels, sr.ReadAsyncReturnKind symbol).TransformMethodBody(b))
        else
            Function(ids, None, ret, body)

    member this.TransformSimpleLambdaExpression (x: SimpleLambdaExpressionData) : _ =
        let parameter = x.Parameter |> this.TransformParameter
        let id = Id.New parameter.ParameterId.Name.Value              
        env.Parameters.Add(parameter.Symbol, (id, false))
        let block = x.Block |> Option.map this.TransformBlock
        let expressionBody = x.ExpressionBody |> Option.map this.TransformExpression
        let body =
            match expressionBody with
            | Some b -> Return b
            | _ -> block.Value
        let symbol = env.SemanticModel.GetSymbolInfo(x.Node).Symbol :?> IMethodSymbol
        let ret = Some (sr.ReadType symbol.ReturnType)
        if symbol.IsAsync then
            let b = 
                body |> Continuation.addLastReturnIfNeeded Undefined
                |> Continuation.AwaitTransformer().TransformStatement 
                |> BreakStatement
                |> Continuation.FreeNestedGotos().TransformStatement
            let labels = Continuation.CollectLabels.Collect b
            Function([id], None, ret, Continuation.AsyncTransformer(labels, sr.ReadAsyncReturnKind symbol).TransformMethodBody(b))
        else
            Function([id], None, ret, body)
    
    member this.TransformParenthesizedLambdaExpression (x: ParenthesizedLambdaExpressionData) : _ =
        let symbol = env.SemanticModel.GetSymbolInfo(x.Node).Symbol :?> IMethodSymbol
        //let parameterList = sr.ReadParameters symbol
        let parameterList = x.ParameterList |> this.TransformParameterList
        let ids =
            parameterList |> List.map (fun p -> 
                let id = Id.New p.ParameterId.Name.Value
                env.Parameters.Add(p.Symbol, (id, false))
                id
            )    
        let block = x.Block |> Option.map this.TransformBlock
        let expressionBody = x.ExpressionBody |> Option.map this.TransformExpression
        let body =
            match expressionBody with
            | Some b -> Return b
            | _ -> block.Value
        let ret = Some (sr.ReadType symbol.ReturnType)
        if symbol.IsAsync then
            let b = 
                body |> Continuation.addLastReturnIfNeeded Undefined
                |> Continuation.AwaitTransformer().TransformStatement 
                |> BreakStatement
                |> Continuation.FreeNestedGotos().TransformStatement
            let labels = Continuation.CollectLabels.Collect b
            Function(ids, None, ret, Continuation.AsyncTransformer(labels, sr.ReadAsyncReturnKind symbol).TransformMethodBody(b))
        else
            Function(ids, None, ret, body)

    member this.TransformInstanceExpression (x: InstanceExpressionData) : _ =
        match x with
        | InstanceExpressionData.ThisExpression x -> Var env.This.Value
        | InstanceExpressionData.BaseExpression x -> Base
        |> withExprSourcePos x.Node

    member this.TransformMemberAccess (node: ExpressionSyntax, expr: option<ExpressionData>, name: SimpleNameData) =
        let symbol = env.SemanticModel.GetSymbolInfo(node).Symbol
        let getExpression() =
            if symbol.IsStatic then
                None
            else
                match expr with
                | Some e -> this.TransformExpression e
                | _ -> Var env.Conditional.Value
                |> Some
        match symbol with
        | :? IPropertySymbol as symbol ->
            if symbol.ContainingType.IsAnonymousType then
                ItemGet(getExpression().Value, Value (String (symbol.Name)), NoSideEffect)
            else
                call symbol.GetMethod (getExpression()) [] // TODO property indexers
        | :? IMethodSymbol as symbol ->
            // TODO: this works for invocations but not always
            let expression = getExpression()
            let conv = env.SemanticModel.GetConversion(node)
            if not conv.Exists || conv.IsIdentity then 
                // if its static, left side has no real expression information
                defaultArg expression Undefined
            elif conv.IsMethodGroup then
                let typ, meth = getTypeAndMethod symbol
                NewDelegate(expression, typ, meth)
            else failwithf "this.TransformIdentifierName: unhandled IMethodSymbol conversion: %A" conv 
        | :? IFieldSymbol as symbol ->
            let field =
                match symbol.CorrespondingTupleField with
                | null -> symbol
                | f -> f
            if Option.isSome expr && field.IsConst then
                getConstantValueOfExpression node 
            else
                let expression = getExpression()
                let typ = sr.ReadNamedType field.ContainingType
                let f = field.Name
                FieldGet(expression, typ, f)
        | :? IEventSymbol as symbol ->
            let expression = getExpression()
            let typ = sr.ReadNamedType symbol.ContainingType
            let f = symbol.Name
            FieldGet(expression, typ, f)
        | null ->
            let expression =
                match expr with
                | Some e -> this.TransformExpression e
                | _ -> Var env.Conditional.Value
            ItemGet(expression, Value (String name.Node.Identifier.Text), NonPure)
        | _ ->
            failwith "member access not handled: not property, event, method or field"      
        |> withExprSourcePos node

    member this.TransformMemberAccessExpression (x: MemberAccessExpressionData) : _ =
        this.TransformMemberAccess(x.Node, Some x.Expression, x.Name)

    member this.TransformConditionalAccessExpression (x: ConditionalAccessExpressionData) : _ =
        let expression = x.Expression |> this.TransformExpression
        let id = Id.New ()
        let whenNotNull = x.WhenNotNull |> RoslynTransformer(env.WithConditional(id)).TransformExpression
        Let (id, expression, Conditional(Var id ^== Value Null, Value Null, whenNotNull))

    member this.TransformMemberBindingExpression (x: MemberBindingExpressionData) : _ =
        this.TransformMemberAccess(x.Node, None, x.Name)

    member this.TransformElementBindingExpression (x: ElementBindingExpressionData) : _ =
        this.TransformElementAccess(x.Node, None, x.ArgumentList)

    member this.TransformArrayCreationExpression (x: ArrayCreationExpressionData) : _ =
        let initializer = x.Initializer |> Option.map (this.TransformInitializerExpression)
        defaultArg initializer (NewArray [])

    member this.TransformImplicitArrayCreationExpression (x: ImplicitArrayCreationExpressionData) : _ =
        let initializer = x.Initializer |> this.TransformInitializerExpression
        initializer

    member this.TransformAwaitExpression (x: AwaitExpressionData) : _ =
        let expression = x.Expression |> this.TransformExpression
        Await expression
        |> withExprSourcePos x.Node

    member this.TransformContinueStatement (x: ContinueStatementData) : _ =
        Continue None
        |> withStatementSourcePos x.Node

    member this.TransformThrowStatement (x: ThrowStatementData) : _ =
        let expression = x.Expression |> Option.map (this.TransformExpression)
        match expression with
        | Some e -> Throw e
        | None -> Throw (Var env.Caught.Value)
        |> withStatementSourcePos x.Node

    member this.TransformThrowExpression (x: ThrowExpressionData) : _ =
        let expression = x.Expression |> this.TransformExpression
        StatementExpr(Throw expression, None) 
        |> withExprSourcePos x.Node

    member this.TransformYieldStatement (x: YieldStatementData) : _ =
        let expression = x.Expression |> Option.map (this.TransformExpression)
        Yield expression
        |> withStatementSourcePos x.Node

    member this.TransformMakeRefExpression (x: MakeRefExpressionData) : _ =
        let expression = x.Expression |> this.TransformExpression
        TODO x

    member this.TransformRefTypeExpression (x: RefTypeExpressionData) : _ =
        let expression = x.Expression |> this.TransformExpression
        TODO x

    member this.TransformRefValueExpression (x: RefValueExpressionData) : _ =
        let expression = x.Expression |> this.TransformExpression
        let typ = x.Type |> this.TransformType
        TODO x

    member this.TransformDefaultExpression (x: DefaultExpressionData) : _ =
        let typ = env.SemanticModel.GetTypeInfo(x.Type.Node).Type |> sr.ReadType
        DefaultValueOf typ

    member this.TransformCastExpression (x: CastExpressionData) : _ =
        // TODO type check
//        let typ = env.SemanticModel.GetTypeInfo(x.Type.Node).Type |> sr.ReadType
        let expression = x.Expression |> this.TransformExpression
        let symbol = env.SemanticModel.GetSymbolInfo(x.Node).Symbol :?> IMethodSymbol
        if isNull symbol then 
            let fromTyp = env.SemanticModel.GetTypeInfo(x.Expression.Node).Type |> sr.ReadType
            let toTyp = env.SemanticModel.GetTypeInfo(x.Type.Node).Type |> sr.ReadType
            match fromTyp, toTyp with
            | ConcreteType { Generics = []; Entity = ft }, ConcreteType { Generics = []; Entity = tt } ->
                NumericConversion ft tt expression
            | _ ->
                Coerce(expression, fromTyp, toTyp)
        else
            call symbol None [ expression ]

     member this.TransformRefExpression (x: RefExpressionData) : _ =
        x.Expression |> this.TransformExpression |> createRef

    member this.TransformQueryExpression (x: QueryExpressionData) : _ =
        let expression = x.FromClause.Expression |> this.TransformExpression
        let symbol = env.SemanticModel.GetDeclaredSymbol(x.FromClause.Node)
        let id = Id.New symbol.Name
        env.RangeVars.Add(symbol, (id, None))
        let body = x.Body |> this.TransformQueryBody
        body (expression, Choice1Of2 symbol)

    member this.TransformQueryBody (x: QueryBodyData) : _ =
        let clauses = x.Clauses |> Seq.map this.TransformQueryClause |> List.ofSeq
        let selectOrGroup = x.SelectOrGroup |> this.TransformSelectOrGroupClause
        let continuation = x.Continuation |> Option.map this.TransformQueryContinuation
        match clauses with
        | [] -> id
        | _ -> List.reduce (>>) clauses 
        >> selectOrGroup
        >>  match continuation with
            | Some cont -> cont 
            | _ -> id

    member this.TransformQueryClause (x: QueryClauseData) : _ =
        match x with
        | QueryClauseData.FromClause    x -> this.TransformFromClause x
        | QueryClauseData.LetClause     x -> this.TransformLetClause x
        | QueryClauseData.WhereClause   x -> this.TransformWhereClause x
        | QueryClauseData.JoinClause    x -> this.TransformJoinClause x
        | QueryClauseData.OrderByClause x -> this.TransformOrderByClause x

    member this.TransformSelectOrGroupClause (x: SelectOrGroupClauseData) : _ =
        match x with
        | SelectOrGroupClauseData.SelectClause x -> this.TransformSelectClause x
        | SelectOrGroupClauseData.GroupClause  x -> this.TransformGroupClause x

    member this.TransformFromClause (x: FromClauseData) : _ =
        let querySymbol = (env.SemanticModel.GetQueryClauseInfo(x.Node).OperationInfo.Symbol :?> IMethodSymbol)
        fun (on, ri) ->
            let a, num =
                match ri with
                | Choice1Of2 ri ->
                    let a = Id.New()
                    env.RangeVars.[ri] <- (a, None) 
                    a, 2
                | Choice2Of2 (a, n, _) -> a, n + 1
            let expression = x.Expression |> this.TransformExpression
            let symbol = env.SemanticModel.GetDeclaredSymbol(x.Node)
            env.RangeVars.Add(symbol, (a, Some (num - 1)))
            let b = Id.New()
            let resSelect = 
                match ri with
                | Choice1Of2 ri ->                                        
                    env.RangeVars.[ri] <- (a, Some 0) 
                    Lambda([a; b], None, NewArray [Var a; Var b])
                | _ ->
                    Lambda([a; b], None, jsConcat (Var a) [NewArray [Var b]])
            let inclSelect expr =
                match ri with
                | Choice1Of2 ri ->
                    env.RangeVars.[ri] <- (a, None)
                | _ -> ()
                env.RangeVars.[symbol] <- (b, None)
                let newResSelect = Lambda([a; b], None, this.TransformExpression expr)     
                queryCall querySymbol [on; Lambda([a], None, expression); newResSelect]      
            queryCall querySymbol [on; Lambda([a], None, expression); resSelect], Choice2Of2 (a, num, Some inclSelect)    

    member this.TransformLetClause (x: LetClauseData) : _ =
        let querySymbol = (env.SemanticModel.GetQueryClauseInfo(x.Node).OperationInfo.Symbol :?> IMethodSymbol)
        fun (on, ri) ->
            let a, num =
                match ri with
                | Choice1Of2 ri ->
                    let a = Id.New()
                    env.RangeVars.[ri] <- (a, None) 
                    a, 2
                | Choice2Of2 (a, n, _) -> a, n + 1
            let expression = x.Expression |> this.TransformExpression
            let symbol = env.SemanticModel.GetDeclaredSymbol(x.Node)
            env.RangeVars.Add(symbol, (a, Some (num - 1)))
            let res = 
                match ri with
                | Choice1Of2 ri ->                                        
                    env.RangeVars.[ri] <- (a, Some 0) 
                    NewArray [Var a; expression]               
                | _ ->
                    jsConcat (Var a) [NewArray [expression]]  
            queryCall querySymbol [on; Lambda([a], None, res)], Choice2Of2 (a, num, None)    

    member this.TransformJoinClause (x: JoinClauseData) : _ =
        let querySymbol = (env.SemanticModel.GetQueryClauseInfo(x.Node).OperationInfo.Symbol :?> IMethodSymbol)
        let inExpression = x.InExpression |> this.TransformExpression
        fun (on, ri) ->
            let a, num =
                match ri with
                | Choice1Of2 ri ->
                    let a = Id.New()
                    env.RangeVars.[ri] <- (a, None) 
                    a, 2
                | Choice2Of2 (a, n, _) -> a, n + 1
            let leftExpression = x.LeftExpression |> this.TransformExpression
            let innerSymbol = env.SemanticModel.GetDeclaredSymbol(x.Node)
            let symbol = 
                match x.Into with
                | Some into -> env.SemanticModel.GetDeclaredSymbol(into.Node)
                | _ -> innerSymbol
            let b = Id.New()
            env.RangeVars.Add(innerSymbol, (b, None))
            let rightExpression = x.RightExpression |> this.TransformExpression
            env.RangeVars.[symbol] <- (a, Some (num - 1))
            let resSelect = 
                match ri with
                | Choice1Of2 ri ->                                        
                    env.RangeVars.[ri] <- (a, Some 0) 
                    Lambda([a; b], None, NewArray [Var a; Var b])
                | _ ->
                    Lambda([a; b], None, jsConcat (Var a) [NewArray [Var b]])
            let inclSelect expr =
                match ri with
                | Choice1Of2 ri ->
                    env.RangeVars.[ri] <- (a, None)
                | _ -> ()
                env.RangeVars.[symbol] <- (b, None)
                let newResSelect = Lambda([a; b], None, this.TransformExpression expr)     
                queryCall querySymbol [on; inExpression; Lambda([a], None, leftExpression); Lambda([b], None, rightExpression); newResSelect]
            queryCall querySymbol [on; inExpression; Lambda([a], None, leftExpression); Lambda([b], None, rightExpression); resSelect], Choice2Of2 (a, num, Some inclSelect)
    
    member this.TransformWhereClause (x: WhereClauseData) : _ =
        let querySymbol = (env.SemanticModel.GetQueryClauseInfo(x.Node).OperationInfo.Symbol :?> IMethodSymbol)
        fun (on, ri) ->
            let expression = x.Condition |> this.TransformExpression
            let a, ri =
                match ri with
                | Choice1Of2 ri -> fst env.RangeVars.[ri], Choice1Of2 ri
                | Choice2Of2 (a, n, _) -> a, Choice2Of2 (a, n, None)
            queryCall querySymbol [on; Lambda([a], None, expression)], ri

    member this.TransformOrderByClause (x: OrderByClauseData) : _ =
        let orderings = x.Orderings |> Seq.map this.TransformOrdering |> List.ofSeq
        List.reduce (>>) orderings

    member this.TransformOrdering (x: OrderingData) : _ =
        let querySymbol = (env.SemanticModel.GetSymbolInfo(x.Node).Symbol :?> IMethodSymbol)
        fun (on, ri) ->
            let expression = x.Expression |> this.TransformExpression
            let a, ri =
                match ri with
                | Choice1Of2 ri -> fst env.RangeVars.[ri], Choice1Of2 ri
                | Choice2Of2 (a, n, _) -> a, Choice2Of2 (a, n, None)
            queryCall querySymbol [on; Lambda([a], None, expression)], ri

    member this.TransformSelectClause (x: SelectClauseData) : _ =
        let symbol = env.SemanticModel.GetSymbolInfo(x.Node).Symbol :?> IMethodSymbol
        fun (on, ri) ->
            match ri with
            | Choice2Of2 (_, _, Some cont) ->
                cont x.Expression
            | _ ->
            let a =
                match ri with
                | Choice1Of2 ri -> fst env.RangeVars.[ri]
                | Choice2Of2 (a, _, _) -> a
            let expression = x.Expression |> this.TransformExpression
            if symbol = null then
                match IgnoreExprSourcePos expression with
                | Var v when v = a -> on
                | _ -> failwith "invalid query form at select clause"
            else
                queryCall symbol [on; Lambda([a], None, expression)]

    member this.TransformGroupClause (x: GroupClauseData) : _ =
        let symbol = env.SemanticModel.GetSymbolInfo(x.Node).Symbol :?> IMethodSymbol
        fun (on, ri) ->
            let groupExpression = x.GroupExpression |> this.TransformExpression
            let byExpression = x.ByExpression |> this.TransformExpression
            let a =
                match ri with
                | Choice1Of2 ri -> fst env.RangeVars.[ri]
                | Choice2Of2 (a, _, _) -> a
            queryCall symbol [on; Lambda([a], None, byExpression); Lambda([a], None, groupExpression)]

    member this.TransformQueryContinuation (x: QueryContinuationData) : _ =
        let symbol = env.SemanticModel.GetDeclaredSymbol(x.Node)
        let id = Id.New symbol.Name
        env.RangeVars.Add(symbol, (id, None))
        let body = x.Body |> this.TransformQueryBody
        fun expr -> body (expr, Choice1Of2 symbol)

    member this.TransformInterpolatedStringExpression (x: InterpolatedStringExpressionData) : _ =
        x.Contents |> Seq.map this.TransformInterpolatedStringContent 
        |> Seq.reduce (^+)

    member this.TransformDeclarationPattern (x: DeclarationPatternData) : (Id -> Expression) =
        let typ = env.SemanticModel.GetTypeInfo(x.Type.Node).Type |> sr.ReadType
        let designation = x.Designation |> this.TransformVariableDesignation
        let rTyp = env.SemanticModel.GetTypeInfo(x.Designation.Node).Type
        fun v -> 
            let c = Id.New("$c", mut = false)
            Let (c, 
                TypeCheck(Var v, typ), 
                Conditional(
                    Var c, 
                    Sequential [ this.PatternSet(designation, Var v, Some rTyp); Value (Bool true) ],
                    Value (Bool false)
                )
            )

    member this.TransformConstantPattern (x: ConstantPatternData) : (Id -> Expression) =
        let expression = x.Expression |> this.TransformExpression
        fun v -> Var v ^== expression

    member this.TransformDiscardPattern (x: DiscardPatternData) : _ =
        fun _ -> Value (Bool true)

    member this.TransformVarPattern (x: VarPatternData) : _ =
        let designation = x.Designation |> this.TransformVariableDesignation
        fun v ->
            Sequential [ this.PatternSet(designation, Var v, None); Value (Bool true) ]

    member this.TransformRecursivePattern (x: RecursivePatternData) : _ =
        //let typ = env.SemanticModel.GetTypeInfo(x.Type.Node).Type |> sr.ReadType
        let positionalPatternClause = x.PositionalPatternClause |> Option.map this.TransformPositionalPatternClause
        let propertyPatternClause = x.PropertyPatternClause |> Option.map this.TransformPropertyPatternClause
        let designation = x.Designation |> Option.map this.TransformVariableDesignation
        match positionalPatternClause with
        | Some ppc -> ppc
        | _ ->
            match propertyPatternClause with
            | Some ppc -> ppc
            | _ ->
                match designation with
                | Some d ->
                    let rTyp = env.SemanticModel.GetTypeInfo(x.Node).Type
                    fun v ->
                        Sequential [ this.PatternSet(d, Var v, Some rTyp); Value (Bool true) ]
                | _ ->
                    failwithf "Empty pattern unexpected: %s" (x.Node.ToString())

    member this.TransformPositionalPatternClause (x: PositionalPatternClauseData) : _ =
        //let symbol = env.SemanticModel.GetSymbolInfo(x.Node).Symbol
        let subpatterns = x.Subpatterns |> Seq.map this.TransformSubpattern |> List.ofSeq
        let len = List.length subpatterns
        let vars = List.init len (fun _ -> Id.New(mut = false))
        fun v ->
            Sequential [
                yield! vars |> List.mapi (fun i tv -> NewVar(tv, ItemGet(Var v, Value (Int i), Pure)))
                yield (
                    (subpatterns, vars) ||> List.map2 (fun p tv -> p tv) |> List.reduce (^&&)
                )
            ]

    member this.TransformPropertyPatternClause (x: PropertyPatternClauseData) : _ =
        let subpatterns = x.Subpatterns |> Seq.map this.TransformSubpattern |> List.ofSeq
        fun v ->
            subpatterns 
            |> List.map (fun p -> p v)
            |> List.reduce (^&&)

    member this.TransformSubpattern (x: SubpatternData) : _ =
        let pattern = x.Pattern |> this.TransformPattern
        match x.ExpressionColon with
        | None -> pattern
        | Some (BaseExpressionColonData.NameColon nc) ->
            let symbol = env.SemanticModel.GetSymbolInfo(nc.Name.Node).Symbol
            let id = Id.New(mut = false)
            if symbol.ContainingType.IsAnonymousType then
                fun v ->
                    Let (id, ItemGet(Var v, Value (String (symbol.Name)), NoSideEffect), pattern id)
            else
                match symbol with
                | :? IPropertySymbol as propSymbol ->
                    fun v ->
                        Let (id, call propSymbol.GetMethod (Some (Var v)) [], pattern id)
                            // TODO property indexers
                | :? IFieldSymbol as fieldSymbol ->
                    let typ = sr.ReadNamedType symbol.ContainingType
                    fun v ->
                        Let (id, FieldGet(Some (Var v), typ, fieldSymbol.Name), pattern id)
                | _ ->
                    failwith "Expecting field or property in recursive pattern"
        | Some (BaseExpressionColonData.ExpressionColon ec) ->
            let rec getSymbols (e: ExpressionData) acc =
                match e with
                | ExpressionData.MemberAccessExpression ma ->
                    let symbol = env.SemanticModel.GetSymbolInfo(ma.Name.Node).Symbol
                    getSymbols ma.Expression (symbol :: acc)
                | _ ->
                    let symbol = env.SemanticModel.GetSymbolInfo(e.Node).Symbol
                    symbol :: acc
            let symbols = getSymbols ec.Expression []
            fun v ->
                let value =
                    (Var v, symbols)
                    ||> List.fold (fun e symbol ->
                        if symbol.ContainingType.IsAnonymousType then
                            ItemGet(e, Value (String (symbol.Name)), NoSideEffect)
                        else
                            match symbol with
                            | :? IPropertySymbol as propSymbol ->
                                call propSymbol.GetMethod (Some e) []
                            | :? IFieldSymbol as fieldSymbol ->
                                let typ = sr.ReadNamedType symbol.ContainingType
                                FieldGet(Some e, typ, fieldSymbol.Name)
                            | _ ->
                                failwith "Expecting field or property in recursive pattern"
                    )
                let id = Id.New(mut = false)
                Let (id, value, pattern id)

    member this.TransformParenthesizedPattern (x: ParenthesizedPatternData) : _ =
        x.Pattern |> this.TransformPattern

    member this.TransformRelationalPattern (x: RelationalPatternData) : _ =
        let expression = x.Expression |> this.TransformExpression
        let op =
            match x.OperatorToken with
            | RelationalPatternOperatorToken.EqualsEqualsToken      -> BinaryOperator.EqualsOp    
            | RelationalPatternOperatorToken.ExclamationEqualsToken -> BinaryOperator.NotEquals
            | RelationalPatternOperatorToken.LessThanToken          -> BinaryOperator.Less
            | RelationalPatternOperatorToken.LessThanEqualsToken    -> BinaryOperator.LessOrEquals
            | RelationalPatternOperatorToken.GreaterThanToken       -> BinaryOperator.Greater
            | RelationalPatternOperatorToken.GreaterThanEqualsToken -> BinaryOperator.GreaterOrEquals
        fun v -> Binary(Var v, op, expression)

    member this.TransformTypePattern (x: TypePatternData) : _ =
        let typ = env.SemanticModel.GetTypeInfo(x.Type.Node).Type |> sr.ReadType
        fun v -> TypeCheck (Var v, typ)

    member this.TransformBinaryPattern (x: BinaryPatternData) : _ =
        let left = x.Left |> this.TransformPattern
        let right = x.Right |> this.TransformPattern
        match x.Kind with
        | BinaryPatternKind.OrPattern -> 
            fun v -> left v ^|| right v
        | BinaryPatternKind.AndPattern -> 
            fun v -> left v ^&& right v

    member this.TransformUnaryPattern (x: UnaryPatternData) : _ =
        let pattern = x.Pattern |> this.TransformPattern
        fun v -> Unary(UnaryOperator.``!``, pattern v)

    member this.TransformPattern (x: PatternData) : _ =
        match x with
        | PatternData.DiscardPattern       x -> this.TransformDiscardPattern x
        | PatternData.DeclarationPattern   x -> this.TransformDeclarationPattern x
        | PatternData.VarPattern           x -> this.TransformVarPattern x
        | PatternData.RecursivePattern     x -> this.TransformRecursivePattern x
        | PatternData.ConstantPattern      x -> this.TransformConstantPattern x
        | PatternData.ParenthesizedPattern x -> this.TransformParenthesizedPattern x
        | PatternData.RelationalPattern    x -> this.TransformRelationalPattern x
        | PatternData.TypePattern          x -> this.TransformTypePattern x
        | PatternData.BinaryPattern        x -> this.TransformBinaryPattern x
        | PatternData.UnaryPattern         x -> this.TransformUnaryPattern x

    member this.TransformIsPatternExpression (x: IsPatternExpressionData) : _ =
        let expression = x.Expression |> this.TransformExpression
        let pattern = x.Pattern |> this.TransformPattern
        let i = Id.New("$i", mut = false)
        Let (i, expression, pattern i)

    member this.TransformInterpolatedStringContent (x: InterpolatedStringContentData) : _ =
        match x with
        | InterpolatedStringContentData.InterpolatedStringText x -> 
            Value (String x.Node.TextToken.ValueText)
        | InterpolatedStringContentData.Interpolation x -> 
            let align =
                x.AlignmentClause |> Option.map (fun a ->
                    match getConstantValueOfExpression a.Value.Node with
                    | Value v -> "," + v.Value.ToString()
                    | _ -> failwith "Decimal not supported for string alignment"
                )
            let format = 
                x.FormatClause |> Option.map (fun f ->
                    ":" + f.Node.FormatStringToken.ValueText
                )
            let expr = x.Expression |> this.TransformExpression
            match align, format with
            | None, None -> expr
            | _ -> 
                let f = "{0" + Option.defaultValue "" align +  Option.defaultValue "" format + "}" 
                Call(None, NonGeneric Definitions.String, NonGeneric Definitions.StringFormat1, [ Value (String f); expr ])

    member this.TransformCheckedStatement (x: CheckedStatementData) : _ =
        this.TransformBlock x.Block

    member this.TransformCheckedExpression (x: CheckedExpressionData) : _ =
        this.TransformExpression x.Expression

    member this.TransformRangeExpression (x: RangeExpressionData) : _ =
        let leftOperand = x.LeftOperand |> Option.map this.TransformExpression
        let rightOperand = x.RightOperand |> Option.map this.TransformExpression
        TODO x

    member this.TransformSwitchExpression (x: SwitchExpressionData) : _ =
        let governingExpression = x.GoverningExpression |> this.TransformExpression
        let arms = x.Arms |> Seq.map this.TransformSwitchExpressionArm |> List.ofSeq
        let res = Id.New(mut = false)
        let sv = Id.New("$sv", mut = false)
        Let (sv, governingExpression, 
            let unmatched = StatementExpr(Throw (Value (String "Value is unmatched.")), None) 
            (arms, unmatched) 
            ||> List.foldBack (fun (pattern, body) rest ->
                Conditional (pattern sv, body, rest)
            )
        )

    member this.TransformSwitchExpressionArm (x: SwitchExpressionArmData) : _ =
        let pattern = x.Pattern |> this.TransformPattern
        let whenClause = x.WhenClause |> Option.map this.TransformWhenClause
        let expression = x.Expression |> this.TransformExpression
        let patternWithWhen =
            fun id ->
                match whenClause, pattern id with
                | Some we, Value (Bool true) -> we
                | Some we, pt -> pt ^&& we  
                | None, pt -> pt
        patternWithWhen, expression

    member this.TransformWithExpression (x: WithExpressionData) : _ =
        let expression = x.Expression |> this.TransformExpression
        let o = Id.New()
        let initializer = x.Initializer |> RoslynTransformer(env.WithInitializing(o)).TransformInitializerExpression
        Let(o, JSRuntime.Clone(expression), Sequential [initializer; Var o])

open System.Linq

let rec private isWebControlType (sr: SymbolReader) (c: INamedTypeSymbol) =
    let typ = sr.ReadNamedTypeDefinition c
    match typ.Value.FullName with
    | "WebSharper.Web.FSharpInlineControl"
    | "WebSharper.Web.InlineControl" ->
        false
    | _ ->
        match c.BaseType with
        | null -> false
        | bCls ->
            let bTyp = sr.ReadNamedTypeDefinition bCls
            bTyp.Value.FullName = "WebSharper.Web.Control" || isWebControlType sr bCls

// Searches for calls within server-side code to method with JavaScript-enabled parameters.
let scanExpression (env: Environment) (node: SyntaxNode) =

    let getTypeAndMethod (symbol: IMethodSymbol) =
        let symbol =
            match symbol.OverriddenMethod with
            | null -> symbol
            | symbol -> symbol
        let eSymbol =
            match symbol.ReducedFrom with
            | null -> symbol
            | symbol -> symbol
        let typ = env.SymbolReader.ReadNamedType eSymbol.ContainingType
        let ma = 
            (symbol.TypeArguments, eSymbol.TypeParameters) 
            ||> Seq.map2 (fun a p -> if a.Equals p then dynTyp else env.SymbolReader.ReadType a) 
            |> List.ofSeq
        let meth = Generic (env.SymbolReader.ReadMethod eSymbol) ma
        typ.Entity, meth.Entity

    for n in node.DescendantNodes() do
        
        let checkQuotedArgs indexes (args: ArgumentListSyntax) =
            args.Arguments |> Seq.iteri (fun i a -> 
                if indexes |> Array.contains i then
                    match a.Expression with
                    | :? LambdaExpressionSyntax as e ->
                        match e.Body with
                        | :? InvocationExpressionSyntax as e ->
                            let esymbol = env.SemanticModel.GetSymbolInfo(e).Symbol :?> IMethodSymbol
                            if not (isNull esymbol) then
                                let etyp, emeth = getTypeAndMethod esymbol
                                env.Compilation.AddQuotedMethod(etyp, emeth, [])
                            let pos = getSourcePos e
                            let argTypes = esymbol.Parameters |> Seq.map (fun p -> env.SymbolReader.ReadType p.Type)
                            for t in argTypes do
                                if t.CanHaveDeserializer then
                                    env.Compilation.AddTypeNeedingDeserialization(t, pos, [])
                        | :? IdentifierNameSyntax as e ->
                            let esymbol = env.SemanticModel.GetSymbolInfo(e).Symbol :?> IPropertySymbol
                            if not (isNull esymbol) then
                                let etyp, emeth = getTypeAndMethod esymbol.GetMethod
                                env.Compilation.AddQuotedMethod(etyp, emeth, [])
                        | e -> 
                            failwithf "Unexpected form in Client-side LINQ lambda body: %s" (e.ToString())

                    | :? MemberAccessExpressionSyntax as e ->
                        let esymbol = env.SemanticModel.GetSymbolInfo(e).Symbol :?> IMethodSymbol
                        if not (isNull esymbol) then
                            let etyp, emeth = getTypeAndMethod esymbol
                            env.Compilation.AddQuotedMethod(etyp, emeth, [])
                        let pos = getSourcePos e
                        let argTypes = esymbol.Parameters |> Seq.map (fun p -> env.SymbolReader.ReadType p.Type)
                        for t in argTypes do
                            if t.CanHaveDeserializer then
                                env.Compilation.AddTypeNeedingDeserialization(t, pos, [])

                    | e -> failwithf "Unexpected form in Client-side LINQ expression: %s" (e.ToString())
            )
        
        match n with 
        | :? InvocationExpressionSyntax as n ->
            let symbol = env.SemanticModel.GetSymbolInfo(n).Symbol :?> IMethodSymbol
            
            if not (isNull symbol) then
                let typ = env.SymbolReader.ReadNamedTypeDefinition symbol.ContainingType
                let meth = env.SymbolReader.ReadMethod symbol
                //failwithf "Found InvocationExpression: %s.%s" typ.Value.FullName meth.Value.MethodName
                match env.Compilation.TryLookupQuotedArgMethod(typ, meth) with
                | Some indexes ->
                    checkQuotedArgs indexes n.ArgumentList
                | _ -> ()        
        | :? ObjectCreationExpressionSyntax as n ->
            let symbol = env.SemanticModel.GetSymbolInfo(n).Symbol :?> IMethodSymbol

            if not (isNull symbol) then
                if isWebControlType env.SymbolReader symbol.ContainingType then
                    let typ = env.SymbolReader.ReadType symbol.ContainingType
                    let pos = getSourcePos n
                    env.Compilation.AddWebControl(typ, pos, []) // TODO C# bundles
                else
                    let typ = env.SymbolReader.ReadNamedTypeDefinition symbol.ContainingType
                    let ctor = env.SymbolReader.ReadConstructor symbol
                    //failwithf "Found ObjectCreationExpression: %s" typ.Value.FullName
                    match env.Compilation.TryLookupQuotedConstArgMethod(typ, ctor) with
                    | Some indexes ->
                        checkQuotedArgs indexes n.ArgumentList
                    | _ -> ()        
        | _ -> ()