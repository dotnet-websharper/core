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

module Seq =
    open System.Linq

    let inline equals (s1: _ seq) (s2: _ seq) = s1.SequenceEqual(s2)

type CSharpMethod =
    {
        IsStatic : bool
        Parameters : list<CSharpParameter>
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

let textSpans =
    System.Runtime.CompilerServices.ConditionalWeakTable()

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
    textSpans.Add(res, ref ts)
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
    failwithf "TODO: handle %sSyntax" (xn.[.. xn.Length - 5])  

let inline NotSupported x = 
    failwithf "Syntax not supported for JavaScript: %s" x

module M = Metadata

type SymbolReader(comp : WebSharper.Compiler.Compilation) as self =

    let attrReader =
        { new A.AttributeReader<Microsoft.CodeAnalysis.AttributeData>() with
            override this.GetAssemblyName attr = attr.AttributeClass.ContainingAssembly.Name
            override this.GetName attr = attr.AttributeClass.Name
            override this.GetCtorArgs attr = attr.ConstructorArguments |> Seq.map (fun a -> a.Value) |> Array.ofSeq          
            override this.GetTypeDef o = self.ReadNamedTypeDefinition (o :?> INamedTypeSymbol)
        }

    member this.RegisterCustomType def (td: INamedTypeSymbol) =
        if not (comp.HasCustomTypeInfo def) then
            let inv = td.DelegateInvokeMethod
            if not (isNull inv) then
                let info =
                    M.DelegateInfo {
                        DelegateArgs =                                
                            inv.Parameters |> Seq.map (fun p -> this.ReadType p.Type) |> List.ofSeq
                        ReturnType = this.ReadType inv.ReturnType
                    }
                comp.AddCustomType(def, info)

    member this.ReadNamedTypeDefinition (x: INamedTypeSymbol) =
        let arity (symbol: INamedTypeSymbol) =
            match symbol.Arity with 0 -> "" | a -> "`" + string a    

        let rec getNamespaceOrTypeAddress acc (symbol: INamespaceOrTypeSymbol) =
            match symbol.ContainingNamespace with
            | null -> acc |> String.concat "."
            | ns -> getNamespaceOrTypeAddress (symbol.Name :: acc) ns   
    
        let rec getTypeAddress acc (symbol: INamedTypeSymbol) =
            match symbol.ContainingType with
            | null -> 
                let ns = getNamespaceOrTypeAddress [] symbol + arity symbol
                if List.isEmpty acc then ns else
                    ns :: acc |> String.concat "+" 
            | t -> getTypeAddress (symbol.Name + arity symbol :: acc) t           

        let res =
            Hashed {
                Assembly = x.ContainingAssembly.Identity.Name
                FullName = getTypeAddress [] x //) + arity x
            }

        this.RegisterCustomType res x

        res

    member this.ReadNamedType (x: INamedTypeSymbol) = 
        if isNull x then
            // TODO: handle dynamic by cheking symbol.ContainingSymbol before calling sr.ReadNamedType
            NonGeneric Definitions.Dynamic     
        else
        let ta = x.TypeArguments |> Seq.map this.ReadType |> List.ofSeq
        let td = this.ReadNamedTypeDefinition x
        Generic td ta
    
    member this.RecognizeNamedType (x: INamedTypeSymbol) =
        let ta = x.TypeArguments |> Seq.map this.ReadType |> List.ofSeq
        let td = this.ReadNamedTypeDefinition x
        let tName = td.Value.FullName
        if tName.StartsWith "System.Tuple" then
            if tName.EndsWith "8" then
                match ta.[7] with
                | TupleType rest -> TupleType (ta.[.. 6] @ rest)
                | _ -> failwith "invalid big tuple type" 
            else TupleType ta
        elif tName = "Microsoft.FSharp.Core.FSharpFunc`2" then
            match ta with
            | [a; r] -> FSharpFuncType(a, r)
            | _ -> failwith "impossible"
        elif tName = "Microsoft.FSharp.Core.Unit" || tName = "System.Void" then
            VoidType
        else
            GenericType td ta

    member this.ReadType (x: ITypeSymbol) : Type =
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
            ConcreteType (NonGeneric Definitions.Obj)    
        | TypeKind.TypeParameter ->
            let t = x :?> ITypeParameterSymbol 
            if t.TypeParameterKind = TypeParameterKind.Method then
                TypeParameter (t.Ordinal + t.DeclaringMethod.ContainingType.TypeParameters.Length)
            else 
                TypeParameter t.Ordinal        
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
            Parameters = x.Parameters |> Seq.map (fun p -> this.ReadType p.Type) |> List.ofSeq
            ReturnType = x.ReturnType |> this.ReadType
            Generics = x.Arity
        }

    member this.ReadConstructor (x: IMethodSymbol) =
        Hashed {
            CtorParameters = x.Parameters |> Seq.map (fun p -> this.ReadType p.Type) |> List.ofSeq
        }

    member this.ReadMember (x: IMethodSymbol) =
        let name = x.Name
        match name with
        | ".ctor" ->
            Member.Constructor <| Hashed {
                CtorParameters = x.Parameters |> Seq.map (fun p -> this.ReadType p.Type) |> List.ofSeq
            }
        | ".cctor" -> Member.StaticConstructor
        | _ ->
            let getMeth (x: IMethodSymbol) =
                Hashed {
                    MethodName = x.Name
                    Parameters = x.Parameters |> Seq.map (fun p -> this.ReadType p.Type) |> List.ofSeq
                    ReturnType = x.ReturnType |> this.ReadType
                    Generics = x.Arity
                }
            if x.IsOverride then
                let o = x.OverriddenMethod.OriginalDefinition
                Member.Override(this.ReadNamedTypeDefinition o.ContainingType, getMeth o)
            elif x.ExplicitInterfaceImplementations.Length > 0 then
                let o = x.ExplicitInterfaceImplementations.[0].OriginalDefinition
                Member.Implementation(this.ReadNamedTypeDefinition o.ContainingType, getMeth o)
            else
                let o = x.OriginalDefinition
                Member.Method (not o.IsStatic, getMeth o)

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

        let id = Id.New x.Name
        {
            ParameterId = id
            Symbol = x
            DefaultValue = defValue
            Type = typ
            RefOrOut = x.RefKind <> RefKind.None
        }

    member this.ReadParameters (x: IMethodSymbol) =
        x.Parameters |> Seq.map this.ReadParameter |> List.ofSeq   

    //let hasSignature (comp: WebSharper.Compiler.Compilation) (s: Method) (x: IMethodSymbol) =
    //    let s = s.Value
    //    x.Arity = s.Generics
    //    && (x.ReturnType |> sr.ReadType) = s.ReturnType
    //    && x.Parameters |> Seq.map (fun p -> sr.ReadType p.Type) |> Seq.equals s.Parameters     
  
    member this.AttributeReader = attrReader

type Environment =
    {
        SemanticModel : SemanticModel 
        Vars : IDictionary<ILocalSymbol, Id>
        Parameters : IDictionary<IParameterSymbol, Id * bool>
        Labels : IDictionary<ILabelSymbol, Id>
        Caught : option<Id>
        Conditional : option<Id>
        Initializing : option<Id>
        Compilation : WebSharper.Compiler.Compilation
        SymbolReader : SymbolReader
        RangeVars : IDictionary<IRangeVariableSymbol, Id * option<int>>
    }
    static member New(model, comp, sr) = 
        { 
            SemanticModel = model
            Vars = Dictionary()
            Parameters = Dictionary()
            Labels = Dictionary()
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

    member this.WithCaught(c) =
        { this with Caught = Some c }

    member this.WithConditional(c) =
        { this with Conditional = Some c }

    member this.WithInitializing(i) =
        { this with Initializing = Some i }

    member this.WithNotInitializing() =
        { this with Initializing = None }

type RoslynTransformer(env: Environment) = 
    let getConstantValueOfExpression x =
        env.SemanticModel.GetConstantValue(x).Value
        |> ReadLiteral |> Value

    let sr = env.SymbolReader

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
        Application(ItemGet(expr, Value (String "concat")), args, true, None)

    let queryCall (symbol: IMethodSymbol) args =
        let qtyp = sr.ReadNamedType symbol.ContainingType
        let ma = symbol.TypeArguments |> Seq.map (sr.ReadType) |> List.ofSeq
        let meth = Generic (sr.ReadMethod symbol.ReducedFrom) ma
        Call (None, qtyp, meth, args)

    let getTypeAndMethod (symbol: IMethodSymbol) =
        let typ = sr.ReadNamedType symbol.ContainingType
        let ma = symbol.TypeArguments |> Seq.map (sr.ReadType) |> List.ofSeq
        let meth = Generic (sr.ReadMethod symbol) ma
        typ, meth

    let call (symbol: IMethodSymbol) thisOpt args =
        let typ, meth = getTypeAndMethod symbol
        Call(thisOpt, typ, meth, args)

    member this.TransformIdentifierName (x: IdentifierNameData) : Expression =
        let symbol = env.SemanticModel.GetSymbolInfo(x.Node).Symbol
        let getTarget() =
            match env.Initializing with
            | Some i -> Var i
            | _ -> This
            |> Some
        match symbol with
        | :? ILocalSymbol as s -> Var env.Vars.[s]
        | :? IParameterSymbol as p -> 
            match env.Parameters.[p] with
            | v, false -> Var v
            | v, true -> GetRef (Var v) 
        | :? IRangeVariableSymbol as v ->
            match env.RangeVars.[v] with
            | v, None -> Var v
            | v, Some i -> ItemGet(Var v, Value (Int i))
        | :? IFieldSymbol as f -> FieldGet((getTarget()), sr.ReadNamedType f.ContainingType, f.Name) 
        | :? IEventSymbol as e -> FieldGet((getTarget()), sr.ReadNamedType e.ContainingType, e.Name)
        | :? IPropertySymbol as p ->
            call p.GetMethod (getTarget()) [] // TODO: indexed properties?
        | :? IMethodSymbol as m -> 
            let conv = env.SemanticModel.GetConversion(x.Node)
            if not conv.Exists || conv.IsIdentity then This
            elif conv.IsMethodGroup then
                let typ, meth = getTypeAndMethod m
                NewDelegate(Some This, typ, meth)
            else failwithf "transformIdentifierName: unhandled IMethodSymbol conversion: %A" conv 
        | null -> 
            err x.Node "transformIdentifierName: Symbol is null"
        | _ -> 
            err x.Node (sprintf "transformIdentifierName: Local variable not found, symbol type: %s" 
                (symbol.GetType().FullName))

    member this.TransformExpression (x: ExpressionData) : Expression =
        try
            match x with
            | ExpressionData.Type                              x -> this.TransformType x
            | ExpressionData.InstanceExpression                x -> this.TransformInstanceExpression x
            | ExpressionData.AnonymousFunctionExpression       x -> this.TransformAnonymousFunctionExpression x
            | ExpressionData.ParenthesizedExpression           x -> this.TransformParenthesizedExpression x
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
            | ExpressionData.CheckedExpression                 _ -> NotSupported "checked"
            | ExpressionData.DefaultExpression                 x -> this.TransformDefaultExpression x
            | ExpressionData.TypeOfExpression                  _ -> NotSupported "typeof"
            | ExpressionData.SizeOfExpression                  _ -> NotSupported "sizeof"
            | ExpressionData.InvocationExpression              x -> this.TransformInvocationExpression x
            | ExpressionData.ElementAccessExpression           x -> this.TransformElementAccessExpression x
            | ExpressionData.CastExpression                    x -> this.TransformCastExpression x
            | ExpressionData.InitializerExpression             x -> this.TransformInitializerExpression x
            | ExpressionData.ObjectCreationExpression          x -> this.TransformObjectCreationExpression x
            | ExpressionData.AnonymousObjectCreationExpression x -> this.TransformAnonymousObjectCreationExpression x
            | ExpressionData.ArrayCreationExpression           x -> this.TransformArrayCreationExpression x
            | ExpressionData.ImplicitArrayCreationExpression   x -> this.TransformImplicitArrayCreationExpression x
            | ExpressionData.StackAllocArrayCreationExpression _ -> NotSupported "stackalloc"
            | ExpressionData.QueryExpression                   x -> this.TransformQueryExpression x
            | ExpressionData.OmittedArraySizeExpression        x -> failwith "not a general expression: OmittedArraySizeExpression"
            | ExpressionData.InterpolatedStringExpression      x -> this.TransformInterpolatedStringExpression x      
            |> fun expr ->
                let conversion = env.SemanticModel.GetConversion(x.Node)
                if conversion.IsUserDefined then
                    let symbol = conversion.MethodSymbol
                    let typ = sr.ReadNamedType symbol.ContainingType
                    let ma = symbol.TypeArguments |> Seq.map (sr.ReadType) |> List.ofSeq
                    let meth = Generic (sr.ReadMethod symbol) ma
                    Call(None, typ, meth, [ expr ])
                else expr
        with e ->
            env.Compilation.AddError(Some (getSourcePos x.Node), WebSharper.Compiler.SourceError("Error while reading C# code: " + e.Message + " at " + e.StackTrace))
            WebSharper.Compiler.Translator.errorPlaceholder       

    member this.TransformType (x: TypeData) : Expression =
        match x with
        | TypeData.Name                x -> this.TransformName x
        | TypeData.PredefinedType      x -> TODO x //this.TransformPredefinedType x
        | TypeData.ArrayType           x -> TODO x //this.TransformArrayType x
        | TypeData.PointerType         x -> TODO x //this.TransformPointerType x
        | TypeData.NullableType        x -> TODO x //this.TransformNullableType x
        | TypeData.OmittedTypeArgument x -> TODO x //this.TransformOmittedTypeArgument x

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
        let eSymbol, isExtensionMethod =
            match symbol.ReducedFrom with
            | null -> symbol, false
            | symbol -> symbol, true
        let typ = sr.ReadNamedType eSymbol.ContainingType
        let ma = symbol.TypeArguments |> Seq.map sr.ReadType |> List.ofSeq
        let meth = Generic (sr.ReadMethod eSymbol) ma
        let argumentList = x.ArgumentList |> this.TransformArgumentList
        let argumentListWithParamsFix = fixParamArray eSymbol x.ArgumentList argumentList
        let argumentListWithThis =
            if isExtensionMethod || not symbol.IsStatic then
                (None, (x.Expression |> this.TransformExpression)) 
                :: (argumentListWithParamsFix |> List.map (fun (i, e) -> i |> Option.map ((+) 1), e))   
            else argumentListWithParamsFix 

        let tempVars, args = readReorderedParams argumentListWithThis

        if isExtensionMethod || symbol.IsStatic then
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
            List.fold (fun e a -> ItemGet(e, a)) expression argumentList
        else
            call symbol.GetMethod (Some expression) argumentList

    member this.TransformElementAccessExpression (x: ElementAccessExpressionData) : _ =
        this.TransformElementAccess(x.Node, Some x.Expression, x.ArgumentList)

    member this.TransformImplicitElementAccess (x: ImplicitElementAccessData) : _ =
        this.TransformElementAccess(x.Node, None, x.ArgumentList)

    member this.TransformArgument (x: ArgumentData) : option<int> * Expression =
        let namedParamOrdinal = 
            x.NameColon |> Option.map (fun nc ->
                let symbol = env.SemanticModel.GetSymbolInfo(nc.Name.Node).Symbol :?> IParameterSymbol
                symbol.Ordinal
            )
        let expression = x.Expression |> this.TransformExpression
        let refOrOut =
            match x.RefOrOutKeyword with
            | Some ArgumentRefOrOutKeyword.RefKeyword 
            | Some ArgumentRefOrOutKeyword.OutKeyword ->
                let e = IgnoreExprSourcePos expression
                match e with
                | Var v ->
                    MakeRef e (fun value -> VarSet(v, value))
                | ItemGet(o, i) ->
                    let ov = Id.New ()
                    let iv = Id.New ()
                    Let (ov, o, Let(iv, i, MakeRef (ItemGet(Var ov, Var iv)) (fun value -> ItemSet(Var ov, Var iv, value))))
                | FieldGet(o, t, f) ->
                    match o with
                    | Some o ->
                        let ov = Id.New ()
                        Let (ov, o, MakeRef (FieldGet(Some (Var ov), t, f)) (fun value -> FieldSet(Some (Var ov), t, f, value)))     
                    | _ ->
                        MakeRef e (fun value -> FieldSet(None, t, f, value))  
                | Application(ItemGet (r, Value (String "get")), [], _, _) ->
                    r
                | Call (thisOpt, typ, getter, args) ->
                    MakeRef e (fun value -> (Call (thisOpt, typ, setterOf getter, args @ [value])))
                | _ -> failwithf "ref argument has unexpected form: %+A" e     
            | None -> expression
        namedParamOrdinal, refOrOut

    member this.TransformArgumentList (x: ArgumentListData) =
        x.Arguments |> Seq.map this.TransformArgument |> List.ofSeq

    member this.TransformBracketedArgumentList (x: BracketedArgumentListData) =
        x.Arguments |> Seq.map this.TransformArgument |> Seq.map snd |> List.ofSeq
 
    member this.TransformLiteralExpression (x: LiteralExpressionData) =
        getConstantValueOfExpression x.Node

    member this.TransformStatement (x: StatementData) : Statement =
        try
            match x with
            | StatementData.Block                     x -> this.TransformBlock x
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
            | StatementData.ForEachStatement          x -> this.TransformForEachStatement x
            | StatementData.UsingStatement            x -> this.TransformUsingStatement x
            | StatementData.FixedStatement            _ -> NotSupported "fixed"
            | StatementData.CheckedStatement          _ -> NotSupported "checked"
            | StatementData.UnsafeStatement           _ -> NotSupported "unsafe"
            | StatementData.LockStatement             _ -> NotSupported "lock"
            | StatementData.IfStatement               x -> this.TransformIfStatement x
            | StatementData.SwitchStatement           x -> this.TransformSwitchStatement x
            | StatementData.TryStatement              x -> this.TransformTryStatement x
        with e ->
            env.Compilation.AddError(Some (getSourcePos x.Node), WebSharper.Compiler.SourceError("Error while reading C# code: " + e.Message + " at " + e.StackTrace))
            ExprStatement WebSharper.Compiler.Translator.errorPlaceholder        

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

    member this.TransformLocalDeclarationStatement (x: LocalDeclarationStatementData) : Statement =
        x.Declaration |> this.TransformVariableDeclaration |> List.map VarDeclaration |> Block 
        |> withStatementSourcePos x.Node

    member this.TransformExpressionStatement (x: ExpressionStatementData) : Statement =
        x.Expression |> this.TransformExpression |> ExprStatement

    member this.TransformVariableDeclarator (x: VariableDeclaratorData) : Id * Expression =    
        let symbol = env.SemanticModel.GetDeclaredSymbol(x.Node) //:?> ILocalSymbol
        let id = 
            match symbol with
            | :? ILocalSymbol as s ->
                let id = Id.New(s.Name)
                env.Vars.Add(s, id)
                id 
            | :? IFieldSymbol as s ->
                Id.New(s.Name)
            | _ -> failwithf "this.TransformVariableDeclarator: invalid symbol type %A" symbol
        let initializer = 
            match x.Initializer with
            | Some i -> i |> this.TransformEqualsValueClause
            | _ -> Undefined
        id, initializer

    member this.TransformSwitchStatement (x: SwitchStatementData) : Statement =
        let expression = x.Expression |> this.TransformExpression
        let sections = x.Sections |> Seq.map (this.TransformSwitchSection) |> List.ofSeq
        CSharpSwitch (expression, sections)
        |> withStatementSourcePos x.Node

    member this.TransformSwitchSection (x: SwitchSectionData) : list<option<Expression>> * Statement =
        let labels = x.Labels |> Seq.map (this.TransformSwitchLabel) |> List.ofSeq
        let statements = x.Statements |> Seq.map (this.TransformStatement) |> List.ofSeq |> CombineStatements
        labels, statements

    member this.TransformSwitchLabel (x: SwitchLabelData) : option<Expression> =
        match x with
        | SwitchLabelData.CaseSwitchLabel    x -> this.TransformCaseSwitchLabel x |> Some
        | SwitchLabelData.DefaultSwitchLabel x -> None //this.TransformDefaultSwitchLabel env x

    member this.TransformCaseSwitchLabel (x: CaseSwitchLabelData) : Expression =
        x.Value |> this.TransformExpression
        |> withExprSourcePos x.Node

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
        let leftSymbol = env.SemanticModel.GetSymbolInfo(x.Left.Node).Symbol
        let trR =
            if Option.isSome env.Initializing then 
                RoslynTransformer(env.WithNotInitializing())
            else this
        match leftSymbol with
        | :? IPropertySymbol as leftSymbol ->
            let typ, setter = getTypeAndMethod leftSymbol.SetMethod
            //if leftSymbol.IsIndexer // TODO property indexers
            
            match x.Kind with
            | AssignmentExpressionKind.SimpleAssignmentExpression ->
                let right = x.Right |> trR.TransformExpression
                if leftSymbol.IsStatic then
                    Call(None, typ, setter, [right]) 
                else
                    let left = x.Left |> this.TransformExpression
                    // eliminate getter
                    match IgnoreExprSourcePos left with
                    | Call (Some v, _, _, args) ->
                        Call (Some v, typ, setter, args @ [right])
                    | _ -> failwithf "this.TransformAssignmentExpression: getter expression not recognized for creating assigment"
            | _ ->
                let left = x.Left |> this.TransformExpression
                let right = x.Right |> trR.TransformExpression
                let symbol = env.SemanticModel.GetSymbolInfo(x.Node).Symbol :?> IMethodSymbol
                let opTyp, operator = getTypeAndMethod symbol
                if leftSymbol.IsStatic then
                    Call(None, typ, setter, [Call(None, opTyp, operator, [left; right])]) 
                else
                    let left = x.Left |> this.TransformExpression
                    // eliminate getter
                    match IgnoreExprSourcePos left with
                    | Call (Some v, _, getter, []) ->
                        let m = Id.New ()
                        let leftWithM = Call (Some (Var m), typ, getter, [])
                        Let (m, v, Call (Some (Var m), typ, setter, [Call(None, opTyp, operator, [leftWithM; right])])) 
                    | _ -> failwith "this.TransformAssignmentExpression: getter expression not recognized for creating compound assigment"
        | _ ->
        let left = x.Left |> this.TransformExpression
        let right = x.Right |> trR.TransformExpression
        match x.Kind with
        | AssignmentExpressionKind.SimpleAssignmentExpression ->
            match IgnoreExprSourcePos left with
            | Var id -> VarSet(id, right)
            | FieldGet (obj, ty, f) -> FieldSet (obj, ty, f, right)
            | ItemGet(obj, i) -> ItemSet (obj, i, right)
            | Application(ItemGet (r, Value (String "get")), [], _, _) ->
                SetRef r right
            | Call (thisOpt, typ, getter, args) ->
                Call (thisOpt, typ, setterOf getter, args @ [right])
            | _ -> TODO x
        | _ ->
            let symbol = env.SemanticModel.GetSymbolInfo(x.Node).Symbol :?> IMethodSymbol
            let opTyp, operator = getTypeAndMethod symbol
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
            | ItemGet(obj, i) ->
                let m = Id.New ()
                let j = Id.New ()
                let leftWithM = ItemGet (Var m, Var j)
                Let (m, obj, Let (j, i, ItemSet(Var m, Var j, Call(None, opTyp, operator, [leftWithM; right]))))
            | Application(ItemGet (r, Value (String "get")), [], _, _) ->
                SetRef r (Call(None, opTyp, operator, [left; right]))
            | Call (thisOpt, typ, getter, args) ->
                Call (thisOpt, typ, setterOf getter, args @ [Call(None, opTyp, operator, [left; right])])
            | _ -> TODO x
            
        |> withExprSourcePos x.Node

    member this.TransformParenthesizedExpression (x: ParenthesizedExpressionData) : Expression =
        x.Expression |> this.TransformExpression

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
            let symbol = env.SemanticModel.GetSymbolInfo(x.Node).Symbol :?> IMethodSymbol
            call symbol None [left; right]
        |> withExprSourcePos x.Node

    member this.TransformConditionalExpression (x: ConditionalExpressionData) : Expression =
        let condition = x.Condition |> this.TransformExpression
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
            Body = body
            IsAsync = symbol.IsAsync
            ReturnType = returnType
        }

    member this.TransformMethodDeclaration (x: MethodDeclarationData) : CSharpMethod =
        let symbol = env.SemanticModel.GetDeclaredSymbol(x.Node)
        let parameterList = x.ParameterList |> this.TransformParameterList
        this.TransformMethodDeclarationBase(symbol, parameterList, x.Body, x.ExpressionBody)

    member this.TransformOperatorDeclaration (x: OperatorDeclarationData) : CSharpMethod =
        let symbol = env.SemanticModel.GetDeclaredSymbol(x.Node)
        let parameterList = x.ParameterList |> this.TransformParameterList
        this.TransformMethodDeclarationBase(symbol, parameterList, x.Body, x.ExpressionBody)

    member this.TransformConversionOperatorDeclaration (x: ConversionOperatorDeclarationData) : CSharpMethod =
        let symbol = env.SemanticModel.GetDeclaredSymbol(x.Node)
        let parameterList = x.ParameterList |> this.TransformParameterList
        this.TransformMethodDeclarationBase(symbol, parameterList, x.Body, x.ExpressionBody)

    member this.TransformParameterList (x: ParameterListData) : list<CSharpParameter> =
        x.Parameters |> Seq.map this.TransformParameter |> List.ofSeq

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
            let id = Id.New t
            {
                ParameterId = id
                Symbol = symbol
                DefaultValue = defValue
                Type = typ
                RefOrOut = symbol.RefKind <> RefKind.None
            }
        | ParameterIdentifier.ArgListKeyword -> NotSupported "__arglist"

    member this.TransformArrowExpressionClause (x: ArrowExpressionClauseData) : Expression =
        x.Expression |> this.TransformExpression

    member this.TransformReturnStatement (x: ReturnStatementData) : Statement =
        defaultArg (x.Expression |> Option.map (this.TransformExpression)) Undefined |> Return

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

    member this.TransformInitializerExpression (x: InitializerExpressionData) : _ =
        let expressions = x.Expressions |> Seq.map (this.TransformExpression) |> List.ofSeq
        match x.Kind with
        | InitializerExpressionKind.ObjectInitializerExpression -> 
            Sequential expressions
        | InitializerExpressionKind.CollectionInitializerExpression -> 
            let addSymbol =
                env.SemanticModel.GetCollectionInitializerSymbolInfo(x.Node).Symbol :?> IMethodSymbol
            if isNull addSymbol then
                // for some reason GetCollectionInitializerSymbolInfo does not always returns the .Add method
                // then we try to look it up on the type by name and arity
                let cSymbol = env.SemanticModel.GetSymbolInfo(x.Node.Parent).Symbol :?> IMethodSymbol
                let cTyp = sr.ReadNamedType cSymbol.ContainingType
                let addMethods =
                    cSymbol.ContainingType.GetMembers("Add").OfType<IMethodSymbol>()
                let addM i = 
                    let candidates = 
                        addMethods
                        |> Seq.filter (fun m -> m.Parameters.Length = i) 
                        |> Array.ofSeq
                    if candidates.Length > 1 then 
                        failwith "Could not look up unique Add method on collection initialization"
                    elif candidates.Length = 0 then
                        failwithf "Add method with %d parameters not found for collection initialization" i
                    else
                        candidates.[0] |> sr.ReadMethod |> NonGeneric
                expressions |> List.map (fun item -> 
                    match IgnoreExprSourcePos item with
                    | ComplexElement cItem ->
                        Call(Some (Var env.Initializing.Value), cTyp, addM (List.length cItem), cItem)
                    | _ -> Call(Some (Var env.Initializing.Value), cTyp, addM 1, [item])
                ) |> Sequential
            else
                let cTyp, addM = getTypeAndMethod addSymbol
                expressions |> List.map (fun item -> 
                    match IgnoreExprSourcePos item with
                    | ComplexElement cItem ->
                        Call(Some (Var env.Initializing.Value), cTyp, addM, cItem)
                    | _ -> Call(Some (Var env.Initializing.Value), cTyp, addM, [item])
                ) |> Sequential
        | InitializerExpressionKind.ArrayInitializerExpression ->
            // TODO: 2-dimensional
            NewArray expressions
        | InitializerExpressionKind.ComplexElementInitializerExpression -> 
            ComplexElement expressions
        |> withExprSourcePos x.Node

    member this.TransformAnonymousObjectCreationExpression (x: AnonymousObjectCreationExpressionData) : _ =
        x.Initializers |> Seq.map this.TransformAnonymousObjectMemberDeclarator |> List.ofSeq
        |> Object

    member this.TransformAnonymousObjectMemberDeclarator (x: AnonymousObjectMemberDeclaratorData) : string * Expression =
        let expression = x.Expression |> this.TransformExpression
        let identifierName =
            match x.NameEquals with
            | Some n -> n.Name.Node.Identifier.Text
            | None -> (x.Expression.Node :?> IdentifierNameSyntax).Identifier.Text
        identifierName, expression

    member this.TransformConstructorDeclaration (x: ConstructorDeclarationData) : _ =
        let parameterList = x.ParameterList |> this.TransformParameterList
        for p in parameterList do
            env.Parameters.Add(p.Symbol, (p.ParameterId, p.RefOrOut))
        let initializer = x.Initializer |> Option.map (this.TransformConstructorInitializer)
        let body = x.Body |> Option.map (this.TransformBlock)
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
        let body = x.Body |> Option.map (this.TransformBlock)
        match x.Kind with
        | AccessorDeclarationKind.GetAccessorDeclaration      
        | AccessorDeclarationKind.SetAccessorDeclaration -> 
            let body =
                match body with
                | Some b -> b
                | _ ->
                    // TODO : make this inlined
                    let pr = symbol.AssociatedSymbol :?> IPropertySymbol
                    if pr.IsStatic then 
                        match x.Kind with 
                        | AccessorDeclarationKind.GetAccessorDeclaration ->     
                            Return <| ItemGet(Self, Value (String ("$" + pr.Name)))
                        | AccessorDeclarationKind.SetAccessorDeclaration -> 
                            let v = parameterList.Head.ParameterId
                            ExprStatement <| ItemSet(Self, Value (String ("$" + pr.Name)), Var v)
                        | _ -> failwith "impossible"
                    else
                        match x.Kind with 
                        | AccessorDeclarationKind.GetAccessorDeclaration ->     
                            Return <| ItemGet(This, Value (String ("$" + pr.Name)))
                        | AccessorDeclarationKind.SetAccessorDeclaration -> 
                            let v = parameterList.Head.ParameterId
                            ExprStatement <| ItemSet(This, Value (String ("$" + pr.Name)), Var v)
                        | _ -> failwith "impossible"
            {
                IsStatic = symbol.IsStatic
                Parameters = parameterList
                Body = body
                IsAsync = symbol.IsAsync
                ReturnType = returnType
            }
        | AccessorDeclarationKind.AddAccessorDeclaration
        | AccessorDeclarationKind.RemoveAccessorDeclaration ->
            {
                IsStatic = symbol.IsStatic
                Parameters = parameterList
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

    member this.TransformIncrOrDecr (node : ExpressionSyntax, operand) =
        let symbol = env.SemanticModel.GetSymbolInfo(node).Symbol :?> IMethodSymbol
        let typ, meth = getTypeAndMethod symbol

        let e = IgnoreExprSourcePos operand
        let callOp v = Call(None, typ, meth, [ v ])
        match e with
        | Var v ->
            VarSet(v, callOp operand)
        | ItemGet(o, i) ->
            let ov = Id.New ()
            let iv = Id.New ()
            ItemSet(Var ov, Var iv, callOp (ItemGet(Var ov, Var iv)))
        | FieldGet(o, t, f) ->
            match o with
            | Some o ->
                let ov = Id.New ()
                Let (ov, o, FieldSet(Some (Var ov), t, f, callOp (FieldGet(Some (Var ov), t, f))))
            | _ ->
                FieldSet(None, t, f, callOp operand)
        | Application(ItemGet (r, Value (String "get")), [], _, _) ->
            SetRef r (callOp operand)
        | Call (thisOpt, typ, getter, args) ->
            Call (thisOpt, typ, setterOf getter, args @ [ callOp operand ])
        | _ -> failwithf "ref argument has unexpected form: %+A" e     
        |> withExprSourcePos node
                                                   
    member this.TransformPostfixUnaryExpression (x: PostfixUnaryExpressionData) : _ =
        let operand = x.Operand |> this.TransformExpression
        this.TransformIncrOrDecr(x.Node, operand)

    member this.TransformPrefixUnaryExpression (x: PrefixUnaryExpressionData) : _ =
        let operand = x.Operand |> this.TransformExpression
        match x.Kind with
        | PrefixUnaryExpressionKind.PreIncrementExpression 
        | PrefixUnaryExpressionKind.PreDecrementExpression ->
            this.TransformIncrOrDecr(x.Node, operand)
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
                NonGeneric (TypeDefinition { Assembly = "mscorlib"; FullName = "System.IDisposable" }),
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
        | Some (symbol, typ) ->
            let err = env.Caught.Value    
            env.Vars.Add(symbol, err)
            let filter = x.Filter |> Option.map (this.TransformCatchFilterClause)
            let typeCheck = TypeCheck(Var err, typ) 
            let cond = 
                match filter with
                | Some f -> Conditional(typeCheck, f, Value (Bool false))
                | None -> typeCheck
            let block = x.Block |> this.TransformBlock
            Some cond, block
        | None ->
            let block = x.Block |> this.TransformBlock
            None, block        

    member this.TransformCatchDeclaration (x: CatchDeclarationData) : _ =
        let symbol = env.SemanticModel.GetDeclaredSymbol(x.Node)
        let typ = sr.ReadType symbol.Type
        symbol, typ

    member this.TransformCatchFilterClause (x: CatchFilterClauseData) : _ =
        x.FilterExpression |> this.TransformExpression

    member this.TransformFinallyClause (x: FinallyClauseData) : _ =
        x.Block |> this.TransformBlock

    member this.TransformForEachStatement (x: ForEachStatementData) : _ =
        let expression = x.Expression |> this.TransformExpression
        let symbol = env.SemanticModel.GetDeclaredSymbol(x.Node)
        let typ = sr.ReadType symbol.Type
        let v = Id.New symbol.Name
        env.Vars.Add(symbol, v)
        let statement = x.Statement |> this.TransformStatement

        // TODO: foreach on System.Collections.IEnumerable 

        let getEnumerator e =
            let ret = Generic (TypeDefinition { Assembly = "mscorlib"; FullName = "System.Collections.Generic.IEnumerator`1" }) [TypeParameter 0]
            Call(
                Some e, 
                Generic (TypeDefinition { Assembly = "mscorlib"; FullName = "System.Collections.Generic.IEnumerable`1" }) [typ],
                NonGeneric (Method { MethodName = "GetEnumerator"; Parameters = []; ReturnType = ConcreteType ret; Generics = 0 }),
                []            
            )

        let getCurrent e =
            Call(
                Some e, 
                Generic (TypeDefinition { Assembly = "mscorlib"; FullName = "System.Collections.Generic.IEnumerator`1" }) [typ],
                NonGeneric (Method { MethodName = "get_Current"; Parameters = []; ReturnType = TypeParameter 0; Generics = 0 }),
                []            
            )

        let moveNext e =
            let btyp = NonGeneric (TypeDefinition { Assembly = "mscorlib"; FullName = "System.Boolean" }) 
            Call(
                Some e, 
                NonGeneric (TypeDefinition { Assembly = "mscorlib"; FullName = "System.Collections.IEnumerator" }),
                NonGeneric (Method { MethodName = "MoveNext"; Parameters = []; ReturnType = ConcreteType btyp; Generics = 0 }),
                []            
            )

        let disp e =
            Call(
                Some e, 
                NonGeneric (TypeDefinition { Assembly = "mscorlib"; FullName = "System.IDisposable" }),
                NonGeneric (Method { MethodName = "Dispose"; Parameters = []; ReturnType = VoidType; Generics = 0 }),
                []            
            )
        
        let en = Id.New ()

        Block [
            VarDeclaration (en, getEnumerator expression)
            VarDeclaration (v, Undefined)
            TryFinally(
                While(
                    moveNext (Var en), 
                    Block [
                        ExprStatement <| VarSet (v, getCurrent (Var en))
                        statement
                    ]
                ), 
                ExprStatement <| disp (Var en)
            )
        ]

    member this.TransformAnonymousFunctionExpression (x: AnonymousFunctionExpressionData) : _ =
        match x with
        | AnonymousFunctionExpressionData.LambdaExpression          x -> this.TransformLambdaExpression x
        | AnonymousFunctionExpressionData.AnonymousMethodExpression x -> TODO x //this.TransformAnonymousMethodExpression x

    member this.TransformLambdaExpression (x: LambdaExpressionData) : _ =
        match x with
        | LambdaExpressionData.SimpleLambdaExpression        x -> this.TransformSimpleLambdaExpression x
        | LambdaExpressionData.ParenthesizedLambdaExpression x -> this.TransformParenthesizedLambdaExpression x

    member this.TransformSimpleLambdaExpression (x: SimpleLambdaExpressionData) : _ =
        let parameter = x.Parameter |> this.TransformParameter
        let id = Id.New parameter.ParameterId.Name.Value              
        env.Parameters.Add(parameter.Symbol, (id, false))
        let body = x.Body |> this.TransformCSharpNode
        let symbol = env.SemanticModel.GetSymbolInfo(x.Node).Symbol :?> IMethodSymbol
        if symbol.IsAsync then
            let b = 
                body |> Continuation.addLastReturnIfNeeded Undefined
                |> Continuation.AwaitTransformer().TransformStatement 
                |> BreakStatement
                |> Continuation.FreeNestedGotos().TransformStatement
            let labels = Continuation.CollectLabels.Collect b
            Function([id], Continuation.AsyncTransformer(labels, sr.ReadAsyncReturnKind symbol).TransformMethodBody(b))
        else
            Function([id], body)
    
    member this.TransformParenthesizedLambdaExpression (x: ParenthesizedLambdaExpressionData) : _ =
        let parameterList = x.ParameterList |> this.TransformParameterList
        let ids =
            parameterList |> List.map (fun p -> 
                let id = Id.New p.ParameterId.Name.Value
                env.Parameters.Add(p.Symbol, (id, false))
                id
            )    
        let body = x.Body |> this.TransformCSharpNode
        let symbol = env.SemanticModel.GetSymbolInfo(x.Node).Symbol :?> IMethodSymbol
        if symbol.IsAsync then
            let b = 
                body |> Continuation.addLastReturnIfNeeded Undefined
                |> Continuation.AwaitTransformer().TransformStatement 
                |> BreakStatement
                |> Continuation.FreeNestedGotos().TransformStatement
            let labels = Continuation.CollectLabels.Collect b
            Function(ids, Continuation.AsyncTransformer(labels, sr.ReadAsyncReturnKind symbol).TransformMethodBody(b))
        else
            Function(ids, body)

    member this.TransformCSharpNode (x: CSharpNodeData) : _ =
        match x with
        | CSharpNodeData.Expression                      x -> this.TransformExpression x |> Return
        | CSharpNodeData.BaseArgumentList                x -> TODO x //this.TransformBaseArgumentList env x
        | CSharpNodeData.QueryClause                     x -> TODO x //this.TransformQueryClause env x
        | CSharpNodeData.SelectOrGroupClause             x -> TODO x //this.TransformSelectOrGroupClause env x
        | CSharpNodeData.InterpolatedStringContent       x -> TODO x //this.TransformInterpolatedStringContent env x
        | CSharpNodeData.Statement                       x -> this.TransformStatement x
        | CSharpNodeData.SwitchLabel                     x -> TODO x //this.TransformSwitchLabel env x
        | CSharpNodeData.MemberDeclaration               x -> TODO x //this.TransformMemberDeclaration env x
        | CSharpNodeData.BaseType                        x -> TODO x //this.TransformBaseType x
        | CSharpNodeData.TypeParameterConstraint         x -> TODO x //this.TransformTypeParameterConstraint env x
        | CSharpNodeData.BaseParameterList               x -> TODO x //this.TransformBaseParameterList env x
        | CSharpNodeData.Cref                            x -> TODO x //this.TransformCref env x
        | CSharpNodeData.BaseCrefParameterList           x -> TODO x //this.TransformBaseCrefParameterList env x
        | CSharpNodeData.XmlNode                         x -> TODO x //this.TransformXmlNode env x
        | CSharpNodeData.XmlAttribute                    x -> TODO x //this.TransformXmlAttribute env x
        | CSharpNodeData.TypeArgumentList                x -> TODO x //this.TransformTypeArgumentList env x
        | CSharpNodeData.ArrayRankSpecifier              x -> TODO x //this.TransformArrayRankSpecifier env x
        | CSharpNodeData.Argument                        x -> TODO x //this.TransformArgument  x
        | CSharpNodeData.NameColon                       x -> TODO x //this.TransformNameColon env x
        | CSharpNodeData.AnonymousObjectMemberDeclarator x -> TODO x //this.TransformAnonymousObjectMemberDeclarator env x
        | CSharpNodeData.QueryBody                       x -> TODO x //this.TransformQueryBody env x
        | CSharpNodeData.JoinIntoClause                  x -> TODO x //this.TransformJoinIntoClause env x
        | CSharpNodeData.Ordering                        x -> TODO x //this.TransformOrdering env x
        | CSharpNodeData.QueryContinuation               x -> TODO x //this.TransformQueryContinuation env x
        | CSharpNodeData.InterpolationAlignmentClause    x -> TODO x //this.TransformInterpolationAlignmentClause env x
        | CSharpNodeData.InterpolationFormatClause       x -> TODO x //this.TransformInterpolationFormatClause env x
        | CSharpNodeData.VariableDeclaration             x -> TODO x //this.TransformVariableDeclaration env x
        | CSharpNodeData.VariableDeclarator              x -> TODO x //this.TransformVariableDeclarator env x
        | CSharpNodeData.EqualsValueClause               x -> TODO x //this.TransformEqualsValueClause env x
        | CSharpNodeData.ElseClause                      x -> TODO x //this.TransformElseClause env x
        | CSharpNodeData.SwitchSection                   x -> TODO x //this.TransformSwitchSection env x
        | CSharpNodeData.CatchClause                     x -> TODO x //this.TransformCatchClause env x
        | CSharpNodeData.CatchDeclaration                x -> TODO x //this.TransformCatchDeclaration env x
        | CSharpNodeData.CatchFilterClause               x -> TODO x //this.TransformCatchFilterClause env x
        | CSharpNodeData.FinallyClause                   x -> TODO x //this.TransformFinallyClause env x
        | CSharpNodeData.CompilationUnit                 x -> TODO x //this.TransformCompilationUnit env x
    //    | CSharpNodeData.ExternAliasDirective            x -> TODO x //this.TransformExternAliasDirective env x
    //    | CSharpNodeData.UsingDirective                  x -> TODO x //this.TransformUsingDirective env x
    //    | CSharpNodeData.AttributeList                   x -> TODO x //this.TransformAttributeList env x
        | CSharpNodeData.AttributeTargetSpecifier        x -> TODO x //this.TransformAttributeTargetSpecifier env x
        | CSharpNodeData.Attribute                       x -> TODO x //this.TransformAttribute env x
        | CSharpNodeData.AttributeArgumentList           x -> TODO x //this.TransformAttributeArgumentList env x
        | CSharpNodeData.AttributeArgument               x -> TODO x //this.TransformAttributeArgument x
        | CSharpNodeData.NameEquals                      x -> TODO x //this.TransformNameEquals env x
        | CSharpNodeData.TypeParameterList               x -> TODO x //this.TransformTypeParameterList env x
        | CSharpNodeData.TypeParameter                   x -> TODO x //this.TransformTypeParameter env x
        | CSharpNodeData.BaseList                        x -> TODO x //this.TransformBaseList env x
    //    | CSharpNodeData.TypeParameterConstraintClause   x -> TODO x //this.TransformTypeParameterConstraintClause env x
        | CSharpNodeData.ExplicitInterfaceSpecifier      x -> TODO x //this.TransformExplicitInterfaceSpecifier env x
        | CSharpNodeData.ConstructorInitializer          x -> TODO x //this.TransformConstructorInitializer env x
        | CSharpNodeData.ArrowExpressionClause           x -> TODO x //this.TransformArrowExpressionClause env x
        | CSharpNodeData.AccessorList                    x -> TODO x //this.TransformAccessorList env x
        | CSharpNodeData.AccessorDeclaration             x -> TODO x //this.TransformAccessorDeclaration env x
        | CSharpNodeData.Parameter                       x -> TODO x //this.TransformParameter env x
        | CSharpNodeData.CrefParameter                   x -> TODO x //this.TransformCrefParameter env x
        | CSharpNodeData.XmlElementStartTag              x -> TODO x //this.TransformXmlElementStartTag env x
        | CSharpNodeData.XmlElementEndTag                x -> TODO x //this.TransformXmlElementEndTag env x
        | CSharpNodeData.XmlName                         x -> TODO x //this.TransformXmlName x
        | CSharpNodeData.XmlPrefix                       x -> TODO x //this.TransformXmlPrefix env x

    member this.TransformInstanceExpression (x: InstanceExpressionData) : _ =
        match x with
        | InstanceExpressionData.ThisExpression x -> This
        | InstanceExpressionData.BaseExpression x -> Base
        |> withExprSourcePos x.Node

    member this.TransformMemberAccess (node: ExpressionSyntax, expr: option<ExpressionData>) =
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
                ItemGet(getExpression().Value, Value (String (symbol.Name)))
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
            let expression = getExpression()
            let typ = sr.ReadNamedType symbol.ContainingType
            let f = symbol.Name
            FieldGet(expression, typ, f)
        | :? IEventSymbol as symbol ->
            let expression = getExpression()
            let typ = sr.ReadNamedType symbol.ContainingType
            let f = symbol.Name
            FieldGet(expression, typ, f)
        | _ ->
            failwith "member access not handled: not property, event, method or field"      
        |> withExprSourcePos node

    member this.TransformMemberAccessExpression (x: MemberAccessExpressionData) : _ =
        this.TransformMemberAccess(x.Node, Some x.Expression)

    member this.TransformConditionalAccessExpression (x: ConditionalAccessExpressionData) : _ =
        let expression = x.Expression |> this.TransformExpression
        let id = Id.New ()
        let whenNotNull = x.WhenNotNull |> RoslynTransformer(env.WithConditional(id)).TransformExpression
        Let (id, expression, Conditional(Var id ^== Value Null, Value Null, whenNotNull))

    member this.TransformMemberBindingExpression (x: MemberBindingExpressionData) : _ =
        this.TransformMemberAccess(x.Node, None)

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
            expression
        else
            call symbol None [ expression ]

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
                    Lambda([a; b], NewArray [Var a; Var b])
                | _ ->
                    Lambda([a; b], jsConcat (Var a) [NewArray [Var b]])
            let inclSelect expr =
                match ri with
                | Choice1Of2 ri ->
                    env.RangeVars.[ri] <- (a, None)
                | _ -> ()
                env.RangeVars.[symbol] <- (b, None)
                let newResSelect = Lambda([a; b], this.TransformExpression expr)     
                queryCall querySymbol [on; Lambda([a], expression); newResSelect]      
            queryCall querySymbol [on; Lambda([a], expression); resSelect], Choice2Of2 (a, num, Some inclSelect)    

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
            queryCall querySymbol [on; Lambda([a], res)], Choice2Of2 (a, num, None)    

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
                    Lambda([a; b], NewArray [Var a; Var b])
                | _ ->
                    Lambda([a; b], jsConcat (Var a) [NewArray [Var b]])
            let inclSelect expr =
                match ri with
                | Choice1Of2 ri ->
                    env.RangeVars.[ri] <- (a, None)
                | _ -> ()
                env.RangeVars.[symbol] <- (b, None)
                let newResSelect = Lambda([a; b], this.TransformExpression expr)     
                queryCall querySymbol [on; inExpression; Lambda([a], leftExpression); Lambda([b], rightExpression); newResSelect]      
            queryCall querySymbol [on; inExpression; Lambda([a], leftExpression); Lambda([b], rightExpression); resSelect], Choice2Of2 (a, num, Some inclSelect)
    
    member this.TransformWhereClause (x: WhereClauseData) : _ =
        let querySymbol = (env.SemanticModel.GetQueryClauseInfo(x.Node).OperationInfo.Symbol :?> IMethodSymbol)
        fun (on, ri) ->
            let expression = x.Condition |> this.TransformExpression
            let a, ri =
                match ri with
                | Choice1Of2 ri -> fst env.RangeVars.[ri], Choice1Of2 ri
                | Choice2Of2 (a, n, _) -> a, Choice2Of2 (a, n, None)
            queryCall querySymbol [on; Lambda([a], expression)], ri

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
            queryCall querySymbol [on; Lambda([a], expression)], ri

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
                queryCall symbol [on; Lambda([a], expression)]

    member this.TransformGroupClause (x: GroupClauseData) : _ =
        let symbol = env.SemanticModel.GetSymbolInfo(x.Node).Symbol :?> IMethodSymbol
        fun (on, ri) ->
            let groupExpression = x.GroupExpression |> this.TransformExpression
            let byExpression = x.ByExpression |> this.TransformExpression
            let a =
                match ri with
                | Choice1Of2 ri -> fst env.RangeVars.[ri]
                | Choice2Of2 (a, _, _) -> a
            queryCall symbol [on; Lambda([a], byExpression); Lambda([a], groupExpression)]

    member this.TransformQueryContinuation (x: QueryContinuationData) : _ =
        let symbol = env.SemanticModel.GetDeclaredSymbol(x.Node)
        let id = Id.New symbol.Name
        env.RangeVars.Add(symbol, (id, None))
        let body = x.Body |> this.TransformQueryBody
        fun expr -> body (expr, Choice1Of2 symbol)

    member this.TransformInterpolatedStringExpression (x: InterpolatedStringExpressionData) : _ =
        x.Contents |> Seq.map this.TransformInterpolatedStringContent 
        |> Seq.reduce (^+)

    member this.TransformInterpolatedStringContent (x: InterpolatedStringContentData) : _ =
        match x with
        | InterpolatedStringContentData.InterpolatedStringText x -> 
            Value (String x.Node.TextToken.ValueText)
        | InterpolatedStringContentData.Interpolation x -> 
            if Option.isSome x.AlignmentClause then failwith "TODO: interpolated string alignment"
            if Option.isSome x.FormatClause then failwith "TODO: interpolated string formatting"
            x.Expression |> this.TransformExpression
