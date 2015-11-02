module internal WebSharper.Compiler.CSharp.ToCSharpAST

//open System.Runtime.CompilerServices

open Microsoft.CodeAnalysis
open Microsoft.CodeAnalysis.CSharp
open Microsoft.CodeAnalysis.CSharp.Syntax
 
open WebSharper.Core
open WebSharper.Core.AST
            
open WebSharper.Compiler.CSharp.RoslynHelpers

open System.Collections.Generic

type Environment =
    {
        SemanticModel : SemanticModel 
        Vars : Dictionary<ILocalSymbol, Id>
        Parameters : IDictionary<IParameterSymbol, Id>
        Labels : Dictionary<ILabelSymbol, Id>
        Caught : option<Id>
        Compilation : Metadata.Compilation
    }
    static member New(model, comp) = 
        { 
            SemanticModel = model
            Vars = Dictionary()
            Parameters = Dictionary()
            Labels = Dictionary()
            Caught = None
            Compilation = comp
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

type CSharpParameter =
    {
        ParameterId : Id
        Symbol : IParameterSymbol
        DefaultValue : option<Expression>
        Type : Type
        RefOrOut : bool
    }

type CSharpMethod =
    {
        IsStatic : bool
        Parameters : list<CSharpParameter>
        Body : Statement
        IsAsync : bool
        ReturnType : Type
    }  

type CSharpConstructorInitializer =
    | ThisInitializer of list<Expression>
    | BaseInitializer of list<Expression>

type CSharpConstructor =
    {
        Parameters : list<CSharpParameter>
        Body : Statement
        Initializer : option<CSharpConstructorInitializer>
    }

//let childrenCSharpExpr expr =
//    let cOE expr =
//        match expr with
//        | Await a -> [a]
//        | NamedParameter (a, b) -> [b]
//        | RefOrOutParameter a -> [a]  
//    childrenDotNetExpr cOE expr
      
//let childrenCSharpStatement statement =
//    let cOS statement = 
//        match statement with
//        | Yield _
//        | GotoCase _ 
//        | Goto _ -> []
//        | CSharpSwitch (_, a) -> a |> List.map snd
//        | Statements a -> a 
//    childrenStatement cOS statement
//
let getConstantValue (env: Environment) x =
    env.SemanticModel.GetConstantValue(x).Value

let getSourcePos (x: CSharpSyntaxNode) =
    let span = x.SyntaxTree.GetLineSpan(x.Span)
    {   
        FileName = span.Path
        Start = 
            let pos = span.StartLinePosition
            pos.Line + 1, pos.Character + 1
        End = 
            let pos = span.EndLinePosition
            pos.Line + 1, pos.Character + 1
    }

let withExprSourcePos (x: CSharpSyntaxNode) expr =
    ExprSourcePos (getSourcePos x, expr)

let withStatementSourcePos (x: CSharpSyntaxNode) statement =
    StatementSourcePos (getSourcePos x, statement)

exception TransformError of obj * message: string
    with
    override this.Message = this.message    

let inline err node message = raise (TransformError (node, message)) 
let inline errf node x = Printf.kprintf (err node) x

let inline TODO() = failwith "TODO" 

let rec getNamedTypeDefinition (x: INamedTypeSymbol) =
    let rec getNamespaceOrTypeAddress acc (symbol: INamespaceOrTypeSymbol) =
        match symbol.ContainingNamespace with
        | null -> acc |> String.concat "."
        | ns -> getNamespaceOrTypeAddress (symbol.Name :: acc) ns   
    
    let rec getTypeAddress acc (symbol: INamedTypeSymbol) =
        match symbol.ContainingType with
        | null -> 
            let ns = getNamespaceOrTypeAddress [] symbol
            if List.isEmpty acc then ns else
                ns :: acc |> String.concat "+" 
        | t -> getTypeAddress (symbol.Name :: acc) t           

    Hashed {
        Assembly = x.ContainingAssembly.Identity.Name
        FullName = (getTypeAddress [] x) + (match x.Arity with 0 -> "" | a -> "`" + string a)
    }

and getNamedType (x: INamedTypeSymbol) = 
    let ta = x.TypeArguments |> Seq.map  getType |> List.ofSeq
    let td = getNamedTypeDefinition x
    concrete (td, ta)
    
and recognizeNamedType (x: INamedTypeSymbol) =
    let ta = x.TypeArguments |> Seq.map getType |> List.ofSeq
    let td = getNamedTypeDefinition x
    let tName = td.Value.FullName
    if tName.StartsWith "System.Tuple" then
        TupleType ta
    elif tName = "Microsoft.FSharp.Core.FSharpFunc`2" then
        let [a; r] = ta
        FSharpFuncType(a, r)
    elif tName = "Microsoft.FSharp.Core.Unit" || tName = "System.Void" then
        VoidType
    else
        ConcreteType (concrete (td, ta))

and getType (x: ITypeSymbol) : Type =
    match x.TypeKind with
    | TypeKind.Array ->
        let t = x :?> IArrayTypeSymbol
        ArrayType (getType t.ElementType, t.Rank)
    | TypeKind.Class
    | TypeKind.Struct
    | TypeKind.Error
    | TypeKind.Delegate
    | TypeKind.Interface ->
        let t = x :?> INamedTypeSymbol 
        recognizeNamedType t
//        let tdef = getNamedType t
//        let targs = t.TypeArguments |> Seq.map getType |> List.ofSeq
//        concreteType (tdef, targs)
    | TypeKind.TypeParameter ->
        let t = x :?> ITypeParameterSymbol 
        GenericType t.Ordinal
    | _ ->
        errf x "transformType: typekind %O not suppported" x.TypeKind

let getMethod (x: IMethodSymbol) =
    Hashed {
//        DefinedBy = getNamedType x.ContainingType 
        MethodName = x.Name
        Parameters = x.Parameters |> Seq.map (fun p -> getType p.Type) |> List.ofSeq
        ReturnType = x.ReturnType |> getType
        Generics = x.Arity
    }

let getConstructor (x: IMethodSymbol) =
    Hashed {
        CtorParameters = x.Parameters |> Seq.map (fun p -> getType p.Type) |> List.ofSeq
    }

let getMember (x: IMethodSymbol) =
    let name = x.Name
    let getParams() = x.Parameters |> Seq.map (fun p -> getType p.Type) |> List.ofSeq
    match name with
    | ".ctor" ->
        Member.Constructor <| Hashed {
            CtorParameters = getParams()
        }
    | ".cctor" -> Member.StaticConstructor
    | _ ->
        let meth =
            Hashed {
        //        DefinedBy = getNamedType x.ContainingType 
                MethodName = x.Name
                Parameters = getParams()
                ReturnType = x.ReturnType |> getType
                Generics = x.Arity
            }
        if x.IsOverride then
            Member.Override(getNamedTypeDefinition x.OverriddenMethod.ContainingType, meth)
        // TODO: more explicit implementations
        // TODO: implicit interface implementations?
        elif x.ExplicitInterfaceImplementations.Length > 0 then
            Member.Implementation(getNamedTypeDefinition x.ExplicitInterfaceImplementations.[0].ContainingType, meth)
        else
            Member.Method (not x.IsStatic, meth)

//let getField (x: IFieldSymbol) =
//    Hashed {
//        FieldName = x.Name
//        Type      = x.Type |> getType
//    }

let getParameter (x: IParameterSymbol) : CSharpParameter =
//    let attributeLists = x.AttributeLists |> Seq.map (transformAttributeList env) |> List.ofSeq
    let typ = getType x.Type
    let default_ = None // TODO if x.HasExplicitDefaultValue then Some 
    let id = Id.New x.Name
    {
        ParameterId = id
        Symbol = x
        DefaultValue = default_
        Type = typ
        RefOrOut = x.RefKind <> RefKind.None
    }

let getParameters (x: IMethodSymbol) =
    x.Parameters |> Seq.map getParameter |> List.ofSeq   

let hasSignature (s: Method) (x: IMethodSymbol) =
    let s = s.Value
    x.Arity = s.Generics
    && (x.ReturnType |> getType) = s.ReturnType
    && x.Parameters |> Seq.map (fun p -> getType p.Type) |> Seq.equals s.Parameters     
  
let transformIdentifierName (env: Environment) (x: IdentifierNameData) : Expression =
    let symbol = env.SemanticModel.GetSymbolInfo(x.Node).Symbol 
    match symbol with
    | :? ILocalSymbol as s -> Var env.Vars.[s]
    | :? IParameterSymbol as p -> Var env.Parameters.[p]
    | :? IFieldSymbol as f -> FieldGet(Some This, getNamedType f.ContainingType, f.Name) 
    | _ -> 
        err x.Node (sprintf "transformIdentifierName: Local variable not found, symbol type: %s, name: %s" 
            (symbol.GetType().FullName) symbol.Name)

let rec transformExpression (env: Environment) (x: ExpressionData) : Expression =
    try
        match x with
        | ExpressionData.Type                              x -> transformType env x
        | ExpressionData.InstanceExpression                x -> transformInstanceExpression env x
        | ExpressionData.AnonymousFunctionExpression       x -> transformAnonymousFunctionExpression env x
        | ExpressionData.ParenthesizedExpression           x -> transformParenthesizedExpression env x
        | ExpressionData.PrefixUnaryExpression             x -> transformPrefixUnaryExpression env x
        | ExpressionData.AwaitExpression                   x -> transformAwaitExpression env x
        | ExpressionData.PostfixUnaryExpression            x -> transformPostfixUnaryExpression env x
        | ExpressionData.MemberAccessExpression            x -> transformMemberAccessExpression env x
        | ExpressionData.ConditionalAccessExpression       x -> transformConditionalAccessExpression env x
        | ExpressionData.MemberBindingExpression           x -> transformMemberBindingExpression env x
        | ExpressionData.ElementBindingExpression          x -> TODO() //transformElementBindingExpression env x
        | ExpressionData.ImplicitElementAccess             x -> TODO() //transformImplicitElementAccess env x
        | ExpressionData.BinaryExpression                  x -> transformBinaryExpression env x
        | ExpressionData.AssignmentExpression              x -> transformAssignmentExpression env x
        | ExpressionData.ConditionalExpression             x -> transformConditionalExpression env x
        | ExpressionData.LiteralExpression                 x -> transformLiteralExpression env x |> Value |> withExprSourcePos x.Node
        | ExpressionData.MakeRefExpression                 x -> TODO() //transformMakeRefExpression env x
        | ExpressionData.RefTypeExpression                 x -> TODO() //transformRefTypeExpression env x
        | ExpressionData.RefValueExpression                x -> TODO() //transformRefValueExpression env x
        | ExpressionData.CheckedExpression                 x -> TODO() //transformCheckedExpression env x
        | ExpressionData.DefaultExpression                 x -> TODO() //transformDefaultExpression env x
        | ExpressionData.TypeOfExpression                  x -> TODO() //transformTypeOfExpression env x
        | ExpressionData.SizeOfExpression                  x -> TODO() //transformSizeOfExpression env x
        | ExpressionData.InvocationExpression              x -> transformInvocationExpression env x
        | ExpressionData.ElementAccessExpression           x -> TODO() //transformElementAccessExpression env x
        | ExpressionData.CastExpression                    x -> TODO() //transformCastExpression env x
        | ExpressionData.InitializerExpression             x -> TODO() //transformInitializerExpression env x
        | ExpressionData.ObjectCreationExpression          x -> transformObjectCreationExpression env x
        | ExpressionData.AnonymousObjectCreationExpression x -> TODO() //transformAnonymousObjectCreationExpression env x
        | ExpressionData.ArrayCreationExpression           x -> transformArrayCreationExpression env x
        | ExpressionData.ImplicitArrayCreationExpression   x -> transformImplicitArrayCreationExpression env x
        | ExpressionData.StackAllocArrayCreationExpression x -> TODO() //transformStackAllocArrayCreationExpression env x
        | ExpressionData.QueryExpression                   x -> TODO() //transformQueryExpression env x
        | ExpressionData.OmittedArraySizeExpression        x -> TODO() //transformOmittedArraySizeExpression env x
        | ExpressionData.InterpolatedStringExpression      x -> TODO() //transformInterpolatedStringExpression env x      
    with e ->
        env.Compilation.AddError(Some (getSourcePos x.Node), Metadata.SourceError("Error while reading C# code: " + e.Message))
        WebSharper.Compiler.ToJavaScript.errorPlaceholder        

and transformType (env: Environment) (x: TypeData) : Expression =
    match x with
    | TypeData.Name                x -> transformName env x
    | TypeData.PredefinedType      x -> TODO() //transformPredefinedType env x
    | TypeData.ArrayType           x -> TODO() //transformArrayType env x
    | TypeData.PointerType         x -> TODO() //transformPointerType env x
    | TypeData.NullableType        x -> TODO() //transformNullableType env x
    | TypeData.OmittedTypeArgument x -> TODO() //transformOmittedTypeArgument env x

and transformName (env: Environment) (x: NameData) : Expression =
    match x with
    | NameData.SimpleName         x -> transformSimpleName env x
    | NameData.QualifiedName      x -> TODO() //transformQualifiedName env x
    | NameData.AliasQualifiedName x -> TODO() //transformAliasQualifiedName env x

and transformSimpleName (env: Environment) (x: SimpleNameData) : Expression =
    match x with
    | SimpleNameData.IdentifierName x -> transformIdentifierName env x |> withExprSourcePos x.Node
    | SimpleNameData.GenericName    x -> TODO() //transformGenericName env x

and transformInvocationExpression (env: Environment) (x: InvocationExpressionData) : Expression =
    let symbol = env.SemanticModel.GetSymbolInfo(x.Node).Symbol.OriginalDefinition :?> IMethodSymbol
    let typ = getNamedType symbol.ContainingType
    let ma = symbol.TypeArguments |> Seq.map getType |> List.ofSeq
    let meth = concrete (getMethod symbol, ma)
    let argumentList = x.ArgumentList |> transformArgumentList env
    if symbol.IsStatic then
        Call(None, typ, meth, argumentList)
    else        
        let expression =  x.Expression |> transformExpression env
        Call(Some expression, typ, meth, argumentList)
    |> withExprSourcePos x.Node

and transformArgument (env: Environment) (x: ArgumentData) : Expression =
    let nameColon = 
        x.NameColon |> Option.map (fun nc -> 
            match transformIdentifierName env nc.Name with
            | Var v -> v
            | _ -> failwith "transformArgument error"
        )
    let expression = x.Expression |> transformExpression env
    let refOrOut =
        if Option.isSome x.RefOrOutKeyword 
        then RefOrOutParameter expression
        else expression
    match nameColon with
    | Some id -> NamedParameter (id, refOrOut)  
    | _ -> refOrOut

and transformArgumentList (env: Environment) (x: ArgumentListData) : list<Expression> =
    x.Arguments |> Seq.map (transformArgument env) |> List.ofSeq
 
and transformLiteralExpression (env: Environment) (x: LiteralExpressionData) : Literal =
    match getConstantValue env x.Node with
    | x when obj.ReferenceEquals(x, null) -> Null      
    | :? bool   as v -> Bool   v
    | :? byte   as v -> Byte   v
    | :? char   as v -> Char   v
    | :? double as v -> Double v
    | :? int    as v -> Int    v
    | :? int16  as v -> Int16  v
    | :? int64  as v -> Int64  v
    | :? sbyte  as v -> SByte  v
    | :? single as v -> Single v
    | :? string as v -> String v
    | :? uint16 as v -> UInt16 v
    | :? uint32 as v -> UInt32 v
    | :? uint64 as v -> UInt64 v
    | v -> errf x.Node "transformLiteralExpression: unrecognized literal type %O" (v.GetType().FullName)

and transformStatement (env: Environment) (x: StatementData) : Statement =
    try
        match x with
        | StatementData.Block                     x -> transformBlock env x
        | StatementData.LocalDeclarationStatement x -> transformLocalDeclarationStatement env x
        | StatementData.ExpressionStatement       x -> transformExpressionStatement env x
        | StatementData.EmptyStatement            x -> transformEmptyStatement env x
        | StatementData.LabeledStatement          x -> transformLabeledStatement env x
        | StatementData.GotoStatement             x -> transformGotoStatement env x
        | StatementData.BreakStatement            x -> transformBreakStatement env x
        | StatementData.ContinueStatement         x -> transformContinueStatement env x
        | StatementData.ReturnStatement           x -> transformReturnStatement env x
        | StatementData.ThrowStatement            x -> transformThrowStatement env x
        | StatementData.YieldStatement            x -> transformYieldStatement env x
        | StatementData.WhileStatement            x -> transformWhileStatement env x
        | StatementData.DoStatement               x -> transformDoStatement env x
        | StatementData.ForStatement              x -> transformForStatement env x
        | StatementData.ForEachStatement          x -> transformForEachStatement env x
        | StatementData.UsingStatement            x -> transformUsingStatement env x
        | StatementData.FixedStatement            x -> TODO() //transformFixedStatement env x
        | StatementData.CheckedStatement          x -> TODO() //transformCheckedStatement env x
        | StatementData.UnsafeStatement           x -> TODO() //transformUnsafeStatement env x
        | StatementData.LockStatement             x -> TODO() //transformLockStatement env x
        | StatementData.IfStatement               x -> transformIfStatement env x
        | StatementData.SwitchStatement           x -> transformSwitchStatement env x
        | StatementData.TryStatement              x -> transformTryStatement env x
    with e ->
        env.Compilation.AddError(Some (getSourcePos x.Node), Metadata.SourceError("Error while reading C# code: " + e.Message))
        ExprStatement WebSharper.Compiler.ToJavaScript.errorPlaceholder        

and transformBlock (env: Environment) (x: BlockData) : Statement =
    x.Statements |> Seq.map (transformStatement env) |> List.ofSeq |> Block
    |> withStatementSourcePos x.Node  
//    let rec varDecls acc vs =
//        match vs with
//        | [] -> acc
//        | (i, e) :: rest ->
//            varDecls (Let (i, e, acc)) rest
//    let rec blockToExpr acc b =
//        match b with
//        | [] -> acc
//        | Statement st :: rest ->  
//            match acc with
//            | Sequential s -> blockToExpr (Sequential (st :: s)) rest
//            | Empty -> blockToExpr st rest 
//            | _ -> blockToExpr (Sequential [st; acc]) rest
//        | VarDeclarations vars :: rest -> 
//            blockToExpr (varDecls acc (vars |> List.rev)) rest 
//    x.Statements |> Seq.map (transformStatement env) |> List.ofSeq |> List.rev
//    |> blockToExpr Empty

and transformEmptyStatement (env: Environment) (x: EmptyStatementData) : Statement =
    Empty

and transformLabeledStatement (env: Environment) (x: LabeledStatementData) : Statement =
    let symbol = env.SemanticModel.GetSymbolInfo(x.Node).Symbol :?> ILabelSymbol
    let label = env.GetLabelId(symbol)
    let statement = x.Statement |> transformStatement env
    Labeled (label, statement)
    |> withStatementSourcePos x.Node

and transformEqualsValueClause (env: Environment) (x: EqualsValueClauseData) : Expression =
    x.Value |> transformExpression env

and transformNameColon (env: Environment) (x: NameColonData) : Expression =
    x.Name |> transformIdentifierName env

and transformNameEquals (env: Environment) (x: NameEqualsData) : Expression =
    x.Name |> transformIdentifierName env

//and transformParameterList (env: Environment) (x: ParameterListData) =
//    x.Parameters |> transformParameter env 

and transformVariableDeclaration (env: Environment) (x: VariableDeclarationData) : list<Id * Expression> =
    x.Variables |> Seq.map (transformVariableDeclarator env) |> List.ofSeq

and transformLocalDeclarationStatement (env: Environment) (x: LocalDeclarationStatementData) : Statement =
    x.Declaration |> transformVariableDeclaration env |> List.map VarDeclaration |> Statements 
    |> withStatementSourcePos x.Node

and transformExpressionStatement (env: Environment) (x: ExpressionStatementData) : Statement =
    x.Expression |> transformExpression env |> ExprStatement

and transformVariableDeclarator (env: Environment) (x: VariableDeclaratorData) : Id * Expression =    
    let symbol = env.SemanticModel.GetDeclaredSymbol(x.Node) :?> ILocalSymbol
    let id = Id.New(symbol.Name)
    env.Vars.Add(symbol, id)
    let initializer = 
        match x.Initializer with
        | Some i -> i |> transformEqualsValueClause env
        | _ -> Undefined
    id, initializer

and transformSwitchStatement (env: Environment) (x: SwitchStatementData) : Statement =
    let expression = x.Expression |> transformExpression env
    let sections = x.Sections |> Seq.map (transformSwitchSection env) |> List.ofSeq
    CSharpSwitch (expression, sections)
    |> withStatementSourcePos x.Node

and transformSwitchSection (env: Environment) (x: SwitchSectionData) : list<option<Expression>> * Statement =
    let labels = x.Labels |> Seq.map (transformSwitchLabel env) |> List.ofSeq
    let statements = x.Statements |> Seq.map (transformStatement env) |> List.ofSeq |> combineStatements
    labels, statements

and transformSwitchLabel (env: Environment) (x: SwitchLabelData) : option<Expression> =
    match x with
    | SwitchLabelData.CaseSwitchLabel    x -> transformCaseSwitchLabel env x |> Some
    | SwitchLabelData.DefaultSwitchLabel x -> None //transformDefaultSwitchLabel env x

and transformCaseSwitchLabel (env: Environment) (x: CaseSwitchLabelData) : Expression =
    x.Value |> transformExpression env
    |> withExprSourcePos x.Node

and transformGotoStatement (env: Environment) (x: GotoStatementData) : Statement =
    match x.Kind with
    | GotoStatementKind.GotoStatement -> 
        let symbol = env.SemanticModel.GetSymbolInfo(x.Node).Symbol :?> ILabelSymbol
        let label = env.GetLabelId(symbol)
        Goto label
    | GotoStatementKind.GotoCaseStatement ->
        let expression = x.Expression |> Option.map (transformExpression env)
        GotoCase expression
    | GotoStatementKind.GotoDefaultStatement -> 
        GotoCase None
    |> withStatementSourcePos x.Node

and transformBreakStatement (env: Environment) (x: BreakStatementData) : Statement  =
    Break None
    |> withStatementSourcePos x.Node

and transformIfStatement (env: Environment) (x: IfStatementData) =
    let condition = x.Condition |> transformExpression env
    let statement = x.Statement |> transformStatement env
    let else_ = 
        match x.Else with
        | Some e -> e |> transformElseClause env
        | _ -> Empty
    If (condition, statement, else_)
    |> withStatementSourcePos x.Node

and transformElseClause (env: Environment) (x: ElseClauseData) : Statement =
    x.Statement |> transformStatement env

and transformAssignmentExpression (env: Environment) (x: AssignmentExpressionData) : Expression =
    let leftSymbol = env.SemanticModel.GetSymbolInfo(x.Left.Node).Symbol.OriginalDefinition
    match leftSymbol with
    | :? IPropertySymbol as symbol ->
        let typ = getNamedType symbol.ContainingType
        let setM = symbol.SetMethod
        let ma = setM.TypeArguments |> Seq.map getType |> List.ofSeq
        let meth = concrete (getMethod setM, ma)
        let right = x.Right |> transformExpression env
        if symbol.IsStatic then
            Call(None, typ, meth, [right]) // TODO property indexers
        else
            let left = x.Left |> transformExpression env
            // eliminate getter
            match left with
            | Call (Some v, _, _, []) ->
                Call (Some v, typ, meth, [right])
            | _ -> TODO()
    | _ ->
    let left = x.Left |> transformExpression env
    let right = x.Right |> transformExpression env
    match x.Kind with
    | AssignmentExpressionKind.SimpleAssignmentExpression ->
        match ignoreExprSourcePos left with
        | Var id -> VarSet(id, right)
        | FieldGet (obj, ty, f) -> FieldSet (obj, ty, f, right)
        | _ -> TODO()
    | AssignmentExpressionKind.AddAssignmentExpression -> TODO()
    | AssignmentExpressionKind.SubtractAssignmentExpression -> TODO()
    | AssignmentExpressionKind.MultiplyAssignmentExpression -> TODO()
    | AssignmentExpressionKind.DivideAssignmentExpression -> TODO()
    | AssignmentExpressionKind.ModuloAssignmentExpression -> TODO()
    | AssignmentExpressionKind.AndAssignmentExpression -> TODO()
    | AssignmentExpressionKind.ExclusiveOrAssignmentExpression -> TODO()
    | AssignmentExpressionKind.OrAssignmentExpression -> TODO()
    | AssignmentExpressionKind.LeftShiftAssignmentExpression -> TODO()
    | AssignmentExpressionKind.RightShiftAssignmentExpression -> TODO()
    |> withExprSourcePos x.Node

and transformParenthesizedExpression (env: Environment) (x: ParenthesizedExpressionData) : Expression =
    x.Expression |> transformExpression env

and transformBinaryExpression (env: Environment) (x: BinaryExpressionData) : Expression =
    let symbol = env.SemanticModel.GetSymbolInfo(x.Node).Symbol.OriginalDefinition :?> IMethodSymbol
    let typ = getNamedType symbol.ContainingType
    let oa = symbol.TypeArguments |> Seq.map getType |> List.ofSeq
    let operator = concrete (getMethod symbol, oa)
    let left = x.Left |> transformExpression env
    let right = x.Right |> transformExpression env
    match x.Kind with
    | BinaryExpressionKind.LogicalOrExpression ->
        Conditional(left, Value (Bool true), right)
    | BinaryExpressionKind.LogicalAndExpression ->
        Conditional(left, right, Value (Bool false))
    | BinaryExpressionKind.CoalesceExpression ->
        let leftType = env.SemanticModel.GetTypeInfo(x.Left.Node).ConvertedType |> getType
        Coalesce(left, leftType, right)
//    | BinaryExpressionKind.IsExpression -> TODO()
//    | BinaryExpressionKind.AsExpression -> TODO()
    | _ -> 
        Call(None, typ, operator, [left; right])
    |> withExprSourcePos x.Node

and transformConditionalExpression (env: Environment) (x: ConditionalExpressionData) : Expression =
    let condition = x.Condition |> transformExpression env
    let whenTrue = x.WhenTrue |> transformExpression env
    let whenFalse = x.WhenFalse |> transformExpression env
    Conditional(condition, whenTrue, whenFalse)
    |> withExprSourcePos x.Node

and transformMethodDeclaration (env: Environment) (x: MethodDeclarationData) : CSharpMethod =
    let symbol = env.SemanticModel.GetDeclaredSymbol(x.Node)
//    let attributeLists = x.AttributeLists |> Seq.map (transformAttributeList env) |> List.ofSeq
    let returnType = getType symbol.ReturnType
//    let explicitInterfaceSpecifier = x.ExplicitInterfaceSpecifier |> Option.map (transformExplicitInterfaceSpecifier env)
//    let typeParameterList = x.TypeParameterList |> Option.map (transformTypeParameterList env)
    let parameterList = getParameters symbol // x.ParameterList |> transformParameterList env
//    let constraintClauses = x.ConstraintClauses |> Seq.map (transformTypeParameterConstraintClause env) |> List.ofSeq
    for p in parameterList do
        env.Parameters.Add(p.Symbol, p.ParameterId)
    let body = 
        match x.Body |> Option.map (transformBlock env) with
        | Some b -> b
        | _ -> 
        match x.ExpressionBody |> Option.map (transformArrowExpressionClause env) with
        | Some v -> Block [ Return v ]
        | _ -> Empty

    {
        IsStatic = symbol.IsStatic
        Parameters = parameterList
        Body = body
        IsAsync = symbol.IsAsync
        ReturnType = returnType
    }

and transformParameterList (env: Environment) (x: ParameterListData) : list<CSharpParameter> =
    x.Parameters |> Seq.map (transformParameter env) |> List.ofSeq

and transformParameter (env: Environment) (x: ParameterData) : CSharpParameter =
//    let attributeLists = x.AttributeLists |> Seq.map (transformAttributeList env) |> List.ofSeq
    let symbol = env.SemanticModel.GetDeclaredSymbol(x.Node)
    let typ = getType symbol.Type
    let default_ = x.Default |> Option.map (transformEqualsValueClause env)
    let id =
        match x.Identifier with
        | ParameterIdentifier.IdentifierToken t -> Id.New t
        | _ -> failwith "transformParameter: arglist__ not supported"
    {
        ParameterId = id
        Symbol = symbol
        DefaultValue = default_
        Type = typ
        RefOrOut = symbol.RefKind <> RefKind.None
    }

and transformArrowExpressionClause (env: Environment) (x: ArrowExpressionClauseData) : Expression =
    x.Expression |> transformExpression env

and transformReturnStatement (env: Environment) (x: ReturnStatementData) : Statement =
    x.Expression |> Option.map (transformExpression env)
    |> Option.fill Undefined |> Return

and transformObjectCreationExpression (env: Environment) (x: ObjectCreationExpressionData) : _ =
    let symbol = env.SemanticModel.GetSymbolInfo(x.Node).Symbol :?> IMethodSymbol
    let typ = getNamedType symbol.ContainingType //x.Type |> transformType env
    let argumentList = x.ArgumentList |> Option.map (transformArgumentList env) |> Option.fill []
    let initializer = x.Initializer |> Option.map (transformInitializerExpression env)
    Ctor (typ, getConstructor symbol, argumentList)

and transformInitializerExpression (env: Environment) (x: InitializerExpressionData) : _ =
    let expressions = x.Expressions |> Seq.map (transformExpression env) |> List.ofSeq
    match x.Kind with
    | InitializerExpressionKind.ObjectInitializerExpression -> TODO()
    | InitializerExpressionKind.CollectionInitializerExpression -> TODO()
    | InitializerExpressionKind.ArrayInitializerExpression ->
        NewArray expressions
    | InitializerExpressionKind.ComplexElementInitializerExpression -> TODO()

and transformConstructorDeclaration (env: Environment) (x: ConstructorDeclarationData) : _ =
//    let attributeLists = x.AttributeLists |> Seq.map (transformAttributeList env) |> List.ofSeq
    let parameterList = x.ParameterList |> transformParameterList env
    let initializer = x.Initializer |> Option.map (transformConstructorInitializer env)
    let body = x.Body |> Option.map (transformBlock env)
    {
        Parameters = parameterList
        Body = body |> Option.fill Empty
        Initializer = initializer
    }

and transformConstructorInitializer (env: Environment) (x: ConstructorInitializerData) : _ =
    let argumentList = x.ArgumentList |> transformArgumentList env
    match x.Kind with
    | ConstructorInitializerKind.BaseConstructorInitializer -> BaseInitializer argumentList
    | ConstructorInitializerKind.ThisConstructorInitializer -> ThisInitializer argumentList

and transformAccessorDeclaration (env: Environment) (x: AccessorDeclarationData) : _ =
//    let attributeLists = x.AttributeLists |> Seq.map (transformAttributeList env) |> List.ofSeq
    let symbol = env.SemanticModel.GetDeclaredSymbol(x.Node)
    let returnType = getType symbol.ReturnType
    let parameterList = getParameters symbol // x.ParameterList |> transformParameterList env
    for p in parameterList do
        env.Parameters.Add(p.Symbol, p.ParameterId)
    let body = x.Body |> Option.map (transformBlock env) |> Option.fill Empty
    match x.Kind with
    | AccessorDeclarationKind.GetAccessorDeclaration      
    | AccessorDeclarationKind.SetAccessorDeclaration -> 
        {
            IsStatic = symbol.IsStatic
            Parameters = parameterList
            Body = body
            IsAsync = symbol.IsAsync
            ReturnType = returnType
        }
    | AccessorDeclarationKind.AddAccessorDeclaration -> TODO()
    | AccessorDeclarationKind.RemoveAccessorDeclaration -> TODO()
    | AccessorDeclarationKind.UnknownAccessorDeclaration -> TODO()

and transformWhileStatement (env: Environment) (x: WhileStatementData) : _ =
    let condition = x.Condition |> transformExpression env
    let statement = x.Statement |> transformStatement env
    While(condition, statement)
    |> withStatementSourcePos x.Node

and transformDoStatement (env: Environment) (x: DoStatementData) : _ =
    let statement = x.Statement |> transformStatement env
    let condition = x.Condition |> transformExpression env
    DoWhile(statement, condition)
    |> withStatementSourcePos x.Node

and transformForStatement (env: Environment) (x: ForStatementData) : _ =
    let declaration = x.Declaration |> Option.map (transformVariableDeclaration env)
    let initializers = x.Initializers |> Seq.map (transformExpression env) |> List.ofSeq
    let condition = x.Condition |> Option.map (transformExpression env)
    let incrementors = x.Incrementors |> Seq.map (transformExpression env) |> List.ofSeq
    let statement = x.Statement |> transformStatement env
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

and transformPostfixUnaryExpression (env: Environment) (x: PostfixUnaryExpressionData) : _ =
    let operand = x.Operand |> transformExpression env
//    match x.Kind with
//    | PostfixUnaryExpressionKind.PostIncrementExpression -> TODO()
//    | PostfixUnaryExpressionKind.PostDecrementExpression -> TODO()
//    TODO()
    let symbol = env.SemanticModel.GetSymbolInfo(x.Node).Symbol.OriginalDefinition :?> IMethodSymbol
    let typ = getNamedType symbol.ContainingType
    let ma = symbol.TypeArguments |> Seq.map getType |> List.ofSeq
    let meth = concrete (getMethod symbol, ma)
    Call(None, typ, meth, [ operand ])
    |> withExprSourcePos x.Node

and transformPrefixUnaryExpression (env: Environment) (x: PrefixUnaryExpressionData) : _ =
    let operand = x.Operand |> transformExpression env
//    match x.Kind with
//    | PrefixUnaryExpressionKind.UnaryPlusExpression -> TODO()
//    | PrefixUnaryExpressionKind.UnaryMinusExpression -> TODO()
//    | PrefixUnaryExpressionKind.BitwiseNotExpression -> TODO()
//    | PrefixUnaryExpressionKind.LogicalNotExpression -> TODO()
//    | PrefixUnaryExpressionKind.PreIncrementExpression -> TODO()
//    | PrefixUnaryExpressionKind.PreDecrementExpression -> TODO()
//    | PrefixUnaryExpressionKind.AddressOfExpression -> TODO()
//    | PrefixUnaryExpressionKind.PointerIndirectionExpression -> TODO()
//    TODO()
    let symbol = env.SemanticModel.GetSymbolInfo(x.Node).Symbol.OriginalDefinition :?> IMethodSymbol
    let typ = getNamedType symbol.ContainingType
    let ma = symbol.TypeArguments |> Seq.map getType |> List.ofSeq
    let meth = concrete (getMethod symbol, ma)
    Call(None, typ, meth, [ operand ])
    |> withExprSourcePos x.Node

and transformUsingStatement (env: Environment) (x: UsingStatementData) : _ =
    let declaration = x.Declaration |> Option.map (transformVariableDeclaration env)
    let expression = x.Expression |> Option.map (transformExpression env)
    let statement = x.Statement |> transformStatement env
    TODO()

and transformTryStatement (env: Environment) (x: TryStatementData) : _ =
    let block = x.Block |> transformBlock env
    let err = Id.New "err"
    let catches = x.Catches |> Seq.map (transformCatchClause (env.WithCaught(err))) |> List.ofSeq
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

    let finally_ = x.Finally |> Option.map (transformFinallyClause env)
    let tryWith =
        match body with
        | Some (err, b) ->
            TryWith(block, err, b)
        | None -> block
    match finally_ with
    | Some f -> TryFinally(f, tryWith)
    | None -> tryWith  
    |> withStatementSourcePos x.Node

and transformCatchClause (env: Environment) (x: CatchClauseData) : _ =
    let declaration = x.Declaration |> Option.map (transformCatchDeclaration env)
    match declaration with
    | Some (symbol, typ) ->
        let err = env.Caught.Value    
        env.Vars.Add(symbol, err)
        let filter = x.Filter |> Option.map (transformCatchFilterClause env)
        let typeCheck = TypeCheck(Var err, typ) 
        let cond = 
            match filter with
            | Some f -> Conditional(typeCheck, f, Value (Bool false))
            | None -> typeCheck
        let block = x.Block |> transformBlock env
        Some cond, block
    | None ->
        let block = x.Block |> transformBlock env
        None, block        

and transformCatchDeclaration (env: Environment) (x: CatchDeclarationData) : _ =
//    let typ = x.Type |> transformType env
    let symbol = env.SemanticModel.GetSymbolInfo(x.Node).Symbol :?> ILocalSymbol
    let typ = getType symbol.Type
    symbol, typ

and transformCatchFilterClause (env: Environment) (x: CatchFilterClauseData) : _ =
    x.FilterExpression |> transformExpression env

and transformFinallyClause (env: Environment) (x: FinallyClauseData) : _ =
    x.Block |> transformBlock env

and transformForEachStatement (env: Environment) (x: ForEachStatementData) : _ =
    let typ = x.Type |> transformType env
    let expression = x.Expression |> transformExpression env
    let statement = x.Statement |> transformStatement env
    TODO()

and transformAnonymousFunctionExpression (env: Environment) (x: AnonymousFunctionExpressionData) : _ =
    match x with
    | AnonymousFunctionExpressionData.LambdaExpression          x -> transformLambdaExpression env x
    | AnonymousFunctionExpressionData.AnonymousMethodExpression x -> TODO() //transformAnonymousMethodExpression env x

and transformLambdaExpression (env: Environment) (x: LambdaExpressionData) : _ =
    match x with
    | LambdaExpressionData.SimpleLambdaExpression        x -> transformSimpleLambdaExpression env x
    | LambdaExpressionData.ParenthesizedLambdaExpression x -> transformParenthesizedLambdaExpression env x

and transformSimpleLambdaExpression (env: Environment) (x: SimpleLambdaExpressionData) : _ =
    let parameter = x.Parameter |> transformParameter env
    // TODO ref and out params
    let id = Id.New parameter.ParameterId.Name.Value              
    env.Parameters.Add(parameter.Symbol, id)
    let body = x.Body |> transformCSharpNode env
    Function([id], body)
    
and transformParenthesizedLambdaExpression (env: Environment) (x: ParenthesizedLambdaExpressionData) : _ =
    let parameterList = x.ParameterList |> transformParameterList env
    // TODO ref and out params
    let ids =
        parameterList |> List.map (fun p -> 
            let id = Id.New p.ParameterId.Name.Value
            env.Parameters.Add(p.Symbol, id)
            id
        )              
    let body = x.Body |> transformCSharpNode env
    Function(ids, body)

and transformCSharpNode (env: Environment) (x: CSharpNodeData) : _ =
    match x with
    | CSharpNodeData.Expression                      x -> transformExpression env x |> Return
    | CSharpNodeData.BaseArgumentList                x -> TODO() //transformBaseArgumentList env x
    | CSharpNodeData.QueryClause                     x -> TODO() //transformQueryClause env x
    | CSharpNodeData.SelectOrGroupClause             x -> TODO() //transformSelectOrGroupClause env x
    | CSharpNodeData.InterpolatedStringContent       x -> TODO() //transformInterpolatedStringContent env x
    | CSharpNodeData.Statement                       x -> transformStatement env x
    | CSharpNodeData.SwitchLabel                     x -> TODO() //transformSwitchLabel env x
    | CSharpNodeData.MemberDeclaration               x -> TODO() //transformMemberDeclaration env x
    | CSharpNodeData.BaseType                        x -> TODO() //transformBaseType env x
    | CSharpNodeData.TypeParameterConstraint         x -> TODO() //transformTypeParameterConstraint env x
    | CSharpNodeData.BaseParameterList               x -> TODO() //transformBaseParameterList env x
    | CSharpNodeData.Cref                            x -> TODO() //transformCref env x
    | CSharpNodeData.BaseCrefParameterList           x -> TODO() //transformBaseCrefParameterList env x
    | CSharpNodeData.XmlNode                         x -> TODO() //transformXmlNode env x
    | CSharpNodeData.XmlAttribute                    x -> TODO() //transformXmlAttribute env x
    | CSharpNodeData.TypeArgumentList                x -> TODO() //transformTypeArgumentList env x
    | CSharpNodeData.ArrayRankSpecifier              x -> TODO() //transformArrayRankSpecifier env x
    | CSharpNodeData.Argument                        x -> TODO() //transformArgument env x
    | CSharpNodeData.NameColon                       x -> TODO() //transformNameColon env x
    | CSharpNodeData.AnonymousObjectMemberDeclarator x -> TODO() //transformAnonymousObjectMemberDeclarator env x
    | CSharpNodeData.QueryBody                       x -> TODO() //transformQueryBody env x
    | CSharpNodeData.JoinIntoClause                  x -> TODO() //transformJoinIntoClause env x
    | CSharpNodeData.Ordering                        x -> TODO() //transformOrdering env x
    | CSharpNodeData.QueryContinuation               x -> TODO() //transformQueryContinuation env x
    | CSharpNodeData.InterpolationAlignmentClause    x -> TODO() //transformInterpolationAlignmentClause env x
    | CSharpNodeData.InterpolationFormatClause       x -> TODO() //transformInterpolationFormatClause env x
    | CSharpNodeData.VariableDeclaration             x -> TODO() //transformVariableDeclaration env x
    | CSharpNodeData.VariableDeclarator              x -> TODO() //transformVariableDeclarator env x
    | CSharpNodeData.EqualsValueClause               x -> TODO() //transformEqualsValueClause env x
    | CSharpNodeData.ElseClause                      x -> TODO() //transformElseClause env x
    | CSharpNodeData.SwitchSection                   x -> TODO() //transformSwitchSection env x
    | CSharpNodeData.CatchClause                     x -> TODO() //transformCatchClause env x
    | CSharpNodeData.CatchDeclaration                x -> TODO() //transformCatchDeclaration env x
    | CSharpNodeData.CatchFilterClause               x -> TODO() //transformCatchFilterClause env x
    | CSharpNodeData.FinallyClause                   x -> TODO() //transformFinallyClause env x
    | CSharpNodeData.CompilationUnit                 x -> TODO() //transformCompilationUnit env x
//    | CSharpNodeData.ExternAliasDirective            x -> TODO() //transformExternAliasDirective env x
//    | CSharpNodeData.UsingDirective                  x -> TODO() //transformUsingDirective env x
//    | CSharpNodeData.AttributeList                   x -> TODO() //transformAttributeList env x
    | CSharpNodeData.AttributeTargetSpecifier        x -> TODO() //transformAttributeTargetSpecifier env x
    | CSharpNodeData.Attribute                       x -> TODO() //transformAttribute env x
    | CSharpNodeData.AttributeArgumentList           x -> TODO() //transformAttributeArgumentList env x
    | CSharpNodeData.AttributeArgument               x -> TODO() //transformAttributeArgument env x
    | CSharpNodeData.NameEquals                      x -> TODO() //transformNameEquals env x
    | CSharpNodeData.TypeParameterList               x -> TODO() //transformTypeParameterList env x
    | CSharpNodeData.TypeParameter                   x -> TODO() //transformTypeParameter env x
    | CSharpNodeData.BaseList                        x -> TODO() //transformBaseList env x
//    | CSharpNodeData.TypeParameterConstraintClause   x -> TODO() //transformTypeParameterConstraintClause env x
    | CSharpNodeData.ExplicitInterfaceSpecifier      x -> TODO() //transformExplicitInterfaceSpecifier env x
    | CSharpNodeData.ConstructorInitializer          x -> TODO() //transformConstructorInitializer env x
    | CSharpNodeData.ArrowExpressionClause           x -> TODO() //transformArrowExpressionClause env x
    | CSharpNodeData.AccessorList                    x -> TODO() //transformAccessorList env x
    | CSharpNodeData.AccessorDeclaration             x -> TODO() //transformAccessorDeclaration env x
    | CSharpNodeData.Parameter                       x -> TODO() //transformParameter env x
    | CSharpNodeData.CrefParameter                   x -> TODO() //transformCrefParameter env x
    | CSharpNodeData.XmlElementStartTag              x -> TODO() //transformXmlElementStartTag env x
    | CSharpNodeData.XmlElementEndTag                x -> TODO() //transformXmlElementEndTag env x
    | CSharpNodeData.XmlName                         x -> TODO() //transformXmlName env x
    | CSharpNodeData.XmlPrefix                       x -> TODO() //transformXmlPrefix env x

and transformInstanceExpression (env: Environment) (x: InstanceExpressionData) : _ =
    match x with
    | InstanceExpressionData.ThisExpression x -> TODO() //transformThisExpression env x
    | InstanceExpressionData.BaseExpression x -> TODO() //transformBaseExpression env x

and transformMemberAccessExpression (env: Environment) (x: MemberAccessExpressionData) : _ =
    let symbol = env.SemanticModel.GetSymbolInfo(x.Node).Symbol.OriginalDefinition
    match symbol with
    | :? IPropertySymbol as symbol ->
        let typ = getNamedType symbol.ContainingType
        let getM = symbol.GetMethod
        let ma = getM.TypeArguments |> Seq.map getType |> List.ofSeq
        let meth = concrete (getMethod getM, ma)
        if symbol.IsStatic then
            Call(None, typ, meth, []) // TODO property indexers
        else
            let expression = x.Expression |> transformExpression env
            Call (Some expression, typ, meth, [])        
    | :? IMethodSymbol as symbol ->
        // TODO: this works for invocations but not always
        let expression = x.Expression |> transformExpression env
        expression
    | _ ->      
    let expression = x.Expression |> transformExpression env
    let name = x.Name |> transformSimpleName env
    match x.Kind with
    | MemberAccessExpressionKind.SimpleMemberAccessExpression -> TODO()
    | MemberAccessExpressionKind.PointerMemberAccessExpression -> TODO()

and transformConditionalAccessExpression (env: Environment) (x: ConditionalAccessExpressionData) : _ =
    let expression = x.Expression |> transformExpression env
    let whenNotNull = x.WhenNotNull |> transformExpression env
    let id = Id.New ()
//    Let(id, expression, Conditional(id ))
    TODO()

and transformMemberBindingExpression (env: Environment) (x: MemberBindingExpressionData) : _ =
    let name = x.Name |> transformSimpleName env
    TODO()

and transformArrayCreationExpression (env: Environment) (x: ArrayCreationExpressionData) : _ =
//    let type_ = x.Type |> transformArrayType env
    let initializer = x.Initializer |> Option.map (transformInitializerExpression env)
    initializer |> Option.fill (NewArray [])

and transformImplicitArrayCreationExpression (env: Environment) (x: ImplicitArrayCreationExpressionData) : _ =
    let initializer = x.Initializer |> transformInitializerExpression env
    initializer

and transformTypeOfExpression (env: Environment) (x: TypeOfExpressionData) : _ =
    let type_ = x.Type |> transformType env
    TODO()

and transformAwaitExpression (env: Environment) (x: AwaitExpressionData) : _ =
    let expression = x.Expression |> transformExpression env
    Await expression

and transformContinueStatement (env: Environment) (x: ContinueStatementData) : _ =
    Continue None

and transformThrowStatement (env: Environment) (x: ThrowStatementData) : _ =
    let expression = x.Expression |> Option.map (transformExpression env)
    match expression with
    | Some e -> Throw e
    | None -> Throw (Var env.Caught.Value)

and transformYieldStatement (env: Environment) (x: YieldStatementData) : _ =
    let expression = x.Expression |> Option.map (transformExpression env)
    Yield expression
//    match x.Kind with
//    | YieldStatementKind.YieldReturnStatement -> TODO()
//    | YieldStatementKind.YieldBreakStatement -> TODO()
//    TODO()
