module WebSharper.Compiler.CSharp.Translator

//open Microsoft.FSharp.Compiler.SourceCodeServices
open Microsoft.CodeAnalysis.CSharp
open System.Collections.Generic

open WebSharper.Core.AST
open WebSharper.Core.Utilities

open Microsoft.CodeAnalysis
open Microsoft.CodeAnalysis.CSharp
open Microsoft.CodeAnalysis.CSharp.Syntax

open WebSharper.Core.Metadata

module A = WebSharper.Compiler.AttributeReader
type private N = NotResolvedMemberKind

type CSharpAttributeReader() =
    inherit A.AttributeReader<AttributeData>()
    override this.GetAssemblyName attr = attr.AttributeClass.ContainingAssembly.Name
    override this.GetName attr = attr.AttributeClass.Name
    override this.GetCtorArgs attr = attr.ConstructorArguments |> Seq.map (fun a -> a.Value) |> Array.ofSeq          
    override this.GetTypeDef o = ToCSharpAST.getNamedTypeDefinition (o :?> INamedTypeSymbol)

let attrReader = CSharpAttributeReader()

let rec getAllTypeMembers (n: INamespaceSymbol) =
    let rec getWithNested (t: INamedTypeSymbol) =
        Seq.append
            (Seq.singleton t)
            (t.GetTypeMembers() |> Seq.collect getWithNested)     
    if n.Name = "System" then Seq.empty else
    Seq.append 
        (n.GetTypeMembers() |> Seq.collect getWithNested)
        (n.GetNamespaceMembers() |> Seq.collect getAllTypeMembers)

let transformInterface (intf: INamedTypeSymbol) =
    if intf.TypeKind <> TypeKind.Interface then None else
    let methodNames = ResizeArray()
    let annot =
        attrReader.GetTypeAnnot(A.TypeAnnotation.Empty, intf.GetAttributes()) // TODO inherited attributes
    let def =
        match annot.ProxyOf with
        | Some d -> d 
        | _ -> ToCSharpAST.getNamedTypeDefinition intf
    for m in intf.GetMembers() do
        let mAnnot = attrReader.GetMemberAnnot(annot, m.GetAttributes())
//        let n = 
//            m |> getAttributes 
//            |> Seq.tryPick (function Name n -> Some n | _ -> None) 
//            |> Option.fallback (fun () -> intfName + m.Name)
        let md = 
            match ToCSharpAST.getMember (m :?> IMethodSymbol) with
            | Member.Method (_, md) -> md
            | _ -> failwith "invalid interface member"
        methodNames.Add(md, mAnnot.Name)
    Some (def, 
        {
            StrongName = annot.Name
            Extends = intf.Interfaces |> Seq.map (fun i -> ToCSharpAST.getNamedTypeDefinition i) |> List.ofSeq
            NotResolvedMethods = List.ofSeq methodNames 
        }
    )

let remotingCode = ref -1 

let initDef =
    Hashed {
        MethodName = "$init"
        Parameters = []
        ReturnType = VoidType
        Generics = 0
    }

let staticInitDef =
    Hashed {
        MethodName = "$staticInit"
        Parameters = []
        ReturnType = VoidType
        Generics = 0
    }

let transformClass (rcomp: CSharpCompilation) (comp: Compilation) (cls: INamedTypeSymbol) =
    if cls.TypeKind <> TypeKind.Class then None else

    let annot = 
        // TODO nested
        attrReader.GetTypeAnnot(A.TypeAnnotation.Empty, cls.GetAttributes())

    let clsMembers = ResizeArray()

    let thisDef = ToCSharpAST.getNamedTypeDefinition cls
    let def =
        match annot.ProxyOf with
        | Some p -> 
            comp.AddProxy(thisDef, p)
            // TODO: warn about public proxy type
            p
        | _ -> thisDef

    let members = cls.GetMembers()

    let getUnresolved (mAnnot: A.MemberAnnotation) kind compiled expr = 
            {
                Kind = kind
                StrongName = mAnnot.Name
                Macros = mAnnot.Macros
                Generator = 
                    match mAnnot.Kind with
                    | Some (A.MemberKind.Generated (g, p)) -> Some (g, p)
                    | _ -> None
                Compiled = compiled 
                Body = expr
                Requires = mAnnot.Requires
            }

    let addMethod mAnnot def kind compiled expr =
        clsMembers.Add (NotResolvedMember.Method (def, (getUnresolved mAnnot kind compiled expr)))
        
    let addConstructor mAnnot def kind compiled expr =
        clsMembers.Add (NotResolvedMember.Constructor (def, (getUnresolved mAnnot kind compiled expr)))

    let inits = ResizeArray()                                               
    let staticInits = ResizeArray()                                               
            
    for mem in members do
        match mem with
        | :? IPropertySymbol as p ->
            let pAnnot = attrReader.GetMemberAnnot(annot, p.GetMethod.GetAttributes())
            match pAnnot.Kind with
            | Some A.MemberKind.JavaScript ->
                let decls = p.DeclaringSyntaxReferences
                if decls.Length = 0 then () else
                let syntax = decls.[0].GetSyntax()
                let model = rcomp.GetSemanticModel(syntax.SyntaxTree, true)
                let data =
                    syntax :?> PropertyDeclarationSyntax
                    |> RoslynHelpers.PropertyDeclarationData.FromNode
                match data.Initializer with
                | Some i ->
                    let b = i |> ToCSharpAST.transformEqualsValueClause (ToCSharpAST.Environment.New (model, comp))
                    let setter = ToCSharpAST.getMethod p.SetMethod
                    if p.IsStatic then
                        staticInits.Add <| Call(None, concrete (def, []), concrete (setter, []), [ b ])
                    else
                        inits.Add <| Call(Some This, concrete (def, []), concrete (setter, []), [ b ])
                | None -> ()
            | _ -> ()
        | :? IFieldSymbol as f -> 
            let fAnnot = attrReader.GetMemberAnnot(annot, f.GetAttributes())
            // TODO: check multiple declarations on the same line
            match fAnnot.Kind with
            | Some A.MemberKind.JavaScript ->
                let decls = f.DeclaringSyntaxReferences
                if decls.Length = 0 then () else
                let syntax = decls.[0].GetSyntax()
                let model = rcomp.GetSemanticModel(syntax.SyntaxTree, true)
                match syntax with
                | :? VariableDeclaratorSyntax as v ->
                    let x, e =
                        RoslynHelpers.VariableDeclaratorData.FromNode v
                        |> ToCSharpAST.transformVariableDeclarator (ToCSharpAST.Environment.New (model, comp))
                    
                    if f.IsStatic then 
                        staticInits.Add <| FieldSet(None, concrete (def, []), x.Name.Value, e)
                    else
                        inits.Add <| FieldSet(Some This, concrete (def, []), x.Name.Value, e)
                | _ -> 
//                    let _ = syntax :?> VariableDeclarationSyntax
                    ()
            | _ -> ()
        | _ -> ()

    let hasInit =
        if inits.Count = 0 then false else 
        Function([], ExprStatement (Sequential (inits |> List.ofSeq)))
        |> addMethod A.MemberAnnotation.BasicJavaScript initDef N.Instance false
        true

    let hasStaticInit =
        if staticInits.Count = 0 then false else
        Function([], ExprStatement (Sequential (staticInits |> List.ofSeq)))
        |> addMethod A.MemberAnnotation.BasicJavaScript staticInitDef N.Static false
        true

    for meth in members.OfType<IMethodSymbol>() do
        let mAnnot = attrReader.GetMemberAnnot(annot, meth.GetAttributes())

//        let syntaxAndModel = 
//            lazy
//            let syntax = meth.DeclaringSyntaxReferences.[0].GetSyntax()
//            syntax, rcomp.GetSemanticModel(syntax.SyntaxTree, true)


//        let getVarsAndThis() =
//            let a, t =
//                args |> List.concat
//                |> function
//                | t :: r when t.IsMemberThisValue || t.IsConstructorThisValue -> r, Some t
//                | a -> a, None
//
//            let a = 
//                a
//                |> function 
//                | [ u ] when ToFSharpAST.isUnit u.FullType -> []
//                | a -> a
//
//            a |> List.map (fun p -> Id.New p.CompiledName),
//            t |> Option.map (fun p -> Id.New p.CompiledName)


        match mAnnot.Kind with
        | Some A.MemberKind.Stub -> ()
        | Some A.MemberKind.Remote -> 
            let def = ToCSharpAST.getMember meth
            match def with
            | Member.Method (isInstance, mdef) ->
                let memberScope =
                    if isInstance then InstanceMember else StaticMember
                let remotingKind =
                    match mdef.Value.ReturnType with
                    | VoidType -> RemoteSend
                    | ConcreteType { Entity = e } when e.Value.FullName = "Microsoft.FSharp.Control.FSharpAsync`1" -> RemoteAsync
                    | ConcreteType { Entity = e } when e.Value.FullName.StartsWith "System.Threading.Tasks.Task" -> RemoteTask
                    | _ -> RemoteSync // TODO: warning
                incr remotingCode
                let methodHandle =
                    {
                        Assembly = comp.AssemblyName
                        Code = !remotingCode
                    }
                addMethod mAnnot mdef (N.Remote(memberScope, remotingKind, methodHandle)) true Undefined
            | _ -> failwith "Only methods can be defined Remote"
        | Some kind ->
            let memdef = ToCSharpAST.getMember meth
            let getParsed() = 
                let decl = meth.DeclaringSyntaxReferences
                if decl.Length > 0 then
                    try
                        let syntax = meth.DeclaringSyntaxReferences.[0].GetSyntax()
                        let model = rcomp.GetSemanticModel(syntax.SyntaxTree, true)
                        let fixMethod (m: ToCSharpAST.CSharpMethod) =
                            let rec endsWithReturn s =
                                match ignoreStatementSourcePos s with
                                | Return _ -> true
                                | (Block ss | Statements ss) -> endsWithReturn (List.last ss) 
                                | _ -> false
                            let addLastReturnIfNeeded s =
                                if endsWithReturn s then s else
                                    combineStatements [ m.Body; Return Undefined ]
                            let b =
                                if m.ReturnType.AssemblyQualifiedName.StartsWith "System.Collections.Generic.IEnumerable" then
                                    // TODO: check if has return or yield
                                    let b = m.Body |> Continuation.FreeNestedGotos().TransformStatement
                                    let labels = Continuation.CollectLabels.Collect b
                                    Continuation.GeneratorTransformer(labels).TransformMethodBody(b)
                                elif m.IsAsync then
                                    let b = 
                                        m.Body |> addLastReturnIfNeeded
                                        |> Continuation.AwaitTransformer().TransformStatement 
                                        |> breakStatement
                                        |> Continuation.FreeNestedGotos().TransformStatement
                                    let labels = Continuation.CollectLabels.Collect b
                                    Continuation.AsyncTransformer(labels).TransformMethodBody(b)
                                else m.Body
                            { m with Body = b |> FixThisScope().Fix }
                        match syntax with
                        | :? MethodDeclarationSyntax as syntax ->
                            syntax
                            |> RoslynHelpers.MethodDeclarationData.FromNode 
                            |> ToCSharpAST.transformMethodDeclaration (ToCSharpAST.Environment.New (model, comp))   
                            |> fixMethod
                        | :? AccessorDeclarationSyntax as syntax ->
                            syntax
                            |> RoslynHelpers.AccessorDeclarationData.FromNode 
                            |> ToCSharpAST.transformAccessorDeclaration (ToCSharpAST.Environment.New (model, comp))   
                            |> fixMethod
                        | :? ArrowExpressionClauseSyntax as syntax ->
                            let body =
                                syntax
                                |> RoslynHelpers.ArrowExpressionClauseData.FromNode 
                                |> ToCSharpAST.transformArrowExpressionClause (ToCSharpAST.Environment.New (model, comp))   
                            {
                                IsStatic = meth.IsStatic
                                Parameters = []
                                Body = Return body
                                IsAsync = meth.IsAsync
                                ReturnType = ToCSharpAST.getType meth.ReturnType
                            } : ToCSharpAST.CSharpMethod
                            |> fixMethod
                        | :? ConstructorDeclarationSyntax as syntax ->
                            let c =
                                syntax
                                |> RoslynHelpers.ConstructorDeclarationData.FromNode 
                                |> ToCSharpAST.transformConstructorDeclaration (ToCSharpAST.Environment.New (model, comp))   
                            let b =
                                match c.Initializer with
                                | Some (ToCSharpAST.BaseInitializer (bTyp, bCtor, args)) ->
                                    combineStatements [
                                        ExprStatement <| BaseCtor(This, bTyp, bCtor, args)
                                        c.Body
                                    ]
                                | Some (ToCSharpAST.ThisInitializer (bCtor, args)) ->
                                    combineStatements [
                                        ExprStatement <| BaseCtor(This, concrete(def, []), bCtor, args)
                                        c.Body
                                    ]
                                | None -> c.Body
                            let b = 
                                if meth.IsStatic then
                                    if hasStaticInit then
                                        combineStatements [ 
                                            ExprStatement <| Call(None, concrete (def, []), concrete (staticInitDef, []), [])
                                            b
                                        ]
                                    else b
                                else
                                    match c.Initializer with
                                    | Some (ToCSharpAST.ThisInitializer _) -> b
                                    | _ ->
                                    if hasInit then 
                                        combineStatements [ 
                                            ExprStatement <| Call(Some This, concrete (def, []), concrete (initDef, []), [])
                                            b
                                        ]
                                    else b
                            {
                                IsStatic = meth.IsStatic
                                Parameters = c.Parameters
                                Body = b |> FixThisScope().Fix
                                IsAsync = false
                                ReturnType = Unchecked.defaultof<Type>
                            } : ToCSharpAST.CSharpMethod
                        | _ -> failwithf "Not recognized method syntax kind: %A" (syntax.Kind())
                    with e ->
                        comp.AddError(None, SourceError(sprintf "Error reading member '%s': %s" meth.Name e.Message))
                        {
                            IsStatic = meth.IsStatic
                            Parameters = []
                            Body = Empty
                            IsAsync = false
                            ReturnType = Unchecked.defaultof<Type>
                        } : ToCSharpAST.CSharpMethod   
                else
                    // implicit constructor
                    let b = 
                        if hasInit then 
                            ExprStatement <| Call(Some This, concrete (def, []), concrete (initDef, []), [])
                        else Empty
                    {
                        IsStatic = true
                        Parameters = []
                        Body = b
                        IsAsync = false
                        ReturnType = Unchecked.defaultof<Type>
                    } : ToCSharpAST.CSharpMethod
                    
            let getBody isInline =
                let parsed = getParsed() 
                let args = parsed.Parameters |> List.map (fun p -> p.ParameterId)
                if isInline then
//                    let vars = argsAndVars |> List.map snd
//                    let thisVar = t |> Option.map (fun p -> Id.New p.CompiledName)
//                    Block [
//                        
//                    ]
//                    List.foldBack (fun (v, h) body ->
//                        NewVar (v, h, body)    
//                    ) (args |> List.mapi (fun i a -> a, Hole i)) (ReplaceThisWithHole0().TransformStatement(parsed.Body))
                    // TODO JS+Inlines
                    failwith "TODO JS+Inline"
                else
                    Function(args, parsed.Body)

//                let a, t = getArgsAndThis()
//
//                let argsAndVars = a |> List.map (fun p -> p, Id.New p.CompiledName)
//                let env = ToFSharpAST.Environment.New (argsAndVars, meth.GenericParameters |> Seq.map (fun p -> p.Name) |> List.ofSeq)  
//                let res =
//                    let b = ToFSharpAST.transformExpression env expr |> FixThisScope().Fix       
//                    //let fix b = b |> FixThisScope().TransformExpression
//                    // TODO : startupcode only for module values
//                    if List.isEmpty args then 
//                        if isInline then
//                            b
//                        else
//                            let scDef, (scContent, scFields) = sc.Value   
//                            let name = Resolve.getRenamed meth.CompiledName scFields
//                            scContent.Add (ExprStatement (ItemSet(Self, Value (String name), b)))
//                            Function([],
//                                Block [ 
//                                    ExprStatement(Cctor scDef)
//                                    Return (FieldGet(None, concrete(scDef, []), name))
//                                ]
//                            )
//                    else
//                        let vars = argsAndVars |> List.map snd
//                        let thisVar = t |> Option.map (fun p -> Id.New p.CompiledName)
//                        if isInline then
//                            List.foldBack (fun (v, h) body ->
//                                Let (v, h, body)    
//                            ) (Option.toList thisVar @ vars |> List.mapi (fun i a -> a, Hole i)) (ReplaceThisWithHole0().TransformExpression(b))
//                        else 
//                            Function(vars, Return b)

            let getVars() =
                // TODO: do not parse method body
                let parsed = getParsed()
                parsed.Parameters |> List.map (fun p -> p.ParameterId)

            match memdef with
            | Member.Method (_, mdef) 
            | Member.Override (_, mdef) 
            | Member.Implementation (_, mdef) ->
                let getKind() =
                    match memdef with
                    | Member.Method (isInstance , _) ->
                        if isInstance then N.Instance else N.Static  
                    | Member.Override (t, _) -> N.Override t 
                    | Member.Implementation (t, _) -> N.Implementation t
                    | _ -> failwith "impossible"
                let jsMethod isInline =
                    addMethod mAnnot mdef (if isInline then N.Inline else getKind()) false (getBody isInline)
                let checkNotAbstract() =
                    if meth.IsAbstract then
                        failwith "Abstract methods cannot be marked with Inline or Macro."
                match kind with
                | A.MemberKind.NoFallback ->
                    checkNotAbstract()
                    addMethod mAnnot mdef N.NoFallback true Undefined
                | A.MemberKind.Inline js ->
                    checkNotAbstract() 
                    let parsed = WebSharper.Compiler.Recognize.createInline None (getVars()) js
                    addMethod mAnnot mdef N.Inline true parsed
                | A.MemberKind.Constant c ->
                    checkNotAbstract() 
                    addMethod mAnnot mdef N.Inline true c                        
                | A.MemberKind.Direct js ->
                    let parsed = WebSharper.Compiler.Recognize.parseDirect None (getVars()) js
                    addMethod mAnnot mdef (getKind()) true parsed
                | A.MemberKind.JavaScript ->
                    jsMethod false
                | A.MemberKind.InlineJavaScript ->
                    checkNotAbstract()
                    jsMethod true
                // TODO: optional properties
//                | A.MemberKind.OptionalField ->
//                    if meth.IsPropertyGetterMethod then
//                        let i = ItemGet(This, Value (String meth.CompiledName.[4..]))   // TODO optional
//                        addMethod mAnnot mdef N.Inline true i
//                    elif meth.IsPropertySetterMethod then  
//                        let i = ItemSet(This, Value (String meth.CompiledName.[4..]), Hole 0)   // TODO optional
//                        addMethod mAnnot mdef N.Inline true i
//                    else failwith "OptionalField attribute not on property"
                | A.MemberKind.Generated _ ->
                    addMethod mAnnot mdef (getKind()) false Undefined
                | _ -> failwith "invalid method kind"
                if mAnnot.IsEntryPoint then
                    let ep = ExprStatement <| Call(None, concrete(def, []), concrete(mdef, []), [])
                    comp.SetEntryPoint(ep)
            | Member.Constructor cdef ->
                let jsCtor isInline =   
                        if isInline then 
                            addConstructor mAnnot cdef N.Inline false (getBody true)
                        else
                            addConstructor mAnnot cdef N.Constructor false (getBody false)
                match kind with
                | A.MemberKind.NoFallback ->
                    addConstructor mAnnot cdef N.NoFallback true Undefined
                | A.MemberKind.Inline js ->
                    let parsed = WebSharper.Compiler.Recognize.createInline None (getVars()) js
                    addConstructor mAnnot cdef N.Inline true parsed 
                | A.MemberKind.Direct js ->
                    let parsed = WebSharper.Compiler.Recognize.parseDirect None (getVars()) js
                    addConstructor mAnnot cdef N.Static true parsed 
                | A.MemberKind.JavaScript -> jsCtor false
                | A.MemberKind.InlineJavaScript -> jsCtor true
                | A.MemberKind.Generated _ ->
                    addConstructor mAnnot cdef N.Static false Undefined
                | A.MemberKind.Remote _
                | A.MemberKind.Stub -> failwith "impossible"
            | Member.StaticConstructor ->
                clsMembers.Add (NotResolvedMember.StaticConstructor (getBody false))
        | _ -> 
            ()

    if hasStaticInit && not (clsMembers |> Seq.exists (function NotResolvedMember.StaticConstructor _ -> true | _ -> false)) then
        let b = Function ([], ExprStatement <| Call(None, concrete (def, []), concrete (staticInitDef, []), []))
        clsMembers.Add (NotResolvedMember.StaticConstructor b)    

    for f in members.OfType<IFieldSymbol>() do
        let mAnnot = attrReader.GetMemberAnnot(annot, f.GetAttributes())
        let nr =
            {
                StrongName = mAnnot.Name
                IsStatic = f.IsStatic
                IsOptional = mAnnot.Kind = Some A.MemberKind.OptionalField 
            }
        clsMembers.Add (NotResolvedMember.Field (f.Name, nr))    
    
    let strongName =
        annot.Name |> Option.map (fun n ->
            if n.Contains "." then n else 
                let origName = thisDef.Value.FullName
                origName.[.. origName.LastIndexOf '.'] + n
        )   

    Some (
        def,
        {
            StrongName = strongName
            BaseClass = cls.BaseType |> ToCSharpAST.getNamedTypeDefinition |> ignoreSystemObject
            Requires = annot.Requires
            Members = List.ofSeq clsMembers
            IsModule = cls.IsStatic // TODO: static classes
            IsProxy = Option.isSome annot.ProxyOf
            Macros = []
        }
    )

let transformAssembly (refMeta : Info) (rcomp: CSharpCompilation) =   
    let comp = Compilation(refMeta)

    let assembly = rcomp.Assembly

    let asmAnnot = 
        attrReader.GetAssemblyAnnot(assembly.GetAttributes())

    comp.AssemblyName <- assembly.Name
    comp.AssemblyRequires <- asmAnnot.Requires
    comp.SiteletDefinition <- asmAnnot.SiteletDefinition

    for t in getAllTypeMembers assembly.GlobalNamespace do
        transformInterface t |> Option.iter comp.AddInterface
        transformClass rcomp comp t |> Option.iter comp.AddClass
    
    comp.Resolve()

    comp

