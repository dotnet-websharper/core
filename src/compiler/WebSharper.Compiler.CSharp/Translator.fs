module WebSharper.Compiler.CSharp.Translator

//open Microsoft.FSharp.Compiler.SourceCodeServices
open Microsoft.CodeAnalysis.CSharp
open System.Collections.Generic

open WebSharper.Core.AST

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
                            if m.ReturnType.AssemblyQualifiedName.StartsWith "System.Collections.Generic.IEnumerable" then
                                // check if has return or yield
                                let b = m.Body |> Continuation.FreeNestedGotos().TransformStatement
                                let labels = Continuation.CollectLabels.Collect b
                                let b = Continuation.GeneratorTransformer(labels).TransformMethodBody(b)
                                { m with Body = b }
                            elif m.IsAsync then
                                let b = 
                                    m.Body 
                                    |> Continuation.AwaitTransformer().TransformStatement 
                                    |> breakStatement
                                    |> Continuation.FreeNestedGotos().TransformStatement
                                let labels = Continuation.CollectLabels.Collect b
                                let b = Continuation.AsyncTransformer(labels).TransformMethodBody(b)
                                { m with Body = b }
                            else m
                        match syntax with
                        | :? MethodDeclarationSyntax as syntax ->
                            syntax
                            |> RoslynHelpers.MethodDeclarationData.FromNode 
                            |> ToCSharpAST.transformMethodDeclaration (ToCSharpAST.Environment.New (model, comp))   
                        | :? AccessorDeclarationSyntax as syntax ->
                            syntax
                            |> RoslynHelpers.AccessorDeclarationData.FromNode 
                            |> ToCSharpAST.transformAccessorDeclaration (ToCSharpAST.Environment.New (model, comp))   
//                        | :? ArrowExpressionClauseSyntax as syntax ->
//                            syntax
//                            |> RoslynHelpers.ArrowExpressionClauseData.FromNode 
//                            |> ToCSharpAST.transformArrowExpressionClause (ToCSharpAST.Environment.New (model, comp))   
                        | :? ConstructorDeclarationSyntax as syntax ->
                            let c =
                                syntax
                                |> RoslynHelpers.ConstructorDeclarationData.FromNode 
                                |> ToCSharpAST.transformConstructorDeclaration (ToCSharpAST.Environment.New (model, comp))   
                            {
                                IsStatic = meth.IsStatic
                                Parameters = []
                                Body = c.Body
                                IsAsync = false
                                ReturnType = Unchecked.defaultof<Type>
                            } : ToCSharpAST.CSharpMethod
                        | _ -> failwithf "Not recognized method syntax kind: %A" (syntax.Kind())
                        |> fixMethod
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
                    {
                        IsStatic = true
                        Parameters = []
                        Body = Empty
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
                let jsMethod isInline =
                    addMethod mAnnot mdef (if isInline then N.Inline else getKind()) false (getBody isInline)
                let checkNotAbstract() =
                    if meth.IsAbstract then
                        failwith "Asctract methods cannot be marked with Inline or Macro."
                match kind with
                | A.MemberKind.NoFallback ->
                    checkNotAbstract()
                    addMethod mAnnot mdef N.NoFallback true Undefined
                | A.MemberKind.Inline js ->
                    checkNotAbstract() 
                    let parsed = WebSharper.Compiler.Recognize.createInline None (getVars()) js
                    addMethod mAnnot mdef N.Inline true parsed
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
                | _ -> failwith "invalid method kind"
            | Member.StaticConstructor ->
                clsMembers.Add (NotResolvedMember.StaticConstructor (getBody false))
        | _ -> ()

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

let transformAssembly (refMeta : Metadata) (rcomp: CSharpCompilation) =   
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

