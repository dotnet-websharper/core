module WebSharper.Compiler.FSharp.Translator

open Microsoft.FSharp.Compiler.SourceCodeServices
open System.Collections.Generic

open WebSharper.Core
open WebSharper.Core.AST
open WebSharper.Core.Metadata

module A = WebSharper.Compiler.AttributeReader

type FSIFD = FSharpImplementationFileDeclaration
type FSMFV = FSharpMemberOrFunctionOrValue

type private StartupCode = ResizeArray<Statement> * HashSet<string> 

type private N = NotResolvedMemberKind

let transformInterface (intf: FSharpEntity) =
    if not intf.IsInterface then None else
    let methodNames = ResizeArray()
    let annot =
        ToFSharpAST.attrReader.GetTypeAnnot(A.TypeAnnotation.Empty, intf.Attributes) // TODO inherited attributes
    let def =
        match annot.ProxyOf with
        | Some d -> d 
        | _ -> ToFSharpAST.getTypeDefinition intf
    for m in intf.MembersFunctionsAndValues do
        if not m.IsProperty then
            let mAnnot = ToFSharpAST.attrReader.GetMemberAnnot(annot, m.Attributes)
            let md = 
                match ToFSharpAST.getMember m with
                | Member.Method (_, md) -> md
                | _ -> failwith "invalid interface member"
            methodNames.Add(md, mAnnot.Name)
    Some (def, 
        {
            StrongName = annot.Name 
            Extends = intf.DeclaredInterfaces |> Seq.map (fun i -> ToFSharpAST.getTypeDefinition i.TypeDefinition) |> List.ofSeq
            NotResolvedMethods = List.ofSeq methodNames 
        }
    )

let remotingCode = ref -1 

let transformClass (sc: Lazy<_ * StartupCode>) (comp: Compilation) (cls: FSharpEntity) members =
    if not (cls.IsClass || cls.IsFSharpExceptionDeclaration || cls.IsFSharpModule || cls.IsFSharpRecord || cls.IsFSharpUnion) then None else

    let annot = 
        // TODO nested
        ToFSharpAST.attrReader.GetTypeAnnot(A.TypeAnnotation.Empty, cls.Attributes)

    let clsMembers = ResizeArray()

    let thisDef = ToFSharpAST.getTypeDefinition cls
    let def =
        match annot.ProxyOf with
        | Some p -> 
            comp.AddProxy(thisDef, p)
            if cls.Accessibility.IsPublic then
                // TODO: warn about public proxy type
                ()
            p
        | _ -> thisDef

    let getUnresolved (mAnnot: A.MemberAnnotation) kind compiled expr = 
            {
                Kind = kind
                StrongName = mAnnot.Name
                Macro = 
                    match mAnnot.Kind with
                    | Some (A.MemberKind.Macro (m, a, _)) -> Some (m, a)
                    | _ -> None
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

    for meth in cls.MembersFunctionsAndValues do
        if meth.IsProperty then () else
        let mAnnot = ToFSharpAST.attrReader.GetMemberAnnot(annot, meth.Attributes)
        
        if mAnnot.Kind = Some A.MemberKind.Stub || (meth.IsDispatchSlot && mAnnot.Kind = Some A.MemberKind.JavaScript) then
            match ToFSharpAST.getMember meth with
            | Member.Method (isInstance, mdef) ->
                if isInstance then              
                    addMethod mAnnot mdef N.Instance true Undefined
                else
                    addMethod mAnnot mdef N.Static true Undefined
            | Member.Constructor cdef -> // Todo stub constructors
                addConstructor mAnnot cdef N.Constructor true Undefined
            | _ -> failwith "Static method can't have Stub attribute"

    let unionOrRecord = cls.IsFSharpUnion || cls.IsFSharpRecord

    for (meth: FSMFV, args: list<list<FSMFV>>, expr: FSharpExpr) in members do
        
        if meth.IsProperty || (unionOrRecord && meth.IsCompilerGenerated) then () else
        let mAnnot = ToFSharpAST.attrReader.GetMemberAnnot(annot, meth.Attributes)
            
        let getArgsAndThis() =
            let a, t =
                args |> List.concat
                |> function
                | t :: r when t.IsMemberThisValue || t.IsConstructorThisValue -> r, Some t
                | a -> a, None

            a
            |> function 
            | [ u ] when ToFSharpAST.isUnit u.FullType -> []
            | a -> a
            , t

        let getVarsAndThis() =
            let a, t = getArgsAndThis()
            a |> List.map (fun p -> Id.New p.CompiledName),
            t |> Option.map (fun p -> Id.New p.CompiledName)
               
        match mAnnot.Kind with
        | Some A.MemberKind.Stub -> ()
        | Some A.MemberKind.Remote -> 
            let def = ToFSharpAST.getMember meth
            match def with
            | Member.Method (isInstance, mdef) ->
                let memberScope =
                    if isInstance then InstanceMember else StaticMember
                let remotingKind =
                    match mdef.Value.ReturnType with
                    | VoidType -> RemoteSend
                    | ConcreteType { Entity = e } when e.Value.FullName = "Microsoft.FSharp.Control.FSharpAsync`1" -> RemoteAsync
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
            let def = ToFSharpAST.getMember meth
//            let isModuleValue = ref false
            let getBody isInline = 
                let a, t = getArgsAndThis()

                let argsAndVars = a |> List.map (fun p -> p, Id.New p.CompiledName)
                let env = ToFSharpAST.Environment.New (argsAndVars, meth.GenericParameters |> Seq.map (fun p -> p.Name) |> List.ofSeq, comp)  
                let res =
                    let b = ToFSharpAST.transformExpression env expr |> FixThisScope().Fix       
                    //let fix b = b |> FixThisScope().TransformExpression
                    // TODO : startupcode only for module values
                    if List.isEmpty args then 
//                        isModuleValue := true
                        if isInline then
                            b
                        else
                            let scDef, (scContent, scFields) = sc.Value   
                            let name = Resolve.getRenamed meth.CompiledName scFields
                            scContent.Add (ExprStatement (ItemSet(Self, Value (String name), b)))
                            Function([],
                                Block [ 
                                    ExprStatement(Cctor scDef)
                                    Return (
                                        if meth.IsMutable then
                                            ToFSharpAST.makeByref (FieldGet(None, concrete(scDef, []), name)) (fun value -> FieldSet(None, concrete(scDef, []), name, value))
                                        else
                                            (FieldGet(None, concrete(scDef, []), name))
                                    )
                                ]
                            )
                    else
                        let vars = argsAndVars |> List.map snd
                        let thisVar = t |> Option.map (fun p -> Id.New p.CompiledName)
                        if isInline then
                            List.foldBack (fun (v, h) body ->
                                Let (v, h, body)    
                            ) (Option.toList thisVar @ vars |> List.mapi (fun i a -> a, Hole i)) (ReplaceThisWithHole0().TransformExpression(b))
                        else 
                            Function(vars, Return b)
                res
            match def with
            | Member.Method (_, mdef) 
            | Member.Override (_, mdef) 
            | Member.Implementation (_, mdef) ->
                let getKind() =
                    match def with
                    | Member.Method (isInstance , _) ->
                        if isInstance then N.Instance else N.Static  
                    | Member.Override (t, _) -> N.Override t 
                    | Member.Implementation (t, _) -> N.Implementation t
                let jsMethod isInline =
                    let kind = if isInline then N.Inline else getKind()
                    let body = getBody isInline
                    addMethod mAnnot mdef kind false body
                    // TODO: only one definition added, this is a workaround for C# compatibility
//                    if !isModuleValue then   
                    if List.isEmpty args && meth.EnclosingEntity.IsFSharpModule then
                        addMethod { mAnnot with Name = None } (Method { mdef.Value with MethodName = "get_" + mdef.Value.MethodName }) kind false body    
                let checkNotAbstract() =
                    if meth.IsDispatchSlot then
                        failwith "Asctract methods cannot be marked with Inline or Macro."
                match kind with
                | A.MemberKind.Macro (m, p, f) -> // TODO macro fallback
                    checkNotAbstract()
                    addMethod mAnnot mdef N.NoFallback true Undefined
                | A.MemberKind.Inline js ->
                    checkNotAbstract() 
                    let vars, thisVar = getVarsAndThis()
                    let parsed = WebSharper.Compiler.Recognize.createInline thisVar vars js
                    addMethod mAnnot mdef N.Inline true parsed
                    // TODO: only one definition added, this is a workaround for C# compatibility
                    if List.isEmpty args && meth.EnclosingEntity.IsFSharpModule then
                        addMethod mAnnot (Method { mdef.Value with MethodName = "get_" + mdef.Value.MethodName }) N.Inline true parsed    
                | A.MemberKind.Direct js ->
                    let vars, thisVar = getVarsAndThis()
                    let parsed = WebSharper.Compiler.Recognize.parseDirect thisVar vars js
                    addMethod mAnnot mdef (getKind()) true parsed
                | A.MemberKind.JavaScript ->
                    jsMethod false
                | A.MemberKind.InlineJavaScript ->
                    checkNotAbstract()
                    jsMethod true
                | A.MemberKind.OptionalField ->
                    if meth.IsPropertyGetterMethod then
                        let i = ItemGet(This, Value (String meth.CompiledName.[4..]))   // TODO optional
                        addMethod mAnnot mdef N.Inline true i
                    elif meth.IsPropertySetterMethod then  
                        let i = ItemSet(This, Value (String meth.CompiledName.[4..]), Hole 0)   // TODO optional
                        addMethod mAnnot mdef N.Inline true i
                    else failwith "OptionalField attribute not on property"
                | _ -> failwith "invalid method kind"
            | Member.Constructor cdef ->
                let jsCtor isInline =   
                        if isInline then 
                            addConstructor mAnnot cdef N.Inline false (getBody true)
                        else
                            addConstructor mAnnot cdef N.Constructor false (getBody false)
                match kind with
                | A.MemberKind.Macro (m, p, f) ->
                    addConstructor mAnnot cdef N.NoFallback true Undefined
                | A.MemberKind.Inline js ->
                    let vars, thisVar = getVarsAndThis()
                    let parsed = WebSharper.Compiler.Recognize.createInline thisVar vars js
                    addConstructor mAnnot cdef N.Inline true parsed 
                | A.MemberKind.Direct js ->
                    let vars, thisVar = getVarsAndThis()
                    let parsed = WebSharper.Compiler.Recognize.parseDirect thisVar vars js
                    addConstructor mAnnot cdef N.Static true parsed 
                | A.MemberKind.JavaScript -> jsCtor false
                | A.MemberKind.InlineJavaScript -> jsCtor true
                | _ -> failwith "invalid method kind"
            | Member.StaticConstructor ->
                clsMembers.Add (NotResolvedMember.StaticConstructor (getBody false))
        | _ -> ()

    for f in cls.FSharpFields do
        let mAnnot = ToFSharpAST.attrReader.GetMemberAnnot(annot, Seq.append f.FieldAttributes f.PropertyAttributes)
        let nr =
            {
                StrongName = mAnnot.Name
                IsStatic = f.IsStatic
                IsOptional = mAnnot.Kind = Some A.MemberKind.OptionalField 
            }
        clsMembers.Add (NotResolvedMember.Field (f.Name, nr))    

    let notTranslated =
        not annot.IsJavaScript 
        && not (cls.IsFSharpExceptionDeclaration || cls.IsFSharpRecord || cls.IsFSharpUnion)
        && clsMembers |> Seq.forall (function NotResolvedMember.Field _ -> true | _ -> false)

    if notTranslated then None else

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
            BaseClass = cls.BaseType |> Option.bind (fun t -> t.TypeDefinition |> ToFSharpAST.getTypeDefinition |> ignoreSystemObject)
            Requires = annot.Requires
            Members = List.ofSeq clsMembers
            IsModule = cls.IsFSharpModule
        }
    )

let transformAssembly (refMeta : Metadata) assemblyName (checkResults: FSharpCheckProjectResults) =   
    
    let comp = Compilation(refMeta)

    let asmAnnot = 
        ToFSharpAST.attrReader.GetAssemblyAnnot(checkResults.AssemblySignature.Attributes)

    comp.AssemblyName <- assemblyName
    comp.AssemblyRequires <- asmAnnot.Requires
    comp.SiteletDefinition <- asmAnnot.SiteletDefinition
                                             
    for file in checkResults.AssemblyContents.ImplementationFiles do
        let sc =
            lazy
            let fileName =
                match file.Declarations.Head with
                | FSIFD.Entity (a, _) -> a.DeclarationLocation.FileName
                | FSIFD.MemberOrFunctionOrValue (_, _, c) -> c.Range.FileName
                | FSIFD.InitAction a -> a.Range.FileName
            let name = "StartupCode$" + assemblyName.Replace('.', '_') + "$" + (System.IO.Path.GetFileNameWithoutExtension fileName).Replace('.', '_')
            let def =
                TypeDefinition {
                    Assembly = assemblyName
                    FullName = name
                }
            def, 
            (ResizeArray(), HashSet() : StartupCode)

        let typesWithMembers = Dictionary<FSharpEntity, ResizeArray<_>>()
        let rec getTypesWithMembers (sc: Lazy<_ * StartupCode>) d =
            match d with
            | FSIFD.Entity (a, b) ->
                if not a.IsFSharpAbbreviation then
                    typesWithMembers.Add (a, ResizeArray())
                    b |> List.iter (getTypesWithMembers sc)
            | FSIFD.MemberOrFunctionOrValue (a, b, c) -> 
                    typesWithMembers.[a.EnclosingEntity].Add(a, b, c)
            | FSIFD.InitAction a ->
                ()  
                // TODO: [<JavaScript>] on whole assembly.
                // TOOD: module "do"-s if file contents structure would be exposed by FCS         
//                let e = ToFSharpAST.transformExpression ToFSharpAST.Environment.Empty a
//                (sc.Value |> snd |> fst).Add (ExprStatement e)

        file.Declarations |> Seq.iter (getTypesWithMembers sc)

        for (KeyValue(c, m)) in typesWithMembers do
            transformInterface c |> Option.iter comp.AddInterface
            transformClass sc comp c m |> Option.iter comp.AddClass
            
        let getStartupCodeClass (def: TypeDefinition, sc: StartupCode) =
            let statements, fields = sc            
            let cctor = Function ([], Block (List.ofSeq statements))
            let members =
                [
                    for f in fields -> 
                        NotResolvedMember.Field(f, 
                            {
                                StrongName = None
                                IsStatic = true
                                IsOptional = false
                            } 
                        )
                    yield NotResolvedMember.StaticConstructor cctor
                ]
            
            def,
            {
                StrongName = None
                BaseClass = None
                Requires = [] //annot.Requires
                Members = members
                IsModule = true
            }
            
        if sc.IsValueCreated then
            getStartupCodeClass sc.Value |> comp.AddClass

    comp.Resolve()

    comp
