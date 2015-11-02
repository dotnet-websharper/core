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

type private SourceMemberOrEntity =
    | SourceMember of FSMFV * list<list<FSMFV>> * FSharpExpr
    | SourceEntity of FSharpEntity * ResizeArray<SourceMemberOrEntity>

let transformInterface (intf: FSharpEntity) =
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

let isILClass (e: FSharpEntity) =
    e.IsClass || e.IsFSharpExceptionDeclaration || e.IsFSharpModule || e.IsFSharpRecord || e.IsFSharpUnion

let rec private transformClass (sc: Lazy<_ * StartupCode>) (comp: Compilation) parentAnnot (cls: FSharpEntity) members =
    let annot = 
        ToFSharpAST.attrReader.GetTypeAnnot(parentAnnot, cls.Attributes)

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

    for m in members do
        match m with
        | SourceMember (meth, args, expr) ->        
            if meth.IsProperty || (unionOrRecord && meth.IsCompilerGenerated) then () else
            let mAnnot = ToFSharpAST.attrReader.GetMemberAnnot(annot, meth.Attributes)
            
            let getArgsAndThis() =
                let a, t =
                    args |> List.concat
                    |> function
                    | t :: r when (t.IsMemberThisValue || t.IsConstructorThisValue) && not meth.IsExtensionMember -> r, Some t
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
                match ToFSharpAST.getMember meth with
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
                let memdef = ToFSharpAST.getMember meth
    //            let isModuleValue = ref false
                let getBody isInline = 
                    let a, t = getArgsAndThis()

                    let argsAndVars = a |> List.map (fun p -> p, Id.New p.CompiledName)
                    let env = ToFSharpAST.Environment.New (argsAndVars, meth.GenericParameters |> Seq.map (fun p -> p.Name) |> List.ofSeq, comp)  
                    let res =
                        let b = ToFSharpAST.transformExpression env expr 
                        let b = 
                            match memdef with
                            | Member.Constructor _ -> ToFSharpAST.fixCtor b
                            | _ -> b
                        let b = FixThisScope().Fix(b)      
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
                match memdef with
                | Member.Method (_, mdef) 
                | Member.Override (_, mdef) 
                | Member.Implementation (_, mdef) ->
                    // remove abstract slot
//                    let abstractOpt = 
//                        clsMembers |> Seq.tryFind (fun m ->
//                            match m with
//                            | NotResolvedMember.Method (d, _) when d = mdef -> true
//                            | _ -> false
//                        )
//                    abstractOpt |> Option.iter (clsMembers.Remove >> ignore)
                    let getKind() =
                        match memdef with
                        | Member.Method (isInstance , _) ->
                            if isInstance then N.Instance else N.Static  
                        | Member.Override (t, _) -> N.Override t 
                        | Member.Implementation (t, _) -> N.Implementation t
                        | _ -> failwith "impossible"
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
                            let i = Application (runtimeGetOptional, [ItemGet(Hole 0, Value (String meth.CompiledName.[4..]))])
                            //ItemGet(Hole 0, Value (String meth.CompiledName.[4..]))   // TODO optional
                            addMethod mAnnot mdef N.Inline true i
                        elif meth.IsPropertySetterMethod then  
                            let i = Application (runtimeSetOptional, [Hole 0; Value (String meth.CompiledName.[4..]); Hole 1])
//                            let i = ItemSet(Hole 0, Value (String meth.CompiledName.[4..]), Hole 1)   // TODO optional
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
        | SourceEntity (ent, nmembers) ->
            transformClass sc comp annot ent nmembers |> Option.iter comp.AddClass   

    for f in cls.FSharpFields do
        let mAnnot = ToFSharpAST.attrReader.GetMemberAnnot(annot, Seq.append f.FieldAttributes f.PropertyAttributes)
        let nr =
            {
                StrongName = mAnnot.Name
                IsStatic = f.IsStatic
                IsOptional = mAnnot.Kind = Some A.MemberKind.OptionalField && ToFSharpAST.isOption f.FieldType
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
    ToFSharpAST.thisAssemblyName <- assemblyName

    let comp = Compilation(refMeta)

    let asmAnnot = 
        ToFSharpAST.attrReader.GetAssemblyAnnot(checkResults.AssemblySignature.Attributes)

    comp.AssemblyName <- assemblyName
    comp.AssemblyRequires <- asmAnnot.Requires
    comp.SiteletDefinition <- asmAnnot.SiteletDefinition
                                             
    for file in checkResults.AssemblyContents.ImplementationFiles do
        let fileName =
            lazy
            match file.Declarations.Head with
            | FSIFD.Entity (a, _) -> a.DeclarationLocation.FileName
            | FSIFD.MemberOrFunctionOrValue (_, _, c) -> c.Range.FileName
            | FSIFD.InitAction a -> a.Range.FileName
        let sc =
            lazy
            let name = "StartupCode$" + assemblyName.Replace('.', '_') + "$" + (System.IO.Path.GetFileNameWithoutExtension fileName.Value).Replace('.', '_')
            let def =
                TypeDefinition {
                    Assembly = assemblyName
                    FullName = name
                }
            def, 
            (ResizeArray(), HashSet() : StartupCode)

        let topLevelTypes = ResizeArray<FSharpEntity * ResizeArray<SourceMemberOrEntity>>()
        let types = Dictionary<FSharpEntity, ResizeArray<SourceMemberOrEntity>>()
        let interfaces = ResizeArray<FSharpEntity>()
        let rec getTypesWithMembers parentMembers (sc: Lazy<_ * StartupCode>) d =
            match d with
            | FSIFD.Entity (a, b) ->
                if not a.IsFSharpAbbreviation then
                    if a.IsInterface then
                        interfaces.Add a
                    elif isILClass a then 
                        let ms = ResizeArray()
                        match parentMembers with
                        | Some (pms: ResizeArray<_>) ->
                            pms.Add (SourceEntity (a, ms))
//                            printfn "nested type: %s" a.AccessPath
                        | _ ->
                            topLevelTypes.Add (a, ms) 
//                            printfn "top level type: %s" a.AccessPath
                        types.Add (a, ms)
                        b |> List.iter (getTypesWithMembers (Some ms) sc)
                    else
                        b |> List.iter (getTypesWithMembers parentMembers sc)
            | FSIFD.MemberOrFunctionOrValue (a, b, c) -> 
                types.[a.EnclosingEntity].Add(SourceMember(a, b, c))
            | FSIFD.InitAction a ->
                ()
                // TODO : warn
//                comp.AddError(Some (ToFSharpAST.getSourcePos a), SourceError "Top level expression")
                // TODO: [<JavaScript>] on whole assembly.

        file.Declarations |> Seq.iter (getTypesWithMembers None sc)

        for i in interfaces do
            transformInterface i |> Option.iter comp.AddInterface

        for c, m in topLevelTypes do
            transformClass sc comp A.TypeAnnotation.Empty c m |> Option.iter comp.AddClass
            
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
