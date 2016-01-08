module WebSharper.Compiler.FSharp.Translator

open Microsoft.FSharp.Compiler.SourceCodeServices
open System.Collections.Generic

open WebSharper.Core
open WebSharper.Core.AST
open WebSharper.Core.Metadata

module A = WebSharper.Compiler.AttributeReader
module QR = WebSharper.Compiler.QuotationReader

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

let isResourceType (e: FSharpEntity) =
    e.AllInterfaces |> Seq.exists (fun i ->
        ToFSharpAST.getTypeDefinition i.TypeDefinition = iResourceDef
    )

let basicInstanceField =
    { 
        StrongName = None
        IsStatic = false
        IsOptional = false 
    }

let rec private transformClass (sc: Lazy<_ * StartupCode>) (comp: Compilation) parentAnnot (cls: FSharpEntity) members =
    let thisDef = ToFSharpAST.getTypeDefinition cls
    
    let annot = 
        ToFSharpAST.attrReader.GetTypeAnnot(parentAnnot, cls.Attributes)

    if isResourceType cls then
        let thisRes = comp.Graph.AddOrLookupNode(ResourceNode thisDef)
        comp.Graph.AddEdge(thisRes, AssemblyNode (comp.AssemblyName, true))
        for req in annot.Requires do
            comp.Graph.AddEdge(thisRes, ResourceNode req)
        None
    else    
    
    let def =
        match annot.ProxyOf with
        | Some p -> 
            comp.AddProxy(thisDef, p)
            if cls.Accessibility.IsPublic then
                // TODO: warn about public proxy type
                ()
            p
        | _ -> thisDef

    let clsMembers = ResizeArray()
    
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

    let annotations = Dictionary ()

    let propertiesWithSetter = 
        cls.MembersFunctionsAndValues |> Seq.filter (fun x -> x.IsProperty && x.HasSetterMethod)
        |> List.ofSeq
        
    let rec getAnnot x : A.MemberAnnotation =
        match annotations.TryFind (x: FSharpMemberOrFunctionOrValue) with
        | Some a -> a
        | _ -> 
            let a = ToFSharpAST.attrReader.GetMemberAnnot(annot, x.Attributes)
            let a =
                if x.IsPropertySetterMethod then
                    match propertiesWithSetter |> List.tryFind (fun p -> p.SetterMethod = x) with
                    | None -> a
                    | Some p ->
                        let pa = getAnnot p
                        if pa.Kind = Some A.MemberKind.Stub then
                            { a with Kind = pa.Kind; Name = pa.Name }
                        else a
                else a
            annotations.Add(x, a)
            a

    let stubs = HashSet()

    let addStub memdef mAnnot def expr = 
        stubs.Add memdef |> ignore
        addMethod mAnnot def N.Inline true expr

    for meth in cls.MembersFunctionsAndValues do
        if meth.IsProperty then () else
        let mAnnot = getAnnot meth
        
        let isStub = mAnnot.Kind = Some A.MemberKind.Stub

        if isStub || (meth.IsDispatchSlot && mAnnot.Kind = Some A.MemberKind.JavaScript) then
//        || meth.IsCompilerGenerated then
//           
//            let body =
//                if meth.IsCompilerGenerated then Value(String "CompilerGenerated (no expr)") else Undefined
            match ToFSharpAST.getMember meth with
            | Member.Method (isInstance, mdef) as memdef ->
                let getItem() =
                    match mAnnot.Name with
                    | Some n -> n 
                    | _ -> mdef.Value.MethodName.[4 ..]
                    |> String |> Value
                if isInstance then                                 
                    if isStub && meth.IsPropertyGetterMethod then
                        addStub memdef mAnnot mdef (ItemGet(Hole 0, (getItem())))    
                    elif isStub && meth.IsPropertySetterMethod then
                        addStub memdef mAnnot mdef (ItemSet(Hole 0, (getItem()), Hole 1))    
                    else 
                        addMethod mAnnot mdef N.Instance true Undefined
                else
                    if isStub && (meth.IsPropertyGetterMethod || meth.IsPropertySetterMethod) then
                        failwith "Static property can't have Stub attribute"
                    else 
                        addMethod mAnnot mdef N.Static true Undefined
            | Member.Constructor cdef -> // Todo stub constructors
                addConstructor mAnnot cdef N.Constructor true Undefined
            | _ -> failwith "Static method can't have Stub attribute"

    let fsharpSpecific = cls.IsFSharpUnion || cls.IsFSharpRecord || cls.IsFSharpExceptionDeclaration

    let clsTparams =
        cls.GenericParameters |> Seq.map (fun p -> p.Name) |> List.ofSeq    

    for m in members do
        match m with
        | SourceMember (meth, args, expr) ->        
            if meth.IsProperty || (fsharpSpecific && meth.IsCompilerGenerated) then () else

            let mAnnot = getAnnot meth
            
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

                if stubs.Contains memdef then () else
    //            let isModuleValue = ref false
                let getBody isInline = 
                    let hasRD =
                        meth.Attributes |> Seq.exists (fun a -> a.AttributeType.FullName = "Microsoft.FSharp.Core.ReflectedDefinitionAttribute")
                    if hasRD then
                        let info =
                            match memdef with
                            | Member.Method (_, mdef) 
                            | Member.Override (_, mdef) 
                            | Member.Implementation (_, mdef) ->
                                Reflection.loadMethod def mdef :> System.Reflection.MethodBase
                            | Member.Constructor cdef ->
                                Reflection.loadConstructor def cdef :> _
                            | _ -> failwith "static constructor with a reflected definition"
                        match FSharp.Quotations.Expr.TryGetReflectedDefinition(info) with
                        | Some q ->
                            comp.AddWarning(Some (ToFSharpAST.getRange expr.Range), SourceWarning "Compiling from reflected definition")
                            QR.transformExpression (QR.Environment.New(comp)) q
                        | _ ->
                            comp.AddError(Some (ToFSharpAST.getRange expr.Range), SourceError "Failed to get reflected definition")
                            WebSharper.Compiler.ToJavaScript.errorPlaceholder
                    else
                    let a, t = getArgsAndThis()
                    let argsAndVars = 
                        [
                            match t with
                            | Some t ->
                                yield t, (Id.New t.CompiledName, ToFSharpAST.ThisArg)
                            | _ -> ()
                            for p in a ->    
                                p, 
                                (Id.New p.CompiledName, 
                                    if ToFSharpAST.isByRef p.FullType then ToFSharpAST.ByRefArg else ToFSharpAST.LocalVar)
                        ]
                    let tparams = clsTparams @ (meth.GenericParameters |> Seq.map (fun p -> p.Name) |> List.ofSeq)
                    let env = ToFSharpAST.Environment.New (argsAndVars, tparams, comp)  
                    let res =
                        let b = ToFSharpAST.transformExpression env expr 
                        let b = 
                            match memdef with
                            | Member.Constructor _ -> 
                                try ToFSharpAST.fixCtor b
                                with e ->
                                    comp.AddError(tryGetExprSourcePos b, SourceError e.Message)
                                    WebSharper.Compiler.ToJavaScript.errorPlaceholder
                            | _ -> b
                        let b = FixThisScope().Fix(b)      
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
                            let thisVar, vars =
                                match argsAndVars with 
                                | (_, (t, ToFSharpAST.ThisArg)) :: a -> Some t, a |> List.map (snd >> fst)  
                                | a -> None, a |> List.map (snd >> fst) 
                            if isInline then
//                                let iv = Option.toList thisVar @ vars |> List.mapi (fun i a -> a, Hole i)
//                                let ib = ReplaceThisWithHole0().TransformExpression(b)
                                let b = 
                                    match thisVar with
                                    | Some t -> ReplaceThisWithVar(t).TransformExpression(b)
                                    | _ -> b
                                makeExprInline (Option.toList thisVar @ vars) b
//                                List.foldBack (fun (v, h) body ->
//                                    Let (v, h, body)    
//                                ) (Option.toList thisVar @ vars |> List.mapi (fun i a -> a, Hole i)) (ReplaceThisWithHole0().TransformExpression(b))
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
                            failwith "Abstract methods cannot be marked with Inline or Macro attributes."
                    match kind with
                    | A.MemberKind.NoFallback ->
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
                    | A.MemberKind.Constant c ->
                        checkNotAbstract() 
                        addMethod mAnnot mdef N.Inline true c                        
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
                    | A.MemberKind.Generated _ ->
                        addMethod mAnnot mdef (getKind()) false Undefined
                    | A.MemberKind.Remote _
                    | A.MemberKind.Stub -> failwith "impossible"
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
                        let vars, thisVar = getVarsAndThis()
                        let parsed = WebSharper.Compiler.Recognize.createInline thisVar vars js
                        addConstructor mAnnot cdef N.Inline true parsed 
                    | A.MemberKind.Direct js ->
                        let vars, thisVar = getVarsAndThis()
                        let parsed = WebSharper.Compiler.Recognize.parseDirect thisVar vars js
                        addConstructor mAnnot cdef N.Static true parsed 
                    | A.MemberKind.JavaScript -> jsCtor false
                    | A.MemberKind.InlineJavaScript -> jsCtor true
                    | A.MemberKind.Generated _ ->
                        addConstructor mAnnot cdef N.Static false Undefined
                    | _ -> failwith "invalid constructor kind"
                | Member.StaticConstructor ->
                    clsMembers.Add (NotResolvedMember.StaticConstructor (getBody false))
            | None 
            | _ -> ()
        | SourceEntity (ent, nmembers) ->
            transformClass sc comp annot ent nmembers |> Option.iter comp.AddClass   

//    let translated = annot.IsJavaScript || clsMembers.Count > 0

    if not annot.IsJavaScript && clsMembers.Count = 0 && annot.Macros.IsEmpty then None else

    if cls.IsFSharpUnion then
        
        let tparamsMap =
            clsTparams |> Seq.mapi (fun i p -> p, i) |> Map.ofSeq
        let g = tparamsMap.Count 
        let uTyp = ConcreteType { Entity = def; Generics = [ for i in 0 .. g - 1 -> GenericType i ] }

        let isStatic =
            cls.Attributes 
            |> ToFSharpAST.hasCompilationRepresentation CompilationRepresentationFlags.UseNullAsTrueValue

        let tagDef =
            Hashed {
                MethodName = "get_Tag"
                Parameters = if isStatic then [uTyp] else []
                ReturnType = ConcreteType { Entity = Hashed { FullName = "System.Int32"; Assembly = "mscorlib"}; Generics = []}
                Generics = 0
            }

        let tagBody =
            // TODO : unions with constant members 
            ItemGet(Hole 0, Value (String "$"))

        addMethod A.MemberAnnotation.BasicInlineJavaScript tagDef N.Inline false tagBody

        for case, i in cls.UnionCases |> Seq.mapi (fun i c -> c, i) do
            let cAnnot = ToFSharpAST.attrReader.GetMemberAnnot(annot, case.Attributes)
            let newDef = 
                Hashed {
                    MethodName = "New" + case.CompiledName
                    Parameters = case.UnionCaseFields |> Seq.map (fun f -> ToFSharpAST.getType tparamsMap f.FieldType) |> List.ofSeq
                    ReturnType = uTyp
                    Generics = 0
                }
            let newBody =
                match cAnnot.Kind with
                | Some (A.MemberKind.Constant c) -> c
                | _ ->
                    CopyCtor(
                        def,
                        Object (
                            ("$", Value (Int i)) ::
                            List.init (case.UnionCaseFields.Count) (fun j -> "$" + string j, Hole j)
                        )
                    )

            addMethod A.MemberAnnotation.BasicInlineJavaScript newDef N.Inline false newBody

            let isDef =
                Hashed {
                    MethodName = "get_Is" + case.CompiledName
                    Parameters = if isStatic then [uTyp] else []
                    ReturnType = ConcreteType { Entity = Hashed { FullName = "System.Boolean"; Assembly = "mscorlib"}; Generics = []}
                    Generics = 0
                }

            let isBody =
                match cAnnot.Kind with
                | Some (A.MemberKind.Constant c) -> 
                    Binary (Hole 0, BinaryOperator.``==``, c)
                | _ ->
                    Binary(ItemGet(Hole 0, Value (String "$")), BinaryOperator.``==``, Value (Int i))

            addMethod A.MemberAnnotation.BasicInlineJavaScript isDef N.Inline false isBody

            let caseDef = 
                Hashed { def.Value with FullName = def.Value.FullName + "+" + case.CompiledName }

            let caseFields =
                case.UnionCaseFields |> Seq.mapi (fun j f ->
                    let fDef =
                        Hashed {
                            MethodName = "get_" + f.Name
                            Parameters = []
                            ReturnType = ToFSharpAST.getType tparamsMap f.FieldType
                            Generics = 0
                        }
                    let fProp =
                        {
                            Kind = NotResolvedMemberKind.Inline
                            StrongName = None
                            Macros = []
                            Generator = None
                            Compiled = true 
                            Body = ItemGet(Hole 0, Value (String ("$" + string j)))
                            Requires = []
                        }
                    NotResolvedMember.Method (fDef, fProp)
                ) |> List.ofSeq

            let caseCls =
                {
                    StrongName = None
                    BaseClass = None
                    Requires = []
                    Members = caseFields
                    IsModule = true
                    IsProxy = false
                    Macros = []
                }

            comp.AddClass(caseDef, caseCls)

    if cls.IsFSharpRecord || cls.IsFSharpExceptionDeclaration then
        let tparamsMap =
            clsTparams |> Seq.mapi (fun i p -> p, i) |> Map.ofSeq
        let cdef =
            Hashed {
                CtorParameters =
                    cls.FSharpFields |> Seq.map (fun f -> ToFSharpAST.getType tparamsMap f.FieldType) |> List.ofSeq
            }
        let body =
            let vars =
                cls.FSharpFields |> Seq.map (fun f -> Id.New f.Name) |> List.ofSeq
            let fields =
                cls.FSharpFields |> Seq.map (fun f -> 
//                    let typAnnot = attrReader.GetTypeAnnot(A.TypeAnnotation.Empty, typ.TypeDefinition.Attributes)
                    let fAnnot = ToFSharpAST.attrReader.GetMemberAnnot(annot, Seq.append f.FieldAttributes f.PropertyAttributes)
                    
                    match fAnnot.Name with Some n -> n | _ -> f.Name
                    , 
                    fAnnot.Kind = Some A.MemberKind.OptionalField && ToFSharpAST.isOption f.FieldType
                )
                |> List.ofSeq
            let obj = 
                Seq.zip (fields) vars
                |> Seq.map (fun ((name, opt), v) -> name, if opt then ItemGet(Var v, Value (String "$0")) else Var v)
                |> List.ofSeq |> Object
            let optFields = 
                fields |> List.choose (fun (f, o) -> 
                    if o then Some (Value (String f)) else None)
            let record =
                CopyCtor(def,
                    if List.isEmpty optFields then obj
                    else Application (runtimeDeleteEmptyFields, [obj; NewArray optFields])
                )
            Lambda (vars, record)

        addConstructor A.MemberAnnotation.BasicJavaScript cdef N.Static false body

        // properties

        for f in cls.FSharpFields do
            let recTyp = concrete (def, List.init cls.GenericParameters.Count GenericType)
            let fTyp = ToFSharpAST.getType tparamsMap f.FieldType
            
            let getDef =
                Hashed {
                    MethodName = "get_" + f.Name
                    Parameters = []
                    ReturnType = fTyp
                    Generics = 0
                }

            let getBody = FieldGet(Some (Hole 0), recTyp, f.Name)
                
            addMethod A.MemberAnnotation.BasicInlineJavaScript getDef N.Inline false getBody

            if f.IsMutable then
                let setDef =
                    Hashed {
                        MethodName = "set_" + f.Name
                        Parameters = [ fTyp ]
                        ReturnType = VoidType
                        Generics = 0
                    }

                let setBody = FieldSet(Some (Hole 0), recTyp, f.Name, Hole 1)
            
                addMethod A.MemberAnnotation.BasicInlineJavaScript setDef N.Inline false setBody

    for f in cls.FSharpFields do
        let fAnnot = ToFSharpAST.attrReader.GetMemberAnnot(annot, Seq.append f.FieldAttributes f.PropertyAttributes)
        let nr =
            {
                StrongName = fAnnot.Name
                IsStatic = f.IsStatic
                IsOptional = fAnnot.Kind = Some A.MemberKind.OptionalField && ToFSharpAST.isOption f.FieldType
            }
        clsMembers.Add (NotResolvedMember.Field (f.Name, nr))    

//    let notTranslated =
//        not annot.IsJavaScript 
////        && not (cls.IsFSharpExceptionDeclaration || cls.IsFSharpRecord || cls.IsFSharpUnion)
//        && clsMembers |> Seq.forall (function NotResolvedMember.Field _ -> true | _ -> false)
//
//    if notTranslated then None else

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
            IsProxy = Option.isSome annot.ProxyOf
            Macros = annot.Macros
        }
    )

let transformAssembly (refMeta : Info) assemblyName (checkResults: FSharpCheckProjectResults) =   
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
//                    if a.IsDelegate then
//                        ToFSharpAST.getAndRegisterTypeDefinition comp a |> ignore
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
                        try
                            types.Add (a, ms)
                        with _ ->
                            comp.AddError(Some (ToFSharpAST.getRange a.DeclarationLocation), SourceError "Duplicate type definition")
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
                IsProxy = false
                Macros = []
            }
            
        if sc.IsValueCreated then
            getStartupCodeClass sc.Value |> comp.AddClass

    comp.Resolve()

    comp
