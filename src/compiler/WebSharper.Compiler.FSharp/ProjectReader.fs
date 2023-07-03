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

module WebSharper.Compiler.FSharp.ProjectReader

open FSharp.Compiler.Symbols
open FSharp.Compiler.CodeAnalysis
open System.Collections.Generic

open WebSharper.Core
open WebSharper.Core.AST
open WebSharper.Core.Metadata
open WebSharper.Compiler
open WebSharper.Compiler.NotResolved
open WebSharper.Compiler.CommandTools

module A = WebSharper.Compiler.AttributeReader
module QR = WebSharper.Compiler.QuotationReader

type FSIFD = FSharpImplementationFileDeclaration
type FSMFV = FSharpMemberOrFunctionOrValue

type private StartupCode = ResizeArray<Statement> * ResizeArray<Statement> * Dictionary<string, Type> 

type private N = NotResolvedMemberKind

type private SourceMemberOrEntity =
    | SourceMember of FSMFV * list<list<FSMFV>> * FSharpExpr
    | SourceEntity of FSharpEntity * ResizeArray<SourceMemberOrEntity>
    | SourceInterface of FSharpEntity 
    | InitAction of FSharpExpr

let annotForTypeOrFile name (annot: A.TypeAnnotation) =
    let mutable annot = annot
    if annot.JavaScriptTypesAndFiles |> List.contains name then
        if not annot.IsForcedNotJavaScript then 
            annot <- { annot with IsJavaScript = true }
    if annot.JavaScriptExportTypesAndFiles |> List.contains name then
        annot <- { annot with IsJavaScriptExport = true }
    annot

// fixes annotation for property setters, we don't want name coflicts
let fixMemberAnnot (getAnnot: _ -> A.MemberAnnotation) (x: FSharpEntity) (m: FSMFV) (a: A.MemberAnnotation) =
    if m.IsPropertySetterMethod then
        let po = 
            x.MembersFunctionsAndValues 
            |> Seq.tryFind (fun p -> p.IsProperty && p.HasSetterMethod && p.SetterMethod = m)
        match po with
        | Some p ->
            let pa = getAnnot p
            if pa.Kind = Some A.MemberKind.Stub then
                { a with Kind = pa.Kind; Name = pa.Name }
            else  
                let name =
                    match a.Name with
                    | Some n as an when an <> pa.Name || not p.HasGetterMethod -> Some n
                    | _ -> pa.Name |> Option.map (fun n -> if p.HasGetterMethod then "set_" + n else n)
                { a with Name = name }
        | _ -> a
    else a

let rec private collectTypeAnnotations (d: Dictionary<FSharpEntity, TypeDefinition * A.TypeAnnotation>) (t: Dictionary<TypeDefinition, FSharpEntity>) (sr: CodeReader.SymbolReader) parentAnnot (cls: FSharpEntity) (members: seq<SourceMemberOrEntity>) =
    let thisDef = sr.ReadTypeDefinition cls
    let annot =
        sr.AttributeReader.GetTypeAnnot(parentAnnot |> annotForTypeOrFile thisDef.Value.FullName, cls.Attributes)
    d.Add(cls, (thisDef, annot))
    t.Add(thisDef, cls)
    for m in members do
        match m with
        | SourceEntity (ent, nmembers) ->
            collectTypeAnnotations d t sr annot ent nmembers
        | SourceInterface ent ->
            collectTypeAnnotations d t sr annot ent Seq.empty
        | _ -> ()

let private getConstraints (genParams: seq<FSharpGenericParameter>) (sr: CodeReader.SymbolReader) tparams =
    genParams |> Seq.map (fun p ->
        let annot = sr.AttributeReader.GetTypeParamAnnot(p.Attributes)
        {
            Type = annot.Type
            Constraints =
                p.Constraints |> Seq.choose (fun c ->
                    if c.IsCoercesToConstraint then
                        Some <| sr.ReadType tparams c.CoercesToTarget
                    else None
                ) |> List.ofSeq
        }
    ) |> List.ofSeq

let private transformInterface (sr: CodeReader.SymbolReader) parentAnnot (intf: FSharpEntity) =
    let annot =
       sr.AttributeReader.GetTypeAnnot(parentAnnot, intf.Attributes)
    if annot.IsForcedNotJavaScript then None else
    let methods = ResizeArray()
    let def =
        match annot.ProxyOf with
        | Some d -> d
        | _ -> sr.ReadTypeDefinition intf

    let tparams =
        intf.GenericParameters |> Seq.mapi (fun i p -> p.Name, i) |> Map.ofSeq

    let intfMethods = 
        intf.MembersFunctionsAndValues
        |> Seq.map (fun m ->
            let mAnnot =
                sr.AttributeReader.GetMemberAnnot(annot, m.Attributes)
                |> fixMemberAnnot (fun a -> sr.AttributeReader.GetMemberAnnot(annot, a.Attributes)) intf m
            m, mAnnot
        )
        |> List.ofSeq

    let isRemote =
        intfMethods |> List.exists (function (_, { Kind = Some (AttributeReader.MemberKind.Remote _) }) -> true | _ -> false)

    if isRemote then None else

    let hasExplicitJS =
        annot.IsJavaScript || (intfMethods |> List.exists (fun (_, mAnnot) -> mAnnot.Kind = Some AttributeReader.MemberKind.JavaScript))
    
    for m in intf.MembersFunctionsAndValues do
        if not m.IsProperty then
            let mAnnot =
                sr.AttributeReader.GetMemberAnnot(annot, m.Attributes)
                |> fixMemberAnnot (fun a -> sr.AttributeReader.GetMemberAnnot(annot, a.Attributes)) intf m
            if not hasExplicitJS || mAnnot.Kind.IsSome then
                let md = 
                    match sr.ReadMember m with
                    | Member.Method (_, md) -> md
                    | _ -> failwith "invalid interface member"
                let gc = getConstraints m.GenericParameters sr tparams
                methods.Add(md, mAnnot.Name, gc)
    
    Some (def, 
        {
            StrongName = annot.Name 
            Extends = intf.DeclaredInterfaces |> Seq.map (fun i -> sr.ReadType tparams i |> getConcreteType) |> List.ofSeq
            NotResolvedMethods = List.ofSeq methods
            Generics = getConstraints intf.GenericParameters sr tparams
            Type = annot.Type
        }
    )

let private isILClass (e: FSharpEntity) =
    e.IsClass || e.IsFSharpExceptionDeclaration || e.IsFSharpModule || e.IsFSharpRecord || e.IsFSharpUnion || e.IsValueType

let private isResourceType (sr: CodeReader.SymbolReader) (e: FSharpEntity) =
    e.AllInterfaces |> Seq.exists (fun i ->
        sr.ReadTypeDefinition i.TypeDefinition = Definitions.IResource
    )

let isAugmentedFSharpType (e: FSharpEntity) =
    e.IsFSharpRecord || e.IsFSharpExceptionDeclaration || (
        e.IsFSharpUnion 
        && not (
            e.Attributes |> Seq.exists (fun a ->
                a.AttributeType.FullName = "Microsoft.FSharp.Core.DefaultAugmentationAttribute"
                && not (snd a.ConstructorArguments.[0] :?> bool)
            )
        )
    )

let isOptionalParam (p: FSharpParameter) =
    p.Attributes |> Seq.exists (fun a ->
        a.AttributeType.FullName = "System.Runtime.InteropServices.OptionalAttribute"
    )

let defaultValueOfParam (p: FSharpParameter) =
    p.Attributes |> Seq.tryPick (fun pa -> 
        if pa.AttributeType.FullName = "System.Runtime.InteropServices.DefaultParameterValueAttribute" then
            Some (ReadLiteral (snd pa.ConstructorArguments.[0]) |> Value) 
        else None
    )

let refKindOfParam (p: FSharpParameter) =
    p.Attributes |> Seq.tryPick (fun a ->
        match a.AttributeType.FullName with
        | "System.Runtime.InteropServices.OutAttribute" -> Some OutRefArg
        | "System.Runtime.InteropServices.InAttribute" -> Some InRefArg
        | _ -> None
    )
    |> Option.defaultValue NotOptimizedFuncArg

let isAbstractClass (e: FSharpEntity) =
    e.Attributes |> Seq.exists (fun a ->
        a.AttributeType.FullName = "Microsoft.FSharp.Core.AbstractClassAttribute"
    )

let private transformInitAction (sc: Lazy<_ * StartupCode>) (comp: Compilation) (sr: CodeReader.SymbolReader) (annot: A.TypeAnnotation) recMembers a =
    if annot.IsJavaScript then
        let _, (_, statements, _) = sc.Value
        let env = CodeReader.Environment.New ([], false, [], comp, sr, recMembers)  
        statements.Add (CodeReader.transformExpression env a |> ExprStatement)   

let private nrInline = N.Inline false

let rec private transformClass (sc: Lazy<_ * StartupCode>) (comp: Compilation) (ac: ArgCurrying.ResolveFuncArgs) (sr: CodeReader.SymbolReader) (classAnnots: Dictionary<FSharpEntity, TypeDefinition * A.TypeAnnotation>) (isInterface: TypeDefinition -> bool) (recMembers: Dictionary<FSMFV, Id * FSharpExpr>) (cls: FSharpEntity) (members: IList<SourceMemberOrEntity>) =
    let thisDef, annot = classAnnots.[cls]

    if isResourceType sr cls then
        if comp.HasGraph then
            let thisRes = comp.Graph.AddOrLookupNode(ResourceNode (thisDef, None))
            for req, po in annot.Requires do
                comp.Graph.AddEdge(thisRes, ResourceNode (req, po |> Option.map ParameterObject.OfObj))
        None
    else    
    
    let def, proxied =
        match annot.ProxyOf with
        | Some p -> 
            let warn msg =
                comp.AddWarning(Some (CodeReader.getRange cls.DeclarationLocation), SourceWarning msg)
            if cls.Accessibility.IsPublic then
                warn "Proxy type should not be public"
            let proxied =
                try
                    let t = Reflection.LoadTypeDefinition p
                    t.GetMembers(Reflection.AllPublicMethodsFlags) |> Seq.choose Reflection.ReadMember
                    |> Seq.collect (fun m ->
                        match m with
                        | Member.Override (_, me) -> [ Member.Method (true, me); m ]
                        | _ -> [ m ]
                    ) |> HashSet
                with _ ->
                    warn ("Proxy target type could not be loaded for signature verification: " + p.Value.FullName)
                    HashSet()
            p, Some proxied
        | _ -> thisDef, None

    let isProxy = Option.isSome annot.ProxyOf 
    let isThisInterface = cls.IsInterface
    let isInterfaceProxy = isProxy && isInterface def

    if annot.IsJavaScriptExport then
        comp.AddJavaScriptExport (ExportNode (TypeNode def))

    let clsMembers = ResizeArray()
    
    let clsTparams =
        cls.GenericParameters |> Seq.mapi (fun i p -> p.Name, i) |> Map.ofSeq

    let thisType = Generic def (List.init cls.GenericParameters.Count TypeParameter)
    let thisTypeForFixer = Some (ConcreteType thisType)

    let getUnresolved (mem: option<FSMFV * Member>) (mAnnot: A.MemberAnnotation) kind compiled curriedArgs expr = 
        let nr =
            {
                Kind = kind
                StrongName = mAnnot.Name
                Generics =
                    match mem with
                    | None -> []
                    | Some (m, mem) ->
                        let skip =
                            match kind, mem with
                            | N.Abstract, _ -> 0
                            | _, (Member.Method _ | Member.Override _ | Member.Implementation _) ->
                                min cls.GenericParameters.Count m.GenericParameters.Count
                            | _ -> 0
                        try
                            getConstraints (Seq.skip skip m.GenericParameters) sr clsTparams
                        with _ ->
                            failwithf "failed getting generic params on %s member %A" def.Value.FullName mem
                        //getConstraints m.GenericParameters sr clsTparams
                Macros = mAnnot.Macros
                Generator = 
                    match mAnnot.Kind with
                    | Some (A.MemberKind.Generated (g, p)) -> Some (g, p)
                    | _ -> None
                Compiled = compiled 
                Pure = mAnnot.Pure
                Body = expr
                Requires = mAnnot.Requires
                FuncArgs = curriedArgs |> Option.map (fun (_, ca, _, _) -> ca)
                Args = 
                    match curriedArgs with 
                    | None -> [] 
                    | Some (_, _, ids, _) -> ids
                Warn = mAnnot.Warn
                JavaScriptOptions = mAnnot.JavaScriptOptions
            }
        match curriedArgs with
        | Some (mem, ca, args, inst) ->
            ac.AddMember(mem, nr, args, inst)
        | _ -> ()
        nr

    let addMethod (mem: option<FSMFV * Member>) (mAnnot: A.MemberAnnotation) (mdef: Method) kind compiled curriedArgs expr =
        match proxied, mem with
        | Some ms, Some (mem, memdef) ->
            if not isInterfaceProxy && not (ms.Contains memdef) then
                let candidates =
                    let n = mdef.Value.MethodName
                    match memdef with
                    | Member.Method (i, _) ->
                        ms |> Seq.choose (fun m ->
                            match m with
                            | Member.Method (mi, m) -> if mi = i && m.Value.MethodName = n then Some (string m.Value) else None
                            | Member.Override (_, m) -> if i && m.Value.MethodName = n then Some (string m.Value) else None 
                            | _ -> None 
                        )    
                    | Member.Override _ ->
                        ms |> Seq.choose (fun m ->
                            match m with
                            | Member.Method (true, m) -> if m.Value.MethodName = n then Some (string m.Value) else None
                            | Member.Override (_, m) -> if m.Value.MethodName = n then Some (string m.Value) else None 
                            | _ -> None 
                        )    
                    | Member.Implementation (intf, _) ->
                        ms |> Seq.choose (fun m ->
                            match m with
                            | Member.Implementation (i, m) -> if i = intf && m.Value.MethodName = n then Some (i.Value.FullName + string m.Value) else None 
                            | _ -> None 
                        )  
                    | _ -> Seq.empty  
                    |> List.ofSeq
                if List.isEmpty candidates then
                    if not (mem.Accessibility.IsPrivate || mem.Accessibility.IsInternal) then
                        let msg = "Proxy member do not match any member names of target class."
                        comp.AddWarning(Some (CodeReader.getRange mem.DeclarationLocation), SourceWarning msg)
                elif def.Value.FullName.StartsWith "Microsoft.FSharp.Core.OptimizedClosures+FSharpFunc`" then
                    () // ignore warnings for OptimizedClosures, they are expected
                else 
                    let msg = sprintf "Proxy member do not match any member signatures of target class %s. Current: %s, candidates: %s" (string def.Value) (string mdef.Value) (String.concat ", " candidates)
                    comp.AddWarning(Some (CodeReader.getRange mem.DeclarationLocation), SourceWarning msg)
        | _ -> ()
        if mAnnot.IsJavaScriptExport then
            comp.AddJavaScriptExport (ExportNode (MethodNode (def, mdef)))
        clsMembers.Add (NotResolvedMember.Method (mdef, (getUnresolved mem mAnnot kind compiled curriedArgs expr)))
        
    let addConstructor (mem: option<FSMFV * Member>) (mAnnot: A.MemberAnnotation) (cdef: Constructor) kind compiled curriedArgs expr =
        match proxied, mem with
        | Some ms, Some (mem, memdef) ->
            if cdef.Value.CtorParameters.Length > 0 && not (ms.Contains memdef) then
                let candidates = 
                    ms |> Seq.choose (function Member.Constructor c -> Some c | _ -> None)
                    |> Seq.map (fun m -> string m.Value) |> List.ofSeq
                if not (mem.Accessibility.IsPrivate || mem.Accessibility.IsInternal) then
                    let msg = sprintf "Proxy constructor do not match any constructor signatures of target class. Current: %s, candidates: %s" (string cdef.Value) (String.concat ", " candidates)
                    comp.AddWarning(Some (CodeReader.getRange mem.DeclarationLocation), SourceWarning msg)
        | _ -> ()
        if mAnnot.IsJavaScriptExport then
            comp.AddJavaScriptExport (ExportNode (ConstructorNode (def, cdef)))
        clsMembers.Add (NotResolvedMember.Constructor (cdef, (getUnresolved mem mAnnot kind compiled curriedArgs expr)))

    let annotations = Dictionary ()
        
    let rec getAnnot x : A.MemberAnnotation =
        let mem = sr.ReadMember(x, cls)
        match annotations.TryFind mem with
        | Some a -> a
        | _ -> 
            let a = 
                sr.AttributeReader.GetMemberAnnot(annot, x.Attributes)
                |> fixMemberAnnot getAnnot cls x
            annotations.Add(mem, a)
            a

    let stubs = HashSet()
    let mutable hasStubMember = false
    let mutable hasNonStubMember = false

    for meth in cls.MembersFunctionsAndValues do
        if meth.IsProperty then () else
        let mAnnot = getAnnot meth
        
        let error m = 
            comp.AddError(Some (CodeReader.getRange meth.DeclarationLocation), SourceError m)

        match mAnnot.Kind with
        | Some A.MemberKind.Stub when not isThisInterface ->
            hasStubMember <- true
            let memdef = sr.ReadMember(meth, cls)
            match memdef with
            | Member.Method (isInstance, mdef) ->
                let expr, err = Stubs.GetMethodInline annot mAnnot (cls.IsFSharpModule && not meth.IsValCompiledAsMethod) isInstance def mdef
                err |> Option.iter error
                stubs.Add memdef |> ignore
                addMethod (Some (meth, memdef)) mAnnot mdef nrInline true None expr
            | Member.Constructor cdef ->
                let expr = Stubs.GetConstructorInline annot mAnnot def cdef
                addConstructor (Some (meth, memdef)) mAnnot cdef nrInline true None expr
            | Member.Implementation _ -> error "Implementation method can't have Stub attribute"
            | Member.Override _ -> error "Override method can't have Stub attribute"
            | Member.StaticConstructor -> error "Static constructor can't have Stub attribute"
        | Some A.MemberKind.JavaScript when meth.IsDispatchSlot && not isThisInterface -> 
            let memdef = sr.ReadMember meth
            match memdef with
            | Member.Method (isInstance, mdef) ->
                if not isInstance then failwith "Abstract method should not be static" 
                addMethod (Some (meth, memdef)) mAnnot mdef N.Abstract true None Undefined
            | _ -> failwith "Member kind not expected for astract method"
        | Some (A.MemberKind.Remote rp) ->
            let memdef = sr.ReadMember meth
            match memdef with
            | Member.Method (isInstance, mdef) ->
                let remotingKind =
                    match mdef.Value.ReturnType with
                    | VoidType -> RemoteSend
                    | ConcreteType { Entity = e } when e = Definitions.Async -> RemoteAsync
                    | ConcreteType { Entity = e } when e = Definitions.Task || e = Definitions.Task1 -> RemoteTask
                    | _ -> RemoteSync
                let handle = 
                    comp.GetRemoteHandle(
                        def.Value.FullName + "." + mdef.Value.MethodName,
                        mdef.Value.Parameters,
                        mdef.Value.ReturnType
                    )
                let pars = Seq.concat meth.CurriedParameterGroups |> List.ofSeq
                let vars =
                    match pars with 
                    | [ u ] when CodeReader.isUnit u.Type -> []
                    | _ ->
                        pars
                        |> List.map (fun p ->
                            Id.New(?name = p.Name, mut = false)
                        ) 
                addMethod (Some (meth, memdef)) mAnnot mdef (N.Remote(remotingKind, handle, vars, rp, None, None)) false None Undefined
            | _ -> error "Only methods can be defined Remote"
        | _ -> ()

    if isThisInterface && clsMembers.Count = 0 then None else

    let fsharpSpecificNonException =
        cls.IsFSharpUnion || cls.IsFSharpRecord || cls.IsValueType

    let fsharpSpecific = 
        fsharpSpecificNonException || cls.IsFSharpExceptionDeclaration

    let fsharpModule = cls.IsFSharpModule

    let clsTparams =
        cls.GenericParameters |> Seq.mapi (fun i p -> p.Name, i) |> Map.ofSeq

    let inlinesOfClass =
        members |> Seq.choose (fun m ->
            match m with 
            | SourceMember (mem, _, _) ->
                let memdef = sr.ReadMember(mem, cls)
                match memdef with
                | Member.Method (_, meth) -> 
                    let mAnnot = getAnnot mem    
                    match mAnnot.Kind with
                    | Some A.MemberKind.JavaScript -> None
                    | Some _ -> Some meth
                    | _ -> None
                | _ -> None
            | _ -> None   
        )
        |> HashSet

    let baseCls =
        if fsharpSpecificNonException || fsharpModule || cls.IsValueType || annot.IsStub || def.Value.FullName = "System.Object" || isInterfaceProxy then
            None
        elif annot.Prototype = Some false then
            cls.BaseType |> Option.bind (fun t -> t |> sr.ReadType clsTparams |> getConcreteType |> ignoreSystemObject)
        else 
            cls.BaseType |> Option.map (fun t -> t |> sr.ReadType clsTparams |> getConcreteType)

    let implements =
        cls.AllInterfaces |> Seq.map (fun t -> t |> sr.ReadType clsTparams |> getConcreteType) |> List.ofSeq

    let mutable selfCtorFields = []

    for i = 0 to members.Count - 1 do
        let m = members.[i]
        match m with
        | SourceMember (meth, args, expr) ->        
            if meth.IsProperty || (fsharpSpecific && meth.IsCompilerGenerated) then () else

            let mAnnot, isModulePattern = 
                if fsharpModule && meth.IsCompilerGenerated && i < members.Count then
                    // in module compiler-generated members created for pattern matching on let, look for the next member
                    match members.[i + 1] with
                    | SourceMember (nmeth, _, _) ->  
                        getAnnot nmeth, true        
                    | _ -> getAnnot meth, false
                else
                    getAnnot meth, false
            
            let getArgsAndThis() =
                let isExt = meth.IsExtensionMember 
                let a, t =
                    args |> List.concat
                    |> function
                    | t :: r when (t.IsMemberThisValue || t.IsConstructorThisValue) && not isExt -> r, Some t
                    | a -> a, None

                try
                    a
                    |> function 
                    | [ u ] when CodeReader.isUnit u.FullType -> []
                    | a -> 
                        let ps = Seq.concat meth.CurriedParameterGroups |> Seq.map Some |> List.ofSeq
                        // extension members have an extra parameter for the this value, not visible in CurriedParameterGroups
                        let ps = if isExt && meth.IsInstanceMember then None :: ps else ps
                        List.zip a ps
                    , t
                with _ ->
                    failwithf "different arguments as params: %s.%s" def.Value.FullName meth.FullName

            let getParamIsOpt (a: list<FSMFV * _>) =
                // if there is only a single parameter and it's generic, make it optional
                match a with
                | [ p, _ ] -> p.FullType.IsGenericParameter
                | _ -> false
            
            let getVarsAndThis() =
                let a, t = getArgsAndThis()
                let isOpt = getParamIsOpt a
                a |> List.map (fun (x, p) -> 
                    CodeReader.namedId None (isOpt || Option.exists isOptionalParam p) x
                ),
                t |> Option.map (fun p -> CodeReader.namedId None false p)
               
            let error m = comp.AddError(Some (CodeReader.getRange meth.DeclarationLocation), SourceError m)
            let warn m = comp.AddWarning(Some (CodeReader.getRange meth.DeclarationLocation), SourceWarning m)

            match mAnnot.Kind with
            | Some A.MemberKind.Stub
            | Some (A.MemberKind.Remote _) -> ()
            | Some kind ->
                hasNonStubMember <- true
                let memdef = sr.ReadMember(meth, cls)

                if stubs.Contains memdef then () else
                let kind =
                    // for Proxy projects only, handle F# inline as WS Inline
                    if Option.isSome comp.ProxyTargetName && kind = A.MemberKind.JavaScript then
                        match meth.InlineAnnotation with
                        | FSharpInlineAnnotation.AggressiveInline
                        | FSharpInlineAnnotation.AlwaysInline ->
                            A.MemberKind.InlineJavaScript
                        | _ -> kind
                    else kind
                let getBody isInline = 
                    let noCurriedOpt =
                        match memdef with
                        | Member.Method _ 
                        | Member.Constructor _ -> false
                        | _ -> true
                    try
                        let fromRD = 
                            let hasRD =
                                meth.Attributes |> Seq.exists (fun a -> a.AttributeType.FullName = "Microsoft.FSharp.Core.ReflectedDefinitionAttribute")
                            if hasRD then
                                let info =
                                    match memdef with
                                    | Member.Method (_, mdef) 
                                    | Member.Override (_, mdef) 
                                    | Member.Implementation (_, mdef) ->
                                        Reflection.LoadMethod def mdef :> System.Reflection.MethodBase
                                    | Member.Constructor cdef ->
                                        Reflection.LoadConstructor def cdef :> _
                                    | Member.StaticConstructor ->
                                        (Reflection.LoadTypeDefinition def).GetConstructors(System.Reflection.BindingFlags.Static).[0] :> _
                                WebSharper.Compiler.ReflectedDefinitionReader.readReflected comp info 
                            else None
                        match fromRD with
                        | Some rd ->
                            // TODO: curried argument optimization for ReflectedDefinition
                            None, rd
                        | _ ->
                        let a, t = getArgsAndThis()
                        let isOpt = getParamIsOpt a
                        let argsAndVars = 
                            [
                                match t with
                                | Some t ->
                                    yield t, (CodeReader.namedId None false t, CodeReader.ThisArg)
                                | _ -> ()
                                for x, p in a ->   
                                    x, 
                                    (CodeReader.namedId None (isOpt || Option.exists isOptionalParam p) x, 
                                        if CodeReader.isByRef x.FullType 
                                        then CodeReader.ByRefArg 
                                        else
                                            if noCurriedOpt then CodeReader.LocalVar
                                            else
                                                match CodeReader.getFuncArg x.FullType with
                                                | NotOptimizedFuncArg -> CodeReader.LocalVar
                                                | _ -> CodeReader.FuncArg
                                    )
                            ]
                        // search for curried function arguments and register them
                        let curriedArgs =
                            if noCurriedOpt then None else
                            let mem =
                                match memdef with
                                | Member.Method (_, mdef) -> Some (ArgCurrying.Member.Method(def, mdef))
                                | Member.Constructor cdef -> Some (ArgCurrying.Member.Constructor(def, cdef))
                                | _ -> None
                            match mem with
                            | None -> None
                            | Some mem ->
                            let ca = 
                                a |> List.map (fun (x, p) -> 
                                    let fa = CodeReader.getFuncArg x.FullType
                                    match fa, p with 
                                    | NotOptimizedFuncArg, Some p ->
                                        if CodeReader.isByRef x.FullType then
                                            refKindOfParam p
                                        else
                                            NotOptimizedFuncArg     
                                    | _ -> fa
                                )
                            if ca |> List.forall ((=) NotOptimizedFuncArg) then None 
                            else 
                                let args =
                                    argsAndVars |> List.map (snd >> fst)
                                    |> if Option.isSome t then List.skip 1 else id    
                                Some (mem, ca, args, Option.isSome t)

                        let tparams = meth.GenericParameters |> Seq.map (fun p -> p.Name) |> List.ofSeq 
                        
                        let isCtor =
                            match memdef with
                            | Member.Constructor _ -> true
                            | _ -> false

                        let env = CodeReader.Environment.New (argsAndVars, isCtor, tparams, comp, sr, recMembers)  
                        let res =
                            let b = CodeReader.transformExpression env expr 
                            match env.RecMemberUsed with
                            | Some (i, expr) ->
                                let env = CodeReader.Environment.New ([], false, [], comp, sr, recMembers)  
                                let b = CodeReader.transformExpression env expr 
                                let bWithFunc =
                                    match env.RecMemberUsed with
                                    | Some (i, expr) ->
                                        let env = CodeReader.Environment.New ([], false, [], comp, sr, recMembers)  
                                        let fb = CodeReader.transformExpression env expr 
                                        SubstituteVar(i, Lambda ([], None, fb)).TransformExpression(b)
                                    | _ -> b
                                let _, (recContent, _, _) = sc.Value
                                recContent.Add (VarDeclaration (i, bWithFunc)) 
                            | _ -> ()
                            let b = 
                                match memdef with
                                | Member.Constructor _ when meth.CompiledName <> "CtorProxy" -> 
                                    try 
                                        let b, cgenFieldNames = CodeReader.fixCtor def baseCls env.This b
                                        selfCtorFields <- cgenFieldNames
                                        b
                                    with e ->
                                        let tryGetExprSourcePos expr =
                                            match expr with
                                            | ExprSourcePos (p, _) -> Some p
                                            | _ -> None
                                        comp.AddError(tryGetExprSourcePos b, SourceError e.Message)
                                        errorPlaceholder
                                | _ -> b
                            if List.isEmpty args && meth.IsModuleValueOrMember then 
                                if isModulePattern then
                                    let scDef, (_, scContent, scFields) = sc.Value   
                                    let var = Id.New(mut = false)
                                    scContent.Add (VarDeclaration (var, TailCalls.optimize None inlinesOfClass b))
                                    Var var
                                elif isInline then
                                    b
                                else
                                    let scDef, (_, scContent, scFields) = sc.Value   
                                    let mtyp =
                                        match memdef with
                                        | Member.Method (_, mdef) -> mdef.Value.ReturnType
                                        | _ -> failwith "F# Module value or member should be represented as a static method"
                                    let name = Resolve.getRenamedInDict meth.CompiledName mtyp scFields
                                    scContent.Add (ExprStatement (ItemSet(JSThis, Value (String name), TailCalls.optimize None inlinesOfClass b)))
                                    Lambda([], Some mtyp, FieldGet(None, NonGeneric scDef, name))
                            else
                                let thisVar, vars =
                                    match argsAndVars with 
                                    | (_, (t, CodeReader.ThisArg)) :: a -> Some t, a |> List.map (snd >> fst)  
                                    | a -> None, a |> List.map (snd >> fst) 
                                let defValues = 
                                    Seq.zip (Seq.concat meth.CurriedParameterGroups) vars |> Seq.choose (fun (p, i) ->
                                        if p.Attributes |> Seq.exists (fun pa -> pa.AttributeType.FullName = "System.Runtime.InteropServices.OptionalAttribute") then
                                            let d =
                                                p.Attributes |> Seq.tryPick (fun pa -> 
                                                    if pa.AttributeType.FullName = "System.Runtime.InteropServices.DefaultParameterValueAttribute" then
                                                        Some (ReadLiteral (snd pa.ConstructorArguments.[0]) |> Value) 
                                                    else None
                                                )
                                            Some (i,
                                                match d with
                                                | Some v -> v
                                                | _ -> DefaultValueOf (sr.ReadType env.TParams p.Type)
                                            )
                                        else None
                                    )
                                    |> List.ofSeq
                                let vars = 
                                    if List.isEmpty defValues then
                                        vars
                                    else
                                        // mark vars with default value setters mutable
                                        vars |> List.map (fun v -> if defValues |> List.exists (fun (i, _) -> i = v) then v.ToMutable() else v)
                                let b =
                                    if List.isEmpty defValues then b else
                                        Sequential [
                                            for i, v in defValues -> VarSet(i, Conditional (Var i ^=== Undefined, v, Var i))
                                            yield b
                                        ]
                                if isInline then
                                    makeExprInline (Option.toList thisVar @ vars) b
                                else 
                                    let returnType =
                                        match memdef with
                                        | Member.Method (_, mdef)  
                                        | Member.Override (_, mdef) 
                                        | Member.Implementation (_, mdef) ->
                                            mdef.Value.ReturnType
                                        | _ -> VoidType
                                    if returnType = VoidType then
                                        Function(vars, env.This, None, ExprStatement b)
                                    else
                                        Function(vars, env.This, Some returnType, Return b)
                        let currentMethod =
                            match memdef with
                            | Member.Method (_, m) -> 
                                Some (def, m) 
                            | _ -> None
                        curriedArgs, TailCalls.optimize currentMethod inlinesOfClass res
                    with e ->
                        error (sprintf "Error reading definition: %s at %s" e.Message e.StackTrace)
                        None, errorPlaceholder

                let getImport() =
                    mAnnot.Import |> Option.map comp.JSImport

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
                    
                    let getInlineKind() =
                        match memdef with
                        | Member.Implementation (t, _) -> 
                            N.InlineImplementation t
                        | _ -> nrInline

                    let addModuleValueProp kind body =
                        if List.isEmpty args && fsharpModule then
                            let iBody = Call(None, NonGeneric def, Generic mdef (List.init mdef.Value.Generics TypeParameter), [])
                            // TODO : check proxy targets for module values
                            addMethod None mAnnot (Method { mdef.Value with MethodName = "get_" + mdef.Value.MethodName }) nrInline false None iBody    
                            if meth.IsMutable then 
                                let setm = 
                                    let me = mdef.Value
                                    Method {
                                        MethodName = "set_" + me.MethodName   
                                        Parameters = [ me.ReturnType ]
                                        ReturnType = VoidType
                                        Generics = 0
                                    }
                                let setb =
                                    match body with
                                    | Function([], arr, ret, Return (FieldGet(None, {Entity = scDef; Generics = []}, name))) ->
                                        let value = CodeReader.newId()                          
                                        Function ([value], arr, ret, (ExprStatement <| FieldSet(None, NonGeneric scDef, name, Var value)))
                                    | _ -> 
                                        error "unexpected form in module let body"
                                        Undefined
                                        //failwith "unexpected form in module let body"
                                addMethod None { mAnnot with Name = None } setm kind false None setb    
                            true
                        else false

                    let addM = addMethod (Some (meth, memdef)) mAnnot mdef

                    let jsMethod isInline =
                        let kind = if isInline || isModulePattern then getInlineKind() else getKind()
                        let ca, body = getBody isInline                        
                        if isModulePattern || addModuleValueProp kind body then
                            addMethod None mAnnot mdef kind false ca body  
                        else addM kind false ca body

                    let checkNotAbstract() =
                        if meth.IsDispatchSlot then
                            error "Abstract methods cannot be marked with Inline, Macro or Constant attributes."
                        elif not isInterfaceProxy then
                            match memdef with
                            | Member.Override (bTyp, _) -> 
                                if not (bTyp = Definitions.Obj || bTyp = Definitions.ValueType) then
                                    error "Override methods cannot be marked with Inline, Macro or Constant attributes."
                            | Member.Implementation _ ->
                                error "Interface implementation methods cannot be marked with Inline, Macro or Constant attributes."
                            | _ -> ()

                    match kind with
                    | A.MemberKind.NoFallback ->
                        checkNotAbstract()
                        addM N.NoFallback true None Undefined
                    | A.MemberKind.Inline (js, ta, dollarVars) ->
                        checkNotAbstract() 
                        let vars, thisVar = getVarsAndThis()
                        try 
                            let nr = N.Inline ta
                            let parsed = WebSharper.Compiler.Recognize.createInline comp.MutableExternals thisVar vars mAnnot.Pure (getImport()) dollarVars comp.AssemblyName js
                            List.iter warn parsed.Warnings
                            if addModuleValueProp nr parsed.Expr then
                                addMethod None mAnnot mdef nr true None parsed.Expr   
                            else addM nr true None parsed.Expr
                        with e ->
                            error ("Error parsing inline JavaScript: " + e.Message + " at " + e.StackTrace)
                    | A.MemberKind.Constant c ->
                        checkNotAbstract() 
                        addM nrInline true None (Value c)                        
                    | A.MemberKind.Direct (js, dollarVars) ->
                        let vars, thisVar = getVarsAndThis()
                        try
                            let parsed = WebSharper.Compiler.Recognize.parseDirect comp.MutableExternals thisVar vars dollarVars comp.AssemblyName js
                            List.iter warn parsed.Warnings
                            addM (getKind()) true None parsed.Expr
                        with e ->
                            error ("Error parsing direct JavaScript: " + e.Message + " at " + e.StackTrace)
                    | A.MemberKind.JavaScript ->
                        jsMethod false
                    | A.MemberKind.InlineJavaScript ->
                        checkNotAbstract()
                        jsMethod true
                    | A.MemberKind.OptionalField ->
                        if meth.IsPropertyGetterMethod then
                            let i = JSRuntime.GetOptional (ItemGet(Hole 0, Value (String meth.CompiledName.[4..]), Pure))
                            addM nrInline true None i
                        elif meth.IsPropertySetterMethod then  
                            let i = JSRuntime.SetOptional (Hole 0) (Value (String meth.CompiledName.[4..])) (Hole 1)
                            addM nrInline true None i
                        else error "OptionalField attribute not on property"
                    | A.MemberKind.Generated _ ->
                        addM (getKind()) false None Undefined
                    | A.MemberKind.AttributeConflict m -> error m
                    | A.MemberKind.Remote _ 
                    | A.MemberKind.Stub -> failwith "should be handled previously"
                    if mAnnot.IsEntryPoint then
                        match memdef with
                        | Member.Method (false, mdef) when List.isEmpty mdef.Value.Parameters ->
                            let ep = ExprStatement <| Call(None, NonGeneric def, NonGeneric mdef, [])
                            if comp.HasGraph then
                                comp.Graph.AddEdge(EntryPointNode, MethodNode (def, mdef))
                            comp.SetEntryPoint(ep)
                        | _ ->
                            error "The SPAEntryPoint must be a static method with no arguments"
                | Member.Constructor cdef ->
                    let addC = addConstructor (Some (meth, memdef)) mAnnot cdef
                    let jsCtor isInline =   
                        if isInline then 
                            addC nrInline false <|| getBody true
                        else
                            addC N.Constructor false <|| getBody false
                    match kind with
                    | A.MemberKind.NoFallback ->
                        addC N.NoFallback true None Undefined
                    | A.MemberKind.Inline (js, ta, dollarVars) ->
                        let vars, thisVar = getVarsAndThis()
                        try
                            let parsed = WebSharper.Compiler.Recognize.createInline comp.MutableExternals thisVar vars mAnnot.Pure (getImport()) dollarVars comp.AssemblyName js
                            List.iter warn parsed.Warnings
                            addC (N.Inline ta) true None parsed.Expr 
                        with e ->
                            error ("Error parsing inline JavaScript: " + e.Message + " at " + e.StackTrace)
                    | A.MemberKind.Direct (js, dollarVars) ->
                        let vars, thisVar = getVarsAndThis()
                        try
                            let parsed = WebSharper.Compiler.Recognize.parseDirect comp.MutableExternals thisVar vars dollarVars comp.AssemblyName js
                            List.iter warn parsed.Warnings
                            addC N.Static true None parsed.Expr 
                        with e ->
                            error ("Error parsing direct JavaScript: " + e.Message + " at " + e.StackTrace)
                    | A.MemberKind.JavaScript -> jsCtor false
                    | A.MemberKind.InlineJavaScript -> jsCtor true
                    | A.MemberKind.Generated _ ->
                        addC N.Static false None Undefined
                    | A.MemberKind.AttributeConflict m -> error m
                    | A.MemberKind.Remote _
                    | A.MemberKind.Stub -> failwith "should be handled previously"
                    | A.MemberKind.OptionalField
                    | A.MemberKind.Constant _ -> failwith "attribute not allowed on constructors"
                | Member.StaticConstructor ->
                    let body =
                        match getBody false with
                        | _, Function([], _, _, body) -> body
                        | _ -> failwithf "static constructor should be a function"
                    clsMembers.Add (NotResolvedMember.StaticConstructor body)
            | None ->
                if isProxy then
                    let memdef = sr.ReadMember(meth, cls)
                    match memdef with
                    | Member.Implementation (t, mdef) ->    
                        addMethod (Some (meth, memdef)) mAnnot mdef (N.MissingImplementation t) true None Undefined
                    | _ -> ()
            let jsArgs =
                meth.CurriedParameterGroups
                |> Seq.concat
                |> Seq.mapi (fun i p -> i, sr.AttributeReader.GetParamAnnot(p.Attributes).ClientAccess)
                |> Seq.choose (fun (i, x) -> if x then Some i else None)
                |> Array.ofSeq
            if not (Array.isEmpty jsArgs) then
                match sr.ReadMember(meth, cls) with
                | Member.Method (_, mdef) -> comp.AddQuotedArgMethod(thisDef, mdef, jsArgs)
                | Member.Constructor cdef -> comp.AddQuotedConstArgMethod(thisDef, cdef, jsArgs)
                | _ -> error "JavaScript attribute on parameter is only allowed on methods and constructors"
            let tparams = meth.GenericParameters |> Seq.map (fun p -> p.Name) |> List.ofSeq 
            let env = CodeReader.Environment.New ([], false, tparams, comp, sr, recMembers)
            CodeReader.scanExpression env meth.LogicalName expr
            |> Seq.iter (fun (pos, mdef, argNames, e) ->
                addMethod None A.MemberAnnotation.BasicJavaScript mdef (N.Quotation(pos, argNames)) false None e 
            )
        | SourceEntity (ent, nmembers) ->
            transformClass sc comp ac sr classAnnots isInterface recMembers ent nmembers |> Option.iter comp.AddClass   
        | SourceInterface i ->
            transformInterface sr annot i |> Option.iter comp.AddInterface
            transformClass sc comp ac sr classAnnots isInterface recMembers i (ResizeArray()) |> Option.iter comp.AddClass   
        | InitAction expr ->
            transformInitAction sc comp sr annot recMembers expr    

    if isInterfaceProxy then
        let methods = 
            clsMembers |> Array.ofSeq |> Array.choose (fun m ->
                match m with
                | NotResolvedMember.Method (mem, {Kind = NotResolvedMemberKind.Abstract; StrongName = sn; Generics = gc }) ->
                    clsMembers.Remove(m) |> ignore
                    Some (mem, sn, gc)
                | _ -> None 
            )     
        let extends =
            annot.ProxyExtends
            |> List.map (fun et ->
                Generic et (List.init et.Value.GenericLength TypeParameter) 
            )

        let intf =
            {
                StrongName = annot.Name 
                Extends = extends
                NotResolvedMethods = List.ofArray methods 
                Generics = getConstraints cls.GenericParameters sr clsTparams
                Type = annot.Type
            }
        comp.AddInterface(def, intf)

    if cls.IsFSharpRecord then
        cls.FSharpFields |> Seq.iter (fun f ->
            let fTyp = sr.ReadType clsTparams f.FieldType
            let fAnnot = sr.AttributeReader.GetMemberAnnot(annot, Seq.append (Seq.append f.FieldAttributes f.PropertyAttributes) f.Attributes)
            match fAnnot.Kind with
            | Some (A.MemberKind.Remote rp) ->

                let args, returnType =
                    match fTyp with
                    | Type.FSharpFuncType (arg, returnType) ->
                        let rec recursiveProcessing (t: Type) (args: Type list) =
                            match t with
                            | Type.FSharpFuncType (arg, returnType) ->
                                recursiveProcessing returnType (List.append args [arg])
                            | t -> args, t
                        recursiveProcessing returnType [arg]
                    | _ ->
                        comp.AddError(None, CompilationError.SourceError "The Remote attribute should only be used with lambda on a record type")
                        [], VoidType
                
                let mdef =
                    Hashed {
                        MethodName = "get_" + f.Name
                        Parameters = []
                        ReturnType = fTyp
                        Generics = 0
                    }

                let remotingKind =
                    match returnType with
                    | VoidType -> RemoteSend
                    | ConcreteType { Entity = e } when e = Definitions.Async -> RemoteAsync
                    | ConcreteType { Entity = e } when e = Definitions.Task || e = Definitions.Task1 -> RemoteTask
                    | _ -> RemoteSync
                let handle = 
                    comp.GetRemoteHandle(
                        def.Value.FullName + "." + f.Name,
                        [],
                        VoidType
                    )
                let pars =
                    match args with
                    | [ VoidType ] -> []
                    | args -> args
                let vars =
                    pars
                    |> List.map (fun p ->
                        Id.New(?name = None, mut = false)
                    ) 
                addMethod None fAnnot mdef (N.Remote(remotingKind, handle, vars, rp, Some returnType, Some pars)) false None Undefined
            | _ -> ()
        )

    if not annot.IsJavaScript && clsMembers.Count = 0 && annot.Macros.IsEmpty then None else

    let ckind = 
        if annot.IsStub || (hasStubMember && not hasNonStubMember)
        then NotResolvedClassKind.Stub
        elif fsharpModule then NotResolvedClassKind.Static
        elif (annot.IsJavaScript && ((isAbstractClass cls && not isInterfaceProxy) || cls.IsFSharpExceptionDeclaration)) || (annot.Prototype = Some true)
        then NotResolvedClassKind.WithPrototype
        else NotResolvedClassKind.Class

    let hasWSPrototype =                
        hasWSPrototype ckind baseCls clsMembers

    let mutable hasSingletonCase = false
    let mutable hasConstantCase = false

    let notForcedNotJavaScript = not annot.IsForcedNotJavaScript

    if annot.IsJavaScript || hasWSPrototype || isAugmentedFSharpType cls then
        if cls.IsFSharpUnion then
            let usesNull =
                cls.UnionCases.Count < 4 // see TaggingThresholdFixedConstant in visualfsharp/src/ilx/EraseUnions.fs
                && cls.Attributes |> CodeReader.hasCompilationRepresentation CompilationRepresentationFlags.UseNullAsTrueValue
                && cls.UnionCases |> Seq.exists (fun c -> c.Fields.Count = 0)

            let mutable nullCase = usesNull 

            let constants = HashSet() 

            let cases =
                cls.UnionCases
                |> Seq.mapi (fun i case ->
                    let constantCase v =
                        hasConstantCase <- true
                        if constants.Add(v) then
                            ConstantFSharpUnionCase v
                        else
                            comp.AddError(Some (CodeReader.getRange case.DeclarationLocation), 
                                SourceError "Union case translated constant value is a duplicate")
                            ConstantFSharpUnionCase (String "$$ERROR$$")
                    let cAnnot = sr.AttributeReader.GetMemberAnnot(annot, case.Attributes)
                    let kind =
                        let argumentless = case.Fields.Count = 0
                        if nullCase && argumentless then
                            nullCase <- false
                            constantCase Null
                        else
                        match cAnnot.Kind with
                        | Some (A.MemberKind.Constant v) -> 
                            constantCase v
                        | _ ->
                            if argumentless && notForcedNotJavaScript then
                                let caseField =
                                    let gen = List.replicate cls.GenericParameters.Count (NonGenericType Definitions.Obj)
                                    GenericType def gen
                                    |> Definitions.SingletonUnionCase case.CompiledName
                                let expr = CopyCtor(def, Object [ "$", MemberKind.Simple, Value (Int i) ])
                                let a = { A.MemberAnnotation.BasicPureJavaScript with Name = Some case.Name }
                                clsMembers.Add (NotResolvedMember.Method (caseField, (getUnresolved None a N.Static false None expr)))
                                hasSingletonCase <- true
                                SingletonFSharpUnionCase
                            else
                                NormalFSharpUnionCase (
                                    case.Fields
                                    |> Seq.map (fun f ->
                                        {
                                            Name = f.Name
                                            UnionFieldType = sr.ReadType clsTparams f.FieldType
                                            DateTimeFormat = 
                                                cAnnot.DateTimeFormat 
                                                |> List.tryPick (fun (target, format) -> if target = Some f.Name then Some format else None)
                                        }
                                    )
                                    |> List.ofSeq
                                )
                    let staticIs =
                        not usesNull || not (
                            case.Attributes
                            |> CodeReader.hasCompilationRepresentation CompilationRepresentationFlags.Instance
                        )
                    {
                        Name = case.Name
                        JsonName = cAnnot.Name
                        Kind = kind
                        StaticIs = staticIs
                    }
                )
                |> List.ofSeq

            let i =
                FSharpUnionInfo {
                    Cases = cases
                    NamedUnionCases = annot.NamedUnionCases
                    HasNull = constants.Contains(Null)
                }

            comp.AddCustomType(def, i)

            if annot.IsJavaScript || hasWSPrototype then
                for index, c in cases |> Seq.indexed do
                    match c.Kind with
                    | NormalFSharpUnionCase fields ->
                        let newCase =
                            Method {
                                MethodName = "New" + c.Name
                                Parameters = fields |> List.map (fun f -> f.UnionFieldType) 
                                ReturnType = ConcreteType thisType
                                Generics = cls.GenericParameters.Count       
                            }
                        let args = fields |> List.map (fun f -> Id.New(f.Name, mut = false, typ = f.UnionFieldType)) 
                        let obj =
                            Object (
                                ("$", MemberKind.Simple, Value (Int index)) ::
                                (args |> List.mapi (fun j a -> "$" + string j, MemberKind.Simple, Var a)) 
                            )
                        let newCaseM =
                            {
                                Kind = NotResolvedMemberKind.Static
                                StrongName = Some c.Name
                                Generics = []
                                Macros = []
                                Generator = None
                                Compiled = false
                                Pure = true
                                FuncArgs = None
                                Args = args
                                Body = Function(args, None, Some (ConcreteType thisType), Return (CopyCtor(def, obj)))
                                Requires = []
                                Warn = None
                                JavaScriptOptions = WebSharper.JavaScriptOptions.None
                            }
                        clsMembers.Add (NotResolvedMember.Method (newCase, newCaseM))
                    | _ -> ()

        if (cls.IsFSharpRecord || cls.IsFSharpExceptionDeclaration) then
            if annot.IsJavaScript || hasWSPrototype then
                let cdef =
                    Hashed {
                        CtorParameters =
                            cls.FSharpFields |> Seq.map (fun f -> sr.ReadType clsTparams f.FieldType) |> List.ofSeq
                    }
                let body =
                    let vars =
                        cls.FSharpFields |> Seq.map (fun f -> Id.New(f.Name, mut = false)) |> List.ofSeq
                    let fields =
                        cls.FSharpFields |> Seq.map (fun f -> 
                            let fAnnot = sr.AttributeReader.GetMemberAnnot(annot, Seq.append f.FieldAttributes f.PropertyAttributes)
                    
                            match fAnnot.Name with Some n -> n | _ -> f.Name
                            , 
                            fAnnot.Kind = Some A.MemberKind.OptionalField && CodeReader.isOption f.FieldType
                        )
                        |> List.ofSeq
                    let obj = 
                        let normalFields =
                            Seq.zip (fields) vars
                            |> Seq.choose (fun ((name, opt), v) -> if opt then None else Some (name, MemberKind.Simple, Var v))
                            |> List.ofSeq |> Object
                        if fields |> List.exists snd then
                            let o = CodeReader.newId()
                            Let(o, normalFields, 
                                Sequential [
                                    for (name, opt), v in Seq.zip fields vars do
                                        if opt then yield JSRuntime.SetOptional (Var o) (Value (String name)) (Var v)
                                    yield Var o
                                ]
                            )
                        else 
                            normalFields
                    Lambda (vars, None, CopyCtor(def, obj))

                let cKind = if annot.IsForcedNotJavaScript then nrInline else N.Static
                addConstructor None A.MemberAnnotation.BasicPureJavaScript cdef cKind false None body

                // properties

                for f in cls.FSharpFields do
                    let fTyp = sr.ReadType clsTparams f.FieldType
            
                    let getDef =
                        Hashed {
                            MethodName = "get_" + f.Name
                            Parameters = []
                            ReturnType = fTyp
                            Generics = 0
                        }

                    let getBody = FieldGet(Some (Hole 0), thisType, f.Name)
                
                    addMethod None A.MemberAnnotation.BasicPureInlineJavaScript getDef nrInline false None getBody

                    if f.IsMutable then
                        let setDef =
                            Hashed {
                                MethodName = "set_" + f.Name
                                Parameters = [ fTyp ]
                                ReturnType = VoidType
                                Generics = 0
                            }

                        let setBody = FieldSet(Some (Hole 0), thisType, f.Name, Hole 1)
            
                        addMethod None A.MemberAnnotation.BasicInlineJavaScript setDef nrInline false None setBody

        if cls.IsFSharpRecord then
            let i = 
                cls.FSharpFields |> Seq.choose (fun f ->
                    let fTyp = sr.ReadType clsTparams f.FieldType
                    let fAnnot = sr.AttributeReader.GetMemberAnnot(annot, Seq.append (Seq.append f.FieldAttributes f.PropertyAttributes) f.Attributes)
                    
                    match fAnnot.Kind with
                    | Some (A.MemberKind.Remote _) -> None
                    | _ ->
                        let isOpt = fAnnot.Kind = Some A.MemberKind.OptionalField && CodeReader.isOption f.FieldType

                        {
                            Name = f.Name
                            JSName = match fAnnot.Name with Some n -> n | _ -> f.Name // TODO : set in resolver instead
                            RecordFieldType = fTyp
                            DateTimeFormat = fAnnot.DateTimeFormat |> List.tryHead |> Option.map snd
                            Optional = isOpt
                            IsMutable = f.IsMutable
                        }
                        |> Some
                )
                |> List.ofSeq |> function [] -> None | l -> l |> FSharpRecordInfo |> Some
            if comp.HasCustomTypeInfo def then
                printfn "Already has custom type info: %s" def.Value.FullName
            else
                i |> Option.iter (fun i -> comp.AddCustomType(def, i))

        if cls.IsValueType && not (cls.IsFSharpRecord || cls.IsFSharpUnion || cls.IsEnum) then
            // add default constructor for structs
            let cdef = ConstructorInfo.Default()
            let fields =
                cls.FSharpFields |> Seq.map (fun f -> 
                    if f.IsMutable then
                        comp.AddError(Some (CodeReader.getRange f.DeclarationLocation), SourceError "Mutable structs are not supported for JavaScript translation")
                    let fAnnot = sr.AttributeReader.GetMemberAnnot(annot, Seq.append f.FieldAttributes f.PropertyAttributes)
                    
                    match fAnnot.Name with Some n -> n | _ -> f.Name
                    , 
                    DefaultValueOf (sr.ReadType clsTparams f.FieldType)
                )
                |> List.ofSeq
            let thisVar = Id.NewThis()
            let body = Function([], Some thisVar, None, ExprStatement (Sequential (fields |> List.map (fun (n, v) -> ItemSet(Var thisVar, Value (String n), v)))))
            addConstructor None A.MemberAnnotation.BasicPureJavaScript cdef N.Constructor false None body
            comp.AddCustomType(def, StructInfo)

    let fieldLoc (f: FSharpField) =
        let l = f.DeclarationLocation
        l.StartLine, l.StartColumn       
    for i, f in cls.FSharpFields |> Seq.sortBy fieldLoc |> Seq.indexed do
        if selfCtorFields |> List.contains f.Name then () else
        let propertyAttributes =
            if f.IsCompilerGenerated && f.Name.EndsWith "@" then
                // `member val` backing field
                let n = f.Name.TrimEnd('@')
                match cls.MembersFunctionsAndValues |> Seq.tryFind (fun p -> p.IsProperty && p.LogicalName = n) with
                | None ->
                    comp.AddWarning(Some (CodeReader.getRange f.DeclarationLocation),
                        SourceWarning ("Cannot find original property for compiler-generated field " + f.Name))
                    f.PropertyAttributes
                | Some p -> p.Attributes
            else
                f.PropertyAttributes
        let fAnnot = sr.AttributeReader.GetMemberAnnot(annot, Seq.append f.FieldAttributes propertyAttributes)
        match fAnnot.Kind with
        | Some (A.MemberKind.Remote _) -> ()
        | _ ->
            let nr =
                {
                    StrongName = fAnnot.Name
                    IsStatic = f.IsStatic
                    IsOptional = fAnnot.Kind = Some A.MemberKind.OptionalField && CodeReader.isOption f.FieldType
                    IsReadonly = not f.IsMutable
                    FieldType = sr.ReadType clsTparams f.FieldType
                    Order = i
                }
            clsMembers.Add (NotResolvedMember.Field (f.Name, nr))

    let strongName =
        annot.Name |> Option.map (fun n ->
            if n.StartsWith "." then n.TrimStart('.') else
            if n.Contains "." then n else 
                let origName = thisDef.Value.FullName
                origName.[.. origName.LastIndexOf '.'] + n
        )   

    Some (
        def,
        {
            StrongName = strongName
            Generics = getConstraints cls.GenericParameters sr clsTparams
            BaseClass = baseCls
            Implements = implements
            Requires = annot.Requires
            Members = List.ofSeq clsMembers
            Kind = ckind
            IsProxy = Option.isSome annot.ProxyOf
            Macros = annot.Macros
            ForceNoPrototype = (annot.Prototype = Some false) || hasConstantCase || isInterfaceProxy
            ForceAddress = hasSingletonCase
            Type = annot.Type
            SourcePos = CodeReader.getRange cls.DeclarationLocation
        }
    )

let transformAssembly (logger: LoggerBase) (comp : Compilation) assemblyName (config: WsConfig) (checkResults: FSharpCheckProjectResults) =   
    comp.AssemblyName <- assemblyName
    comp.ProxyTargetName <- config.ProxyTargetName
    let sr = CodeReader.SymbolReader(comp)    
    
    let mutable asmAnnot =
        sr.AttributeReader.GetAssemblyAnnot(checkResults.AssemblySignature.Attributes)

    match config.JavaScriptScope with
    | JSDefault -> ()
    | JSAssembly -> asmAnnot <- { asmAnnot with IsJavaScript = true }
    | JSFilesOrTypes a -> asmAnnot <- { asmAnnot with JavaScriptTypesAndFiles = List.ofArray a @ asmAnnot.JavaScriptTypesAndFiles }

    match config.ProjectType with
    | Some Proxy -> asmAnnot <- { asmAnnot with IsJavaScript = true }
    | _ ->  ()

    for jsExport in config.JavaScriptExport do
        comp.AddJavaScriptExport jsExport
        match jsExport with
        | ExportCurrentAssembly -> asmAnnot <- { asmAnnot with IsJavaScript = true }
        | ExportByName n -> asmAnnot <- { asmAnnot with JavaScriptTypesAndFiles = n :: asmAnnot.JavaScriptTypesAndFiles }
        | _ -> ()

    let rootTypeAnnot = asmAnnot.RootTypeAnnot

    comp.AssemblyRequires <- asmAnnot.Requires
    comp.SiteletDefinition <- asmAnnot.SiteletDefinition

    if asmAnnot.IsJavaScriptExport then
        comp.AddJavaScriptExport ExportCurrentAssembly
    for s in asmAnnot.JavaScriptExportTypesFilesAndAssemblies do
        comp.AddJavaScriptExport (ExportByName s)
    
    let lookupAssembly =
        lazy
        Map [
            yield assemblyName, checkResults.AssemblySignature
            for r in checkResults.ProjectContext.GetReferencedAssemblies() do
                yield r.SimpleName, r.Contents 
        ]

    let typeImplLookup = Dictionary<TypeDefinition, FSharpEntity>()
    
    let lookupTypeDefinition (typ: TypeDefinition) =
        match typeImplLookup.TryGetValue(typ) with
        | true, ores -> Some ores
        | _ ->
            let t = typ.Value
            let assemblies =
                if t.Assembly = "netstandard" then
                    lookupAssembly.Value |> Map.toSeq |> Seq.choose (fun (n, a) -> if n.StartsWith "System" then Some a else None)     
                else
                    [| lookupAssembly.Value.[t.Assembly] |]
            let path = t.FullName.Split('+')
            let mutable res =
                assemblies |> Seq.collect (fun a -> a.Entities) |> Seq.tryFind (fun e ->
                    match e.TryFullName with
                    | Some fn when fn = path.[0] -> true
                    | _ -> false
                )
            for i = 1 to path.Length - 1 do
                if res.IsSome then
                    res <- res.Value.NestedEntities |> Seq.tryFind (fun e -> e.CompiledName = path.[i])
            res |> Option.iter (fun td -> typeImplLookup.Add(typ, td))
            res

    let isInterface (typ: TypeDefinition) =
        match lookupTypeDefinition typ with
        | Some e -> e.IsInterface
        | None ->
            try
                let t = Reflection.LoadTypeDefinition typ
                t.IsInterface
            with _ ->
                let msg = "Proxy target type could not be loaded: " + typ.Value.FullName
                comp.AddWarning(None, SourceWarning msg)
                false

    let readAttribute (a: FSharpAttribute) =
        try
            let fixTypeValue (o: obj) =
                match o with
                | :? FSharpType as t -> box (sr.ReadType Map.empty t)
                | _ -> o
            sr.ReadTypeDefinition a.AttributeType,
            a.ConstructorArguments |> Seq.map (snd >> fixTypeValue >> ParameterObject.OfObj) |> Array.ofSeq
        with _ ->
            failwithf "Failed to read custom attribute from F#: %s" a.AttributeType.FullName

    let readAttributes (a: seq<FSharpAttribute>) =
        a |> Seq.map readAttribute |> List.ofSeq

    let lookupTypeAttributes (typ: TypeDefinition) =
        lookupTypeDefinition typ |> Option.map (fun e ->
            e.Attributes |> Seq.map readAttribute |> List.ofSeq
        )

    let lookupFieldAttributes (typ: TypeDefinition) (field: string) =
        lookupTypeDefinition typ |> Option.bind (fun e -> 
            e.FSharpFields |> Seq.tryFind (fun f -> f.Name = field) |> Option.map (fun f ->
                Seq.append f.FieldAttributes f.PropertyAttributes |> Seq.map readAttribute |> List.ofSeq
            ) 
        )

    let lookupMethodAttributes (typ: TypeDefinition) (meth: Method) =
        lookupTypeDefinition typ |> Option.bind (fun e -> 
            e.MembersFunctionsAndValues
            |> Seq.tryFind (fun m -> 
                match sr.ReadMember(m, e) with
                | Member.Method (_, m)
                | Member.Override (_, m)
                | Member.Implementation (_, m) when m = meth -> true
                | _ -> false
            ) 
            |> Option.map (fun m ->
                m.Attributes |> readAttributes
            ) 
            |> Option.orElseWith (fun () ->
                if e.IsFSharpUnion then 
                    let mn = meth.Value.MethodName
                    let caseName =
                        if mn.StartsWith "New" then Some (mn.Substring(3))
                        elif mn.StartsWith "get_" then Some (mn.Substring(4))
                        else None
                    caseName |> Option.bind (fun cn ->
                        let case = e.UnionCases |> Seq.tryFind (fun c -> c.CompiledName = cn)
                        case |> Option.map (fun c ->
                            c.Attributes |> readAttributes
                        )
                    )
                elif e.IsFSharpRecord then
                    let mn = meth.Value.MethodName
                    let fieldName = 
                        if mn.StartsWith "get_" then Some (mn.Substring(4)) else None
                    fieldName |> Option.bind (fun fn ->
                        let field = e.FSharpFields |> Seq.tryFind (fun f -> f.Name = fn)
                        field |> Option.map (fun f ->
                            f.PropertyAttributes |> readAttributes
                        )
                    )
                else None
            )
        ) 

    let lookupConstructorAttributes (typ: TypeDefinition) (ctor: Constructor) =
        lookupTypeDefinition typ |> Option.bind (fun e -> 
            e.MembersFunctionsAndValues
            |> Seq.tryFind (fun m -> 
                match sr.ReadMember(m, e) with
                | Member.Constructor c when c = ctor -> true
                | _ -> false
            ) 
            |> Option.map (fun m ->
                m.Attributes |> readAttributes
            ) 
        )

    let reflectCustomType (typeDef: TypeDefinition): CustomTypeInfo =
        let cls = lookupTypeDefinition(typeDef)
        let branchOnType (entity: FSharpEntity) =
            let clsTparams =
                lazy 
                entity.GenericParameters |> Seq.mapi (fun i p -> p.Name, i) |> Map.ofSeq
            if entity.IsDelegate then
                let tparams = 
                    entity.GenericParameters
                    |> Seq.mapi (fun i p -> p.Name, i) |> Map.ofSeq
                let inv = entity.MembersFunctionsAndValues |> Seq.find(fun m -> m.CompiledName = "Invoke")
                DelegateInfo {
                    DelegateArgs =
                        inv.CurriedParameterGroups |> Seq.concat |> Seq.map (fun p -> sr.ReadType tparams p.Type, None) |> List.ofSeq
                    ReturnType = sr.ReadType tparams inv.ReturnParameter.Type
                }
            else if entity.IsEnum then
                let sampleField = entity.FSharpFields |> Seq.find (fun f -> f.IsLiteral)
                let sampleValue = sampleField.LiteralValue.Value
                let underlyingType = Reflection.ReadTypeDefinition (sampleValue.GetType())
                CustomTypeInfo.EnumInfo underlyingType
            else if entity.IsFSharpRecord then
                let tAnnot = sr.AttributeReader.GetTypeAnnot(AttributeReader.TypeAnnotation.Empty, entity.Attributes) 
                entity.FSharpFields |> Seq.choose (fun f ->
                    let fAnnot = sr.AttributeReader.GetMemberAnnot(tAnnot, Seq.append f.FieldAttributes f.PropertyAttributes)
                    match fAnnot.Kind with
                    | Some (A.MemberKind.Remote _) -> None
                    | _ ->
                        let isOpt = fAnnot.Kind = Some A.MemberKind.OptionalField && CodeReader.isOption f.FieldType
                        let fTyp = sr.ReadType clsTparams.Value f.FieldType
                        {
                            Name = f.Name
                            JSName = match fAnnot.Name with Some n -> n | _ -> f.Name
                            RecordFieldType = fTyp
                            DateTimeFormat = fAnnot.DateTimeFormat |> List.tryHead |> Option.map snd
                            Optional = isOpt
                            IsMutable = f.IsMutable
                        } |> Some 
                )
                |> List.ofSeq |> function [] -> CustomTypeInfo.NotCustomType | l -> FSharpRecordInfo l
            else if entity.IsFSharpUnion then
                let tAnnot = sr.AttributeReader.GetTypeAnnot(AttributeReader.TypeAnnotation.Empty, entity.Attributes)
                let usesNull =
                    entity.UnionCases.Count < 4 // see TaggingThresholdFixedConstant in visualfsharp/src/ilx/EraseUnions.fs
                    && entity.Attributes |> CodeReader.hasCompilationRepresentation CompilationRepresentationFlags.UseNullAsTrueValue
                    && entity.UnionCases |> Seq.exists (fun c -> c.Fields.Count = 0)

                let cases =
                    entity.UnionCases
                    |> Seq.map (fun case ->
                        let cAnnot = sr.AttributeReader.GetMemberAnnot(tAnnot, case.Attributes)
                        let kind =
                            match cAnnot.Kind with
                            | Some (A.MemberKind.Constant v) -> 
                                ConstantFSharpUnionCase v
                            | _ ->
                                NormalFSharpUnionCase (
                                    case.Fields
                                    |> Seq.map (fun f ->
                                        {
                                            Name = f.Name
                                            UnionFieldType = sr.ReadType clsTparams.Value f.FieldType
                                            DateTimeFormat = 
                                                cAnnot.DateTimeFormat 
                                                |> List.tryPick (fun (target, format) -> if target = Some f.Name then Some format else None)
                                        }
                                    )
                                    |> List.ofSeq
                                )
                        let staticIs =
                            not usesNull || not (
                                case.Attributes
                                |> CodeReader.hasCompilationRepresentation CompilationRepresentationFlags.Instance
                            )
                        {
                            Name = case.Name
                            JsonName = cAnnot.Name
                            Kind = kind
                            StaticIs = staticIs
                        }
                    )
                    |> List.ofSeq
                
                FSharpUnionInfo {
                    Cases = cases
                    NamedUnionCases = tAnnot.NamedUnionCases
                    HasNull = usesNull && cases |> List.exists (fun c -> c.Kind = ConstantFSharpUnionCase Null) 
                }
            else
                CustomTypeInfo.NotCustomType 

        cls
        |> Option.map branchOnType
        |> Option.defaultValue CustomTypeInfo.NotCustomType

    let tryReflectCustomType typeDef =
        try
            reflectCustomType typeDef
        with
        | _ -> CustomTypeInfo.NotCustomType

    comp.CustomTypesReflector <- tryReflectCustomType
    comp.LookupTypeAttributes <- lookupTypeAttributes
    comp.LookupFieldAttributes <- lookupFieldAttributes 
    comp.LookupMethodAttributes <- lookupMethodAttributes
    comp.LookupConstructorAttributes <- lookupConstructorAttributes

    let argCurrying = ArgCurrying.ResolveFuncArgs(comp)

    checkResults.AssemblyContents.ImplementationFiles
    |> List.sortByDescending (fun f -> List.isEmpty f.Declarations)
    |> List.distinctBy (fun f -> f.FileName)
    |> Seq.iter (fun file ->
        if List.isEmpty file.Declarations then () else
        let filePath =
            match file.Declarations.Head with
            | FSIFD.Entity (a, _) -> a.DeclarationLocation.FileName
            | FSIFD.MemberOrFunctionOrValue (_, _, c) -> c.Range.FileName
            | FSIFD.InitAction a -> a.Range.FileName
        let sc =
            lazy
            let name = "$StartupCode_" + System.IO.Path.GetFileNameWithoutExtension filePath
            let def =
                TypeDefinition {
                    Assembly = comp.FindProxiedAssembly(assemblyName)
                    FullName = name
                }
            def, 
            (ResizeArray(), ResizeArray(), Dictionary() : StartupCode)

        let rootTypeAnnot = rootTypeAnnot |> annotForTypeOrFile (System.IO.Path.GetFileName filePath)
        let topLevelTypes = ResizeArray<SourceMemberOrEntity>()
        let types = Dictionary<FSharpEntity, ResizeArray<SourceMemberOrEntity>>()
        let recMembers = Dictionary<FSMFV, Id * FSharpExpr>()
        let rec getTypesWithMembers (parentMembers: ResizeArray<_>) d =
            match d with
            | FSIFD.Entity (a, b) ->
                if not a.IsFSharpAbbreviation then
                    if a.IsInterface then
                        parentMembers.Add (SourceInterface a)
                    elif isILClass a then 
                        let ms = ResizeArray()
                        parentMembers.Add (SourceEntity (a, ms))
                        try
                            types.Add (a, ms)
                        with _ ->
                            comp.AddError(Some (CodeReader.getRange a.DeclarationLocation), SourceError "Duplicate type definition")
                        b |> List.iter (getTypesWithMembers ms)
                    else
                        b |> List.iter (getTypesWithMembers parentMembers)
            | FSIFD.MemberOrFunctionOrValue (a, b, c) -> 
                let m = SourceMember(a, b, c)
                match a.DeclaringEntity with
                | Some e ->
                    types[e].Add(m)
                | _ ->                            
                    let i = Id.New(a.LogicalName)
                    recMembers.Add(a, (i, c))
            | FSIFD.InitAction a ->
                parentMembers.Add (InitAction a)

        file.Declarations |> Seq.iter (getTypesWithMembers topLevelTypes)

        let classAnnotations = Dictionary()
        
        for t in topLevelTypes do
            match t with
            | SourceEntity (c, m) ->
                collectTypeAnnotations classAnnotations typeImplLookup sr rootTypeAnnot c m
            | SourceInterface i ->
                collectTypeAnnotations classAnnotations typeImplLookup sr rootTypeAnnot i Seq.empty
            | _ -> ()

        // register all proxies for signature redirection
        for (def, annot) in classAnnotations.Values do
            match annot.ProxyOf with
            | Some p -> comp.AddProxy(def, p, annot.IsProxyInteral)
            | _ -> ()

        for t in topLevelTypes do
            match t with
            | SourceMember _ -> failwith "impossible: top level member"
            | InitAction _ -> failwith "impossible: top level init action"
            | SourceEntity (c, m) ->
                transformClass sc comp argCurrying sr classAnnotations isInterface recMembers c m |> Option.iter comp.AddClass
            | SourceInterface i ->
                transformInterface sr rootTypeAnnot i |> Option.iter comp.AddInterface
                transformClass sc comp argCurrying sr classAnnotations isInterface recMembers i [||] |> Option.iter comp.AddClass
            
        let getStartupCodeClass (def: TypeDefinition, sc: StartupCode) =

            let recDecls, statements, fields = sc       
            let cctor = Block (List.ofSeq (Seq.append recDecls statements))
            let members =
                [
                    for KeyValue(f, t) in fields -> 
                        NotResolvedMember.Field(f, 
                            {
                                StrongName = None
                                IsStatic = true
                                IsOptional = false
                                IsReadonly = true
                                FieldType = t
                                Order = 0
                            } 
                        )
                    yield NotResolvedMember.StaticConstructor cctor
                ]
            
            def,
            {
                StrongName = None
                Generics = []
                BaseClass = None
                Implements = []
                Requires = [] //annot.Requires
                Members = members
                Kind = NotResolvedClassKind.Static
                IsProxy = false
                Macros = []
                ForceNoPrototype = false
                ForceAddress = false
                Type = None
                SourcePos =
                    {
                        FileName = filePath
                        Start = 0, 0
                        End = 0, 0
                    }
            }
            
        if sc.IsValueCreated then
            getStartupCodeClass sc.Value |> comp.AddClass
    )
    
    logger.TimedStage "Parsing with FCS"

    argCurrying.ResolveAll()

    logger.TimedStage "Analyzing function arguments"

    comp.Resolve()

    logger.TimedStage "Resolving names"

    comp
