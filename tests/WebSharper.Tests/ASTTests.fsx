#I __SOURCE_DIRECTORY__
#r "../../packages/fcs/FSharp.Compiler.Service/lib/net45/FSharp.Compiler.Service.dll"
#r "../../packages/Mono.Cecil/lib/net40/Mono.Cecil.dll"
#r "../../packages/Mono.Cecil/lib/net40/Mono.Cecil.Mdb.dll"
#r "../../packages/Mono.Cecil/lib/net40/Mono.Cecil.Pdb.dll"
#r "System.Configuration.dll"
#r "System.Core.dll"
#r "System.Data.dll"
#r "System.Data.Linq.dll"
#r "System.dll"
#r "System.Numerics.dll"
#r "System.Web.dll"
#r "System.Xml.dll"
#r "System.Xml.Linq.dll"
#r "../../build/Release/WebSharper.Core.JavaScript.dll"
#r "../../build/Release/WebSharper.Core.dll"
#r "../../build/Release/WebSharper.JavaScript.dll"
#r "../../build/Release/WebSharper.JQuery.dll"
#r "../../build/Release/WebSharper.Main.dll"
//#r "../../build/Release/WebSharper.Collections.dll"
//#r "../../build/Release/WebSharper.Control.dll"
//#r "../../build/Release/WebSharper.Web.dll"
#r "../../build/Release/FSharp/WebSharper.Compiler.dll"
#r "../../build/Release/FSharp/WebSharper.Compiler.FSharp.dll"

fsi.ShowDeclarationValues = false

open System
open System.IO
open System.Collections.Generic
open Microsoft.FSharp.Compiler
open Microsoft.FSharp.Compiler.SourceCodeServices

// Create an interactive checker instance 
let checker = FSharpChecker.Create(keepAssemblyContents=true)

[<AutoOpen>]
module Utils = 
    let rec printExpr low (e:FSharpExpr) = 
        match e with 
        | BasicPatterns.AddressOf(e1) -> "&"+printExpr 0 e1
        | BasicPatterns.AddressSet(e1,e2) -> printExpr 0 e1 + " <- " + printExpr 0 e2
        | BasicPatterns.Application(f,tyargs,args) -> quote low (printExpr 10 f + printTyargs tyargs + " " + printCurriedArgs args)
        | BasicPatterns.BaseValue(_) -> "base"
        | BasicPatterns.Call(Some obj,v,tyargs1,tyargs2,argsL) -> printObjOpt (Some obj) + v.CompiledName  + printTyargs tyargs2 + printTupledArgs argsL
        | BasicPatterns.Call(None,v,tyargs1,tyargs2,argsL) -> v.EnclosingEntity.Value.CompiledName + printTyargs tyargs1 + "." + v.CompiledName  + printTyargs tyargs2 + " " + printTupledArgs argsL
        | BasicPatterns.Coerce(ty1,e1) -> quote low (printExpr 10 e1 + " :> " + printTy ty1)
        | BasicPatterns.DefaultValue(ty1) -> "dflt"
        | BasicPatterns.FastIntegerForLoop _ -> "for-loop"
        | BasicPatterns.ILAsm(s,tyargs,args) -> s + printTupledArgs args 
        | BasicPatterns.ILFieldGet _ -> "ILFieldGet"
        | BasicPatterns.ILFieldSet _ -> "ILFieldSet"
        | BasicPatterns.IfThenElse (a,b,c) -> "(if " + printExpr 0 a + " then " + printExpr 0 b + " else " + printExpr 0 c + ")"
        | BasicPatterns.Lambda(v,e1) -> "fun " + v.CompiledName + " -> " + printExpr 0 e1
        | BasicPatterns.Let((v,e1),b) -> "let " + (if v.IsMutable then "mutable " else "") + v.CompiledName + ": " + printTy v.FullType + " = " + printExpr 0 e1 + " in " + printExpr 0 b
        | BasicPatterns.LetRec(vse,b) -> "let rec ... in " + printExpr 0 b
        | BasicPatterns.NewArray(ty,es) -> "[|" + (es |> Seq.map (printExpr 0) |> String.concat "; ") +  "|]" 
        | BasicPatterns.NewDelegate(ty,es) -> "new-delegate" 
        | BasicPatterns.NewObject(v,tys,args) -> "new " + v.EnclosingEntity.Value.CompiledName + printTupledArgs args 
        | BasicPatterns.NewRecord(v,args) -> 
            let fields = v.TypeDefinition.FSharpFields
            "{" + ((fields, args) ||> Seq.map2 (fun f a -> f.Name + " = " + printExpr 0 a) |> String.concat "; ") + "}" 
        | BasicPatterns.NewTuple(v,args) -> printTupledArgs args 
        | BasicPatterns.NewUnionCase(ty,uc,args) -> uc.CompiledName + printTupledArgs args 
        | BasicPatterns.Quote(e1) -> "quote" + printTupledArgs [e1]
        | BasicPatterns.FSharpFieldGet(obj, ty,f) -> printObjOpt obj + f.Name 
        | BasicPatterns.FSharpFieldSet(obj, ty,f,arg) -> printObjOpt obj + f.Name + " <- " + printExpr 0 arg
        | BasicPatterns.Sequential(e1,e2) -> "(" + printExpr 0 e1 + "; " + printExpr 0 e2 + ")"
        | BasicPatterns.ThisValue _ -> "this"
        | BasicPatterns.TryFinally(e1,e2) -> "try " + printExpr 0 e1 + " finally " + printExpr 0 e2
        | BasicPatterns.TryWith(e1,_,_,vC,eC) -> "try " + printExpr 0 e1 + " with " + vC.CompiledName + " -> " + printExpr 0 eC
        | BasicPatterns.TupleGet(ty,n,e1) -> printExpr 10 e1 + ".Item" + string n
        | BasicPatterns.DecisionTree(dtree,targets) -> "match " + printExpr 10 dtree + " targets ..."
        | BasicPatterns.DecisionTreeSuccess (tg,es) -> "$" + string tg
        | BasicPatterns.TypeLambda(gp1,e1) -> "FUN ... -> " + printExpr 0 e1 
        | BasicPatterns.TypeTest(ty,e1) -> printExpr 10 e1 + " :? " + printTy ty
        | BasicPatterns.UnionCaseSet(obj,ty,uc,f1,e1) -> printExpr 10 obj + "." + f1.Name + " <- " + printExpr 0 e1
        | BasicPatterns.UnionCaseGet(obj,ty,uc,f1) -> printExpr 10 obj + "." + f1.Name
        | BasicPatterns.UnionCaseTest(obj,ty,f1) -> printExpr 10 obj + ".Is" + f1.Name
        | BasicPatterns.UnionCaseTag(obj,ty) -> printExpr 10 obj + ".Tag" 
        | BasicPatterns.ObjectExpr(ty,basecall,overrides,iimpls) -> "{ " + printExpr 10 basecall + " with " + printOverrides overrides + " " + printIimpls iimpls + " }"
        | BasicPatterns.TraitCall(tys,nm,_,argtys,tinst,args) -> "trait call " + nm + printTupledArgs args
        | BasicPatterns.Const(obj,ty) -> 
            match obj with 
            | :? string  as s -> "\"" + s + "\""
            | null -> "()"
            | _ -> string obj
        | BasicPatterns.Value(v) -> v.CompiledName
        | BasicPatterns.ValueSet(v,e1) -> quote low (v.CompiledName + " <- " + printExpr 0 e1)
        | BasicPatterns.WhileLoop(e1,e2) -> "while " + printExpr 0 e1 + " do " + printExpr 0 e2 + " done"
        | _ -> failwith (sprintf "unrecognized %+A" e)

    and quote low s = if low > 0 then "(" + s + ")" else s
    and printObjOpt e = match e with None -> "" | Some e -> printExpr 10 e + "."
    and printTupledArgs args = "(" + String.concat "," (List.map (printExpr 0) args) + ")"
    and printCurriedArgs args = String.concat " " (List.map (printExpr 10) args)
    and printParams (vs: FSharpMemberOrFunctionOrValue list) = "(" + String.concat "," (vs |> List.map (fun v -> v.CompiledName)) + ")"
    and printCurriedParams (vs: FSharpMemberOrFunctionOrValue list list) = String.concat " " (List.map printParams vs)
    and printTy ty = ty.Format(FSharpDisplayContext.Empty)
    and printTyargs tyargs = match tyargs with [] -> "" | args -> "<" + String.concat "," (List.map printTy tyargs) + ">"
    and printOverrides ors = String.concat ";" (List.map printOverride ors)
    and printOverride o = 
        match o.CurriedParameterGroups with
        | [t] :: a ->
            "member " + t.CompiledName + "." + o.Signature.Name + printCurriedParams a + " = " + printExpr 10 o.Body
        | _ -> failwith "wrong this argument in object expression override"
    and printIimpls iis = String.concat ";" (List.map printImlementation iis)
    and printImlementation (i, ors) = "interface " + printTy i + " with " + printOverrides ors

    let rec printDeclaration (excludes:HashSet<_> option) (d: FSharpImplementationFileDeclaration) = 
        seq {
           match d with 
            | FSharpImplementationFileDeclaration.Entity(e,ds) ->
                yield sprintf "type %s" e.LogicalName
                yield! printDeclarations excludes ds
            | FSharpImplementationFileDeclaration.MemberOrFunctionOrValue(v,vs,e) ->
            
               if not v.IsCompilerGenerated && 
                  not (match excludes with None -> false | Some t -> t.Contains v.CompiledName) then
                let text = 
                    //printfn "%s" v.CompiledName
//                 try
                    if v.IsMember then 
                        sprintf "member %s%s = %s @ %s" v.CompiledName (printCurriedParams vs)  (printExpr 0 e) (e.Range.ToShortString())
                    else 
                        sprintf "let %s%s = %s @ %s" v.CompiledName (printCurriedParams vs) (printExpr 0 e) (e.Range.ToShortString())
//                 with e -> 
//                     printfn "FAILURE STACK: %A" e
//                     sprintf "!!!!!!!!!! FAILED on %s @ %s, message: %s" v.CompiledName (v.DeclarationLocation.ToString()) e.Message
                yield text
            | FSharpImplementationFileDeclaration.InitAction(e) ->
                yield sprintf "do %s" (printExpr 0 e) }
    and printDeclarations excludes ds = 
        seq { for d in ds do 
                yield! printDeclaration excludes d }

    let rec exprsOfDecl (d: FSharpImplementationFileDeclaration) = 
        seq {
           match d with 
            | FSharpImplementationFileDeclaration.Entity(e,ds) ->
                yield! exprsOfDecls ds
            | FSharpImplementationFileDeclaration.MemberOrFunctionOrValue(v,vs,e) ->
               if not v.IsCompilerGenerated then
                  yield e, e.Range
            | FSharpImplementationFileDeclaration.InitAction(e) ->
                yield e, e.Range }
    and exprsOfDecls ds = 
        seq { for d in ds do 
                yield! exprsOfDecl d }

    let printGenericConstraint name (p: FSharpGenericParameterConstraint) =
        if p.IsCoercesToConstraint then
            Some <| name + " :> " + printTy p.CoercesToTarget 
        elif p.IsComparisonConstraint then 
            Some <| name + " : comparison"
        elif p.IsEqualityConstraint then
            Some <| name + " : equality"
        elif p.IsReferenceTypeConstraint then
            Some <| name + " : class"
        elif p.IsNonNullableValueTypeConstraint then
            Some <| name + " : struct"
        elif p.IsEnumConstraint then
            Some <| name + " : enum"
        elif p.IsSupportsNullConstraint then
            Some <| name + " : null"
        else None

    let printGenericParameter (p: FSharpGenericParameter) =
        let name = 
            if p.Name.StartsWith "?" then "_"
            elif p.IsSolveAtCompileTime then "^" + p.Name 
            else "'" + p.Name
        let constraints =
            p.Constraints |> Seq.choose (printGenericConstraint name) |> List.ofSeq
        name, constraints
    
    let printMemberSignature (v: FSharpMemberOrFunctionOrValue) =
        let genParams =
            let ps = v.GenericParameters |> Seq.map printGenericParameter |> List.ofSeq
            if List.isEmpty ps then "" else
                let constraints = ps |> List.collect snd
                "<" + (ps |> Seq.map fst |> String.concat ", ") + 
                    (if List.isEmpty constraints then "" else " when " + String.concat " and " constraints) + ">"

        v.CompiledName + genParams + ": " + printTy v.FullType

    let rec collectMembers (e:FSharpExpr) = 
        match e with 
        | BasicPatterns.AddressOf(e) -> collectMembers e
        | BasicPatterns.AddressSet(e1,e2) -> Seq.append (collectMembers e1) (collectMembers e2)
        | BasicPatterns.Application(f,_,args) -> Seq.append (collectMembers f) (Seq.collect collectMembers args)
        | BasicPatterns.BaseValue(_) -> Seq.empty
        | BasicPatterns.Call(Some obj,v,_,_,argsL) -> Seq.concat [ collectMembers obj; Seq.singleton v; Seq.collect collectMembers argsL ]
        | BasicPatterns.Call(None,v,_,_,argsL) -> Seq.concat [ Seq.singleton v; Seq.collect collectMembers argsL ]
        | BasicPatterns.Coerce(_,e) -> collectMembers e
        | BasicPatterns.DefaultValue(_) -> Seq.empty
        | BasicPatterns.FastIntegerForLoop (fromArg, toArg, body, _) -> Seq.collect collectMembers [ fromArg; toArg; body ]
        | BasicPatterns.ILAsm(_,_,args) -> Seq.collect collectMembers args 
        | BasicPatterns.ILFieldGet (Some e,_,_) -> collectMembers e
        | BasicPatterns.ILFieldGet _ -> Seq.empty
        | BasicPatterns.ILFieldSet (Some e,_,_,v) -> Seq.append (collectMembers e) (collectMembers v)
        | BasicPatterns.ILFieldSet _ -> Seq.empty
        | BasicPatterns.IfThenElse (a,b,c) -> Seq.collect collectMembers [ a; b; c ]
        | BasicPatterns.Lambda(v,e1) -> collectMembers e1
        | BasicPatterns.Let((v,e1),b) -> Seq.append (collectMembers e1) (collectMembers b)
        | BasicPatterns.LetRec(vse,b) -> Seq.append (vse |> Seq.collect (snd >> collectMembers)) (collectMembers b)
        | BasicPatterns.NewArray(_,es) -> Seq.collect collectMembers es
        | BasicPatterns.NewDelegate(ty,es) -> collectMembers es
        | BasicPatterns.NewObject(v,tys,args) -> Seq.append (Seq.singleton v) (Seq.collect collectMembers args)
        | BasicPatterns.NewRecord(v,args) -> Seq.collect collectMembers args
        | BasicPatterns.NewTuple(v,args) -> Seq.collect collectMembers args
        | BasicPatterns.NewUnionCase(ty,uc,args) -> Seq.collect collectMembers args
        | BasicPatterns.Quote(e1) -> collectMembers e1
        | BasicPatterns.FSharpFieldGet(Some obj, _,_) -> collectMembers obj
        | BasicPatterns.FSharpFieldGet _ -> Seq.empty
        | BasicPatterns.FSharpFieldSet(Some obj,_,_,arg) -> Seq.append (collectMembers obj) (collectMembers arg)
        | BasicPatterns.FSharpFieldSet(None,_,_,arg) -> collectMembers arg
        | BasicPatterns.Sequential(e1,e2) -> Seq.append (collectMembers e1) (collectMembers e2)
        | BasicPatterns.ThisValue _ -> Seq.empty
        | BasicPatterns.TryFinally(e1,e2) -> Seq.append (collectMembers e1) (collectMembers e2)
        | BasicPatterns.TryWith(e1,_,f,_,eC) -> Seq.collect collectMembers [ e1; f; eC ]
        | BasicPatterns.TupleGet(ty,n,e1) -> collectMembers e1
        | BasicPatterns.DecisionTree(dtree,targets) -> Seq.append (collectMembers dtree) (targets |> Seq.collect (snd >> collectMembers))
        | BasicPatterns.DecisionTreeSuccess (tg,es) -> Seq.collect collectMembers es
        | BasicPatterns.TypeLambda(gp1,e1) -> collectMembers e1
        | BasicPatterns.TypeTest(ty,e1) -> collectMembers e1
        | BasicPatterns.UnionCaseSet(obj,ty,uc,f1,e1) -> Seq.append (collectMembers obj) (collectMembers e1)
        | BasicPatterns.UnionCaseGet(obj,ty,uc,f1) -> collectMembers obj
        | BasicPatterns.UnionCaseTest(obj,ty,f1) -> collectMembers obj
        | BasicPatterns.UnionCaseTag(obj,ty) -> collectMembers obj
        | BasicPatterns.ObjectExpr(ty,basecall,overrides,iimpls) -> 
            seq {
                yield! collectMembers basecall
                for o in overrides do
                    yield! collectMembers o.Body
                for _, i in iimpls do
                    for o in i do
                        yield! collectMembers o.Body
            }
        | BasicPatterns.TraitCall(tys,nm,_,argtys,tinst,args) -> Seq.collect collectMembers args
        | BasicPatterns.Const(obj,ty) -> Seq.empty
        | BasicPatterns.Value(v) -> Seq.singleton v
        | BasicPatterns.ValueSet(v,e1) -> Seq.append (Seq.singleton v) (collectMembers e1)
        | BasicPatterns.WhileLoop(e1,e2) -> Seq.append (collectMembers e1) (collectMembers e2) 
        | _ -> failwith (sprintf "unrecognized %+A" e)

    let rec printMembersOfDeclatations ds = 
        seq { 
            for d in ds do 
            match d with 
            | FSharpImplementationFileDeclaration.Entity(_,ds) ->
                yield! printMembersOfDeclatations ds
            | FSharpImplementationFileDeclaration.MemberOrFunctionOrValue(v,vs,e) ->
                yield printMemberSignature v
                yield! collectMembers e |> Seq.map printMemberSignature
            | FSharpImplementationFileDeclaration.InitAction(e) ->
                yield! collectMembers e |> Seq.map printMemberSignature
        }

let wsRefs =
    let wsLib x = 
        Path.Combine(__SOURCE_DIRECTORY__, @"..\..\build\Release", x + ".dll")
    List.map wsLib [
        "WebSharper.Core.JavaScript"
        "WebSharper.Core"
        "WebSharper.JavaScript"
        "WebSharper.JQuery"
        "WebSharper.Main"
        //"WebSharper.Collections"
        //"WebSharper.Control"
        //"WebSharper.Web"
        //"WebSharper.Sitelets"
        //"WebSharper.Tests"
        //"WebSharper.InterfaceGenerator.Tests"
    ]

let mkProjectCommandLineArgs (dllName, fileNames) = 
    let sysLib x =
        Path.Combine(System.Runtime.InteropServices.RuntimeEnvironment.GetRuntimeDirectory(), x + ".dll") 
    [|  yield "--simpleresolution" 
        yield "--noframework" 
        yield "--debug:full" 
        yield "--define:DEBUG" 
        yield "--optimize-" 
        yield "--out:" + dllName
        yield "--doc:test.xml" 
        yield "--warn:3" 
        yield "--fullpaths" 
        yield "--flaterrors" 
        yield "--target:library" 
        for x in fileNames do 
            yield x
        let references =
            [ 
                sysLib "mscorlib"
                sysLib "System"
                sysLib "System.Core"
                Path.Combine(Environment.GetFolderPath(Environment.SpecialFolder.ProgramFilesX86),
                    @"Reference Assemblies\Microsoft\FSharp\.NETFramework\v4.0\4.4.0.0\FSharp.Core.dll")
            ]
        for r in references do
            yield "-r:" + r
        for r in wsRefs do
            yield "-r:" + r
     |]

let metadata =
    let metas =
        wsRefs |> Seq.choose(
            WebSharper.Compiler.FrontEnd.ReadFromFile WebSharper.Compiler.FrontEnd.ReadOptions.FullMetadata
        )
    { 
        WebSharper.Core.Metadata.Info.UnionWithoutDependencies metas with
            Dependencies = WebSharper.Core.DependencyGraph.Graph.NewWithDependencyAssemblies(metas |> Seq.map (fun m -> m.Dependencies)).GetData()
    }

metadata.ResourceHashes |> Seq.iter (fun (KeyValue(k, v)) -> printfn "%sk?h=%d" k v)

open System.IO
let translate source = 

    let fileName1 = Path.ChangeExtension(Path.GetTempFileName(), ".fs")
    let base2 = Path.GetTempFileName()
    let dllName = Path.ChangeExtension(base2, ".dll")
    let projFileName = Path.ChangeExtension(base2, ".fsproj")
    let fileSource1 = source
    File.WriteAllText(fileName1, fileSource1)

    let args = mkProjectCommandLineArgs (dllName, [fileName1])
    let options =  checker.GetProjectOptionsFromCommandLineArgs (projFileName, args)

    let wholeProjectResults = checker.ParseAndCheckProject(options) |> Async.RunSynchronously
    let file1 = wholeProjectResults.AssemblyContents.ImplementationFiles.[0]

    let fsDeclarations = 
         file1.Declarations |> Utils.printDeclarations None |> List.ofSeq

    fsDeclarations |> List.iter (printfn "%s") 

    let comp = 
        WebSharper.Compiler.FSharp.ProjectReader.transformAssembly
            (WebSharper.Compiler.Compilation(metadata, false, UseLocalMacros = false))
            "TestProject"
            wholeProjectResults

    let printErrors() =
        [
            for pos, e in comp.Errors -> pos, string e, true
            for pos, e in comp.Warnings -> pos, string e, false
        ]
        |> List.iter (printfn "%A")

    let expressions =
        Seq.concat [
            comp.CompilingMethods |> Seq.map (fun (KeyValue(_,(_,_,a))) -> a)
            comp.CompilingConstructors |> Seq.map (fun (KeyValue(_,(_,a))) -> a)
            comp.CompilingImplementations |> Seq.map (fun (KeyValue(_,(_,a))) -> a)
            comp.CompilingStaticConstructors |> Seq.map (fun (KeyValue(_,(_,a))) -> a)
        ]
        |> List.ofSeq

    expressions |> List.iter (WebSharper.Core.AST.Debug.PrintExpression >> printfn "compiling: %s")

    WebSharper.Compiler.Translator.DotNetToJavaScript.CompileFull comp

    if not (List.isEmpty comp.Errors) then printErrors() else

    let currentMeta = comp.ToCurrentMetadata()
    let compiledExpressions = 
        currentMeta.Classes.Values |> Seq.collect (fun c ->
            Seq.concat [
                c.Methods.Values |> Seq.map (fun (_,_,_,a) -> a)
                c.Constructors.Values |> Seq.map (fun (_,_,a) -> a)
                c.Implementations.Values |> Seq.map snd
                c.StaticConstructor |> Option.map snd |> Option.toList |> Seq.ofList
            ]
        )
        |> List.ofSeq 
        
    compiledExpressions |> List.iter (WebSharper.Core.AST.Debug.PrintExpression >> printfn "compiled: %s")

    printErrors()

    let pkg = WebSharper.Compiler.Packager.packageAssembly metadata currentMeta [] (Some "") false
    
    let js, map = pkg |> WebSharper.Compiler.Packager.programToString WebSharper.Core.JavaScript.Readable WebSharper.Core.JavaScript.Writer.CodeWriter                                       

    compiledExpressions |> List.iter (WebSharper.Core.AST.Debug.PrintExpression >> printfn "compiled: %s")
    js |> printfn "%s" 

translate """
module GenericTest

open WebSharper
open WebSharper.JavaScript

[<JavaScript>]
type Foo<'T> =
    [<Name "Foo">]
    abstract Foo: unit -> 'T

[<JavaScript>]
type Bar<'T, 'U> (u: 'U) =
    interface Foo<'U> with
        member this.Foo() = u

    """
let translateQ q =
    let comp = 
        WebSharper.Compiler.Compilation(metadata, false, UseLocalMacros = false)
    WebSharper.Compiler.QuotationReader.readExpression comp q

let f x y = x + y

translateQ <@ 1 |> ignore @> 
|> WebSharper.Core.AST.Debug.PrintExpression

open WebSharper.JavaScript
open FSharp.Quotations
open WebSharper.Core

let getBody expr =
    let mi =
        match expr with
        | Patterns.Call(_, mi, _) -> mi
        | Patterns.PropertyGet(_, p, _) -> p.GetMethod
        | Patterns.PropertySet(_, p, _, _) -> p.SetMethod
        | _ -> failwithf "not recognized: %A" expr
    let typ = AST.Reflection.ReadTypeDefinition mi.DeclaringType 
    let cls = metadata.Classes.[typ]
    match AST.Reflection.ReadMember mi |> Option.get with
    | AST.Member.Method (_, meth) 
    | AST.Member.Override (_, meth) ->
        let _, _, _, expr = cls.Methods.[meth]
        expr
    | AST.Member.Implementation (intf, meth) ->
        let _, expr = cls.Implementations.[intf, meth]
        expr
    | AST.Member.Constructor ctor ->
        let _, _, expr = cls.Constructors.[ctor]
        expr
    | AST.Member.StaticConstructor ->
        let _, expr = cls.StaticConstructor |> Option.get
        expr

getBody <@ JS.Document.Cookie <- X<_> @>
|> WebSharper.Core.AST.Debug.PrintExpression

let me = WebSharper.Compiler.Recognize.GetMutableExternals metadata