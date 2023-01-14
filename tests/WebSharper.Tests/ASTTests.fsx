// $begin{copyright}
//
// This file is part of WebSharper
//
// Copyright (c) 2008-2018 IntelliFactory
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

#r "nuget: FSharp.Compiler.Service"
#r "../../build/Release/FSharp/net6.0/Mono.Cecil.dll"
#r "../../build/Release/FSharp/net6.0/Mono.Cecil.Mdb.dll"
#r "../../build/Release/FSharp/net6.0/Mono.Cecil.Pdb.dll"
#r "../../build/Release/FSharp/netstandard2.0/WebSharper.Compiler.FSharp.dll"
#r "../../build/Release/netstandard2.0/WebSharper.Compiler.dll"
#r "../../build/Release/netstandard2.0/WebSharper.Core.JavaScript.dll"
#r "../../build/Release/netstandard2.0/WebSharper.Core.dll"
#r "../../build/Release/netstandard2.0/WebSharper.JavaScript.dll"
#r "../../build/Release/netstandard2.0/WebSharper.Main.dll"
#r "../../build/Release/netstandard2.0/WebSharper.MathJS.dll"
#r "../../build/Release/netstandard2.0/WebSharper.MathJS.Extensions.dll"
#r "../../build/Release/netstandard2.0/WebSharper.Collections.dll"
#r "../../build/Release/netstandard2.0/WebSharper.Control.dll"
#r "../../build/Release/netstandard2.0/WebSharper.Web.dll"
#r "../../build/Release/netstandard2.0/WebSharper.Sitelets.dll"

fsi.ShowDeclarationValues <- false

open System
open System.IO
open System.Collections.Generic
open FSharp.Compiler
open FSharp.Compiler.Symbols
open FSharp.Compiler.CodeAnalysis

module P = FSharpExprPatterns

// Create an interactive checker instance 
let checker = FSharpChecker.Create(keepAssemblyContents=true)

[<AutoOpen>]
module Utils = 
    let rec printExpr low (e:FSharpExpr) = 
        match e with 
        | P.AddressOf(e1) -> "&"+printExpr 0 e1
        | P.AddressSet(e1,e2) -> printExpr 0 e1 + " <- " + printExpr 0 e2
        | P.Application(f,tyargs,args) -> quote low (printExpr 10 f + printTyargs tyargs + " " + printCurriedArgs args)
        | P.BaseValue(_) -> "base"
        | P.Call(Some obj,v,tyargs1,tyargs2,argsL) -> printObjOpt (Some obj) + v.CompiledName  + printTyargs tyargs2 + printTupledArgs argsL
        | P.Call(None,v,tyargs1,tyargs2,argsL) -> v.DeclaringEntity.Value.CompiledName + printTyargs tyargs1 + "." + v.CompiledName  + printTyargs tyargs2 + " " + printTupledArgs argsL
        | P.Coerce(ty1,e1) -> quote low (printExpr 10 e1 + " :> " + printTy ty1)
        | P.DefaultValue(ty1) -> "dflt"
        | P.FastIntegerForLoop _ -> "for-loop"
        | P.ILAsm(s,tyargs,args) -> s + printTupledArgs args 
        | P.ILFieldGet _ -> "ILFieldGet"
        | P.ILFieldSet _ -> "ILFieldSet"
        | P.IfThenElse (a,b,c) -> "(if " + printExpr 0 a + " then " + printExpr 0 b + " else " + printExpr 0 c + ")"
        | P.Lambda(v,e1) -> "fun " + v.CompiledName + " -> " + printExpr 0 e1
        | P.Let((v,e1,_),b) -> "let " + (if v.IsMutable then "mutable " else "") + v.CompiledName + ": " + printTy v.FullType + " = " + printExpr 0 e1 + " in " + printExpr 0 b
        | P.LetRec(vse,b) -> "let rec ... in " + printExpr 0 b
        | P.NewArray(ty,es) -> "[|" + (es |> Seq.map (printExpr 0) |> String.concat "; ") +  "|]" 
        | P.NewDelegate(ty,es) -> "new-delegate" 
        | P.NewObject(v,tys,args) -> "new " + v.DeclaringEntity.Value.CompiledName + printTyargs tys + printTupledArgs args 
        | P.NewRecord(v,args) -> 
            let fields = v.TypeDefinition.FSharpFields
            "{" + ((fields, args) ||> Seq.map2 (fun f a -> f.Name + " = " + printExpr 0 a) |> String.concat "; ") + "}" 
        | P.NewAnonRecord(v,args) -> 
            "{| ... |}"
            //let fields = v.TypeDefinition.FSharpFields
            //"{|" + ((fields, args) ||> Seq.map2 (fun f a -> f.Name + " = " + printExpr 0 a) |> String.concat "; ") + "|}" 
        | P.NewTuple(v,args) -> printTupledArgs args 
        | P.NewUnionCase(ty,uc,args) -> uc.CompiledName + printTupledArgs args 
        | P.Quote(e1) -> "quote" + printTupledArgs [e1]
        | P.FSharpFieldGet(obj, ty,f) -> printObjOpt obj + "." + f.Name 
        | P.FSharpFieldSet(obj, ty,f,arg) -> printObjOpt obj + f.Name + " <- " + printExpr 0 arg
        | P.AnonRecordGet(obj, ty, index) -> printExpr 10 obj + "." + ty.AnonRecordTypeDetails.SortedFieldNames.[index] 
        | P.Sequential(e1,e2) -> "(" + printExpr 0 e1 + "; " + printExpr 0 e2 + ")"
        | P.ThisValue _ -> "this"
        | P.TryFinally(e1,e2,_,_) -> "try " + printExpr 0 e1 + " finally " + printExpr 0 e2
        | P.TryWith(e1,_,_,vC,eC,_,_) -> "try " + printExpr 0 e1 + " with " + vC.CompiledName + " -> " + printExpr 0 eC
        | P.TupleGet(ty,n,e1) -> printExpr 10 e1 + ".Item" + string n
        | P.DecisionTree(dtree,targets) -> "match " + printExpr 10 dtree + " targets ..."
        | P.DecisionTreeSuccess (tg,es) -> "$" + string tg
        | P.TypeLambda(gp1,e1) -> "FUN ... -> " + printExpr 0 e1 
        | P.TypeTest(ty,e1) -> printExpr 10 e1 + " :? " + printTy ty
        | P.UnionCaseSet(obj,ty,uc,f1,e1) -> printExpr 10 obj + "." + f1.Name + " <- " + printExpr 0 e1
        | P.UnionCaseGet(obj,ty,uc,f1) -> printExpr 10 obj + "." + f1.Name
        | P.UnionCaseTest(obj,ty,f1) -> printExpr 10 obj + ".Is" + f1.Name
        | P.UnionCaseTag(obj,ty) -> printExpr 10 obj + ".Tag" 
        | P.ObjectExpr(ty,basecall,overrides,iimpls) -> "{ " + printExpr 10 basecall + " with " + printOverrides overrides + " " + printIimpls iimpls + " }"
        | P.TraitCall(tys,nm,_,argtys,tinst,args) -> "trait call " + nm + printTupledArgs args
        | P.Const(obj,ty) -> 
            match obj with 
            | :? string  as s -> "\"" + s + "\""
            | null -> "()"
            | _ -> string obj
        | P.Value(v) -> v.CompiledName
        | P.ValueSet(v,e1) -> quote low (v.CompiledName + " <- " + printExpr 0 e1)
        | P.WhileLoop(e1,e2,_) -> "while " + printExpr 0 e1 + " do " + printExpr 0 e2 + " done"
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
                        sprintf "member %s%s = %s @ %s" v.CompiledName (printCurriedParams vs)  (printExpr 0 e) (e.Range.ToString())
                    else 
                        sprintf "let %s%s = %s @ %s" v.CompiledName (printCurriedParams vs) (printExpr 0 e) (e.Range.ToString())
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
        | P.AddressOf(e) -> collectMembers e
        | P.AddressSet(e1,e2) -> Seq.append (collectMembers e1) (collectMembers e2)
        | P.Application(f,_,args) -> Seq.append (collectMembers f) (Seq.collect collectMembers args)
        | P.BaseValue(_) -> Seq.empty
        | P.Call(Some obj,v,_,_,argsL) -> Seq.concat [ collectMembers obj; Seq.singleton v; Seq.collect collectMembers argsL ]
        | P.Call(None,v,_,_,argsL) -> Seq.concat [ Seq.singleton v; Seq.collect collectMembers argsL ]
        | P.Coerce(_,e) -> collectMembers e
        | P.DefaultValue(_) -> Seq.empty
        | P.FastIntegerForLoop (fromArg, toArg, body, _, _, _) -> Seq.collect collectMembers [ fromArg; toArg; body ]
        | P.ILAsm(_,_,args) -> Seq.collect collectMembers args 
        | P.ILFieldGet (Some e,_,_) -> collectMembers e
        | P.ILFieldGet _ -> Seq.empty
        | P.ILFieldSet (Some e,_,_,v) -> Seq.append (collectMembers e) (collectMembers v)
        | P.ILFieldSet _ -> Seq.empty
        | P.IfThenElse (a,b,c) -> Seq.collect collectMembers [ a; b; c ]
        | P.Lambda(v,e1) -> collectMembers e1
        | P.Let((v,e1,_),b) -> Seq.append (collectMembers e1) (collectMembers b)
        | P.LetRec(vse,b) -> Seq.append (vse |> Seq.collect (fun (_,e,_) -> collectMembers e)) (collectMembers b)
        | P.NewArray(_,es) -> Seq.collect collectMembers es
        | P.NewDelegate(ty,es) -> collectMembers es
        | P.NewObject(v,tys,args) -> Seq.append (Seq.singleton v) (Seq.collect collectMembers args)
        | P.NewRecord(v,args) -> Seq.collect collectMembers args
        | P.NewTuple(v,args) -> Seq.collect collectMembers args
        | P.NewUnionCase(ty,uc,args) -> Seq.collect collectMembers args
        | P.Quote(e1) -> collectMembers e1
        | P.FSharpFieldGet(Some obj, _,_) -> collectMembers obj
        | P.FSharpFieldGet _ -> Seq.empty
        | P.FSharpFieldSet(Some obj,_,_,arg) -> Seq.append (collectMembers obj) (collectMembers arg)
        | P.FSharpFieldSet(None,_,_,arg) -> collectMembers arg
        | P.Sequential(e1,e2) -> Seq.append (collectMembers e1) (collectMembers e2)
        | P.ThisValue _ -> Seq.empty
        | P.TryFinally(e1,e2,_,_) -> Seq.append (collectMembers e1) (collectMembers e2)
        | P.TryWith(e1,_,f,_,eC,_,_) -> Seq.collect collectMembers [ e1; f; eC ]
        | P.TupleGet(ty,n,e1) -> collectMembers e1
        | P.DecisionTree(dtree,targets) -> Seq.append (collectMembers dtree) (targets |> Seq.collect (snd >> collectMembers))
        | P.DecisionTreeSuccess (tg,es) -> Seq.collect collectMembers es
        | P.TypeLambda(gp1,e1) -> collectMembers e1
        | P.TypeTest(ty,e1) -> collectMembers e1
        | P.UnionCaseSet(obj,ty,uc,f1,e1) -> Seq.append (collectMembers obj) (collectMembers e1)
        | P.UnionCaseGet(obj,ty,uc,f1) -> collectMembers obj
        | P.UnionCaseTest(obj,ty,f1) -> collectMembers obj
        | P.UnionCaseTag(obj,ty) -> collectMembers obj
        | P.ObjectExpr(ty,basecall,overrides,iimpls) -> 
            seq {
                yield! collectMembers basecall
                for o in overrides do
                    yield! collectMembers o.Body
                for _, i in iimpls do
                    for o in i do
                        yield! collectMembers o.Body
            }
        | P.TraitCall(tys,nm,_,argtys,tinst,args) -> Seq.collect collectMembers args
        | P.Const(obj,ty) -> Seq.empty
        | P.Value(v) -> Seq.singleton v
        | P.ValueSet(v,e1) -> Seq.append (Seq.singleton v) (collectMembers e1)
        | P.WhileLoop(e1,e2,_) -> Seq.append (collectMembers e1) (collectMembers e2) 
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
        Path.Combine(__SOURCE_DIRECTORY__, @"..\..\build\Release\netstandard2.0", x + ".dll")
    List.map wsLib [
        "WebSharper.Core.JavaScript"
        "WebSharper.Core"
        "WebSharper.JavaScript"
        "WebSharper.Main"
        "WebSharper.Collections"
        "WebSharper.Control"
        "WebSharper.Web"
        "WebSharper.MathJS"
        "WebSharper.MathJS.Extensions"
        "WebSharper.Testing"
        //"WebSharper.Sitelets"
        //"WebSharper.Tests"
        //"WebSharper.InterfaceGenerator.Tests"
    ]

let mkProjectCommandLineArgs (dllName, fileNames) = 
    let sysLib x =
        Path.Combine(System.Runtime.InteropServices.RuntimeEnvironment.GetRuntimeDirectory(), x + ".dll") 
    [|  yield "--simpleresolution" 
//        yield "--noframework" 
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
//        let references =
//            [ 
//                sysLib "mscorlib"
//                sysLib "System"
//                sysLib "System.Core"
//                Path.Combine(Environment.GetFolderPath(Environment.SpecialFolder.ProgramFilesX86),
//                    @"Reference Assemblies\Microsoft\FSharp\.NETFramework\v4.0\4.4.0.0\FSharp.Core.dll")
//            ]
//        for r in references do
//            yield "-r:" + r
        for r in wsRefs do
            yield "-r:" + r
     |]

let metadata =
    let metas =
        wsRefs |> Seq.choose(
            WebSharper.Compiler.FrontEnd.ReadFromFile WebSharper.Core.Metadata.FullMetadata
        )
    { 
        WebSharper.Core.Metadata.Info.UnionWithoutDependencies metas with
            Dependencies = WebSharper.Core.DependencyGraph.Graph.NewWithDependencyAssemblies(metas |> Seq.map (fun m -> m.Dependencies)).GetData()
    }

metadata.ResourceHashes |> Seq.iter (fun (KeyValue(k, v)) -> printfn "%sk?h=%d" k v)

let stExpr s = WebSharper.Core.AST.StatementExpr(s, None)

open System.IO

let translate source = 

    let tempFileName = Path.GetTempFileName()
    let fileName = Path.ChangeExtension(tempFileName, ".fs")
    let base2 = Path.GetTempFileName()
    let dllName = Path.ChangeExtension(base2, ".dll")
    let projFileName = Path.ChangeExtension(base2, ".fsproj")
    File.WriteAllText(fileName, source)

    let args = mkProjectCommandLineArgs (dllName, [fileName])
    let options =  checker.GetProjectOptionsFromCommandLineArgs (projFileName, args)

    let wholeProjectResults = checker.ParseAndCheckProject(options) |> Async.RunSynchronously
    if wholeProjectResults.HasCriticalErrors then
        for err in wholeProjectResults.Diagnostics |> Seq.filter (fun e -> e.Severity = Diagnostics.FSharpDiagnosticSeverity.Error) do
            printfn "F# Error: %d:%d-%d:%d %s" err.StartLine err.StartColumn err.EndLine err.EndColumn err.Message
    else
    for err in wholeProjectResults.Diagnostics do
        printfn "F# Error/Warning: %d:%d-%d:%d %s" err.StartLine err.StartColumn err.EndLine err.EndColumn err.Message
    let file1 = wholeProjectResults.AssemblyContents.ImplementationFiles.[0]

    let fsDeclarations = 
         file1.Declarations |> Utils.printDeclarations None |> List.ofSeq

    let logger = WebSharper.Compiler.ConsoleLogger()

    let comp = 
        WebSharper.Compiler.FSharp.ProjectReader.transformAssembly
            logger
            (WebSharper.Compiler.Compilation(metadata, false, UseLocalMacros = false))
            "TestProject"
            WebSharper.Compiler.CommandTools.WsConfig.Empty
            wholeProjectResults

    let expressions =
        Seq.concat [
            comp.CompilingMethods |> Seq.map (fun (KeyValue(_,(_,_,a))) -> a)
            comp.CompilingConstructors |> Seq.map (fun (KeyValue(_,(_,a))) -> a)
            comp.CompilingImplementations |> Seq.map (fun (KeyValue(_,(_,a))) -> a)
            comp.CompilingStaticConstructors |> Seq.map (fun (KeyValue(_,a)) -> stExpr a)
        ]
        |> List.ofSeq

    fsDeclarations |> List.iter (printfn "%s") 
    expressions |> List.iter (WebSharper.Core.AST.Debug.PrintExpression >> printfn "compiling: %s")

    try
        WebSharper.Compiler.Translator.DotNetToJavaScript.CompileFull comp
    with e ->
        printfn "Compile error: %A" e

    if not (List.isEmpty comp.Errors) then
        for pos, err in comp.Errors do
            printfn "WebSharper Error: %A %O" pos err 
    else

    let currentMeta = comp.ToCurrentMetadata()
    let compiledExpressions = 
        currentMeta.Classes.Values |> Seq.collect (
            function
            | _, _, Some c ->
                Seq.concat [
                    c.Methods.Values |> Seq.map (fun a -> a.Expression)
                    c.Constructors.Values |> Seq.map (fun a -> a.Expression)
                    c.Implementations.Values |> Seq.map (fun a -> a.Expression)
                    c.StaticConstructor |> Option.map stExpr |> Option.toList |> Seq.ofList
                ]
            | _ -> Seq.empty
        )
        |> List.ofSeq 
        
    let errors =
        [
            for pos, e in comp.Errors -> pos, string e, true
            for pos, e in comp.Warnings -> pos, string e, false
        ]
    errors |> List.iter (printfn "%A")

    let pkg = WebSharper.Compiler.JavaScriptPackager.packageAssembly WebSharper.Core.JavaScript.TypeScript metadata currentMeta "TestProject" None WebSharper.Compiler.JavaScriptPackager.OnLoadIfExists
    
    for (file, p) in pkg do
        printfn "packaged %s: %s" file (WebSharper.Core.AST.Debug.PrintStatement (WebSharper.Core.AST.Block p))

    let jsFiles = 
        pkg 
        |> Array.map (fun (file, p) ->
            let js, map = WebSharper.Compiler.JavaScriptPackager.programToString WebSharper.Core.JavaScript.TypeScript WebSharper.Core.JavaScript.Readable WebSharper.Core.JavaScript.Writer.CodeWriter p
            file, js
        )

    //compiledExpressions |> List.iter (WebSharper.Core.AST.Debug.PrintExpression >> printfn "compiled: %s")
    for (name, js) in jsFiles do 
        printfn "File: %s" name
        printfn "%s" js

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
    match metadata.Classes.[typ] with
    | _, _, Some cls ->
        match AST.Reflection.ReadMember mi |> Option.get with
        | AST.Member.Method (_, meth) 
        | AST.Member.Override (_, meth) ->
            cls.Methods.[meth].Expression
        | AST.Member.Implementation (intf, meth) ->
            cls.Implementations.[intf, meth].Expression
        | AST.Member.Constructor ctor ->
            cls.Constructors.[ctor].Expression
        | AST.Member.StaticConstructor ->
            cls.StaticConstructor |> Option.get |> stExpr
    | _ -> failwithf "class data not found: %A" typ

translate """
namespace WebSharper.Tests

open WebSharper
open WebSharper.JavaScript
open WebSharper.Testing
open System

[<JavaScript>]
module Test =
    let add1 (x) = x + 1

"""



//translate """
//module M

//open WebSharper

//[<JavaScript>]
//let anonymousRecord() = 
//    let r = {| A = 42 |}
//    r.A
//"""

//translate """
//module M

//open WebSharper

//[<JavaScript>]
//let stringInterpolation() = 
//    //sprintf "x=%d %d" 5 6
//    //$"x={(5, 5)}" 
//    $"x=%d{5}" 
//"""

//translate """
//module M

//open WebSharper

//[<Inline " { var sc = import('./pkg/scenariolib.js'); console.log(sc); return sc; } ">]
//let testImport () = ()

//[<JavaScript>]
//let useImport() = 
//    testImport ()
//"""


//translate """
//module M

//open WebSharper

//[<Inline>]
//let tailRecSingleInline n =
//    let rec f n =
//        if n > 0 then f (n - 1) else 0
//    f n
//"""

//translate """
//module M

//open WebSharper

//module Bug923 =
//    type V2<[<Measure>] 'u> =
//        struct
//            val x : float<'u>
//            val y : float<'u>
//            new (x, y) = {x=x; y=y}
//        end

//        static member (+) (a : V2<_>, b : V2<_>) = 
//            V2 (a.x + b.x, a.y + b.y)

//    [<JavaScript>]
//    let addFloatsWithMeasures (a: float<'a>) (b: float<'a>) = a + b

//    """

//getBody <@ JS.Document.Cookie <- X<_> @>
//|> WebSharper.Core.AST.Debug.PrintExpression

//let me = WebSharper.Compiler.Recognize.GetMutableExternals metadata