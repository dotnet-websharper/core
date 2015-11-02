module WebSharper.Compiler.Packager

open System.Collections.Generic

open WebSharper.Core
open WebSharper.Core.AST
module M = WebSharper.Core.Metadata

let packageAssembly (merged: M.Metadata) (current: M.Metadata) =
    let addresses = Dictionary()
    let statements = ResizeArray()

    let glob = Id.New "Global"
    statements.Add <| VarDeclaration (glob, This)
    let glob = Var glob

    let safeObject expr = Binary(expr, BinaryOperator.``||``, Object []) 
//
    let builtin address =
        address |> List.fold (fun e n -> ItemGet(e, Value (String n))) glob
    
    let rec getAddress (address: Address) =
        match addresses.TryGetValue address with
        | true, v -> v
        | _ ->
            match address.Value with
            | [] -> glob
            | [ name ] ->
                let var = Id.New name
                let f = Value (String name)
                statements.Add <| VarDeclaration (var, ItemSet(glob, f, ItemGet(glob, f) |> safeObject))                
                let res = Var var
                //topLevel.Add(name, res)
                addresses.Add(address, res)
                res
            | name :: r ->
                let parent = getAddress (Hashed r)
                let f = Value (String name)
                let var = Id.New name
                statements.Add <| VarDeclaration (var, ItemSet(parent, f, ItemGet(parent, f) |> safeObject))                
                let res = Var var
                addresses.Add(address, res)
                res

    let getFieldAddress (address: Address) =
        match address.Value with
        | name :: r ->
            getAddress (Hashed r), Value (String name)
        | _ -> failwith "packageAssembly: empty address"

//    let getField address =
//        let o, x = getFieldAddress address  
//        ItemGet(o, x)

    let transformAddresses =
        { new Transformer() with
            override this.TransformGlobalAccess a =
                if addresses.ContainsKey a then getAddress a else GlobalAccess a        
        }.TransformExpression
            
//    let getCompiled c =
//        match !c with
//        | M.Static e ->
//            e |> transformAddresses
//        | _ -> failwith "packageAssembly: not compiled"
    
    let packaged = HashSet()

    let package a expr =
//        if packaged.Add a then
        let o, x = getFieldAddress a
        statements.Add <| ExprStatement (ItemSet (o, x, transformAddresses expr))    

    let packageCtor a expr =
//        if packaged.Add a then
        let o, x = getFieldAddress a
        // TODO: this is a hack
        let av = 
            match getAddress a with
            | Var v -> v
            | _ -> failwith "packageCtor error"
        statements.Add <| ExprStatement (VarSet (av, ItemSet (o, x, transformAddresses expr)))    

    let packageGlobal a expr =
//        if packaged.Add a then
        let o, x = getFieldAddress a
        statements.Add <| ExprStatement (ItemSet (o, x, expr))    
//    for c in current.Classes.Values do

    let classes = Dictionary(current.Classes)

    let rec packageClass (c: M.ClassInfo) =

        match c.BaseClass with
        | Some b ->
            match classes.TryFind b with
            | Some bc ->
                classes.Remove b |> ignore
                packageClass bc
            | _ -> ()
        | _ -> ()

        match c.StaticConstructor with
        | Some (ccaddr, body) -> packageGlobal ccaddr <| Application (runtimeCctor, [ body ])
        | _ -> ()

        match c.Address with 
        | None -> ()
        | Some addr ->
            
            let prototype = 
                Object [
                    for info, body in Seq.append c.Methods.Values c.Implementations.Values do
                        match info with
                        | M.Instance mname ->
                            if body <> Undefined then
                                yield mname, body
                        | _ -> ()
                ]
                            
            let baseType =
                match c.BaseClass |> Option.bind (M.tryLookupClass merged) |> Option.bind (fun b -> b.Address) with
                | Some ba -> GlobalAccess ba
                | _ -> Value Null
             
//            match prototype, c.BaseClass with
//            | Object [], None -> ()
//            | _ ->
//                packageCtor addr <| Application (runtimeClass, [ prototype; baseType; GlobalAccess addr])
            if not c.IsModule then
                packageCtor addr <| Application (runtimeClass, [ prototype; baseType; GlobalAccess addr])

        for info, body in c.Methods.Values do
            match info with
            | M.Static maddr ->
                if body <> Undefined then
                    if body <> Undefined then
                        package maddr body
            | _ -> ()

        for info, body in c.Constructors.Values do
            match info with
            | M.Constructor caddr ->
                if body <> Undefined then
                    if Option.isSome c.Address then
                        package caddr <| 
                            match c.Address with
                            | Some addr -> Application (runtimeCtor, [ body; GlobalAccess addr ])
                            | _ -> body
                    else
                        package caddr body
            | M.Static maddr ->
                if body <> Undefined then
                    package maddr body
            | _ -> ()
            
    
    while classes.Count > 0 do
        let (KeyValue(t, c)) = classes |> Seq.head
        classes.Remove t |> ignore
        packageClass c

    let statements = List.ofSeq statements 
    if List.isEmpty statements then Undefined else
        Application(Function([], Block (List.ofSeq statements)), [])

let exprToString asmName pref statement =
    let program =
        statement
        |> ToJavaScriptSyntax.transformExpr (WebSharper.Compiler.ToJavaScriptSyntax.Environment.New(asmName, pref))
        |> WebSharper.Core.JavaScript.Syntax.Ignore
        |> WebSharper.Core.JavaScript.Syntax.Action
        |> fun x -> [ x ]
    let w = WebSharper.Core.JavaScript.Writer.CodeWriter()
    WebSharper.Core.JavaScript.Writer.WriteProgram pref w program
    w.GetCodeFile(), w.GetMapFile()
