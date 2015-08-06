module WebSharper.Compiler.Common.Packager

open System.Collections.Generic

module M = WebSharper.Core.Metadata
open WebSharper.Core.AST

//type AddressTransformer(tr) =
//    inherit Transformer()
//
//    override this.TransformGlobalAccess a =

let packageAssembly (assembly: M.Assembly) =
    let addresses = Dictionary()
    let statements = ResizeArray()

    let glob = Id "Global"
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
                let var = Id name
                let f = Value (String name)
                statements.Add <| VarDeclaration (var, ItemSet(glob, f, ItemGet(glob, f) |> safeObject))                
                let res = Var var
                //topLevel.Add(name, res)
                addresses.Add(address, res)
                res
            | name :: r ->
                let parent = getAddress (Hashed r)
                let f = Value (String name)
                let var = Id name
                statements.Add <| VarDeclaration (var, ItemSet(parent, f, ItemGet(parent, f) |> safeObject))                
                let res = Var var
                addresses.Add(address, res)
                res

    let getFieldAddress (address: Address) =
        match address.Value with
        | name :: r ->
            getAddress (Hashed r), Value (String name)
        | _ -> failwith "packageAssembly: empty address"

    let getField address =
        let o, x = getFieldAddress address  
        ItemGet(o, x)

    let transformAddresses =
        { new Transformer() with
            override this.TransformGlobalAccess a =
                if addresses.ContainsKey a then getAddress a else getField a        
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
        statements.Add <| ExprStatement (ItemSet (o, x, expr))    

    let packageNode a (n: M.Node) =
        if packaged.Add a then
            match n.Info with
            | M.Static _ -> package a n.Body
            | M.Instance _ ->
                match a.Value with
                | _ :: p ->
                    let p = Hashed p
                    if packaged.Add p then
                        let proto = Object []
                        package p proto
                    package a n.Body
                    
//    let rec package a t =
//        if packaged.Add a then
//            match t with 
//            | M.Content (c, _, _) ->
//                let o, x = getFieldAddress a
//                statements.Add <| ExprStatement (ItemSet (o, x, getCompiled c))    
//            | M.Prototype (t, p, _, _) ->
//                let o, x = getFieldAddress a 
//                let proto = 
//                    Object (
//                        p |> Seq.map (fun (KeyValue (k, v)) ->
//                            k, getCompiled v
//                        ) |> List.ofSeq
//                    )
//                let proto =
//                    match assembly.Classes.[t].BaseClass with
//                    | Some b ->
//                        let bInfo = assembly.Classes.[b]
//                        match bInfo.Address, bInfo.Prototype with
//                        | Some a, Some pr ->
//                            let paddr = Hashed (pr :: a.Value)
//                            package paddr assembly.Translated.[paddr]
//                            Application(builtin ["Object"; "create" ], [ getField paddr; proto ])
//                        | _ -> proto
//                    | None -> proto
//                statements.Add <| ExprStatement (ItemSet (o, x, proto))    

//    let runtimeCtor =
//        let ctor = Id "ctor"
//        let proto = Id "proto"
//        Function ([ctor; proto], 
//            let f = Id "f"
//            Block [
//                ExprStatement <| ItemSet(Var ctor, Value (String "prototype"), Var proto)
//                Return (Var ctor)
//            ]
//        )
//        |> M.CompiledExpr
//    package (Hashed ["Ctor"; "Runtime"]) (M.Content (ref runtimeCtor, null, null))
//
//    let runtimeCctor =
//        let cctor = Id "cctor"
//        Function([cctor],
//            let init = Id "init"
//            Return (
//                Function ([],
//                    Block [
//                        VarDeclaration (init, Value (Bool true))
//                        If (Var init,
//                            Block [
//                                ExprStatement (VarSet (init, Value (Bool false)))
//                                ExprStatement (Application (Var cctor, []))        
//                            ], Empty)    
//                    ]
//                )
//            )
//        )
//    package (Hashed ["Cctor"; "Runtime"]) runtimeCctor

    for (KeyValue (a, t)) in assembly.Translated do packageNode a t

    Application(Function([], Block (List.ofSeq statements)), [])
    
//    let getCompiled c =
//        match !c with
//        | M.CompiledExpr e ->
//            e
//    let rec package x =
//        match x with
////        | M.Module o ->
////            Object (
////                o |> Seq.map (fun (KeyValue (k, v)) -> 
////                    k, package v
////                ) |> List.ofSeq            
////            ) 
//        | M.Content c -> getCompiled c
//        | M.TypeFunction f ->
//            let x = Common.Id.New()
//            Sequential [
//                VarSet(x, Function([], Empty))
//                PropertySet(Var x, Value (String "prototype"), 
//                    Object (
//                        f |> Seq.map (fun (KeyValue (k, v)) ->
//                            k, getCompiled v
//                        ) |> List.ofSeq
//                    )
//                )
//                Var x
//            ]
//
//    Common.Block (
//        Common.VarDeclaration (Common.Id.New "Global", JavaScript.This) :: (
//            assembly.Translated |> Seq.map (fun (KeyValue (k, v)) ->
//                Common.VarDeclaration (Common.Id.New k, package v)
//            ) |> List.ofSeq  
//        )
//    )
    //packageObject assembly.Translated

let exprToString statement =
    statement
    |> ToJavaScriptSyntax.transformExpr (WebSharper.Compiler.Common.ToJavaScriptSyntax.Environment.New())
    |> WebSharper.Core.JavaScript.Syntax.Ignore
    |> WebSharper.Core.JavaScript.Syntax.Action
    |> List.singleton
    |> WebSharper.Core.JavaScript.Writer.ProgramToString
        WebSharper.Core.JavaScript.Readable
