module WebSharper.Compiler.Common.ToJavaScript
 
open WebSharper.Core
open WebSharper.Core.AST

let fail _ = failwith "Transform error: .NET common to Core"

module M = WebSharper.Core.Metadata

type ToJavaScript(meta) =
    inherit Transformer()

    let mutable innerVars = None //[] : list<Id>

    let mutable currentNode = None

    let macros = System.Collections.Generic.Dictionary<TypeDefinition, Macro>()
    let generators = System.Collections.Generic.Dictionary<TypeDefinition, Generator>()

//    abstract GetInlineArgs : obj ->     
//    abstract CompileCall : obj * M.Node -> unit
//    abstract CompileCtor : obj * Address * M.Node -> unit

    member this.CompileNode (node: M.Node, info) =
        let c = currentNode
        currentNode <- Some node
        node.Body <- this.TransformExpression(node.Body)
        node.Info <- info
        currentNode <- c

    member this.CompileCall (node: M.Node, thisObj, typ, meth, args) =
        match node.Info with
        | M.Instance name ->
            Application(ItemGet(this.TransformExpression (Option.get thisObj), Value (String name)), args |> List.map this.TransformExpression) 
        | M.Static address ->
            currentNode.Value.Dependencies.Add address |> ignore
            Application(GlobalAccess address, args |> List.map this.TransformExpression)
        | M.Inline ->
//            let name = typ.Entity.Value.FullName + "." + meth.Entity.Value.MethodName
            Substitution((Option.toList thisObj @ args) |> List.map this.TransformExpression).TransformExpression(node.Body)
        // TODO : return dependencies/requires
        | M.Macro (macro, parameter, fallback) ->
//            fallback |> Option.iter (fun f -> this.Compile ({node with NodeInfo = f}, thisObj, args))
            let m =
                let mt = System.Type.GetType(macro.Value.AssemblyQualifiedName)
                try 
                    match mt.GetConstructor([|typeof<WebSharper.Core.Metadata.Assembly>|]) with
                    | null -> mt.GetConstructor([||]).Invoke([||])
                    | mctor -> mctor.Invoke([|meta|])
                    :?> WebSharper.Core.Macro
                with _ -> failwithf "Macro load error %s" macro.Value.AssemblyQualifiedName
            m.TranslateCall(thisObj, typ, meth, args, parameter) |> this.TransformExpression
        | M.NotCompiled info ->
            this.CompileNode(node, info)
            this.CompileCall(node, thisObj, typ, meth, args)
        | M.NotGenerated _ ->
            failwith "TODO: generators"
    
    override this.TransformCall (thisObj, typ, meth, args) =
        let node = M.lookupClassMethod meta typ.Entity meth.Entity
        this.CompileCall(node, thisObj, typ, meth, args)

    member this.CompileCtor(node: M.Node, typ, ctor, args) =
        match node.Info with
        | M.Static address ->
            New(GlobalAccess address, args |> List.map this.TransformExpression)
        | M.Inline -> 
            Substitution(args).TransformExpression(node.Body)
        | M.Macro (macro, parameter, fallback) ->
//            fallback |> Option.iter (fun f -> this.Compile ({node with NodeInfo = f}, thisObj, args))
            let m =
                let mt = System.Type.GetType(macro.Value.AssemblyQualifiedName)
                try 
                    match mt.GetConstructor([|typeof<WebSharper.Core.Metadata.Assembly>|]) with
                    | null -> mt.GetConstructor([||]).Invoke([||])
                    | mctor -> mctor.Invoke([|meta|])
                    :?> WebSharper.Core.Macro
                with _ -> failwithf "Macro load error %s" macro.Value.AssemblyQualifiedName
            m.TranslateCtor(typ, ctor, args, parameter) |> this.TransformExpression
        | M.NotCompiled info ->
            this.CompileNode(node, info)
            this.CompileCtor(node, typ, ctor, args)
        | M.NotGenerated _ ->
            failwith "TODO: generators"
        | _ -> failwith "invalid metadata for constructor"

    override this.TransformCtor(typ, ctor, args) =
        let node = M.lookupConstructor meta typ.Entity ctor
        this.CompileCtor(node, typ, ctor, args)

    member this.CompileCCtor (node: M.Node, typ) =
        match node.Info with
        | M.Static address ->
            Application(GlobalAccess address, [])
        | M.NotCompiled info ->
            this.CompileNode(node, info)
            this.CompileCCtor(node, typ)
        | _ -> failwith "invalid metadata for static constructor"

    override this.TransformCCtor typ =
        let node = M.lookupStaticConstructor meta typ.Entity
        this.CompileCCtor(node, typ)

    override this.TransformFieldGet (expr, typ, field) =
        match M.lookupField meta typ.Entity field with
        | M.InstanceField fname ->
            ItemGet(this.TransformExpression expr, Value (String fname)) 
        | M.StaticField faddr ->
            GlobalAccess faddr   
        | M.OptionalInstanceField fname -> failwith "TODO"
        | M.OptionalStaticField faddr -> failwith "TODO"

    override this.TransformFieldSet (expr, typ, field, value) =
        match M.lookupField meta typ.Entity field with
        | M.InstanceField fname ->
            ItemSet(this.TransformExpression expr, Value (String fname), this.TransformExpression value) 
        | M.StaticField faddr ->
            let f :: a = faddr.Value
            ItemSet(GlobalAccess (Hashed a), Value (String f), this.TransformExpression value)
        | M.OptionalInstanceField fname -> failwith "TODO"
        | M.OptionalStaticField faddr -> failwith "TODO"

    override this.TransformLet(var, value, body) =
        innerVars <- Some (var :: innerVars.Value) 
        Sequential [ VarSet(var, this.TransformExpression value); this.TransformExpression body ]

    override this.TransformNewVar(var, value) =
        innerVars <- Some (var :: innerVars.Value)
        VarSet(var, this.TransformExpression value)

    override this.TransformTypeCheck(expr, typ) =
        Application (globalAccess ["checkType"], [Value (String typ.AssemblyQualifiedName); this.TransformExpression expr]) // TODO
//            let typeof x = (!e).TypeOf &== str x
//            match t with
//            | R.Type.Concrete (t, []) ->
//                match t.FullName with
//                | "Microsoft.FSharp.Core.Unit"
//                | "System.Void" ->
//                    typeof "undefined"
//                | "System.Boolean" ->
//                    typeof "boolean"
//                | "System.Byte"
//                | "System.SByte"
//                | "System.Char"
//                | "System.Single"
//                | "System.Double"
//                | "System.Int16"
//                | "System.Int32"
//                | "System.Int64"
//                | "System.UInt16"
//                | "System.UInt32"
//                | "System.UInt64" ->
//                    typeof "number"
//                | "System.String" ->
//                    typeof "string"
//                | "System.IDisposable" ->
//                    (!e).[!~(C.String "Dispose")] &!= !~C.Undefined
//                | _ ->
//                    match meta.DataType t with
//                    | None | Some (M.Object _) | Some (M.Interface _) ->
//                        err "Failed to compile a type test: " t.FullName
//                    | Some (M.Class (fn, _, _))
//                    | Some (M.Record (fn, _))
//                    | Some (M.Exception fn) ->
//                        (!e).InstanceOf(glob fn)
//            | _ ->
//                error "Type tests do not support generic and array types."

    override this.TransformStatement st =
        let defVars =
            if Option.isNone innerVars then
                innerVars <- Some []
                true
            else false
        let res = base.TransformStatement st
        if defVars then
            let res =
                if List.isEmpty innerVars.Value then res
                else 
                    let decl = innerVars.Value |> List.map (fun v -> VarDeclaration (v, Undefined))
                    Block (decl @ [ res ])
            innerVars <- None
            res
        else res
        |> breakStatement

