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

module WebSharper.Compiler.Stubs

open WebSharper.Core.AST
open AttributeReader

let GetSimpleTypeName (tdef: TypeDefinition) =
    let n = tdef.Value.FullName.Split('.', '+') |> Array.last
    n.Split('`').[0]

let GetMethodInline asmName (tAnnot: TypeAnnotation) (mAnnot: MemberAnnotation) isModuleValue isInstance (tdef: TypeDefinition) (mdef: Method) =
    let mutable error = None
    let mname, isGet, isSet =
        let mname = mdef.Value.MethodName
        if mname.Length < 4 then
            mname, false, false
        else
            match mname.Substring(0, 4) with
            | "get_" -> mname.Substring(4), true, false
            | "set_" -> mname.Substring(4), false, true
            | _ -> mname, false, false
    if isInstance then                                 
        let item =
            match mAnnot.Name with
            | Some n -> n 
            | _ -> mname
            |> String |> Value
        if isGet then
            if List.length mdef.Value.Parameters <> 0 then
                error <- Some "Stub property cannot take arguments"
            ItemGet(Hole 0, item, NoSideEffect)    
        elif isSet then
            if List.length mdef.Value.Parameters <> 1 then
                error <- Some "Stub property cannot take arguments"
            let arg =
                if mdef.Value.Parameters.Head.TypeDefinition = WebSharper.Core.AST.Definitions.FSharpOption then
                    Expression.Conditional(Hole 1, ItemGet(Hole 1, Expression.Value <| Literal.String "$0", Purity.Pure), Expression.Undefined)
                else
                    Hole 1
            ItemSet(Hole 0, item, arg)
        else
            let args =
                List.mapi (fun (i: int) (arg : Type) ->
                    match arg with
                    | Type.ConcreteType ct when ct.Entity = WebSharper.Core.AST.Definitions.FSharpOption ->
                        Expression.Conditional(Hole (i+1), ItemGet(Hole (i+1), Expression.Value <| Literal.String "$0", Purity.Pure), Expression.Undefined)
                    | _ ->
                        Hole (i+1)
                ) mdef.Value.Parameters
            Appl(ItemGet(Hole 0, item, NoSideEffect), args, NonPure, None)
    else
        let useAddress f =
            match mAnnot.Import with
            | Some i ->
                Address.Import asmName i |> f
            | _ ->
                let n = 
                    match mAnnot.Name with
                    | Some n -> n
                    | _ -> mname
                let p = n.Split('.')
                match p with
                | [||] -> 
                    error <- Some "Empty string not allowed as name for member"
                    errorPlaceholder
                | [| n |] -> 
                    match tAnnot.Import with
                    | Some i ->
                        (Address.Import asmName i).Sub(n) |> f
                    | _ ->
                        match tAnnot.Name with
                        | Some cn ->
                            Address.LibAddr ((cn.Split('.') |> List.ofArray) @ [ n ]) |> f
                        | _ ->
                            Address.LibAddr ([ GetSimpleTypeName tdef; n ]) |> f
                | _ ->
                    Address.LibAddr (p |> List.ofArray) |> f
        if isGet || isModuleValue then
            useAddress GlobalAccess            
        elif isSet then
            useAddress (fun a -> GlobalAccessSet(a, Hole 0))   
        else 
            useAddress (fun a ->
                let l = mdef.Value.Parameters.Length
                let args =
                    List.mapi (fun (i: int) (arg : Type) ->
                        match arg with
                        | Type.ConcreteType ct when ct.Entity = WebSharper.Core.AST.Definitions.FSharpOption ->
                            Expression.Conditional(Hole i, ItemGet(Hole i, Expression.Value <| Literal.String "$0", Purity.Pure), Expression.Undefined)
                        | _ ->
                            Hole i
                    ) mdef.Value.Parameters
                Appl(GlobalAccess a, args, NonPure, Some l)            
            )
    , error

let GetConstructorInline asmName (tAnnot: TypeAnnotation) (mAnnot: MemberAnnotation) (tdef: TypeDefinition) (cdef: Constructor) =
    let argTypes = cdef.Value.CtorParameters
    let args =
        List.mapi (fun (i: int) (arg : Type) ->
            match arg with
            | Type.ConcreteType ct when ct.Entity = WebSharper.Core.AST.Definitions.FSharpOption ->
                Expression.Conditional(Hole i, ItemGet(Hole i, Expression.Value <| Literal.String "$0", Purity.Pure), Expression.Undefined)
            | _ ->
                Hole i
        ) argTypes
    match mAnnot.Import |> Option.orElse tAnnot.Import with
    | Some i ->
        let f = GlobalAccess (Address.Import asmName i)
        New(f, [], args)           
    | _ ->
        let a =
            match mAnnot.Name with
            | Some a -> a.Split('.') |> List.ofArray
            | _ -> 
                match tAnnot.Name with
                | Some a -> a.Split('.') |> List.ofArray
                | _ ->
                    [ GetSimpleTypeName tdef ]
        match a with
        | [] -> 
            Object []
        | _ ->
            let f = if a.IsEmpty then errorPlaceholder else Global a
            New(f, [], args)
