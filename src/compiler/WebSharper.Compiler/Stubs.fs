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
            ItemSet(Hole 0, item, Hole 1)    
        else 
            let l = mdef.Value.Parameters.Length
            let args = List.init l (fun i -> Hole (i + 1))
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
                let args = List.init l Hole
                Appl(GlobalAccess a, args, NonPure, Some l)            
            )
    , error

let GetConstructorInline asmName (tAnnot: TypeAnnotation) (mAnnot: MemberAnnotation) (tdef: TypeDefinition) (cdef: Constructor) =
    let l = cdef.Value.CtorParameters.Length
    let args = List.init l Hole
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
