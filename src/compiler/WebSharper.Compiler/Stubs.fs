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

let GetMethodInline (tAnnot: TypeAnnotation) (mAnnot: MemberAnnotation) isInstance (mdef: Method) =
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
            Application(ItemGet(Hole 0, item, NoSideEffect), args, NonPure, None)
    else
        let getAddressAndName isProp =
            let n = 
                match mAnnot.Name with
                | Some n -> n
                | _ -> mname
            let p = n.Split('.')
            match p with
            | [||] -> 
                error <- Some "Empty string not allowed as name for member"
                None
            | [| n |] -> 
                match tAnnot.Name with
                | Some cn ->
                    Some (cn.Split('.') |> List.ofArray, n)
                | _ ->
                    if isProp then
                        error <- Some "Static stub property with short name requres the type to have a Name attribute"
                        None
                    else Some ([], n)
            | _ ->
                let l = p.Length
                Some (p.[ .. l - 2] |> List.ofArray, p.[l - 1])
        let propAddressAndName() =
            match getAddressAndName true with
            | None ->
                errorPlaceholder, errorPlaceholder
            | Some (a, n) ->
                Global a, Value (String n)
        if isGet then
            let a, n = propAddressAndName()
            ItemGet(a, n, NoSideEffect)
        elif isSet then
            let a, n = propAddressAndName()
            ItemSet(a, n, Hole 0)
        else 
            let a =
                match getAddressAndName false with
                | None -> 
                    errorPlaceholder
                | Some (a, n) -> 
                    Global (a @ [n]) 
            let l = mdef.Value.Parameters.Length
            let args = List.init l Hole
            Application(a, args, NonPure, Some l)
    , error

let GetConstructorInline (tAnnot: TypeAnnotation) (mAnnot: MemberAnnotation) (tdef: TypeDefinition) (cdef: Constructor) =
    let a =
        match mAnnot.Name with
        | Some a ->  a.Split('.') |> List.ofArray
        | _ -> 
            match tAnnot.Name with
            | Some a -> a.Split('.') |> List.ofArray
            | _ ->
                let typeName =
                    let n = tdef.Value.FullName.Split('.', '+') |> Array.last
                    n.Split('`').[0]
                [ typeName ]
    match a with
    | [] -> 
        Object []
    | _ ->
        let l = cdef.Value.CtorParameters.Length
        let args = List.init l Hole
        let f = if a.IsEmpty then errorPlaceholder else Global a
        New(f, args)
