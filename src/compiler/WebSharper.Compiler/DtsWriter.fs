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
module WebSharper.Compiler.DtsWriter

open WebSharper.Core

module M = WebSharper.Core.Metadata
module S = WebSharper.Core.JavaScript.Syntax

let private any = S.Var(S.Id.New("any"))

let private getNamespaceAndName (td: AST.TypeDefinition) (addr: AST.Address) (c: option<M.ClassInfo>) =
    match addr.Address.Value with
    | t :: ns -> 
        let id = S.Id.New(t)
        List.rev ns, [S.TypeAlias(id, any); S.Vars([id, None], S.VarDecl)]
    | [] -> failwithf "Class with empty address: %s" td.Value.FullName

let rec private groupNamespaces s =
    s
    |> Seq.groupBy (fst >> List.tryHead)
    |> Seq.collect (function
        | None, xs ->
            // declarations
            Seq.collect snd xs
        | Some ns, xs ->
            // nested namespaces
            S.Namespace(S.Id.New(ns),
                xs
                |> Seq.map (fun (ns, s) -> ns.Tail, s)
                |> groupNamespaces
            )
            |> Seq.singleton
    )
    |> List.ofSeq

let WriteDts (meta: M.Info) : S.Program =
    seq {
        for KeyValue (td, (addr, _, c)) in meta.Classes do
            yield getNamespaceAndName td addr c
        for KeyValue (td, i) in meta.Interfaces do
            yield getNamespaceAndName td i.Address None
    }
    |> groupNamespaces
    |> List.map S.Declare
