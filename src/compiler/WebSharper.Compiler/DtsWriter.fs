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

let private getNamespaceAndName (td: AST.TypeDefinition) (addr: AST.Address) =
    match addr.Address.Value with
    | t :: ns -> 
        let id = S.Id.New(t)
        List.rev ns, S.TypeAlias(id, any)
    | [] -> failwithf "Class with empty address: %s" td.Value.FullName

let rec private groupNamespaces (s: seq<list<string> * S.Statement>) =
    s
    |> Seq.groupBy (fst >> List.tryHead)
    |> Seq.collect (function
        | None, xs -> Seq.map snd xs    // declarations
        | Some ns, xs ->                // nested namespaces
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
            if c.IsSome then
                yield getNamespaceAndName td addr
        for KeyValue (td, i) in meta.Interfaces do
            yield getNamespaceAndName td i.Address
    }
    |> groupNamespaces
    |> List.map S.Declare
