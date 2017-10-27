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
open WebSharper.Core.Metadata

module M = WebSharper.Core.Metadata
module S = WebSharper.Core.JavaScript.Syntax

let private any = S.Var(S.Id.New("any"))

let private getNamespaceAndName (td: AST.TypeDefinition) (addr: AST.Address) (c: option<M.ClassInfo>) =
    match addr.Address.Value with
    | t :: ns -> 
        let typid =
            let n = td.Value.FullName
            let last = n.Length - 1
            match n.LastIndexOf('`', last, last - n.LastIndexOf '+') with
            | -1 -> t
            | i ->
                t + "<" + String.concat "," [for j in 0 .. int n.[i+1..] - 1 -> "T" + string j] + ">"
            |> S.Id.New
        let varid = Option.map (fun _ -> S.Id.New(t)) c
        List.rev ns, typid, varid
    | [] -> failwithf "Class with empty address: %s" td.Value.FullName

let rec private groupNamespaces s =
    let g = s |> List.groupBy (fun (ns, _, _) -> List.tryHead ns)
    g
    |> List.collect (function
        | None, xs ->
            // declarations
            xs |> List.collect (fun (_, typid, varid) ->
                [
                    yield S.TypeAlias(typid, any)
                    match varid with
                    | Some varid when
                        g |> List.forall (function
                            // Don't output `var x` if there's also `namespace x`
                            | (Some ns, _) when ns = typid.Name -> false
                            | _ -> true) ->
                        yield S.Vars([varid, None], S.VarDecl)
                    | _ -> ()
                ]
            )
        | Some ns, xs ->
            // nested namespaces
            S.Namespace(S.Id.New(ns),
                xs
                |> List.map (fun (ns, typid, varid) -> ns.Tail, typid, varid)
                |> groupNamespaces
            )
            |> List.singleton
    )

let WriteDts (meta: M.Info) : S.Program =
    [
        for KeyValue (td, (addr, _, c)) in meta.Classes do
            if c |> Option.exists (fun c -> c.Type.IsNone) then
                yield getNamespaceAndName td addr c
        for KeyValue (td, i) in meta.Interfaces do
            if i.Type.IsNone then
                yield getNamespaceAndName td i.Address None
    ]
    |> groupNamespaces
    |> List.map S.Declare
