// $begin{copyright}
//
// This file is part of WebSharper
//
// Copyright (c) 2008-2014 IntelliFactory
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

module internal WebSharper.Sitelets.RouterInferCommon

open System
open System.Collections.Generic
open WebSharper
open WebSharper.Core

module M = WebSharper.Core.Metadata
module P = FSharp.Quotations.Patterns

type Annotation =
    {
        EndPoint : option<option<string> * string>
        Query : option<Set<string>>
        Json : option<option<string>>
        FormData : option<Set<string>>
        IsWildcard : bool
        DateTimeFormat: option<Choice<string, Map<string, string>>>
    }

module Annotation =
    let Empty = 
        {
            EndPoint = None
            Query = None
            Json = None
            FormData = None
            IsWildcard = false
            DateTimeFormat = None
        }

    let Combine a b =
        let comb f a b =
            match a, b with
            | Some a, Some b -> Some (f a b)
            | Some _, _ -> a
            | _, Some _ -> b
            | _ -> None
        let pcomb a b =
            if String.IsNullOrEmpty a || a = "/" then b
            elif String.IsNullOrEmpty b || b = "/" then a
            elif b.StartsWith "/" then a + b else a + "/" + b
        {
            EndPoint = comb (fun (_, ae) (_, be) -> None, pcomb ae be) a.EndPoint b.EndPoint // todo combine methods
            Query = comb Set.union a.Query b.Query
            Json = comb (fun a b -> comb (fun _ _ -> failwith "multiple json fields") a b) a.Json b.Json 
            FormData = comb Set.union a.FormData b.FormData 
            IsWildcard = b.IsWildcard
            DateTimeFormat =
                comb (fun ad bd ->
                    match ad, bd with 
                    | Choice2Of2 a, Choice2Of2 b -> Choice2Of2 (b |> Map.foldBack Map.add a) 
                    | _, b -> b
                ) a.DateTimeFormat b.DateTimeFormat
        }
    
[<AbstractClass>]
type AttributeReader<'A>() =

    abstract GetAssemblyName : 'A -> string
    abstract GetName : 'A -> string
    abstract GetCtorArgOpt : 'A -> string option
    abstract GetCtorParamArgs : 'A -> string[]

    member this.GetAnnotation(attrs: seq<'A>, ?name: string) =
        let ep = ResizeArray()
        let ms = ResizeArray()
        let cn = ref None 
        let wn = ref None 
        let q = ref None
        let fd = ref None
        let mutable j = None
        let mutable w = false
        let mutable dt = None
        let addToSet s attr =
            let set =
                match !s with
                | Some set -> set
                | None ->
                    let set = HashSet()
                    s := Some set
                    set
            this.GetCtorParamArgs(attr) |> Array.iter (set.Add >> ignore)
        for attr in attrs do
            match this.GetAssemblyName attr with
            | "WebSharper.Core" ->
                match this.GetName attr with
                | "EndPointAttribute" ->
                    this.GetCtorArgOpt(attr) |> Option.iter ep.Add
                | "NameAttribute" ->
                    wn := this.GetCtorArgOpt(attr)
                | "MethodAttribute" ->
                    this.GetCtorParamArgs(attr) |> Array.iter ms.Add
                | "QueryAttribute" ->
                    addToSet q attr
                | "JsonAttribute" ->
                    j <- this.GetCtorArgOpt(attr) |> Some
                | "FormDataAttribute" ->
                    addToSet fd attr
                | "WildcardAttribute" ->
                    w <- true
                | "DateTimeFormatAttribute" ->
                    match this.GetCtorParamArgs(attr) with
                    | [| f |] -> dt <- Some (Choice1Of2 f)
                    | [| i; f |] ->
                        dt <- 
                            match dt with
                            | Some (Choice2Of2 m) -> m
                            | _ -> Map.empty
                            |> Map.add i f |> Choice2Of2 |> Some
                    | _ -> ()
                | _ -> ()
            | "FSharp.Core" ->
                match this.GetName attr with
                | "CompiledNameAttribute" ->
                    cn := this.GetCtorArgOpt(attr)
                | _ -> ()
            | _ -> ()
        if ep.Count = 0 then
            match name with
            | Some n ->
                match !wn with
                | Some wn -> ep.Add wn
                | _ ->
                match !cn with 
                | Some cn -> ep.Add cn 
                | _ -> ep.Add n
            | _ -> ()
        let endpoints =
            if ms.Count = 0 then
                ep |> Seq.map (fun e -> 
                    match e.IndexOf(" ") with
                    | -1 -> None, e
                    | i -> Some (e.Substring(0, i)), e.Substring(i + 1)
                ) |> List.ofSeq
            else
                [
                    for e in ep do
                        for m in ms ->
                            Some m, e
                ]
        // todo: multiple endpoint attributes    
        {
            EndPoint = List.tryHead endpoints 
            Query = !q |> Option.map Set.ofSeq
            Json = j
            FormData = !fd |> Option.map Set.ofSeq
            IsWildcard = w
            DateTimeFormat = dt
        }
