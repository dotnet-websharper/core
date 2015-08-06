// $begin{copyright}
//
// This file is part of WebSharper
//
// Copyright (c) 2008-2015 IntelliFactory
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

module WebSharper.Compiler.Verifier

open System.Collections.Generic
module J = WebSharper.Core.Json
module R = WebSharper.Core.Reflection

let (|Async|_|) (t: TypeReference) =
    match t.Shape with
    | TypeShape.GenericInstanceType [x]
        when t.FullName.StartsWith "Microsoft.FSharp.Control.FSharpAsync`1" ->
            Some x
    | _ ->
        None

type Status =
    | Correct
    | Incorrect of string
    | CriticallyIncorrect of string


[<Sealed>]
type State(logger: Logger) =

    let log priority loc text =
        logger.Log {
            Priority = priority
            Location = { ReadableLocation = loc; SourceLocation = None }
            Text = text
        }

    let memoize (f: R.Type -> bool) : R.Type -> bool =
        let cache = Dictionary()
        fun x ->
            match cache.TryGetValue(x) with
            | true, x -> x
            | _ ->
                cache.[x] <- true
                let v = f x
                cache.[x] <- v
                v

    let jP = J.Provider.Create()

    let canEncodeToJson =
        Adapter.AdaptType >>
            memoize (fun t ->
                try
                    let t = t.Load()
                    try
                        let enc = jP.GetEncoder(t)
                        true
                    with WebSharper.Core.Json.NoEncoderException _ ->
                        false
                with e ->
                    log Warning (string t) ("Failed to load type: " + string e)
                    true)

    let canDecodeFromJson =
        Adapter.AdaptType >>
            memoize (fun t ->
                try
                    let t = t.Load()
                    try
                        let enc = jP.GetDecoder(t)
                        true
                    with WebSharper.Core.Json.NoDecoderException _ ->
                        false
                with e ->
                    log Warning (string t) ("Failed to load type: " + string e)
                    true)

    let getRemoteContractError (m: MethodDefinition) : Status =
        if m.DeclaringType.HasGenericParameters then
            CriticallyIncorrect "Static remote methods must be defined on non-generic types."
        elif m.HasGenericParameters then
            CriticallyIncorrect "Remote methods must not be generic."
        else
            let p =
                m.Parameters
                |> Seq.tryFind (fun p ->
                    not (canDecodeFromJson p.ParameterType))
            match p with
            | Some p ->
                let msg = "Cannot decode a parameter from JSON: "
                Incorrect (msg + p.Name)
            | None ->
                match m.ReturnType with
                | None -> Correct
                | Some (Async t) ->
                    if canEncodeToJson t
                        then Correct
                        else Incorrect "Cannot encode the return type to JSON."
                | Some t ->
                    if canEncodeToJson t
                        then "Synchronous remote methods are strongly advised against; consider returning an Async<'T>."
                        else "Cannot encode the return type to JSON."
                    |> Incorrect

    let getWebControlError (t: TypeDefinition) : Status =
        if not (canEncodeToJson t) then
            CriticallyIncorrect ("Cannot encode the Web.Control type to JSON: " + t.FullName)
        else
            let body =
                t.Properties
                |> Seq.tryPick (fun x ->
                    let fN = "WebSharper.Web.Control"
                    if x.Name = "Body"
                        && x.GetMethod.IsSome
                        && x.GetMethod.Value.IsVirtual
                    then Some x
                    else None)
            match body with
            | None -> CriticallyIncorrect ("Web.Control type must override the Body property: " + t.FullName)
            | Some p ->
                let fN = typeof<ReflectedDefinitionAttribute>.FullName
                let ok =
                    p.CustomAttributes
                    |> Seq.exists (fun x -> x.AttributeType.FullName = fN)
                if ok then Correct else
                    CriticallyIncorrect ("JavaScript attribute is required on the Body property: " + t.FullName)

    member this.VerifyRemoteMethod(m: MethodDefinition) =
        getRemoteContractError m

    member this.VerifyWebControl(t: TypeDefinition) =
        getWebControlError t

let Create logger =
    State(logger)
