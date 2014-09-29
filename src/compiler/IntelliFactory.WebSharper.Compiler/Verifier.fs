// $begin{copyright}
//
// This file is part of WebSharper
//
// Copyright (c) 2008-2013 IntelliFactory
//
// GNU Affero General Public License Usage
// WebSharper is free software: you can redistribute it and/or modify it under
// the terms of the GNU Affero General Public License, version 3, as published
// by the Free Software Foundation.
//
// WebSharper is distributed in the hope that it will be useful, but WITHOUT
// ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
// FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License
// for more details at <http://www.gnu.org/licenses/>.
//
// If you are unsure which license is appropriate for your use, please contact
// IntelliFactory at http://intellifactory.com/contact.
//
// $end{copyright}

module IntelliFactory.WebSharper.Compiler.Verifier

open System.Collections.Generic
module J = IntelliFactory.WebSharper.Core.Json
module R = IntelliFactory.WebSharper.Core.Reflection

let (|Async|_|) (t: TypeReference) =
    match t.Shape with
    | TypeShape.GenericInstanceType [x]
        when t.FullName = "Microsoft.FSharp.Control.FSharpAsync`1" ->
            Some x
    | _ ->
        None

type Status =
    | Correct
    | Incorrect of string


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
                    with IntelliFactory.WebSharper.Core.Json.NoEncoderException _ ->
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
                    with IntelliFactory.WebSharper.Core.Json.NoDecoderException _ ->
                        false
                with e ->
                    log Warning (string t) ("Failed to load type: " + string e)
                    true)

    let getRemoteContractError (m: MethodDefinition) : option<string> =
        if m.DeclaringType.HasGenericParameters then
            Some "Static remote methods must be defined on non-generic types."
        elif m.HasGenericParameters then
            Some "Remote methods must not be generic."
        else
            let p =
                m.Parameters
                |> Seq.tryFind (fun p ->
                    not (canDecodeFromJson p.ParameterType))
            match p with
            | Some p ->
                let msg = "Cannot decode a parameter from JSON: "
                Some (msg + p.Name)
            | None ->
                match m.ReturnType with
                | None -> None
                | Some (Async t) | Some t ->
                    if canEncodeToJson t
                        then None
                        else Some "Cannot encode the return type to JSON."

    let getWebControlError (t: TypeDefinition) =
        if not (canEncodeToJson t) then
            Some "Cannot encode the Web.Control type to JSON."
        else
            let body =
                t.Properties
                |> Seq.tryPick (fun x ->
                    let fN = "IntelliFactory.WebSharper.Web.Control"
                    if x.Name = "Body"
                        && x.GetMethod.IsSome
                        && x.GetMethod.Value.IsVirtual
                    then Some x
                    else None)
            match body with
            | None -> Some "Web.Control types must override the Body property."
            | Some p ->
                let fN = typeof<ReflectedDefinitionAttribute>.FullName
                let ok =
                    p.CustomAttributes
                    |> Seq.exists (fun x -> x.AttributeType.FullName = fN)
                if ok then None else
                    Some "JavaScript attribute is required on the Body property."

    member this.VerifyRemoteMethod(m: MethodDefinition) =
        match getRemoteContractError m with
        | None -> Correct
        | Some msg -> Incorrect msg

    member this.VerifyWebControl(t: TypeDefinition) =
        match getWebControlError t with
        | None -> Correct
        | Some msg -> Incorrect msg

let Create logger =
    State(logger)
