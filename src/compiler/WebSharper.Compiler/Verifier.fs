// $begin{copyright}
//
// This file is part of WebSharper
//
// Copyright (c) 2008-2018 IntelliFactory
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

// Checks RPC signatures for encoder support
module WebSharper.Compiler.Verifier

open WebSharper.Core.AST

open System.Collections.Generic
module J = WebSharper.Core.Json

let private (|AsyncOrTask|_|) (t: Type) =
    match t with
    | ConcreteType { Generics = []; Entity = td } when td = Definitions.Task ->
        Some None
    | ConcreteType { Generics = [ x ]; Entity = td } when td = Definitions.Async || td = Definitions.Task1 ->
        Some (Some x)
    | _ ->
        None

type private VerifyEncoderResult =
    | CantLoad
    | CantGet
    | Ok

type Status =
    | Correct
    | Incorrect of string
    | CriticallyIncorrect of string

[<Sealed>]
type State(tryGetAnonRecordFields: TypeDefinition -> string list option, jP: J.Provider) =

    let combineVerifyResults results =
        if results |> List.exists ((=) CantLoad) then CantLoad
        elif results |> List.exists ((=) CantGet) then CantGet
        else Ok

    let canEncodeToJson =
        let cache = Dictionary<Type, VerifyEncoderResult>()
        let rec check typ =
            match cache.TryGetValue typ with
            | true, value -> value
            | _ ->
                cache.[typ] <- Ok
                let value =
                    try
                        let runtimeType = Reflection.LoadType typ
                        try
                            jP.GetEncoder(runtimeType) |> ignore
                            Ok
                        with _ ->
                            CantGet
                    with _ ->
                        match typ with
                        | ConcreteType { Entity = td; Generics = args } when Option.isSome (tryGetAnonRecordFields td) ->
                            args |> List.map check |> combineVerifyResults
                        | _ ->
                            CantLoad
                cache.[typ] <- value
                value
        check

    let canDecodeFromJson =
        let cache = Dictionary<Type, VerifyEncoderResult>()
        let rec check typ =
            match cache.TryGetValue typ with
            | true, value -> value
            | _ ->
                cache.[typ] <- Ok
                let value =
                    try
                        let runtimeType = Reflection.LoadType typ
                        try
                            jP.GetDecoder(runtimeType) |> ignore
                            Ok
                        with _ ->
                            CantGet
                    with _ ->
                        match typ with
                        | ConcreteType { Entity = td; Generics = args } when Option.isSome (tryGetAnonRecordFields td) ->
                            args |> List.map check |> combineVerifyResults
                        | _ ->
                            CantLoad
                cache.[typ] <- value
                value
        check
   
    let getRemoteContractError (td: TypeDefinition) (m: Method) : Status =
        let m = m.Value
        let isGenericTD = 
            try (Reflection.LoadTypeDefinition td).IsGenericTypeDefinition
            with _ -> false
        if isGenericTD then
            CriticallyIncorrect "Static remote methods must be defined on non-generic types"
        elif m.Generics > 0 then
            CriticallyIncorrect "Remote methods must not be generic"
        else
            let ps =
                m.Parameters
                |> List.map (fun p -> p, canDecodeFromJson p)
            // We check parameter types for decoders first, then result for encoder,
            // then report first type load error if one is encountered.
            let p = ps |> Seq.tryFind (snd >> (=) CantGet) 
            match p with
            | Some (p, _) -> CriticallyIncorrect (sprintf "Cannot decode argument type '%s' from JSON." p.AssemblyQualifiedName)
            | None ->
                let checkForLoadErrors() =
                    let p = ps |> Seq.tryFind (snd >> (=) CantLoad) 
                    match p with
                    | Some (p, _) -> Incorrect (sprintf "Failed to load argument type '%s' to verify decoding from JSON." p.AssemblyQualifiedName)
                    | None -> Correct
                let rec returnType t =
                    match t with
                    | FSharpFuncType (_, ret) ->
                        returnType ret
                    | _ -> t
                match returnType m.ReturnType with
                | VoidType -> checkForLoadErrors()
                | AsyncOrTask t ->
                    match t with
                    | Some t ->
                        match canEncodeToJson t with
                        | Ok -> checkForLoadErrors()
                        | CantLoad -> Incorrect (sprintf "Failed to load return type '%s' to verify encoding to JSON." t.AssemblyQualifiedName)
                        | CantGet -> CriticallyIncorrect (sprintf "Cannot encode return type '%s' to JSON." t.AssemblyQualifiedName)
                    | _ -> checkForLoadErrors()
                | t ->
                    match canEncodeToJson t with
                    | Ok -> Incorrect "Synchronous RPC is deprecated, consider returning an Async or Task."
                    | CantLoad -> Incorrect (sprintf "Failed to load return type '%s' to verify encoding to JSON. Also, synchronous RPC is deprecated, consider returning an Async or Task" t.AssemblyQualifiedName)
                    | CantGet -> CriticallyIncorrect (sprintf "Cannot encode return type '%s' to JSON. Also, synchronous RPC is deprecated, consider returning an Async or Task" t.AssemblyQualifiedName)

    let getWebControlError (t: TypeDefinition) : Status =
        match canEncodeToJson (ConcreteType (NonGeneric t)) with
        | Ok -> Correct
        | CantGet -> CriticallyIncorrect ("Cannot encode the Web.Control type to JSON: " + t.Value.AssemblyQualifiedName)
        | CantLoad -> Incorrect (sprintf "Failed to load return type '%s' to verify encoding to JSON." (string t))

    member this.VerifyRemoteMethod(t: TypeDefinition, m: Method) =
        getRemoteContractError t m

    member this.VerifyWebControl(t: TypeDefinition) =
        getWebControlError t
