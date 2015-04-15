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

module WebSharper.Core.Remoting

open System
open System.Collections.Generic
open System.Reflection

module A = WebSharper.Core.Attributes
module FI = FastInvoke
module J = WebSharper.Core.Json
module M = WebSharper.Core.Metadata
module R = WebSharper.Core.Reflection

type FST = Reflection.FSharpType
type FSV = Reflection.FSharpValue
type Headers = string -> option<string>

type IAsyncAdapter =
    abstract member Box : obj -> Async<obj>

exception InvalidAsyncException

[<Sealed>]
type AsyncAdapter<'T>() =
    interface IAsyncAdapter with
        member this.Box(x: obj) : Async<obj> =
            match x with
            | :? Async<'T> as a ->
                async {
                    let! x = a
                    return box x
                }
            | _ ->
                raise InvalidAsyncException

let getResultEncoder (jP: J.Provider) (m: MethodInfo) =
    let t = m.ReturnType
    let tD = if t.IsGenericType then t.GetGenericTypeDefinition() else t
    if t.IsGenericType && tD = typedefof<Async<_>> then
        let eT = t.GetGenericArguments().[0]
        let aa =
            typedefof<AsyncAdapter<_>>.MakeGenericType(eT)
            |> Activator.CreateInstance :?> IAsyncAdapter
        let enc = jP.GetEncoder eT
        fun (x: obj) ->
            async {
                let! x = aa.Box x
                return jP.Pack (enc.Encode x)
            }
    elif t = typeof<Void> || t = typeof<unit> then
        let enc = jP.GetEncoder typeof<unit>
        fun (x: obj) ->
            jP.Pack (enc.Encode null)
            |> async.Return
    else
        let enc = jP.GetEncoder t
        fun (x: obj) ->
            jP.Pack (enc.Encode x)
            |> async.Return

type Response =
    {
        Content : string
        ContentType : string
    }

type Request =
    {
        Body : string
        Headers : Headers
    }

let getParameterDecoder (jP: J.Provider) (m: MethodInfo) =
    let par = m.GetParameters()
    match par.Length with
    | 0 -> fun _ -> [||]
    | 1 ->
        let decoder = jP.GetDecoder par.[0].ParameterType
        fun x ->
            match x with
            | J.Array [j] -> [| decoder.Decode j |]
            | _ -> raise J.DecoderException
    | _ ->
        let tt =
            m.GetParameters()
            |> Array.map (fun p -> p.ParameterType)
            |> FST.MakeTupleType
        let tR = FSV.PreComputeTupleReader tt
        let decoder = jP.GetDecoder tt
        fun v -> tR (decoder.Decode v)

exception InvalidHandlerException

type IHandlerFactory =
    abstract member Create : Type -> option<obj>

let locker = obj ()

let mutable factory =
    {
        new IHandlerFactory with
            member this.Create _ = None
    }

let SetHandlerFactory rhf =
    lock locker (fun () -> factory <- rhf)

let toConverter (mk: option<IHandlerFactory>) (jP: J.Provider) (m: MethodInfo) =
    let enc = getResultEncoder jP m
    let dec = getParameterDecoder jP m
    let run = FI.Compile m
    let factory = defaultArg mk factory
    if m.IsStatic then
        fun j ->
            run.InvokeN(dec j) |> enc
    else
        fun j ->
            let t = m.DeclaringType
            match factory.Create t with
            | Some inst ->
                let args = dec j
                let ps = Array.zeroCreate (args.Length + 1)
                for i in 1 .. args.Length do
                    ps.[i] <- args.[i - 1]
                ps.[0] <- inst
                run.InvokeN(ps) |> enc
            | None ->
                raise InvalidHandlerException

[<Literal>]
let HEADER_NAME = "x-websharper-rpc"

let IsRemotingRequest (h: Headers) =
    match h HEADER_NAME with
    | Some _ -> true
    | None ->
        match h "Access-Control-Request-Headers" with
        | Some s when s.Contains HEADER_NAME -> true
        | _ -> false

exception InvalidHeadersException

let handle getConverter req =
    match req.Headers HEADER_NAME with
    | None -> raise InvalidHeadersException
    | Some m ->
        let m = M.MethodHandle.Unpack m
        let args = J.Parse req.Body
        let conv = getConverter m
        let convd = conv args
        async {
            let! x = convd
            let r = J.Stringify x
            return {
                ContentType = "application/json"
                Content = r
            }
        }

exception NoRemoteAttributeException

let makeHandler mk info =
    let jP = J.Provider.CreateTyped info
    let getConverter =
        let getConverter m =
            match info.GetRemoteMethod m with
            | None -> raise NoRemoteAttributeException
            | Some m -> toConverter mk jP (m.Load None)
        let d = Dictionary()
        fun m ->
            let v = lock d (fun () ->
                match d.TryGetValue m with
                | true, x -> Some x
                | _ -> None)
            match v with
            | Some x -> x
            | None ->
                let y = getConverter m
                lock d (fun () -> d.[m] <- y)
                y
    handle getConverter

[<Sealed>]
type Server(handle: Request -> Async<Response>) =
    static member Create mk info = Server (makeHandler mk info)
    member this.HandleRequest(req: Request) = handle req
