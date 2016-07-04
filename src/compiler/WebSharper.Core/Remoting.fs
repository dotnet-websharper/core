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

module WebSharper.Core.Remoting

open System
open System.Collections.Generic
open System.Reflection
open System.Threading.Tasks

module FI = FastInvoke
module J = WebSharper.Core.Json
module M = WebSharper.Core.Metadata
//module R = WebSharper.Core.Reflection

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

[<Sealed>]
type TaskAdapter<'T>() =
    interface IAsyncAdapter with
        member this.Box(x: obj) : Async<obj> =
            match x with
            | :? Task<'T> as a ->
                async {
                    let! x = Async.AwaitTask a
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
    elif t.IsGenericType && tD = typedefof<Task<_>> then
        let eT = t.GetGenericArguments().[0]
        let aa =
            typedefof<TaskAdapter<_>>.MakeGenericType(eT)
            |> Activator.CreateInstance :?> IAsyncAdapter
        let enc = jP.GetEncoder eT
        fun (x: obj) ->
            async {
                let! x = aa.Box x
                return jP.Pack (enc.Encode x)
            }
    elif tD = typeof<Task> then
        fun (x: obj) ->
            let enc = jP.GetEncoder<unit>()
            async {
                let! x = Async.AwaitIAsyncResult (unbox<Task> x)
                return jP.Pack (enc.Encode ())
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

let handlers = Dictionary<System.Type, obj>()

let AddHandler (t: System.Type) (h: obj) =
    lock handlers (fun () -> handlers.Add(t, h))

let toConverter (jP: J.Provider) (m: MethodInfo) =
    let enc = getResultEncoder jP m
    let dec = getParameterDecoder jP m
    let run = FI.Compile m
    if m.IsStatic then
        fun j ->
            let v = dec j
            run.InvokeN(v) |> enc
    else
        fun j ->
            let t = m.DeclaringType
            match handlers.TryFind t with
            | None -> 
                raise InvalidHandlerException
            | Some inst ->
                let args = dec j
                let ps = Array.zeroCreate (args.Length + 1)
                for i in 1 .. args.Length do
                    ps.[i] <- args.[i - 1]
                ps.[0] <- inst
                run.InvokeN(ps) |> enc

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
exception NoRemoteAttributeException

[<Sealed>]
type Server(info, jP) =
    let remote = M.Utilities.getRemoteMethods info
    let d = Dictionary()
    let getConverter m =
        match remote.TryFind m with
        | None -> raise NoRemoteAttributeException
        | Some (td, m) -> toConverter jP (AST.Reflection.LoadMethod td m)
    let getCachedConverter m =
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

    member this.HandleRequest(req: Request) =
        match req.Headers HEADER_NAME with
        | None -> raise InvalidHeadersException
        | Some m ->
            let m = M.MethodHandle.Unpack m
            let args = J.Parse req.Body
            let conv = getCachedConverter m
            let convd = conv args
            async {
                let! x = convd
                let r = J.Stringify x
                return {
                    ContentType = "application/json"
                    Content = r
                }
            }

    member this.JsonProvider = jP

    static member Create info jP = Server(info, jP)
