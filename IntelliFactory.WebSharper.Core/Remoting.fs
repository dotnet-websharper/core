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

module IntelliFactory.WebSharper.Core.Remoting

open System
open System.Collections.Generic
open System.Reflection

module A = IntelliFactory.WebSharper.Core.Attributes
module FI = IntelliFactory.FastInvoke
module J = IntelliFactory.WebSharper.Core.Json
module M = IntelliFactory.WebSharper.Core.Metadata
module R = IntelliFactory.WebSharper.Core.Reflection

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
    | _      -> false

exception InvalidHeadersException

let handle getConverter req =
    match req.Headers HEADER_NAME with
    | None -> raise InvalidHeadersException
    | Some m ->
        let m = M.MethodHandle.Unpack m
        let args = J.Parse req.Body
        let conv = getConverter m
        async {
            let! x = conv args
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
