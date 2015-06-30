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

module WebSharper.Json

open WebSharper
open WebSharper.JavaScript
open WebSharper.Core.Attributes

[<JavaScript>]
module private Encode =

    let Id x = x

    let Tuple (encs: (obj -> obj)[]) =
        box (fun args ->
            Array.map2 (fun f x -> f x) encs args)

    let List (encEl: 'T -> obj) =
        box (fun (l: list<'T>) ->
            let a : obj[] = [||]
            l |> List.iter (fun x -> a.JS.Push (encEl x) |> ignore)
            a)

    let Array (encEl: 'T -> obj) =
        box (fun (a: 'T[]) -> Array.map encEl a)

    let Set (encEl: 'T -> obj) =
        box (fun (s: Set<'T>) ->
            let a : obj[] = [||]
            s |> Set.iter (fun x -> a.JS.Push (encEl x) |> ignore)
            a)

    let StringMap (encEl: 'T -> obj) =
        box (fun (m: Map<string, 'T>) ->
            let o = New []
            m |> Map.iter (fun k v -> o?(k) <- encEl v)
            o)

    let StringDictionary (encEl: 'T -> obj) =
        box (fun (d: System.Collections.Generic.Dictionary<string, 'T>) ->
            let o = New []
            for KeyValue(k, v) in d :> seq<_> do o?(k) <- encEl v
            o)

[<JavaScript>]
module private Decode =

    let Tuple (decs: (obj -> obj)[]) = Encode.Tuple decs

    let List (decEl: obj -> 'T) =
        box (fun (a: obj[]) ->
            List.init a.Length (fun i -> decEl a.[i]))

    let Set (decEl: obj -> 'T) =
        box (fun (a: obj[]) ->
            Set.ofArray(Array.map decEl a))

    let Array decEl = Encode.Array decEl

    let StringMap (decEl: obj -> 'T) =
        box (fun (o: obj) ->
            let m = ref Map.empty
            JS.ForEach o (fun k -> m := Map.add k o?(k) !m; false)
            !m)

    let StringDictionary (decEl: obj -> 'T) =
        box (fun (o: obj) ->
            let d = System.Collections.Generic.Dictionary()
            JS.ForEach o (fun k -> d.[k] <- o?(k); false)
            d)

[<AutoOpen>]
module private Macro =

    module Q = WebSharper.Core.Quotations
    module R = WebSharper.Core.Reflection
    module T = WebSharper.Core.Reflection.Type
    module J = WebSharper.Core.JavaScript.Core
    module M = WebSharper.Core.Macros
    type T = WebSharper.Core.Reflection.Type
    type E = J.Expression

    let cString s = !~ (J.String s)
    let inline cInt i = !~ (J.Integer (int64 i))
    let cCall t m x = J.Call (t, cString m, x)
    let cCallG l m x = cCall (J.Global l) m x
    let cCallE m x = cCallG ["WebSharper"; "Json"; "Encode"] m x
    let cCallD m x = cCallG ["WebSharper"; "Json"; "Decode"] m x
    let (|T|) (t: R.TypeDefinition) = t.FullName
    let (>>=) (m: Choice<E, string> as x) (f: E -> Choice<E, string>) =
        match m with
        | Choice1Of2 e -> f e
        | Choice2Of2 _ -> x
    let ok x = Choice1Of2 x : Choice<E, string>
    let fail x = Choice2Of2 x : Choice<E, string>

    module Funs =
        let id = J.Global ["WebSharper"; "Json"; "Encode"; "Id"]

    let rec encode call (t: T) =
        match t with
        | T.Array (t, 1) ->
            encode call t >>= fun e ->
            ok (call "Array" [e])
        | T.Array _ ->
            fail "JSON serialization for multidimensional arrays is not supported."
        | T.Concrete (T ("Microsoft.FSharp.Core.Unit"
                        |"System.Boolean"
                        |"System.SByte" | "System.Byte"
                        |"System.Int16" | "System.UInt16"
                        |"System.Int32" | "System.UInt32"
                        |"System.Int64" | "System.UInt64"
                        |"System.Single"| "System.Double"
                        |"System.Decimal"
                        |"System.String"), []) ->
            ok Funs.id
        | T.Concrete (T "Microsoft.FSharp.Collections.FSharpList`1", [t]) ->
            encode call t >>= fun e ->
            ok (call "List" [e])
        | T.Concrete (T "Microsoft.FSharp.Collections.FSharpSet`1", [t]) ->
            encode call t >>= fun e ->
            ok (call "Set" [e])
        | T.Concrete (T "Microsoft.FSharp.Collections.FSharpMap`2",
                        [T.Concrete (T "System.String", []); t]) ->
            encode call t >>= fun e ->
            ok (call "StringMap" [e])
        | T.Concrete (T "System.Collections.Generic.Dictionary`2",
                        [T.Concrete (T "System.String", []); t]) ->
            encode call t >>= fun e ->
            ok (call "StringDictionary" [e])
        | T.Concrete (T n, ts) when n.StartsWith "System.Tuple`" ->
            ((fun es -> ok (call "Tuple" [J.NewArray es])), ts)
            ||> List.fold (fun k t ->
                fun es -> encode call t >>= fun e -> k (e :: es))
            <| []
        | t ->
            fail ("Type not supported for serialization: " + t.FullName)

    type SerializeMacro() =

        interface M.IMacro with
            member this.Translate(q, tr) =
                match q with
                // Serialize<'T> x
                | Q.CallModule({Generics = [t]}, [x]) ->
                    match encode cCallE t with
                    | Choice1Of2 enc ->
                        cCallG ["JSON"] "stringify" [J.Application(enc, [tr x])]
                    | Choice2Of2 msg ->
                        failwithf "%A: %s" t msg
                | _ -> tr q

    type DeserializeMacro() =

        interface M.IMacro with
            member this.Translate(q, tr) =
                match q with
                // Deserialize<'T> x
                | Q.CallModule({Generics = [t]}, [x]) ->
                    match encode cCallD t with
                    | Choice1Of2 dec ->
                        J.Application(dec, [cCallG ["JSON"] "parse" [tr x]])
                    | Choice2Of2 msg ->
                        failwithf "%A: %s" t msg
                | _ -> tr q

/// Serializes an object to JSON using the same readable format as Sitelets.
/// For plain JSON stringification, see Json.Stringify.
[<Macro(typeof<SerializeMacro>)>]
let Serialize<'T> (x: 'T) = X<string>

/// Deserializes a JSON string using the same readable format as Sitelets.
/// For plain JSON parsing, see Json.Parse.
[<Macro(typeof<DeserializeMacro>)>]
let Deserialize<'T> (x: string) = X<'T>
