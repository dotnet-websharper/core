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

type private OptionalFieldKind =
    | NotOption = 0     // The field doesn't have type option<'T>
    | NormalOption = 1  // The field has type option<'T>
    | MarkedOption = 2  // The field has type option<'T> and is marked [<OptionalField>]

[<JavaScript>]
module private Encode =

    let Id () = box id

    let Tuple (encs: (unit -> obj -> obj)[]) =
        box (fun () ->
            box (fun args ->
                Array.map2 (fun f x -> f () x) encs args))

    let DateTime() =
        box (fun () ->
            box (fun (x: System.DateTime) ->
                x.JS.ToISOString()))

    let List (encEl: unit -> 'T -> obj) =
        box (fun () ->
            box (fun (l: list<'T>) ->
                let a : obj[] = [||]
                let encEl = encEl()
                l |> List.iter (fun x -> a.JS.Push (encEl x) |> ignore)
                a))

    let Record _ (fields: (string * (unit -> obj -> obj) * OptionalFieldKind)[]) =
        box (fun () ->
            box (fun (x: obj) ->
                let o = New []
                fields |> Array.iter (fun (name, enc, kind) ->
                    match kind with
                    | OptionalFieldKind.NotOption ->
                        o?(name) <- enc () x?(name)
                    | OptionalFieldKind.NormalOption ->
                        match x?(name) with
                        | Some x -> o?(name) <- enc () x
                        | None -> ()
                    | OptionalFieldKind.MarkedOption ->
                        if JS.HasOwnProperty x name then
                            o?(name) <- enc () x?(name)
                    | _ -> failwith "Invalid field option kind")
                o))

    let Union _ (discr: string) (cases: (string * (string * string * (unit -> obj -> obj))[])[]) =
        box (fun () ->
            box (fun (x: obj) ->
                let o = New []
                let tag = x?("$")
                let tagName, fields = cases.[tag]
                if JS.TypeOf discr = JS.Kind.String then o?(discr) <- tagName
                fields |> Array.iter (fun (from, ``to``, enc) ->
                    match from with
                    | null -> // inline record
                        let record = enc () (x?("$0"))
                        JS.ForEach record (fun f -> o?(f) <- record?(f); false)
                    | from -> // normal args
                        o?(``to``) <- enc () (x?(from)))
                o))

    let Array (encEl: unit -> 'T -> obj) =
        box (fun () ->
            box (fun (a: 'T[]) ->
                let encEl = encEl()
                Array.map encEl a))

    let Set (encEl: unit -> 'T -> obj) =
        box (fun () ->
            box (fun (s: Set<'T>) ->
                let a : obj[] = [||]
                let encEl = encEl()
                s |> Set.iter (fun x -> a.JS.Push (encEl x) |> ignore)
                a))

    let StringMap (encEl: unit -> 'T -> obj) =
        box (fun () ->
            box (fun (m: Map<string, 'T>) ->
                let o = New []
                let encEl = encEl()
                m |> Map.iter (fun k v -> o?(k) <- encEl v)
                o))

    let StringDictionary (encEl: unit -> 'T -> obj) =
        box (fun () ->
            box (fun (d: System.Collections.Generic.Dictionary<string, 'T>) ->
                let o = New []
                let encEl = encEl()
                for KeyValue(k, v) in d :> seq<_> do o?(k) <- encEl v
                o))

[<JavaScript>]
module private Decode =

    let Tuple (decs: (unit -> obj -> obj)[]) =
        Encode.Tuple decs

    let DateTime() =
        box (fun () ->
            box (fun (x: string) ->
                Date(x).Self))

    let List (decEl: unit -> obj -> 'T) =
        box (fun () ->
            box (fun (a: obj[]) ->
                let decEl = decEl()
                List.init a.Length (fun i -> decEl a.[i])))

    let Set (decEl: unit -> obj -> 'T) =
        box (fun () ->
            box (fun (a: obj[]) ->
                let decEl = decEl()
                Set.ofArray(Array.map decEl a)))

    let Record (t: obj) (fields: (string * (unit -> obj -> obj) * OptionalFieldKind)[]) =
        box (fun () ->
            box (fun (x: obj) ->
                let o = if t ===. JS.Undefined then New [] else JS.New t
                fields |> Array.iter (fun (name, enc, kind) ->
                    match kind with
                    | OptionalFieldKind.NotOption ->
                        o?(name) <- enc () x?(name)
                    | OptionalFieldKind.NormalOption ->
                        o?(name) <-
                            if JS.HasOwnProperty x name
                            then Some (enc () x?(name))
                            else None
                    | OptionalFieldKind.MarkedOption ->
                        if JS.HasOwnProperty x name then
                            o?(name) <- (enc () x?(name))
                    | _ -> failwith "Invalid field option kind")
                o))

    let Union (t: obj) (discr: string) (cases: (string * (string * string * (unit -> obj -> obj))[])[]) =
        box (fun () ->
            box (fun (x: obj) ->
                let o = if t ===. JS.Undefined then New [] else JS.New t
                let tag =
                    // [<NamedUnionCases(discr)>]
                    if JS.TypeOf discr = JS.Kind.String then
                        let tagName = x?(discr)
                        cases |> Array.findIndex (fun (name, _) -> name = tagName)
                    else // [<NamedUnionCases>]
                        let r = ref JS.Undefined
                        JS.ForEach discr (fun k ->
                            if JS.HasOwnProperty x k then r := discr?(k); true else false)
                        !r
                o?("$") <- tag
                cases.[tag] |> snd |> Array.iter (fun (from, ``to``, dec) ->
                    match from with
                    | null -> // inline record
                        o?("$0") <- dec () x
                    | from -> // normal args
                        o?(from) <- dec () (x?(``to``)))
                o))

    let Array decEl =
        Encode.Array decEl

    let StringMap (decEl: unit -> obj -> 'T) =
        box (fun () ->
            box (fun (o: obj) ->
                let m = ref Map.empty
                let decEl = decEl ()
                JS.ForEach o (fun k -> m := Map.add k o?(k) !m; false)
                !m))

    let StringDictionary (decEl: unit -> obj -> 'T) =
        box (fun () ->
            box (fun (o: obj) ->
                let d = System.Collections.Generic.Dictionary()
                let decEl = decEl ()
                JS.ForEach o (fun k -> d.[k] <- o?(k); false)
                d))

[<AutoOpen>]
module private Macro =

    module Q = WebSharper.Core.Quotations
    module R = WebSharper.Core.Reflection
    module T = WebSharper.Core.Reflection.Type
    module J = WebSharper.Core.JavaScript.Core
    module JI = WebSharper.Core.Json.Internal
    module M = WebSharper.Core.Macros
    type FST = Microsoft.FSharp.Reflection.FSharpType
    type T = WebSharper.Core.Reflection.Type
    type E = J.Expression

    let cString s = !~ (J.String s)
    let inline cInt i = !~ (J.Integer (int64 i))
    let cCall t m x = J.Call (t, cString m, x)
    let cCallG l m x = cCall (J.Global l) m x
    let cCallE m x = cCallG ["WebSharper"; "Json"; "Encode"] m x
    let cCallD m x = cCallG ["WebSharper"; "Json"; "Decode"] m x
    let (|T|) (t: R.TypeDefinition) = t.FullName

    type EncodeResult = Choice<E, string>

    let (>>=) (m as x: EncodeResult) (f: E -> EncodeResult) =
        match m with
        | Choice1Of2 e -> f e
        | Choice2Of2 _ -> x
    let ok x = Choice1Of2 x : EncodeResult
    let fail x = Choice2Of2 x : EncodeResult

    let flags =
        System.Reflection.BindingFlags.Public
        ||| System.Reflection.BindingFlags.NonPublic

    module Funs =
        let id = J.Global ["WebSharper"; "Json"; "Encode"; "Id"]

    let getEncoding name call warn (t: T) =
        let ctx = System.Collections.Generic.Dictionary()
        let rec encode t =
            match t with
            | T.Array (t, 1) ->
                encode t >>= fun e ->
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
                encode t >>= fun e ->
                ok (call "List" [e])
            | T.Concrete (T "Microsoft.FSharp.Collections.FSharpSet`1", [t]) ->
                encode t >>= fun e ->
                ok (call "Set" [e])
            | T.Concrete (T "Microsoft.FSharp.Collections.FSharpMap`2",
                            [T.Concrete (T "System.String", []); t]) ->
                encode t >>= fun e -> 
                ok (call "StringMap" [e])
            | T.Concrete (T "System.Collections.Generic.Dictionary`2",
                            [T.Concrete (T "System.String", []); t]) ->
                encode t >>= fun e ->
                ok (call "StringDictionary" [e])
            | T.Concrete (T n, ts) when n.StartsWith "System.Tuple`" ->
                ((fun es -> ok (call "Tuple" [J.NewArray es])), ts)
                ||> List.fold (fun k t ->
                    fun es -> encode t >>= fun e -> k (e :: es))
                <| []
            | T.Concrete (T "System.DateTime", []) ->
                ok (call "DateTime" [])
            | T.Concrete (td, args) ->
                match ctx.TryGetValue td with
                | true, (id, _) -> ok (J.Var id)
                | false, _ ->
                    let id = J.Id()
                    ctx.[td] <- (id, !~J.Null)
                    ((fun es ->
                        encRecType t td es >>= fun e ->
                        ctx.[td] <- (id, e)
                        ok (J.Var id)
                     ), args)
                    ||> List.fold (fun k t es ->
                        encode t >>= fun e -> k ((t, e) :: es))
                    <| []
            | T.Generic _ ->
                fail (name + ": Cannot de/serialize a generic value. You must call this function with a concrete type.")
        // Encode a type that might be recursively defined
        and encRecType t td args =
            let typeAddress() =
                let n =
                    match td.Name.LastIndexOf '`' with
                    | -1 -> td.Name
                    | i -> td.Name.[..i-1]
                J.FieldGet(J.Global td.DeclaringAddress, cString n)
            match td, args with
            | td, args ->
                let t = t.Load(false)
                if FST.IsRecord(t, flags) then
                    let fields =
                        FST.GetRecordFields(t, flags)
                        |> Array.map (fun f ->
                            JI.GetName f, f, f.PropertyType)
                    ((fun es ->
                        ok (call "Record" [typeAddress(); J.NewArray es])
                     ), fields)
                    ||> Array.fold (fun k (n, f, t) ->
                        fun es ->
                            if not (Array.isEmpty (f.GetCustomAttributes(typeof<DateTimeFormatAttribute>, false))) then
                                warn (sprintf "Warning: This record field has a custom DateTime format: %s.%s. \
                                    Client-side JSON serialization does not support custom DateTime formatting. \
                                    This field will be serialized using ISO format."
                                    f.DeclaringType.FullName f.Name)
                            let t, optionKind =
                                if t.IsGenericType && t.GetGenericTypeDefinition() = typedefof<option<_>> then
                                    let kind =
                                        if Array.isEmpty (f.GetCustomAttributes(typeof<OptionalFieldAttribute>, false)) then
                                            OptionalFieldKind.NormalOption
                                        else OptionalFieldKind.MarkedOption
                                    t.GetGenericArguments().[0], cInt (int kind)
                                else t, cInt (int OptionalFieldKind.NotOption)
                            encode (T.FromType t) >>= fun e ->
                            k (J.NewArray [cString n; e; optionKind] :: es))
                    <| []
                elif FST.IsUnion(t, flags) then
                    let discr, cases = JI.GetUnionEncoding t
                    ((0, fun cases ->
                        let cases = J.NewArray cases
                        let discr =
                            match discr with
                            | JI.NoField discrFields ->
                                discrFields
                                |> List.map (fun (name, id) -> name, cInt id)
                                |> J.NewObject
                            | JI.StandardField -> cString "$"
                            | JI.NamedField n -> cString n
                        ok (call "Union" [typeAddress(); discr; cases])
                        ), cases)
                    ||> Array.fold (fun (i, k) case ->
                        i + 1, fun es ->
                            match case with
                            | JI.Normal (caseName, argNames) ->
                                ((0, fun argNames ->
                                    let tag =
                                        match discr with
                                        | JI.StandardField -> cInt i
                                        | _ -> cString caseName
                                    k (J.NewArray [tag; J.NewArray argNames] :: es)
                                 ), argNames)
                                ||> Array.fold (fun (j, k) (argName, argT, argFlags) ->
                                    if argFlags |> Array.exists (function JI.DateTimeFormat _ -> true) then
                                        warn (sprintf "Warning: This union case field has a custom DateTime format: %s.%s [%s]. \
                                            Client-side JSON serialization does not support custom DateTime formatting. \
                                            This field will be serialized using ISO format."
                                            t.FullName caseName argName)
                                    j + 1, fun es ->
                                        encode (T.FromType argT) >>= fun e ->
                                        k (J.NewArray [cString ("$" + string j); cString argName; e] :: es))
                                |> snd
                                <| []
                            | JI.InlineRecord(name, record) ->
                                let tag =
                                    match discr with
                                    | JI.StandardField -> cInt i
                                    | _ -> cString name
                                encode (T.FromType record) >>= fun e ->
                                k (J.NewArray [tag; J.NewArray [J.NewArray [!~J.Null; !~J.Null; e]]] :: es)
                    )
                    |> snd
                    <| []
                else
                    fail (name + ": Type not supported: " + t.FullName)
        match encode t with
        | Choice1Of2 x ->
            J.LetRecursive(
                [for KeyValue(_, (id, e)) in ctx do
                    let xid = J.Id()
                    yield xid, !~J.Null
                    // function() { if (!xid) { xid = e() }; return xid; }
                    yield id, J.Lambda(None, [],
                        J.Sequential(
                            J.IfThenElse(
                                J.Unary(J.UnaryOperator.``!``, J.Var xid),
                                J.VarSet(xid, J.Application(e, [])),
                                !~J.Null),
                            J.Var xid))
                    ],
                x)
        | Choice2Of2 msg -> failwithf "%A: %s" t msg

    type SerializeMacro() =

        let encode name warn t arg =
            let enc = getEncoding name cCallE warn t
            J.Application(J.Application(enc, []), [arg])

        let Encode warn t arg =
            encode "Encode" warn t arg

        let Serialize warn t arg =
            cCallG ["JSON"] "stringify" [encode "Serialize" warn t arg]

        let decode name warn t arg =
            let dec = getEncoding name cCallD warn t
            J.Application(J.Application(dec, []), [arg])

        let Decode warn t arg =
            decode "Decode" warn t arg

        let Deserialize warn t arg =
            decode "Deserialize" warn t (cCallG ["JSON"] "parse" [arg])


        interface M.IMacro with
            member this.Translate(q, tr) =
                let warn = ignore // to change when we implement warn in macros
                match q with
                // Serialize<'T> x
                | Q.CallModule({Generics = [t]; Entity = e}, [x])
                | Q.Call({Generics = [t]; Entity = e}, [x]) ->
                    (match e.Name with
                    | "Encode" -> Encode
                    | "Decode" -> Decode
                    | "Serialize" -> Serialize
                    | "Deserialize" -> Deserialize
                    | _ -> failwith "Invalid macro invocation")
                        warn t (tr x)
                | _ -> failwith "Invalid macro invocation"

/// Encodes an object in such a way that JSON stringification
/// results in the same readable format as Sitelets.
[<Macro(typeof<SerializeMacro>)>]
let Encode<'T> (x: 'T) = X<obj>

/// Serializes an object to JSON using the same readable format as Sitelets.
/// For plain JSON stringification, see Json.Stringify.
[<Macro(typeof<SerializeMacro>)>]
let Serialize<'T> (x: 'T) = X<string>

/// Decodes an object parsed from the same readable JSON format as Sitelets.
[<Macro(typeof<SerializeMacro>)>]
let Decode<'T> (x: obj) = X<'T>

/// Deserializes a JSON string using the same readable format as Sitelets.
/// For plain JSON parsing, see Json.Parse.
[<Macro(typeof<SerializeMacro>)>]
let Deserialize<'T> (x: string) = X<'T>
