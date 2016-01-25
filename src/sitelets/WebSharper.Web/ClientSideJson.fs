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

open System.Collections.Generic
open WebSharper
open WebSharper.JavaScript
open WebSharper.Core.Attributes

type OptionalFieldKind =
    /// The field doesn't have type option<'T>
    | NotOption = 0
    /// The field has type option<'T>
    | NormalOption = 1
    /// The field has type option<'T> and is marked [<OptionalField>]
    | MarkedOption = 2

[<JavaScript>]
type Provider() =

    static member Id () = id

    abstract EncodeTuple : (unit -> obj -> obj)[] -> (unit -> obj[] -> obj)
    default this.EncodeTuple encs =
        fun () args ->
            box (Array.map2 (fun f x -> f () x) encs args)

    abstract EncodeDateTime : unit -> (unit -> System.DateTime -> obj)
    default this.EncodeDateTime () =
        fun () x ->
            box (x.JS.ToISOString())

    abstract EncodeList : (unit -> 'T -> obj) -> (unit -> list<'T> -> obj)
    default this.EncodeList encEl =
        fun () (l: list<'T>) ->
            let a : obj[] = [||]
            let encEl = encEl()
            l |> List.iter (fun x -> a.JS.Push (encEl x) |> ignore)
            box a

    abstract EncodeRecord : obj -> (string * (unit -> obj -> obj) * OptionalFieldKind)[] -> (unit -> 'T -> obj)
    default this.EncodeRecord _ fields =
        fun () x ->
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
            o

    abstract EncodeUnion : obj -> string -> (string * (string * string * (unit -> obj -> obj) * OptionalFieldKind)[])[] -> (unit -> 'T -> obj)
    default this.EncodeUnion _ discr cases =
        fun () x ->
            if JS.TypeOf x ===. JS.Object then
                let o = New []
                let tag = x?("$")
                let tagName, fields = cases.[tag]
                if JS.TypeOf discr = JS.Kind.String then o?(discr) <- tagName
                fields |> Array.iter (fun (from, ``to``, enc, kind) ->
                    match from with
                    | null -> // inline record
                        let record = enc () (x?("$0"))
                        JS.ForEach record (fun f -> o?(f) <- record?(f); false)
                    | from -> // normal args
                        match kind with
                        | OptionalFieldKind.NotOption ->
                            o?(``to``) <- enc () (x?(from))
                        | OptionalFieldKind.NormalOption ->
                            match x?(from) with
                            | Some x -> o?(``to``) <- enc () x
                            | None -> ()
                        | _ -> failwith "Invalid field option kind")
                o
            else box x // [<Constant>]

    abstract EncodeArray : (unit -> 'T -> obj) -> (unit -> 'T[] -> obj)
    default this.EncodeArray encEl =
        fun () (a: 'T[]) ->
            let encEl = encEl()
            box (Array.map encEl a)

    abstract EncodeSet : (unit -> 'T -> obj) -> (unit -> Set<'T> -> obj)
    default this.EncodeSet encEl =
        fun () (s: Set<'T>) ->
            let a : obj[] = [||]
            let encEl = encEl()
            s |> Set.iter (fun x -> a.JS.Push (encEl x) |> ignore)
            box a

    abstract EncodeStringMap : (unit -> 'T -> obj) -> (unit -> Map<string, 'T> -> obj)
    default this.EncodeStringMap encEl =
        fun () (m: Map<string, 'T>) ->
            let o = New []
            let encEl = encEl()
            m |> Map.iter (fun k v -> o?(k) <- encEl v)
            o

    abstract EncodeStringDictionary : (unit -> 'T -> obj) -> (unit -> Dictionary<string, 'T> -> obj)
    default this.EncodeStringDictionary encEl =
        fun () (d: Dictionary<string, 'T>) ->
            let o = New []
            let encEl = encEl()
            for KeyValue(k, v) in d :> seq<_> do o?(k) <- encEl v
            o

    abstract DecodeTuple : (unit -> obj -> obj)[] -> (unit -> obj -> obj[])
    default this.DecodeTuple decs =
        As (this.EncodeTuple decs)

    abstract DecodeDateTime : unit -> (unit -> obj -> System.DateTime)
    default this.DecodeDateTime() =
        fun () x ->
            Date(x :?> string).Self

    abstract DecodeList : (unit -> obj -> 'T) -> (unit -> obj -> list<'T>)
    default this.DecodeList decEl =
        fun () a ->
            let decEl = decEl()
            List.init (a :?> obj[]).Length (fun i -> decEl (a :?> obj[]).[i])

    abstract DecodeSet : (unit -> obj -> 'T) -> (unit -> obj -> Set<'T>)
    default this.DecodeSet (decEl: unit -> obj -> 'T) : (unit -> obj -> Set<'T>) =
        fun () a ->
            let decEl = decEl()
            Set.ofArray(Array.map decEl (a :?> obj[]))

    abstract DecodeRecord : obj -> (string * (unit -> obj -> obj) * OptionalFieldKind)[] -> (unit -> obj -> 'T)
    default this.DecodeRecord t fields =
        fun () (x: obj) ->
        let o = if t ===. JS.Undefined then New [] else JS.New t
        fields |> Array.iter (fun (name, dec, kind) ->
            match kind with
            | OptionalFieldKind.NotOption ->
                o?(name) <- dec () x?(name)
            | OptionalFieldKind.NormalOption ->
                o?(name) <-
                    if JS.HasOwnProperty x name
                    then Some (dec () x?(name))
                    else None
            | OptionalFieldKind.MarkedOption ->
                if JS.HasOwnProperty x name then
                    o?(name) <- (dec () x?(name))
            | _ -> failwith "Invalid field option kind")
        o

    abstract DecodeUnion : obj -> string -> (string * (string * string * (unit -> obj -> obj) * OptionalFieldKind)[])[] -> (unit -> obj -> 'T)
    default this.DecodeUnion t discr cases =
        fun () (x: obj) ->
            if JS.TypeOf x ===. JS.Object then
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
                cases.[tag] |> snd |> Array.iter (fun (from, ``to``, dec, kind) ->
                    match from with
                    | null -> // inline record
                        o?("$0") <- dec () x
                    | from -> // normal args
                        match kind with
                        | OptionalFieldKind.NotOption ->
                            o?(from) <- dec () (x?(``to``))
                        | OptionalFieldKind.NormalOption ->
                            o?(from) <-
                                if JS.HasOwnProperty x ``to``
                                then Some (dec () x?(``to``))
                                else None
                        | _ -> failwith "Invalid field option kind")
                o
            else x :?> 'T // [<Constant>]

    abstract DecodeArray : (unit -> obj -> 'T) -> (unit -> obj -> 'T[])
    default this.DecodeArray decEl =
        As (this.EncodeArray (As decEl))

    abstract DecodeStringMap : (unit -> obj -> 'T) -> (unit -> obj -> Map<string, 'T>)
    default this.DecodeStringMap decEl =
        fun () (o: obj) ->
            let m = ref Map.empty
            let decEl = decEl ()
            JS.ForEach o (fun k -> m := Map.add k o?(k) !m; false)
            !m

    abstract DecodeStringDictionary : (unit -> obj -> 'T) -> (unit -> obj -> Dictionary<string, 'T>)
    default this.DecodeStringDictionary (decEl: unit -> obj -> 'T) : (unit -> obj -> Dictionary<string, 'T>) =
        fun () (o: obj) ->
            let d = System.Collections.Generic.Dictionary()
            let decEl = decEl ()
            JS.ForEach o (fun k -> d.[k] <- o?(k); false)
            d

[<JavaScript>]
module private Internals =

    let Provider = Provider()

type Provider with
    [<JavaScript; Inline>]
    static member Default = Internals.Provider

module Macro =

    module J = WebSharper.Core.JavaScript.Core
    module M = WebSharper.Core.Macros
    module Q = WebSharper.Core.Quotations
    module R = WebSharper.Core.Reflection
    module T = WebSharper.Core.Reflection.Type
    module JI = WebSharper.Core.Json.Internal
    type private FST = Microsoft.FSharp.Reflection.FSharpType
    type private T = WebSharper.Core.Reflection.Type
    type private BF = System.Reflection.BindingFlags

    type Parameters =
        {
            Warn : string -> unit
            Provider : J.Expression
        }

    [<AutoOpen>]
    module private Internals =

        let cString s = !~ (J.String s)
        let inline cInt i = !~ (J.Integer (int64 i))
        let cCall t m x = J.Call (t, cString m, x)
        let cCallG l m x = cCall (J.Global l) m x
        let cCallE m x = cCallG ["WebSharper"; "Json"; "Encode"] m x
        let cCallD m x = cCallG ["WebSharper"; "Json"; "Decode"] m x
        let (|T|) (t: R.TypeDefinition) = t.FullName
        let invoke x isEnc n = cCall x ((if isEnc then "Encode" else "Decode") + n)

        type EncodeResult = Choice<J.Expression, string>

        let (>>=) (m as x: EncodeResult) (f: J.Expression -> EncodeResult) =
            match m with
            | Choice1Of2 e -> f e
            | Choice2Of2 _ -> x
        let ok x = Choice1Of2 x : EncodeResult
        let fail x = Choice2Of2 x : EncodeResult

        let flags = BF.Public ||| BF.NonPublic

        module Funs =
            let id = J.Global ["WebSharper"; "Json"; "Provider"; "Id"]

        let getEncoding name isEnc param tr (t: T) =
            let ctx = System.Collections.Generic.Dictionary()
            let call = invoke param.Provider isEnc
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
                                |"System.String"| "System.Guid"
                                |"WebSharper.Core.Json+Encoded"), []) ->
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
                    | true, (id, e, _) ->
                        ctx.[td] <- (id, e, true)
                        ok (J.Var id)
                    | false, _ ->
                        let id = J.Id()
                        ctx.[td] <- (id, !~J.Null, false)
                        ((fun es ->
                            encRecType t args es >>= fun e ->
                            let _, _, multiple = ctx.[td]
                            ctx.[td] <- (id, e, multiple)
                            ok (J.Var id)
                         ), args)
                        ||> List.fold (fun k t es ->
                            encode t >>= fun e -> k ((t, e) :: es))
                        <| []
                | T.Generic _ ->
                    fail (name + ": Cannot de/serialize a generic value. You must call this function with a concrete type.")
            // Encode a type that might be recursively defined
            and encRecType t targs args =
                let tt = t.Load(false)
                if FST.IsRecord(tt, flags) then
                    let fields =
                        FST.GetRecordFields(tt, flags)
                        |> Array.map (fun f ->
                            JI.GetName f, f, f.PropertyType)
                    ((fun es ->
                        let es, tts = List.unzip es
                        // In order to construct a value of the right type, we need the
                        // JS class corresponding to our type. To get it directly we would
                        // need access to the metadata. For now the best way we have is
                        // to compile a dummy object creation and extract the class from it.
                        let tn =
                            match tr (Q.NewRecord(t, List.map Q.DefaultValue tts)) with
                            // Runtime.New(rec, {...})
                            // Runtime.New(rec, Runtime.DeleteEmptyFields({...}, [...]))
                            | J.Call (_, J.Constant (J.String "New"), [x; _]) -> x
                            // Runtime.DeleteEmptyFields({...}, [...])
                            | J.Call (_, J.Constant (J.String "DeleteEmptyFields"), _) 
                            // {...}
                            | J.NewObject _ -> !~J.Undefined
                            | x -> failwithf "Invalid compiled record creation: %O" x
                        ok (call "Record" [tn; J.NewArray es])
                        ), fields)
                    ||> Array.fold (fun k (n, f, t) ->
                        fun es ->
                            if not (Array.isEmpty (f.GetCustomAttributes(typeof<DateTimeFormatAttribute>, false))) then
                                param.Warn (sprintf "Warning: This record field has a custom DateTime format: %s.%s. \
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
                            let tt = (T.FromType t)
                            encode tt >>= fun e ->
                            k ((J.NewArray [cString n; e; optionKind], tt) :: es))
                    <| []
                elif FST.IsUnion(tt, flags) then
                    let discr, cases = JI.GetUnionEncoding tt
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
                        // In order to construct a value of the right type, we need the
                        // JS class corresponding to our type. To get it directly we would
                        // need access to the metadata. For now the best way we have is
                        // to compile a dummy object creation and extract the class from it.
                        let tn =
                            let c1 = FST.GetUnionCases(tt, flags).[0]
                            let uc : Q.Concrete<R.UnionCase> =
                                { Generics = targs
                                  Entity = R.UnionCase.Create (R.TypeDefinition.FromType tt) c1.Name }
                            let args =
                                c1.GetFields()
                                |> Array.map (fun f -> Q.DefaultValue (T.FromType f.PropertyType))
                                |> List.ofArray
                            match tr (Q.NewUnionCase(uc, args)) with
                            // Runtime.New(union, {...})
                            | J.Call (_, J.Constant (J.String "New"), [x; _]) -> x
                            // {...}
                            | J.NewObject _
                            // [<Constant>]
                            | J.Constant _ -> !~J.Undefined
                            | x -> failwithf "Invalid compiled union creation: %O" x
                        ok (call "Union" [tn; discr; cases])
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
                                        param.Warn (sprintf "Warning: This union case field has a custom DateTime format: %s.%s [%s]. \
                                            Client-side JSON serialization does not support custom DateTime formatting. \
                                            This field will be serialized using ISO format."
                                            tt.FullName caseName argName)
                                    let argT, optionKind =
                                        if argT.IsGenericType && argT.GetGenericTypeDefinition() = typedefof<option<_>> then
                                            argT.GetGenericArguments().[0], cInt (int OptionalFieldKind.NormalOption)
                                        else argT, cInt (int OptionalFieldKind.NotOption)
                                    j + 1, fun es ->
                                        encode (T.FromType argT) >>= fun e ->
                                        k (J.NewArray [cString ("$" + string j); cString argName; e; optionKind] :: es))
                                |> snd
                                <| []
                            | JI.InlineRecord(name, record) ->
                                let tag =
                                    match discr with
                                    | JI.StandardField -> cInt i
                                    | _ -> cString name
                                encode (T.FromType record) >>= fun e ->
                                k (J.NewArray [tag; J.NewArray [J.NewArray [!~J.Null; !~J.Null; e]]] :: es)
                            | JI.Constant _ -> k (!~J.Null :: es)
                    )
                    |> snd
                    <| []
                else
                    fail (name + ": Type not supported: " + tt.FullName)
            match encode t with
            | Choice1Of2 x ->
                if ctx |> Seq.forall (fun (KeyValue(_, (_, _, multiple))) -> not multiple) then
                    // Every type is present only once and non-recursive;
                    // no need for "let"s at all, we can just have one big expression.
                    let rec sub x =
                        let res =
                            x |> J.Substitute (fun id ->
                                ctx
                                |> Array.ofSeq
                                |> Array.tryPick (fun (KeyValue(k, (id', e, _))) ->
                                    if id = id' then
                                        ctx.Remove(k) |> ignore
                                        Some e
                                    else None))
                        if ctx.Count = 0 then res else sub res
                    sub x
                else
                    J.LetRecursive(
                        let fld = !~(J.String "x")
                        [for KeyValue(_, (id, e, multiple)) in ctx do
                            let xid = J.Id()
                            // xid = {}
                            yield xid, J.NewObject []
                            // id = function() { if (!xid.x) { xid.x = e() }; return xid.x; }
                            yield id, J.Lambda(None, [],
                                J.Sequential(
                                    J.IfThenElse(
                                        J.Unary(J.UnaryOperator.``!``, J.FieldGet(J.Var xid, fld)),
                                        J.FieldSet(J.Var xid, fld, J.Application(e, [])),
                                        !~J.Null),
                                    J.FieldGet(J.Var xid, fld)))
                        ], x)
            | Choice2Of2 msg -> failwithf "%A: %s" t msg

        let encodeLambda name param tr t =
            J.Application(getEncoding name true param tr t, [])

        let encode name param tr t arg =
            J.Application(encodeLambda name param tr t, [arg])

        let decodeLambda name param tr t =
            J.Application(getEncoding name false param tr t, [])

        let decode name param tr t arg =
            J.Application(decodeLambda name param tr t, [arg])

    type Parameters with

        static member Default (tr : Q.Expression -> J.Expression) =
            {
                Warn = ignore
                Provider =
                    Q.PropertyGet(
                        {
                            Entity = typeof<Provider>.GetProperty("Default", BF.Static ||| BF.Public) |> R.Property.Parse
                            Generics = []
                        },
                        []
                    )
                    |> tr
            }

    let Encode param tr t arg =
        // ENCODE()(arg)
        encode "Encode" param tr t (tr arg)

    let EncodeLambda param tr t =
        // ENCODE()
        encodeLambda "EncodeLambda" param tr t

    let Serialize param tr t arg =
        // JSON.stringify(ENCODE()(arg))
        cCallG ["JSON"] "stringify" [encode "Serialize" param tr t (tr arg)]

    let SerializeLambda param tr t =
        let enc = J.Id()
        let arg = J.Id()
        // let enc = ENCODE() in fun arg -> JSON.stringify(enc(arg))
        J.Let(enc, encodeLambda "SerializeLambda" param tr t,
            J.Lambda(None, [arg],
                cCallG ["JSON"] "stringify" [J.Application(J.Var enc, [J.Var arg])]))

    let Decode param tr t arg =
        // DECODE()(arg)
        decode "Decode" param tr t (tr arg)

    let DecodeLambda param tr t =
        // DECODE()
        decodeLambda "DecodeLambda" param tr t

    let Deserialize param tr t arg =
        // DECODE()(JSON.parse(arg))
        decode "Deserialize" param tr t (cCallG ["JSON"] "parse" [tr arg])

    let DeserializeLambda param tr t =
        // let dec = DECODE() in fun arg -> dec(JSON.parse(arg))
        let dec = J.Id()
        let arg = J.Id()
        J.Let(dec, decodeLambda "DeserializeLambda" param tr t,
            J.Lambda(None, [arg],
                J.Application(J.Var dec, [cCallG ["JSON"] "parse" [J.Var arg]])))

    type SerializeMacro() =

        static let rec last = function
            | [x] -> x
            | x :: l -> last l
            | _ -> failwith ""

        interface M.IMacro with
            member this.Translate(q, tr) =
                let param = Parameters.Default tr // TODO: change Warn when we implement warn in macros
                match q with
                | Q.CallModule({Generics = [t]; Entity = e}, args)
                | Q.Call({Generics = [t]; Entity = e}, args) ->
                    let f, provider =
                        match e.Name with
                        | "Encode" -> Encode, param.Provider
                        | "Decode" -> Decode, param.Provider
                        | "Serialize" -> Serialize, param.Provider
                        | "Deserialize" -> Deserialize, param.Provider
                        | "EncodeWith" -> Encode, tr (List.head args)
                        | "DecodeWith" -> Decode, tr (List.head args)
                        | "SerializeWith" -> Serialize, tr (List.head args)
                        | "DeserializeWith" -> Deserialize, tr (List.head args)
                        | _ -> failwith "Invalid macro invocation"
                    let id = J.Id()
                    J.Let(id, provider, f {param with Provider = J.Var id} tr t (last args))
                | _ -> failwith "Invalid macro invocation"

open Macro

/// Encodes an object in such a way that JSON stringification
/// results in the same readable format as Sitelets.
[<Macro(typeof<SerializeMacro>)>]
let Encode<'T> (x: 'T) = X<obj>

/// Encodes an object in such a way that JSON stringification
/// results in the same readable format as Sitelets.
[<Macro(typeof<SerializeMacro>)>]
let EncodeWith<'T> (provider: Provider) (x: 'T) = X<obj>

/// Serializes an object to JSON using the same readable format as Sitelets.
/// For plain JSON stringification, see Json.Stringify.
[<Macro(typeof<SerializeMacro>)>]
let Serialize<'T> (x: 'T) = X<string>

/// Serializes an object to JSON using the same readable format as Sitelets.
/// For plain JSON stringification, see Json.Stringify.
[<Macro(typeof<SerializeMacro>)>]
let SerializeWith<'T> (provider: Provider) (x: 'T) = X<string>

/// Decodes an object parsed from the same readable JSON format as Sitelets.
[<Macro(typeof<SerializeMacro>)>]
let Decode<'T> (x: obj) = X<'T>

/// Decodes an object parsed from the same readable JSON format as Sitelets.
[<Macro(typeof<SerializeMacro>)>]
let DecodeWith<'T> (provider: Provider) (x: obj) = X<'T>

/// Deserializes a JSON string using the same readable format as Sitelets.
/// For plain JSON parsing, see Json.Parse.
[<Macro(typeof<SerializeMacro>)>]
let Deserialize<'T> (x: string) = X<'T>

/// Deserializes a JSON string using the same readable format as Sitelets.
/// For plain JSON parsing, see Json.Parse.
[<Macro(typeof<SerializeMacro>)>]
let DeserializeWith<'T> (provider: Provider) (x: string) = X<'T>

let (|Object|Array|Number|String|Boolean|Undefined|) (o: WebSharper.Core.Json.Encoded) =
    match JS.TypeOf o with
    | JS.Kind.Boolean -> Boolean (As<bool> o)
    | JS.Kind.Number -> Number (As<float> o)
    | JS.Kind.String -> String (As<string> o)
    | JS.Kind.Undefined -> Undefined o
    | JS.Kind.Function -> failwith ""
    | JS.Kind.Object ->
        if JS.InstanceOf o JS.Window?Array then
            Array (As<WebSharper.JavaScript.Array<WebSharper.Core.Json.Encoded>> o)
        else
            Object (As<WebSharper.JavaScript.Object<WebSharper.Core.Json.Encoded>> o)
