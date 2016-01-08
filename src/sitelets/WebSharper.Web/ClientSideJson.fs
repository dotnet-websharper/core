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

    let Union _ (discr: string) (cases: (string * (string * string * (unit -> obj -> obj) * OptionalFieldKind)[])[]) =
        box (fun () ->
            box (fun (x: obj) ->
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
                else x // [<Constant>]
            ))

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
                o))

    let Union (t: obj) (discr: string) (cases: (string * (string * string * (unit -> obj -> obj) * OptionalFieldKind)[])[]) =
        box (fun () ->
            box (fun (x: obj) ->
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
                else x // [<Constant>]
            ))

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

module private MacroInternals =

//    module Q = WebSharper.Core.Quotations
//    module R = WebSharper.Core.Reflection
//    module T = WebSharper.Core.Reflection.Type
    module M = WebSharper.Core.Metadata
    open WebSharper.Core.AST
    module JI = WebSharper.Core.Json.Internal
    type FST = Microsoft.FSharp.Reflection.FSharpType
//    type T = WebSharper.Core.Reflection.Type
//    type E = J.Expression

    let cString s = !~ (Literal.String s)
    let inline cInt i = !~ (Int i)
    let cCall t m x = Application (ItemGet(t, cString m), x)
    let cCallG l m x = cCall (globalAccess l) m x
    let cCallE m x = cCallG ["WebSharper"; "Json"; "Encode"] m x
    let cCallD m x = cCallG ["WebSharper"; "Json"; "Decode"] m x
    let (|T|) (t: TypeDefinition) = t.Value.FullName
    let (|C|_|) (t: Type) =
        match t with 
        | ConcreteType { Entity = e; Generics = g} -> Some (e, g)
        | _ -> None

    type EncodeResult = Choice<Expression, string>

    let (>>=) (m as x: EncodeResult) (f: Expression -> EncodeResult) =
        match m with
        | Choice1Of2 e -> f e
        | Choice2Of2 _ -> x
    let ok x = Choice1Of2 x : EncodeResult
    let fail x = Choice2Of2 x : EncodeResult

    let flags =
        System.Reflection.BindingFlags.Public
        ||| System.Reflection.BindingFlags.NonPublic

    module Funs =
        let id = globalAccess ["WebSharper"; "Json"; "Encode"; "Id"]

    let getEncoding name call warn (comp: M.Compilation) (t: Type) =
        let ctx = System.Collections.Generic.Dictionary()
        let rec encode t =
            match t with
            | ArrayType (t, 1) ->
                encode t >>= fun e ->
                ok (call "Array" [e])
            | ArrayType _ ->
                fail "JSON serialization for multidimensional arrays is not supported."
            | C (T ("Microsoft.FSharp.Core.Unit"
                            |"System.Boolean"
                            |"System.SByte" | "System.Byte"
                            |"System.Int16" | "System.UInt16"
                            |"System.Int32" | "System.UInt32"
                            |"System.Int64" | "System.UInt64"
                            |"System.Single"| "System.Double"
                            |"System.Decimal"
                            |"System.String"
                            |"WebSharper.Core.Json+Encoded"), []) ->
                ok Funs.id
            | C (T "Microsoft.FSharp.Collections.FSharpList`1", [t]) ->
                encode t >>= fun e ->
                ok (call "List" [e])
            | C (T "Microsoft.FSharp.Collections.FSharpSet`1", [t]) ->
                encode t >>= fun e ->
                ok (call "Set" [e])
            | C (T "Microsoft.FSharp.Collections.FSharpMap`2",
                            [C (T "System.String", []); t]) ->
                encode t >>= fun e -> 
                ok (call "StringMap" [e])
            | C (T "System.Collections.Generic.Dictionary`2",
                            [C (T "System.String", []); t]) ->
                encode t >>= fun e ->
                ok (call "StringDictionary" [e])
            | TupleType ts ->
                ((fun es -> ok (call "Tuple" [NewArray es])), ts)
                ||> List.fold (fun k t ->
                    fun es -> encode t >>= fun e -> k (e :: es))
                <| []
            | C (T "System.DateTime", []) ->
                ok (call "DateTime" [])
            | C (td, args) ->
                match ctx.TryGetValue td with
                | true, (id, e, _) ->
                    ctx.[td] <- (id, e, true)
                    ok (Var id)
                | false, _ ->
                    let id = Id.New()
                    ctx.[td] <- (id, Value Null, false)
                    ((fun es ->
                        encRecType t args es >>= fun e ->
                        let _, _, multiple = ctx.[td]
                        ctx.[td] <- (id, e, multiple)
                        ok (Var id)
                        ), args)
                    ||> List.fold (fun k t es ->
                        encode t >>= fun e -> k ((t, e) :: es))
                    <| []
            // TODO: ByRefType
            | GenericType _ ->
                fail (name + ": Cannot de/serialize a generic value. You must call this function with a concrete type.")
        // Encode a type that might be recursively defined
        and encRecType t targs args =
            let tt = Reflection.loadType t
            if FST.IsRecord(tt, flags) then
                let fields =
                    FST.GetRecordFields(tt, flags)
                    |> Array.map (fun f ->
                        JI.GetName f, f, f.PropertyType)
                ((fun es ->
                    let es, tts = List.unzip es
                    let tn = 
                        match comp.TryLookupClassInfo t.TypeDefinition |> Option.bind (fun cls -> cls.Address) with
                        | Some a -> GlobalAccess a
                        | _ -> failwithf "Cannot look up type address for: %s" t.AssemblyQualifiedName 
                    ok (call "Record" [tn; NewArray es])
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
                        let tt = (Reflection.getType t)
                        encode tt >>= fun e ->
                        k ((NewArray [cString n; e; optionKind], tt) :: es))
                <| []
            elif FST.IsUnion(tt, flags) then
                let discr, cases = JI.GetUnionEncoding tt
                ((0, fun cases ->
                    let cases = NewArray cases
                    let discr =
                        match discr with
                        | JI.NoField discrFields ->
                            discrFields
                            |> List.map (fun (name, id) -> name, cInt id)
                            |> Object
                        | JI.StandardField -> cString "$"
                        | JI.NamedField n -> cString n
                    let tn =
                        match comp.TryLookupClassInfo t.TypeDefinition |> Option.bind (fun cls -> cls.Address) with
                        | Some a -> GlobalAccess a
                        | _ -> failwithf "Cannot look up type address for: %s" t.AssemblyQualifiedName 
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
                                k (NewArray [tag; NewArray argNames] :: es)
                                ), argNames)
                            ||> Array.fold (fun (j, k) (argName, argT, argFlags) ->
                                if argFlags |> Array.exists (function JI.DateTimeFormat _ -> true) then
                                    warn (sprintf "Warning: This union case field has a custom DateTime format: %s.%s [%s]. \
                                        Client-side JSON serialization does not support custom DateTime formatting. \
                                        This field will be serialized using ISO format."
                                        tt.FullName caseName argName)
                                let argT, optionKind =
                                    if argT.IsGenericType && argT.GetGenericTypeDefinition() = typedefof<option<_>> then
                                        argT.GetGenericArguments().[0], cInt (int OptionalFieldKind.NormalOption)
                                    else argT, cInt (int OptionalFieldKind.NotOption)
                                j + 1, fun es ->
                                    encode (Reflection.getType argT) >>= fun e ->
                                    k (NewArray [cString ("$" + string j); cString argName; e; optionKind] :: es))
                            |> snd
                            <| []
                        | JI.InlineRecord(name, record) ->
                            let tag =
                                match discr with
                                | JI.StandardField -> cInt i
                                | _ -> cString name
                            encode (Reflection.getType record) >>= fun e ->
                            k (NewArray [tag; NewArray [NewArray [!~Null; !~Null; e]]] :: es)
                        | JI.Constant _ -> k (!~Null :: es)
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
                let trI =
                    { new Transformer() with
                        override this.TransformVar id = 
                            let repl =
                                ctx
                                |> Array.ofSeq
                                |> Array.tryPick (fun (KeyValue(k, (id', e, _))) ->
                                    if id = id' then
                                        ctx.Remove(k) |> ignore
                                        Some e
                                    else None)    
                            match repl with
                            | Some e -> e
                            | _ -> Var id
                    }
                let rec sub x =
                    let res = trI.TransformExpression x 
                    if ctx.Count = 0 then res else sub res
                sub x
            else
                LetRec(
                    let fld = !~(String "x")
                    [for KeyValue(_, (id, e, multiple)) in ctx do
                        let xid = Id.New()
                        // xid = {}
                        yield xid, Object []
                        // id = function() { if (!xid.x) { xid.x = e() }; return xid.x; }
                        yield id, Lambda([],
                            Sequential [
                                Conditional(
                                    Unary(UnaryOperator.``!``, ItemGet(Var xid, fld)),
                                    ItemSet(Var xid, fld, Application(e, [])),
                                    !~Null);
                                ItemGet(Var xid, fld)])
                    ], x)
        | Choice2Of2 msg -> failwithf "%A: %s" t msg

module Macro =

//    module J = WebSharper.Core.JavaScript.Core
//    module M = WebSharper.Core.Macros
//    module Q = WebSharper.Core.Quotations
    open WebSharper.Core
    open WebSharper.Core.AST
    open MacroInternals

    let private encodeLambda name warn comp t =
        Application(getEncoding name cCallE warn comp t, [])

    let private encode name warn comp t arg =
        Application(encodeLambda name warn comp t, [arg])

    let Encode warn t arg =
        // ENCODE()(arg)
        encode "Encode" warn t arg

    let EncodeLambda warn comp t =
        // ENCODE()
        encodeLambda "EncodeLambda" warn comp t

    let Serialize warn comp t arg =
        // JSON.stringify(ENCODE()(arg))
        cCallG ["JSON"] "stringify" [encode "Serialize" warn comp t arg]

    let SerializeLambda warn comp t =
        let enc = Id.New()
        let arg = Id.New()
        // let enc = ENCODE() in fun arg -> JSON.stringify(enc(arg))
        Let(enc, encodeLambda "SerializeLambda" warn comp t,
            Lambda([arg],
                cCallG ["JSON"] "stringify" [Application(Var enc, [Var arg])]))

    let private decodeLambda name warn comp t =
        Application(getEncoding name cCallD warn comp t, [])

    let private decode name warn comp t arg =
        Application(decodeLambda name warn comp t, [arg])

    let Decode warn comp t arg =
        // DECODE()(arg)
        decode "Decode" warn comp t arg

    let DecodeLambda warn comp t =
        // DECODE()
        decodeLambda "DecodeLambda" warn comp t

    let Deserialize warn comp t arg =
        // DECODE()(JSON.parse(arg))
        decode "Deserialize" warn comp t (cCallG ["JSON"] "parse" [arg])

    let DeserializeLambda warn comp t =
        // let dec = DECODE() in fun arg -> dec(JSON.parse(arg))
        let dec = Id.New()
        let arg = Id.New()
        Let(dec, decodeLambda "DeserializeLambda" warn comp t,
            Lambda([arg],
                Application(Var dec, [cCallG ["JSON"] "parse" [Var arg]])))

    module M = WebSharper.Core.Metadata

    type SerializeMacro(comp : M.Compilation) =
        inherit WebSharper.Core.Macro()
        override this.TranslateCall(_,_,m,a,_) =
            let warn msg = comp.AddWarning(None, M.SourceWarning msg)
            match a with
            | [x] ->
                (match m.Entity.Value.MethodName with
                | "Encode" -> Encode
                | "Decode" -> Decode
                | "Serialize" -> Serialize
                | "Deserialize" -> Deserialize
                | _ -> failwith "Invalid macro invocation")
                    warn comp m.Generics.Head x |> MacroOk
            | _ -> failwith "Invalid macro invocation"

open Macro

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
