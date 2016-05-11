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

type OptionalFieldKind =
    /// The field doesn't have type option<'T>
    | NotOption = 0
    /// The field has type option<'T>
    | NormalOption = 1
    /// The field has type option<'T> and is marked [<OptionalField>]
    | MarkedOption = 2

[<JavaScript>]
type Provider() =

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
            if JS.TypeOf x ===. JS.Object && x !=. null then
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

    module M = WebSharper.Core.Metadata
    open WebSharper.Core.AST
    module JI = WebSharper.Core.Json.Internal
    type private BF = System.Reflection.BindingFlags

    type Parameters =
        {
            Warn : string -> unit
            Provider : Expression
        }

    [<AutoOpen>]
    module private Internals =

        let cString s = !~ (Literal.String s)
        let inline cInt i = !~ (Int i)

        let mainJsonModule =
            TypeDefinition {
                FullName = "WebSharper.Json"
                Assembly = "WebSharper.Main"
            }
        let mJson (comp: M.ICompilation) f args =
            let m = comp.GetClassInfo(mainJsonModule).Value.Methods.Keys |> Seq.find (fun m -> m.Value.MethodName = f)
            Call(None, NonGeneric mainJsonModule, NonGeneric m, args)

        let (|T|) (t: TypeDefinition) = t.Value.FullName
        let (|C|_|) (t: Type) =
            match t with 
            | ConcreteType { Entity = e; Generics = g} -> Some (e, g)
            | _ -> None

        let providerType = 
            TypeDefinition {
                FullName = "WebSharper.Json+Provider"
                Assembly = "WebSharper.Web"
            }
        let invoke (comp: M.ICompilation) x isEnc n args = 
            let f = (if isEnc then "Encode" else "Decode") + n
            let m = comp.GetClassInfo(providerType).Value.Methods.Keys |> Seq.find (fun m -> m.Value.MethodName = f)
            Call(Some x, NonGeneric providerType, NonGeneric m, args)

        type EncodeResult = Choice<Expression, string>

        let (>>=) (m as x: EncodeResult) (f: Expression -> EncodeResult) =
            match m with
            | Choice1Of2 e -> f e
            | Choice2Of2 _ -> x
        let ok x = Choice1Of2 x : EncodeResult
        let fail x = Choice2Of2 x : EncodeResult

        let ident = 
            let x = Id.New()
            Lambda([], Lambda ([x], Var x))

        let getEncoding name isEnc param warn (comp: M.ICompilation) (t: Type) =
            let ctx = System.Collections.Generic.Dictionary()
            let call = invoke comp param.Provider isEnc
            let rec encode t =
                match t with
                | ArrayType (t, 1) ->
                    encode t >>= fun e ->
                    ok (call "Array" [e])
                | ArrayType _ ->
                    fail "JSON serialization for multidimensional arrays is not supported."
                | VoidType
                | C (T ("Microsoft.FSharp.Core.Unit"
                                |"System.Boolean"
                                |"System.SByte" | "System.Byte"
                                |"System.Int16" | "System.UInt16"
                                |"System.Int32" | "System.UInt32"
                                |"System.Int64" | "System.UInt64"
                                |"System.Single"| "System.Double"
                                |"System.Decimal"
                                |"System.String"| "System.Guid"
                                |"WebSharper.Core.Json+Encoded"), []) ->
                    ok ident
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
                | ConcreteType _ -> failwith "impossible"
                | FSharpFuncType _ -> 
                    fail (name + ": Cannot de/serialize a function value.")
                | ByRefType _ ->
                    fail (name + ": Cannot de/serialize a byref value.")
                | TypeParameter _ ->
                    fail (name + ": Cannot de/serialize a generic value. You must call this function with a concrete type.")
            // Encode a type that might be recursively defined
            and encRecType t targs args =
                match comp.GetCustomTypeInfo t.TypeDefinition with
                | M.EnumInfo _ -> ok ident
                | M.FSharpRecordInfo fields ->
                    ((fun es ->
                        let es, tts = List.unzip es
                        let tn = 
                            match comp.GetClassInfo t.TypeDefinition |> Option.bind (fun cls -> cls.Address) with
                            | Some a -> GlobalAccess a
                            | _ -> failwithf "Cannot look up type address for: %s" t.AssemblyQualifiedName 
                        ok (call "Record" [tn; NewArray es])
                        ), fields)
                    ||> List.fold (fun k f es -> // f                        
                            if Option.isSome f.DateTimeFormat then
                                warn (sprintf "Warning: This record field has a custom DateTime format: %s.%s. \
                                    Client-side JSON serialization does not support custom DateTime formatting. \
                                    This field will be serialized using ISO format."
                                    f.RecordFieldType.TypeDefinition.Value.FullName f.Name)
                            let t, optionKind =
                                match f.RecordFieldType with
                                | ConcreteType { Entity = d; Generics = [p] } when d.Value.FullName = "Microsoft.FSharp.Core.FSharpOption`1" ->
                                    let kind =
                                        if f.Optional then
                                            OptionalFieldKind.MarkedOption    
                                        else OptionalFieldKind.NormalOption 
                                    p, cInt (int kind)
                                | t ->    
                                    t, cInt (int OptionalFieldKind.NotOption)
                            encode (t.SubstituteGenerics (Array.ofList targs)) >>= fun e ->
                            k ((NewArray [cString f.Name; e; optionKind], t) :: es))
                    <| []
                // TODO: handle nested case type (possible when using from C#)
                | M.FSharpUnionInfo u ->
                    let tryGetInlinableRecordInfo (uci: M.FSharpUnionCaseInfo) =
                        match uci.Kind with 
                        | M.NormalFSharpUnionCase [f] when f.Name = "Item" ->
                            let rec inl uft =
                                match uft with
                                | ConcreteType { Entity = fTd } as ft ->
                                    match comp.GetCustomTypeInfo fTd with
                                    | M.FSharpRecordInfo fRec -> Some (ft, fRec)
                                    | _ -> None
                                | TypeParameter i -> inl (targs.[i])
                                | _ -> None
                            inl f.UnionFieldType
                        | _ -> None
                    let discr =
                        match u.NamedUnionCases with
                        | None -> JI.StandardField
                        | Some None -> 
                            let allCases =
                                u.Cases |> Seq.mapi (fun i uci ->
                                    i,
                                    match tryGetInlinableRecordInfo uci with
                                    | Some (_, fRec) ->
                                        fRec |> Seq.filter (fun rf ->
                                            match rf.RecordFieldType with
                                            | ConcreteType { Entity = e } when 
                                                e.Value.FullName = "Microsoft.FSharp.Core.FSharpOption`1" -> false
                                            | _ -> true
                                        )
                                        |> Seq.map (fun rf -> rf.Name) |> Set.ofSeq
                                    | _ -> 
                                        match uci.Kind with 
                                        | M.NormalFSharpUnionCase fs ->
                                            fs |> Seq.map ( fun f -> f.Name) |> Set.ofSeq
                                        | _ -> Set.empty
                                )
                                |> Map.ofSeq
                            let findDistinguishingCase (cases: Map<int, Set<string>>) =
                                cases
                                |> Map.tryPick (fun t fs ->
                                    let allOtherFields =
                                        allCases
                                        |> Seq.choose (fun (KeyValue(t', fs)) ->
                                            if t = t' then None else Some fs)
                                        |> Set.unionMany
                                    let uniqueCases = fs - allOtherFields
                                    if Set.isEmpty uniqueCases then
                                        None
                                    else Some (Seq.head uniqueCases, t)
                                )
                            let rec buildTable acc cases =
                                if Map.isEmpty cases then acc else
                                match findDistinguishingCase cases with
                                | None -> failwithf "No decoder for %s" t.AssemblyQualifiedName
                                | Some (name, tag) ->
                                    buildTable
                                        <| (name, tag) :: acc
                                        <| Map.remove tag cases
                            buildTable [] allCases |> JI.NoField
                        | Some (Some n) -> JI.NamedField n
                    let cases = u.Cases
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
                            match comp.GetClassInfo t.TypeDefinition |> Option.bind (fun cls -> cls.Address) with
                            | Some a -> GlobalAccess a
                            | _ -> failwithf "Cannot look up type address for: %s" t.AssemblyQualifiedName 
                        ok (call "Union" [tn; discr; cases])
                        ), cases)
                    ||> List.fold (fun (i, k) case ->
                        i + 1, fun es ->
                            match tryGetInlinableRecordInfo case with
                            | Some (ft, _) -> 
                                let tag =
                                    match discr with
                                    | JI.StandardField -> cInt i
                                    | _ -> cString (match case.JsonName with Some n -> n | _ -> case.Name)
                                encode ft >>= fun e ->
                                k (NewArray [tag; NewArray [NewArray [!~Null; !~Null; e]]] :: es)
                            | _ ->
                            match case.Kind with
                            | M.NormalFSharpUnionCase fields ->
                                ((0, fun argNames ->
                                    let tag =
                                        match u.NamedUnionCases with
                                        | None -> cInt i
                                        | _ -> cString (match case.JsonName with Some n -> n | _ -> case.Name)
                                    k (NewArray [tag; NewArray argNames] :: es)
                                    ), fields)
                                ||> List.fold (fun (j, k) f -> //(argName, argT, argFlags) ->
                                    if Option.isSome f.DateTimeFormat then
                                        warn (sprintf "Warning: This union case field has a custom DateTime format: %s.%s [%s]. \
                                            Client-side JSON serialization does not support custom DateTime formatting. \
                                            This field will be serialized using ISO format."
                                            f.UnionFieldType.TypeDefinition.Value.FullName case.Name f.Name)
                                    let argT, optionKind =
                                        match f.UnionFieldType with
                                        | ConcreteType { Entity = d; Generics = [p] } when d.Value.FullName = "Microsoft.FSharp.Core.FSharpOption`1" ->
                                            p, cInt (int OptionalFieldKind.NormalOption)
                                        | t ->    
                                            t, cInt (int OptionalFieldKind.NotOption)
                                    j + 1, fun es ->
                                        encode (argT.SubstituteGenerics (Array.ofList targs)) >>= fun e ->
                                        k (NewArray [cString ("$" + string j); cString f.Name; e; optionKind] :: es))
                                |> snd
                                <| []
                            | M.ConstantFSharpUnionCase _ -> k (!~Null :: es)
                    )
                    |> snd
                    <| []
                | _ -> 
                    fail (name + ": Type not supported: " + t.TypeDefinition.Value.FullName)
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

        let encodeLambda name param warn comp t =
            Application(getEncoding name true param warn comp t, [])

        let encode name param warn comp t arg =
            Application(encodeLambda name param warn comp t, [arg])

        let decodeLambda name param warn comp t =
            Application(getEncoding name false param warn comp t, [])

        let decode name param warn comp t arg =
            Application(decodeLambda name param warn comp t, [arg])

    type Parameters with

        static member Default =
            {
                Warn = ignore
                Provider =
                    let prTyp = typeof<Provider>
                    Call(
                        None, 
                        NonGeneric (Reflection.ReadTypeDefinition prTyp),
                        NonGeneric (Reflection.ReadMethod (prTyp.GetProperty("Default", BF.Static ||| BF.Public).GetGetMethod())),
                        []
                    )
            }

    let Encode param warn comp t arg =
        // ENCODE()(arg)
        encode "Encode" param warn comp t arg

    let EncodeLambda param warn t =
        // ENCODE()
        encodeLambda "EncodeLambda" param warn t

    let Serialize param warn comp t arg =
        // JSON.stringify(ENCODE()(arg))
        mJson comp "Stringify" [encode "Serialize" param warn comp t arg]

    let SerializeLambda param warn comp t =
        let enc = Id.New()
        let arg = Id.New()
        // let enc = ENCODE() in fun arg -> JSON.stringify(enc(arg))
        Let(enc, encodeLambda "SerializeLambda" param warn comp t,
            Lambda([arg],
                mJson comp "Stringify" [Application(Var enc, [Var arg])]))

    let Decode param warn comp t arg =
        // DECODE()(arg)
        decode "Decode" param warn comp t arg

    let DecodeLambda param warn comp t =
        // DECODE()
        decodeLambda "DecodeLambda" param warn comp t

    let Deserialize param warn comp t arg =
        // DECODE()(JSON.parse(arg))
        decode "Deserialize" param warn comp t (mJson comp "Parse" [arg])

    let DeserializeLambda param warn comp t =
        // let dec = DECODE() in fun arg -> dec(JSON.parse(arg))
        let dec = Id.New()
        let arg = Id.New()
        Let(dec, decodeLambda "DeserializeLambda" param warn comp t,
            Lambda([arg],
                Application(Var dec, [mJson comp "Parse" [Var arg]])))

    type SerializeMacro() =
        inherit WebSharper.Core.Macro()

        static let rec last = function
            | [x] -> x
            | x :: l -> last l
            | _ -> failwith ""

        override this.TranslateCall(c) =
            let warning = ref None
            let warn msg = 
                warning := Some msg
            let param = Parameters.Default
            let f, provider =
                match c.Method.Entity.Value.MethodName with
                | "Encode" -> Encode, param.Provider
                | "Decode" -> Decode, param.Provider
                | "Serialize" -> Serialize, param.Provider
                | "Deserialize" -> Deserialize, param.Provider
                | "EncodeWith" -> Encode, List.head c.Arguments
                | "DecodeWith" -> Decode, List.head c.Arguments
                | "SerializeWith" -> Serialize, List.head c.Arguments
                | "DeserializeWith" -> Deserialize, List.head c.Arguments
                | _ -> failwith "Invalid macro invocation"
            let id = Id.New()
            let res =
                Let(id, provider, f {param with Provider = Var id} warn c.Compilation c.Method.Generics.Head (last c.Arguments))
                |> WebSharper.Core.MacroOk
            match !warning with
            | None -> res
            | Some msg -> WebSharper.Core.MacroWarning(msg, res)

open Macro

/// Encodes an object in such a way that JSON stringification
/// results in the same readable format as Sitelets.
/// Client-side only.
[<Macro(typeof<SerializeMacro>)>]
let Encode<'T> (x: 'T) = X<obj>

/// Encodes an object in such a way that JSON stringification
/// results in the same readable format as Sitelets.
/// Client-side only.
[<Macro(typeof<SerializeMacro>)>]
let EncodeWith<'T> (provider: Provider) (x: 'T) = X<obj>

/// Serializes an object to JSON using the same readable format as Sitelets.
/// For plain JSON stringification, see Json.Stringify.
[<Macro(typeof<SerializeMacro>)>]
let Serialize<'T> (x: 'T) =
    Web.Shared.PlainJson.GetEncoder<'T>().Encode x
    |> Web.Shared.PlainJson.Pack
    |> Core.Json.Stringify

/// Serializes an object to JSON using the same readable format as Sitelets.
/// For plain JSON stringification, see Json.Stringify.
/// Client-side only.
[<Macro(typeof<SerializeMacro>)>]
let SerializeWith<'T> (provider: Provider) (x: 'T) = X<string>

/// Decodes an object parsed from the same readable JSON format as Sitelets.
/// Client-side only.
[<Macro(typeof<SerializeMacro>)>]
let Decode<'T> (x: obj) = X<'T>

/// Decodes an object parsed from the same readable JSON format as Sitelets.
/// Client-side only.
[<Macro(typeof<SerializeMacro>)>]
let DecodeWith<'T> (provider: Provider) (x: obj) = X<'T>

/// Deserializes a JSON string using the same readable format as Sitelets.
/// For plain JSON parsing, see Json.Parse.
[<Macro(typeof<SerializeMacro>)>]
let Deserialize<'T> (x: string) =
    Core.Json.Parse x
    |> Web.Shared.PlainJson.GetDecoder<'T>().Decode

/// Deserializes a JSON string using the same readable format as Sitelets.
/// For plain JSON parsing, see Json.Parse.
/// Client-side only.
[<Macro(typeof<SerializeMacro>)>]
let DeserializeWith<'T> (provider: Provider) (x: string) = X<'T>

/// Test the shape of a JSON encoded value.
/// Client-side only.
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