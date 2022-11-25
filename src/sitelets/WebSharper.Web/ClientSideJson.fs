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

module internal WebSharper.ClientSideJson

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
    /// The field has type Optional<'T>
    | ErasedOption = 3

[<JavaScript>]
module Provider =
    let Id () = 
        ()
        fun () -> id

    let EncodeTuple (encs: (unit -> obj -> obj)[]) : (unit -> obj[] -> obj) =
        ()
        fun () args ->
            box (Array.map2 (fun f x -> f () x) encs args)

    let EncodeDateTime () =
        ()
        fun () (x: System.DateTime) ->
            box (x.JS.ToISOString())

    let EncodeDateTimeOffset () =
        ()
        fun () (x: System.DateTimeOffset) ->
            box (New [ "d" => x.DateTime.JS.ToISOString(); "o" => x?o ])

    let EncodeList (encEl: unit -> 'T -> obj) : (unit -> list<'T> -> obj) =
        ()
        fun () (l: list<'T>) ->
            let a : obj[] = [||]
            let e = encEl()
            l |> List.iter (fun x -> a.JS.Push (e x) |> ignore)
            box a

    let EncodeRecord (_: obj) (fields: (string * (unit -> obj -> obj) * OptionalFieldKind)[]) : (unit -> 'T -> obj) =
        ()
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
                | OptionalFieldKind.ErasedOption ->
                    if x?(name) ===. JS.Undefined then
                        o?(name) <- enc () x?(name)
                | _ -> failwith "Invalid field option kind")
            o

    let EncodeUnion (_: obj) (discr: obj) (cases: (string * (string * string * (unit -> obj -> obj) * OptionalFieldKind)[])[]) : (unit -> 'T -> obj) =
        ()
        fun () x ->
            if JS.TypeOf x ===. JS.Object && x !=. null then
                let o = New []
                let tag = x?("$")
                let tagName, fields = cases.[tag]
                if JS.TypeOf discr = JS.Kind.String then o?(As<string> discr) <- tagName
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

    let EncodeArray (encEl: (unit -> 'T -> obj)) : (unit -> 'T[] -> obj) =
        ()
        fun () (a: 'T[]) ->
            let e = encEl()
            box (Array.map e a)

    let EncodeSet (encEl: (unit -> 'T -> obj)) : (unit -> Set<'T> -> obj) =
        ()
        fun () (s: Set<'T>) ->
            let a : obj[] = [||]
            let e = encEl()
            s |> Set.iter (fun x -> a.JS.Push (e x) |> ignore)
            box a

    let EncodeStringMap (encEl:(unit -> 'T -> obj)) : (unit -> Map<string, 'T> -> obj) =
        ()
        fun () (m: Map<string, 'T>) ->
            let o = New []
            let e = encEl()
            m |> Map.iter (fun k v -> o?(k) <- e v)
            o

    let EncodeArrayMap (encKey:(unit -> 'K -> obj)) (encEl:(unit -> 'V -> obj)) : (unit -> Map<'K, 'V> -> obj) =
        ()
        fun () (m: Map<'K, 'V>) ->
            let a : obj[][] = [||]
            let k = encKey()
            let e = encEl()
            m |> Map.iter (fun key el -> a.JS.Push([| [| k key; e el |] |]) |> ignore)
            box a

    let EncodeStringDictionary (encEl:(unit -> 'T -> obj)) : (unit -> Dictionary<string, 'T> -> obj) =
        ()
        fun () (d: Dictionary<string, 'T>) ->
            let o = New []
            let e = encEl()
            for KeyValue(k, v) in d :> seq<_> do o?(k) <- e v
            o

    let EncodeArrayDictionary (encKey: (unit -> 'K -> obj)) (encEl: (unit -> 'V -> obj)) : (unit -> Dictionary<'K, 'V> -> obj) =
        ()
        fun () (d: Dictionary<'K, 'V>) ->
            let a : obj[][] = [||]
            let k = encKey()
            let e = encEl()
            for KeyValue(key, el) in d do a.JS.Push([| [| k key; e el |] |]) |> ignore
            box a

    let EncodeLinkedList (encEl:(unit -> 'T -> obj)) : (unit -> LinkedList<'T> -> obj) =
        ()
        fun () (d: LinkedList<'T>) ->
            let o = Array<obj>()
            let e = encEl()
            for x in d :> seq<'T> do o.Push(e x) |> ignore
            box o

    let DecodeTuple (decs: (unit -> obj -> obj)[]) : (unit -> obj -> obj[]) =
        As (EncodeTuple decs)

    let DecodeDateTime() =
        ()
        fun () (x: obj) ->
            if JS.HasOwnProperty x "d" then
                Date(x?d: string).Self
            else 
                Date(x :?> string).Self

    let DecodeDateTimeOffset() =
        ()
        fun () (x: obj) ->
            if JS.HasOwnProperty x "d" then
                System.DateTimeOffset(Date(x?d: string).Self, System.TimeSpan.FromMinutes x?o)
            else 
                System.DateTimeOffset(Date(x :?> string).Self, System.TimeSpan.Zero)

    let DecodeList (decEl: (unit -> obj -> 'T)) : (unit -> obj -> list<'T>) =
        ()
        fun () (a : obj) ->
            let e = decEl()
            List.init (a :?> obj[]).Length (fun i -> e (a :?> obj[]).[i])

    let DecodeSet (decEl: unit -> obj -> 'T) : (unit -> obj -> Set<'T>) =
        ()
        fun () (a : obj) ->
            let e = decEl()
            Set.ofArray(Array.map e (a :?> obj[]))

    let DecodeRecord (t: obj) (fields: (string * (unit -> obj -> obj) * OptionalFieldKind)[]) : (unit -> obj -> 'T) =
        ()
        fun () (x: obj) ->
            let o = if t ===. JS.Undefined then New [] else JS.New t
            fields |> Array.iter (fun (name, dec, kind) ->
                match kind with
                | OptionalFieldKind.NotOption ->
                    if JS.HasOwnProperty x name then
                        o?(name) <- dec () x?(name)
                    else failwith ("Missing mandatory field: " + name)
                | OptionalFieldKind.NormalOption ->
                    o?(name) <-
                        if JS.HasOwnProperty x name
                        then Some (dec () x?(name))
                        else None
                | OptionalFieldKind.MarkedOption ->
                    if JS.HasOwnProperty x name then
                        o?(name) <- (dec () x?(name))
                | OptionalFieldKind.ErasedOption ->
                    if x?(name) ===. JS.Undefined then
                        o?(name) <- (dec () x?(name))
                | _ -> failwith "Invalid field option kind")
            o

    let DecodeUnion (t: obj) (discr: obj) (cases: (string * (string * string * (unit -> obj -> obj) * OptionalFieldKind)[])[]) : (unit -> obj -> 'T) =
        ()
        fun () (x: obj) ->
            if JS.TypeOf x ===. JS.Object && x !=. null then
                let o = if t ===. JS.Undefined then New [] else JS.New t
                let tag =
                    // [<NamedUnionCases(discr)>]
                    if JS.TypeOf discr ===. JS.Kind.String then
                        let tagName = x?(As<string> discr)
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
                        let r = dec () x
                        // eliminate tag field if record deserializer is identity
                        if As<bool> ``to`` then 
                            JS.Delete r (As<string> discr)
                        o?("$0") <- r
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

    let DecodeArray (decEl :(unit -> obj -> 'T)) : (unit -> obj -> 'T[]) =
        As (EncodeArray (As decEl))

    let DecodeStringMap (decEl :(unit -> obj -> 'T)) : (unit -> obj -> Map<string, 'T>) =
        ()
        fun () (o: obj) ->
            let m = ref Map.empty
            let decEl = decEl ()
            JS.ForEach o (fun k -> m := Map.add k (decEl o?(k)) !m; false)
            !m

    let DecodeArrayMap (decKey :(unit -> obj -> 'K)) (decEl :(unit -> obj -> 'V)) : (unit -> obj -> Map<'K, 'V>) =
        ()
        fun () (o: obj) ->
            let decKey = decKey()
            let decEl = decEl()
            let mutable m = Map<'K, 'V> []
            for k, v in o :?> (obj * obj)[] do m <- Map.add (decKey k) (decEl v) m
            m

    let DecodeStringDictionary (decEl: unit -> obj -> 'T) : (unit -> obj -> Dictionary<string, 'T>) =
        ()
        fun () (o: obj) ->
            let d = System.Collections.Generic.Dictionary()
            let decEl = decEl ()
            JS.ForEach o (fun k -> d.Add(k, decEl o?(k)); false)
            d

    let DecodeArrayDictionary (decKey :(unit -> obj -> 'K)) (decEl :(unit -> obj -> 'V)) : (unit -> obj -> Dictionary<'K, 'V>) =
        ()
        fun () (o: obj) ->
            let decKey = decKey()
            let decEl = decEl()
            let d = Dictionary<'K, 'V>()
            for k, v in o :?> (obj * obj)[] do d.Add(decKey k, decEl v)
            d

    let DecodeLinkedList (decEl: unit -> obj -> 'T) : (unit -> obj -> LinkedList<'T>) =
        ()
        fun () (o: obj) ->
            let l = LinkedList<'T>()
            let decEl = decEl()
            for x in o :?> obj[] do l.AddLast(decEl x) |> ignore
            l

module Macro =

    module M = WebSharper.Core.Metadata
    open WebSharper.Core.AST
    module JI = WebSharper.Core.Json.Internal
    type private BF = System.Reflection.BindingFlags

    type Parameters =
        {
            Warnings : ResizeArray<string>
            Dependencies : ResizeArray<M.Node>
            Compilation : M.ICompilation
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
                FullName = "WebSharper.ClientSideJson+Provider"
                Assembly = "WebSharper.Web"
            }
        let invoke (comp: M.ICompilation) isEnc n args = 
            let f = (if isEnc then "Encode" else "Decode") + n
            let m = comp.GetClassInfo(providerType).Value.Methods.Keys |> Seq.find (fun m -> m.Value.MethodName = f)
            Call(None, NonGeneric providerType, NonGeneric m, args)
        let invokeId (comp: M.ICompilation) = 
            let m = comp.GetClassInfo(providerType).Value.Methods.Keys |> Seq.find (fun m -> m.Value.MethodName = "Id")
            Call(None, NonGeneric providerType, NonGeneric m, [])

        type EncodeResult = Choice<Expression, string, Type>

        let (>>=) (x: EncodeResult) (f: Expression -> EncodeResult) =
            match x with
            | Choice1Of3 e -> f e
            | _ -> x
        let ok x = Choice1Of3 x : EncodeResult
        let fail x = Choice2Of3 x : EncodeResult
        let generic t = Choice3Of3 t : EncodeResult

        let mapOk f x =
            match x with
            | Choice1Of3 x -> Choice1Of3 (f x) 
            | _ -> x

        /// Returns None if MacroNeedsResolvedTypeArg.
        let getEncoding name isEnc param (t: Type) : EncodeResult =
            let warn msg = param.Warnings.Add msg
            let addTypeDep td = param.Dependencies.Add (M.TypeNode td)
            let comp = param.Compilation
            let call = invoke comp isEnc
            let ident = invokeId comp 
            let isIdent r =
                match r with 
                | Choice1Of3 e when obj.ReferenceEquals(e, ident) -> true
                | _ -> false
            let rec encode t =
                match t with
                | ArrayType (t, 1)
                | C (T "System.Collections.Generic.List`1", [t])
                | C (T "System.Collections.Generic.Queue`1", [t])
                | C (T "System.Collections.Generic.Stack`1", [t]) ->
                    encode t >>= fun e ->
                    ok (call "Array" [e])
                | ArrayType _ ->
                    fail "JSON serialization for multidimensional arrays is not supported."
                | TSType (TSType.ArrayOf t) ->
                    encode (TSType t) >>= fun e ->
                    ok (call "Array" [e])
                | VoidType
                | TSType (TSType.Number | TSType.String | TSType.Null | TSType.Void)
                | C (T ("Microsoft.FSharp.Core.Unit"
                            | "System.Boolean"
                            | "System.SByte"   | "System.Byte"
                            | "System.Int16"   | "System.UInt16"
                            | "System.Int32"   | "System.UInt32"
                            | "System.Int64"   | "System.UInt64"
                            | "System.Single"  | "System.Double"
                            | "System.String"  | "System.Guid"
                            | "WebSharper.Core.Json+Encoded"), []) ->
                    ok ident
                | C (T "System.Object", []) ->
                    if isEnc then 
                        fail "JSON serialization for System.Object is not supported; only deserialization." 
                    else ok ident
                | C (T "Microsoft.FSharp.Collections.FSharpList`1", [t]) ->
                    encode t >>= fun e ->
                    ok (call "List" [e])
                | C (T "Microsoft.FSharp.Collections.FSharpSet`1", [t]) ->
                    encode t >>= fun e ->
                    ok (call "Set" [e])
                | C (T "Microsoft.FSharp.Collections.FSharpMap`2", [k; t]) ->
                    match k with
                    | C (T "System.String", []) ->
                        encode t >>= fun e -> 
                        ok (call "StringMap" [e])
                    | _ ->
                        encode k >>= fun k -> 
                        encode t >>= fun e -> 
                        ok (call "ArrayMap" [k; e])
                | C (T "System.Collections.Generic.Dictionary`2", [k; t]) ->
                    match k with
                    | C (T "System.String", []) ->
                        encode t >>= fun e ->
                        ok (call "StringDictionary" [e])
                    | _ ->
                        encode k >>= fun k ->
                        encode t >>= fun e ->
                        ok (call "ArrayDictionary" [k; e])
                | C (T "System.Collections.Generic.LinkedList`1", [t]) ->
                    encode t >>= fun e ->
                    ok (call "LinkedList" [e])
                | TupleType (ts, _) ->
                    ((fun es -> ok (call "Tuple" [NewArray es])), ts)
                    ||> List.fold (fun k t ->
                        fun es -> encode t >>= fun e -> k (e :: es))
                    <| []
                | TSType (TSType.Tuple ts) ->
                    encodeTuple (List.map TSType ts)
                | C (T "System.DateTime", []) ->
                    ok (call "DateTime" [])
                | C (T "System.DateTimeOffset", []) ->
                    ok (call "DateTimeOffset" [])
                | C (td, args) ->                    
                    let top = comp.AssemblyName.Replace(".","$") + if isEnc then "_JsonEncoder" else "_JsonDecoder"
                    let key = M.CompositeEntry [ M.StringEntry top; M.TypeEntry t ]
                    match comp.GetMetadataEntries key with                    
                    | M.StringEntry "id" :: _ ->
                        ok ident
                    | M.CompositeEntry [ M.TypeDefinitionEntry gtd; M.MethodEntry gm ] :: _ ->
                        Lambda([], None, Call(None, NonGeneric gtd, NonGeneric gm, [])) |> ok
                    | _ ->
                        let gtd, gm, _ = comp.NewGenerated("j")
                        comp.AddMetadataEntry(key, M.CompositeEntry [ M.TypeDefinitionEntry gtd; M.MethodEntry gm ])
                        ((fun es ->
                            let enc = encRecType t args es
                            if isIdent enc then
                                comp.AddMetadataEntry(key, M.StringEntry "id")
                                comp.AddGeneratedInline(gm, ident)
                                enc
                            else
                                enc >>= fun e ->
                                let gv = comp.NewGeneratedVar("v")
                                let v = Var gv
                                let b = Lambda ([], None, Conditional(v, v, VarSet(gv, Appl(e, [], NonPure, Some 0))))
                                comp.AddGeneratedCode(gm, b)
                                Lambda([], None, Call(None, NonGeneric gtd, NonGeneric gm, [])) |> ok
                         ), args)
                        ||> List.fold (fun k t es ->
                            encode t >>= fun e -> k ((t, e) :: es))
                        <| []
                | ConcreteType _ -> failwith "impossible"
                | FSharpFuncType _ -> 
                    fail (name + ": Cannot de/serialize a function value.")
                | ByRefType _ ->
                    fail (name + ": Cannot de/serialize a byref value.")
                | TSType _ ->
                    fail (name + ": Cannot de/serialize this type: unknown [<Type>].")
                | LocalTypeParameter
                | StaticTypeParameter _ 
                | TypeParameter _ ->
                    generic t
            and encodeTuple ts =
                ((fun es -> ok (call "Tuple" [NewArray es])), ts)
                ||> List.fold (fun k t ->
                    fun es -> encode t >>= fun e -> k (e :: es))
                <| []
            // Encode a type that might be recursively defined
            and encRecType t targs args =
                let td = t.TypeDefinition
                match comp.GetCustomTypeInfo td with
                | M.EnumInfo _ -> ok ident
                | M.FSharpRecordInfo fields ->
                    let fieldEncoders =
                        fields
                        |> List.map (fun f ->
                            if Option.isSome f.DateTimeFormat then
                                warn (sprintf "Warning: This record field has a custom DateTime format: %s.%s. \
                                    Client-side JSON serialization does not support custom DateTime formatting. \
                                    This field will be serialized using ISO format."
                                    f.RecordFieldType.TypeDefinition.Value.FullName f.Name)
                            let t, optionKind =
                                match f.RecordFieldType with
                                | ConcreteType { Entity = d; Generics = [p] } when d.Value.FullName = "Microsoft.FSharp.Core.FSharpOption`1" ->
                                    if f.Optional then p, OptionalFieldKind.MarkedOption    
                                    else p, OptionalFieldKind.NormalOption 
                                | ConcreteType { Entity = d; Generics = [p] } when d.Value.FullName = "WebSharper.JavaScript.Optional`1" ->
                                    p, OptionalFieldKind.ErasedOption
                                | t ->    
                                    t, OptionalFieldKind.NotOption
                            f.JSName, optionKind, encode (t.SubstituteGenerics (Array.ofList targs))
                        )  
                    let pr =
                        match comp.GetClassInfo td with
                        | Some cls -> 
                            addTypeDep td 
                            if cls.HasWSPrototype then
                                GlobalAccess cls.Address
                            else Undefined
                        | _ -> Undefined
                    if pr = Undefined && fieldEncoders |> List.forall (fun (_, fo, fe) ->
                        fo <> OptionalFieldKind.NormalOption && isIdent fe
                    )
                    then ok ident
                    else
                        ((fun es ->
                            let es, tts = List.unzip es
                            ok (call "Record" [pr; NewArray es])
                            ), fieldEncoders)
                        ||> List.fold (fun k (fn, fo, fe) es ->                     
                                fe >>= fun e ->
                                k ((NewArray [cString fn; e; cInt (int fo)], t) :: es))
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
                    let isOption (t: Type) =
                        match t with
                        | ConcreteType { Entity = e } ->
                            e.Value.FullName = "Microsoft.FSharp.Core.FSharpOption`1"
                        | _ -> false
                    let discr =
                        match u.NamedUnionCases with
                        | None -> JI.StandardField
                        | Some None -> 
                            let allCases =
                                u.Cases |> Seq.mapi (fun i uci ->
                                    i,
                                    match tryGetInlinableRecordInfo uci with
                                    | Some (_, fRec) ->
                                        fRec |> Seq.filter (fun rf -> not (isOption rf.RecordFieldType))
                                        |> Seq.map (fun rf -> rf.JSName) |> Set.ofSeq
                                    | None ->
                                        match uci.Kind with 
                                        | M.NormalFSharpUnionCase fs ->
                                            fs
                                            |> List.choose (fun f ->
                                                if isOption f.UnionFieldType then None else Some f.Name)
                                            |> Set.ofList
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
                        let cases = Cast(TSType.Any, NewArray cases)
                        let discr =
                            match discr with
                            | JI.NoField discrFields ->
                                Cast(TSType.Any,
                                    discrFields
                                    |> List.map (fun (name, id) -> name, cInt id)
                                    |> Object
                                )
                            | JI.StandardField -> cString "$"
                            | JI.NamedField n -> cString n
                        let tn =
                            match comp.GetClassInfo td with
                            | Some cls -> 
                                addTypeDep td
                                if cls.HasWSPrototype then
                                    GlobalAccess { cls.Address with Address = PlainAddress ("$" :: cls.Address.Address.Value) }
                                else
                                    Undefined
                            | _ -> Undefined
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
                                let encF = encode ft 
                                let elimTag =
                                    if isIdent encF then !~(Bool true) else !~Null
                                encF >>= fun e ->
                                k (NewArray [tag; NewArray [NewArray [!~Null; elimTag; e]]] :: es)
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
                            | M.SingletonFSharpUnionCase ->
                                let tag =
                                    match u.NamedUnionCases with
                                    | None -> cInt i
                                    | _ -> cString (match case.JsonName with Some n -> n | _ -> case.Name)
                                k (NewArray [tag; NewArray []] :: es)
                            | M.ConstantFSharpUnionCase _ -> k (!~Null :: es)
                    )
                    |> snd
                    <| []
                | _ -> 
                    match comp.GetClassInfo td with
                    | Some cls ->
                        let fieldEncoders =
                            cls.Fields.Values
                            |> Seq.choose (fun (f, _, ft) ->
                                let jsNameTypeAndOption =
                                    let isOption name isMarked =
                                        match ft with
                                        | ConcreteType { Entity = d; Generics = [p] } when d.Value.FullName = "Microsoft.FSharp.Core.FSharpOption`1" ->
                                            if isMarked then
                                                Some (name, p, OptionalFieldKind.MarkedOption) 
                                            else
                                                Some (name, p, OptionalFieldKind.NormalOption) 
                                        | ConcreteType { Entity = d; Generics = [p] } when d.Value.FullName = "WebSharper.JavaScript.Optional`1" ->
                                            Some (name, p, OptionalFieldKind.ErasedOption) 
                                        | ft ->    
                                            Some (name, ft, OptionalFieldKind.NotOption)
                                    match f with
                                    | M.InstanceField n -> isOption n false
                                    | M.IndexedField i -> isOption (string i) false
                                    | M.OptionalField n -> isOption n true
                                    | M.StaticField _
                                    | M.VarField _ -> None
                                jsNameTypeAndOption |> Option.map (fun (jsName, t, optionKind) ->
                                    jsName, optionKind, encode (t.SubstituteGenerics (Array.ofList targs))
                                )
                            ) |> List.ofSeq
                        let pr =
                            match comp.GetClassInfo td with
                            | Some cls -> 
                                addTypeDep td 
                                if cls.HasWSPrototype then
                                    GlobalAccess cls.Address
                                else Undefined
                            | _ -> Undefined
                        if pr = Undefined && fieldEncoders |> List.forall (fun (_, fo, fe) ->
                            fo <> OptionalFieldKind.NormalOption && isIdent fe
                        )
                        then ok ident
                        else
                            ((fun es ->
                                let es, tts = List.unzip es
                                ok (call "Record" [pr; NewArray es])
                                ), fieldEncoders)
                            ||> List.fold (fun k (fn, fo, fe) es ->                     
                                    fe >>= fun e ->
                                    k ((NewArray [cString fn; e; cInt (int fo)], t) :: es))
                            <| []
                    | _ ->
                        fail (name + ": Type not supported: " + t.TypeDefinition.Value.FullName)
            encode t

        let encodeLambda name param t =
            getEncoding name true param t
            |> mapOk (fun x -> Appl(x, [], Pure, Some 0))

        let encode name param t arg =
            encodeLambda name param t
            |> mapOk (fun x -> Appl(x, [arg], Pure, Some 1))

        let decodeLambda name param t =
            getEncoding name false param t
            |> mapOk (fun x ->
                Cast(
                    TSType.Lambda([TSType.Any], param.Compilation.GetTSTypeOf(t)),
                    Appl(x, [], Pure, Some 0)
                )
            )

        let decode name param t arg =
            decodeLambda name param t
            |> mapOk (fun x -> Appl(x, [arg], Pure, Some 1))

    let Encode param t arg =
        // ENCODE()(arg)
        encode "Encode" param t arg

    let EncodeLambda warn t =
        // ENCODE()
        encodeLambda "EncodeLambda" warn t

    let Serialize param t arg =
        // JSON.stringify(ENCODE()(arg))
        encode "Serialize" param t arg
        |> mapOk (fun x -> mJson param.Compilation "Stringify" [x])

    let SerializeLambda param t =
        encodeLambda "SerializeLambda" param t
        |> mapOk (fun x ->
            let enc = Id.New(mut = false)
            let arg = Id.New(mut = false)
            // let enc = ENCODE() in fun arg -> JSON.stringify(enc(arg))
            Let(enc, x,
                Lambda([arg], Some (NonGenericType Definitions.String),
                    mJson param.Compilation "Stringify" [Appl(Var enc, [Var arg], Pure, Some 1)])))

    let Decode param t arg =
        // DECODE()(arg)
        decode "Decode" param t arg

    let DecodeLambda param t =
        // DECODE()
        decodeLambda "DecodeLambda" param t

    let Deserialize param t arg =
        // DECODE()(JSON.parse(arg))
        decode "Deserialize" param t (mJson param.Compilation "Parse" [arg])

    let DeserializeLambda param t =
        decodeLambda "DeserializeLambda" param t
        |> mapOk (fun x ->
            let dec = Id.New(mut = false)
            let arg = Id.New(mut = false)
            // let dec = DECODE() in fun arg -> dec(JSON.parse(arg))
            Let(dec, x,
                Lambda([arg], Some t,
                    Appl(Var dec, [mJson param.Compilation "Parse" [Var arg]], Pure, Some 1))))

    type SerializeMacro() =
        inherit WebSharper.Core.Macro()

        static let rec last = function
            | [x] -> x
            | x :: l -> last l
            | _ -> failwith ""

        override this.TranslateCall(c) =
            let f =
                match c.Method.Entity.Value.MethodName with
                | "Encode" -> Encode
                | "Decode" -> Decode
                | "Serialize" -> Serialize
                | "Deserialize" -> Deserialize
                | _ -> failwith "Invalid macro invocation"
            let param = 
                {
                    Compilation = c.Compilation
                    Warnings = ResizeArray()
                    Dependencies = ResizeArray()
                }
            let res =
                match f param c.Method.Generics.Head (last c.Arguments) with
                | Choice1Of3 x -> WebSharper.Core.MacroOk x
                | Choice2Of3 e -> WebSharper.Core.MacroError e
                | Choice3Of3 t -> WebSharper.Core.MacroNeedsResolvedTypeArg t
            let resWithWarnings =
                if param.Warnings.Count > 0 then
                    param.Warnings |> Seq.fold (fun res msg -> 
                        WebSharper.Core.MacroWarning (msg, res)) res
                else res
            if param.Dependencies.Count > 0 then
                WebSharper.Core.MacroDependencies (List.ofSeq param.Dependencies, resWithWarnings)
            else resWithWarnings    
