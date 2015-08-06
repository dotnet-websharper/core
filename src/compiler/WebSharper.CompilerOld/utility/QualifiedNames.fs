// $begin{copyright}
//
// This file is part of WebSharper
//
// Copyright (c) 2008-2015 IntelliFactory
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

namespace WebSharper

/// Facilities for dealing with qualified names required by the compiler.
module internal QualifiedNames  =
    open System
    open System.Collections.Generic
    open System.Threading
    open System.Text.RegularExpressions

    (* TODO: configurable clash detection between module names and definitions *)

    [<AbstractClass>]
    type Printer<'T>() =
        abstract Builder : obj
        abstract Show : 'T -> string

        override x.Equals(other: obj) =
            match other with
            | :? Printer<'T> as o ->
                Object.ReferenceEquals(x.Builder, o.Builder)
            | _ -> false

        override x.GetHashCode() =
            hash x.Builder

        interface IComparable with
            member x.CompareTo(other: obj) =
                match other with
                | :? Printer<'T> as o ->
                    if Object.ReferenceEquals(x.Builder, o.Builder) then 0 else
                        invalidArg "other" "Attempting to compare objects from differen builders"
                | _ ->
                    invalidArg "other" "Invalid comparison"

    type Id =
        {
            IdCode : uint32
            IdPrinter : Printer<Id>
        }

        override id.ToString() =
            id.IdPrinter.Show(id)

    type Name =
        {
            NameCode : uint32
            NamePrinter : Printer<Name>
        }

        override name.ToString() =
            name.NamePrinter.Show(name)

    exception InvalidIdentifier of string with

        override err.Message =
            match err :> exn with
            | InvalidIdentifier k -> string k
            | _ -> "impossible"

    let alnum = Regex(@"^\w+$")

    let isValidId id =
        match id with
        | null | "" -> false
        | _ -> alnum.IsMatch(id)

    type Config =
        {
            ConcatIdentifiers : list<string> -> string
            InvalidId : string -> exn
            IsValidId : string -> bool
        }

        static member Default =
            {
                ConcatIdentifiers = String.concat "."
                InvalidId = InvalidIdentifier
                IsValidId = isValidId
            }

    [<Sealed>]
    type Table<'T when 'T : equality>() =
        let forward = Dictionary<'T,uint32>()
        let reverse = Dictionary<uint32,'T>()

        member t.Decode(x) =
            reverse.[x]

        member t.Encode(x) =
            match forward.TryGetValue(x) with
            | true, c -> c
            | _ ->
                let code = uint32 forward.Count + 1u
                forward.[x] <- code
                reverse.[code] <- x
                code

    let packInts (a: uint32) (b: uint32) : uint64 =
        (uint64 a <<< 32) ||| uint64 b

    let unpackInt1 (pair: uint64) =
        uint32 (pair >>> 32)

    let unpackInt2 (pair: uint64) =
        uint32 (pair &&& 0xffffffffUL)

    [<Literal>]
    let NO_PARENT =
        0u

    [<Sealed>]
    type Builder private (cfg) =
        let addresses = Table<uint64>()
        let names = Table<string>()

        let checkId id =
            if not (cfg.IsValidId id) then
                raise (cfg.InvalidId id)

        let showId id =
            names.Decode(id.IdCode)

        let decodeName name =
            names.Decode(name)

        let lookupName addr =
            let rec lookup acc addr =
                let pair = addresses.Decode(addr)
                let parent = unpackInt1 pair
                let name = unpackInt2 pair
                match parent with
                | NO_PARENT -> decodeName name :: acc
                | p -> lookup (decodeName name :: acc) p
            lookup [] addr

        let showName name =
            lookupName name.NameCode
            |> String.concat "."

        let encodeName name =
            checkId name
            names.Encode(name)

        member private b.EncodeName(addr) =
            let code = addresses.Encode(addr)
            b.RecoverName(code)

        member b.GetId(name) =
            let pair = addresses.Decode(name.NameCode)
            b.RecoverId(unpackInt2 pair)

        member b.Id(id) =
            checkId id
            b.RecoverId(names.Encode(id))

        member private b.Init() =
            let bb = box b
            b.IdPrinter <-
                {
                    new Printer<Id>() with
                        member p.Builder = bb
                        member p.Show(id) = showId id
                }
            b.NamePrinter <-
                {
                    new Printer<Name>() with
                        member p.Builder = bb
                        member p.Show(name) = showName name
                }

        member b.Match(name) =
            let pair = addresses.Decode(name.NameCode)
            let parent = unpackInt1 pair
            let id = b.RecoverId(unpackInt2 pair)
            match parent with
            | NO_PARENT -> Choice2Of2 id
            | p -> Choice1Of2 (b.RecoverName(parent), id)

        member b.Nested(parent, id) =
            packInts parent.NameCode id.IdCode
            |> b.EncodeName

        member b.RecoverId(idCode) =
            {
                IdCode = idCode
                IdPrinter = b.IdPrinter
            }

        member b.RecoverName(nameCode) =
            {
                NameCode = nameCode
                NamePrinter = b.NamePrinter
            }

        member b.Root(id) =
            packInts NO_PARENT id.IdCode
            |> b.EncodeName

        member b.Config = cfg

        member val IdPrinter =
            Unchecked.defaultof<Printer<Id>> with get, set

        member val NamePrinter =
            Unchecked.defaultof<Printer<Name>> with get, set

        static member Create(?cfg) =
            let b = Builder(defaultArg cfg Config.Default)
            b.Init()
            b

    type Id with

        member id.Builder =
            id.IdPrinter.Builder :?> Builder

        member id.Text =
            id.IdPrinter.Show(id)

    type Name with

        member name.Builder =
            name.NamePrinter.Builder :?> Builder

        member name.Id =
            name.Builder.GetId(name)

        member name.Item
            with get id =
                name.Builder.Nested(name, id)

        member name.Text =
            name.NamePrinter.Show(name)

    let (|Nested|Root|) (name: Name) =
        name.Builder.Match(name)

    [<Sealed>]
    exception NameClash of Name with

        override err.Message =
            match err :> exn with
            | NameClash addr -> addr.Text
            | _ -> "impossible"

    type NameMap<'T> =
        {
            NMBuilder : Builder
            NMCodes : uint32 []
            NMValues : 'T []
        }

        member nm.ContainsKey(n) =
            Array.BinarySearch(nm.NMCodes, n.NameCode) >= 0
            && n.Builder = nm.NMBuilder

        member nm.Iterate(f: Name -> 'T -> unit) =
            match nm with
            | { NMBuilder = b; NMCodes = cs; NMValues = vs } ->
                for i in 0 .. nm.NMCodes.Length - 1 do
                    f (b.RecoverName(cs.[i])) vs.[i]

        member nm.TryFind(n) =
            let j = Array.BinarySearch(nm.NMCodes, n.NameCode)
            if j < 0 || n.Builder <> nm.NMBuilder then None else
                Some nm.NMValues.[j]

    let zero = Builder.Create()

    [<Sealed>]
    type NameMap =

        static member Empty() =
            {
                NMBuilder = zero
                NMCodes = Array.empty
                NMValues = Array.empty
            }

        static member Merge(ms: seq<NameMap<'T>>) =
            let ms =
                ms
                |> Seq.filter (fun f -> f.NMCodes.Length > 0)
                |> Seq.toArray
            match ms.Length with
            | 0 -> NameMap.Empty()
            | 1 -> ms.[0]
            | _ ->
                let ok =
                    ms
                    |> Array.forall (fun x -> x.NMBuilder = ms.[0].NMBuilder)
                if not ok then
                    invalidArg "maps" "Cannot merge maps with different builders"
                let d = SortedDictionary<uint32,_>()
                for nm in ms do
                    match nm with
                    | { NMBuilder = b; NMCodes = cs; NMValues = vs } ->
                        for i in 0 .. nm.NMCodes.Length - 1 do
                            let key = cs.[i]
                            if d.ContainsKey(key) then
                                raise (NameClash (b.RecoverName(key)))
                            d.[key] <- vs.[i]
                let codes = Array.zeroCreate d.Count
                let values = Array.zeroCreate d.Count
                d
                |> Seq.iteri (fun i (KeyValue (k, v)) ->
                    codes.[i] <- k
                    values.[i] <- v)
                {
                    NMBuilder = ms.[0].NMBuilder
                    NMCodes = codes
                    NMValues = values
                }

        static member Singleton(n: Name, x) =
            {
                NMBuilder = n.Builder
                NMCodes = [| n.NameCode |]
                NMValues = [| x |]
            }

    type IdVector =
        {
            IVBuilder : Builder
            IVCodes : uint32 []
        }

        member vec.IndexOf(id: Id) =
            if id.Builder = vec.IVBuilder then
                let j = Array.IndexOf(vec.IVCodes, id.IdCode)
                if j < 0 then -1 else j
            else -1

        member vec.Item
            with get (x: int) =
                if x >= 0 && x < vec.IVCodes.Length then
                    let code = vec.IVCodes.[x]
                    vec.IVBuilder.RecoverId(code)
                    |> Some
                else None

        static member Create(elements: seq<Id>) =
            let elements = Seq.toArray elements
            if elements.Length = 0 then
                {
                    IVBuilder = zero
                    IVCodes = Array.empty
                }
            else
                {
                    IVBuilder = elements.[0].Builder
                    IVCodes = [| for e in elements -> e.IdCode |]
                }
