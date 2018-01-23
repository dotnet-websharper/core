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

namespace WebSharper.Sitelets.Tests

open WebSharper

module Json =

    [<JavaScript>]
    module Types =

        type SimpleRecord =
            {
                x: int
                y: float
                z: int * string
                t: string[]
            }

        type SimpleObject(x: string, y: int) =
            member this.X = x
            member this.Y = y

            override this.Equals(a) =
                match a with
                | :? SimpleObject as a -> this.X = a.X && this.Y = a.Y
                | _ -> false

            override this.GetHashCode() =
                hash this.X + hash this.Y

        type RecordWithOptions<'T>() =

            member this.Test() = "Wrong class :("

        type RecordWithOptions =
            {
                ox : option<int>
                [<OptionalField>] oy: option<string>
            }

            member this.Test() = this.ox

        type ObjectWithOptions(ox: option<int>, oy: option<string>) =
            [<OptionalField>]
            let oy = oy
            member this.X = ox
            member this.Y = oy
            member this.Test() = ox

            override this.Equals(a) =
                match a with
                | :? ObjectWithOptions as a -> this.X = a.X && this.Y = a.Y
                | _ -> false

            override this.GetHashCode() =
                hash this.X + hash this.Y

        [<NamedUnionCases "case">]
        type SimpleUnion =
            |                           Nullary
            | [<Name "un">]     Unary of x: int
            | [<Name "bin">]    Binary of y: string * z: float

        [<NamedUnionCases>]
        type ImplicitUnion =
            | Impl1 of x: int
            | Impl2 of y: float * z: string
            | Impl3 of y: int * t: int

        type Rec = { rx: int; ry: string }

        [<NamedUnionCases "a">]
        type UnionWithInlineRecord =
            | Inline of Rec
            | NotInline of r: Rec

        [<NamedUnionCases "result">]
        type GenericUnion<'T> =
            | [<Name "success">] Success of 'T
            | [<Name "failure">] Failure of message: string

            member this.Test() = 12

        type UnionWithConstants =
            | [<Constant "foo">] Foo
            | [<Constant "bar">] Bar
            | [<Constant 12>] Twelve
            | [<Constant null>] Null

        type GenericUnion() =

            member this.Test() = "Wrong class :("

        type PersonData =
            { firstName: string
              lastName: string
              born: System.DateTime
              died: option<System.DateTime> }

        type Id = { id : int }

    open WebSharper.Sitelets

    type Action =
        | [<Json "x">] Int of x: int
        | [<Json "x">] Float of x: float
        | [<Json "x">] String of x: string
        | [<Json "x">] Tuple2 of x: (int * string)
        | [<Json "x">] Tuple3 of x: (string * int * bool)
        | [<Json "x">] Array of x: string[]
        | [<Json "x">] List of x: list<int>
        | [<Json "x">] Set of x: Set<int>
        | [<Json "x">] Map of x: Map<string, int>
        | [<Json "x">] ComplexMap of x: Map<Types.Id, int>
        | [<Json "x">] Dictionary of x: System.Collections.Generic.Dictionary<string, int>
        | [<Json "x">] ComplexDictionary of x: System.Collections.Generic.Dictionary<Types.Id, int>
        | [<Json "x">] SimpleRecord of x: Types.SimpleRecord
        | [<Json "x">] SimpleRecordArray of x: Types.SimpleRecord[]
        | [<Json "x">] RecordOptions of x: Types.RecordWithOptions
        | [<Json "x">] SimpleUnion of x: Types.SimpleUnion
        | [<Json "x">] ImplicitUnion of x: Types.ImplicitUnion
        | [<Json "x">] UnionInlineRecord of x: Types.UnionWithInlineRecord
        | [<Json "x">] GenericUnionString of x: Types.GenericUnion<string>
        | [<Json "x">] GenericUnionRecord of x: Types.GenericUnion<Types.Rec>
        | [<Json "x">] UnionConstants of x: Types.UnionWithConstants
        | [<Json "x">] DateTime of x: System.DateTime
        | [<Json "x">] DateTimeOffset of x: System.DateTimeOffset
        | [<Json "x">] ResizeArray of x: ResizeArray<int>
        | [<Json "x">] Queue of x: System.Collections.Generic.Queue<int>
        | [<Json "x">] Stack of x: System.Collections.Generic.Stack<int>
        | [<Json "x">] LinkedList of x: System.Collections.Generic.LinkedList<int>

    let Content = function
        | Int x -> Content.Json x
        | Float x -> Content.Json x
        | String x -> Content.Json x
        | Tuple2 x -> Content.Json x
        | Tuple3 x -> Content.Json x
        | Array x -> Content.Json x
        | List x -> Content.Json x
        | Set x -> Content.Json x
        | Map x -> Content.Json x
        | ComplexMap x -> Content.Json x
        | Dictionary x -> Content.Json x
        | ComplexDictionary x -> Content.Json x
        | SimpleRecord x -> Content.Json x
        | SimpleRecordArray x -> Content.Json x
        | RecordOptions x -> Content.Json x
        | SimpleUnion x -> Content.Json x
        | ImplicitUnion x -> Content.Json x
        | UnionInlineRecord x -> Content.Json x
        | GenericUnionString x -> Content.Json x
        | GenericUnionRecord x -> Content.Json x
        | UnionConstants x -> Content.Json x
        | DateTime x -> Content.Json x
        | DateTimeOffset x -> Content.Json x
        | ResizeArray x -> Content.Json x
        | Queue x -> Content.Json x
        | Stack x -> Content.Json x
        | LinkedList x -> Content.Json x
