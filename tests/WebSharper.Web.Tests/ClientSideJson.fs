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

/// Tests Remoting functionality, including instance and static
/// remote functions, returning async, unit and sync values, and
/// sending/returning unions, lists, options, scalars and records.
namespace WebSharper.Web.Tests

open System.Collections.Generic
open WebSharper
open WebSharper.JavaScript
open WebSharper.Testing

[<JavaScript>]
module ClientSideJson =

    type SimpleRecord =
        {
            x: int
            y: float
            z: int * string
            t: string[]
        }

    type RecordWithOptions =
        {
            ox : option<int>
            [<OptionalField>] oy: option<string>
        }

    [<NamedUnionCases "case">]
    type SimpleUnion =
        |                           Nullary
        | [<CompiledName "un">]     Unary of x: int
        | [<CompiledName "bin">]    Binary of y: string * z: float

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
        | [<CompiledName "success">] Success of 'T
        | [<CompiledName "failure">] Failure of message: string

    type PersonData =
        { firstName: string
          lastName: string
          born: System.DateTime
          died: option<System.DateTime> }

    type Id = { id : int }

    let Tests =
        TestCategory "Client-side JSON" {

            Property "serialize int" (fun (x: int) -> Do {
                equal (Json.Serialize x) (Json.Stringify x)
            })

            Property "deserialize int" (fun (x: int) -> Do {
                equal (Json.Deserialize (Json.Stringify x)) x
            })

            Property "serialize float" (fun (x: float) -> Do {
                equal (Json.Serialize x) (Json.Stringify x)
            })

            Property "deserialize float" (fun (x: float) -> Do {
                equal (Json.Deserialize (Json.Stringify x)) x
            })

            Property "serialize string" (fun (x: string) -> Do {
                equal (Json.Serialize x) (Json.Stringify x)
            })

            Property "deserialize string" (fun (x: string) -> Do {
                equal (Json.Deserialize (Json.Stringify x)) x
            })

            Property "serialize tuple2" (fun (x: int * float) -> Do {
                equal (Json.Serialize x) (Json.Stringify x)
            })

            Property "deserialize tuple2" (fun (x: int * float) -> Do {
                equal (Json.Deserialize (Json.Stringify x)) x
            })

            Property "serialize tuple3" (fun (x: string * int * float) -> Do {
                equal (Json.Serialize x) (Json.Stringify x)
            })

            Property "deserialize tuple3" (fun (x: string * int * float) -> Do {
                equal (Json.Deserialize (Json.Stringify x)) x
            })

            Property "serialize array" (fun (x: string[]) -> Do {
                equal (Json.Serialize x) (Json.Stringify x)
            })

            Property "deserialize array" (fun (x: string[]) -> Do {
                equal (Json.Deserialize (Json.Stringify x)) x
            })

            Property "serialize list" (fun (x: list<int>) -> Do {
                equal (Json.Serialize x) (Json.Stringify (Array.ofList x))
            })

            Property "deserialize list" (fun (x: list<int>) -> Do {
                equal (Json.Deserialize (Json.Stringify (Array.ofList x))) x
            })

            Property "serialize set" (fun (x: int[]) -> Do {
                let x = Set.ofArray x
                equal (Json.Serialize x) (Json.Stringify (Set.toArray x))
            })

            Property "deserialize set" (fun (x: int[]) -> Do {
                let x = Set.ofArray x
                equal (Json.Deserialize (Json.Stringify (Set.toArray x))) x
            })

            Property "serialize map" (fun (x: (string * int)[]) -> Do {
                let m = Map.ofArray x
                let x = Map.toArray m
                equal (Json.Serialize m) (Json.Stringify (New (As x)))
            })

            Property "deserialize map" (fun (x: (string * int)[]) -> Do {
                let m = Map.ofArray x
                let x = New (As (Map.toArray m))
                equal (Json.Deserialize (Json.Stringify x)) m
            })

            Property "serialize dictionary" (fun (x: (string * int)[]) -> Do {
                let d = Dictionary()
                do for k, v in x :> seq<_> do d.[k] <- v
                equal (Json.Parse (Json.Serialize d)) (New (As x))
            })

            Property "deserialize dictionary" (fun (x: (string * int)[]) -> Do {
                let d = Dictionary()
                do for k, v in x :> seq<_> do d.[k] <- v
                let ser = New (As x)
                let deser =
                    Json.Stringify ser
                    |> Json.Deserialize<Dictionary<string, int>>
                let y = New []
                do for KeyValue(k, v) in deser :> seq<_> do y?(k) <- v
                equal y ser
            })

            Test "serialize simple record" {
                let r (x: int) : SimpleRecord =
                    {
                        x = x
                        y = 3.14
                        z = (x, "test123")
                        t = [|"a"; "b"|]
                    }
                equal (Json.Serialize (r 12)) (Json.Stringify (r 12))
                equal (Json.Serialize [|r 13; r 42|]) (Json.Stringify [|r 13; r 42|])
            }

            Test "deserialize simple record" {
                let r (x: int) : SimpleRecord =
                    {
                        x = x
                        y = 3.14
                        z = (x, "test123")
                        t = [|"a"; "b"|]
                    }
                equal (Json.Deserialize (Json.Stringify (r 12))) (r 12)
                equal (Json.Deserialize (Json.Stringify [|r 13; r 42|])) [|r 13; r 42|]
            }

            Test "serialize record with options" {
                equal (Json.Serialize { ox = Some 1; oy = Some "2" } |> Json.Parse)
                    (New ["ox" => 1; "oy" => "2"])
                equal (Json.Serialize { ox = None; oy = Some "2" } |> Json.Parse)
                    (New ["oy" => "2"])
                equal (Json.Serialize { ox = Some 1; oy = None } |> Json.Parse)
                    (New ["ox" => 1])
                equal (Json.Serialize { ox = None; oy = None } |> Json.Parse)
                    (New [])
            }

            Test "deserialize record with options" {
                equal (Json.Deserialize """{"ox":1,"oy":"2"}""")    { ox = Some 1; oy = Some "2" }
                equal (Json.Deserialize """{"oy":"2"}""")           { ox = None; oy = Some "2" }
                equal (Json.Deserialize """{"ox":1}""")             { ox = Some 1; oy = None }
                equal (Json.Deserialize """{}""")                   { ox = None; oy = None }
            }

            Test "serialize simple union" {
                equal (Json.Serialize Nullary |> Json.Parse)
                    (New ["case" => "Nullary"])
                equal (Json.Serialize (Unary 1) |> Json.Parse)
                    (New ["case" => "un"; "x" => 1])
                equal (Json.Serialize (Binary ("aaa", 3.14)) |> Json.Parse)
                    (New ["case" => "bin"; "y" => "aaa"; "z" => 3.14])
            }

            Test "deserialize simple union" {
                equal (Json.Deserialize """{"case":"Nullary"}""")                Nullary
                equal (Json.Deserialize """{"case":"un","x":1}""")               (Unary 1)
                equal (Json.Deserialize """{"case":"bin","y":"aaa","z":3.14}""") (Binary ("aaa", 3.14))
            }

            Test "serialize implicit union" {
                equal (Json.Serialize (Impl1 123) |> Json.Parse)
                    (New ["x" => 123])
                equal (Json.Serialize (Impl2 (2.718, "test")) |> Json.Parse)
                    (New ["y" => 2.718; "z" => "test"])
                equal (Json.Serialize (Impl3 (45, 67)) |> Json.Parse)
                    (New ["y" => 45; "t" => 67])
            }

            Test "deserialize implicit union" {
                equal (Json.Deserialize """{"x":123}""")                (Impl1 123)
                equal (Json.Deserialize """{"y":2.718,"z":"test"}""")   (Impl2 (2.718, "test"))
                equal (Json.Deserialize """{"y":45,"t":67}""")          (Impl3 (45, 67))
            }

            Test "serialize union with inline record" {
                equal (Json.Serialize (NotInline {rx = 9; ry = "o"}) |> Json.Parse)
                    (New ["a" => "NotInline"; "r" => New ["rx" => 9; "ry" => "o"]])
                equal (Json.Serialize (Inline {rx = 9; ry = "o"}) |> Json.Parse)
                    (New ["a" => "Inline"; "rx" => 9; "ry" => "o"])
            }

            Test "deserialize union with inline record" {
                equal (Json.Deserialize """{"a":"NotInline","r":{"rx":9,"ry":"o"}}""")
                    (NotInline {rx = 9; ry = "o"})
                equal (Json.Deserialize """{"a":"Inline","rx":9,"ry":"o"}""")
                    (Inline {rx = 9; ry = "o"})
            }

            let now = System.DateTime.Now
            let d = Date(Date.UTC(2011, 9, 5, 14, 48, 0)) // "2011-10-05T14:48:00.000Z"

            Test "serialize generic union" {
                equal (Json.Serialize (Success "x") |> Json.Parse)
                    (New ["result" => "success"; "Item" => "x"])
                equal (Json.Serialize (Success { rx = 9; ry = "o" }) |> Json.Parse)
                    (New ["result" => "success"; "rx" => 9; "ry" => "o"])
                equal (Json.Serialize (Failure "it failed" : GenericUnion<string>) |> Json.Parse)
                    (New ["result" => "failure"; "message" => "it failed"])
            }

            Test "deserialize generic union" {
                equal (Json.Deserialize """{"result":"success","Item":"x"}""")
                    (Success "x")
                equal (Json.Deserialize """{"result":"success","rx":9,"ry":"o"}""")
                    (Success { rx = 9; ry = "o" })
                equal (Json.Deserialize """{"result":"failure","message":"it failed"}""")
                    (Failure "it failed" : GenericUnion<string>)
                equal (Json.Deserialize """{"result":"success","Item":[[{"id":1},{"firstName":"Alonzo","lastName":"Church","born":"1903-06-14T00:00:00.0000000"}],[{"id":2},{"firstName":"Alan","lastName":"Turing","born":"1912-06-23T00:00:00.0000000","died":"1954-06-07T00:00:00.0000000"}]]}""")
                    (Success [|
                        {id = 1}, { firstName = "Alonzo"; lastName = "Church"; born = Date(Date.Parse("1903-06-14T00:00:00.0000000")).Self; died = None }
                        {id = 2}, { firstName = "Alan"; lastName = "Turing"; born = Date(Date.Parse("1912-06-23T00:00:00.0000000")).Self; died = Some(Date(Date.Parse("1954-06-07T00:00:00.0000000")).Self) }
                    |])
            }

            Test "serialize System.DateTime" {
                let serAndParse (d: System.DateTime) : Date =
                    new Date(Date.Parse(Json.Parse(Json.Serialize d) :?> string))
                equal (serAndParse d.Self) d
                equal (serAndParse now) now.JS
            }

            Test "deserialize System.DateTime" {
                let strAndDeser (d: Date) : System.DateTime =
                    Json.Deserialize (Json.Stringify (d.ToISOString()))
                equal (strAndDeser d) d.Self
                equal (strAndDeser now.JS) now
            }

        }
