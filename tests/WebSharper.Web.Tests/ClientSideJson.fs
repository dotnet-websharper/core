﻿// $begin{copyright}
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
open WebSharper.JQuery
open WebSharper.Testing
open WebSharper.Sitelets.Tests.Json.Types

type private Enum =
    | Case1 = 1
    | Case2 = 2

[<JavaScript>]
module ClientSideJson =

    type Record =
        {
            a : int
            b : string
        }


    [<Inline>]
    let InlineSerialize x = "x" + Json.Serialize x

    [<Inline>]
    let InlineDeserialize<'T> (x: string) = Json.Deserialize x.[1..] : 'T

    type TestType = { foo: string option }

    module Bug735 =
        type test_class_i = 
            | Test_class_i of Test_class_i:string
        type test_class_o (test_field: test_class_i option) =
            member r.test_field = test_field

            override this.Equals(a) =
                match a with
                | :? test_class_o as a -> this.test_field = a.test_field
                | _ -> false

            override this.GetHashCode() =
                hash this.test_field

    let ClientTests =
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

            Property "deserialize set with complex key type" (fun (x: (int * string)[]) -> Do {
                let x = Set.ofArray [| for a, b in x -> { a = a; b = b } |]
                equal (Json.Deserialize (Json.Stringify (Set.toArray x))) x
            })

            Property "serialize map" (fun (x: (string * int)[]) -> Do {
                let m = Map.ofArray x
                let x = Map.toArray m
                equal (Json.Serialize m) (Json.Stringify (New (As x)))
            })

            Property "serialize map with complex key type" (fun (x: (int * string * int)[]) -> Do {
                let a = Array.distinctBy fst [| for a, b, c in x -> { a = a; b = b }, c |]
                let m = Map.ofArray a
                let enc = Json.Encode m :?> (Record * int)[]
                Array.sortInPlaceBy fst a
                Array.sortInPlaceBy fst enc
                equal enc a
            })

            Property "deserialize map" (fun (x: (string * int)[]) -> Do {
                let m = Map.ofArray x
                let x = New (As (Map.toArray m))
                equal (Json.Deserialize (Json.Stringify x)) m
            })

            Property "deserialize map with complex key type" (fun (x: (int * string * int)[]) -> Do {
                let a = Array.distinctBy fst [| for a, b, c in x -> { a = a; b = b }, c |]
                let m = Map.ofArray a
                isTrue (Json.Deserialize (Json.Stringify a) = m)
            })

            Property "serialize dictionary" (fun (x: (string * int)[]) -> Do {
                let m = Map.ofArray x
                let x = Map.toArray m
                let d = Dictionary()
                do for k, v in x :> seq<_> do d.[k] <- v
                equal (Json.Parse (Json.Serialize d)) (New (As x))
            })

            Property "deserialize dictionary" (fun (x: (string * int)[]) -> Do {
                let m = Map.ofArray x
                let x = Map.toArray m
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

            Property "deserialize dictionary with complex key type" (fun (x: (int * string * int)[]) -> Do {
                let a = Array.distinctBy fst [| for a, b, c in x -> { a = a; b = b }, c |]
                let d = Dictionary()
                do for k, v in a :> seq<_> do d.[k] <- v
                let enc = Json.Encode d :?> (Record * int)[]
                Array.sortInPlaceBy fst a
                Array.sortInPlaceBy fst enc
                equal enc a
            })

            Test "de/serialize record with optional value" {
                let t1 = { foo = Some "foo" }
                let t2 : TestType = 
                    WebSharper.Json.Encode t1 |> WebSharper.Json.Decode
                equal t1 t2
            }

            Test "de/serialize string dictionary with optional value" {
                let t1 = Dictionary<string,TestType>()
                t1.Add("foo", { foo = Some "foo" })
                t1.Add("bar", { foo = None })
                let t2 : Dictionary<string,TestType> = 
                    WebSharper.Json.Encode t1 |> WebSharper.Json.Decode
                equal (Array.ofSeq t1) (Array.ofSeq t2)
            }

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
                raisesMsg (Json.Deserialize<SimpleRecord> """{"x":1,"y":43,"t":[]}""") "Missing mandatory field raises exception"
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
                let x = (Json.Deserialize """{"ox":1,"oy":"2"}""")
                equal x                                     { ox = Some 1; oy = Some "2" }
                equalMsg (x.Test()) (Some 1) "prototype is set"
                equal (Json.Deserialize """{"oy":"2"}""")   { ox = None; oy = Some "2" }
                equal (Json.Deserialize """{"ox":1}""")     { ox = Some 1; oy = None }
                equal (Json.Deserialize """{}""")           { ox = None; oy = None }
            }

            Test "serialize simple object" {
                let o (x: int) = SimpleObject("hello", x)
                equal (Json.Serialize (o 12)) (Json.Stringify (o 12))
                equal (Json.Serialize [|o 13; o 42|]) (Json.Stringify [|o 13; o 42|])
            }

            Test "deserialize simple object" {
                let o (x: int) = SimpleObject("hello", x)
                equal (Json.Deserialize (Json.Stringify (o 12))) (o 12)
                equal (Json.Deserialize (Json.Stringify [|o 13; o 42|])) [|o 13; o 42|]
                raisesMsg (Json.Deserialize<SimpleObject> """{"x":1}""") "Missing mandatory field raises exception"
            }

            Test "serialize object with options" {
                equal (Json.Serialize (ObjectWithOptions(Some 1, Some "2")) |> Json.Parse)
                    (New ["ox" => 1; "oy" => "2"])
                equal (Json.Serialize (ObjectWithOptions(None, Some "2")) |> Json.Parse)
                    (New ["oy" => "2"])
                equal (Json.Serialize (ObjectWithOptions(Some 1, None)) |> Json.Parse)
                    (New ["ox" => 1])
                equal (Json.Serialize (ObjectWithOptions(None, None)) |> Json.Parse)
                    (New [])
            }

            Test "deserialize object with options" {
                let x = (Json.Deserialize """{"ox":1,"oy":"2"}""")
                equal x                                     (ObjectWithOptions(Some 1, Some "2"))
                equalMsg (x.Test()) (Some 1) "prototype is set"
                equal (Json.Deserialize """{"oy":"2"}""")   (ObjectWithOptions(None, Some "2"))
                equal (Json.Deserialize """{"ox":1}""")     (ObjectWithOptions(Some 1, None))
                equal (Json.Deserialize """{}""")           (ObjectWithOptions(None, None))
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
                equal (Json.Serialize ((Success "x", Success { rx = 9; ry = "o" })) |> Json.Parse)
                    (box [| New ["result" => "success"; "Item" => "x"]; New ["result" => "success"; "rx" => 9; "ry" => "o"] |])
            }

            Test "deserialize generic union" {
                let x = (Json.Deserialize """{"result":"success","Item":"x"}""")
                equal x (Success "x")
                equalMsg (x.Test()) 12 "prototype is set"
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

            Test "serialize union with constants" {
                equal (Json.Serialize Foo |> Json.Parse |> unbox) "foo"
                equal (Json.Serialize Bar |> Json.Parse |> unbox) "bar"
                equal (Json.Serialize Twelve |> Json.Parse |> unbox) 12
                equal (Json.Serialize Null |> Json.Parse |> unbox) null
            }

            Test "deserialize union with constants" {
                equal (Json.Deserialize (Json.Stringify "foo")) Foo
                equal (Json.Deserialize (Json.Stringify "bar")) Bar
                equal (Json.Deserialize (Json.Stringify 12)) Twelve
                equal (Json.Deserialize (Json.Stringify null)) Null
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

            let nowO = System.DateTimeOffset(now, System.TimeSpan.FromHours 1.)
            let dO = System.DateTimeOffset(d.Self, System.TimeSpan.FromHours -1.)

            Test "serialize System.DateTimeOffset" {
                let serAndParse (d: System.DateTimeOffset) : Date * int =
                    let r = Json.Parse(Json.Serialize d)
                    new Date(Date.Parse(r?d: string)), r?o
                equal (serAndParse dO) (d, -60)
                equal (serAndParse nowO) (now.JS, 60)
            }

            Test "deserialize System.DateTimeOffset" {
                let serAndDeser (d: System.DateTimeOffset) : System.DateTimeOffset =
                    Json.Deserialize (Json.Serialize d)
                equal (serAndDeser dO) dO
                equal (serAndDeser nowO) nowO
            }

            Test "deserialize System.DateTimeOffset from ISO string" {
                let serAndDeser (d: System.DateTimeOffset) : System.DateTimeOffset =
                    Json.Deserialize (Json.Serialize d.DateTime)
                equal (serAndDeser dO).DateTime d.Self
                equal (serAndDeser nowO).DateTime now
            }

            Test "deserialize System.DateTime from a System.DateTimeOffset" {
                let serAndDeser (d: System.DateTimeOffset) : System.DateTime =
                    Json.Deserialize (Json.Serialize d.DateTime)
                equal (serAndDeser dO) d.Self
                equal (serAndDeser nowO) now
            }

            Test "serialize enum" {
                equal (Json.Serialize Enum.Case1) "1"
                equal (Json.Serialize Enum.Case2) "2"
                equal (Json.Serialize (enum<Enum> 3)) "3"
            }

            Test "deserialize enum" {
                equal (Json.Deserialize "1") Enum.Case1
                equal (Json.Deserialize "2") Enum.Case2
                equal (Json.Deserialize "3") (enum<Enum> 3)
            }

            Test "serialize: defer type resolution in inline function" {
                equal (InlineSerialize 42) "x42"
            }

            Test "deserialize: defer type resolution in inline function" {
                equal (InlineDeserialize "x42") 42
            }

            Test "serialize ResizeArray" {
                equal (Json.Serialize (ResizeArray<int>())) "[]"
                equal (Json.Serialize (ResizeArray [3; 0; 423])) "[3,0,423]"
            }

            Test "deserialize ResizeArray" {
                equal (Json.Deserialize "[]") (ResizeArray<int>())
                equal (Json.Deserialize "[87,9,124]") (ResizeArray [87;9;124])
            }

            Test "serialize Queue" {
                equal (Json.Serialize (Queue<int>())) "[]"
                equal (Json.Serialize (Queue [3; 0; 423])) "[3,0,423]"
            }

            Test "deserialize Queue" {
                equal (Json.Deserialize "[]") (Queue<int>())
                equal (Json.Deserialize "[87,9,124]") (Queue [87;9;124])
            }

            Test "serialize Stack" {
                equal (Json.Serialize (Stack<int>())) "[]"
                equal (Json.Serialize (Stack [3; 0; 423])) "[423,0,3]"
            }

            Test "deserialize Stack" {
                equal (Json.Deserialize "[]") (Stack<int>())
                equal (Json.Deserialize "[87,9,124]") (Stack [124;9;87])
            }

            Test "serialize LinkedList" {
                equal (Json.Serialize (LinkedList<int>())) "[]"
                equal (Json.Serialize (LinkedList [3; 0; 423])) "[3,0,423]"
            }

            Test "deserialize LinkedList" {
                let l = Json.Deserialize<LinkedList<int>> "[]"
                equal l.First null
                let l = Json.Deserialize<LinkedList<int>> "[87,9,124]"
                equal l.First.Value 87
                l.RemoveFirst()
                equal l.First.Value 9
                l.RemoveFirst()
                equal l.First.Value 124
                l.RemoveFirst()
                equal l.First null
            }

            Test "deserialize obj" {
                equalMsg (Json.Deserialize<obj> "null") (box ()) "null"
                equalMsg (Json.Deserialize<obj> "123") (box 123) "int"
                equalMsg (Json.Deserialize<obj> "123.456") (box 123.456) "float"
                equalMsg (Json.Deserialize<obj> "\"test\"") (box "test") "string"
                equalMsg (Json.Deserialize<obj> """{"a":1,"b":2}""") (New ["a" => 1; "b" => 2] : obj) "plain object"
            }

            Test "#735 optional union field on object" {
                let l = [Bug735.test_class_o(Some (Bug735.Test_class_i "foo"))]
                let o = Json.Encode<Bug735.test_class_o list> l
                equal (o?("0")?test_field?Test_class_i) "foo"
                let l2 = Json.Decode<Bug735.test_class_o list> o
                equal l l2
                equal l2.Head.test_field (Some (Bug735.Test_class_i "foo")) 
            }
        }

    let baseEcho (url: string) (serializedArg: string) (decode: obj -> 't) : Async<'t> =
        Async.FromContinuations <| fun (ok, ko, _) ->
            JQuery.Ajax(
                JQuery.AjaxSettings(
                    Url = url,
                    Type = JQuery.RequestType.POST,
                    ContentType = Union2Of2("application/json"),
                    DataType = JQuery.DataType.Json,
                    Data = serializedArg,
                    Success = (fun data _ _ -> ok (decode data)),
                    Error = (fun jqXHR _ _ -> ko (System.Exception(jqXHR.ResponseText)))
                )
            )
            |> ignore

    let echoObj jsonBaseUri (r: 'T) : Async<'T> =
        baseEcho (jsonBaseUri + "Object") (Json.Stringify r) (Json.Decode<obj> >> unbox)

    let SiteletRoundTripTests jsonBaseUri =
        let echo url serializedArg decode =
            baseEcho (jsonBaseUri + url) serializedArg decode
        TestCategory "Client to Sitelet JSON round-trip" {

            Property "int" (fun (x: int) -> Do {
                equalAsync (echo "Int" (Json.Serialize x) Json.Decode) x
            })

            Property "float" (fun (x: float) -> Do {
                approxEqualAsync (echo "Float" (Json.Serialize x) Json.Decode) x
            })

            Property "string" (fun (x: string) -> Do {
                equalAsync (echo "String" (Json.Serialize x) Json.Decode) x
            })

            Property "tuple2" (fun (x: int * string) -> Do {
                equalAsync (echo "Tuple2" (Json.Serialize x) Json.Decode) x
            })

            Property "tuple3" (fun (x: string * int * bool) -> Do {
                equalAsync (echo "Tuple3" (Json.Serialize x) Json.Decode) x
            })

            Property "array" (fun (x: string[]) -> Do {
                equalAsync (echo "Array" (Json.Serialize x) Json.Decode) x
            })

            Property "list" (fun (x: list<int>) -> Do {
                equalAsync (echo "List" (Json.Serialize x) Json.Decode) x
            })

            Property "set" (fun (x: int[]) -> Do {
                let x = Set.ofArray x
                equalAsync (echo "Set" (Json.Serialize x) Json.Decode) x
            })

            Property "map" (fun (x: (string * int)[]) -> Do {
                let m = Map.ofArray x
                equalAsync (echo "Map" (Json.Serialize m) Json.Decode) m
            })

            Property "map with complex keys" (fun (x: (int * int)[]) -> Do {
                let m = Map.ofArray [| for k, v in x -> { WebSharper.Sitelets.Tests.Json.Types.id = k }, v |]
                equalAsync (echo "ComplexMap" (Json.Serialize m) Json.Decode) m
            })

            Property "dictionary" (fun (x: (string * int)[]) -> Do {
                let m = Map.ofArray x
                let x = Map.toArray m
                let d = Dictionary()
                do for k, v in x :> seq<_> do d.[k] <- v
                let! (r : Dictionary<string, int>) =
                    echo "Dictionary" (Json.Serialize d) Json.Decode
                let y = New []
                do for KeyValue(k, v) in r :> seq<_> do y?(k) <- v
                equal y (New (As x))
            })

            Property "dictionary with complex keys" (fun (x: (int * int)[]) -> Do {
                let d = Dictionary()
                do for k, v in x :> seq<_> do d.[{ WebSharper.Sitelets.Tests.Json.Types.id = k }] <- v
                let! (r : Dictionary<WebSharper.Sitelets.Tests.Json.Types.Id, int>) =
                    echo "ComplexDictionary" (Json.Serialize d) Json.Decode
                forEach d (fun (KeyValue(k, v)) -> Do { equal v r.[k] })
                forEach r (fun (KeyValue(k, v)) -> Do { equal v d.[k] })
            })

            Test "simple record" {
                let r (x: int) : SimpleRecord =
                    {
                        x = x
                        y = 3.14
                        z = (x, "test123")
                        t = [|"a"; "b"|]
                    }
                equalAsync (echo "SimpleRecord" (Json.Serialize (r 12)) Json.Decode) (r 12)
                equalAsync (echo "SimpleRecordArray" (Json.Serialize [|r 13; r 42|]) Json.Decode) [|r 13; r 42|]
            }

            Test "record with options" {
                let f (r: RecordWithOptions) =
                    echo "RecordOptions" (Json.Serialize r) Json.Decode<RecordWithOptions>
                let r1 = { ox = Some 1; oy = Some "2" }
                equalAsync (f r1) r1
                let r2 = { ox = None; oy = Some "2" }
                equalAsync (f r2) r2
                let r3 = { ox = Some 1; oy = None }
                equalAsync (f r3) r3
                let r4 = { ox = None; oy = None }
                equalAsync (f r4) r4
            }

            Test "simple union" {
                let f (r: SimpleUnion) =
                    echo "SimpleUnion" (Json.Serialize r) Json.Decode<SimpleUnion>
                equalAsync (f Nullary) Nullary
                equalAsync (f (Unary 1)) (Unary 1)
                equalAsync (f (Binary ("aaa", 3.14))) (Binary ("aaa", 3.14))
            }

            Test "implicit union" {
                let f (r: ImplicitUnion) =
                    echo "ImplicitUnion" (Json.Serialize r) Json.Decode<ImplicitUnion>
                equalAsync (f (Impl1 123)) (Impl1 123)
                equalAsync (f (Impl2 (2.718, "test"))) (Impl2 (2.718, "test"))
                equalAsync (f (Impl3 (45, 67))) (Impl3 (45, 67))
            }

            Test "union with inline record" {
                let f (r: UnionWithInlineRecord) =
                    echo "UnionInlineRecord" (Json.Serialize r) Json.Decode<UnionWithInlineRecord>
                equalAsync (f (NotInline {rx = 9; ry = "o"})) (NotInline {rx = 9; ry = "o"})
                equalAsync (f (Inline {rx = 9; ry = "o"})) (Inline {rx = 9; ry = "o"})
            }

            Test "generic union" {
                equalAsync (echo "GenericUnionString" (Json.Serialize (Success "x")) Json.Decode)
                    (Success "x")
                equalAsync (echo "GenericUnionRecord" (Json.Serialize (Success { rx = 9; ry = "o" })) Json.Decode)
                    (Success { rx = 9; ry = "o" })
                let fail = Failure "it failed": GenericUnion<string>
                equalAsync (echo "GenericUnionString" (Json.Serialize fail) Json.Decode) fail
            }

            Test "union with constants" {
                let f (r: UnionWithConstants) =
                    echo "UnionConstants" (Json.Serialize r) Json.Decode<UnionWithConstants>
                equalAsync (f Foo) Foo
                equalAsync (f Bar) Bar
                equalAsync (f Twelve) Twelve
                equalAsync (f Null) Null
            }

            let now = System.DateTime.Now
            let d = Date(Date.UTC(2011, 9, 5, 14, 48, 0)) // "2011-10-05T14:48:00.000Z"

            Test "System.DateTime" {
                let f (r: System.DateTime) =
                    echo "DateTime" (Json.Serialize r) Json.Decode<System.DateTime>
                equalAsync (f d.Self) d.Self
                equalAsync (f now) now
            }

            let nowO = System.DateTimeOffset(now, System.TimeSpan.FromHours 1.)
            let dO = System.DateTimeOffset(d.Self, System.TimeSpan.FromHours -1.)

            Test "System.DateTimeOffset" {
                let f (r: System.DateTimeOffset) =
                    echo "DateTimeOffset" (Json.Serialize r) Json.Decode<System.DateTimeOffset>
                equalAsync (f dO) dO
                equalAsync (f nowO) nowO
            }

            Test "Queue" {
                let f (r: Queue<int>) =
                    echo "Queue" (Json.Serialize r) Json.Decode<Queue<int>>
                equalAsync (f (Queue())) (Queue())
                equalAsync (f (Queue [34; 5; 58])) (Queue [34; 5; 58])
            }

            Test "Stack" {
                let f (r: Stack<int>) =
                    echo "Stack" (Json.Serialize r) Json.Decode<Stack<int>>
                equalAsync (f (Stack())) (Stack())
                equalAsync (f (Stack [34; 5; 58])) (Stack [34; 5; 58])
            }

            Test "LinkedList" {
                let f (r: LinkedList<int>) =
                    echo "LinkedList" (Json.Serialize r) Json.Decode<LinkedList<int>>
                let! res = f (LinkedList())
                equal (List.ofSeq res) []
                let! res2 = f (LinkedList [34; 5; 58])
                equal (List.ofSeq res2) [34; 5; 58]
            }

            Test "obj" {
                equalMsgAsync (echoObj jsonBaseUri null) null "null"
                property (fun (x: int) -> Do { equalMsgAsync (echoObj jsonBaseUri x) x "int" })
                property (fun (x: float) -> Do { approxEqualMsgAsync (echoObj jsonBaseUri x) x "float" })
                property (fun (x: string) -> Do { equalMsgAsync (echoObj jsonBaseUri x) x "string" })
                property (fun (x: int[]) -> Do { equalMsgAsync (echoObj jsonBaseUri x) x "array" })
                property (fun (x: (string * int)[]) -> Do {
                    let x : obj = New (As x)
                    equalMsgAsync (echoObj jsonBaseUri x) x "object"
                })
            }
        }
