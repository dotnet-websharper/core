#load "../FSharpTester.fsx"

let source = """
namespace WebSharper.Tests

open WebSharper

[<JavaScript; Struct>]
type StructRecord = { Value: int; mutable Count: int }

[<JavaScript>]
module RecordSpreads =
    let spread (source: StructRecord) = struct {| ...source; Label = "label" |}
"""

FSharpTester.toJSFiles source
