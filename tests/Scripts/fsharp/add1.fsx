#load "../FSharpTester.fsx"

let source = """
namespace WebSharper.Tests

open WebSharper

[<JavaScript>]
module Test =

    let add1 x = x + 1
    let test = add1 41
"""

FSharpTester.toJSFiles source
