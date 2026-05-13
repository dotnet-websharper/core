#load "../FSharpTester.fsx"

let source = """
namespace WebSharper.Tests

open WebSharper
open WebSharper.JavaScript
open System.Collections.Generic

[<JavaScript>]
module Test =

    let test =
        let inline (+@) x y = x + x * y
        printfn "%d" (1 +@ 1)
        // Call that uses float.
        printfn "%f" (1.0 +@ 0.5)
"""

FSharpTester.toJSFiles source
