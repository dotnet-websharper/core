#load "../FSharpTester.fsx"

let source = """
namespace WebSharper.Tests

open WebSharper

[<JavaScript>]
module ExtensionOperators =
    type System.String with
        static member ( * ) (s: string, n: int) = String.replicate n s

    [<Inline>]
    let inline multiply (x: ^T) (n: int) = x * n

    let generic n = multiply "ha" n
"""

FSharpTester.toJSFiles source
