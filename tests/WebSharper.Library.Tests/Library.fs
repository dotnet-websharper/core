namespace WebSharper.Library.Tests

open WebSharper

[<JavaScript>]
type Arith = 
    static member Add x y = x + y

    static member Concat (x: string) y = x + y