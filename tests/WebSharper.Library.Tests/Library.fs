namespace WebSharper.Library.Tests

open WebSharper

[<JavaScript>]
type Arith = 
    static member Add x y = x + y

    static member Concat (x: string) y = x + y

    static member DictAddAndLookup (x: string) =
        let d = System.Collections.Generic.Dictionary<string, int>()
        d.Add(x, 1)
        match d.TryGetValue(x) with
        | true, v -> v
        | false, _ -> 0