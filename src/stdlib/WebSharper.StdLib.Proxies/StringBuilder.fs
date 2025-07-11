namespace WebSharper

open System.Text
open WebSharper.JavaScript

module StringBuilder =

    open WebSharper

    [<Proxy(typeof<StringBuilder>)>]
    type StringBuilderProxy() =
        let mutable strings = [||]
        
        new (init: string) as this = StringBuilderProxy() then this.Append(init) |> ignore

        member this.Append(part: string) =
            strings.JS.Push(part) |> ignore
            this |> As<System.Text.StringBuilder>
        
        [<Inline>]
        member this.Append(part: int) =
            this.Append(string part)
        
        [<Inline>]
        member this.Append(part: int64) =
            this.Append(string part)
        
        [<Inline>]
        member this.Append(part: int16) =
            this.Append(string part)
        
        [<Inline>]
        member this.Append(part: int8) =
            this.Append(string part)
        
        [<Inline>]
        member this.Append(part: uint) =
            this.Append(string part)
        
        [<Inline>]
        member this.Append(part: uint64) =
            this.Append(string part)
        
        [<Inline>]
        member this.Append(part: uint16) =
            this.Append(string part)
        
        [<Inline>]
        member this.Append(part: uint8) =
            this.Append(string part)
        
        [<Inline>]
        member this.Append(part: single) =
            this.Append(string part)
        
        [<Inline>]
        member this.Append(part: double) =
            this.Append(string part)
        
        [<Inline>]
        member this.Append(part: obj) =
            this.Append(string part)
        
        [<Inline>]
        member this.Append(part: bool) =
            this.Append(string part)
        
        [<Inline>]
        member this.Append(part: char) =
            this.Append(string part)
        
        member this.AppendLine(part: string) =
            strings.JS.Push(part) |> ignore
            strings.JS.Push("\n") |> ignore
            this |> As<System.Text.StringBuilder>
        
        [<Inline>]
        member this.AppendLine(part: int) =
            this.AppendLine(string part)
        
        [<Inline>]
        member this.AppendLine(part: int64) =
            this.AppendLine(string part)
        
        [<Inline>]
        member this.AppendLine(part: int16) =
            this.AppendLine(string part)
        
        [<Inline>]
        member this.AppendLine(part: int8) =
            this.AppendLine(string part)
        
        [<Inline>]
        member this.AppendLine(part: uint) =
            this.AppendLine(string part)
        
        [<Inline>]
        member this.AppendLine(part: uint64) =
            this.AppendLine(string part)
        
        [<Inline>]
        member this.AppendLine(part: uint16) =
            this.AppendLine(string part)
        
        [<Inline>]
        member this.AppendLine(part: uint8) =
            this.AppendLine(string part)
        
        [<Inline>]
        member this.AppendLine(part: bool) =
            this.AppendLine(string part)
        
        [<Inline>]
        member this.AppendLine(part: obj) =
            this.AppendLine(string part)
        
        [<Inline>]
        member this.AppendLine(part: double) =
            this.AppendLine(string part)
        
        [<Inline>]
        member this.AppendLine(part: single) =
            this.AppendLine(string part)
        
        [<Inline>]
        member this.AppendLine(part: char) =
            this.AppendLine(string part)
        
        override this.ToString() =
            let res = strings.JS.Join("")
            strings <- [| res |]
            res

        member this.Chars
            with get i =
                let s = this.ToString()
                s[i]
            and set i (v: char) =
                let s = this.ToString()
                if i < 0 || i >= s.Length then 
                    raise (System.ArgumentOutOfRangeException())
                let ns = s.JS.Slice(0, i) + string v + s.JS.Slice(i + 1)
                strings <- [| ns |]

        member this.Clear() =
            strings <- [||]
            this |> As<System.Text.StringBuilder>

        member this.Insert(i: int, part: string) =
            let s = this.ToString()
            if i < 0 || i > s.Length then 
                raise (System.ArgumentOutOfRangeException())
            let ns = s.JS.Slice(0, i) + part + s.JS.Slice(i)
            strings <- [| ns |]
            this |> As<System.Text.StringBuilder>

        [<Inline>]
        member this.Insert(i: int, part: int) =
            this.Insert(i, string part)
        
        [<Inline>]
        member this.Insert(i: int, part: int64) =
            this.Insert(i, string part)
        
        [<Inline>]
        member this.Insert(i: int, part: int16) =
            this.Insert(i, string part)
        
        [<Inline>]
        member this.Insert(i: int, part: int8) =
            this.Insert(i, string part)
        
        [<Inline>]
        member this.Insert(i: int, part: uint) =
            this.Insert(i, string part)
        
        [<Inline>]
        member this.Insert(i: int, part: uint64) =
            this.Insert(i, string part)
        
        [<Inline>]
        member this.Insert(i: int, part: uint16) =
            this.Insert(i, string part)
        
        [<Inline>]
        member this.Insert(i: int, part: uint8) =
            this.Insert(i, string part)
        
        [<Inline>]
        member this.Insert(i: int, part: single) =
            this.Insert(i, string part)
        
        [<Inline>]
        member this.Insert(i: int, part: double) =
            this.Insert(i, string part)
        
        [<Inline>]
        member this.Insert(i: int, part: obj) =
            this.Insert(i, string part)
        
        [<Inline>]
        member this.Insert(i: int, part: bool) =
            this.Insert(i, string part)
        
        [<Inline>]
        member this.Insert(i: int, part: char) =
            this.Insert(i, string part)

        member this.Remove(i: int, l: int) =
            let s = this.ToString()
            if i < 0 || l < 0 || i + l > s.Length then 
                raise (System.ArgumentOutOfRangeException())
            let ns = s.JS.Slice(0, i) + s.JS.Slice(i + l)
            strings <- [| ns |]
            this |> As<System.Text.StringBuilder>    
            
        member this.Replace(search: string, replace: string) =
            let s = this.ToString()
            let ns = s.Replace(search, replace)
            strings <- [| ns |]
            this |> As<System.Text.StringBuilder>    

        [<Inline>]
        member this.Replace(search: char, replace: char) =
            this.Replace(string search, string replace)

        member this.Replace(search: string, replace: string, i: int, l: int) =
            let s = this.ToString()
            if i < 0 || l < 0 || i + l > s.Length then 
                raise (System.ArgumentOutOfRangeException())
            let ns = s.JS.Slice(0, i) + s.JS.Slice(i, l).Replace(search, replace) + s.JS.Slice(i + l)
            strings <- [| ns |]
            this |> As<System.Text.StringBuilder>    

        [<Inline>]
        member this.Replace(search: char, replace: char, i: int, l: int) =
            this.Replace(string search, string replace, i, l)

        member this.Length =
            strings |> Array.sumBy (fun s -> s.Length)




