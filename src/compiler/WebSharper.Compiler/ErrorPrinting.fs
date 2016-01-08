module WebSharper.Compiler.ErrorPrinting

let private stringThatIsAProxyForANewlineInFlatErrors = string (char 29)
let NormalizeErrorString (text : string) =    
    if text = null then nullArg "text"
    let text = text.Trim()

    let buf = System.Text.StringBuilder()
    let mutable i = 0
    while i < text.Length do
        let delta = 
            match text.[i] with
            | '\r' when i + 1 < text.Length && text.[i + 1] = '\n' ->
                // handle \r\n sequence - replace it with one single space
                buf.Append(stringThatIsAProxyForANewlineInFlatErrors) |> ignore
                2
            | '\n' ->
                buf.Append(stringThatIsAProxyForANewlineInFlatErrors) |> ignore
                1
            | c ->
                // handle remaining chars: control - replace with space, others - keep unchanged
                let c = if System.Char.IsControl(c) then ' ' else c
                buf.Append(c) |> ignore
                1
        i <- i + delta
    buf.ToString()

