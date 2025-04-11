namespace WebSharper

open System.Text
open WebSharper.JavaScript

module StringBuilder =

    open WebSharper

    [<Proxy(typeof<StringBuilder>)>]
    type StringBuilderProxy() =
        let lines = WebSharper.JavaScript.Array()
        let mutable currentLine = ""
        member this.Append(part: string) =
            currentLine <- currentLine + part
            this |> As<System.Text.StringBuilder>
        member this.Append(part: int) =
            currentLine <- currentLine + string part
            this |> As<System.Text.StringBuilder>
        member this.Append(part: int64) =
            currentLine <- currentLine + string part
            this |> As<System.Text.StringBuilder>
        member this.Append(part: int16) =
            currentLine <- currentLine + string part
            this |> As<System.Text.StringBuilder>
        member this.Append(part: int8) =
            currentLine <- currentLine + string part
            this |> As<System.Text.StringBuilder>
        member this.Append(part: uint) =
            currentLine <- currentLine + string part
            this |> As<System.Text.StringBuilder>
        member this.Append(part: uint64) =
            currentLine <- currentLine + string part
            this |> As<System.Text.StringBuilder>
        member this.Append(part: uint16) =
            currentLine <- currentLine + string part
            this |> As<System.Text.StringBuilder>
        member this.Append(part: uint8) =
            currentLine <- currentLine + string part
            this |> As<System.Text.StringBuilder>
        member this.Append(part: single) =
            currentLine <- currentLine + string part
            this |> As<System.Text.StringBuilder>
        member this.Append(part: double) =
            currentLine <- currentLine + string part
            this |> As<System.Text.StringBuilder>
        member this.Append(part: obj) =
            currentLine <- currentLine + string part
            this |> As<System.Text.StringBuilder>
        member this.Append(part: bool) =
            currentLine <- currentLine + string part
            this |> As<System.Text.StringBuilder>
        member this.Append(part: char) =
            currentLine <- currentLine + string part
            this |> As<System.Text.StringBuilder>
        member this.AppendLine(part) =
            currentLine <- currentLine + part
            lines.Push(currentLine) |> ignore
            currentLine <- ""
            this |> As<System.Text.StringBuilder>
        member this.AppendLine(part: int) =
            currentLine <- currentLine + string part
            lines.Push(currentLine) |> ignore
            currentLine <- ""
            this |> As<System.Text.StringBuilder>
        member this.AppendLine(part: int64) =
            currentLine <- currentLine + string part
            lines.Push(currentLine) |> ignore
            currentLine <- ""
            this |> As<System.Text.StringBuilder>
        member this.AppendLine(part: int16) =
            currentLine <- currentLine + string part
            lines.Push(currentLine) |> ignore
            currentLine <- ""
            this |> As<System.Text.StringBuilder>
        member this.AppendLine(part: int8) =
            currentLine <- currentLine + string part
            lines.Push(currentLine) |> ignore
            currentLine <- ""
            this |> As<System.Text.StringBuilder>
        member this.AppendLine(part: uint) =
            currentLine <- currentLine + string part
            lines.Push(currentLine) |> ignore
            currentLine <- ""
            this |> As<System.Text.StringBuilder>
        member this.AppendLine(part: uint64) =
            currentLine <- currentLine + string part
            lines.Push(currentLine) |> ignore
            currentLine <- ""
            this |> As<System.Text.StringBuilder>
        member this.AppendLine(part: uint16) =
            currentLine <- currentLine + string part
            lines.Push(currentLine) |> ignore
            currentLine <- ""
            this |> As<System.Text.StringBuilder>
        member this.AppendLine(part: uint8) =
            currentLine <- currentLine + string part
            lines.Push(currentLine) |> ignore
            currentLine <- ""
            this |> As<System.Text.StringBuilder>
        member this.AppendLine(part: bool) =
            currentLine <- currentLine + string part
            lines.Push(currentLine) |> ignore
            currentLine <- ""
            this |> As<System.Text.StringBuilder>
        member this.AppendLine(part: obj) =
            currentLine <- currentLine + string part
            lines.Push(currentLine) |> ignore
            currentLine <- ""
            this |> As<System.Text.StringBuilder>
        member this.AppendLine(part: double) =
            currentLine <- currentLine + string part
            lines.Push(currentLine) |> ignore
            currentLine <- ""
            this |> As<System.Text.StringBuilder>
        member this.AppendLine(part: single) =
            currentLine <- currentLine + string part
            lines.Push(currentLine) |> ignore
            currentLine <- ""
            this |> As<System.Text.StringBuilder>
        member this.AppendLine(part: char) =
            currentLine <- currentLine + string part
            lines.Push(currentLine) |> ignore
            currentLine <- ""
            this |> As<System.Text.StringBuilder>
        override this.ToString() =
            if currentLine <> "" then
                lines.Push(currentLine) |> ignore
            lines.Join("\n")




