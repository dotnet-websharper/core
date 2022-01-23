namespace WebSharper.StaticHtml.Tests.NetStandard

open WebSharper
open WebSharper.JavaScript
open System

[<JavaScript>]
module Client =

    type Node =
        | Elt of string * Node[]
        | Text of string
        | Attr of string * string

        member this.ToNode() =
            match this with
            | Text t -> Choice1Of2 (JS.Document.CreateTextNode(t) :> Dom.Node)
            | Elt (n, ch) ->
                let e = JS.Document.CreateElement(n)
                for ch in ch do
                    match ch.ToNode() with
                    | Choice1Of2 n -> e.AppendChild(n) |> ignore
                    | Choice2Of2 a -> e.SetAttributeNode(a) |> ignore
                Choice1Of2 (e :> Dom.Node)
            | Attr (n, v) -> Choice2Of2 (JS.Document.CreateAttribute(n, Value = v))

        interface IControlBody with
            member this.ReplaceInDom x =
                match this.ToNode() with
                | Choice1Of2 n -> x.ParentNode.ReplaceChild(n, x) |> ignore
                | Choice2Of2 _ -> x.ParentNode.RemoveChild(x) |> ignore

    let Elt n ([<ParamArray>] ch) = Node.Elt(n, ch)

    let Text t = Node.Text(t)

    let Main () =

        Elt "div" [| Text "And this too." |]
