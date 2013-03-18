module Website.Controls

open IntelliFactory.WebSharper
open IntelliFactory.WebSharper.Html

[<Sealed>]
type EntryPoint() =
    inherit Web.Control()

    [<JavaScript>]
    override this.Body =
        Client.EntryPoint()
        Span [] :> _
