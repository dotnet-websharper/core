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

[<Sealed>]
type Tests() =
    inherit Web.Control()

    [<JavaScript>]
    override this.Body =
        do IntelliFactory.WebSharper.Tests.Regression.Tests
        Span [] :> _
