namespace IntelliFactory.Html.Web

[<AbstractClass>]
type Control() =
    inherit System.Web.UI.Control()

    abstract member Element : IntelliFactory.Html.Html.Element<unit>

    override this.Render writer =
        let w = new IntelliFactory.Html.Html.Writer(writer)
        w.Write this.Element
