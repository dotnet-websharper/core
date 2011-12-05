namespace IntelliFactory.WebSharper.Sitelets
open System

/// Provides Sitelets functionality
module PageResponder  =

    open System.IO
    open System.Web
    open System.Text.RegularExpressions
    open IntelliFactory.WebSharper
    open IntelliFactory.WebSharper.Web

    let private Escape (s: string) =
        Regex.Replace(s, @"[&<>']",
            new MatchEvaluator(fun m ->
                match m.Groups.[0].Value.[0] with
                | '&'   -> "&amp;"
                | '<'   -> "&lt;"
                | '>'   -> "&gt;"
                | '\''  -> "&#39;"
                | _     -> failwith "unreachable")
            )

    let PageResponse (genPage: Context<'Action> -> Page)  : Content<'Action> = // (context : Context<'Action>) : Http.Response =
        Content.CustomContent <| fun context ->
            let htmlPage = genPage context
            let writeBody (stream: Stream) =

                // Finds all the client side controls on the page.
                let controls =
                    htmlPage.Body
                    |> Seq.collect (fun elem ->
                        elem.CollectAnnotations ()
                    )

                // Resolve resources for the set of types and this assembly
                let resources =
                    controls
                    |> Seq.map (fun x -> x.GetType())
                    |> Seq.distinct
                    |> Seq.map (fun t ->
                        IntelliFactory.PowerPack.MetaId.MemberId.Of t
                        |> Resources.MemberNode
                    )
                    |> context.Metadata.DependencyResolver.GetDependencies

                // Meta tag encoding the client side controls
                let meta =
                    let json = 
                        Lazy.Create (fun _ -> context.Metadata)
                        |> IntelliFactory.WebSharper.Remoting.Json.New
                    let id = Settings.WebSharperDataName
                    let name = Settings.WebSharperDataName
                    controls
                    |> Seq.map (fun c -> c.ID, c)
                    |> Array.ofSeq
                    |> json.Stringify
                    |> Escape
                    |> sprintf "<meta id='%s' name='%s' content='%s' />" id name

                let renderHead (tw: UI.HtmlTextWriter) =
                    // Render meta
                    tw.WriteLine meta
                    // Render resources
                    for r in resources do
                        r.WriteHtml context.ResourceContext tw

                let renderBody (tw: UI.HtmlTextWriter) =
                    let writer = new IntelliFactory.Html.Html.Writer(tw)
                    for elem in htmlPage.Body do
                        writer.Write elem

                // Create html writer from stream
                use textWriter = new StreamWriter(stream)
                textWriter.AutoFlush <- true
                use htmlWriter = new System.Web.UI.HtmlTextWriter(textWriter)
                htmlPage.Template htmlPage.Title renderHead renderBody htmlWriter
            {
                Status = Http.Status.Ok
                Headers = []
                WriteBody = writeBody
            }
