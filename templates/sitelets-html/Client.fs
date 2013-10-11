namespace $safeprojectname$

open IntelliFactory.WebSharper
open IntelliFactory.WebSharper.Html

[<JavaScript>]
module Client =

    let Main () =
        let input = Input [Text ""]
        let label = Div [Text ""]
        Div [
            input
            label
            Button [Text "Click"]
            |>! OnClick (fun _ _ ->
                label.Text <- "You entered: " + input.Value)
        ]
