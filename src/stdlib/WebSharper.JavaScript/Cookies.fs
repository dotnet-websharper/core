module WebSharper.JavaScript.Cookies.Definition

open WebSharper.InterfaceGenerator
open WebSharper.JavaScript

let Res = Resource "Cookies" "Cookies.js"

let Options =
    Pattern.Config "Cookies.Options" {
        Required = []
        Optional =
            [
                "path", T<string>
                "domain", T<string>
                "expires", Ecma.Definition.EcmaDate.Type
                "secure", T<bool>
            ]
    }

let Cookies =
    Class "Cookies"
    |=> Nested [Options]
    |+> Static [
        "get" => T<string>?key ^-> T<option<string>>
        |> WithInline "$wsruntime.GetOptional(Cookies.get($key))"
        "set" => T<string>?key * T<string>?value * !?Options ^-> T<unit>
        "expire" => T<string>?key * !?Options ^-> T<unit>
        "defaults" =? Options
        "enabled" =? T<bool>
    ]
    |> Requires [Res]

let Namespaces =
    [
        Namespace "WebSharper.JavaScript" [
            Cookies
        ]
        Namespace "WebSharper.JavaScript.Resources" [
            Res
        ]
    ]