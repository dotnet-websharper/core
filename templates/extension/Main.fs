namespace $safeprojectname$

open IntelliFactory.WebSharper

module Definition =
    open IntelliFactory.WebSharper.InterfaceGenerator

    let I1 =
        Interface "I1"
        |+> [
                "test1" => T<string> ^-> T<string>
                "radius1" =@ T<float>
            ]

    let I2 =
        Generic / fun t1 t2 ->
            Interface "I2"
            |+> [
                    Generic - fun m1 -> "foo" => m1 * t1 ^-> t2
                ]

    let C1 =
        let C1T = Type.New ()
        Class "C1"
        |=> C1T
        |+> Protocol [
                "foo" =% T<int>
            ]
        |+> [
                Constructor (T<unit> + T<int>)
                "mem"   => (T<unit> + T<int> ^-> T<unit>)
                "test2" => (C1T -* T<int> ^-> T<unit>) * T<string> ^-> T<string>
                "radius2" =@ T<float>
                |> WithSourceName "R2"
                "length"   =% T<int>
                |> WithSourceName "L2"
            ]

    let Assembly =
        Assembly [
            Namespace "Extension1" [
                 I1
                 Generic - I2
                 C1
            ]
        ]

open IntelliFactory.WebSharper.InterfaceGenerator

[<Sealed>]
type Extension() =
    interface IExtension with
        member ext.Assembly =
            Definition.Assembly

[<assembly: Extension(typeof<Extension>)>]
do ()
