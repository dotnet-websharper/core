namespace MyApplication

open WebSharper

[<JavaScript>]
module Model =
    type login =
        {
            username: string
            password: string
        }
        static member Default =
            { username=""; password=""; }
    
    
