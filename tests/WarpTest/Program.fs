// $begin{copyright}
//
// This file is part of WebSharper
//
// Copyright (c) 2008-2018 IntelliFactory
//
// Licensed under the Apache License, Version 2.0 (the "License"); you
// may not use this file except in compliance with the License.  You may
// obtain a copy of the License at
//
//     http://www.apache.org/licenses/LICENSE-2.0
//
// Unless required by applicable law or agreed to in writing, software
// distributed under the License is distributed on an "AS IS" BASIS,
// WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or
// implied.  See the License for the specific language governing
// permissions and limitations under the License.
//
// $end{copyright}
open WebSharper
open WebSharper.Html.Server
let app =
    Warp.CreateSPA(fun _ ->
        [
            Div [
                Testing.Runner.Run [
                    typeof<WebSharper.Collections.Tests.Dictionary.Foo>.Assembly
                    typeof<WebSharper.Tests.AddMacro>.Assembly
                    typeof<WebSharper.Web.Tests.HelloWorld>.Assembly
                    typeof<WebSharper.Html5.Tests.Samples>.Assembly
                ]
            ]
        ])
    |> Warp.Run
printfn "Started."
do System.Diagnostics.Process.Start("http://localhost:9000") |> ignore
System.Console.ReadLine() |> ignore
app.Stop()