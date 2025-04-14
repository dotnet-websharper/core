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
namespace WebSharper.AspNetCore

open Microsoft.Extensions.DependencyInjection
open Microsoft.Extensions.Configuration
#nowarn "44" // Internal calls to obsolete methods

open System
open System.Diagnostics
open System.Runtime.CompilerServices
open System.Runtime.InteropServices
open System.Reflection
open Microsoft.AspNetCore.Builder

open WebSharper.Constants

[<Extension>]
type ApplicationBuilderExtensions =

    /// Use the WebSharper server side.
    [<Extension>]
    static member UseWebSharper
        (
            this: IApplicationBuilder,
            build: Action<WebSharperBuilder>
        ) =
        let builder = WebSharperBuilder(this.ApplicationServices)
        if not (isNull build) then build.Invoke(builder)
        let options = builder.Build()
        if options.UseRemoting then 
            this.Use(Remoting.Middleware options) |> ignore
        if options.UseSitelets then 
            this.Use(Sitelets.Middleware options) |> ignore
        options.UseExtension this options
        this

    /// Use the WebSharper server side.
    [<Extension>]
    static member UseWebSharper
        (
            this: IApplicationBuilder
        ) =
        ApplicationBuilderExtensions.UseWebSharper(this, null)

    /// Use the WebSharper server side for remoting only.
    [<Extension>]
    static member UseWebSharperRemoting
        (
            this: IApplicationBuilder,
            build: Action<WebSharperBuilder>,
            headers: (string * string) []
        ) =
        ApplicationBuilderExtensions.UseWebSharper(this, fun builder ->
            builder.UseSitelets(false) |> ignore
            builder.UseRemoting(true, headers) |> ignore
            if not (isNull build) then build.Invoke(builder)
        )

    /// Use the WebSharper server side for remoting only.
    [<Extension>]
    static member UseWebSharperRemoting
        (
            this: IApplicationBuilder
        ) =
        ApplicationBuilderExtensions.UseWebSharperRemoting(this, null, [||])

    /// Use the WebSharper server side for remoting only.
    [<Extension>]
    static member UseWebSharperRemoting
        (
            this: IApplicationBuilder,
            build: Action<WebSharperBuilder>
        ) =
        ApplicationBuilderExtensions.UseWebSharperRemoting(this, build, [||])

    /// Use the WebSharper server side for remoting only.
    [<Extension>]
    static member UseWebSharperRemoting
        (
            this: IApplicationBuilder,
            headers: (string * string) []
        ) =
        ApplicationBuilderExtensions.UseWebSharperRemoting(this, null, headers)

    /// Use the WebSharper server side for sitelets only.
    [<Extension>]
    static member UseWebSharperSitelets
        (
            this: IApplicationBuilder,
            build: Action<WebSharperBuilder>
        ) =
        ApplicationBuilderExtensions.UseWebSharper(this, fun builder ->
            builder.UseRemoting(false) |> ignore
            if not (isNull build) then build.Invoke(builder)
        )

    [<Extension>]
    static member UseWebSharperSitelets
        (
            this: IApplicationBuilder
        ) =
        ApplicationBuilderExtensions.UseWebSharperSitelets(this, null)

    /// Use vite for JavaScript localhost debugging.
    [<Extension>]
    static member UseWebSharperScriptRedirect
        (
            this: IApplicationBuilder,
            redirectUrlRoot: string,
            startVite: bool
        ) =

        let redirectUrlRoot =
            if isNull redirectUrlRoot then 
                let config = this.ApplicationServices.GetRequiredService<IConfiguration>().GetSection("websharper")
                let fromConfig = config.Item(RUNTIMESETTING_DEBUGSCRIPTREDIRECTURL)
                if isNull fromConfig then
                    "http://localhost:5173"
                else
                    fromConfig
            else
                redirectUrlRoot
        if startVite then
            let proc = 
                new Process(
                    StartInfo = new ProcessStartInfo(
                        FileName = "npx",
                        Arguments = $"vite --port {System.Uri(redirectUrlRoot).Port} --strictPort true",
                        UseShellExecute = true
                    )
                )
            printfn $"Starting vite on port {System.Uri(redirectUrlRoot).Port}"
            proc.Start() |> ignore
        this.Use(ScriptRedirect.Middleware redirectUrlRoot) |> ignore
        this

    /// Use vite for JavaScript localhost debugging.
    [<Extension>]
    static member UseWebSharperScriptRedirect
        (
            this: IApplicationBuilder
        ) =
        ApplicationBuilderExtensions.UseWebSharperScriptRedirect(this, null, false)

    /// Use vite for JavaScript localhost debugging.
    [<Extension>]
    static member UseWebSharperScriptRedirect
        (
            this: IApplicationBuilder,
            redirectUrlRoot: string
        ) =
        ApplicationBuilderExtensions.UseWebSharperScriptRedirect(this, redirectUrlRoot, false)

    /// Use vite for JavaScript localhost debugging.
    [<Extension>]
    static member UseWebSharperScriptRedirect
        (
            this: IApplicationBuilder,
            startVite: bool
        ) =
        ApplicationBuilderExtensions.UseWebSharperScriptRedirect(this, null, startVite)

