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
#nowarn "44" // Internal calls to obsolete methods

open System
open System.Runtime.CompilerServices
open System.Runtime.InteropServices
open Microsoft.AspNetCore.Builder
open Microsoft.AspNetCore.Hosting
open Microsoft.Extensions.Configuration
open Microsoft.Extensions.Logging
open WebSharper.Sitelets

[<Extension>]
type ApplicationBuilderExtensions =

    // Remoting                        

    [<Extension>]
    [<Obsolete "Use UseWebSharper(Action<WebSharperBuilder>)">]
    static member UseWebSharperRemoting(this: IApplicationBuilder, options: WebSharperOptions) =
        //for s in this.ApplicationServices.GetServices<IRemotingService>() do
        //    s.Register()
        this.Use(Remoting.Middleware options)

    [<Extension>]
    [<Obsolete "Use UseWebSharper(Action<WebSharperBuilder>)">]
    static member UseWebSharperRemoting
        (
            this: IApplicationBuilder,
            _env: IHostingEnvironment,
            [<Optional>] config: IConfiguration,
            [<Optional>] binDir: string
        ) =
        WebSharperOptions.Create(this.ApplicationServices, None, None, None, Option.ofObj config, None, Option.ofObj binDir, false, true, fun _ _ -> ())
        |> this.UseWebSharperRemoting

    // Sitelets

    [<Extension>]
    [<Obsolete "Use UseWebSharper(Action<WebSharperBuilder>)">]
    static member UseWebSharperSitelets(this: IApplicationBuilder, options: WebSharperOptions) =
        this.Use(Sitelets.Middleware options)

    [<Extension>]
    [<Obsolete "Use UseWebSharper(Action<WebSharperBuilder>)">]
    static member UseWebSharperSitelets<'T when 'T : equality>
        (
            this: IApplicationBuilder,
            _env: IHostingEnvironment,
            [<Optional>] sitelet: Sitelet<'T>,
            [<Optional>] config: IConfiguration,
            [<Optional>] binDir: string
        ) =
        WebSharperOptions.Create(this.ApplicationServices, sitelet, config, null, binDir, true, false)
        |> this.UseWebSharperSitelets

    // All

    [<Extension>]
    [<Obsolete "Use UseWebSharper(Action<WebSharperBuilder>)">]
    static member UseWebSharper(this: IApplicationBuilder, options: WebSharperOptions) =
        if options.UseRemoting then this.UseWebSharperRemoting(options) |> ignore
        if options.UseSitelets then this.UseWebSharperSitelets(options) |> ignore
        options.UseExtension this options
        this

    [<Extension>]
    [<Obsolete "Use UseWebSharper(Action<WebSharperBuilder>)">]
    static member UseWebSharper
        (
            this: IApplicationBuilder,
            _env: IHostingEnvironment,
            [<Optional>] sitelet: Sitelet<'T>,
            [<Optional>] config: IConfiguration,
            [<Optional>] binDir: string
        ) =
        WebSharperOptions.Create(this.ApplicationServices, sitelet, config, null, binDir, true, true)
        |> this.UseWebSharper

    /// Use the WebSharper server side.
    [<Extension>]
    static member UseWebSharper
        (
            this: IApplicationBuilder,
            [<Optional>] build: Action<WebSharperBuilder>
        ) =
        let builder = WebSharperBuilder(this.ApplicationServices)
        if not (isNull build) then build.Invoke(builder)
        this.UseWebSharper(builder.Build())
