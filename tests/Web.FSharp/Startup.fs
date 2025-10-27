open Microsoft.AspNetCore
open Microsoft.AspNetCore.Builder
open Microsoft.AspNetCore.Hosting
open Microsoft.AspNetCore.Http
open Microsoft.Extensions.Configuration
open Microsoft.Extensions.DependencyInjection
open Microsoft.Extensions.Hosting
open System
open System.Collections.Generic
open System.Linq
open System.Threading.Tasks
open WebSharper.AspNetCore

type Startup () =
    
    let site = new WebSharper.Tests.Website.WebsiteEntryPoint()
    let siteletRef = ref site.Sitelet
    
    member x.ConfigureServices(services: IServiceCollection) =
        services.AddWebSharperServices().AddSiteletRef(siteletRef) |> ignore
        services
            .AddAuthentication("WebSharper")
            .AddCookie("WebSharper")
        |> ignore

    member x.Configure(app: IApplicationBuilder, env: IWebHostEnvironment) =
        if env.IsDevelopment() then
            app.UseDeveloperExceptionPage() |> ignore

        app.UseAuthentication()
            .UseWebSharperScriptRedirect(startVite = true)
            .UseStaticFiles()
            .UseWebSharper()
            .Run(fun context ->
                context.Response.StatusCode <- 404
                context.Response.WriteAsync("Page not found"))

[<EntryPoint>]
let main args =
    WebHost.CreateDefaultBuilder(args)
        .UseStartup<Startup>()
        .Build()
        .Run()
    0
