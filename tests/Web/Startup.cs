using Microsoft.AspNetCore;
using Microsoft.AspNetCore.Builder;
using Microsoft.AspNetCore.Hosting;
using Microsoft.AspNetCore.Http;
using Microsoft.Extensions.Configuration;
using Microsoft.Extensions.DependencyInjection;
using Microsoft.Extensions.Hosting;
using Microsoft.Extensions.Logging.Console;
using System;
using System.Collections.Generic;
using System.Linq;
using System.Threading.Tasks;
using WebSharper.AspNetCore;

namespace Web
{
    public class Startup
    {
        public void ConfigureServices(IServiceCollection services)
        {
            var site = new WebSharper.Tests.Website.WebsiteEntryPoint();
            services
                .AddWebSharper()
                .AddSitelet(site.Sitelet)
                .AddAuthentication("WebSharper")
                .AddCookie("WebSharper", options => { });
        }

        public void Configure(IApplicationBuilder app, IWebHostEnvironment env)
        {
            if (env.IsDevelopment()) { app.UseDeveloperExceptionPage(); }

            app.UseAuthentication()
                .UseStaticFiles()
                .UseWebSharper()
                .Run(context =>
                {
                    context.Response.StatusCode = 404;
                    return context.Response.WriteAsync("Page not found");
                });
        }

        public static void Main(string[] args)
        {
            WebHost.CreateDefaultBuilder(args)
                .UseStartup<Startup>()
                .Build()
                .Run();
        }
    }
}
