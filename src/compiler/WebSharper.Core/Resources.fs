// $begin{copyright}
//
// This file is part of WebSharper
//
// Copyright (c) 2008-2016 IntelliFactory
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

module WebSharper.Core.Resources

open System
open System.IO
open System.Reflection
open System.Web
open System.Web.UI
module CT = ContentTypes

type Rendering =
    | RenderInline of string
    | RenderLink of string
    | Skip

type MediaType =
    | Css
    | Js

type RenderLocation =
    | Scripts
    | Styles
    | Meta

    static member ForMediaType t =
        match t with
        | Js -> Scripts
        | Css -> Styles

type Context =
    {
        DebuggingEnabled : bool
        DefaultToHttp : bool
        GetAssemblyRendering : string -> Rendering
        GetSetting : string -> option<string>
        GetWebResourceRendering : Type -> string -> Rendering
        RenderingCache : System.Collections.Concurrent.ConcurrentDictionary<IResource, (RenderLocation -> HtmlTextWriter) -> unit>
        ResourceDependencyCache : System.Collections.Concurrent.ConcurrentDictionary<Metadata.Node Set, IResource list>
    }

and IResource =
    abstract member Render : Context -> ((RenderLocation -> HtmlTextWriter) -> unit)

let cleanLink dHttp (url: string) =
    if dHttp && url.StartsWith("//")
        then "http:" + url
        else url

let link dHttp (html: HtmlTextWriter) (url: string) =
    html.AddAttribute("type", CT.Text.Css.Text)
    html.AddAttribute("rel", "stylesheet")
    html.AddAttribute("href", cleanLink dHttp url)
    html.RenderBeginTag "link"
    html.RenderEndTag()
    html.WriteLine()

let inlineStyle (html: HtmlTextWriter) (text: string) =
    html.AddAttribute("type", CT.Text.Css.Text)
    html.RenderBeginTag "style"
    html.Write(text)
    html.RenderEndTag()
    html.WriteLine()

let script dHttp (html: HtmlTextWriter) (url: string) =
    html.AddAttribute("src", cleanLink dHttp url)
    html.AddAttribute("type", CT.Text.JavaScript.Text)
    html.AddAttribute("charset", "UTF-8")
    html.RenderBeginTag "script"
    html.RenderEndTag()

let inlineScript (html: HtmlTextWriter) (text: string) =
    html.AddAttribute("type", CT.Text.JavaScript.Text)
    html.AddAttribute("charset", "UTF-8")
    html.RenderBeginTag "script"
    html.Write(text)
    html.RenderEndTag()

let thisAssemblyToken =
    typeof<Rendering>.Assembly.GetName().GetPublicKeyToken()

let AllReferencedAssemblies = 
    lazy
    try
        System.Web.Compilation.BuildManager.GetReferencedAssemblies()
        |> Seq.cast<System.Reflection.Assembly>
        |> Seq.toList
    with _ ->
    let trace =
        System.Diagnostics.TraceSource("WebSharper",
            System.Diagnostics.SourceLevels.All)

    let d = System.Collections.Generic.Dictionary<string, System.Reflection.Assembly>()
    let rec loop (asm: System.Reflection.Assembly) =
        asm.GetReferencedAssemblies()
        |> Array.iter (fun asmName ->
            if not (d.ContainsKey asmName.Name) then
                try
                    let asm = System.AppDomain.CurrentDomain.Load(asmName)
                    if not asm.IsDynamic then
                        d.Add(asmName.Name, asm)
                        loop asm
                with _ ->
                    trace.TraceEvent(System.Diagnostics.TraceEventType.Warning, 1,
                        "Failed to load referenced assembly for metadata: ", asmName.FullName))
    let asms =
        System.AppDomain.CurrentDomain.GetAssemblies()
        |> Array.filter (fun asm -> not asm.IsDynamic)
    asms |> Array.iter (fun asm -> d.Add(asm.GetName().Name, asm))
    asms |> Array.iter loop
    d
    |> Seq.map (fun (KeyValue(_, v)) -> v)
    |> List.ofSeq

type Rendering with

    member r.Emit(mkHtml: RenderLocation -> HtmlTextWriter, mt, ?defaultToHttp) =
        r.Emit(mkHtml (RenderLocation.ForMediaType mt), mt, ?defaultToHttp = defaultToHttp)

    member r.Emit(html: HtmlTextWriter, mt, ?defaultToHttp) =
        let dHttp = defaultArg defaultToHttp false
        match r with
        | Rendering.RenderInline text ->
            match mt with
            | Css -> inlineStyle html text
            | Js -> inlineScript html text
        | Rendering.RenderLink url ->
            match mt with
            | Css -> link dHttp html url
            | Js -> script dHttp html url
        | Rendering.Skip -> ()

    static member TryGetCdn(ctx: Context, assemblyName: string, filename: string) =
        let fullAsmName, shortName = 
            if assemblyName.Contains "," then
                Some assemblyName, assemblyName.Split(',').[0]
            else
                let fullAsmName =
                    AllReferencedAssemblies.Value
                    |> List.tryPick (fun a -> 
                        if a.FullName.StartsWith (assemblyName + ",") then Some (a.FullName) else None          
                    )
                fullAsmName, assemblyName
        match fullAsmName with
        | None -> None
        | Some fullAsmName ->
        match ctx.GetSetting ("WebSharper.CdnFormat." + shortName) with
        | Some urlFormat -> Some urlFormat
        | None ->
            let isStdlib = AssemblyName(fullAsmName).GetPublicKeyToken() = thisAssemblyToken
            if isStdlib &&
                (defaultArg (ctx.GetSetting "WebSharper.StdlibUseCdn") "false").ToLowerInvariant() = "true"
            then
                let def = "//cdn.websharper.com/{assembly}/{version}/{filename}"
                Some (defaultArg (ctx.GetSetting "WebSharper.StdlibCdnFormat") def)
            else None
        |> Option.map (fun urlFormat ->
            let asm = Assembly.Load(fullAsmName)
            let ver =
                asm.GetCustomAttributes(typeof<AssemblyFileVersionAttribute>, false)
                |> Array.tryPick (fun x ->
                    Some (x :?> AssemblyFileVersionAttribute).Version)
            urlFormat
                .Replace("{assembly}", shortName)
                .Replace("{filename}", filename)
                .Replace("{version}", defaultArg ver "latest")
            |> RenderLink
        )

    static member TryGetCdn(ctx: Context, asm: Assembly, filename: string) =
        Rendering.TryGetCdn(ctx, asm.FullName, filename)

    static member GetWebResourceRendering(ctx: Context, t: Type, filename: string) =
        match Rendering.TryGetCdn(ctx, t.Assembly, filename) with
        | Some r -> r
        | None -> ctx.GetWebResourceRendering t filename

    static member RenderCached(ctx: Context, res: IResource, getWriter) =
        let render = ctx.RenderingCache.GetOrAdd(res, valueFactory = System.Func<_,_>(fun (res: IResource) -> res.Render ctx))
        render getWriter

type Kind =
    | Basic of string
    | Complex of string * list<string>

let tryFindWebResource (t: Type) (spec: string) =
    let ok name = name = spec || (name.StartsWith spec && name.EndsWith spec)
    t.Assembly.GetManifestResourceNames()
    |> Seq.tryFind ok

[<AbstractClass>]
type BaseResource(kind: Kind) =

    new (spec: string) =
        new BaseResource(Basic spec)

    new (b: string, x: string, [<System.ParamArray>] xs: string []) =
        new BaseResource(Complex(b, x :: List.ofArray xs))

    interface IResource with
        member this.Render ctx =
            let dHttp = ctx.DefaultToHttp
            match kind with
            | Basic spec ->
                let self = this.GetType()
                let id = self.FullName
                let mt = if spec.EndsWith ".css" then Css else Js
                let r =
                    match ctx.GetSetting id with
                    | Some url -> RenderLink url
                    | None ->
                        match tryFindWebResource self spec with
                        | Some e -> Rendering.GetWebResourceRendering(ctx, self, e)
                        | None -> RenderLink spec
                fun writer -> r.Emit(writer, mt, dHttp)
            | Complex (b, xs) ->
                let id = this.GetType().FullName
                let b = defaultArg (ctx.GetSetting id) b
                let urls =
                    xs |> List.map (fun x ->
                        let url = b.TrimEnd [| '/' |] + "/" + x.TrimStart [| '/' |]
                        url, url.EndsWith ".css"     
                    )     
                fun writer ->
                    for url, isCss in urls do
                        if isCss then
                            link dHttp (writer Styles) url
                        else script dHttp (writer Scripts) url

[<Sealed>]
type Runtime() =
    interface IResource with
        member this.Render ctx =
            let name = if ctx.DebuggingEnabled then "Runtime.js" else "Runtime.min.js"
            let t = typeof<WebSharper.Core.JavaScript.Syntax.Expression>
            let ren = Rendering.GetWebResourceRendering(ctx, t, name)
            fun writer -> ren.Emit(writer, Js, ctx.DefaultToHttp)

    static member Instance = Runtime() :> IResource
