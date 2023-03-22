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

module WebSharper.Core.Resources

open System
open System.IO
open System.Net
open System.Reflection

open WebSharper.Constants

module CT = ContentTypes
    
type HtmlTextWriter(w: TextWriter, indent: string) =
    inherit System.IO.TextWriter(w.FormatProvider)

    let mutable tagStack = System.Collections.Generic.Stack()
    let currentAttributes = ResizeArray()

    new (w) = new HtmlTextWriter(w, "\t")

    override this.Write(c: char) = w.Write(c)
    override this.Write(s: string) = w.Write(s)
    override this.Encoding = w.Encoding

    member this.PushTag(name: string) =
        tagStack.Push(name)

    member this.PopTag() =
        if tagStack.Count = 0 then
            raise (System.InvalidOperationException("A PopEndTag was called without a corresponding PushEndTag."))
        tagStack.Pop()

    // TODO dotnet: newlines and indentation
    member this.RenderBeginTag(name: string) =
        this.PushTag(name)
        this.Write('<')
        this.Write(name)
        if currentAttributes.Count > 0 then
            for struct (name, value) in currentAttributes do
                this.WriteAttribute(name, value)
            currentAttributes.Clear()
        this.Write('>')

    member this.RenderEndTag() =
        this.WriteEndTag(this.PopTag())

    member this.WriteBeginTag(name: string) =
        this.Write("<")
        this.Write(name)

    member this.WriteFullBeginTag(name: string) =
        this.WriteBeginTag(name)
        this.Write(">")

    member this.WriteEndTag(name) =
        this.Write("</")
        this.Write(name)
        this.Write(">")

    member this.WriteEncodedText(text: string) =
        WebUtility.HtmlEncode(text, w)

    member this.AddAttribute(name: string, value: string) =
        currentAttributes.Add(struct (name, value))

    member this.WriteAttribute(name: string, value: string) =
        this.WriteAttribute(name, value, true)

    member this.WriteAttribute(name: string, value: string, encode: bool) =
        this.Write(" {0}=\"", name)
        if encode then
            WebUtility.HtmlEncode(value, w)
        else
            w.Write(value)
        this.Write("\"")

    static member SelfClosingTagEnd = " />"

    static member TagLeftChar = '>'

    static member TagRightChar = '>'

    /// Checks whether an element should be rendered as self-closing,
    /// ie. <x /> instead of <x></x>
    static member IsSelfClosingTag (name: string) =
        List.exists ((=) (name.ToLowerInvariant())) [
            "area"
            "base"
            "basefont"
            "br"
            "col"
            "embed"
            "frame"
            "hr"
            "img"
            "input"
            "isindex"
            "keygen"
            "link"
            "meta"
            "param"
            "source"
            "track"
            "wbr"
        ]

    member this.WriteStartCode(scriptBaseUrl: option<string>, ?includeScriptTag: bool, ?skipAssemblyDir: bool, ?types: list<AST.Address>) =
        let includeScriptTag = defaultArg includeScriptTag true
        let skipAssemblyDir = defaultArg skipAssemblyDir false
        if includeScriptTag then
            this.WriteLine("""<script type="{0}">""", CT.Text.Module.Text)
        match scriptBaseUrl with
        | Some url -> 
            this.WriteLine("""import Runtime, {{ Start as StartWS }} from "{0}WebSharper.Core.JavaScript/Runtime.js";""", url)
            this.WriteLine("""Runtime.ScriptBasePath = '{0}';""", url)
            if skipAssemblyDir then
                this.WriteLine("""Runtime.ScriptSkipAssemblyDir = true;""")
            match types with
            | None | Some [] -> ()
            | Some types ->
                this.WriteLine("""import {{ Activate }} from "{0}WebSharper.Main/WebSharper.Activator.js";""", url)
                let imported = System.Collections.Generic.Dictionary<AST.CodeResource, string>()
                let activate = System.Text.StringBuilder("Activate([")
                let mutable first = true
                for t in types do
                    if first then
                        first <- false
                    else
                        activate.Append(",") |> ignore
                    match t.Module with  
                    | AST.JavaScriptFile f ->
                        this.WriteLine("""import "{1}{2}/{3}.js";""", url, f.Assembly, f.Name)
                    | AST.JavaScriptModule m
                    | AST.DotNetType m ->
                        match imported.TryGetValue(m) with
                        | true, i ->
                            activate.Append(i) |> ignore
                        | _ ->
                            let i = "i" + string (imported.Count + 1)
                            imported.Add(m, i)
                            this.WriteLine("""import * as {0} from "{1}{2}/{3}.js";""", i, url, m.Assembly, m.Name)
                            activate.Append(i) |> ignore
                    | _ ->
                        ()
                activate.Append("]);") |> ignore
                this.WriteLine(string activate)
            this.WriteLine """StartWS();"""
        | None -> ()
        if includeScriptTag then
            this.WriteLine("""</script>""")

    static member WriteStartCode(writer: TextWriter, scriptBaseUrl: option<string>, ?includeScriptTag: bool, ?skipAssemblyDir: bool, ?types: list<AST.Address>) =
        writer.WriteLine()
        use w = new HtmlTextWriter(writer)
        w.WriteStartCode(scriptBaseUrl, ?includeScriptTag = includeScriptTag, ?skipAssemblyDir = skipAssemblyDir, ?types = types)

type Rendering =
    | RenderInline of string
    | RenderLink of string
    | Skip

type MediaType =
    | Css
    | Js
    | JsModule

type RenderLocation =
    | Scripts
    | Styles
    | Meta

    static member ForMediaType t =
        match t with
        | Js | JsModule -> Scripts
        | Css -> Styles

type Context =
    {
        DebuggingEnabled : bool
        DefaultToHttp : bool
        ScriptBaseUrl : option<string>
        //GetResourceHash : string * string -> int
        GetAssemblyRendering : string -> Rendering
        GetSetting : string -> option<string>
        GetWebResourceRendering : Type -> string -> Rendering
        WebRoot : string  
        RenderingCache : System.Collections.Concurrent.ConcurrentDictionary<IResource, (RenderLocation -> HtmlTextWriter) -> unit>
        ResourceDependencyCache : System.Collections.Concurrent.ConcurrentDictionary<Metadata.Node Set, IResource list>
    }

and IResource =
    abstract member Render : Context -> ((RenderLocation -> HtmlTextWriter) -> unit)

type IDownloadableResource =
    abstract Unpack : string -> unit    
    abstract member GetImports : unit -> string[]

type IExternalScriptResource =
    inherit IResource
    abstract member Urls : Context -> string[]

let cleanLink dHttp (url: string) =
    if dHttp && url.StartsWith("//")
        then "http:" + url
        else url

let link dHttp (html: HtmlTextWriter) (url: string) =
    if not (String.IsNullOrWhiteSpace(url)) then
        html.AddAttribute("type", CT.Text.Css.Text)
        html.AddAttribute("rel", "stylesheet")
        html.AddAttribute("href", cleanLink dHttp url)
        html.RenderBeginTag "link"
        html.RenderEndTag()
        html.WriteLine()

let inlineStyle (html: HtmlTextWriter) (text: string) =
    if not (String.IsNullOrWhiteSpace(text)) then
        html.AddAttribute("type", CT.Text.Css.Text)
        html.RenderBeginTag "style"
        html.Write(text)
        html.RenderEndTag()
        html.WriteLine()

let script dHttp (html: HtmlTextWriter) isModule (url: string) =
    if not (String.IsNullOrWhiteSpace(url)) then
        html.AddAttribute("src", cleanLink dHttp url)
        html.AddAttribute("type", if isModule then CT.Text.Module.Text else CT.Text.JavaScript.Text)
        html.AddAttribute("charset", "UTF-8")
        html.RenderBeginTag "script"
        html.RenderEndTag()

let inlineScript (html: HtmlTextWriter) isModule (text: string) =
    if not (String.IsNullOrWhiteSpace(text)) then
        html.AddAttribute("type", if isModule then CT.Text.Module.Text else CT.Text.JavaScript.Text)
        html.AddAttribute("charset", "UTF-8")
        html.RenderBeginTag "script"
        html.Write(text)
        html.RenderEndTag()

let thisAssemblyToken =
    typeof<Rendering>.Assembly.GetName().GetPublicKeyToken()

type Rendering with

    member r.Emit(mkHtml: RenderLocation -> HtmlTextWriter, mt, ?defaultToHttp) =
        r.Emit(mkHtml (RenderLocation.ForMediaType mt), mt, ?defaultToHttp = defaultToHttp)

    member r.Emit(html: HtmlTextWriter, mt, ?defaultToHttp) =
        let dHttp = defaultArg defaultToHttp false
        match r with
        | Rendering.RenderInline text ->
            match mt with
            | Css -> inlineStyle html text
            | Js -> inlineScript html false text
            | JsModule -> inlineScript html true text
        | Rendering.RenderLink url ->
            match mt with
            | Css -> link dHttp html url
            | Js -> script dHttp html false url
            | JsModule -> script dHttp html true url
        | Rendering.Skip -> ()

    static member TryGetCdn(ctx: Context, assemblyName: string, filename: string) =
        let shortName = 
            if assemblyName.Contains "," then
                assemblyName.Split(',').[0]
            else
                assemblyName
        let cdnFormatOpt =
            ctx.GetSetting (RUNTIMESETTING_CDNFORMAT_PREFIX + shortName) 
            |> Option.orElseWith (fun () -> ctx.GetSetting (RUNTIMESETTING_OLD_CDNFORMAT_PREFIX + shortName))
        match cdnFormatOpt with
        | Some urlFormat -> Some urlFormat
        | None ->
            let isStdlib = AssemblyName(assemblyName).GetPublicKeyToken() = thisAssemblyToken
            if isStdlib &&
                (
                    ctx.GetSetting RUNTIMESETTING_STDLIB_USECDN 
                    |> Option.orElseWith (fun () -> ctx.GetSetting RUNTIMESETTING_OLD_STDLIB_USECDN)
                    |> Option.exists (fun s -> s.ToLowerInvariant() = "true")
                )
            then
                ctx.GetSetting RUNTIMESETTING_STDLIB_CDNFORMAT
                |> Option.orElseWith (fun () -> ctx.GetSetting RUNTIMESETTING_OLD_STDLIB_CDNFORMAT)
                |> Option.defaultValue "//cdn.websharper.com/{assembly}/{version}/{filename}"
                |> Some
            else None
        |> Option.map (fun urlFormat ->
            let asmName = AssemblyName(assemblyName)
            let version = asmName.Version.ToString()
            urlFormat
                .Replace("{assembly}", shortName)
                .Replace("{filename}", filename)
                .Replace("{version}", version)
            |> RenderLink
        )

    static member TryGetCdn(ctx: Context, assembly: Assembly, filename: string) =
        Rendering.TryGetCdn(ctx, assembly.FullName, filename)

    static member GetWebResourceRendering(ctx: Context, resource: Type, filename: string) =
        match Rendering.TryGetCdn(ctx, resource.Assembly, filename) with
        | Some r -> r
        | None -> ctx.GetWebResourceRendering resource filename

    static member RenderCached(ctx: Context, resource: IResource, getWriter) =
        let render = ctx.RenderingCache.GetOrAdd(resource, valueFactory = System.Func<_,_>(fun (res: IResource) -> res.Render ctx))
        render getWriter

type Kind =
    | Basic of string
    | Complex of string * list<string>

let tryFindWebResource (t: Type) (spec: string) =
    let ok name = name = spec || (name.StartsWith spec && name.EndsWith spec)
    t.Assembly.GetManifestResourceNames()
    |> Seq.tryFind ok

let tryGetUriFileName (u: string) =
    if u.StartsWith "http:" || u.StartsWith "https:" || u.StartsWith "//" then
        let parts = u.Split([| '/' |], StringSplitOptions.RemoveEmptyEntries)
        Array.tryLast parts
    else
        None

let EmptyResource =
   { new IResource with member this.Render _ = ignore }

type BaseResource(kind: Kind) as this =
        
    let self = this.GetType()
    let name = self.FullName

    let isLocal (ctx: Context) =
        ctx.GetSetting RUNTIMESETTING_USEDOWNLOADEDRESOURCES 
        |> Option.exists (fun s -> s.ToLowerInvariant() = "true")

    new (spec: string) =
        new BaseResource(Basic spec)

    new (b: string, x: string, [<System.ParamArray>] xs: string []) =
        new BaseResource(Complex(b, x :: List.ofArray xs))

    member this.GetLocalName() =
        name.Replace('+', '.').Split('`').[0]

    interface IExternalScriptResource with
        member this.Urls ctx =
            let dHttp = ctx.DefaultToHttp
            let localFolder f =
                ctx.WebRoot +  "Scripts/WebSharper/" + this.GetLocalName() + "/" + f
            match kind with
            | Basic spec ->
                if spec.EndsWith ".css" then [||] else
                match ctx.GetSetting name with
                | Some url -> [|url|]
                | None ->
                    match tryFindWebResource self spec with
                    | Some _ -> [||]
                    | None ->
                        if isLocal ctx then
                            match tryGetUriFileName spec with
                            | Some f -> [|localFolder f|]
                            | _ -> [|spec|]
                        else
                            [|spec|]
            | Complex (b, xs) ->
                let b = defaultArg (ctx.GetSetting name) b
                let urls =
                    xs |> List.choose (fun x ->
                        let url = b.TrimEnd('/') + "/" + x.TrimStart('/')
                        if url.EndsWith ".css" then None else Some url
                    )
                let urls = 
                    if isLocal ctx then 
                        urls |> List.map (fun u ->
                            match tryGetUriFileName u with
                            | Some f -> localFolder f
                            | None -> u
                        )
                    else urls
                urls |> Array.ofList
    
    interface IResource with
        member this.Render ctx =
            let dHttp = ctx.DefaultToHttp
            let localFolder isCss f =
                ctx.WebRoot + 
                (if isCss then "Content/WebSharper/" else "Scripts/WebSharper/") + this.GetLocalName() + "/" + f
            match kind with
            | Basic spec ->
                let mt = 
                    if spec.EndsWith ".css" then Css 
                    elif spec.EndsWith ".mjs" then JsModule 
                    else Js
                let r =
                    match ctx.GetSetting name with
                    | Some url -> RenderLink url
                    | None ->
                        match tryFindWebResource self spec with
                        | Some e -> Rendering.GetWebResourceRendering(ctx, self, e)
                        | None ->
                            if isLocal ctx then
                                match tryGetUriFileName spec with
                                | Some f ->
                                    RenderLink (localFolder (mt = Css) f)
                                | _ ->
                                    RenderLink spec
                            else
                                RenderLink spec
                fun writer -> r.Emit(writer, mt, dHttp)
            | Complex (b, xs) ->
                let b = defaultArg (ctx.GetSetting name) b
                let urls =
                    xs |> List.map (fun x ->
                        let url = b.TrimEnd('/') + "/" + x.TrimStart('/')
                        url, url.EndsWith ".css"     
                    )  
                let urls = 
                    if isLocal ctx then 
                        urls |> List.map (fun (u, isCss) ->
                            match tryGetUriFileName u with
                            | Some f ->
                                localFolder isCss f, isCss
                            | _ ->
                                u, isCss
                        )
                    else urls
                fun writer ->
                    for url, isCss in urls do
                        if isCss then
                            link dHttp (writer Styles) url
                        else script dHttp (writer Scripts) false url

    interface IDownloadableResource with
        member this.Unpack path =
            let download (paths: string list) =
                let urls =
                    paths |> List.choose (fun p ->
                        let p = if p.StartsWith "//" then "http:" + p else p
                        match Uri.TryCreate(p, UriKind.Absolute) with
                        | true, uri when not uri.IsFile -> 
                            tryGetUriFileName p |> Option.map (fun f -> uri, f)
                        | _ -> None
                    )
                if List.isEmpty urls |> not then
                    use wc = new System.Net.WebClient()    
                    let localName = this.GetLocalName()
                    let cssDir = Path.Combine (path, "Content", "WebSharper", localName)
                    let jsDir = Path.Combine (path, "Scripts", "WebSharper", localName)
                    for url, f in urls do
                        let localDir = if f.EndsWith ".css" then cssDir else jsDir
                        let localPath = Path.Combine(localDir, f)
                        if not (Directory.Exists localDir) then
                            Directory.CreateDirectory localDir |> ignore
                        printfn "Downloading %A to %s" url localPath
                        let tempLocalPath = localPath + ".download"
                        wc.DownloadFile(url, tempLocalPath)
                        if File.Exists tempLocalPath then
                            if File.Exists localPath then
                                File.Delete localPath
                            File.Move(tempLocalPath, localPath)
            match kind with
            | Basic spec ->
                download [ spec ]
            | Complex (b, xs) ->
                download (xs |> List.map (fun x -> b.TrimEnd('/') + "/" + x.TrimStart('/')))

        member this.GetImports() = 
            let getPath spec =
                match tryFindWebResource self spec with 
                | Some f -> "./" + this.GetLocalName() + "/" + f
                | _ -> spec
            
            match kind with
            | Basic spec ->
                if spec.EndsWith ".js" then
                    [| getPath spec |]
                else [||]
            | Complex (b, xs) ->
                xs |> Seq.choose (fun x ->
                    if x.EndsWith ".js" then 
                        let spec = b.TrimEnd('/') + "/" + x.TrimStart('/')
                        Some (getPath spec) 
                    else None    
                ) |> Array.ofSeq 

[<Sealed>]
type Runtime() =
    interface IResource with
        member this.Render ctx =
            //let name = if ctx.DebuggingEnabled then "Runtime.js" else "Runtime.min.js"
            let name = "Runtime.js"
            let t = typeof<WebSharper.Core.JavaScript.Syntax.Expression>
            let ren = Rendering.GetWebResourceRendering(ctx, t, name)
            //fun writer -> ren.Emit(writer, JsModule, ctx.DefaultToHttp)
            ignore

    static member Instance = Runtime() :> IResource
