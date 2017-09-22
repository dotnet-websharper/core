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

#if NET461
type HtmlTextWriter = System.Web.UI.HtmlTextWriter
#else
type HtmlTextWriter(w: TextWriter, indentString: string) =
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
            for (name, value) in currentAttributes do
                this.Write("{0}=\"{1}\"", name, value) // TODO: escape
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
        
    member this.AddAttribute(name: string, value: string) =
        currentAttributes.Add((name, value))
#endif

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
        //GetResourceHash : string * string -> int
        GetAssemblyRendering : string -> Rendering
        GetSetting : string -> option<string>
        GetWebResourceRendering : Type -> string -> Rendering
        RenderingCache : System.Collections.Concurrent.ConcurrentDictionary<IResource, (RenderLocation -> HtmlTextWriter) -> unit>
        ResourceDependencyCache : System.Collections.Concurrent.ConcurrentDictionary<Metadata.Node Set, IResource list>
    }

and IResource =
    abstract member Render : Context -> ((RenderLocation -> HtmlTextWriter) -> unit)

type IDownloadableResource =
    abstract Unpack : string -> unit    

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
#if NET461
    let fromWeb =
        try
            System.Web.Compilation.BuildManager.GetReferencedAssemblies()
            |> Seq.cast<System.Reflection.Assembly>
            |> Seq.toList
        with _ ->
            []
    match fromWeb with
    | _::_ -> fromWeb
    | [] ->
#endif
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
    asms |> Array.iter (fun asm -> 
        let name = asm.GetName().Name
        match d.TryGetValue(name) with
        | true, prevAsm ->
            let n = "WebSharper.meta"
            if Array.exists ((=) n) (asm.GetManifestResourceNames()) && 
                prevAsm.GetName().Version <> asm.GetName().Version
            then
                failwithf "WebSharper assembly referenced with multiple times with different versions: %s" name
        | _ ->
            d.Add(name, asm)
    )
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

let tryGetUriFileName (u: string) =
    try
        let parts = u.Split('/')
        parts.[parts.Length - 1] |> Some
    with _ -> None

let EmptyResource =
   { new IResource with member this.Render _ = ignore }

type BaseResource(kind: Kind) as this =
        
    let self = this.GetType()
    let name = self.FullName

    new (spec: string) =
        new BaseResource(Basic spec)

    new (b: string, x: string, [<System.ParamArray>] xs: string []) =
        new BaseResource(Complex(b, x :: List.ofArray xs))

    member this.GetLocalName() =
        name.Replace('+', '.').Split('`').[0]
    
    interface IResource with
        member this.Render ctx =
            let dHttp = ctx.DefaultToHttp
            let isLocal = ctx.GetSetting "UseDownloadedResources" |> Option.exists (fun s -> s.ToLower() = "true")
            let localFolder isCss f =
                (if isCss then "Content/WebSharper/" else "Scripts/WebSharper/") + this.GetLocalName() + "/" + f
            match kind with
            | Basic spec ->
                let mt = if spec.EndsWith ".css" then Css else Js
                let r =
                    match ctx.GetSetting name with
                    | Some url -> RenderLink url
                    | None ->
                        match tryFindWebResource self spec with
                        | Some e -> Rendering.GetWebResourceRendering(ctx, self, e)
                        | None ->
                            if isLocal then
                                match tryGetUriFileName spec with
                                | Some f ->
                                    RenderLink (localFolder (mt = Css)  f)
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
                    if isLocal then 
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
                        else script dHttp (writer Scripts) url

    interface IDownloadableResource with
        member this.Unpack path =
            use wc = new System.Net.WebClient()    
            let localName = this.GetLocalName()
            let cssDir = Path.Combine (path, "Content", "WebSharper", localName)
            let jsDir = Path.Combine (path, "Scripts", "WebSharper", localName)
            let download (url: string) =
                match tryGetUriFileName url with
                | Some f ->
                    let localDir = if url.EndsWith ".css" then cssDir else jsDir
                    let localPath = Path.Combine(localDir, f)
                    if not (Directory.Exists localDir) then
                        Directory.CreateDirectory localDir |> ignore
                    let url = if url.StartsWith("//") then "http:" + url else url
                    printfn "Downloading %A to %s" url localPath
                    let tempLocalPath = localPath + ".download"
                    wc.DownloadFile(url, tempLocalPath)
                    if File.Exists tempLocalPath then
                        if File.Exists localPath then
                            File.Delete localPath
                        File.Move(tempLocalPath, localPath)
                | _ ->
                    ()
            match kind with
            | Basic spec ->
                download spec
            | Complex (b, xs) ->
                for x in xs do
                    download (b.TrimEnd('/') + "/" + x.TrimStart('/'))

[<Sealed>]
type Runtime() =
    interface IResource with
        member this.Render ctx =
            let name = if ctx.DebuggingEnabled then "Runtime.js" else "Runtime.min.js"
            let t = typeof<WebSharper.Core.JavaScript.Syntax.Expression>
            let ren = Rendering.GetWebResourceRendering(ctx, t, name)
            fun writer -> ren.Emit(writer, Js, ctx.DefaultToHttp)

    static member Instance = Runtime() :> IResource
