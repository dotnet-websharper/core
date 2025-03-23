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

namespace WebSharper.Sitelets

open System.IO
open System.Threading
open System.Threading.Tasks
open WebSharper
open WebSharper.Core

module CT = WebSharper.Core.ContentTypes

[<CompiledName "FSharpContent">]
type Content<'Endpoint> =
    | CustomContent of (Context<'Endpoint> -> Http.Response)
    | CustomContentAsync of (Context<'Endpoint> -> Async<Http.Response>)

    static member ToResponse<'T> (c: Content<'T>) (ctx: Context<'T>) : Async<Http.Response> =
        match c with
        | CustomContent x -> async.Return (x ctx)
        | CustomContentAsync x -> x ctx

    member c.Box() : Content<obj> =
        match c with
        | CustomContent x -> CustomContent (fun ctx -> x (Context.Map box ctx))
        | CustomContentAsync x -> CustomContentAsync (fun ctx -> x (Context.Map box ctx))

    static member Unbox (c: Content<obj>) : Content<'T> =
        match c with
        | CustomContent x -> CustomContent (fun ctx -> x (Context.Map unbox ctx))
        | CustomContentAsync x -> CustomContentAsync (fun ctx -> x (Context.Map unbox ctx))

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Content =
    open System
    open System.Collections.Generic
    open System.IO
    open System.Text.RegularExpressions
    type private HtmlTextWriter = WebSharper.Core.Resources.HtmlTextWriter

    type private Func<'A,'B> = System.Func<'A,'B>

    module M = WebSharper.Core.Metadata
//    module R = WebSharper.Core.Reflection
    module J = WebSharper.Core.Json    

    let defaultEncoding = new System.Text.UTF8Encoding(false) :> System.Text.Encoding

    type ActivateControl =
        | DecodeFromJson of id: string * data: string * deserializer: AST.Address

    let escape (s: string) =
        Regex.Replace(s, @"[&<>']",
            new MatchEvaluator(fun m ->
                match m.Groups.[0].Value.[0] with
                | '&'-> "&amp;"
                | '<'-> "&lt;"
                | '>' -> "&gt;"
                | '\'' -> "&#39;"
                | _ -> failwith "unreachable"))

    let writeResources (ctx: Web.Context) (controls: seq<#IRequiresResources>) (tw: Core.Resources.RenderLocation -> HtmlTextWriter) =
        let uniqueIdSource =
            let mutable i = 0
            { new IUniqueIdSource with
                override this.NewId() =
                    i <- i + 1
                    string i
            }
        
        // Resolve resources for the set of types and this assembly
        let requiresAndCode =
            controls
            |> Seq.collect (fun c -> c.Requires (ctx.Metadata, ctx.Json, uniqueIdSource))
            |> Array.ofSeq
        
        let requires =  
            requiresAndCode
            |> Array.choose (function ClientRequire n -> Some n | _ -> None) 

        if requires.Length > 0 then

            let resources =
                ctx.ResourceContext.ResourceDependencyCache.GetOrAdd(Set.ofArray requires, fun nodes ->
                    ctx.Dependencies.GetResources ctx.Metadata nodes
                )

            // Render resources
            for r in resources do
                Core.Resources.Rendering.RenderCached(ctx.ResourceContext, r, tw)

        let toActivate = 
            requiresAndCode
            |> Seq.choose (function ClientBundle _ | ClientRequire _ -> None | n -> Some n)
            |> Seq.indexed
            |> Array.ofSeq

        if toActivate.Length > 0 then
            
            let scriptsTw = tw Core.Resources.Scripts
            
            let requiredBundles =
                requiresAndCode
                |> Seq.choose (function ClientBundle n -> Some n | _ -> None)
                |> Seq.distinct
                |> Array.ofSeq

            let allImports =
                let rec importsOf a =
                    match a with
                    | ClientImport f ->
                        Seq.singleton f
                    | ClientArrayData a ->
                        a |> Seq.collect importsOf
                    | ClientObjectData a ->
                        a |> Seq.collect (snd >> importsOf)
                    | ClientApply (c, args) ->
                        Seq.append (importsOf c) (args |> Seq.collect importsOf)
                    | ClientReplaceInDom (_, c) 
                    | ClientReplaceInDomWithBody (_, c)
                    | ClientAddEventListener (_, _, c)
                    | ClientInitialize (_, c) ->
                        importsOf c   
                    | _ ->
                        Seq.empty
                
                requiresAndCode
                |> Seq.collect importsOf
                |> Array.ofSeq

#if DEBUG
            scriptsTw.WriteLine("<!--")
            scriptsTw.WriteLine("imports needed:")
            for i in allImports do
                scriptsTw.WriteLine(i.ToString())
#endif

            let hasRoot =
                ctx.Metadata.PreBundle.Count = 1 && ctx.Metadata.PreBundle.ContainsKey("root")
            
            let bundleName = 
                if hasRoot then
                    Some "root"
                elif ctx.Metadata.PreBundle.Count > 0 && allImports.Length > 0 then
#if DEBUG
                    scriptsTw.WriteLine($"bundles: %A{requiredBundles}")
#endif
                    match requiredBundles with
                    | [||] ->
                        if ctx.Metadata.PreBundle.ContainsKey("all") then Some "all" else None
                    | _ ->
                        requiredBundles
                        |> Seq.tryFind (fun b ->
                            match ctx.Metadata.PreBundle.TryFind b with
                            | Some bundle ->
                                if allImports |> Seq.forall (fun a -> bundle.ContainsKey a.Root) then
                                    true
                                else
#if DEBUG
                                    scriptsTw.WriteLine($"failed to use bundle {b}:")
                                    for i in bundle.Keys do
                                        scriptsTw.WriteLine(i.ToString())
#endif
                                    false
                            | _ ->
                                false
                        )
                        |> Option.orElse (
                            if ctx.Metadata.PreBundle.ContainsKey("all") then Some "all" else None
                        )
                else
                    None
#if DEBUG
            scriptsTw.WriteLine("-->")
#endif

            let bundle =
                bundleName
                |> Option.map (fun b ->
                    ctx.Metadata.PreBundle[b]
                )
            
            let activate (url: string) =
                let imported = Dictionary<AST.Address, string>()
                let elems = HashSet<string>()

                let lookupElement i =
                    let v = "e" + i
                    if not (elems.Contains(i)) then
                        scriptsTw.WriteLine($"""let {v} = document.querySelector("[ws-{i}]");""")
                        elems.Add(i) |> ignore
                    v

                let rec getCode a =
                    match a with
                    | ClientBundle _ 
                    | ClientRequire _ ->
                        ""
                    | ClientJsonData d ->
                        J.Stringify d
                    | ClientArrayData a ->
                        $"""[{ a |> Seq.map getCode |> String.concat "," }]"""    
                    | ClientObjectData a ->
                        $"""{{{ a |> Seq.map (fun (n, v) -> $"\"{n}\":{getCode v}" ) |> String.concat "," }}}"""    
                    | ClientImport f ->
                        let rec findInPreBundle a =
                            match bundle with
                            | None -> None
                            | Some bundle ->
                                match bundle.TryFind a with
                                | Some b -> 
                                    if hasRoot then
                                        match imported.TryGetValue(f) with   
                                        | true, i ->
                                            Some i
                                        | _ ->
                                            let i = "i" + string (imported.Count + 1)
                                            match f.Address |> List.rev with
                                            | [] -> failwith "empty address"
                                            | a :: r ->
                                                let j = 
                                                    match a with
                                                    | "default" -> 
                                                        match r with    
                                                        | [] ->     
                                                            i :: r |> String.concat "."
                                                        | _ :: rr ->
                                                            i :: rr |> String.concat "."
                                                    | _ ->
                                                        i :: r |> String.concat "."
                                                imported.Add(f, j)
                                                let asmName = bundle[AST.Address.Global()]
                                                scriptsTw.WriteLine($"""import {{ {b} as {i} }} from "{url}{asmName}/root.js";""")
                                                Some j
                                    else
                                        Some ("wsbundle." + b)
                                | None ->
                                    match a.Address with 
                                    | h :: t ->
                                        match findInPreBundle { a with Address = t } with
                                        | Some f -> Some (f + "." + h)
                                        | _ -> None
                                    | [] -> None
                        match findInPreBundle f with
                        | Some b -> b
                        | None ->
                            match f.Module with
                            | AST.DotNetType m ->
                                match imported.TryGetValue(f) with
                                | true, i ->
                                    i
                                | _ ->
                                    let i = "i" + string (imported.Count + 1)
                                    match f.Address |> List.rev with
                                    | [] -> failwith "empty address"
                                    | a :: r ->
                                        let j = i :: r |> String.concat "."
                                        imported.Add(f, j)
                                        match a with
                                        | "default" ->
                                            scriptsTw.WriteLine($"""import {i} from "{url}{m.Assembly}/{m.Name}.js";""")
                                        | _ ->
                                            scriptsTw.WriteLine($"""import {{ {a} as {i} }} from "{url}{m.Assembly}/{m.Name}.js";""")
                                        j
                            | _ ->
                                f.Address |> List.rev |> String.concat "."
                    | ClientApply (c, args) ->
                        $"""{getCode c}({ args |> Seq.map getCode |> String.concat "," })"""
                    | ClientReplaceInDom (i, c) -> 
                        $"""{getCode c}.ReplaceInDom({lookupElement i})"""
                    | ClientReplaceInDomWithBody (i, c) -> 
                        $"""{getCode c}.Body.ReplaceInDom({lookupElement i})"""
                    | ClientAddEventListener (i, ev, c) ->
                        let el = lookupElement i
                        if String.IsNullOrEmpty(ev) then
                            $"""{el}.addEventListener({el}.getAttribute('ws-{i}'),{getCode c})"""
                        else
                            $"""{el}.addEventListener("{ev}",{getCode c})"""
                    | ClientDOMElement i ->
                        lookupElement i
                    | ClientInitialize (_, c) ->
                        getCode c

                for ii, a in toActivate do
                    match a with
                    | ClientInitialize (i, c) ->
                        let v = "o" + string ii
                        scriptsTw.WriteLine($"""let {v} = {getCode c};""")
                        scriptsTw.WriteLine($"""{v}.$preinit("{i}");""")
                    | _ -> ()

                for ii, a in toActivate do
                    match a with
                    | ClientInitialize (i, _) ->
                        let v = "o" + string ii
                        scriptsTw.WriteLine($"""{v}.$init("{i}");""")
                    | ClientBundle _
                    | ClientRequire _ ->
                        ()
                    | _ ->
                        scriptsTw.Write(getCode a)
                        scriptsTw.WriteLine(";")

                for ii, a in toActivate do
                    match a with
                    | ClientInitialize (i, _) ->
                        let v = "o" + string ii
                        scriptsTw.WriteLine($"""{v}.$postinit("{i}");""")
                    | _ -> ()

            Some activate, if hasRoot then None else bundleName |> Option.map Array.singleton 
        else    
            None, None

    open Microsoft.FSharp.Quotations
    open WebSharper.Web.ClientSideInternals

    let compile (meta: M.Info) (json: J.Provider) (q: Expr) applyCode =
        let reqs = ResizeArray<M.Node>()
        let rec compile' (q: Expr) : option<ClientCode> =
            match getLocation q with
            | Some p ->
                match meta.Quotations.TryGetValue(p) with
                | false, _ ->
                    None
                | true, quot ->
                    let fail() =
                        failwithf "Error in Handler: Couldn't find JavaScript address for method %s.%s" quot.TypeDefinition.Value.FullName quot.Method.Value.MethodName
                    match meta.Classes.TryGetValue quot.TypeDefinition with
                    | true, (clAddr, _, Some c) ->
                        let argIndices = Map (quot.Arguments |> List.mapi (fun i x -> x, i))
                        let args = Array.zeroCreate<ClientCode> quot.Arguments.Length
                        reqs.Add(M.MethodNode (quot.TypeDefinition, quot.Method))
                        reqs.Add(M.TypeNode quot.TypeDefinition)
                        let setArg (name: string) (value: obj) =
                            let i = argIndices[name]
                            if obj.ReferenceEquals(args[i], null) then
                                args[i] <-
                                    match value with
                                    | :? Expr as q ->
                                        compile' q |> Option.get
                                    | value ->
                                        let typ = value.GetType()
                                        reqs.Add(M.TypeNode (WebSharper.Core.AST.Reflection.ReadTypeDefinition typ))
                                        Web.Control.EncodeClientObject(meta, json, value)
                        findArgs Set.empty setArg q
                        let addr =
                            match c.Methods.TryGetValue quot.Method with
                            | true, m ->
                                match m.CompiledForm with
                                | M.CompiledMember.Static (name, false, AST.MemberKind.Simple) -> 
                                    clAddr.Static(name)
                                | M.CompiledMember.GlobalFunc (addr, false) -> 
                                    addr
                                | M.CompiledMember.Func (name, false) -> 
                                    clAddr.Func(name)
                                | _ -> fail()
                            | _ -> fail()
                        //let funcall = String.concat "." (List.rev addr)
                        let code = ClientApply(ClientImport addr, args)
                        Some code
                    | _ -> fail()
            | None -> None
        compile' q
        |> Option.map (fun s ->
            applyCode s :: (reqs |> Seq.map ClientRequire |> List.ofSeq) :> seq<_>
        )

    type RenderedResources =
        {
            Scripts : string
            Styles : string
            Meta : string
        }

        member this.Item
            with get (x: string) =
                match x.ToLowerInvariant() with
                | "scripts" -> this.Scripts
                | "styles" -> this.Styles
                | "meta" -> this.Meta
                | _ -> failwith "Invalid rendered resource identifier"

    let getSeparateResourcesAndScripts ctx controls : RenderedResources =
        use scriptsW = new StringWriter()
        let scriptsTw = new HtmlTextWriter(scriptsW, " ")
        use stylesW = new StringWriter()
        let stylesTw = new HtmlTextWriter(stylesW, " ")
        use metaW = new StringWriter()
        let metaTw = new HtmlTextWriter(metaW, " ")
        let activation, bundleNames =
            writeResources ctx controls (function
                | Core.Resources.Scripts -> scriptsTw
                | Core.Resources.Styles -> stylesTw
                | Core.Resources.Meta -> metaTw)
        if Option.isSome activation then
            scriptsTw.WriteStartCode(ctx.ResourceContext.ScriptBaseUrl, ?activation = activation, ?bundleNames = bundleNames)
        {
            Scripts = scriptsW.ToString()
            Styles = stylesW.ToString()
            Meta = metaW.ToString()
        }

    let getResourcesAndScripts ctx controls =
        use w = new StringWriter()
        use tw = new HtmlTextWriter(w, " ")
        let activation, bundleNames = writeResources ctx controls (fun _ -> tw)
        if Option.isSome activation then
            tw.WriteStartCode(ctx.ResourceContext.ScriptBaseUrl, ?activation = activation, ?bundleNames = bundleNames)
        w.ToString()
    
    let toCustomContentAsync (genPage: Context<'T> -> Async<Page>) context : Async<Http.Response> =
        async {
            let! htmlPage = genPage context
            let writeBody (stream: Stream) =
                let body = Seq.cache htmlPage.Body
                let renderHead (tw: HtmlTextWriter) =
                    let activation, bundleNames = writeResources context body (fun _ -> tw)
                    for elem in htmlPage.Head do
                        elem.Write(context, tw)
                    if Option.isSome activation then
                        tw.WriteStartCode(context.ResourceContext.ScriptBaseUrl, ?activation = activation, ?bundleNames = bundleNames)
                let renderBody (tw: HtmlTextWriter) =
                    for elem in body do
                        elem.Write(context, tw)
                // Create html writer from stream
                use textWriter = new StreamWriter(stream, System.Text.Encoding.UTF8, 1024, leaveOpen = true)
                use htmlWriter = new HtmlTextWriter(textWriter, " ")
                htmlPage.Renderer htmlPage.Doctype htmlPage.Title
                    renderHead renderBody htmlWriter
                textWriter.Flush()
            return {
                Status = Http.Status.Ok
                Headers = [Http.Header.Custom "Content-Type" "text/html; charset=utf-8"]
                WriteBody = Http.WriteBody writeBody
            }
        }

    let JsonContent<'T, 'U> (f: Context<'T> -> 'U) =
        let encoder = Json.ServerSideProvider.GetEncoder<'U>()
        Content.CustomContent <| fun ctx ->
            let x = f ctx
            {
                Status = Http.Status.Ok
                Headers = [Http.Header.Custom "Content-Type" "application/json"]
                WriteBody = Http.WriteBody (fun s ->
                    use tw = new StreamWriter(s, System.Text.Encoding.UTF8, 1024, leaveOpen = true)
                    x
                    |> encoder.Encode
                    |> WebSharper.Core.Json.Write tw
                )
            }

    let JsonContentAsync<'T, 'U> (f: Context<'T> -> Async<'U>) =
        let encoder = Json.ServerSideProvider.GetEncoder<'U>()
        Content.CustomContentAsync <| fun ctx ->
            async {
                let! x = f ctx
                return {
                    Status = Http.Status.Ok
                    Headers = [Http.Header.Custom "Content-Type" "application/json"]
                    WriteBody = Http.WriteBody (fun s ->
                        use tw = new StreamWriter(s, System.Text.Encoding.UTF8, 1024, leaveOpen = true)
                        x
                        |> encoder.Encode
                        |> WebSharper.Core.Json.Write tw
                    )
                }
            }

    let ToResponse<'T> (c: Content<'T>) (ctx: Context<'T>) : Async<Http.Response> =
        Content<_>.ToResponse c ctx

    let FromContext f =
        Content.CustomContentAsync (fun ctx -> async {
            let! content = f ctx
            return! ToResponse content ctx
        })
        |> async.Return

    let ToResponseAsync c ctx = ToResponse c ctx

    let FromAsync ac =
        CustomContentAsync <| fun ctx -> async {
            let! c = ac
            return! ToResponse c ctx
        }

    let delay1 f =
        fun arg -> async { return f arg }

    let MapResponseAsync<'T> (f: Http.Response -> Async<Http.Response>) (content: Async<Content<'T>>) =
        let genResp content =
            match content with
            | CustomContent gen -> delay1 gen
            | CustomContentAsync x -> x
        CustomContentAsync <| fun context ->
            async {
                let! content = content
                let! result = genResp content context
                return! f result
            }
        |> async.Return

    let MapResponse<'T> (f: Http.Response -> Http.Response) (content: Async<Content<'T>>) =
        let genResp content =
            match content with
            | CustomContent gen -> delay1 gen
            | CustomContentAsync x -> x
        CustomContentAsync <| fun context ->
            async {
                let! content = content
                let! result = genResp content context
                return f result
            }
        |> async.Return

    let SetHeaders<'T> (headers: seq<Http.Header>) (cont: Async<Content<'T>>) =
        cont
        |> MapResponse (fun resp -> { resp with Headers = headers })

    let WithHeaders<'T> (headers: seq<Http.Header>) (cont: Async<Content<'T>>) =
        cont
        |> MapResponse (fun resp ->
            let headers = (List.ofSeq headers) @ (List.ofSeq resp.Headers)
            { resp with Headers = headers }
        )

    let WithHeader<'T> (name: string) (value: string) (cont: Async<Content<'T>>) =
        cont |> WithHeaders [Http.Header.Custom name value]

    let WithContentType<'T> (contentType: string) (cont: Async<Content<'T>>) =
        cont |> WithHeaders [Http.Header.Custom "Content-Type" contentType]

    let SetStatus<'T> (status: Http.Status) (cont: Async<Content<'T>>) =
        cont
        |> MapResponse (fun resp -> { resp with Status = status })

    let SetBody<'T> (writeBody: System.IO.Stream -> unit) (cont: Async<Content<'T>>) =
        cont
        |> MapResponse (fun resp -> { resp with WriteBody = Http.WriteBody writeBody })

    /// Emits a 301 Moved Permanently response to a given URL.
    let RedirectToUrl<'T> (url: string) : Content<'T> =
        CustomContent <| fun ctx ->
            {
                Status = Http.Status.Custom 301 (Some "Moved Permanently")
                Headers = [Http.Header.Custom "Location" url]
                WriteBody = Http.EmptyBody
            }

    /// Emits a 301 Moved Permanently response to a given action.
    let Redirect<'T> (endpoint: 'T) =
        CustomContentAsync <| fun ctx ->
            let resp = RedirectToUrl (ctx.Link endpoint)
            ToResponse resp ctx

    let RedirectPermanentToUrl url = RedirectToUrl url |> async.Return
    let RedirectPermanent endpoint = Redirect endpoint |> async.Return

    /// Emits a 307 Redirect Temporary response to a given url.
    let RedirectTemporaryToUrl<'T> (url: string) : Async<Content<'T>> =
        CustomContent <| fun ctx ->
            {
                Status = Http.Status.Custom 307 (Some "Temporary Redirect")
                Headers = [Http.Header.Custom "Location" url]
                WriteBody = Http.EmptyBody
            }
        |> async.Return

    /// Emits a 307 Redirect Temporary response to a given url.
    let RedirectTemporary<'T> (endpoint: 'T) : Async<Content<'T>> =
        CustomContentAsync <| fun ctx -> async {
            let! content = RedirectTemporaryToUrl (ctx.Link endpoint)
            return! ToResponse content ctx
        }
        |> async.Return

    /// Constructs a status code response.
    let httpStatusContent<'T> status : Async<Content<'T>> =
        CustomContent <| fun ctx ->
            {
                Status = status
                Headers = []
                WriteBody = Http.EmptyBody
            }
        |> async.Return

    let Unauthorized<'T> : Async<Content<'T>> =
        httpStatusContent Http.Status.Unauthorized

    let Forbidden<'T> : Async<Content<'T>> =
        httpStatusContent Http.Status.Forbidden

    let NotFound<'T> : Async<Content<'T>> =
        httpStatusContent Http.Status.NotFound

    let NotImplemented<'T> : Async<Content<'T>> =
        httpStatusContent Http.Status.NotImplemented

    let ServerError<'T> : Async<Content<'T>> =
        httpStatusContent Http.Status.InternalServerError

    let MethodNotAllowed<'T> : Async<Content<'T>> =
        httpStatusContent Http.Status.MethodNotAllowed

    let Ok<'T> : Async<Content<'T>> =
        httpStatusContent Http.Status.Ok          

    let getOkOrigin (allows: CorsAllows) (request: Http.Request) =
        request.Headers
        |> Seq.tryFind (fun h -> h.Name.ToLowerInvariant() = "origin")
        |> Option.bind (fun origin ->
            if allows.Origins |> Seq.contains origin.Value then Some origin.Value
            elif allows.Origins |> Seq.contains "*" then Some "*"
            else None
        )

    let Cors (cors: Cors<'T>) (allows: CorsAllows -> CorsAllows) (content: 'T -> Async<Content<'U>>) : Async<Content<'U>> =
        let allows = allows (defaultArg cors.DefaultAllows CorsAllows.Empty)
        let headers (ctx: Context<'U>) =
            match getOkOrigin allows ctx.Request with
            | Some origin ->
                [
                    yield Http.Header.Custom "Access-Control-Allow-Origin" origin
                    match List.ofSeq allows.Methods with
                    | [] -> ()
                    | l -> yield Http.Header.Custom "Access-Control-Allow-Methods" (String.concat ", " l)
                    match List.ofSeq allows.Headers with
                    | [] -> ()
                    | l -> yield Http.Header.Custom "Access-Control-Allow-Headers" (String.concat ", " l)
                    match List.ofSeq allows.ExposeHeaders with
                    | [] -> ()
                    | l -> yield Http.Header.Custom "Access-Control-Expose-Headers" (String.concat ", " l)
                    match allows.MaxAge with
                    | None -> ()
                    | Some age -> yield Http.Header.Custom "Access-Control-Max-Age" (string age)
                    if allows.Credentials then
                        yield Http.Header.Custom "Access-Control-Allow-Credentials" "true"
                ]
            | None -> []
        match cors.EndPoint with
        | None ->
            CustomContent <| fun ctx ->
                {
                    Status = Http.Status.Ok
                    Headers = headers ctx
                    WriteBody = Http.EmptyBody
                }
            |> async.Return
        | Some ep ->
            CustomContentAsync <| fun ctx -> async {
                let! content = content ep
                let! resp =
                    match content with
                    | CustomContent f -> async.Return (f ctx)
                    | CustomContentAsync f -> f ctx
                return { resp with Headers = Seq.append (headers ctx) resp.Headers }
            }
            |> async.Return

    let Bundle name (contents: #seq<#WebSharper.Web.INode>) =
        Seq.append
            (Seq.singleton (Web.BundleNode(name) :> Web.INode))
            (Seq.cast contents)  
                   
    let BundleScope (name: string) (contents: 'A) =
        contents

    let BundleScopes (names: string[]) (contents: 'A) =
        contents

[<System.Runtime.CompilerServices.Extension; Sealed>]
type ContextExtensions =

    [<System.Runtime.CompilerServices.Extension>]
    static member GetSeparateResourcesAndScripts(this, controls) =
        Content.getSeparateResourcesAndScripts this controls

    [<System.Runtime.CompilerServices.Extension>]
    static member GetResourcesAndScripts(this, controls) =
        Content.getResourcesAndScripts this controls

open Microsoft.FSharp.Quotations.Patterns
module R = WebSharper.Core.AST.Reflection
module M = WebSharper.Core.Metadata

type Content<'Endpoint> with

    static member Custom (response: Http.Response) : Async<Content<'Endpoint>> =
        Content.CustomContent <| fun _ -> response
        |> async.Return

    static member Custom (?Status: Http.Status, ?Headers: seq<Http.Header>, ?WriteBody: System.IO.Stream -> unit) : Async<Content<'Endpoint>> =
        ({
            Status = defaultArg Status Http.Status.Ok
            Headers = defaultArg Headers Seq.empty
            WriteBody = defaultArg (WriteBody |> Option.map Http.WriteBody) Http.EmptyBody
        } : Http.Response)
        |> Content.Custom

    static member CustomAsync (?Status: Http.Status, ?Headers: seq<Http.Header>, ?WriteBody: System.IO.Stream -> Task) : Async<Content<'Endpoint>> =
        ({
            Status = defaultArg Status Http.Status.Ok
            Headers = defaultArg Headers Seq.empty
            WriteBody = defaultArg (WriteBody |> Option.map Http.WriteBodyAsync) Http.EmptyBody
        } : Http.Response)
        |> Content.Custom

    static member Json x : Async<Content<'Endpoint>> =
        Content.JsonContent <| fun _ -> x
        |> async.Return

    static member Page (?Body: #seq<#WebSharper.Web.INode>, ?Head:#seq<#WebSharper.Web.INode>, ?Title: string, ?Doctype: string, ?Bundle: string) : Async<Content<'Endpoint>> =
        let body = match Body with None -> Seq.empty | Some x -> Seq.cast x
        let bodyWithBundle = 
            match Bundle with
            | None -> body 
            | Some b -> Seq.append (Seq.singleton (Web.BundleNode(b) :> Web.INode)) body
        Content.Page({
            Doctype = Some (match Doctype with Some d -> d | None -> "<!DOCTYPE html>")
            Title = Title
            Head = match Head with None -> Seq.empty | Some x -> Seq.cast x
            Body = bodyWithBundle
            Renderer = Page.Default.Renderer
        })

    static member Page (page: Page, ?Bundle: string) : Async<Content<'Endpoint>> =
        let pageWithBundle =
            match Bundle with
            | None -> page 
            | Some b -> 
                { page with Body = Seq.append (Seq.singleton (Web.BundleNode(b) :> Web.INode)) page.Body }
        Content.CustomContentAsync (Content.toCustomContentAsync (fun _ -> async { return pageWithBundle }))
        |> async.Return

    static member Text (text: string, ?encoding: System.Text.Encoding) : Async<Content<'Endpoint>> =
        let encoding = defaultArg encoding Content.defaultEncoding
        Content.CustomAsync(
            WriteBody = fun s ->
                task {
                    let w = new System.IO.StreamWriter(s, encoding, 1024)
                    do! w.WriteAsync(text)
                    do! w.FlushAsync()
                }
        )

    static member File (path: string, ?AllowOutsideRootFolder: bool, ?ContentType) : Async<Content<'Endpoint>> =
        let allowOutsideRootFolder = defaultArg AllowOutsideRootFolder false
        Content.CustomContent <| fun ctx ->
            if Path.IsPathRooted path && not allowOutsideRootFolder then
                failwith "Cannot serve file from outside the application's root folder"
            let rootFolder = DirectoryInfo(ctx.WebRootFolder).FullName
            let path =
                if path.StartsWith "~/" || path.StartsWith @"~\" then
                    Path.Combine(rootFolder, path.[2..])
                else
                    Path.Combine(rootFolder, path)
            let fi = System.IO.FileInfo(path)
            if fi.FullName.StartsWith rootFolder || allowOutsideRootFolder then
                {
                    Status = Http.Status.Ok
                    Headers = [if ContentType.IsSome then yield Http.Header.Custom "Content-Type" ContentType.Value]
                    WriteBody = Http.WriteBodyAsync(fun out ->
                        task {
                            use inp = fi.OpenRead()
                            do! inp.CopyToAsync(out, 16 * 1024)
                        }
                    )
                }
            else
                failwith "Cannot serve file from outside the application's root folder"
        |> async.Return

    static member PageFromFile (path: string, [<JavaScript; ReflectedDefinition>] init: Quotations.Expr<unit -> unit>) : Async<Content<'Endpoint>> =
        Content.CustomContent <| fun ctx ->
            if Path.IsPathRooted path then
                failwith "Cannot serve file from outside the application's root folder"
            let rootFolder = DirectoryInfo(ctx.RootFolder).FullName
            let path =
                if path.StartsWith "~/" || path.StartsWith @"~\" then
                    Path.Combine(rootFolder, path.[2..])
                else
                    Path.Combine(rootFolder, path)
            let bundleName = Path.GetFileNameWithoutExtension(path)
            let fi = System.IO.FileInfo(path)
            if fi.FullName.StartsWith rootFolder then
                {
                    Status = Http.Status.Ok
                    Headers = [Http.Header.Custom "Content-Type" "text/html; charset=utf-8"]
                    WriteBody = Http.WriteBodyAsync(fun out ->
                        task {
                            use inp = new StreamReader(path)
                            let! contents = inp.ReadToEndAsync()                            
                            use scriptsW = new StringWriter()
                            use tw = new HtmlTextWriter(scriptsW, " ")
                            let applyCode code =
                                ClientApply(code, [])
                            let reqs = 
                                match Content.compile ctx.Metadata ctx.Json init applyCode with
                                | Some b -> b 
                                | _ ->
                                    let m =
                                        match init with
                                        | Lambda (x1, Call(None, m, [Var x2])) when x1 = x2 -> m
                                        | _ -> failwithf "Invalid handler function: %A" init
                                    let loc = WebSharper.Web.ClientSideInternals.getLocation' init
                                    let meth = R.ReadMethod m
                                    let declType = R.ReadTypeDefinition m.DeclaringType
                                    let reqs = [M.MethodNode (declType, meth); M.TypeNode declType]
                                    let fail() =
                                        failwithf "Error in Handler%s: Couldn't find JavaScript address for method %s.%s"
                                            loc declType.Value.FullName meth.Value.MethodName
                                    let code =
                                        match ctx.Metadata.Classes.TryGetValue declType with
                                        | true, (clAddr, _, Some c) ->
                                            let addr =
                                                match c.Methods.TryGetValue meth with
                                                | true, info ->
                                                    match info.CompiledForm with
                                                    | M.CompiledMember.Static (name, false, Core.AST.MemberKind.Simple) ->
                                                        clAddr.Sub(name)
                                                    | M.CompiledMember.GlobalFunc (addr, false) ->
                                                        addr
                                                    | M.CompiledMember.Func (name, false) ->
                                                        clAddr.Func(name)
                                                    | _ -> fail()
                                                | _ -> fail()
                                            applyCode (ClientImport addr)
                                        | _ -> fail()
                                    code :: (reqs |> List.map ClientRequire) :> seq<_>
                            let body = [ Web.BundleNode(bundleName, reqs = reqs) ]
                            let activation, bundleNames = Content.writeResources ctx body (fun _ -> tw)
                            if Option.isSome activation then
                                tw.WriteStartCode(ctx.ResourceContext.ScriptBaseUrl, ?activation = activation, ?bundleNames = bundleNames)
                            let resp = contents.Replace("""<script ws-replace="scripts"></script>""", scriptsW.ToString())
                            let w = new StreamWriter(out, System.Text.Encoding.UTF8, 1024, leaveOpen = true)
                            do! w.WriteAsync(resp)
                            do! w.FlushAsync()
                        }
                    )
                }
            else
                failwith "Cannot serve file from outside the application's root folder"
        |> async.Return

    static member MvcResult (result: obj) : Async<Content<'Endpoint>> =
        ({
            Status = Http.Status.Ok
            Headers =Seq.empty
            WriteBody = Http.MvcBody result
        } : Http.Response)
        |> Content.Custom

open System
open System.Threading.Tasks
open System.Runtime.InteropServices
open System.Runtime.CompilerServices

#nowarn "49" // allow uppercase parameter names

[<AutoOpen>]
module Internal =

    type Task<'A> with
        member t.Map (f: 'A -> 'B) =
            t.ContinueWith(fun (t: Task<'A>) -> f t.Result)

[<CompiledName "Content"; Struct; NoEquality; NoComparison>]
type CSharpContent =

    val private c: Content<obj>

    new(c) = { c = c }

    static member ToContent (c: Async<Content<obj>>) : Task<CSharpContent> =
        Async.StartAsTask(c)
            .ContinueWith(fun (t: Task<Content<obj>>) -> CSharpContent t.Result)

    static member Opt x =
        match box x with
        | null -> None
        | _ -> Some x

    member this.AsContent = this.c

    static member FromContent(c: Content<obj>) = new CSharpContent(c)

    static member TaskAsContent (t: Task<CSharpContent>) =
        t.Map(fun c -> c.AsContent)

    static member Json<'U> (x: 'U) : Task<CSharpContent> =
        Content.Json x
        |> CSharpContent.ToContent

    static member Page
        (
            [<Optional>] Body: Web.INode,
            [<Optional>] Head: Web.INode,
            [<Optional>] Title: string,
            [<Optional>] Doctype: string,
            [<Optional>] Bundle: string
        ) =
        let body = Option.map Seq.singleton (CSharpContent.Opt Body) 
        let bodyWithBundle = 
            match Bundle with
            | null -> body 
            | b -> Some (Seq.append (Seq.singleton (Web.BundleNode(b) :> Web.INode)) (Seq.concat (Option.toArray body)))
        Content.Page(
            ?Body = bodyWithBundle,
            ?Head = Option.map Seq.singleton (CSharpContent.Opt Head),
            ?Title = CSharpContent.Opt Title, ?Doctype = CSharpContent.Opt Doctype)
        |> CSharpContent.ToContent

    static member Page (page: Page) =
        Content.Page(page)
        |> CSharpContent.ToContent

    static member Bundle (name, contents: Web.INode) =
        Web.BundleNode(name, contents) :> Web.INode

    static member BundleScope (name: string, contents: 'A) =
        contents

    static member BundleScopes (name: string[], contents: 'A) =
        contents

    static member Text (text: string, [<Optional>] Encoding: System.Text.Encoding) =
        Content.Text(text, ?encoding = CSharpContent.Opt Encoding)
        |> CSharpContent.ToContent

    static member File (path: string, [<Optional>] AllowOutsideRootFolder: bool, [<Optional>] ContentType: string) =
        Content.File(path, AllowOutsideRootFolder, ?ContentType = CSharpContent.Opt ContentType)
        |> CSharpContent.ToContent

    static member MvcResult (result: obj) =
        Content.MvcResult(result)
        |> CSharpContent.ToContent

    static member Custom (response: Http.Response) =
        Content.Custom(response)
        |> CSharpContent.ToContent

    static member Custom
        (
            [<Optional>] Status: Http.Status,
            [<Optional>] Headers: seq<Http.Header>,
            [<Optional>] WriteBody: Action<Stream>
        ) =
        Content.Custom(?Status = CSharpContent.Opt Status, ?Headers = CSharpContent.Opt Headers, WriteBody = WriteBody.Invoke)
        |> CSharpContent.ToContent

    static member CustomAsync
        (
            [<Optional>] Status: Http.Status,
            [<Optional>] Headers: seq<Http.Header>,
            [<Optional>] WriteBody: Func<Stream, Task>
        ) =
        Content.CustomAsync(?Status = CSharpContent.Opt Status, ?Headers = CSharpContent.Opt Headers, WriteBody = WriteBody.Invoke)
        |> CSharpContent.ToContent

    static member FromContext (f: Func<Context, Task<CSharpContent>>) =
        Content.FromContext (fun x -> Async.AwaitTask (f.Invoke(Context x).Map(fun c -> c.AsContent)))
        |> CSharpContent.ToContent

    [<Extension>]
    static member ToResponse (content: CSharpContent, context: Context) =
        Content.ToResponse content.AsContent context
        |> Async.StartAsTask

    static member FromTask (content: Task<CSharpContent>) =
        content
        |> CSharpContent.TaskAsContent
        |> Async.AwaitTask
        |> Content.FromAsync
        |> CSharpContent

    [<Extension>]
    static member MapResponse (content: Task<CSharpContent>, f: Func<Http.Response, Http.Response>) =
        content
        |> CSharpContent.TaskAsContent
        |> Async.AwaitTask
        |> Content.MapResponse f.Invoke
        |> CSharpContent.ToContent

    [<Extension>]
    static member MapResponseAsync (content: Task<CSharpContent>, f: Func<Http.Response, Task<Http.Response>>) =
        content
        |> CSharpContent.TaskAsContent
        |> Async.AwaitTask
        |> Content.MapResponseAsync (fun r -> Async.AwaitTask (f.Invoke r))
        |> CSharpContent.ToContent

    [<Extension>]
    static member WithHeaders (content: Task<CSharpContent>, headers: seq<Http.Header>) =
        content
        |> CSharpContent.TaskAsContent
        |> Async.AwaitTask
        |> Content.WithHeaders headers
        |> CSharpContent.ToContent

    [<Extension>]
    static member WithHeaders (content: Task<CSharpContent>, [<ParamArray>] headers: Http.Header[]) =
        content
        |> CSharpContent.TaskAsContent
        |> Async.AwaitTask
        |> Content.WithHeaders headers
        |> CSharpContent.ToContent

    [<Extension>]
    static member WithHeader (content: Task<CSharpContent>, name: string, value: string) =
        content
        |> CSharpContent.TaskAsContent
        |> Async.AwaitTask
        |> Content.WithHeader name value
        |> CSharpContent.ToContent

    [<Extension>]
    static member WithContentType (content: Task<CSharpContent>, contentType: string) =
        content
        |> CSharpContent.TaskAsContent
        |> Async.AwaitTask
        |> Content.WithContentType contentType
        |> CSharpContent.ToContent

    [<Extension>]
    static member SetHeaders (content: Task<CSharpContent>, headers: seq<Http.Header>) =
        content
        |> CSharpContent.TaskAsContent
        |> Async.AwaitTask
        |> Content.SetHeaders headers
        |> CSharpContent.ToContent

    [<Extension>]
    static member SetHeaders (content: Task<CSharpContent>, [<ParamArray>] headers: Http.Header[]) =
        content
        |> CSharpContent.TaskAsContent
        |> Async.AwaitTask
        |> Content.SetHeaders headers
        |> CSharpContent.ToContent

    [<Extension>]
    static member SetStatus (content: Task<CSharpContent>, status: Http.Status) =
        content
        |> CSharpContent.TaskAsContent
        |> Async.AwaitTask
        |> Content.SetStatus status
        |> CSharpContent.ToContent

    [<Extension>]
    static member SetStatus (content: Task<CSharpContent>, code: int, [<Optional>] message: string) =
        content
        |> CSharpContent.TaskAsContent
        |> Async.AwaitTask
        |> Content.SetStatus (Http.Status.Custom code (CSharpContent.Opt message))
        |> CSharpContent.ToContent

    [<Extension>]
    static member SetBody (content: Task<CSharpContent>, writeBody: Action<Stream>) =
        content
        |> CSharpContent.TaskAsContent
        |> Async.AwaitTask
        |> Content.SetBody writeBody.Invoke
        |> CSharpContent.ToContent

    static member RedirectPermanent (action: obj) =
        Content.RedirectPermanent action
        |> CSharpContent.ToContent

    static member RedirectPermanentToUrl (url: string) =
        Content.RedirectPermanentToUrl url
        |> CSharpContent.ToContent

    static member RedirectTemporary (action: obj) =
        Content.RedirectTemporary action
        |> CSharpContent.ToContent

    static member RedirectTemporaryToUrl (url: string) =
        Content.RedirectTemporaryToUrl url
        |> CSharpContent.ToContent

    static member Unauthorized() =
        Content.Unauthorized
        |> CSharpContent.ToContent

    static member Forbidden() =
        Content.Forbidden
        |> CSharpContent.ToContent

    static member NotFound() =
        Content.NotFound
        |> CSharpContent.ToContent

    static member ServerError() =
        Content.ServerError
        |> CSharpContent.ToContent

    static member MethodNotAllowed() =
        Content.MethodNotAllowed
        |> CSharpContent.ToContent
