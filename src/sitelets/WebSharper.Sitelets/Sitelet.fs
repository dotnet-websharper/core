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

#nowarn "44" // Obsolete CustomContent, CustomContentAsync, PageContent, PageContentAsync

open System
open System.Threading.Tasks
open System.Runtime.CompilerServices

type Sitelet<'T when 'T : equality> =
    {
        Router : IRouter<'T>
        Controller : Controller<'T>
    }

    static member (+) (s1: Sitelet<'T>, s2: Sitelet<'T>) =
        {
            Router = IRouter.Add s1.Router s2.Router
            Controller =
                {
                    Handle = fun endpoint ->
                        match s1.Router.Link endpoint with
                        | Some _ -> s1.Controller.Handle endpoint
                        | None -> s2.Controller.Handle endpoint
                }
        }

    static member ( <|> ) (s1: Sitelet<'T>, s2: Sitelet<'T>) = s1 + s2

/// Provides combinators over sitelets.
[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Sitelet =
    open Microsoft.FSharp.Quotations
    open Microsoft.FSharp.Reflection

    module C = Content
    type C<'T> = Content<'T>

    /// Creates an empty sitelet.
    let Empty<'T when 'T : equality> : Sitelet<'T> =
        {
            Router = Router.Empty<'T>
            Controller =
                {
                    Handle = fun endpoint ->
                        Content.CustomContent <| fun _ ->
                            {
                                Status = Http.Status.NotFound
                                Headers = []
                                WriteBody = ignore
                            }
                }
        }

    /// Creates a WebSharper.Sitelet using the given router and handler function.
    let New (router: IRouter<'T>) (handle: Context<'T> -> 'T -> Async<Content<'T>>) =
        {
            Router = router
            Controller =
            {
                Handle = fun ep ->
                    Content.CustomContentAsync <| fun ctx -> async {
                        let! content = handle ctx ep
                        return! WebSharper.Sitelets.Content.ToResponse content ctx
                    }
            }
        }

    /// Represents filters for protecting sitelets.
    type Filter<'T> =
        {
            VerifyUser : string -> bool;
            LoginRedirect : 'T -> 'T
        }

    /// Constructs a protected sitelet given the filter specification.
    let Protect (filter: Filter<'T>) (site: Sitelet<'T>)
        : Sitelet<'T> =
        {
            Router = site.Router
            Controller =
                { Handle = fun endpoint ->
                    let prot = filter
                    let failure ctx = async {
                        let! c = Content.RedirectTemporary (prot.LoginRedirect endpoint)
                        return! Content.ToResponse c ctx
                    }
                    Content.CustomContentAsync <| fun ctx ->
                        async {
                            try
                                let! loggedIn = ctx.UserSession.GetLoggedInUser ()
                                match loggedIn with
                                | Some user ->
                                    if prot.VerifyUser user then
                                        let content = site.Controller.Handle endpoint
                                        return! Content.ToResponse content ctx
                                    else
                                        return! failure ctx
                                | None ->
                                    return! failure ctx
                            with :? NullReferenceException ->
                                // If server crashes or is restarted and doesn't have a hardcoded machine
                                // key then GetLoggedInUser() throws an exception. Log out in this case.
                                do! ctx.UserSession.Logout()
                                return! failure ctx
                        }
                }
        }

    /// Constructs a singleton sitelet that contains exactly one endpoint
    /// and serves a single content value at a given location.
    let Content (location: string) (endpoint: 'T) (cnt: Context<'T> -> Async<Content<'T>>) =
        {
            Router = Router.Single endpoint location
            Controller = { Handle = fun _ ->
                Content.CustomContentAsync <| fun ctx -> async {
                    let! cnt = cnt ctx
                    return! Content.ToResponse cnt ctx
                }
            }
        }

    /// Maps over the sitelet endpoint type. Requires a bijection.
    let Map (f: 'T1 -> 'T2) (g: 'T2 -> 'T1) (s: Sitelet<'T1>) : Sitelet<'T2> =
        {
            Router = IRouter.Map f g s.Router
            Controller =
                {
                    Handle = fun endpoint ->
                        match s.Controller.Handle <| g endpoint with
                        | Content.CustomContent genResp ->
                            CustomContent (genResp << Context.Map f)
                        | Content.CustomContentAsync genResp ->
                            CustomContentAsync (genResp << Context.Map f)
                }
        }

    /// Maps over the sitelet endpoint type. Requires a bijection.
    let TryMap (f: 'T1 -> 'T2 option) (g: 'T2 -> 'T1 option) (s: Sitelet<'T1>) : Sitelet<'T2> =
        {
            Router = IRouter.TryMap f g s.Router
            Controller =
                { Handle = fun a ->
                    match g a with
                    | Some ea -> C.CustomContentAsync <| fun ctx ->
                        C.ToResponse (s.Controller.Handle ea) (Context.Map (f >> Option.get) ctx)
                    | None -> failwith "Invalid endpoint in Sitelet.Embed" }
        }

    /// Maps over the sitelet endpoint type with only an injection.
    let Embed embed unembed sitelet =
        {
            Router = IRouter.Embed embed unembed sitelet.Router
            Controller =
                { Handle = fun a ->
                    match unembed a with
                    | Some ea -> C.CustomContentAsync <| fun ctx ->
                        C.ToResponse (sitelet.Controller.Handle ea) (Context.Map embed ctx)
                    | None -> failwith "Invalid endpoint in Sitelet.Embed" }
        }

    let tryGetEmbedFunctionsFromExpr (expr: Expr<'T1 -> 'T2>) =
        match expr with
        | ExprShape.ShapeLambda(_, Patterns.NewUnionCase (uci, _)) ->
            let embed (y: 'T1) = FSharpValue.MakeUnion(uci, [|box y|]) :?> 'T2
            let unembed (x: 'T2) =
                let uci', args' = FSharpValue.GetUnionFields(box x, uci.DeclaringType)
                if uci.Tag = uci'.Tag then
                    Some (args'.[0] :?> 'T1)
                else None
            Some (embed, unembed)
        | _ -> None
 
    /// Maps over the sitelet endpoint type, where the destination type
    /// is a discriminated union with a case containing the source type.
    let EmbedInUnion (case: Expr<'T1 -> 'T2>) sitelet =
        match tryGetEmbedFunctionsFromExpr case with
        | Some (embed, unembed) -> Embed embed unembed sitelet
        | None -> failwith "Invalid union case in Sitelet.EmbedInUnion"

    /// Shifts all sitelet locations by a given prefix.
    let Shift (prefix: string) (sitelet: Sitelet<'T>) =
        {
            Router = IRouter.Shift prefix sitelet.Router
            Controller = sitelet.Controller
        }

    /// Combines several sitelets, leftmost taking precedence.
    /// Is equivalent to folding with the choice operator.
    let Sum (sitelets: seq<Sitelet<'T>>) : Sitelet<'T> =
        let sitelets = Array.ofSeq sitelets 
        if Seq.isEmpty sitelets then Empty else
            {
                Router = IRouter.Sum (sitelets |> Seq.map (fun s -> s.Router))
                Controller =
                    {
                        Handle = fun endpoint ->
                            sitelets 
                            |> Array.pick (fun s -> 
                                match s.Router.Link endpoint with
                                | Some _ -> Some (s.Controller.Handle endpoint)
                                | None -> None
                            )
                    }
            }

    /// Serves the sum of the given sitelets under a given prefix.
    /// This function is convenient for folder-like structures.
    let Folder<'T when 'T : equality> (prefix: string)
                                      (sitelets: seq<Sitelet<'T>>) =
        Shift prefix (Sum sitelets)

    /// Boxes the sitelet endpoint type to Object type.
    let Box (sitelet: Sitelet<'T>) : Sitelet<obj> =
        {
            Router = IRouter.Box sitelet.Router
            Controller =
                { Handle = fun a ->
                    C.CustomContentAsync <| fun ctx ->
                        C.ToResponse (sitelet.Controller.Handle (unbox a)) (Context.Map box ctx)
                }
        }

    let Upcast sitelet = Box sitelet

    /// Reverses the Box operation on the sitelet.
    let Unbox<'T when 'T : equality> (sitelet: Sitelet<obj>) : Sitelet<'T> =
        {
            Router = IRouter.Unbox sitelet.Router
            Controller =
                { Handle = fun a ->
                    C.CustomContentAsync <| fun ctx ->
                        C.ToResponse (sitelet.Controller.Handle (box a)) (Context.Map unbox ctx)
                }
        }

    let UnsafeDowncast sitelet = Unbox sitelet

    /// Constructs a sitelet with an inferred router and a given controller
    /// function.
    let Infer<'T when 'T : equality> (handle : Context<'T> -> 'T -> Async<Content<'T>>) =
        {
            Router = Router.IInfer<'T>()
            Controller = { Handle = fun x ->
                C.CustomContentAsync <| fun ctx -> async {
                    let! content = handle ctx x
                    return! C.ToResponse content ctx
                }
            }
        }

    [<Obsolete>]
    let InferWithCustomErrors<'T when 'T : equality> (handle : Context<'T> -> ParseRequestResult<'T> -> Async<Content<'T>>) =
        {
            Router = Router.IInferWithCustomErrors<'T>()
            Controller = { Handle = fun x ->
                C.CustomContentAsync <| fun ctx -> async {
                    let ctx = (Context.Map ParseRequestResult.Success ctx)
                    let! content = handle ctx x
                    return! C.ToResponse content ctx
                }
            }
        }

    let InferPartial (embed: 'T1 -> 'T2) (unembed: 'T2 -> 'T1 option) (mkContent: Context<'T2> -> 'T1 -> Async<Content<'T2>>) : Sitelet<'T2> =
        {
            Router = Router.IInfer<'T1>() |> IRouter.Embed embed unembed
            Controller = { Handle = fun p ->
                C.CustomContentAsync <| fun ctx -> async {
                    match unembed p with
                    | Some e ->
                        let! content = mkContent ctx e
                        return! C.ToResponse content ctx
                    | None -> return failwith "Invalid endpoint in Sitelet.InferPartial"
                }
            }
        }

    let InferPartialInUnion (case: Expr<'T1 -> 'T2>) mkContent =
        match tryGetEmbedFunctionsFromExpr case with
        | Some (embed, unembed) -> InferPartial embed unembed mkContent
        | None -> failwith "Invalid union case in Sitelet.InferPartialInUnion"

type RouteHandler<'T> = delegate of Context<obj> * 'T -> Task<CSharpContent> 

[<CompiledName "Sitelet"; Sealed>]
type CSharpSitelet =
        
    static member Empty = Sitelet.Empty<obj>   

    static member New(router: Router<'T>, handle: RouteHandler<'T>) =
        Sitelet.New (Router.Box router) (fun ctx ep -> 
            async {
                let! c = handle.Invoke(ctx, unbox<'T> ep) |> Async.AwaitTask
                return c.AsContent
            }
        )

    static member Content (location: string, endpoint: 'T, cnt: Func<Context<'T>, Task<Content<'T>>>) =
        Sitelet.Content location endpoint (cnt.Invoke >> Async.AwaitTask) 
        
    static member Sum ([<ParamArray>] sitelets: Sitelet<'T>[]) =
        Sitelet.Sum sitelets

    static member Folder (prefix, [<ParamArray>] sitelets: Sitelet<'T>[]) =
        Sitelet.Folder prefix sitelets

type Sitelet<'T when 'T : equality> with
    member this.Box() =
        Sitelet.Box this

    member this.Protect (verifyUser: Func<string, bool>, loginRedirect: Func<'T, 'T>) =
        this |> Sitelet.Protect {
            VerifyUser = verifyUser.Invoke 
            LoginRedirect = loginRedirect.Invoke
        }

    member this.Map (embed: Func<'T, 'U>, unembed: Func<'U, 'T>) =
        Sitelet.TryMap (embed.Invoke >> ofObjNoConstraint) (unembed.Invoke >> ofObjNoConstraint) this
        
    member this.Shift (prefix: string) =
        Sitelet.Shift prefix this

[<Extension>]
type SiteletExtensions =
    static member Unbox<'T when 'T: equality>(sitelet: Sitelet<obj>) =
        Sitelet.Unbox<'T> sitelet

type SiteletBuilder() =

    let mutable sitelets = []

    member this.With<'T when 'T : equality>(content: Func<Context, 'T, Task<CSharpContent>>) =
        let sitelet =
            Sitelet.InferPartial
                box
                tryUnbox<'T>
                (fun ctx endpoint -> async {
                    let! content =
                        content.Invoke(Context(ctx), endpoint)
                        |> Async.AwaitTask
                    return
                        match content.AsContent with
                        | CustomContent f -> CustomContent (f << Context.Map box)
                        | CustomContentAsync f -> CustomContentAsync (f << Context.Map box)
                })
        sitelets <- sitelet :: sitelets
        this

    member this.With(path: string, content: Func<Context, Task<CSharpContent>>) =
        let content ctx =
            content.Invoke(Context(ctx))
                .ContinueWith(fun (t: Task<CSharpContent>) -> t.Result.AsContent)
            |> Async.AwaitTask
        sitelets <- Sitelet.Content path (box path) content :: sitelets
        this

    member this.Install() =
        Sitelet.Sum (List.rev sitelets)
