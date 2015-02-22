// $begin{copyright}
//
// This file is part of WebSharper
//
// Copyright (c) 2008-2014 IntelliFactory
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

namespace WebSharper.Formlets.Tests

open WebSharper
open WebSharper.JavaScript
open WebSharper.Html.Client
open WebSharper.Formlets
open System

module Utils =

    [<Inline "console.log($x)">]
    let Log x = ()

module BF =
    open IntelliFactory.Formlets.Base

    [<JavaScript>]
    let RX = IntelliFactory.Reactive.Reactive.Default

    [<JavaScript>]
    let U = { Reactive = RX; DefaultLayout = DefaultLayout}

    [<JavaScript>]
    let L = new LayoutUtils({Reactive = U.Reactive})

    // Generic formlet type.
    type BFormlet<'B,'T> =
        {
            Layout : Layout<'B>
            Build : unit -> Form<'B,'T>
            Utils : Utils<'B>
        }
        interface IFormlet<'B,'T> with
            [<ReflectedDefinition>]
            member this.Layout = this.Layout
            [<ReflectedDefinition>]
            member this.Build () = this.Build ()
            [<ReflectedDefinition>]
            member this.MapResult (f : Result<'T> -> Result<'U>) =
                {
                    Layout = this.Layout
                    Build = fun () ->
                        let form = this.Build()
                        let state = this.Utils.Reactive.Select form.State (fun x -> f x)
                        let state = form.State
                        {
                            Body = form.Body
                            Dispose = form.Dispose
                            Notify = form.Notify
                            State =  state
                        }
                    Utils = this.Utils
                } :> IFormlet<'B,'T>

    [<JavaScript>]
    let BuildForm(formlet: IFormlet<'B,'T>) =
        let form = formlet.Build()
        match formlet.Layout.Apply form.Body with
        | None ->
            form
        | Some (body, d) ->
            { form with
                Body = U.Reactive.Return (Tree.Set body)
                Dispose = fun () -> form.Dispose(); d.Dispose()
            }

    /// Creates a new formlet with the default layout.
    [<JavaScript>]
    let New build =
        { 
            Build = build
            Layout = L.Default<Body>()
            Utils = U 
        } 

    [<JavaScript>]
    let Fail<'T> fs : Form<Body, 'T> =
        {
            Body    = U.Reactive.Never ()
            Dispose = id
            Notify  = ignore
            State   = U.Reactive.Return (Failure fs)
        }

    [<JavaScript>]
    let Join (formlet: IFormlet<Body, IFormlet<Body, 'T>>) : IFormlet<Body, 'T> =

        Utils.Log "In Join"

        let build  () =
            let form1 = BuildForm(formlet)
            let formStream =
                U.Reactive.Select form1.State (fun res ->
                        match res with
                        | Success innerF ->
                            BuildForm innerF
                        | Failure fs ->
                            Fail fs
                )
                |> U.Reactive.Heat

            let body =
                let right =
                    let value =
                        U.Reactive.Select formStream (fun f ->
                            let delete = U.Reactive.Return (Tree.Delete())
                            U.Reactive.Concat delete f.Body
                        )
                    U.Reactive.Select (U.Reactive.Switch(value)) Tree.Right
                U.Reactive.Merge (U.Reactive.Select (form1.Body) Tree.Left) right

            let state =
                U.Reactive.Switch (U.Reactive.Select formStream (fun f -> f.State))
            let notify o =
                form1.Notify o

            let dispose () =
                form1.Dispose()


            // What's going on with the body stream
            body.Subscribe (fun edit ->
                let x =
                    match edit with
                    | Tree.Edit.Left _ -> "Left"
                    | Tree.Edit.Replace _ -> "Replace"
                    | Tree.Edit.Right _ -> "Right"
                Utils.Log x
            )
            |> ignore

            { Body = body; Notify = notify; Dispose = dispose; State = state}

        New build
        :> IFormlet<_,_>


    /// Maps the result.
    [<JavaScript>]
    let MapResult<'T,'U> (f: Result<'T> -> Result<'U>)
                            (formlet: IFormlet<Body,'T>) : IFormlet<Body,'U>  =
        {
            Build = fun () ->
                let form = formlet.Build()
                let state = U.Reactive.Select form.State (fun x -> f x)
                {
                    Body = form.Body
                    Dispose = form.Dispose
                    Notify = form.Notify
                    State =  state
                }
            Layout = formlet.Layout
            Utils = U
        } :> IFormlet<_,_>

    [<JavaScript>]
    let Map (f: 'T -> 'U) (formlet: IFormlet<Body, 'T>) : IFormlet<Body, 'U> =
        MapResult (Result.Map f) formlet

    [<JavaScript>]
    let Bind<'T1, 'T2> (formlet: IFormlet<Body,'T1>) (f: 'T1 -> IFormlet<Body,'T2>) : IFormlet<Body, 'T2> =
        formlet 
        |> Map f 
        |> Join

module F =
    open IntelliFactory.Formlets.Base

    [<JavaScript>]
    let PropagateRenderFrom (f1: IFormlet<_,_>) f2 =
        if JS.HasOwnProperty f1 "Render" then
            f2?Render <- f1?Render
        f2

    [<JavaScript>]
    let Bind (fl: Formlet<'T>)  (f: 'T -> Formlet<'U>) : Formlet<'U> =
        BF.Bind fl (fun x ->
            let y = f x
            y :> IFormlet<_,_>
        )
        |> PropagateRenderFrom fl
        |> OfIFormlet

    [<Inline "typeof($x) === 'undefined'">]
    let IsUndefined x = true

module Combined =
    let Display () =
        Tests.FormletTest.TestMany

//        let p = Div []
//        let x = F.IsUndefined p.Id
//        Utils.Log(x)
//        [E:\repos\if-ws-formlets\web\Default.aspx
//            Controls.Button "Yes" |> Formlet.Map (fun _ -> true)
//            Controls.Button "No" |> Formlet.Map (fun _ -> false)
//        ]
//        |> Formlet.Choose
//        |> Formlet.Map (fun x ->
//            Utils.Log ("Result: ", x)
//        )
//        :> _

