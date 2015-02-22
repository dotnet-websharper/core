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
namespace WebSharper.Formlets

open WebSharper
open WebSharper.JavaScript
open IntelliFactory.Formlets.Base
open IntelliFactory.Reactive
open WebSharper.Html.Client

/// Defines formlets and their operations.
[<AutoOpen>]
module Data =

    [<JavaScript>]
    let private NewBody = Formlets.Body.New

    [<JavaScript>]
    let internal RX = Reactive.Default

    [<JavaScript>]
    let internal Layout =
        LayoutProvider(
            LayoutUtils({Reactive = IntelliFactory.Reactive.Reactive.Default})
        )

    /// Default provider of Reactive implementation and layout.
    [<JavaScript>]
    let mutable DefaultLayout = Layout.Vertical

    [<JavaScript>]
    let UtilsProvider() = { Reactive = RX; DefaultLayout = DefaultLayout}

    [<JavaScript>]
    let internal BaseFormlet() = FormletProvider(UtilsProvider())

    // WebSharper formlet type.
    [<JavaScript>]
    type Formlet<'T>
        (
            buildInternal: unit -> Form<Body, 'T>,
            layoutInternal: Layout<Body>,
            formletBase: FormletProvider<Body>,
            utils: IntelliFactory.Formlets.Base.Utils<Body>
        ) =
        inherit Pagelet()

        member val internal ElementInternal : option<Element> = None with get, set

        /// Returns the internal element. Multiple invocations of `Run` will
        /// return the same element.
        member this.Run (f: 'T -> unit) =
            match this.ElementInternal with
            | Some el   ->
                el :> Pagelet
            | None      ->
                let formlet = formletBase.ApplyLayout this
                let form = formlet.Build()
                form.State.Subscribe (fun res ->
                    Result.Map f res |> ignore
                ) |> ignore
                // Defaults to vertical layout
                let el =
                    match formlet.Layout.Apply(form.Body) with
                    | Some (body, _) ->
                        body.Element
                    | None ->
                        let (body, _) = DefaultLayout.Apply(form.Body).Value
                        body.Element
                this.ElementInternal <- Some el
                el :> Pagelet

        /// IFormlet implementation.
        interface IFormlet<Body,'T> with
            member this.Build () = buildInternal ()
            member this.Layout = layoutInternal
            member this.MapResult (f : Result<'T> -> Result<'U>) =
                let x =
                    Formlet<'U>(
                        buildInternal = (fun () ->
                            let form = buildInternal()
                            {
                                Body = form.Body
                                Dispose = form.Dispose
                                Notify = form.Notify
                                State =  utils.Reactive.Select form.State (fun x -> f x)
                            }),
                        layoutInternal = layoutInternal,
                        formletBase = formletBase,
                        utils = utils
                    )
                unbox x

        /// Pagelet implementation.
        override this.Body =
            (this.Run ignore).Body
        override this.Render () =
            (this.Run ignore).Render ()

    /// Formlet result type.
    type Result<'T> = IntelliFactory.Formlets.Base.Result<'T>

    // Ugly hack for propagating render from IPagelets
    [<JavaScript>]
    let internal PropagateRenderFrom (f1: IFormlet<_,_>) f2 =
        if JS.HasOwnProperty f1 "Render" then
            f2?Render <- f1?Render
        f2

    /// Construct a formlet from an IFormlet.
    [<JavaScript>]
    let OfIFormlet (formlet: IFormlet<Body, 'T>) : Formlet<'T> =
        Formlet<'T>(
            buildInternal = formlet.Build,
            layoutInternal = formlet.Layout,
            formletBase = BaseFormlet (),
            utils = UtilsProvider ()
        )
        |> PropagateRenderFrom formlet

    [<JavaScript>]
    let internal MkFormlet f  : Formlet<'T> =
        (BaseFormlet()).New <| fun () ->
            let (body,reset,state) = f ()
            {
                Body =
                    NewBody body None
                    |> Tree.Set
                    |> RX.Return
                Notify  = fun _ -> reset ()
                Dispose = fun () -> ()
                State   = state
            }
        |> OfIFormlet

    [<Inline("$text.match(new RegExp($regex))")>]
    let internal RegexMatch regex text = false

    type internal ValidatorProvidor [<JavaScript>] () =
        interface IValidatorProvider with
            [<JavaScript>]
            member this.Matches regex text =
                RegexMatch regex text

    /// Basic validation provider.
    [<JavaScript>]
    let Validator = Validator(ValidatorProvidor())

    /// Changes a function inside the formlet to a
    /// function over values of the formlet.
    [<JavaScript>]
    [<Name "$">]
    let (<*>) (f : Formlet<'T -> 'U>) (x : Formlet<'T>) : Formlet<'U> =
        (BaseFormlet()).Apply f x
        |> OfIFormlet

/// Formlet builder.
type FormletBuilder[<JavaScript>]() =

    [<JavaScript>]
    member this.Return x =
        (BaseFormlet()).Return x
        |> OfIFormlet

    [<JavaScript>]
    member this.Bind(formlet : Formlet<'T>, f : 'T -> Formlet<'U>) =
        (BaseFormlet()).Bind formlet (fun x ->
            let y = f x
            y :> IFormlet<_,_>
        )
        |> PropagateRenderFrom formlet
        |> OfIFormlet

    [<JavaScript>]
    member this.Delay (f : unit -> Formlet<'T>) =
        (BaseFormlet()).Delay (fun x ->
            f x :> IFormlet<_,_>
        )
        |> OfIFormlet

    [<JavaScript>]
    member this.ReturnFrom f =
        f |> OfIFormlet

module Formlet =

    /// Utility for constructing new formlets from a function
    // generating a tupe of an HTML element, reset function and
    // state.
    [<JavaScript>]
    let BuildFormlet f = MkFormlet f


    /// Utility function for constructing a formlet from a
    /// form generating function.
    [<JavaScript>]
    let New f =
        (BaseFormlet()).New f
        |> OfIFormlet

    /// Creates a formlet by either applying the layout component
    /// of the given formlet or the default layout.
    [<JavaScript>]
    let WithLayoutOrDefault (formlet: Formlet<'T>) : Formlet<'T> =
        (BaseFormlet()).WithLayoutOrDefault formlet
        |> PropagateRenderFrom formlet
        |> OfIFormlet


    /// Lifts a value into a formlet with an empty body,
    /// yielding that value.
    [<JavaScript>]
    let Return x =
        (BaseFormlet()).Return x
        |> OfIFormlet

    /// Lifts a value into a formlet with an empty body,
    /// yielding that value.
    [<Inline>]
    [<JavaScript>]
    let Yield x = Return x

    /// Converts the given formlet into a formlet enhanced
    /// with a cancelation formlet. When the canecelation formlet
    /// fires, the formlet will produce the value None.
    [<JavaScript>]
    let WithCancelation (formlet: Formlet<'T>)  (c: Formlet<unit>) =
        (BaseFormlet()).WithCancelation formlet c
        |> PropagateRenderFrom formlet
        |> OfIFormlet

    /// Initializes a formlet with a default value
    /// that is triggered first.
    [<JavaScript>]
    let InitWith value (formlet: Formlet<'T>) =
        (BaseFormlet()).InitWith value formlet
        |> PropagateRenderFrom formlet
        |> OfIFormlet

    /// Initializes a formlet with a failing state.
    [<JavaScript>]
    let InitWithFailure (formlet: Formlet<'T>)  =
        (BaseFormlet()).InitWithFailure formlet
        |> PropagateRenderFrom formlet
        |> OfIFormlet

    /// Enhances a formlet with a horizontal layout-manager.
    [<JavaScript>]
    let Horizontal (formlet: Formlet<'T>)  =
        (BaseFormlet()).WithLayout Layout.Horizontal formlet
        |> PropagateRenderFrom formlet
        |> OfIFormlet

    /// Enhances a formlet with a vertical layout-manager.
    [<JavaScript>]
    let Vertical (formlet: Formlet<'T>)  =
        (BaseFormlet()).WithLayout Layout.Vertical formlet
        |> PropagateRenderFrom formlet
        |> OfIFormlet

    /// Enhances a formlet with a flowlet layout-manager.
    [<JavaScript>]
    let Flowlet (formlet: Formlet<'T>)  =
        (BaseFormlet()).WithLayout Layout.Flowlet formlet
        |> PropagateRenderFrom formlet
        |> OfIFormlet

    [<JavaScript>]
    let internal ReplaceFirstWithFailure (formlet: Formlet<'T>)  =
        (BaseFormlet()).ReplaceFirstWithFailure formlet
        |> PropagateRenderFrom formlet
        |> OfIFormlet

    /// Creates a formlet that never fires a value.
    [<JavaScript>]
    let Never<'T> () =
        (BaseFormlet()).Never<'T> ()
        |> OfIFormlet

    /// Creates an empty formlet not firing any value.
    [<JavaScript>]
    let Empty<'T> () =
        (BaseFormlet()).Empty<'T> ()
        |> OfIFormlet

    /// Creates an empty formlet.
    [<JavaScript>]
    let internal ReturnEmpty x =
        (BaseFormlet()).ReturnEmpty x
        |> OfIFormlet

    /// Builds a form component from a formlet.
    [<JavaScript>]
    let BuildForm (f: Formlet<'T>)  =
        (BaseFormlet()).BuildForm f

    /// Given a formlet yielding optional values,
    /// creates a formlet that removes its visual
    /// component when the value None is triggered.
    [<JavaScript>]
    let Deletable (formlet: Formlet<option<'T>>)  =
        (BaseFormlet()).Deletable formlet
        |> PropagateRenderFrom formlet
        |> OfIFormlet

    /// Creates an empty failing formlet with
    /// the given list of error messages.
    [<JavaScript>]
    let FailWith<'T> fs =
        (BaseFormlet()).FailWith<'T> fs
        |> OfIFormlet

    /// Formlet builder
    [<JavaScript>]
    let Do = new FormletBuilder()

    /// Maps the successful results of a formlet.
    [<JavaScript>]
    let Map f (formlet: Formlet<'T>)  =
        (BaseFormlet()).Map f formlet
        |> PropagateRenderFrom formlet
        |> OfIFormlet

    /// Maps the body component of a formlet.
    [<JavaScript>]
    let MapBody f (formlet: Formlet<'T>)  =
        (BaseFormlet()).MapBody f formlet
        |> PropagateRenderFrom formlet
        |> OfIFormlet


    /// Maps the results of a formlet.
    [<JavaScript>]
    let MapResult f (formlet: Formlet<'T>)  =
        (BaseFormlet()).MapResult f formlet
        |> PropagateRenderFrom formlet
        |> OfIFormlet

    /// Construct a formlet from a formlet generating function.
    [<JavaScript>]
    let Delay (f: unit -> Formlet<'T>) =
        (BaseFormlet()).Delay (fun () -> f () :> IFormlet<_,_>)
        |> OfIFormlet

    /// Given a formlet and a function, creates a formlet that is updated
    /// every time the formlet value changes by applying the function.
    [<JavaScript>]
    let Bind (fl: Formlet<'T>)  (f: 'T -> Formlet<'U>) : Formlet<'U> =
        (BaseFormlet()).Bind fl (fun x ->
            let y = f x
            y :> IFormlet<_,_>
        )
        |> PropagateRenderFrom fl
        |> OfIFormlet

    /// Given a formlet and a function, creates a formlet that is replaced
    /// the first time the formlet value is triggered.
    [<JavaScript>]
    let Replace (formlet : Formlet<'T>) (f: 'T -> Formlet<'U>) =
        (BaseFormlet()).Replace formlet (fun x -> f x :> IFormlet<_,_>)
        |> PropagateRenderFrom formlet
        |> OfIFormlet

    /// Joins the results of a formlet producing formlet,
    /// into a sequence of formlets, all propagating their
    /// results to the outer resulting formlet.
    [<JavaScript>]
    let Join (formlet : Formlet<Formlet<'T>>)  =
        formlet
        |> Map (fun f -> f :> IFormlet<_,_>) :> IFormlet<_,_>
        |> (BaseFormlet()).Join
        |> PropagateRenderFrom formlet
        |> OfIFormlet

    /// Joins the result of a formlet producing formlet
    /// by only considering the results of the latest
    /// produced formlet.
    [<JavaScript>]
    let Switch (formlet : Formlet<Formlet<'T>>)  =
        formlet
        |> Map (fun f -> f :> IFormlet<_,_>) :> IFormlet<_,_>
        |> (BaseFormlet()).Switch
        |> PropagateRenderFrom formlet
        |> OfIFormlet

    /// Changes the order of produced body stream.
    [<JavaScript>]
    let FlipBody (formlet: Formlet<'T>) =
        (BaseFormlet()).FlipBody formlet
        |> PropagateRenderFrom formlet
        |> OfIFormlet

    /// Converts a formlet producing formlet into
    /// a formlet producing a list of values corresponding
    /// to results of all the generated formlets.
    [<JavaScript>]
    let SelectMany (formlet : Formlet<Formlet<'T>>) =
        formlet
        |> Map (fun f -> f :> IFormlet<_,_>) :> IFormlet<_,_>
        |> (BaseFormlet()).SelectMany
        |> PropagateRenderFrom formlet
        |> OfIFormlet

    /// Lifts the result of a formlet to its output value.
    [<JavaScript>]
    let LiftResult (formlet: Formlet<'T>) =
        (BaseFormlet()).LiftResult formlet
        |> PropagateRenderFrom formlet
        |> OfIFormlet

    /// Given a list of formlets produce a composed formlet
    /// producing a list of values.
    [<JavaScript>]
    let Sequence (fs: seq<Formlet<'T>>) =
        fs
        |> Seq.map (fun x -> x :> IFormlet<_,_>)
        |> (BaseFormlet()).Sequence
        |> OfIFormlet

    /// Enhances a formlet with a layout component.
    [<JavaScript>]
    let WithLayout l (formlet : Formlet<'T>) =
        (BaseFormlet()).WithLayout l formlet
        |> PropagateRenderFrom formlet
        |> OfIFormlet

    /// Enhances a formlet with a function to be invoked
    /// on all notification events.
    [<JavaScript>]
    let WithNotification c (formlet : Formlet<'T>) =
        (BaseFormlet()).WithNotification c formlet
        |> PropagateRenderFrom formlet
        |> OfIFormlet


    /// Constructs a formlet and its associated notification handler.
    [<JavaScript>]
    let WithNotificationChannel (formlet : Formlet<'T>) =
        (BaseFormlet()).WithNotificationChannel formlet
        |> PropagateRenderFrom formlet
        |> OfIFormlet

    /// Applies the current layout component.
    [<JavaScript>]
    let ApplyLayout (formlet : Formlet<'T>) =
        (BaseFormlet()).ApplyLayout formlet
        |> PropagateRenderFrom formlet
        |> OfIFormlet

    /// Maps the HTML elements of the body component of a formlet.
    [<JavaScript>]
    let MapElement (f: Element -> Element) (formlet: Formlet<'T>) : Formlet<'T> =
        formlet
        |> (BaseFormlet()).MapBody (fun b ->
            {b with Element = f b.Element}
        )
        |> PropagateRenderFrom formlet
        |> OfIFormlet


    /// Given an element generator function, creates a formlet
    /// containing the element as the form body and a successful unit value state.
    [<JavaScript>]
    let OfElement (genElem: unit -> Element) : Formlet<unit> =
        MkFormlet <| fun () ->
            let elem = genElem ()
            elem, ignore, (RX.Return(Success ()))


    [<JavaScript>]
    let internal WithLabel label (formlet: Formlet<'T>) : Formlet<'T> =
        formlet
        |> (BaseFormlet()).MapBody (fun body ->
            {body with Label = label}
        )
        |> PropagateRenderFrom formlet
        |> OfIFormlet

    /// Given a callback function for handling results,
    /// and a formlet, produces an IPagelet containing the
    /// generated form component.
    [<JavaScript>]
    let Run (f: 'T -> unit) (formlet: Formlet<'T>) =
        formlet.Run f

    /// Generalized bind, with the ability to group the
    /// customize the layout of the body parts.
    [<JavaScript>]
    let BindWith    (compose: Body -> Body -> Body)
                    (formlet: Formlet<'T>)
                    (f: 'T -> Formlet<'U>) : Formlet<'U> =
        (BaseFormlet()).BindWith compose formlet (fun x -> f x :> IFormlet<_,_>)
        |> PropagateRenderFrom formlet
        |> OfIFormlet

    /// Renders a formlet. Ignores the values produced by
    /// the formlet.
    [<JavaScript>]
    let Render (formlet: Formlet<unit>) =
        formlet.Run ignore
        |> PropagateRenderFrom formlet

    /// Given a sequenec of formlets, returns a formlet whose result
    /// is the last triggered formlet value of any of the
    /// formlets of the sequence.
    [<JavaScript>]
    let Choose (fs : seq<Formlet<'T>>) =
        let count = ref 0
        fs
        |> Seq.map (fun f ->
            f
            |> Map (fun x ->
                incr count
                (x,count.Value)
            )
            |> InitWithFailure
            |> LiftResult
        )
        |> Sequence
        |> Map (fun xs ->
            xs
            |> List.choose (fun x ->
                match x with
                | Result.Success v  -> Some v
                | _                 -> None
            )
            |> List.sortBy (fun (_,ix) -> ix)
            |> List.rev
            |> List.tryPick (fun (x,_) -> Some x)
        )
        |> Validator.Is (fun x -> Option.isSome x) ""
        |> Map (fun x -> x.Value)

