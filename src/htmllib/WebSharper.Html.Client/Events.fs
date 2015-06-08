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

namespace WebSharper.Html.Client

open System.Runtime.CompilerServices
open WebSharper

/// Provides the event interfaces and the default event support.
module Events =
    open WebSharper
    open WebSharper.JavaScript
    open WebSharper.JQuery

    /// Represents mouse event data.
    type MouseEvent =
        {
            X : int
            Y : int
            Event : Dom.MouseEvent
        }

    /// Represents character code data.
    type CharacterCode =
        {
            CharacterCode : int
            Event : Dom.KeyboardEvent
        }

    /// Represents key code data
    type KeyCode =
        {
            KeyCode : int
            Event : Dom.KeyboardEvent
        }

    /// Interface for event handling implementation
    type IEventSupport =

        // Generic
        abstract member OnEvent<'T when 'T :> Pagelet>         : eventName: string -> ('T -> Dom.Event -> unit) -> 'T -> unit

        // Mouse
        abstract member OnClick<'T when 'T :> Pagelet>         : ('T -> MouseEvent -> unit) -> 'T -> unit
        abstract member OnDoubleClick<'T when 'T :> Pagelet>   : ('T -> MouseEvent -> unit) -> 'T -> unit
        abstract member OnMouseDown<'T when 'T :> Pagelet>     : ('T -> MouseEvent -> unit) -> 'T -> unit
        abstract member OnMouseEnter<'T when 'T :> Pagelet>    : ('T -> MouseEvent -> unit) -> 'T -> unit
        abstract member OnMouseLeave<'T when 'T :> Pagelet>    : ('T -> MouseEvent -> unit) -> 'T -> unit
        abstract member OnMouseMove<'T when 'T :> Pagelet>     : ('T -> MouseEvent -> unit) -> 'T -> unit
        abstract member OnMouseOut<'T when 'T :> Pagelet>      : ('T -> MouseEvent -> unit) -> 'T -> unit
        abstract member OnMouseUp<'T when 'T :> Pagelet>      : ('T -> MouseEvent -> unit) -> 'T -> unit

        // Keys
        abstract member OnKeyDown<'T when 'T :> Pagelet>       : ('T -> KeyCode -> unit) -> 'T ->  unit
        abstract member OnKeyPress<'T when 'T :> Pagelet>      : ('T -> CharacterCode -> unit) -> 'T ->  unit
        abstract member OnKeyUp<'T when 'T :> Pagelet>         : ('T -> KeyCode -> unit) -> 'T ->  unit

        // Other
        abstract member OnBlur<'T when 'T :> Pagelet>          : ('T -> Dom.Event -> unit) -> 'T -> unit
        abstract member OnChange<'T when 'T :> Pagelet>        : ('T -> Dom.Event -> unit) -> 'T -> unit
        abstract member OnFocus<'T when 'T :> Pagelet>         : ('T -> Dom.Event -> unit) -> 'T -> unit
        abstract member OnError<'T when 'T :> Pagelet>         : ('T -> Dom.Event -> unit) -> 'T -> unit
        abstract member OnLoad<'T when 'T :> Pagelet>          : ('T -> Dom.Event -> unit) -> 'T -> unit
        abstract member OnUnLoad<'T when 'T :> Pagelet>        : ('T -> Dom.Event -> unit) -> 'T -> unit
        abstract member OnResize<'T when 'T :> Pagelet>        : ('T -> Dom.Event -> unit) -> 'T -> unit
        abstract member OnScroll<'T when 'T :> Pagelet>        : ('T -> Dom.Event -> unit) -> 'T -> unit
        abstract member OnSelect<'T when 'T :> Pagelet>        : ('T -> Dom.Event -> unit) -> 'T -> unit
        abstract member OnSubmit<'T when 'T :> Pagelet>        : ('T -> Dom.Event -> unit) -> 'T -> unit

    type internal JE = WebSharper.JQuery.Event

    type internal JQueryEventSupport [<JavaScript>] () =

        [<JavaScript>]
        member private this.OnMouse<'T when 'T :> Pagelet> name (f: 'T -> MouseEvent -> unit) (el: 'T)  =
            JQuery.Of(el.Body).Bind(name, fun _ (ev: JE) ->
                f el {X = ev.PageX; Y = ev.PageY; Event = ev.AsDomEvent :?> _}
            ).Ignore

        interface IEventSupport with

            [<JavaScript>]
            member this.OnEvent ev f el =
                JQuery.Of(el.Body).Bind(ev, fun _ ev -> f el ev.AsDomEvent).Ignore

            [<JavaScript>]
            member this.OnBlur f el  =
                JQuery.Of(el.Body).Bind("blur", fun _ ev -> f el ev.AsDomEvent).Ignore

            [<JavaScript>]
            member this.OnChange f el =
                JQuery.Of(el.Body).Bind("change", fun _ ev -> f el ev.AsDomEvent).Ignore

            [<JavaScript>]
            member this.OnClick f el =
                this.OnMouse "click" f el

            [<JavaScript>]
            member this.OnDoubleClick f el =
                this.OnMouse "dblclick" f el

            [<JavaScript>]
            member this.OnMouseDown f el =
                this.OnMouse "mousedown" f el

            [<JavaScript>]
            member this.OnMouseEnter f el =
                this.OnMouse "mouseenter" f el

            [<JavaScript>]
            member this.OnMouseLeave f el =
                this.OnMouse "mouseleave" f el

            [<JavaScript>]
            member this.OnMouseMove f el =
                this.OnMouse "mousemove" f el

            [<JavaScript>]
            member this.OnMouseOut f el =
                this.OnMouse "mouseout" f el

            [<JavaScript>]
            member this.OnMouseUp f el =
                this.OnMouse "mouseup" f el

            [<JavaScript>]
            member this.OnKeyDown f el =
                JQuery.Of(el.Body).Bind("keydown", fun _ (ev: JE) ->
                    f el {KeyCode = ev?keyCode; Event = ev.AsDomEvent :?> _}
                ).Ignore

            [<JavaScript>]
            member this.OnKeyPress f el =
                JQuery.Of(el.Body)
                    .Keypress(fun _ (ev: JE) -> f el ({ CharacterCode = ev.Which; Event = ev.AsDomEvent :?> _ }))
                    .Ignore

            [<JavaScript>]
            member this.OnKeyUp f el  =
                JQuery.Of(el.Body).Bind("keyup", fun _ (ev: JE) ->
                    f el {KeyCode = ev?keyCode; Event = ev.AsDomEvent :?> _}
                ).Ignore

            [<JavaScript>]
            member this.OnFocus f el =
                JQuery.Of(el.Body).Bind("focus", fun _ ev -> f el ev.AsDomEvent).Ignore

            [<JavaScript>]
            member this.OnLoad f el =
                JQuery.Of(el.Body).Bind("load", fun _ ev -> f el ev.AsDomEvent).Ignore

            [<JavaScript>]
            member this.OnUnLoad f el =
                JQuery.Of(el.Body).Bind("unload", fun _ ev -> f el ev.AsDomEvent).Ignore

            [<JavaScript>]
            member this.OnResize f el =
                JQuery.Of(el.Body).Bind("resize", fun _ ev -> f el ev.AsDomEvent).Ignore

            [<JavaScript>]
            member this.OnScroll f el =
                JQuery.Of(el.Body).Bind("scroll", fun _ ev -> f el ev.AsDomEvent).Ignore

            [<JavaScript>]
            member this.OnSelect f el =
                JQuery.Of(el.Body).Bind("select", fun _ ev -> f el ev.AsDomEvent).Ignore

            [<JavaScript>]
            member this.OnSubmit f el =
                JQuery.Of(el.Body).Bind("submit", fun _ ev -> f el ev.AsDomEvent).Ignore

            [<JavaScript>]
            member this.OnError f el =
                JQuery.Of(el.Body).Bind("error", fun _ ev -> f el ev.AsDomEvent).Ignore

/// Provides aliases to the commonly used events.
[<AutoOpen>]
module EventsPervasives =

    [<JavaScript>]
    let Events = Events.JQueryEventSupport() :> Events.IEventSupport

    [<Inline>]
    [<JavaScript>]
    let OnEvent e x = Events.OnEvent e x

    [<Inline>]
    [<JavaScript>]
    let OnBlur x = Events.OnBlur (fun el _ -> x el)

    [<Inline>]
    [<JavaScript>]
    let OnChange x = Events.OnChange (fun el _ -> x el)

    [<Inline>]
    [<JavaScript>]
    let OnClick x = Events.OnClick x

    [<Inline>]
    [<JavaScript>]
    let OnDoubleClick x = Events.OnDoubleClick x

    [<Inline>]
    [<JavaScript>]
    let OnError x = Events.OnError (fun el _ -> x el)

    [<Inline>]
    [<JavaScript>]
    let OnFocus x = Events.OnFocus (fun el _ -> x el)

    [<Inline>]
    [<JavaScript>]
    let OnKeyDown x = Events.OnKeyDown x

    [<Inline>]
    [<JavaScript>]
    let OnKeyPress x = Events.OnKeyPress x

    [<Inline>]
    [<JavaScript>]
    let OnKeyUp x = Events.OnKeyUp x

    [<Inline>]
    [<JavaScript>]
    let OnLoad x = Events.OnLoad (fun el _ -> x el)

    [<Inline>]
    [<JavaScript>]
    let OnMouseDown x = Events.OnMouseDown x

    [<Inline>]
    [<JavaScript>]
    let OnMouseEnter x = Events.OnMouseEnter x

    [<Inline>]
    [<JavaScript>]
    let OnMouseLeave x = Events.OnMouseLeave x

    [<Inline>]
    [<JavaScript>]
    let OnMouseMove x = Events.OnMouseMove x

    [<Inline>]
    [<JavaScript>]
    let OnMouseOut x = Events.OnMouseOut x

    [<Inline>]
    [<JavaScript>]
    let OnMouseUp x = Events.OnMouseUp x

    [<Inline>]
    [<JavaScript>]
    let OnResize x = Events.OnResize (fun el _ -> x el)

    [<Inline>]
    [<JavaScript>]
    let OnScroll x = Events.OnScroll (fun el _ -> x el)

    [<Inline>]
    [<JavaScript>]
    let OnSelect x = Events.OnSelect (fun el _ -> x el)

    [<Inline>]
    [<JavaScript>]
    let OnSubmit x = Events.OnSubmit (fun el _ -> x el)

    [<Inline>]
    [<JavaScript>]
    let OnUnLoad x = Events.OnUnLoad (fun el _ -> x el)


[<Extension>]
type EventsExtensions =

    [<Extension; Inline; JavaScript>]
    static member On(this: Element, name, x) = Events.OnEvent name x this; this

    [<Extension; Inline; JavaScript>]
    static member OnBlur(this: Element, x) = Events.OnBlur x this; this

    [<Extension; Inline; JavaScript>]
    static member OnChange(this: Element, x) = Events.OnChange x this; this

    [<Extension; Inline; JavaScript>]
    static member OnClick(this: Element, x) = Events.OnClick x this; this

    [<Extension; Inline; JavaScript>]
    static member OnDoubleClick(this: Element, x) = Events.OnDoubleClick x this; this

    [<Extension; Inline; JavaScript>]
    static member OnError(this: Element, x) = Events.OnError x this; this

    [<Extension; Inline; JavaScript>]
    static member OnFocus(this: Element, x) = Events.OnFocus x this; this

    [<Extension; Inline; JavaScript>]
    static member OnKeyDown(this: Element, x) = Events.OnKeyDown x this; this

    [<Extension; Inline; JavaScript>]
    static member OnKeyPress(this: Element, x) = Events.OnKeyPress x this; this

    [<Extension; Inline; JavaScript>]
    static member OnKeyUp(this: Element, x) = Events.OnKeyUp x this; this

    [<Extension; Inline; JavaScript>]
    static member OnLoad(this: Element, x) = Events.OnKeyUp x this; this

    [<Extension; Inline; JavaScript>]
    static member OnMouseDown(this: Element, x) = Events.OnMouseDown x this; this

    [<Extension; Inline; JavaScript>]
    static member OnMouseEnter(this: Element, x) = Events.OnMouseEnter x this; this

    [<Extension; Inline; JavaScript>]
    static member OnMouseLeave(this: Element, x) = Events.OnMouseLeave x this; this

    [<Extension; Inline; JavaScript>]
    static member OnMouseMove(this: Element, x) = Events.OnMouseMove x this; this

    [<Extension; Inline; JavaScript>]
    static member OnMouseOut(this: Element, x) = Events.OnMouseOut x this; this

    [<Extension; Inline; JavaScript>]
    static member OnMouseUp(this: Element, x) = Events.OnMouseUp x this; this

    [<Extension; Inline; JavaScript>]
    static member OnResize(this: Element, x) = Events.OnResize x this; this

    [<Extension; Inline; JavaScript>]
    static member OnScroll(this: Element, x) = Events.OnScroll x this; this

    [<Extension; Inline; JavaScript>]
    static member OnSelect(this: Element, x) = Events.OnSelect x this; this

    [<Extension; Inline; JavaScript>]
    static member OnSubmit(this: Element, x) = Events.OnSubmit x this; this

    [<Extension; Inline; JavaScript>]
    static member OnUnLoad(this: Element, x) = Events.OnUnLoad x this; this
