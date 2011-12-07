// $begin{copyright}
// 
// This file is part of WebSharper
// 
// Copyright (c) 2008-2011 IntelliFactory
// 
// GNU Affero General Public License Usage
// WebSharper is free software: you can redistribute it and/or modify it under
// the terms of the GNU Affero General Public License, version 3, as published
// by the Free Software Foundation.
//
// WebSharper is distributed in the hope that it will be useful, but WITHOUT
// ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
// FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License
// for more details at <http://www.gnu.org/licenses/>.
//
// If you are unsure which license is appropriate for your use, please contact
// IntelliFactory at http://intellifactory.com/contact.
//
// $end{copyright}

namespace IntelliFactory.WebSharper.Html

/// Provides the event interfaces and the default event support.
module Events =
    open IntelliFactory.WebSharper
    open IntelliFactory.WebSharper.Html
    open IntelliFactory.WebSharper.JQuery

    /// Represents mouse event data.
    type MouseEvent =
        {
            X : int
            Y : int
        }

    /// Represents character code data.
    type CharacterCode =
        {
            CharacterCode : int
        }

    /// Represents key code data
    type KeyCode =
        {
            KeyCode : int
        }

    /// Interface for event handling implementation
    type IEventSupport =

        // Mouse
        abstract member OnClick<'T when 'T :> IPagelet>         : ('T -> MouseEvent -> unit) -> 'T -> unit
        abstract member OnDoubleClick<'T when 'T :> IPagelet>   : ('T -> MouseEvent -> unit) -> 'T -> unit
        abstract member OnMouseDown<'T when 'T :> IPagelet>     : ('T -> MouseEvent -> unit) -> 'T -> unit
        abstract member OnMouseEnter<'T when 'T :> IPagelet>    : ('T -> MouseEvent -> unit) -> 'T -> unit
        abstract member OnMouseLeave<'T when 'T :> IPagelet>    : ('T -> MouseEvent -> unit) -> 'T -> unit
        abstract member OnMouseMove<'T when 'T :> IPagelet>     : ('T -> MouseEvent -> unit) -> 'T -> unit
        abstract member OnMouseOut<'T when 'T :> IPagelet>      : ('T -> MouseEvent -> unit) -> 'T -> unit
        abstract member OnMouseUp<'T when 'T :> IPagelet>      : ('T -> MouseEvent -> unit) -> 'T -> unit

        // Keys
        abstract member OnKeyDown<'T when 'T :> IPagelet>       : ('T -> KeyCode -> unit) -> 'T ->  unit
        abstract member OnKeyPress<'T when 'T :> IPagelet>      : ('T -> CharacterCode -> unit) -> 'T ->  unit
        abstract member OnKeyUp<'T when 'T :> IPagelet>         : ('T -> KeyCode -> unit) -> 'T ->  unit

        // Other
        abstract member OnBlur<'T when 'T :> IPagelet>          : ('T -> unit) -> 'T -> unit
        abstract member OnChange<'T when 'T :> IPagelet>        : ('T -> unit) -> 'T -> unit
        abstract member OnFocus<'T when 'T :> IPagelet>         : ('T -> unit) -> 'T -> unit
        abstract member OnError<'T when 'T :> IPagelet>         : ('T -> unit) -> 'T -> unit
        abstract member OnLoad<'T when 'T :> IPagelet>          : ('T -> unit) -> 'T -> unit
        abstract member OnUnLoad<'T when 'T :> IPagelet>        : ('T -> unit) -> 'T -> unit
        abstract member OnResize<'T when 'T :> IPagelet>        : ('T -> unit) -> 'T -> unit
        abstract member OnScroll<'T when 'T :> IPagelet>        : ('T -> unit) -> 'T -> unit
        abstract member OnSelect<'T when 'T :> IPagelet>        : ('T -> unit) -> 'T -> unit
        abstract member OnSubmit<'T when 'T :> IPagelet>        : ('T -> unit) -> 'T -> unit

    type internal JE = IntelliFactory.WebSharper.JQuery.Event

    type internal JQueryEventSupport [<JavaScript>] () =

        [<JavaScript>]
        member private this.OnMouse<'T when 'T :> IPagelet> name (f: 'T -> MouseEvent -> unit) (el: 'T)  =
            let h =
                new Func<_,_,_>(fun (_ : Dom.Element) (ev: JE)->
                    f el {X = ev.PageX; Y = ev.PageY}
                )
            JQuery.Of(el.Body).Bind(name,h).Ignore

        interface IEventSupport with

            [<JavaScript>]
            member this.OnBlur f el  =
                JQuery.Of(el.Body).Bind("blur", new Func<_,_,_>(fun _ ev -> f el )   ).Ignore

            [<JavaScript>]
            member this.OnChange f el =
                JQuery.Of(el.Body).Bind("change", new Func<_,_,_>(fun _ ev -> f el )   ).Ignore

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
                let h =
                    new Func<_,_,_>(fun (_ : Dom.Element) (ev: JE)->
                        f el {KeyCode = ev?keyCode}
                    )
                JQuery.Of(el.Body).Bind("keydown",h).Ignore

            [<JavaScript>]
            member this.OnKeyPress f el =
                JQuery.Of(el.Body)
                    .Keypress(fun _ arg -> f el ({ CharacterCode = arg.Which }))
                    .Ignore

            [<JavaScript>]
            member this.OnKeyUp f el  =
                let h =
                    new Func<_,_,_>(fun (_ : Dom.Element) (ev: JE)->
                        f el {KeyCode = ev?keyCode}
                    )
                JQuery.Of(el.Body).Bind("keyup",h).Ignore

            [<JavaScript>]
            member this.OnFocus f el =
                JQuery.Of(el.Body).Bind("focus", new Func<_,_,_>(fun _ ev -> f el )).Ignore

            [<JavaScript>]
            member this.OnLoad f el =
                JQuery.Of(el.Body).Bind("load", new Func<_,_,_>(fun _ ev -> f el )).Ignore

            [<JavaScript>]
            member this.OnUnLoad f el =
                JQuery.Of(el.Body).Bind("unload", new Func<_,_,_>(fun _ ev -> f el )   ).Ignore

            [<JavaScript>]
            member this.OnResize f el =
                JQuery.Of(el.Body).Bind("resize", new Func<_,_,_>(fun _ ev -> f el )   ).Ignore

            [<JavaScript>]
            member this.OnScroll f el =
                JQuery.Of(el.Body).Bind("scroll", new Func<_,_,_>(fun _ ev -> f el )   ).Ignore

            [<JavaScript>]
            member this.OnSelect f el =
                JQuery.Of(el.Body).Bind("select", new Func<_,_,_>(fun _ ev -> f el )   ).Ignore

            [<JavaScript>]
            member this.OnSubmit f el =
                JQuery.Of(el.Body).Bind("submit", new Func<_,_,_>(fun _ ev -> f el )   ).Ignore

            [<JavaScript>]
            member this.OnError f el =
                JQuery.Of(el.Body).Bind("error", new Func<_,_,_>(fun _ ev -> f el )   ).Ignore

/// Provides aliases to the commonly used events.
[<AutoOpen>]
module EventsPervasives =
    open IntelliFactory.WebSharper

    [<JavaScript>]
    let Events = Events.JQueryEventSupport() :> Events.IEventSupport

    [<Inline>]
    [<JavaScript>]
    let OnBlur x = Events.OnBlur x

    [<Inline>]
    [<JavaScript>]
    let OnChange x = Events.OnChange x

    [<Inline>]
    [<JavaScript>]
    let OnClick x = Events.OnClick x

    [<Inline>]
    [<JavaScript>]
    let OnDoubleClick x = Events.OnDoubleClick x

    [<Inline>]
    [<JavaScript>]
    let OnError x = Events.OnError x

    [<Inline>]
    [<JavaScript>]
    let OnFocus x = Events.OnFocus x

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
    let OnLoad x = Events.OnLoad x

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
    let OnResize x = Events.OnResize x

    [<Inline>]
    [<JavaScript>]
    let OnScroll x = Events.OnScroll x

    [<Inline>]
    [<JavaScript>]
    let OnSelect x = Events.OnSelect x

    [<Inline>]
    [<JavaScript>]
    let OnSubmit x = Events.OnSubmit x

    [<Inline>]
    [<JavaScript>]
    let OnUnLoad x = Events.OnUnLoad x
