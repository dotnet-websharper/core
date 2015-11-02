// $begin{copyright}
//
// This file is part of WebSharper
//
// Copyright (c) 2008-2015 IntelliFactory
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

module WebSharper.Html5.Tests

open WebSharper
open WebSharper.JavaScript
open WebSharper.JQuery

[<JavaScript>]
type Elt (name) =
    let dom = JS.Document.CreateElement(name)
    let mutable afterInsert = [||]

    member this.Dom = dom

    member this.OnAfterInsert(f : Dom.Element -> unit) =
        afterInsert.JS.Push(f) |> ignore
        this

    member this.InsertInto (parent: Dom.Node) =
        parent.AppendChild(dom) |> ignore
        Array.iter (fun f -> f dom) afterInsert
        afterInsert <- [||]

    interface IControlBody with
        member this.ReplaceInDom old =
            old.ParentNode.ReplaceChild(dom, old) |> ignore
            Array.iter (fun f -> f dom) afterInsert
            afterInsert <- [||]

module Utils =
    [<Inline "setInterval($x, $n)" >]
    let SetInterval (x: obj) (n: int) = X

module SamplesInternals =

    exception NoGeoLocation of Geolocation.PositionError

    [<JavaScript>]
    let GetPosition() : (Async<Geolocation.Position>) =
        Async.FromContinuations(fun (onOk, onErr, _) ->
            JS.Window.Navigator.Geolocation.GetCurrentPosition(
                (fun pos -> onOk pos),
                (fun err -> onErr (NoGeoLocation err))
            )
        )

    [<JavaScript>]
    let Canvas (height: int, width: int) (f: CanvasRenderingContext2D -> unit) =
        Elt("canvas").OnAfterInsert(fun e ->
            e.SetAttribute("height", string height)
            e.SetAttribute("width", string width)
            let context = As<CanvasElement>(e).GetContext "2d"
            f context
        )

    [<JavaScript>]
    let Example1 (ctx: CanvasRenderingContext2D) =
        ctx.FillText("Hello World", 60., 70.)
        ctx.FillText("Hello World", 60., 70., 1.)
        ctx.FillStyle <- "rgb(200, 0, 0)"
        ctx.FillRect(10., 10., 55., 50.)
        ctx.FillStyle <- "rgba(0, 0, 200, 0.5)"
        ctx.FillRect(30., 30., 55., 50.)

    [<JavaScript>]
    let Example2 (ctx: CanvasRenderingContext2D) =
        ctx.FillRect(25., 25., 100., 100.)
        ctx.ClearRect(45., 45., 60., 60.)
        ctx.StrokeRect(50., 50., 50., 50.)

    [<JavaScript>]
    let Example3 (ctx: CanvasRenderingContext2D) =
        ctx.BeginPath()
        ctx.Arc(75., 75., 50., 0., Math.PI * 2., true);
        ctx.MoveTo(110., 75.)
        ctx.Arc(75., 75., 35., 0., Math.PI, false);
        ctx.MoveTo(65., 65.)
        ctx.Arc(60., 65., 5., 0., Math.PI *2., true);
        ctx.MoveTo(95., 65.)
        ctx.Arc(90., 65., 5., 0., Math.PI *2., true);
        ctx.Stroke()

    [<JavaScript>]
    let Example4 (ctx: CanvasRenderingContext2D) =
        // Filled triangle
        ctx.BeginPath()
        ctx.MoveTo(25., 25.)
        ctx.LineTo(105., 25.)
        ctx.LineTo(25., 105.)
        ctx.Fill()

        // Stroked triangle
        ctx.BeginPath()
        ctx.MoveTo(125., 125.)
        ctx.LineTo(125., 45.)
        ctx.LineTo(45., 125.)
        ctx.ClosePath()
        ctx.Stroke()

    [<JavaScript>]
    let Example5 (ctx: CanvasRenderingContext2D) =
        ctx.BeginPath();
        ctx.MoveTo(75., 25.)
        ctx.QuadraticCurveTo(25., 25., 25., 62.5)
        ctx.QuadraticCurveTo(25., 100., 50., 100.)
        ctx.QuadraticCurveTo(50., 120., 30., 125.)
        ctx.QuadraticCurveTo(60., 120., 65., 100.)
        ctx.QuadraticCurveTo(125., 100., 125., 62.5)
        ctx.QuadraticCurveTo(125., 25., 75., 25.)
        ctx.Stroke()

    [<JavaScript>]
    let Example6 (ctx: CanvasRenderingContext2D) =
        let roundedRect(x: float, y: float, width: float, height: float, radius: float)  =
            ctx.BeginPath()
            ctx.MoveTo(x, (y+radius))
            ctx.LineTo(x, (y+height-radius))
            ctx.QuadraticCurveTo(x, (y+height), (x+radius), (y+height))
            ctx.LineTo((x+width-radius), (y+height))
            ctx.QuadraticCurveTo(x+width, y+height, x+width, y+height-radius)
            ctx.LineTo(x+width, y+radius)
            ctx.QuadraticCurveTo(x+width, y, x+width-radius, y)
            ctx.LineTo(x+radius, y)
            ctx.QuadraticCurveTo(x, y, x, y+radius)
            ctx.Stroke()

        roundedRect(12.,12.,150.,150.,15.)
        roundedRect(19.,19.,150.,150.,9.)
        roundedRect(53.,53.,49.,33.,10.)
        roundedRect(53.,119.,49.,16.,6.)
        roundedRect(135.,53.,49.,33.,10.)
        roundedRect(135.,119.,25.,49.,10.)
        ctx.BeginPath()
        ctx.Arc(37., 37., 13., -Math.PI/7., Math.PI/7., true)
        ctx.LineTo(31., 37.)
        ctx.Fill()
        for i in 0..7 do
            ctx.FillRect(51.+(float i)*16., 35., 4., 4.)
        for i in 0..5 do
            ctx.FillRect(115., 51.+(float i)*16., 4., 4.)
        for i in 0..7 do
            ctx.FillRect(51.+(float i)*16., 99., 4., 4.)
        ctx.BeginPath()
        ctx.MoveTo(83., 116.)
        ctx.LineTo(83., 102.)
        ctx.BezierCurveTo(83., 94., 89., 88., 97., 88.)
        ctx.BezierCurveTo(105., 88., 111., 94., 111., 102.)
        ctx.LineTo(111., 116.)
        ctx.LineTo(106.333, 111.333)
        ctx.LineTo(101.666, 116.)
        ctx.LineTo(97., 111.333)
        ctx.LineTo(92.333, 116.)
        ctx.LineTo(87.666, 111.333)
        ctx.LineTo(83., 116.)
        ctx.Fill()
        ctx.FillStyle <- "white"
        ctx.BeginPath()
        ctx.MoveTo(91., 96.)
        ctx.BezierCurveTo(88., 96., 87., 99., 87., 101.)
        ctx.BezierCurveTo(87., 103., 88., 106., 91., 106.)
        ctx.BezierCurveTo(94., 106., 95., 103., 95., 101.)
        ctx.BezierCurveTo(95., 99., 94., 96., 91., 96.)
        ctx.MoveTo(103., 96.)
        ctx.BezierCurveTo(100., 96., 99., 99., 99., 101.)
        ctx.BezierCurveTo(99., 103., 100., 106., 103., 106.)
        ctx.BezierCurveTo(106., 106., 107., 103., 107., 101.)
        ctx.BezierCurveTo(107., 99., 106., 96., 103., 96.)
        ctx.Fill()
        ctx.FillStyle <- "black"
        ctx.BeginPath()
        ctx.Arc(101., 102., 2., 0., Math.PI*2., true)
        ctx.Fill()
        ctx.BeginPath()
        ctx.Arc(89., 102., 2., 0., Math.PI*2., true)
        ctx.Fill()

    [<JavaScript>]
    let Example7 (ctx: CanvasRenderingContext2D) =
        let img = Elt("img")
        img.Dom.AddEventListener("load", (fun () ->
            ctx.DrawImage(img.Dom, 0., 0.)
            ctx.BeginPath()
            ctx.MoveTo(30.,  96.)
            ctx.LineTo(70.,  66.)
            ctx.LineTo(103., 76.)
            ctx.LineTo(170., 15.)
            ctx.Stroke()
        ), false)
        img.Dom.SetAttribute("src", "backdrop.png")

    [<JavaScript>]
    let Example8 (ctx: CanvasRenderingContext2D) =
        let paint () =
            let now = new Date()
            ctx.Save();
            ctx.ClearRect(0., 0., 150., 150.)
            ctx.Translate(75., 75.)
            ctx.Scale(0.4, 0.4)
            ctx.Rotate(-Math.PI/2.)
            ctx.StrokeStyle <- "black"
            ctx.FillStyle <- "white"
            ctx.LineWidth <- 8.
            ctx.LineCap <- LineCap.Round

            ctx.Save()
            // Hours marks
            for i in 1..12 do
                ctx.BeginPath()
                ctx.Rotate(Math.PI/6.)
                ctx.MoveTo(100., 0.)
                ctx.LineTo(120., 0.)
                ctx.Stroke()
            ctx.Restore()
            // Minute marks
            ctx.Save()
            ctx.LineWidth <- 5.
            for i in 0..59 do
                if (i % 5) <> 0 then
                    ctx.BeginPath()
                    ctx.MoveTo(117., 0.)
                    ctx.LineTo(120., 0.)
                    ctx.Stroke()
                ctx.Rotate (Math.PI / 30.)
            ctx.Restore()

            let sec = now.GetSeconds() |> float
            let min = now.GetMinutes() |> float
            let hr  =
                now.GetHours()
                |> fun hr -> if hr >= 12 then hr-12 else hr
                |> float

            ctx.FillStyle <- "black"

            // write Hours
            ctx.Save()
            ctx.Rotate( hr*(Math.PI/6.) + (Math.PI/360.)*min + (Math.PI/21600.)*sec )
            ctx.LineWidth <- 14.
            ctx.BeginPath()
            ctx.MoveTo(-20., 0.)
            ctx.LineTo(80., 0.)
            ctx.Stroke()
            ctx.Restore()

            // write Minutes
            ctx.Save()
            ctx.Rotate( (Math.PI/30.)*min + (Math.PI/1800.)*sec )
            ctx.LineWidth <- 10.
            ctx.BeginPath()
            ctx.MoveTo(-28., 0.)
            ctx.LineTo(112., 0.)
            ctx.Stroke()
            ctx.Restore()

            // Write seconds
            ctx.Save()
            ctx.Rotate(sec * Math.PI/30.)
            ctx.StrokeStyle <- "#D40000"
            ctx.FillStyle <- "#D40000"
            ctx.LineWidth <- 6.
            ctx.BeginPath()
            ctx.MoveTo(-30., 0.)
            ctx.LineTo(83., 0.)
            ctx.Stroke()
            ctx.BeginPath()
            ctx.Arc(0., 0., 10., 0., (Math.PI*2.), true)
            ctx.Fill()
            ctx.BeginPath()
            ctx.Arc(95., 0., 10., 0., (Math.PI*2.), true)
            ctx.Stroke()
            ctx.FillStyle <- "#555"
            ctx.Arc(0., 0., 3., 0., (Math.PI*2.), true)
            ctx.Fill()
            ctx.Restore()

            ctx.BeginPath()
            ctx.LineWidth <- 14.
            ctx.StrokeStyle <- "#325FA2"
            ctx.Arc(0., 0., 142., 0., (Math.PI*2.), true)
            ctx.Stroke()
            ctx.Restore()
        Utils.SetInterval paint 1000

open WebSharper.Testing

type TestBuilder with

    [<JavaScript>]
    [<CustomOperation("fixture", MaintainsVariableSpace = true)>]
    member this.Fixture<'A>
        (
            r: Runner<'A>,
            [<ProjectionParameter>] el: 'A -> Elt
        ) : Runner<'A> =
        fun isTrueer ->
            let v = r isTrueer
            Choice2Of2 (async {
                let! args =
                    match v with
                    | Choice1Of2 args -> async.Return args
                    | Choice2Of2 args -> args
                let el = el args
                do! Async.FromContinuations (fun (ok, _, _) ->
                    let fixture = JQuery.Of("#qunit-fixture").Empty().Get(0)
                    el.OnAfterInsert(fun _ -> ok())
                        .InsertInto(fixture)
                )
                return args
            })

[<JavaScript>]
let Tests =
    TestCategory "HTML5" {

        Test "Geolocation" {
            let! position = SamplesInternals.GetPosition()
            let coords = position.Coords
            isFalse (JS.IsNaN coords.Latitude)
            isFalse (JS.IsNaN coords.Longitude)
        }

        Test "LocalStorage" {
            let storage = JS.Window.LocalStorage
            let key = "intReference"
            let intReference = storage.GetItem(key)
            let v =
                if intReference = null || intReference = JS.Undefined then
                    0
                else int intReference
            let newV = (v + 1).ToString()
            storage.SetItem(key, newV)
            let storedNewV = storage.GetItem(key)
            equal newV storedNewV
        }

        Test "Canvas 1" {
            fixture (SamplesInternals.Canvas (100, 200) SamplesInternals.Example1)
            expect 0
        }

        Test "Canvas 2" {
            fixture (SamplesInternals.Canvas (150, 200) SamplesInternals.Example2)
            expect 0
        }

        Test "Canvas 3" {
            fixture (SamplesInternals.Canvas (150, 200) SamplesInternals.Example3)
            expect 0
        }

        Test "Canvas 4" {
            fixture (SamplesInternals.Canvas (150, 200) SamplesInternals.Example4)
            expect 0
        }

        Test "Canvas 5" {
            fixture (SamplesInternals.Canvas (150, 200) SamplesInternals.Example5)
            expect 0
        }

        Test "Canvas 6" {
            fixture (SamplesInternals.Canvas (150, 150) SamplesInternals.Example6)
            expect 0
        }

        Test "Canvas 7" {
            fixture (SamplesInternals.Canvas (180, 130) SamplesInternals.Example7)
            expect 0
        }

        Test "Canvas 8" {
            fixture (SamplesInternals.Canvas (200, 200) SamplesInternals.Example8)
            expect 0
        }

    }
