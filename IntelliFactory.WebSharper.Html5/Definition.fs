// $begin{copyright}
//
// This file is part of WebSharper
//
// Copyright (c) 2008-2013 IntelliFactory
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

namespace IntelliFactory.WebSharper.Html5

open IntelliFactory.WebSharper.InterfaceGenerator
open IntelliFactory.WebSharper.Dom
open IntelliFactory.WebSharper.EcmaScript

module Utils =
    let RenamedEnumStrings n f l =
        List.map (fun x -> (f x, sprintf "'%s'" x)) l
        |> List.toSeq
        |> Pattern.EnumInlines n

    let EnumInts n l = 
        List.map (fun (x, i) -> (x, sprintf "%d" i)) l
        |> List.toSeq
        |> Pattern.EnumInlines n

module Canvas =
    open Utils

    let GlobalCompositeOperation =
        let renamer n =
            match n with
            | "source-atop" -> "SourceATop"
            | "source-in" -> "SourceIn"
            | "source-out" -> "SourceOut"
            | "source-over" -> "SourceOver"
            | "destination-atop" -> "DestinationATop"
            | "destination-in" -> "DestinationIn"
            | "destination-out" -> "DestinationOut"
            | "destination-over" -> "DestinationOver"
            | x -> x
        
        RenamedEnumStrings "GlobalCompositeOperation" renamer [
            // A atop B. Display the source image wherever both images are opaque. Display the destination image wherever the destination image is opaque but the source image is transparent. Display transparency elsewhere.
            "source-atop"
            // A in B. Display the source image wherever both the source image and destination image are opaque. Display transparency elsewhere.
            "source-in"
            // A out B. Display the source image wherever the source image is opaque and the destination image is transparent. Display transparency elsewhere.
            "source-out"
            // A over B. Display the source image wherever the source image is opaque. Display the destination image elsewhere.
            "source-over"
            // B atop A. Same as source-atop but using the destination image instead of the source image and vice versa.
            "destination-atop"
            // B in A. Same as source-in but using the destination image instead of the source image and vice versa.
            "destination-in"
            // B out A. Same as source-out but using the destination image instead of the source image and vice versa.
            "destination-out"
            // B over A. Same as source-over but using the destination image instead of the source image and vice versa.
            "destination-over"
            // A plus B. Display the sum of the source image and destination image, with color values approaching 1 as a limit.
            "lighter"
            // A (B is ignored). Display the source image instead of the destination image.
            "copy"
            // A xor B. Exclusive OR of the source image and destination image.
            "xor"
        ]

    let CanvasGradient =
        Class "CanvasGradient"
        |+> Protocol [
            "addColorStop" => (T<float> * T<string>) ^-> T<unit>
        ]
    
    let Repetition = 
        let renamer n = 
            match n with
            | "repeat-x" -> "RepeatX"
            | "repeat-y" -> "RepeatY"
            | "no-repeat" -> "NoRepeat"
            | x -> x
        RenamedEnumStrings "Repetition" renamer [
            "repeat"
            "repeat-x"
            "repeat-y"
            "no-repeat"
        ]

    let CanvasPattern =
        Class "CanvasPattern"
    
    let LineCap = 
        Pattern.EnumStrings "LineCap" [
            "butt"
            "round"
            "square"
        ]
    
    let LineJoin = 
        Pattern.EnumStrings "LineJoin" [
            "round"
            "bevel"
            "miter"
        ]

    let TextAlign = 
        Pattern.EnumStrings "TextAlign" [
            "start"
            "end"
            "left"
            "right"
            "center"
        ]
    
    let TextBaseline = 
        Pattern.EnumStrings "TextBaseLine" [
            "top"
            "hanging"
            "middle"
            "alphabetic"
            "ideographic"
            "bottom"
        ]
    
    let TextMetrics = 
        Class "TextMetrics"
        |+> Protocol ["width" =@ T<float> ]
    
    let CanvasPixelArray = 
        Class "CanvasPixelArray"
        |+> Protocol [
            "length" =? T<int>
            "Get" => T<int> ^-> T<int>
            |> WithInline "$this[$0]"
            "Set" => T<int> * T<int> ^-> T<int>
            |> WithInline "$this[$0] = $1"
        ]

    let ImageData =
        Class "ImageData"
        |+> Protocol [
            "height" =? T<int> 
            "width" =? T<int> 
            "data" =? CanvasPixelArray
        ]
    
    let CanvasRenderingContext2D = 
        Class "CanvasRenderingContext2D"
        |+> Protocol [
            "canvas" =? T<Element> // FIXME
            // push state on state stack
            "save" => T<unit> ^-> T<unit>
            // pop state stack and restore state
            "restore" => T<unit> ^-> T<unit>
            "scale" => (T<float> * T<float>) ^-> T<unit>
            "rotate" => (T<float>) ^-> T<unit>
            "translate" => (T<float> * T<float>) ^-> T<unit>
            "transform" => (T<float> * T<float> * T<float> * T<float> * T<float> * T<float>) ^-> T<unit>
            "setTransform" => (T<float> * T<float> * T<float> * T<float> * T<float> * T<float>) ^-> T<unit>
            "globalAlpha" =@ T<float>
            "globalCompositeOperation" =@ GlobalCompositeOperation
            // (default black)
            "strokeStyle" =@ T<obj>
            // (default black)
            "fillStyle" =@ T<obj>
            "createLinearGradient" => (T<float> * T<float> * T<float> * T<float>) ^-> CanvasGradient
            "createRadialGradient" => (T<float> * T<float> * T<float> * T<float> * T<float> * T<float>) ^-> CanvasGradient
            "createPattern" => (T<Element> * Repetition) ^-> CanvasPattern
            // (default 1)
            "lineWidth" =@ T<float> 
            // "butt", "round", "square" (default "butt")
            "lineCap" =@ LineCap  
            // "round", "bevel", "miter" (default "miter")
            "lineJoin" =@ LineJoin
            // (default 10)            
            "miterLimit" =@ T<float>
            // (default 0)
            "shadowOffsetX" =@ T<float>
            // (default 0)
            "shadowOffsetY" =@ T<float>
            // (default 0)
            "shadowBlur" =@ T<float>
            // (default transparent black)
            "shadowColor" =@ T<string>

            // rects
            "clearRect" => (T<float> * T<float> * T<float> * T<float>) ^-> T<unit>
            "fillRect" => (T<float> * T<float> * T<float> * T<float>) ^-> T<unit>
            "strokeRect" => (T<float> * T<float> * T<float> * T<float>) ^-> T<unit>

            // path API
            "beginPath" => T<unit> ^-> T<unit>
            "closePath" => T<unit> ^-> T<unit>
            "moveTo" => (T<float> * T<float>) ^-> T<unit>
            "lineTo" => (T<float> * T<float>) ^-> T<unit>
            "quadraticCurveTo" => (T<float> * T<float> * T<float> * T<float>) ^-> T<unit>
            "bezierCurveTo" => (T<float> * T<float> * T<float> * T<float> * T<float> * T<float>) ^-> T<unit>
            "arcTo" => (T<float> * T<float> * T<float> * T<float> * T<float>) ^-> T<unit>
            "rect" => (T<float> * T<float> * T<float> * T<float>) ^-> T<unit>
            "arc" => (T<float> * T<float> * T<float> * T<float> * T<float> * T<bool>) ^-> T<unit>
            "fill" => T<unit> ^-> T<unit>
            "stroke" => T<unit> ^-> T<unit>
            "clip" => T<unit> ^-> T<unit>
            "isPointInPath" => (T<float> * T<float>) ^-> T<bool>

            // focus management
            "drawFocusRing" => (T<Element>?el * T<float>?x * T<float>?y * !? T<bool>) ^-> T<bool>

            // text
            // (default 10px sans-serif)
            "font" =@ T<string>
            "textAlign" =@ TextAlign
            "textBaseline" =@ TextBaseline

            "fillText" => T<string> * T<float> * T<float> ^-> T<unit>
            "fillText" => T<string> * T<float> * T<float> * T<float> ^-> T<unit>
            "strokeText" => T<string> * T<float> * T<float> ^-> T<unit>
            "strokeText" => T<string> * T<float> * T<float> * T<float> ^-> T<unit>
            "measureText" => T<string> ^-> TextMetrics

            // drawing images
            "drawImage" => (T<Element> * T<float> * T<float>) ^-> T<unit>
            "drawImage" => (T<Element> * T<float> * T<float> * T<float> * T<float>) ^-> T<unit>
            "drawImage" => (T<Element> * T<float> * T<float> * T<float> * T<float> * T<float> * T<float> * T<float> * T<float>) ^-> T<unit>

            // pixel manipulation
            "createImageData" => (T<float> * T<float>) ^-> ImageData
            "createImageData" => (ImageData) ^-> ImageData
            "getImageData"    => (T<float> * T<float> * T<float> * T<float>) ^-> ImageData

            "putImageData" => (ImageData * T<float> * T<float> ) ^-> T<unit>
            "putImageData" => (ImageData * T<float> * T<float> * T<float> * T<float> * T<float> * T<float>) ^-> T<unit>
        ]

module AudioVideoCommon =
    
    let TimeRanges = 
        Class "TimeRanges"
        |+> Protocol [
            "length" =? T<int>
            "start" => (T<int>) ^-> T<int>
            "end" => T<int> ^-> T<int>
        ]

    let MediaError = 
        Utils.EnumInts "MediaError" [
            "MEDIA_ERR_ABORTED", 1
            "MEDIA_ERR_NETWORK", 2
            "MEDIA_ERR_DECODE", 3
            "MEDIA_ERR_SRC_NOT_SUPPORTED", 4
        ]
    
    let TrackType =
        Pattern.EnumStrings "TrackType" [
            "subtitles"
            "captions"
            "descriptions"
            "chapters"
            "metadata"
        ]
    
    let TimedTrack = Class "TimedTrack"

    let TimedTrackCue =
        Class "TimedTrackCue"
        |+> Protocol [
            "track" =? TimedTrack
            "id" => T<string>

            "startTime" =? T<float>
            "endTime" =? T<float>
            "pauseOnExit" =? T<bool>

            "direction" =? T<string>
            "snapToLines" =? T<bool>
            "linePosition" =? T<int>
            "textPosition" =? T<int>
            "size" =? T<int>
            "alignment" =? T<string>
            
            "voice" =? T<string>
            "getCueAsSource" => (T<unit>) ^-> T<string>
            "getCueAsHTML" => (T<unit>) ^-> T<obj> // FIXME DocumentFragment

            "onenter" => T<unit> ^-> T<unit>
            "onexit" => T<unit> ^-> T<unit>

        ]

    let TimedTrackCueList = 
        Class "TimedTrackCueList"
        |+> Protocol [
            "length" =? T<int>
            "getter" => (T<int>) ^-> TimedTrackCue
            "getCueById" => (T<string>) ^-> TimedTrackCue
        ]

    do  TimedTrack
        |+> Protocol [
            "kind" =? TrackType
            "label" =? T<string> 
            "language" =? T<string> 

            "NONE" =? T<int>
            "LOADING" =? T<int>
            "LOADED" =? T<int>
            "ERROR" =? T<int>

            "readyState" =? T<int>

            "onload" => T<unit> ^-> T<unit>
            "onerror" => T<unit> ^-> T<unit>

            "OFF" =? T<int>
            "HIDDEN" =? T<int>
            "SHOWING" =? T<int>

            "mode" =? T<int>

            "cues" =? TimedTrackCueList
            "activeCues" =? TimedTrackCueList

            "oncuechange" => T<int> ^-> T<int>
        ] |> ignore

    let MutableTimedTrack = 
        Class "MutableTimedTrack"
        |=> Inherits TimedTrack
        |+> Protocol [
            "addCue" => TimedTrackCue ^-> T<unit>
            "removeCue" => TimedTrackCue ^-> T<unit>
        ]

module Elements =
    open Canvas
    open AudioVideoCommon

    let CanvasElement =
        Class "CanvasElement"
        // |=> Inherits Type.Node
        |+> Protocol [
            "width" =@ T<int>
            "height" =@ T<int>
            "toDataURL" => !? T<string>?a * !? T<float>?b ^-> T<string>
            "getContext" => T<string> ^-> Canvas.CanvasRenderingContext2D
        ]

    let HTMLMediaElement =
        Class "HTMLMediaElement"
        // |=> Inherits Type.Node
        |+> Protocol [
            // error state
            "error" =?  MediaError

            // network state
            "src" =@ T<string>
            "currentSrc" =? T<string>

            "NETWORK_EMPTY" =? T<int>
            "NETWORK_IDLE" =? T<int>
            "NETWORK_LOADING" =? T<int>
            "NETWORK_NO_SOURCE" =? T<int>

            "networkState" =? T<int>

            "preload" =@ T<string>
            "buffered" =? TimeRanges

            "load" => T<unit> ^-> T<unit>
            "canPlayType" => T<string> ^-> T<unit>

            // ready state
            "HAVE_NOTHING" =? T<int>
            "HAVE_METADATA" =? T<int>
            "HAVE_CURRENT_DATA" =? T<int>
            "HAVE_FUTURE_DATA" =? T<int>
            "HAVE_ENOUGH_DATA" =? T<int>

            "readyState" =? T<int>
            "seeking" =? T<bool>

            // playback state
            "autoplay" =@ T<bool>
            "loop" =@ T<bool>
            "currentTime" =@ T<float>
            "defaultPlaybackRate" =@ T<float>
            "playbackRate" =@ T<float>

            "startOffsetTime" =? T<Date>
            "played" =? TimeRanges
            "seekable" =? TimeRanges
            "ended" =? T<bool>
            "paused" =? T<bool>
            "duration" =? T<float>
            "initialTime" =? T<float>

            "play" => T<unit> ^-> T<unit>
            "pause" => T<unit> ^-> T<unit>

            // controls
            "controls" =@ T<bool>
            "volume" =@ T<float>
            "muted" =@ T<bool>
            
            // timed tracks
            "tracks" =? Type.ArrayOf TimedTrack
            "addTrack" => (T<string> * TrackType * T<string>) ^-> MutableTimedTrack
        ]

    let HTMLVideoElement = 
        Class "HTMLVideoElement"
        |=> Inherits HTMLMediaElement
        |+> Protocol [
            "width" =@ T<string>
            "height" =@ T<string>
            "videoWidth" =? T<int>
            "videoHeight" =? T<int>
            "poster" =@ T<string>
        ]

    let HTMLAudioElement =
        Class "HTMLAudioElement"
        |=> Inherits HTMLMediaElement
        |+> Protocol [
            Constructor T<unit> |> WithInline "new Audio()"
            Constructor T<string> |> WithInline "new Audio($0)"
        ]

module Geolocation =
    
    let PositionOptions = 
        Pattern.Config "PositionOptions" {
            Required = []
            Optional = 
                ["enableHighAccuracy", T<bool>
                 "timeout", T<int>
                 "maximumAge", T<int>
                ]
        }

    let Coordinates =
        Class "Coordinates"
        |+> Protocol [
            "latitude" =? T<float>
            "longitude" =? T<float>
            "altitude" =? T<float>
            "accuracy" =? T<float>
            "altitudeAccuracy" =? T<float>
            "heading" =? T<float>
            "speed" =? T<float>        
        ]

    let Position = 
        Class "Position"
        |+> Protocol [
            "coords" =? Coordinates
            "timestamp" =? T<Date>
        ]

    let PositionError = 
        Class "PositionError"
        |+> Protocol [
            "UNKNOWN_ERROR" =? T<int>
            "PERMISSION_DENIED" =? T<int>
            "POSITION_UNAVAILABLE" =? T<int>
            "TIMEOUT" =? T<int>
            "code" =? T<int>
            "message" =? T<string>
        ]
         
    let Geolocation =
        let positionCallback = Position ^-> T<unit>
        let errorCallback = PositionError ^-> T<unit>
        
        Class "Geolocation"
        |+> Protocol [
            "getCurrentPosition" => (positionCallback?p * !? errorCallback?e * !? PositionOptions?o) ^-> T<unit>
            "watchPosition" => (positionCallback?p * !? errorCallback?e * !? PositionOptions?o) ^-> T<int>
            "clearWatch" => T<int> ^-> T<unit>
        ]

    let NavigatorGeolocation =
        Class "NavigatorGeolocation" 
        |+> Protocol ["geolocation" =? Geolocation]

module WebStorage =

    let Storage =
        Class "Storage"
        |+> Protocol [
                "length" =? T<int>
                "key" => T<int -> string>
                "getItem" => T<string->string>
                "setItem" => T<string>?key * T<string>?value ^-> T<unit>
                "removeItem" => T<string -> unit>
                "clear" => T<unit->unit>
            ]

    let StorageEvent =
        Class "StorageEvent"
        |+> Protocol [
                "key" =? T<string>
                "newValue" =? T<string>
                "oldValue" =? T<string>
                "storageArea" =? Storage
                "url" =? T<string>
            ]

module AppCache =
    let ApplicationCache =
        Class "ApplicationCache"
        // |=> Implements [T<EventTarget>]
        |+> Protocol [
            // update status
            "UNCACHED" =? T<int>
            "IDLE" =? T<int>
            "CHECKING" =? T<int>
            "DOWNLOADING" =? T<int>
            "UPDATEREADY" =? T<int>
            "OBSOLETE" =? T<int>
            "status" =? T<int>

              // updates
            "update" => T<unit> ^-> T<unit>
            "swapCache" => T<unit> ^-> T<unit>

            // events
            ///  Function onchecking;
            ///  Function onerror;
            ///  Function onnoupdate;
            ///  Function ondownloading;
            ///  Function onprogress;
            ///  Function onupdateready;
            ///  Function oncached;
            ///  Function onobsolete;
        ]

module WebWorkers =

    let WorkerNavigator = Class "WorkerNavigator"
    let MessagePortArray = Class "MessagePortArray"

    let WorkerUtils =
        Class "WorkerUtils"
        |+> [
            "importScripts" => (!+ T<string>) ^-> T<unit>
            "navigator" =? WorkerNavigator
        ]

    let WorkerLocation =
        Class "WorkerLocation"

    let WorkerGlobalScope = 
        let WorkerGlobalScope = Class "WorkerGlobalScope"
        WorkerGlobalScope
        // |=> Implements [T<EventTarget>; WorkerUtils]
        |+> Protocol [
            "self" =? WorkerGlobalScope
            "location" =? WorkerLocation
            "close" => T<unit> ^-> T<unit>
            // attribute Function onerror;
        ]
    
    let SharedWorkerScope =   
        Class "SharedWorkerGlobalScope"
            |=> Inherits WorkerGlobalScope
            |+> Protocol [
                "name" =? T<string>
                "applicationCache" =? AppCache.ApplicationCache
                //           attribute Function onconnect;
            ]

    let DedicatedWorkerGlobalScope =
        Class "DedicatedWorkerGlobalScope"
        |=> Inherits WorkerGlobalScope
        |+> Protocol [
            "postMessage" => (T<obj> * !? MessagePortArray) ^-> T<unit>
            /// attribute Function onmessage;
            
        ]
        
    let AbstractWorker =
        Class "AbstractWorker"
        // |=> Implements [T<EventTarget>]
        |+> Protocol [
            // attribute Function onerror;
            ]

    let Worker =           
        Class "Worker"
        |=> Inherits AbstractWorker
        |+> Protocol [
            "terminate" => T<unit> ^-> T<unit>
            "postMessage" => (T<obj> * !? MessagePortArray) ^-> T<unit>
            // attribute Function onmessage;
        ]

module General = 
    let BarProp =
        let BarProp = Class "BarProp"
        BarProp
        |+> Protocol [
            "visible" =@ T<bool>
        ]
    
    let History =
        let History = Class "History"
        History
        |+> Protocol [
            "length" =? T<int>
            "go" => T<unit> ^-> T<unit>
            "go" => T<int> ^-> T<unit>
            "back" => T<unit> ^-> T<unit>
            "forward" => T<unit> ^-> T<unit>
            "pushState" => T<obj> * T<string> ^-> T<unit>
            "pushState" => T<obj> * T<string> * T<string> ^-> T<unit>
            "replaceState" => T<obj> * T<string> ^-> T<unit>
            "replaceState" => T<obj> * T<string> * T<string> ^-> T<unit>
        ]

    let Location =
        Class "Location" 
        |+> Protocol [
            "href" =@ T<string>
            "assign" => T<string> ^-> T<unit> 
            "replace" => T<string> ^-> T<unit> 
            "reload" => T<unit> ^-> T<unit> 

                // URL decomposition IDL attributes 
            "protocol" =@ T<string>
            "host" =@ T<string>
            "hostname" =@ T<string>
            "port" =@ T<string>
            "pathname" =@ T<string>
            "search" =@ T<string>
            "hash" =@ T<string>

            "resolveURL" => T<string> ^-> T<unit>
        ]

    let UndoManager =
        Class "UndoManager"
        |+> Protocol [
            "length" =? T<int>
            "item" => T<int> ^-> T<obj>
            "position" =? T<int>
            "add" => T<obj> * T<string> ^-> T<unit>
            "remove" => T<int> ^-> T<unit>
            "clearUndo" => T<unit> ^-> T<unit> 
            "clearRedo" => T<unit> ^-> T<unit> 
        ]

    let WindowProxyType = Class "Window"
    let MessagePortType = Class "MessagePort"

    let MessageEvent =
        Class "MessageEvent"
        // |=> Implements [T<Event>]
        |+> Protocol [
            "data" =? T<obj>
            "origin" =? T<string>
            "lastEventId" =? T<string>
            "source" =? WindowProxyType
            "ports" =? Type.ArrayOf(MessagePortType)
            "initMessageEvent" => T<string> * T<bool> * T<bool> * T<obj> * T<string> * T<string> * WindowProxyType * Type.ArrayOf(MessagePortType) ^-> T<unit>
        ]

    let MessagePort =
        MessagePortType
        |+> Protocol [
            "postMessage" => T<obj> * Type.ArrayOf(MessagePortType) ^-> T<unit>
            "start" => T<unit> ^-> T<unit>
            "close" => T<unit> ^-> T<unit>
            "onmessage" =@ MessageEvent ^-> T<unit>
        ]

    let Window = 
        let f = T<Event> ^-> T<unit>
        WindowProxyType
        |+> [ "self" =? WindowProxyType |> WithGetterInline "window" ]  // Because it conflicts with the class name
        |+> Protocol [
            "history" =? History
            "document" =? T<Document>
            "name" =@ T<string>
            "location" =? Location
            "undoManager" =? UndoManager

            "locationbar" =? BarProp
            "menubar" =? BarProp
            "personalbar" =? BarProp
            "scrollbars" =? BarProp
            "statusbar" =? BarProp
            "toolbar" =? BarProp

            "frames" =? WindowProxyType
            "length" =? T<int>
            "top" =? WindowProxyType
            "opener" =? WindowProxyType
            "parent" =? WindowProxyType
            "frameElement" =? T<Element>
            "open" => (T<string> * T<string> * T<string> * T<string>) ^->  WindowProxyType
            "open" => (T<string> * T<string> * T<string>) ^->  WindowProxyType
            "open" => (T<string> * T<string>) ^->  WindowProxyType
            "open" => (T<string>) ^->  WindowProxyType
            "open" => (T<unit>) ^->  WindowProxyType

            "navigator" =? Geolocation.NavigatorGeolocation
            "applicationCache" =? AppCache.ApplicationCache
            "localStorage" =? WebStorage.Storage
            "sessionStorage" =? WebStorage.Storage
            "alert" => T<string> ^-> T<unit>
            "confirm" => T<string> ^-> T<bool>
            "prompt" => T<string> ^-> T<string>
            "prompt" => T<string> * T<string> ^-> T<string>
            "print" => T<unit> ^-> T<unit>
            "showModalDialog" => T<string> * T<obj> ^-> T<bool>
            "showModalDialog" => T<string> ^-> T<bool>

            "postMessage" => T<string> * T<string> * Type.ArrayOf(MessagePort) ^-> T<unit> 
            "postMessage" => T<string> * T<string> ^-> T<unit> 

            "onabort" =@ f
            "onafterprint" =@ f
            "onbeforeprint" =@ f
            "onbeforeunload" =@ f
            "onblur" =@ f
            "oncanplay" =@ f
            "oncanplaythrough" =@ f
            "onchange" =@ f
            "onclick" =@ f
            "oncontextmenu" =@ f

            "oncuechange" =@ f

            "ondblclick" =@ f
            "ondrag" =@ f
            "ondragend" =@ f
            "ondragenter" =@ f
            "ondragleave" =@ f
            "ondragover" =@ f
            "ondragstart" =@ f
            "ondrop" =@ f
            "ondurationchange" =@ f
            "onemptied" =@ f
            "onended" =@ f
            "onerror" =@ f
            "onfocus" =@ f
            "onformchange" =@ f
            "onforminput" =@ f
            "onhashchange" =@ f
            "oninput" =@ f
            "oninvalid" =@ f
            "onkeydown" =@ f
            "onkeypress" =@ f
            "onkeyup" =@ f
            "onload" =@ f
            "onloadeddata" =@ f
            "onloadedmetadata" =@ f
            "onloadstart" =@ f
            "onmessage" =@ f
            "onmousedown" =@ f
            "onmousemove" =@ f
            "onmouseout" =@ f
            "onmouseover" =@ f
            "onmouseup" =@ f
            "onmousewheel" =@ f
            "onoffline" =@ f
            "ononline" =@ f
            "onpause" =@ f
            "onplay" =@ f
            "onplaying" =@ f
            "onpagehide" =@ f
            "onpageshow" =@ f
            "onpopstate" =@ f
            "onprogress" =@ f
            "onratechange" =@ f
            "onreadystatechange" =@ f
            "onredo" =@ f
            "onreset" =@ f
            "onresize" =@ f
            "onscroll" =@ f
            "onseeked" =@ f
            "onseeking" =@ f
            "onselect" =@ f
            "onshow" =@ f
            "onstalled" =@ f
            "onstorage" =@ WebStorage.StorageEvent ^-> T<unit>
            "onsubmit" =@ f
            "onsuspend" =@ f
            "ontimeupdate" =@ f
            "onundo" =@ f
            "onunload" =@ f
            "onvolumechange" =@ f
            "onwaiting" =@ f
        ]

module TypedArrays =


    let ArrayBuffer =
        let ArrayBuffer = Type.New()
        Class "ArrayBuffer"
        |=> ArrayBuffer
        |+> Protocol [
                "byteLength" =? T<int>
                /// Warning: although part of the spec, may not work in IE10 as of 6/6/2013.
                "slice" => T<int> * T<int> ^-> ArrayBuffer
            ]
        |+> [ Constructor T<int> ]

    module DataView =

        let private Getter<'T> name =
            name => T<int>?byteOffset * !? T<bool>?littleEndian ^-> T<'T>

        let private Setter<'T> name =
            name => T<int>?byteOffset * T<'T>?value * !? T<bool>?littleEndian ^-> T<unit>

        let Class =
            Class "DataView"
            |+> [
                    Constructor ArrayBuffer
                    Constructor (ArrayBuffer * T<int>?byteOffset)
                    Constructor (ArrayBuffer * T<int>?byteOffset * T<int>?byteLength)
                ]
            |+> Protocol [
                    "getInt8" => T<int>?byteOffset ^-> T<sbyte>
                    "getUint8" => T<int>?byteOffset ^-> T<byte>
                    Getter<int16> "getInt16"
                    Getter<uint16> "getUint16"
                    Getter<int32> "getInt32"
                    Getter<uint32> "getUint32"
                    Getter<float32> "getFloat32"
                    Getter<double> "getFloat64"
                    "setInt8" => T<int>?byteOffset * T<sbyte>?value ^-> T<unit>
                    "setUint8" => T<int>?byteOffset * T<byte>?value ^-> T<unit>
                    Setter<int16> "setInt16"
                    Setter<uint16> "setUint16"
                    Setter<int32> "setInt32"
                    Setter<uint32> "setUint32"
                    Setter<float32> "setFloat32"
                    Setter<double> "setFloat64"
                ]

    let ArrayBufferView =
        Class "ArrayBufferView"
        |+> Protocol
            [
                "buffer" =? ArrayBuffer
                "byteOffset" =? T<int>
                "byteLength" =? T<int>
            ]

    let private MakeTypedArray typedArray (elementType: Type.Type) =
        let self = Type.New()
        Class typedArray
        |=> self
        |=> Inherits ArrayBufferView
        |+> [
                Constructor T<unit>
                Constructor T<int>
                Constructor self
                Constructor (Type.ArrayOf elementType)
                Constructor (ArrayBuffer?buffer * !? T<int>?byteOffset * !? T<int>?length)
                "BYTES_PER_ELEMENT" =? T<int>
            ]
        |+> Protocol [
                "length" =? T<int>
                "get" =>
                    T<int>?offset ^-> elementType
                    |> WithInline "$this[$offset]"
                "set" =>
                    T<int>?offset * elementType?value ^-> T<unit>
                    |> WithInline "void($this[$offset]=$value)"
                "set" => self?array * !? T<int>?offset ^-> T<unit>
                "set" => (Type.ArrayOf elementType)?array * !? T<int>?offset ^-> T<unit>
                "subarray" => T<int64>?``begin`` * T<int>?``end`` ^-> self
            ]

    let Int8Array = MakeTypedArray "Int8Array" T<sbyte>
    let Uint8Array = MakeTypedArray "Uint8Array" T<byte>
    let Uint8ClampedArray = MakeTypedArray "Uint8ClampedArray" T<byte>
    let Int16Array = MakeTypedArray "Int16Array" T<int16>
    let Uint16Array = MakeTypedArray "Uint16Array" T<uint16>
    let Int32Array = MakeTypedArray "Int32Array" T<int32>
    let Uint32Array = MakeTypedArray "Uint32Array" T<uint32>
    let Float32Array = MakeTypedArray "Float32Array" T<float32>
    let Float64Array = MakeTypedArray "Float64Array" T<double>

module File =

    let BlobPropertyBag =
        Pattern.Config "BlobPropertyBag" {
            Required = []
            Optional =
                [
                    "type", T<string>
                ]
        }

    let Blob =
        let Blob = Type.New()
        Class "Blob"
        |=> Blob
        |+> [
                Constructor T<unit>
                Constructor ((Type.ArrayOf TypedArrays.ArrayBuffer + Type.ArrayOf TypedArrays.ArrayBufferView + Blob + T<string>) * !?BlobPropertyBag)
            ]
        |+> Protocol [
                "size" =? T<int>
                "type" =? T<string>
                "slice" => T<int>?start * T<int>?``end`` * T<string>?contentType ^-> Blob
                "close" => T<unit> ^-> T<unit>
            ]

    let File =
        Class "File"
        |=> Inherits Blob
        |+> Protocol [
                "name" =? T<string>
                "lastModifiedDate" =? T<Date>
            ]

    let ProgressEvent =
        Class "ProgressEvent"
        |=> Inherits T<Event>
        |+> Protocol [
                "lengthComputable" =? T<bool>
                "loaded" =? T<int>
                "total" =? T<int>
            ]

    let FileList =
        let FileList = Type.New()
        Class "FileList"
        |=> FileList
        |+> Protocol [
                "item" => T<int> ^-> File
                "length" =? T<int>
            ]
        |+> [
                "ofElement" => T<Element>?input ^-> FileList
                |> WithInline "$input.files"
                "ofEvent" => ProgressEvent?event ^-> FileList
                |> WithInline "$event.target.files"
            ]

    let FileReaderReadyState =
        Pattern.EnumInlines "FileReaderReadyState" [
            "Empty", "0"
            "Loading", "1"
            "Done", "2"
        ]

    let FileReader =
        let EventListener = (ProgressEvent + T<unit>) ^-> T<unit>
        Generic / fun t ->
        Class "FileReader"
        |=> Inherits T<EventTarget>
        |+> Protocol [
                "abort" => T<unit> ^-> T<unit>
                "readyState" =? FileReaderReadyState
                "result" =? t
                "error" =? T<Error>
                "onloadstart" =@ ProgressEvent ^-> T<unit>
                "onprogress" =@ ProgressEvent ^-> T<unit>
                "onload" =@ ProgressEvent ^-> T<unit>
                "onabort" =@ ProgressEvent ^-> T<unit>
                "onerror" =@ ProgressEvent ^-> T<unit>
                "onloadend" =@ ProgressEvent ^-> T<unit>
        ]

    let TextFileReader =
        Class "TextFileReader"
        |=> Inherits (FileReader T<string>)
        |+> [
                Constructor T<unit>
                |> WithInline "new FileReader()"
            ]
        |+> Protocol [
                "readAsText" => Blob * !?T<string>?encoding ^-> T<unit>
                "readAsDataURL" => Blob ^-> T<unit>
            ]

    let BinaryFileReader =
        Class "BinaryFileReader"
        |=> Inherits (FileReader TypedArrays.ArrayBuffer)
        |+> [
                Constructor T<unit>
                |> WithInline "new FileReader()"
            ]
        |+> Protocol [
                "readAsArrayBuffer" => Blob ^-> T<unit>
            ]

module WebSockets =

    let WebSocketReadyState =
        Pattern.EnumInlines "WebSocketReadyState" [
            "Connecting", "0"
            "Open", "1"
            "Closing", "2"
            "Closed", "3"
        ]

    let WebSocket =
        Class "WebSocket"
        |+> Protocol
            [
                "readyState" =? WebSocketReadyState
                "bufferedAmount" =? T<int>
                "onopen" =@ T<unit->unit>
                "onclose" =@ T<unit->unit>
                "onerror" =@ T<unit->unit>
                "extensions" =? T<string>
                "protocol" =? T<string>
                "close" => T<unit->unit>
                "close" => T<int> ^-> T<unit>
                "close" => T<int> * T<string> ^-> T<unit>
                "onmessage" =@ (General.MessageEvent ^-> T<unit>)
                "binaryType" =@ T<string>
                "send" => T<string->unit>
                "send" => TypedArrays.ArrayBuffer ^-> T<unit>
                (* TODO: describe FILE Api: http://dev.w3.org/2006/webapi/FileAPI *)
                // "send" => Blob ^-> T<unit>
            ]
        |+> [
                Constructor T<string>
                Constructor (T<string> * T<string[]>)
            ]

module Definition =

    let Assembly =
        Assembly [
            Namespace "IntelliFactory.WebSharper.Html5" [
                AudioVideoCommon.MediaError
                AudioVideoCommon.MutableTimedTrack
                AudioVideoCommon.TimeRanges
                AudioVideoCommon.TimedTrack
                AudioVideoCommon.TimedTrackCue
                AudioVideoCommon.TimedTrackCueList
                AudioVideoCommon.TrackType
                AppCache.ApplicationCache
                Canvas.CanvasGradient
                Canvas.CanvasPattern
                Canvas.CanvasPixelArray
                Canvas.CanvasRenderingContext2D
                Canvas.GlobalCompositeOperation
                Canvas.ImageData
                Canvas.LineCap
                Canvas.LineJoin
                Canvas.Repetition
                Canvas.TextAlign
                Canvas.TextBaseline
                Canvas.TextMetrics
                Elements.CanvasElement
                Elements.HTMLAudioElement
                Elements.HTMLVideoElement
                Elements.HTMLMediaElement
                File.BinaryFileReader
                File.Blob
                File.BlobPropertyBag
                File.File
                File.FileList
                Generic - File.FileReader
                File.FileReaderReadyState
                File.ProgressEvent
                File.TextFileReader
                Geolocation.Coordinates
                Geolocation.Geolocation
                Geolocation.NavigatorGeolocation
                Geolocation.Position
                Geolocation.PositionError
                Geolocation.PositionOptions
                General.BarProp
                General.History
                General.Location
                General.MessageEvent
                General.MessagePort
                General.UndoManager
                General.Window
                TypedArrays.DataView.Class
                TypedArrays.ArrayBuffer
                TypedArrays.ArrayBufferView
                TypedArrays.Float32Array
                TypedArrays.Float64Array
                TypedArrays.Int16Array
                TypedArrays.Int32Array
                TypedArrays.Int8Array
                TypedArrays.Uint16Array
                TypedArrays.Uint32Array
                TypedArrays.Uint8Array
                TypedArrays.Uint8ClampedArray
                WebSockets.WebSocket
                WebSockets.WebSocketReadyState
                WebStorage.Storage
                WebStorage.StorageEvent
            ]
        ]

module Main =

    [<EntryPoint>]
    let Start args =
        Compiler.Create().Start(args, Definition.Assembly)
