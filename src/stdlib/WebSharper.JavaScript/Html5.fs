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

namespace WebSharper.JavaScript.Html5

open WebSharper.InterfaceGenerator
open WebSharper.JavaScript
open WebSharper.JavaScript.Ecma.Definition
open WebSharper.JavaScript.Dom.Definition

module Utils =
    let RenamedEnumStrings n f l =
        List.map (fun x -> (f x, sprintf "'%s'" x)) l
        |> List.toSeq
        |> Pattern.EnumInlines n

    let EnumInts n l = 
        List.map (fun (x, i) -> (x, sprintf "%d" i)) l
        |> List.toSeq
        |> Pattern.EnumInlines n

module Decl =
    let HTMLCanvasElementClass = Class "HTMLCanvasElement"
    let HTMLImageElementClass = Class "HTMLImageElement"
    let HTMLVideoElementClass = Class "HTMLVideoElement"
    let SVGImageElementClass = Class "SVGImageElement"

module TypedArrays =


    let ArrayBuffer =
        Class "ArrayBuffer"
        |+> Instance [
                "byteLength" =? T<int>
                "slice" => T<int> * T<int> ^-> TSelf
                |> WithComment "Warning: although part of the spec, may not work in IE10 as of 6/6/2013."
            ]
        |+> Static [ Constructor T<int> ]

    module DataView =

        let private Getter<'T> name =
            name => T<int>?byteOffset * !? T<bool>?littleEndian ^-> T<'T>

        let private Setter<'T> name =
            name => T<int>?byteOffset * T<'T>?value * !? T<bool>?littleEndian ^-> T<unit>

        let Class =
            Class "DataView"
            |+> Static [
                    Constructor ArrayBuffer
                    Constructor (ArrayBuffer * T<int>?byteOffset)
                    Constructor (ArrayBuffer * T<int>?byteOffset * T<int>?byteLength)
                ]
            |+> Instance [
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
        |+> Instance
            [
                "buffer" =? ArrayBuffer
                "byteOffset" =? T<int>
                "byteLength" =? T<int>
            ]

    let private MakeTypedArray typedArray (elementType: Type.Type) =
        Class typedArray
        |=> Inherits ArrayBufferView
        |+> Static [
                Constructor T<unit>
                Constructor T<int>
                Constructor TSelf
                Constructor (Type.ArrayOf elementType)
                Constructor (ArrayBuffer?buffer * !? T<int>?byteOffset * !? T<int>?length)
                "BYTES_PER_ELEMENT" =? T<int>
            ]
        |+> Instance [
                "length" =? T<int>
                "get" =>
                    T<int>?offset ^-> elementType
                    |> WithInline "$this[$offset]"
                "set" =>
                    T<int>?offset * elementType?value ^-> T<unit>
                    |> WithInline "void($this[$offset]=$value)"
                "set" => TSelf?array * !? T<int>?offset ^-> T<unit>
                "set" => (Type.ArrayOf elementType)?array * !? T<int>?offset ^-> T<unit>
                "subarray" => T<int64>?``begin`` * T<int>?``end`` ^-> TSelf
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

module Media =
    let MediaStreamTrackContentHint =
        Pattern.EnumInlines "MediaStreamTrackContentHint"
            [
                "Empty", "''"
                "Speech", "'speech'"
                "SpeechRecognition", "'speech-recognition'"
                "Music", "'music'"
                "Motion", "'motion'"
                "Detail", "'detail'"
                "Text", "'text'"
            ]

    let MediaStreamTrackClass =
        Class "MediaStreamTrackClass"

    let ULongRange =
        Pattern.Config "ULongRange"
            {
                Required = []
                Optional = [
                    "max", T<uint64>
                    "min", T<uint64>
                ]
            }

    let DoubleRange =
        Pattern.Config "DoubleRange"
            {
                Required = []
                Optional = [
                    "max", T<float>
                    "min", T<float>
                ]
            }

    let MediaStreamTrackState =
        Pattern.EnumStrings "MediaStreamTrackState"
            [
                "live"
                "ended"
            ]

    let MediaTrackCapabilities =
        Pattern.Config "MediaTrackCapabilities"
            {
                Required = []
                Optional = [
                    "facingMode", !| T<string>
                    "resizeMode", !| T<string>

                    "echoCancellation", !|T<bool>
                    "autoGainControl", !|T<bool>
                    "noiseSuppression", !|T<bool>
                    
                    "deviceId", T<string>
                    "groupId", T<string>

                    "width", ULongRange.Type
                    "height", ULongRange.Type
                    "channelCount", ULongRange.Type
                    "sampleRate", ULongRange.Type
                    "sampleSize", ULongRange.Type

                    "latency", DoubleRange.Type
                    "aspectRatio", DoubleRange.Type
                    "frameRate", DoubleRange.Type
                ]
            }

    let ConstrainDOMStringParameters =
        Pattern.Config "ConstrainDOMStringParameters"
            {
                Required = []
                Optional = [
                    "exact", T<string> + !|T<string>
                    "ideal", T<string> + !|T<string>
                ]
            }

    let ConstrainDOMString = T<string> + !|T<string> + ConstrainDOMStringParameters

    let ConstrainBooleanParameters = 
        Pattern.Config "ConstrainBooleanParameters" {
            Required = []
            Optional = [
                "exact", T<bool>
                "ideal", T<bool>
            ]
        }       

    let ConstrainBoolean = T<bool> + ConstrainBooleanParameters

    let ConstrainULongRange =
        Pattern.Config "ConstrainULongRange"
            {
                Required = []
                Optional = [
                    "exact", T<uint64>
                    "ideal", T<uint64>
                ]
            }
            |=> Inherits ULongRange

    let ConstrainDoubleRange =
        Pattern.Config "ConstrainDoubleRange"
            {
                Required = []
                Optional = [
                    "exact", T<float>
                    "ideal", T<float>
                ]
            }
            |=> Inherits DoubleRange

    let ConstrainDouble = T<double> + ConstrainDoubleRange
    let ConstrainULong = T<double> + ConstrainULongRange

    let MediaTrackConstraintSet =
        Pattern.Config "MediaTrackConstraintSet"
            {
                Required = []
                Optional = [
                    "facingMode", ConstrainDOMString
                    "resizeMode", ConstrainDOMString

                    "echoCancellation", ConstrainBoolean
                    "autoGainControl", ConstrainBoolean
                    "noiseSuppression", ConstrainBoolean
                    
                    "deviceId", ConstrainDOMString
                    "groupId", ConstrainDOMString

                    "width", ConstrainULong
                    "height", ConstrainULong
                    "channelCount", ConstrainULong
                    "sampleRate", ConstrainULong
                    "sampleSize", ConstrainULong

                    "latency", ConstrainDouble
                    "aspectRatio", ConstrainDouble
                    "frameRate", ConstrainDouble
                ]
            }

    let MediaTrackConstraints =
        Pattern.Config "MediaTrackConstraints"
            {
                Required = []
                Optional = [
                    "advanced", !| MediaTrackConstraintSet
                ]
            }
            |=> Inherits MediaTrackConstraintSet

    let MediaTrackSettings =
        Pattern.Config "MediaTrackSettings"
            {
                Required = []
                Optional = [
                    "facingMode", T<string>
                    "resizeMode", T<string>

                    "echoCancellation", T<bool>
                    "autoGainControl", T<bool>
                    "noiseSuppression", T<bool>
                        
                    "deviceId", T<string>
                    "groupId", T<string>

                    "width", T<uint64>
                    "height", T<uint64>
                    "channelCount", T<uint64>
                    "sampleRate", T<uint64>
                    "sampleSize", T<uint64>

                    "latency", T<float>
                    "aspectRatio", T<float>
                    "frameRate", T<float>
                ]
            }

    let MediaTrackSupportedConstraints =
        Class "MediaTrackSupportedConstraints"
        |+> Static [
            "facingMode" =? T<bool>
            "resizeMode" =? T<bool>
            "echoCancellation" =? T<bool>
            "autoGainControl" =? T<bool>
            "noiseSuppression" =? T<bool>
            "deviceId" =? T<bool>
            "groupId" =? T<bool>
            "width" =? T<bool>
            "height" =? T<bool>
            "channelCount" =? T<bool>
            "sampleRate" =? T<bool>
            "sampleSize" =? T<bool>
            "latency" =? T<bool>
            "aspectRatio" =? T<bool>
            "frameRate" =? T<bool>
        ]
            
    let MediaStreamTrackEvent =
        Class "MediaStreamTrackEvent"
        |=> Inherits Dom.Interfaces.Event
        |+> Static [
            Constructor T<unit>
        ]
        |+> Instance [
            "track" =? MediaStreamTrackClass
        ]

    let MediaStreamTrack =
        MediaStreamTrackClass
        |=> Inherits Dom.Interfaces.EventTarget
        |+> Instance [
            "contentHint" =? MediaStreamTrackContentHint.Type
                |> WithWarning "This is not part of the standard API"
            "kind" =? T<string>
            "id" =? T<string>
            "label" =? T<string>
            "enabled" =@ T<bool>
            "muted" => T<bool>
            "readyState" =? MediaStreamTrackState

            "clone" => T<unit> ^-> TSelf
            "stop" => T<unit> ^-> T<unit>
            "getCapabilities" => T<unit> ^-> MediaTrackCapabilities
            "getConstraints" => T<unit> ^-> MediaTrackConstraints
            "getSettings" => T<unit> ^-> MediaTrackSettings
            "applyConstraints" => !?MediaTrackConstraints?constraints ^-> EcmaPromise.[T<unit>]

            "onended" =@ MediaStreamTrackEvent ^-> T<unit>
            "onmute" =@ MediaStreamTrackEvent ^-> T<unit>
            "onunmute" =@ MediaStreamTrackEvent ^-> T<unit>
        ]

    let MediaStream =
        Class "MediaStream"
        |=> Inherits Dom.Interfaces.EventTarget
        |+> Static [
            Constructor T<unit>
            Constructor TSelf?stream
            Constructor !|MediaStreamTrack
        ]
        |+> Instance [
            "active" =? T<bool>
            "id" =? T<string>

            "addTrack" => MediaStreamTrack ^-> T<unit>
            "clone" => T<unit> ^-> TSelf
            "getAudioTracks" => T<unit> ^-> !| MediaStreamTrack
            "getVideoTracks" => T<unit> ^-> !| MediaStreamTrack
            "getTracks" => T<unit> ^-> !| MediaStreamTrack
            "getTrackById" => T<string> ^-> !? MediaStreamTrack
            "removeTrack" => MediaStreamTrack ^-> T<unit>

            "onaddtrack" =@ MediaStreamTrackEvent ^-> T<unit>
            "onremovetrack" =@ MediaStreamTrackEvent ^-> T<unit>
            "onactive" =@ MediaStreamTrackEvent ^-> T<unit>
            "oninactive" =@ MediaStreamTrackEvent ^-> T<unit>
        ]

    let MediaDeviceKind =
        Pattern.EnumStrings "MediaDeviceKind"
            [
                "audioinput"
                "audiooutput"
                "videoinput"
            ]


    let MediaDeviceInfo =
        Interface "MediaDeviceInfo"
        |+> [
            "deviceId" =? T<string>
            "kind" =? MediaDeviceKind
            "label" =? T<string>
            "groupId" =? T<string>
        ]

    let InputDeviceInfo =
        Interface "InputDeviceInfo"
        |+> [
            "getCapabilities" => T<unit> ^-> MediaTrackCapabilities
        ]

    let MediaDevices =
        Class "MediaDevices"
        |=> Inherits Dom.Interfaces.EventTarget
        |+> Instance [
            "enumerateDevices" => T<unit> ^-> EcmaPromise.[!|MediaDeviceInfo]
            "ondevicechange" => Dom.Interfaces.Event ^-> T<unit>
        ]

    let CanvasCaptureMediaStreamTrack =
        Class "CanvasCaptureMediaStreamTrack"
        |=> Inherits MediaStreamTrackClass
        |+> Instance [
            "canvas" =? Decl.HTMLCanvasElementClass
            "requestFrame" => T<unit> ^-> T<unit>
        ]

module Geometry =
    let DOMPointInit =
        Pattern.Config "DOMPointInit"
            {
                Required = []
                Optional = [
                    "x", T<float>
                    "y", T<float>
                    "z", T<float>
                    "w", T<float>
                ]
            }

    let DOMMatrix2DInit =
        Pattern.Config "DOMMatrix2DInit"
            {
                Required = []
                Optional = [
                    "a" , T<float>
                    "b" , T<float>
                    "c" , T<float>
                    "d" , T<float>
                    "e" , T<float>
                    "f" , T<float>
                    "m11" , T<float>
                    "m12" , T<float>
                    "m21" , T<float>
                    "m22" , T<float>
                ]
            }

    let DOMMatrixInit =
        Pattern.Config "DOMMatrixInit"
            {
                Required = []
                Optional = [
                    "m13" , T<float>
                    "m14" , T<float>
                    "m23" , T<float>
                    "m24" , T<float>
                    "m31" , T<float>
                    "m32" , T<float>
                    "m33" , T<float>
                    "m34" , T<float>
                    "m41" , T<float>
                    "m42" , T<float>
                    "m43" , T<float>
                    "m44" , T<float>
                    "is2D" , T<bool>
                ]
            }
        |=> Inherits DOMMatrix2DInit

    let DOMPointReadOnly =
        Class "DOMPointReadOnly"
        |+> Static [
            Constructor (!?T<float>?x * !?T<float>?y * !?T<float>?z * !?T<float>?w)
            "fromPoint" => !?DOMPointInit ^-> TSelf
        ]
        |+> Instance [
            "x" =? T<float>
            "y" =? T<float>
            "z" =? T<float>
            "w" =? T<float>

            "matrixTransform" => !?DOMMatrixInit?matrix ^-> TSelf
        ]

    let DOMPoint =
        Class "DOMPoint"
        |=> Inherits DOMPointReadOnly
    
    let DOMMatrixReadOnly =
        Class "DOMMatrixReadOnly"
        |+> Static [
            Constructor T<unit>
            Constructor T<string>
            Constructor !|T<float>

            "fromMatrix" => DOMMatrixInit?other ^-> TSelf
            "fromFloat32Array" => TypedArrays.Float32Array?other ^-> TSelf
            "fromFloat64Array" => TypedArrays.Float64Array?other ^-> TSelf
        ]
        |+> Instance [
            "a" =? T<float>
            "b" =? T<float>
            "c" =? T<float>
            "d" =? T<float>
            "e" =? T<float>
            "f" =? T<float>

            "m11" =? T<float>
            "m12" =? T<float>
            "m13" =? T<float>
            "m14" =? T<float>
            "m21" =? T<float>
            "m22" =? T<float>
            "m23" =? T<float>
            "m24" =? T<float>
            "m31" =? T<float>
            "m32" =? T<float>
            "m33" =? T<float>
            "m34" =? T<float>
            "m41" =? T<float>
            "m42" =? T<float>
            "m43" =? T<float>
            "m44" =? T<float>

            "is2D" =? T<bool>
            "isIdentity" =? T<bool>

            "flipX" => T<unit> ^-> TSelf
            "flipY" => T<unit> ^-> TSelf
            "inverse" => T<unit> ^-> TSelf
            "translate" => !?T<float>?tx * !?T<float>?ty * !?T<float>?tz ^-> TSelf
            "scale" => !?T<float>?scaleX * !?T<float>?scaleY * !?T<float>?scaleZ * !?T<float>?originX * !?T<float>?originY * !?T<float>?originZ ^-> TSelf
            "scaleNonUniform" => !?T<float>?scaleX * !?T<float>?scaleY ^-> TSelf
            "scale3D" => !?T<float>?scale * !?T<float>?originX * !?T<float>?originY * !?T<float>?originZ ^-> TSelf
            "rotate" => !?T<float>?rotX * !?T<float>?rotY * !?T<float>?rotZ ^-> TSelf
            "rotateFromVector" => !?T<float>?x * !?T<float>?y ^-> TSelf
            "rotateAxisAngle" => !?T<float>?x * !?T<float>?y * !?T<float>?z * !?T<float>?angle ^-> TSelf
            "skewX" => !?T<float>?sx ^-> TSelf
            "skewY" => !?T<float>?sY ^-> TSelf
            "multiply" => !?DOMMatrixInit?other ^-> TSelf
            "transformPoint" => !?DOMPointInit?point ^-> DOMPoint
            "toFloat32Array" => T<unit> ^-> TypedArrays.Float32Array
            "toFloat64Array" => T<unit> ^-> TypedArrays.Float64Array
            "toJSON" => T<unit> ^-> T<obj>
        ]

    let DOMMatrix =
        Class "DOMMatrix"
        |=> Inherits DOMMatrixReadOnly
        |+> Instance [
            "invertSelf" => T<unit> ^-> TSelf
            "translateSelf" => !?T<float>?tx * !?T<float>?ty * !?T<float>?tz ^-> TSelf
            "scaleSelf" => !?T<float>?scaleX * !?T<float>?scaleY * !?T<float>?scaleZ * !?T<float>?originX * !?T<float>?originY * !?T<float>?originZ ^-> TSelf
            "scale3DSelf" => !?T<float>?scale * !?T<float>?originX * !?T<float>?originY * !?T<float>?originZ ^-> TSelf
            "rotateSelf" => !?T<float>?rotX * !?T<float>?rotY * !?T<float>?rotZ ^-> TSelf
            "rotateFromVectorSelf" => !?T<float>?x * !?T<float>?y ^-> TSelf
            "rotateAxisAngleSelf" => !?T<float>?x * !?T<float>?y * !?T<float>?z * !?T<float>?angle ^-> TSelf
            "skewXSelf" => !?T<float>?sx ^-> TSelf
            "skewYSelf" => !?T<float>?sY ^-> TSelf
            "multiplySelf" => !?DOMMatrixInit?other ^-> TSelf
            "preMultiplySelf" => !?DOMMatrixInit?other ^-> TSelf
        ]

    let DOMQuadInit =
        Pattern.Config "DOMQuadInit" {
            Required = []
            Optional = [
                "p1", DOMPointInit.Type
                "p2", DOMPointInit.Type
                "p3", DOMPointInit.Type
                "p4", DOMPointInit.Type
            ]
        }

    let DOMRectInit =
        Pattern.Config "DOMRectInit" {
            Required = []
            Optional = [
                "x", T<float>
                "y", T<float>
                "width", T<float>
                "height", T<float>
            ]
        }

    let DOMRectReadOnly =
        Class "DOMRectReadOnly"
        |+> Static [
            Constructor (T<float>?x * T<float>?y * T<float>?width * T<float>?height)
            "fromRect" => DOMRectInit?other ^-> TSelf
        ]
        |+> Instance [
            "x" =? T<float>
            "y" =? T<float>
            "width" =? T<float>
            "height" =? T<float>
            "top" =? T<float>
            "right" =? T<float>
            "bottom" =? T<float>
            "left" =? T<float>
        ]

    let DOMRect =
        Class "DOMRect"
        |=> Inherits DOMRectReadOnly

    let DOMQuad =
        Class "DOMQuad"
        |+> Static [
            Constructor (DOMPointInit?p1 * DOMPointInit?p2 * DOMPointInit?p3 * DOMPointInit?p4)
            "fromRect" => DOMRectInit?other ^-> TSelf
            "fromQuad" => DOMQuadInit?other ^-> TSelf
        ]
        |+> Instance [
            "p1" =? DOMPoint
            "p2" =? DOMPoint
            "p3" =? DOMPoint
            "p4" =? DOMPoint

            "getBounds" => T<unit> ^-> DOMRect
        ]

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
        |+> Instance [
            "addColorStop" => (T<float> * T<string>) ^-> T<unit>
        ]
    
    let OffscreenCanvasClass =
        Class "OffscreenCanvas"

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

    let TextDirection =
        Pattern.EnumStrings "CanvasTextDirection" [
            "ltr"
            "rtl"
            "inherit"
        ]

    let PredefinedColorSpace =
        Pattern.EnumInlines "PredefinedColorSpace" [
            "srgb", "'srgb'"
            "display-p3", "DisplayP3"
        ]
    
    let LineJoin = 
        Pattern.EnumStrings "CanvasLineJoin" [
            "round"
            "bevel"
            "miter"
        ]

    let TextAlign = 
        Pattern.EnumStrings "CanvasTextAlign" [
            "start"
            "end"
            "left"
            "right"
            "center"
        ]
    
    let TextBaseline = 
        Pattern.EnumStrings "CanvasTextBaseLine" [
            "top"
            "hanging"
            "middle"
            "alphabetic"
            "ideographic"
            "bottom"
        ]
    
    let ImageDataSettings =
        Pattern.Config "ImageDataSettings" 
            {
                Required = []
                Optional = [
                    "colorSpace", PredefinedColorSpace.Type
                ]
            }

    let TextMetrics = 
        Class "TextMetrics"
        |+> Instance ["width" =@ T<float> ]
    
    let CanvasPixelArray = 
        Class "CanvasPixelArray"
        |+> Instance [
            "length" =? T<int>
            "Get" => T<int> ^-> T<int>
            |> WithInline "$this[$0]"
            "Set" => T<int> * T<int> ^-> T<int>
            |> WithInline "$this[$0] = $1"
        ]

    let ImageData =
        Class "ImageData"
        |+> Instance [
            "height" =? T<int> 
            "width" =? T<int> 
            "colorSpace" =? PredefinedColorSpace
            "data" =? TypedArrays.Uint8ClampedArray
        ]

    let CanvasState =
        Interface "CanvasState"
        |+> [
            "save" => T<unit> ^-> T<unit>
            "restore" => T<unit> ^-> T<unit>
            "reset" => T<unit> ^-> T<unit>
            "isContextLost" => T<unit> ^-> T<bool>
        ]

    let CanvasTransform =
        Interface "CanvasTransform"
        |+> [
            "scale" => (T<float>?x * T<float>?y) ^-> T<unit>
            "rotate" => (T<float>?angle) ^-> T<unit>
            "translate" => (T<float>?x * T<float>?y) ^-> T<unit>
            "transform" => (T<float>?a * T<float>?b * T<float>?c * T<float>?d * T<float>?e * T<float>?f) ^-> T<unit>
            "setTransform" => (T<float>?a * T<float>?b * T<float>?c * T<float>?d * T<float>?e * T<float>?f) ^-> T<unit>
            //"getTransform" => T<unit> ^-> DOMMatrix //!!!!!
            "resetTransform" => T<unit> ^-> T<unit>
        ]

    let ImageSmoothingQuality =
        Pattern.EnumStrings "ImageSmoothingQuality"
            [
                "low"
                "medium"
                "high"
            ]

    let CanvasCompositing =
        Interface "CanvasCompositing"
        |+> [
            "globalAlpha" =@ T<float>
            "globalCompositeOperation" =@ GlobalCompositeOperation
        ]

    let CanvasImageSmoothing =
        Interface "CanvasImageSmoothing"
        |+> [
            "imageSmootingEnabled" =@ T<bool>
            "imageSmoothingQuality" =@ ImageSmoothingQuality
        ]

    let HTMLOrSVGImageElement =
        Decl.HTMLImageElementClass + Decl.SVGImageElementClass

    let CanvasImageSource =
        HTMLOrSVGImageElement + Decl.HTMLVideoElementClass + Decl.HTMLCanvasElementClass + OffscreenCanvasClass //+ ImageBitmap + VideoFrame

    let CanvasFillStrokeStyles =
        Interface "CanvasFillStrokeStyles"
        |+> [
            "strokeStyle" =@ T<string> + CanvasGradient + CanvasPattern
            "fillStyle" =@ T<string> + CanvasGradient + CanvasPattern
            "createLinearGradient" => (T<float>?x0 * T<float>?y0 * T<float>?x1 * T<float>?y1) ^-> CanvasGradient
            "createRadialGradient" => (T<float>?x0 * T<float>?y0 * T<float>?r0 * T<float>?x1 * T<float>?y1 * T<float>?r1) ^-> CanvasGradient
            "createConicGradient" => (T<float>?angle * T<float>?x * T<float>?y) ^-> CanvasGradient
            "createPattern" => (CanvasImageSource?image * !?Repetition?repetition) ^-> CanvasPattern
        ]

    let CanvasShadowStyles =
        Interface "CanvasShadowStyles"
        |+> [
            "shadowOffsetX" =@ T<float>
            "shadowOffsetY" =@ T<float>
            "shadowBlur" =@ T<float>
            "shadowColor" =@ T<string>
        ]

    let CanvasFilters =
        Interface "CanvasFilters"
        |+> [
            "filter" =@ T<string>
        ]

    let CanvasRect =
        Interface "CanvasRect"
        |+> [
            "clearRect" => (T<float>?x * T<float>?y * T<float>?w * T<float>?h) ^-> T<unit>
            "fillRect" => (T<float>?x * T<float>?y * T<float>?w * T<float>?h) ^-> T<unit>
            "strokeRect" => (T<float>?x * T<float>?y * T<float>?w * T<float>?h) ^-> T<unit>
        ]

    let CanvasFillRule =
        Pattern.EnumStrings "CanvasFillRule" ["nonzero"; "evenodd"]

    let CanvasPath =
        Interface "CanvasPath"
        |+> [
            "closePath" => T<unit> ^-> T<unit>
            "roundRect" => (T<float>?x * T<float>?y * T<float>?w * T<float>?h * !?(T<float> + Geometry.DOMPointInit + !|(T<float> + Geometry.DOMPointInit))?radii) ^-> T<unit>
            "moveTo" => T<float>?x * T<float>?y ^-> T<unit>
            "lineTo" => T<float>?x * T<float>?y ^-> T<unit>
            "bezierCurveTo" => T<float>?cp1x * T<float>?cp1y * T<float>?cp2x * T<float>?cp2y * T<float>?x * T<float>?y ^-> T<unit>
            "quadraticCurveTo" => T<float>?cpx * T<float>?cpy * T<float>?x * T<float>?y ^-> T<unit>
            "arc" => (T<float>?x * T<float>?y * T<float>?radius * T<float>?startAngle * T<float>?endAngle * !?T<bool>?counterclockwise) ^-> T<unit>
            "arcTo" => (T<float>?x1 * T<float>?y1 * T<float>?x2 * T<float>?y2 * T<float>?radius) ^-> T<unit>
            "ellipse" => (T<float>?x * T<float>?y * T<float>?radiusX * T<float>?radiusY * T<float>?rotation * T<float>?startAngle * T<float>?endAngle * !?T<bool>?counterclockwise) ^-> T<unit>
            "rect" => (T<float>?x * T<float>?y * T<float>?width * T<float>?height) ^-> T<unit>
        ]

    let Path2D =
        Class "Path2D"
        |=> Implements [CanvasPath] 
        |+> Static [
            Constructor T<unit>
            Constructor TSelf
            Constructor T<string>
        ]
        |+> Instance [
            "addPath" => TSelf * !?Geometry.DOMMatrix2DInit?transform ^-> T<unit>
        ]

    let CanvasDrawPath =
        Interface "CanvasDrawPath"
        |+> [
            "beginPath" => T<unit> ^-> T<unit>
            "fill" => !?CanvasFillRule ^-> T<unit>
            "fill" => Path2D * !?CanvasFillRule ^-> T<unit>
            "stroke" => T<unit> ^-> T<unit>
            "stroke" => Path2D ^-> T<unit>
            "clip" => !?CanvasFillRule ^-> T<unit>
            "clip" => Path2D * !?CanvasFillRule ^-> T<unit>
            "isPointInPath" => (T<float> * T<float> * !?CanvasFillRule) ^-> T<bool>
            "isPointInPath" => (Path2D * !?CanvasFillRule) ^-> T<bool>
            "isPointInStroke" => (T<float> * T<float> * !?CanvasFillRule) ^-> T<bool>
            "isPointInStroke" => (Path2D * T<float> * !?CanvasFillRule) ^-> T<bool>
        ]

    let CanvasUserInterface =
        Interface "CanvasUserInterface"
        |+> [
            "drawFocusIfNeeded" => Dom.Interfaces.Element?element ^-> T<unit>
            "drawFocusIfNeeded" => Path2D?path * Dom.Interfaces.Element?element ^-> T<unit>
            "scrollPathIntoView" => T<unit> ^-> T<unit>
            "scrollPathIntoView" => Path2D?path ^-> T<unit>
        ]

    let CanvasText =
        Interface "CanvasText"
        |+> [
            "fillText" => T<string>?text * T<float>?x * T<float>?y * !?T<float>?maxWidth ^-> T<unit>
            "strokeText" => T<string>?text * T<float>?x * T<float>?y * !?T<float>?maxWidth ^-> T<unit>
            "measureText" => T<string>?text ^-> TextMetrics
        ]

    let CanvasDrawImage =
        Interface "CanvasDrawImage"
        |+> [
            "drawImage" => (Dom.Interfaces.Element * T<float> * T<float>) ^-> T<unit>
            "drawImage" => (Dom.Interfaces.Element * T<float> * T<float> * T<float> * T<float>) ^-> T<unit>
            "drawImage" => (Dom.Interfaces.Element * T<float> * T<float> * T<float> * T<float> * T<float> * T<float> * T<float> * T<float>) ^-> T<unit>
        ]

    let CanvasImageData =
        Interface "CanvasImageData"
        |+> [
            "createImageData" => (T<float> * T<float> * !?ImageDataSettings) ^-> ImageData
            "createImageData" => (ImageData) ^-> ImageData
            "getImageData"    => (T<float> * T<float> * T<float> * T<float> * !?ImageDataSettings) ^-> ImageData

            "putImageData" => (ImageData * T<float> * T<float> ) ^-> T<unit>
            "putImageData" => (ImageData * T<float> * T<float> * T<float> * T<float> * T<float> * T<float>) ^-> T<unit>
        ]

    let CanvasPathDrawingStyles =
        Interface "CanvasPathDrawingStyles"
        |+> [
            "lineWidth" =@ T<float> 
            "lineCap" =@ LineCap  
            "lineJoin" =@ LineJoin
            "miterLimit" =@ T<float>
            "getLineDash" => T<unit> ^-> T<float []>
            "setLineDash" => T<float []> ^-> T<unit>
            "lineDashOffset" =@ T<float>
        ]

    let CanvasFontKerning = Pattern.EnumStrings "CanvasFontKerning" ["auto"; "normal"; "none"]

    let CanvasFontStretch =
        Pattern.EnumInlines "CanvasFontStretch" [
            "UltraCondensed", "'ultra-condensed'"
            "ExtraCondensed", "'extra-condensed'"
            "Condensed", "'condensed'"
            "SemiCondensed", "'semi-condensed'"
            "UltraExpanded", "'ultra-expanded'"
            "ExtraExpanded", "'extra-expanded'"
            "Expanded", "'expanded'"
        ]

    let CanvasFontVariantCaps =
        Pattern.EnumInlines "CanvasFontVariantCaps" [
            "Normal", "'normal'"
            "SmallCaps", "'small-caps'"
            "AllSmallCaps", "'all-small-caps'"
            "PetiteCaps", "'petite-caps'"
            "AllPetiteCaps", "'all-petite-caps'"
            "Unicase", "'unicase'"
            "TitlingCaps", "'titling-caps'"
        ]

    let CanvasTextRendering = Pattern.EnumStrings "CanvasTextRendering" ["auto"; "optimizeSpeed"; "optimizeLegibility"; "geometricPrecision"]

    let CanvasTextDrawingStyles =
        Interface "CanvasTextDrawingStyles"
        |+> [
            "font" =@ T<string>
            "textAlign" =@ TextAlign
            "textBaseline" =@ TextBaseline
            "direction" =@ TextDirection
            "letterSpacing" =@ T<string>
            "fontKerning" =@ CanvasFontKerning
            "fontStretch" =@ CanvasFontStretch
            "fontVariantCaps" =@ CanvasFontVariantCaps
            "textRendering" =@ CanvasTextRendering
            "wordSpacing" =@ T<string>
        ]
    
    let CanvasRenderingContext2D = 
        Class "CanvasRenderingContext2D"
        |=> Implements
            [
                CanvasState
                CanvasTransform
                CanvasCompositing
                CanvasImageSmoothing
                CanvasFillStrokeStyles
                CanvasShadowStyles
                CanvasFilters
                CanvasRect
                CanvasDrawPath
                CanvasUserInterface
                CanvasText
                CanvasDrawImage
                CanvasImageData
                CanvasPathDrawingStyles
                CanvasTextDrawingStyles
                CanvasPath
            ]
        |+> Instance [
            "canvas" =? Decl.HTMLCanvasElementClass
        ]

    let OffScreenCanvas =
        Class "OffScreenCanvas"

module AudioVideoCommon =
    
    let TimeRanges = 
        Class "TimeRanges"
        |+> Instance [
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
        |+> Instance [
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
        |+> Instance [
            "length" =? T<int>
            "getter" => (T<int>) ^-> TimedTrackCue
            "getCueById" => (T<string>) ^-> TimedTrackCue
        ]

    do  TimedTrack
        |+> Instance [
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
        |+> Instance [
            "addCue" => TimedTrackCue ^-> T<unit>
            "removeCue" => TimedTrackCue ^-> T<unit>
        ]

    let MediaPreload =
        Pattern.EnumStrings "Preload" [
            "none"
            "metadata"
            "auto"
        ]

module EventHandlers =
    
    let private eh = Dom.Interfaces.Event ^-> T<unit>

    let ElementContentEditable =
        Instance [
            "contentEditable" =@ T<string>
            "isContentEditable" =? T<bool>
        ]

    let GlobalEventHandlers =
        Instance [
            "onabort" =@ eh
            "onauxclick" =@ eh
            "onblur" =@ eh
            "oncancel" =@ eh
            "oncanplay" =@ eh
            "oncanplaythrough" =@ eh
            "onchange" =@ eh
            "onclick" =@ eh
            "onclose" =@ eh
            "oncontextmenu" =@ eh
            "oncuechange" =@ eh
            "ondblclick" =@ eh
            "ondrag" =@ eh
            "ondragend" =@ eh
            "ondragenter" =@ eh
            "ondragexit" =@ eh
            "ondragleave" =@ eh
            "ondragover" =@ eh
            "ondragstart" =@ eh
            "ondrop" =@ eh
            "ondurationchange" =@ eh
            "onemptied" =@ eh
            "onended" =@ eh
            "onerror" =@ eh
            "onfocus" =@ eh
            "oninput" =@ eh
            "oninvalid" =@ eh
            "onkeydown" =@ eh
            "onkeypress" =@ eh
            "onkeyup" =@ eh
            "onload" =@ eh
            "onloadeddata" =@ eh
            "onloadedmetadata" =@ eh
            "onloadend" =@ eh
            "onloadstart" =@ eh
            "onmousedown" =@ eh
            "onmouseenter" =@ eh
            "onmouseleave" =@ eh
            "onmousemove" =@ eh
            "onmouseout" =@ eh
            "onmouseover" =@ eh
            "onmouseup" =@ eh
            "onwheel" =@ eh
            "onpause" =@ eh
            "onplay" =@ eh
            "onplaying" =@ eh
            "onprogress" =@ eh
            "onratechange" =@ eh
            "onreset" =@ eh
            "onresize" =@ eh
            "onscroll" =@ eh
            "onseeked" =@ eh
            "onseeking" =@ eh
            "onselect" =@ eh
            "onshow" =@ eh
            "onstalled" =@ eh
            "onsubmit" =@ eh
            "onsuspend" =@ eh
            "ontimeupdate" =@ eh
            "ontoggle" =@ eh
            "onvolumechange" =@ eh
            "onwaiting" =@ eh
        ]

    let DocumentAndElementEventHandlers =
        Instance [
            "oncopy" =@ eh
            "oncut" =@ eh
            "onpaste" =@ eh
        ]

module Streamable =

    let ReadableStream =
        Class "ReadableStream"

    let WritableStream =
        Class "WritableStream"

    let ReadableStreamReaderMode =
        Pattern.EnumStrings "ReadableStreamReaderMode" [
            "byob"
        ]

    let ReadableStreamGetReaderOptions =
        Pattern.Config "ReadableStreamGetReaderOptions"
            {
                Required =
                    [
                        "mode", ReadableStreamReaderMode.Type
                    ]
                Optional = []
            }

    let QueuingStrategy =
        Pattern.Config "QueuingStrategy"
            {
                Required =
                    [
                        "highWaterMark", T<float>
                        "size", T<obj> ^-> T<float>
                    ]
                Optional = []
            }

    let QueuingStrategyInit =
        Pattern.Config "QueuingStrategyInit"
            {
                Required =
                    [
                        "highWaterMark", T<float>
                    ]
                Optional = []
            }

    let ReadableStreamResult =
        Pattern.Config "ReadableStreamResult"
            {
                Required =
                    [
                        "value", T<obj>
                        "done", T<bool>
                    ]
                Optional = []
            }

    let ByteLengthQueuingStrategy =
        Class "ByteLengthQueuingStrategy"
        |+> Static [
            Constructor QueuingStrategyInit
        ]
        |+> Instance [
            "highWaterMark" =? T<float>
            "size" => !+T<obj> ^-> T<obj>
        ]

    let CountQueuingStrategy =
        Class "CountQueuingStrategy"
        |+> Static [
            Constructor QueuingStrategyInit
        ]
        |+> Instance [
            "highWaterMark" =? T<float>
            "size" => !+T<obj> ^-> T<obj>
        ]

    let ReadableStreamGenericReader =
        Class "ReadableStreamGenericReader"
        |+> Instance [
            "closed" =? EcmaPromise.[T<unit>]
            "cancel" => !?T<obj>?reason ^-> EcmaPromise.[T<unit>]
        ]

    let ReadableStreamDefaultReader =
        Class "ReadableStreamDefaultReader"
        |=> Inherits ReadableStreamGenericReader
        |+> Static [
            Constructor ReadableStream
        ]
        |+> Instance [
            "read" => T<unit> ^-> ReadableStreamResult
            "releaseLock" => T<unit> ^-> T<unit>
        ]

    let ReadableStreamBYOBReader =
        Class "ReadableStreamBYOBReader"
        |=> Inherits ReadableStreamGenericReader
        |+> Static [
            Constructor ReadableStream
        ]
        |+> Instance [
            "read" => TypedArrays.ArrayBufferView?view ^-> ReadableStreamResult
            "releaseLock" => T<unit> ^-> T<unit>
        ]

    let ReadableStreamReader = ReadableStreamDefaultReader + ReadableStreamBYOBReader

    let ReadableStreamBYOBRequest =
        Class "ReadableStreamBYOBRequest"
        |+> Instance [
            "view" =? TypedArrays.ArrayBufferView

            "respond" => T<uint>?bytesWritten ^-> T<unit>
            "respondWithNewView" => TypedArrays.ArrayBufferView?view ^-> T<unit>
        ]

    let ReadableByteStreamController =
        Class "ReadableByteStreamController"
        |+> Instance [
            "byobRequest" =? !?ReadableStreamBYOBRequest
            "desiredSize" =? T<float>

            "close" => T<unit> ^-> T<unit>
            "enqueue" => TypedArrays.ArrayBufferView?chunk ^-> T<unit>
            "error" => !?T<obj>?e ^-> T<unit>
        ]

    let ReadableStreamDefaultController =
        Class "ReadableStreamDefaultController"
        |+> Instance [
            "desiredSize" =? T<float>

            "close" => T<unit> ^-> T<unit>
            "enqueue" => !?T<obj>?chunk ^-> T<unit>
            "error" => !?T<obj>?e ^-> T<unit>
        ]

    let WritableStreamDefaultController =
        Class "WritableStreamDefaultController"
        |+> Instance [
            "signal" =? Dom.Interfaces.AbortSignal
            "error" => !?T<obj>?e ^-> T<unit>
        ]

    let QS = QueuingStrategy + CountQueuingStrategy + ByteLengthQueuingStrategy

    let StreamPipeOptions =
        Pattern.Config "StreamPipeOptions"
            {
                Required =
                    [
                        "signal", Dom.Interfaces.AbortSignal.Type
                    ]
                Optional =
                    [
                        "preventClose", T<bool>
                        "preventAbort", T<bool>
                        "preventCancel", T<bool>
                    ]
            }

    let ReadableWritablePair =
        Pattern.Config "ReadableWritablePair"
            {
                Required =
                    [
                        "readable", ReadableStream.Type
                        "writable", WritableStream.Type
                    ]
                Optional = []
            }
    let RS =
        ReadableStream
        |+> Static [
            Constructor (!?T<obj>?underlyingSource * !?QS?strategy)
        ]
        |+> Instance [
            "locked" =? T<bool>

            "cancel" => !?T<obj>?reason ^-> EcmaPromise.[T<unit>]
            "getReader" => !?ReadableStreamGetReaderOptions?options ^-> ReadableStreamReader
            "pipeThrough" => ReadableWritablePair?transform * !? StreamPipeOptions?options ^-> TSelf
            "pipeTo" => WritableStream?destination * !? StreamPipeOptions?options ^-> EcmaPromise.[T<unit>]
            "tee" => T<unit>
        ]

    let WritableStreamDefaultWriter =
        Class "WritableStreamDefaultWriter"
        |+> Static [
            Constructor WritableStream?stream
        ]
        |+> Instance [
            "closed" =? EcmaPromise.[T<unit>]
            "desiredSize" =? T<double>
            "ready" =? EcmaPromise.[T<unit>]

            "abort" => !?T<obj>?reason ^-> EcmaPromise.[T<unit>]
            "close" => T<unit> ^-> EcmaPromise.[T<unit>]
            "write" => !?T<obj>?chunk ^-> EcmaPromise.[T<unit>]
            "releaseLock" => T<unit> ^-> T<unit>
        ]

    let WS =
        WritableStream
        |+> Static [
            Constructor (!?T<obj>?underlyingSink * !?QS?strategy)
        ]
        |+> Instance [
            "locked" =? T<bool>

            "abort" => !?T<obj>?reason ^-> EcmaPromise.[T<unit>]
            "close" => T<unit> ^-> EcmaPromise.[T<unit>]
            "getWriter" => T<unit> ^-> WritableStreamDefaultWriter
        ]

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
        Class "Blob"
        |+> Static [
                Constructor T<unit>
                Constructor ((Type.ArrayOf TypedArrays.ArrayBuffer + Type.ArrayOf TypedArrays.ArrayBufferView + Type.ArrayOf TSelf + Type.ArrayOf T<string>) * !?BlobPropertyBag)
            ]
        |+> Instance [
                "size" =? T<int>
                "type" =? T<string>
                "slice" => T<int>?start * T<int>?``end`` * T<string>?contentType ^-> TSelf
                "arrayBuffer" => T<unit> ^-> EcmaPromise.[TypedArrays.ArrayBuffer]
                "text" => T<unit> ^-> EcmaPromise.[T<string>]
                "stream" => T<unit> ^-> Streamable.ReadableStream
            ]

    let File =
        Class "File"
        |=> Inherits Blob
        |+> Instance [
                "name" =? T<string>
                "lastModifiedDate" =? EcmaDate |> Obsolete
                "lastModifed" =? T<int>
                "webkitRelativePath" =? T<string>
            ]

    let ProgressEvent =
        Class "ProgressEvent"
        |=> Inherits Dom.Interfaces.Event
        |+> Instance [
                "lengthComputable" =? T<bool>
                "loaded" =? T<int>
                "total" =? T<int>
            ]

    let FileList =
        Class "FileList"
        |+> Instance [
                "item" => T<int> ^-> File
                "length" =? T<int>
            ]
        |+> Static [
                "ofElement" => Dom.Interfaces.Element?input ^-> TSelf
                |> WithInline "$input.files"
                "ofEvent" => ProgressEvent?event ^-> TSelf
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
        Generic - fun t ->
        Class "FileReader"
        |=> Inherits Dom.Interfaces.EventTarget
        |+> Instance [
                "abort" => T<unit> ^-> T<unit>
                "readyState" =? FileReaderReadyState
                "result" =? t
                "error" =? T<exn>
                "onloadstart" =@ ProgressEvent ^-> T<unit>
                "onprogress" =@ ProgressEvent ^-> T<unit>
                "onload" =@ ProgressEvent ^-> T<unit>
                "onabort" =@ ProgressEvent ^-> T<unit>
                "onerror" =@ ProgressEvent ^-> T<unit>
                "onloadend" =@ ProgressEvent ^-> T<unit>
                "readAsArrayBuffer " => Blob ^-> T<unit>
                "readAsText" => Blob * !?T<string>?encoding ^-> T<unit>
                "readAsDataURL" => Blob ^-> T<unit>
                "readAsBinaryString" => Blob * !?T<string>?encoding ^-> T<unit>
        ]

    let TextFileReader =
        Class "TextFileReader"
        |=> Inherits (FileReader.[T<string>])
        |+> Static [
                Constructor T<unit>
                |> WithInline "new FileReader()"
            ]
        |+> Instance [
                "readAsText" => Blob * !?T<string>?encoding ^-> T<unit>
                "readAsDataURL" => Blob ^-> T<unit>
            ]

    let BinaryFileReader =
        Class "BinaryFileReader"
        |=> Inherits (FileReader.[TypedArrays.ArrayBuffer])
        |+> Static [
                Constructor T<unit>
                |> WithInline "new FileReader()"
            ]
        |+> Instance [
                "readAsArrayBuffer" => Blob ^-> T<unit>
            ]


module Elements =
    open Canvas
    open AudioVideoCommon

    let HTMLElement =
        Class "HTMLElement"
        |=> Inherits Dom.Interfaces.Element
        |+> Instance [
            "title" =@ T<string>
            "lang" =@ T<string>
            "translate" =@ T<bool>
            "dir" =@ T<string>

            "hidden" =@ T<bool>
            "tabIndex" =@ T<int>

            "click" => T<unit> ^-> T<unit>
            "focus" => T<unit> ^-> T<unit>
            "blur" => T<unit> ^-> T<unit>

            "accessKey" =@ T<string>
            "accessKeyLabel" =? T<string>
            "draggable" =@ T<bool>
            "spellCheck" =@ T<bool>
            "forceSpellCheck" => T<unit> ^-> T<unit>

            "innerText" =@ T<string>
        ]
        |+> EventHandlers.GlobalEventHandlers
        |+> EventHandlers.DocumentAndElementEventHandlers
        |+> EventHandlers.ElementContentEditable

    let HTMLMenuElement =
        Class "HTMLMenuElement"
        |=> Inherits HTMLElement
        |+> Instance [
            "type" =@ T<string>
            "label" =@ T<string>
        ]

    let HTMLMenuItemElement =
        Class "HTMLMenuItemElement"
        |=> Inherits HTMLElement
        |+> Instance [
            "type" =@ T<string>
            "label" =@ T<string>
            "icon" =@ T<string>
            "disabled" =@ T<bool>
            "checked" =@ T<bool>
            "radiogroup" =@ T<string>
            "default" =@ T<bool>

        ]

    let RadioNodeList =
        Class "RadioNodeList"
        |=> Inherits Dom.Interfaces.NodeList
        |+> Instance [
            "value" =@ T<string>
        ]

    let HTMLFormControlsCollection =
        Class "HTMLFormControlsCollection"
        |=> Inherits Dom.Interfaces.HTMLCollection
        |+> Instance [
            "namedItem" => T<string> ^-> T<obj> //?? it returns either a RadioNodeList or an Element
        ]

    let HTMLFormElement =
        Class "HTMLFormElement"
        |=> Inherits HTMLElement
        |+> Instance [
            "acceptCharset" =@ T<string>
            "action" =@ T<string>
            "autocomplete" =@ T<string>
            "enctype" =@ T<string>
            "encoding" =@ T<string>
            "method" =@ T<string>
            "name" =@ T<string>
            "noValidate" =@ T<bool>
            "target" =@ T<string>

            "elements" =? HTMLFormControlsCollection

            "length" =? T<int>
            "submit" => T<unit> ^-> T<unit>
            "reset" => T<unit> ^-> T<unit>
            "checkValidity" => T<unit> ^-> T<bool>
            "reportValidity" => T<unit> ^-> T<bool>
        ]

    let HTMLLabelElement =
        Class "HTMLLabelElement"
        |=> Inherits HTMLElement
        |+> Instance [
            "form" =? HTMLFormElement
            "control" =? HTMLElement
            "htmlFor" =@ T<string>
        ]

    let SelectionMode =
        Pattern.EnumStrings "SelectionMode" [
            "select"
            "start"
            "end"
            "preserve"
        ]

    let HTMLInputElement =
        Class "HTMLInputElement"
        |=> Inherits HTMLElement
        |+> Instance [
            "accept" =@ T<string>
            "alt" =@ T<string>
            "autocomplete" =@ T<string>
            "autofocus" =@ T<bool>
            "defaultChecked" =@ T<bool>
            "checked" =@ T<bool>
            "dirName" =@ T<string>
            "form" =? HTMLFormElement
            "files" =? File.FileList
            "formAction" =@ T<string>
            "formEnctype" =@ T<string>
            "formMethod" =@ T<string>
            "formNoValidate" => T<bool>
            "formTarget" =@ T<string>
            "height" =@ T<int>
            "indeterminate" =@ T<bool>
            "inputMode" =@ T<string>
            "list" =? HTMLElement
            "max" =@ T<string>
            "maxLength" =@ T<int>
            "min" =@ T<string>
            "minLength" =@ T<int>
            "multiple" =@ T<bool>
            "name" =@ T<string>
            "pattern" =@ T<string>
            "placeholder" =@ T<string>
            "readOnly" =@ T<bool>
            "required" =@ T<bool>
            "size" =@ T<int>
            "src" =@ T<string>
            "step" =@ T<string>
            "type" =@ T<string>
            "value" =@ T<string>
            "valueAsDate" =@ T<obj>
            "valueAsNumber" =@ T<double>
            "width" =@ T<int>
            
            "stepUp" => !?T<int> ^-> T<unit> |> WithComment "The paramter deafults to 1"
            "stepDown" => !?T<int> ^-> T<unit> |> WithComment "The paramter deafults to 1"

            "willValidate" =? T<bool>
            "validity" =? T<obj>
            "validationMessage" =? T<string>
            "checkValidity" => T<unit> ^-> T<bool>
            "reportValidity" => T<unit> ^-> T<bool>
            "setCustomValidity" => T<string> ^-> T<unit>

            "labels" =? Dom.Interfaces.NodeList

            "select" => T<unit> ^-> T<unit>
            
            "selectionStart" =@ T<int>
            "selectionEnd" =@ T<int>
            "selectionDirection" =@ T<string>
            "setRangeText" => T<string> ^-> T<unit>
            "setRangeText" => (T<string> * T<int> * T<int> * !?SelectionMode) ^-> T<unit>
            "setSelectionRange" => (T<int> * T<int> * T<string>) ^-> T<unit>
        ]

    let HTMLButtonElement =
        Class "HTMLButtonElement"
        |=> Inherits HTMLElement
        |+> Instance [
            "autofocus" =@ T<bool>
            "disabled" =@ T<bool>
            "form" =? HTMLFormElement
            "formAction" =@ T<string>
            "formEnctype" =@ T<string>
            "formMethod" =@ T<string>
            "formNoValidate" => T<bool>
            "formTarget" =@ T<string>
            "name" =@ T<string>
            "type" =@ T<string>
            "value" =@ T<string>
            "menu" =@ HTMLMenuElement

            "willValidate" =? T<bool>
            "validity" =? T<obj>
            "validationMessage" =? T<string>
            "checkValidity" => T<unit> ^-> T<bool>
            "reportValidity" => T<unit> ^-> T<bool>
            "setCustomValidity" => T<string> ^-> T<unit>

            "labels" =? Dom.Interfaces.NodeList
        ]

    let HTMLOptGroupElement =
        Class "HTMLOptGroupElement"
        |=> Inherits HTMLElement
        |+> Instance [
            "disabled" =@ T<bool>
            "label" =@ T<string>
        ]

    let HTMLOptionElement =
        Class "HTMLOptionElement"
        |=> Inherits HTMLElement
        |+> Instance [
            "disabled" =@ T<bool>
            "label" =@ T<string>
            "form" =? HTMLFormElement
            "defaultSelected" =@ T<bool>
            "selected" =@ T<bool>
            "value" =@ T<string>
            "text" =@ T<string>
            "index" =? T<int>
        ]

    let HTMLOptionsCollection =
        Class "HTMLOptionsCollection"
        |=> Inherits Dom.Interfaces.HTMLCollection
        |+> Instance [
            "length" =@ T<int>
            "add" => (HTMLOptionElement + HTMLOptGroupElement) * !?(HTMLElement + T<int>) ^-> T<unit>
            "remove" => T<int> ^-> T<unit>
            "selectedIndex" =@ T<int>
        ]

    let HTMLSelectElement = 
        Class "HTMLSelectElement"
        |=> Inherits HTMLElement
        |+> Instance [
            "autocomplete" =@ T<string>
            "autofocus" =@ T<bool>
            "disabled" =@ T<bool>
            "form" =? HTMLFormElement
            "multiple" =@ T<bool>
            "name" =@ T<string>
            "required" =@ T<bool>
            "size" =@ T<int>
            
            "type" =? T<string>
            
            // HTMLOptionsCollection
            "length" =? T<int>
            "item" => T<int> ^-> Dom.Interfaces.Element
            "namedItem" => T<string> ^-> HTMLOptionElement
            "add" => HTMLOptGroupElement * HTMLElement ^-> T<unit>
            "add" => HTMLOptionElement * HTMLElement ^-> T<unit>
            "add" => HTMLOptGroupElement * T<int> ^-> T<unit>
            "add" => HTMLOptionElement * T<int> ^-> T<unit>
            "add" => HTMLOptGroupElement ^-> T<unit>
            "add" => HTMLOptionElement ^-> T<unit>
            "remove" => T<unit> ^-> T<unit>
            "remove" => T<int> ^-> T<unit>

            "willValidate" =? T<bool>
            "validity" =? T<obj>
            "validationMessage" =? T<string>
            "checkValidity" => T<unit> ^-> T<bool>
            "reportValidity" => T<unit> ^-> T<bool>
            "setCustomValidity" => T<string> ^-> T<unit>

            "labels" =? Dom.Interfaces.NodeList
        ]

    let HTMLDataListElement =
        Class "HTMLDataListElement"
        |=> Inherits HTMLElement
        |+> Instance [
            "options" =? Dom.Interfaces.HTMLCollection
        ]

    let HTMLTextAreaElement =
        Class "HTMLTextAreaElement"
        |=> Inherits HTMLElement
        |+> Instance [
            "autocomplete" =@ T<string>
            "autofocus" =@ T<bool>
            "cols" =@ T<int>
            "checked" =@ T<bool>
            "dirName" =@ T<string>
            "disabled" =@ T<bool>
            "form" =? HTMLFormElement
            "inputMode" =@ T<string>
            "maxLength" =@ T<int>
            "minLength" =@ T<int>
            "name" =@ T<string>
            "placeholder" =@ T<string>
            "readOnly" =@ T<bool>
            "required" =@ T<bool>
            "rows" =@ T<int>
            "wrap" =@ T<string>
            "type" =@ T<string>
            "value" =@ T<string>
            "textLength" =@ T<int>
            "defaultValue" =@ T<string>

            "willValidate" =? T<bool>
            "validity" =? T<obj>
            "validationMessage" =? T<string>
            "checkValidity" => T<unit> ^-> T<bool>
            "reportValidity" => T<unit> ^-> T<bool>
            "setCustomValidity" => T<string> ^-> T<unit>

            "labels" =? Dom.Interfaces.NodeList

            "select" => T<unit> ^-> T<unit>
            
            "selectionStart" =@ T<int>
            "selectionEnd" =@ T<int>
            "selectionDirection" =@ T<string>
            "setRangeText" => T<string> ^-> T<unit>
            "setRangeText" => (T<string> * T<int> * T<int> * !?SelectionMode) ^-> T<unit>
            "setSelectionRange" => (T<int> * T<int> * T<string>) ^-> T<unit>
        ]

    let HTMLOutputElement =
        Class "HTMLOutputElement"
        |=> Inherits HTMLElement
        |+> Instance [
            "htmlFor" =? Dom.Interfaces.DOMTokenList
            "form" =? HTMLFormElement
            "name" =@ T<string>
            "type" =? T<string>
            "defaultValue" =@ T<string>
            "value" =@ T<string>

            "willValidate" =? T<bool>
            "validity" =? T<obj>
            "validationMessage" =? T<string>
            "checkValidity" => T<unit> ^-> T<bool>
            "reportValidity" => T<unit> ^-> T<bool>
            "setCustomValidity" => T<string> ^-> T<unit>

            "labels" =? Dom.Interfaces.NodeList
        ]

    let HTMLProgressElement =
        Class "HTMLProgressElement"
        |=> Inherits HTMLElement
        |+> Instance [
            "value" =@ T<double>
            "max" =@ T<double>
            "position" =? T<double>

            "labels" =? Dom.Interfaces.NodeList
        ]

    let HTMLMeterElement =
        Class "HTMLMeterElement"
        |=> Inherits HTMLElement
        |+> Instance [
            "value" =@ T<double>
            "max" =@ T<double>
            "min" =@ T<double>
            "low" =@ T<double>
            "high" =@ T<double>
            "optimum" =@ T<double>

            "labels" =? Dom.Interfaces.NodeList
        ]

    let HTMLDetailsElement =
        Class "HTMLDetailsElement"
        |=> Inherits HTMLElement
        |+> Instance [
            "open" =@ T<bool>
        ]

    let HTMLLegendElement =
        Class "HTMLLegendElement"
        |=> Inherits HTMLElement
        |+> Instance [
            "form" =? HTMLFormElement
        ]

    let HTMLFieldSetElement =
        Class "HTMLFieldSetElement"
        |=> Inherits HTMLElement
        |+> Instance [
            "disabled" =@ T<bool>
            "form" =? HTMLFormElement
            "name" =@ T<string>
            "type" =? T<string>
            "elements" =? Dom.Interfaces.HTMLCollection

            "willValidate" =? T<bool>
            "validity" =? T<obj>
            "validationMessage" =? T<string>
            "checkValidity" => T<unit> ^-> T<bool>
            "reportValidity" => T<unit> ^-> T<bool>
            "setCustomValidity" => T<string> ^-> T<unit>
        ]

    let HTMLDialogElement =
        Class "HTMLDialogElement"
        |=> Inherits HTMLElement
        |+> Instance [
            "open" =@ T<bool>
            "returnValue" =@ T<string>
            "show" => T<unit> ^-> T<unit>
            "showModal" => T<unit> ^-> T<unit>
            "close" => (!?T<string>) ^-> T<unit>
        ]

    let CanvasElement =
        Decl.HTMLCanvasElementClass
        |=> Inherits HTMLElement
        |+> Instance [
            "width" =@ T<int>
            "height" =@ T<int>
            "toDataURL" => !? T<string>?a * !? T<float>?b ^-> T<string>
            "getContext" => T<string> ^-> Canvas.CanvasRenderingContext2D
            "captureStream" => T<int> ^-> Media.MediaStream
            "toBlob" => T<unit> ^-> File.Blob
            "transferControlToOffScreen" => T<unit> ^-> Canvas.OffscreenCanvasClass
        ]

    let OldCanvasElement =
        Class "CanvasElement"
        |=> Inherits HTMLElement
        |+> Static [
            Constructor T<unit>
                |> ObsoleteWithMessage "CanvasElement is now obsolete, use HTMLCanvasElement instead" 
                |> WithInline "new HTMLCanvasElement()"
        ]
        |+> Instance [
            "width" =@ T<int>
            "height" =@ T<int>
            "toDataURL" => !? T<string>?a * !? T<float>?b ^-> T<string>
            "getContext" => T<string> ^-> Canvas.CanvasRenderingContext2D
            "captureStream" => T<int> ^-> Media.MediaStream
            "toBlob" => T<unit> ^-> File.Blob
            "transferControlToOffScreen" => T<unit> ^-> Canvas.OffscreenCanvasClass
        ]


    let HTMLMediaElement =
        Class "HTMLMediaElement"
        |=> Inherits HTMLElement
        |+> Instance [
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

            "startOffsetTime" =? EcmaDate
            "played" =? TimeRanges
            "seekable" =? TimeRanges
            "ended" =? T<bool>
            "paused" =? T<bool>
            "duration" =? T<float>
            "initialTime" =? T<float>
            "crossOrigin" =@ T<string>
            "defaultMuted" =@ T<bool>
            "disableRemotePlayback" =@ T<bool>
            "mediaGroup" =@ T<string>
            "preload" =@ MediaPreload

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
        Decl.HTMLVideoElementClass
        |=> Inherits HTMLMediaElement
        |+> Instance [
            "width" =@ T<string>
            "height" =@ T<string>
            "videoWidth" =? T<int>
            "videoHeight" =? T<int>
            "poster" =@ T<string>
        ]

    let HTMLAudioElement =
        Class "HTMLAudioElement"
        |=> Inherits HTMLMediaElement
        |+> Instance [
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
        |+> Instance [
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
        |+> Instance [
            "coords" =? Coordinates
            "timestamp" =? EcmaDate
        ]

    let PositionError = 
        Class "PositionError"
        |+> Instance [
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
        |+> Instance [
            "getCurrentPosition" => (positionCallback?p * !? errorCallback?e * !? PositionOptions?o) ^-> T<unit>
            "watchPosition" => (positionCallback?p * !? errorCallback?e * !? PositionOptions?o) ^-> T<int>
            "clearWatch" => T<int> ^-> T<unit>
        ]

module WebStorage =

    let Storage =
        Class "Storage"
        |+> Instance [
                "length" =? T<int>
                "key" => T<int> ^-> T<string>
                "getItem" => T<string> ^-> T<string>
                "setItem" => T<string>?key * T<string>?value ^-> T<unit>
                "removeItem" => T<string -> unit>
                "clear" => T<unit->unit>
            ]

    let StorageEvent =
        Class "StorageEvent"
        |+> Instance [
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
        |+> Instance [
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
            "abort" => T<unit> ^-> T<unit>

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

module Fetch =

    let FormDataEntryValue = T<string> + File.File

    let FormData =
        Class "FormData"
        |+> Instance [
            Constructor(!? Elements.HTMLFormElement?form)
            "append" => T<string>?names * T<string>?value ^-> T<unit> 
            "append" => T<string>?names * File.Blob?blobValue * !? T<string>?filename ^-> T<unit> 
            "delete" => T<string>?name ^-> T<unit> 
            "get" => T<string>?name ^-> FormDataEntryValue
            "getAll" => T<string>?name ^-> !|FormDataEntryValue
            "has"  => T<string>?name ^-> T<bool>
            "set" => T<string>?names * T<string>?value ^-> T<unit> 
            "set" => T<string>?names * File.Blob?blobValue * !? T<string>?filename ^-> T<unit> 
            "" =@ FormDataEntryValue |> Indexed T<string> 
            // TODO: entries iterator
            // TODO: keys iterator
            // TODO: values iterator
        ]

    let URLSearchParams =
        Class "URLSearchParams"
        |+> Static [
            Constructor T<string>
            Constructor T<(string * string)[]>
            Constructor T<string[]>
            Constructor EcmaObjectG.[T<string>]
        ]
        |+> Instance [
            "append" => T<string>?key * T<string>?value ^-> T<unit>
            "delete" => T<string>?key ^-> T<unit>
            "get" => T<string>?key ^-> T<string>
            "getAll" => T<string>?key ^-> T<string[]>
            "has" => T<string>?key ^-> T<bool>
            "set" => T<string>?key * T<string>?value ^-> T<unit>
            "sort" => T<unit> ^-> T<unit>
        ]

    let XMLHttpRequestResponseType =
        Pattern.EnumStrings "XMLHttpRequestResponseType" [
            "arraybuffer"
            "blob"
            "document"
            "json"
            "text"
        ]
        |+> Static [
            "deafult" => T<string> 
            |> WithInline ""
            |> WithComment "The default value is text"
        ]

    let XMLHttpRequestEventTarget =
        let EH = Dom.Interfaces.Event ^-> T<unit>
        Class "XMLHttpRequestEventTarget"
        |=> Inherits Dom.Interfaces.EventTarget
        |+> Instance [
            "onloadstart" =@ EH
            "onprogress" =@ EH
            "onabort" =@ EH
            "onerror" =@ EH
            "onload" =@ EH
            "ontimeout" =@ EH
            "onloadend" =@ EH
        ]

    let XMLHttpRequestUpload =
        Class "XMLHttpRequestUpload"
        |=> Inherits XMLHttpRequestEventTarget

    let XMLHttpRequest =
        let EH = Dom.Interfaces.Event ^-> T<unit>
        Class "XMLHttpRequest"
        |=> Inherits XMLHttpRequestEventTarget
        |+> Static [
            Constructor T<unit>

            "UNSENT" =? T<int>
            "OPENED" =? T<int>
            "HEADERS_RECEIVED" =? T<int>
            "LOADING" =? T<int>
            "DONE" =? T<int>
        ]
        |+> Instance [
            "onreadystatechange" =@ EH
            "readyState" =? T<int>

            // request
            "open" => T<string>?``method`` * T<string>?url ^-> T<unit>
            "open" => T<string>?``method`` * T<string>?url * T<bool>?async * !?T<string>?username * !?T<string>?password ^-> T<unit>
            "setRequestHeader" => T<string>?name * T<string>?value ^-> T<unit>
            "timeout" =@ T<int>
            "withCredentials" =@ T<bool>
            "upload" =? XMLHttpRequestUpload
            "send" => !? (Dom.Interfaces.Document + T<string> + FormData + URLSearchParams + File.Blob + TypedArrays.ArrayBuffer)?body ^-> T<unit>
            "abort" => T<unit> ^-> T<unit>

            // response
            "responseURL" =? T<string>
            "status" =? T<int>
            "statusText" =? T<string>
            "getResponseHeader" => T<string> ^-> T<string>
            "getAllResponseHeaders" => T<unit> ^-> T<string>
            "overrideMimeType" => T<string>?mime ^-> T<unit>
            "responseType" =@ XMLHttpRequestResponseType
            "response" =? T<obj>
            "responseText" =? T<string>
            "responseXML" =? Dom.Interfaces.Document
        ]

    let URL =
        Class "URL"
        |+> Static [
            Constructor (T<string>?url * !?T<string>?``base``)
        ]
        |+> Instance [
            "hash" =@ T<string>
            "host" =@ T<string>
            "hostname" =@ T<string>
            "href" =@ T<string>
            "origin" =? T<string>
            "password" =@ T<string>
            "pathname" =@ T<string>
            "port" =@ T<string>
            "protocol" =@ T<string>
            "search" =@ T<string>
            "searchParams" =? URLSearchParams
            "username" =@ T<string>
        ]
        |+> Static [
            "createObjectURL" => (File.Blob + File.File) ^-> T<string>
            "revokeObjectURL" => T<string> ^-> T<unit>
        ]

    let Headers =
        Class "Headers"
        |+> Static [
            Constructor T<unit>
            Constructor TSelf
            Constructor EcmaObjectG.[T<string>]
        ]
        |+> Instance [
            "append" => T<string> * T<string> ^-> T<unit>
            "delete" => T<string> ^-> T<unit>
            "get" => T<string> ^-> T<string>
            "has" => T<string> ^-> T<bool>
            "set" => T<string> * T<string> ^-> T<unit>
            // TODO: entries iterator
            // TODO: keys iterator
            // TODO: values iterator
        ]

    let Mode =
        Pattern.EnumStrings "RequestMode" [
            "same-origin"
            "no-cors"
            "cors"
            "navigate"
        ]

    let Credentials =
        Pattern.EnumStrings "RequestCredentials" [
            "omit"
            "same-origin"
            "include"
        ]

    let Cache =
        Pattern.EnumStrings "RequestCache" [
            "default"
            "no-store"
            "reload"
            "no-cache"
            "force-cache"
            "only-if-cached"
        ]

    let Redirect =
        Pattern.EnumStrings "Redirect" [
            "follow"
            "error"
            "manual"
        ]

    let Referrer =
        Pattern.EnumStrings "Referrer" [
            "no-referrer"
            "client"
        ]
        |+> Static [
            "url" => T<string>?url ^-> TSelf
            |> WithInline "$url"
        ]

    let ReferrerPolicy =
        Pattern.EnumStrings "ReferrerPolicy" [
            "no-referrer"
            "no-referrer-when-downgrade"
            "origin"
            "origin-when-cross-origin"
            "unsafe-url"
        ]

    let RequestOptions =
        Pattern.Config "RequestOptions" {
            Required = []
            Optional =
                [
                    "method", T<string>
                    "headers", Headers.Type
                    "body", T<obj>
                    "mode", Mode.Type
                    "credentials", Credentials.Type
                    "cache", Cache.Type
                    "redirect", Redirect.Type
                    "referrer", Referrer.Type
                    "referrerPolicy", ReferrerPolicy.Type
                    "integrity", T<string>
                    "keepalive", T<bool>
                    "signal", Dom.Interfaces.AbortSignal.Type
                ]
        }

    let Body =
        Instance [
            "body" =? Streamable.ReadableStream
            "bodyUsed" =? T<bool>
            "arrayBuffer" => T<unit> ^-> EcmaPromise.[TypedArrays.ArrayBuffer]
            "blob" => T<unit> ^-> EcmaPromise.[File.Blob]
            "formData" => T<unit> ^-> EcmaPromise.[FormData]
            "json" => T<unit> ^-> EcmaPromise.[T<obj>]
            "text" => T<unit> ^-> EcmaPromise.[T<string>]
        ]

    let RequestDestination =
        Pattern.EnumStrings "RequestDestination" [
            "audio"
            "audioworklet"
            "document"
            "embed"
            "font"
            "image"
            "manifest"
            "object"
            "paintworklet"
            "report"
            "script"
            "serviceworker"
            "sharedworker"
            "style"
            "track"
            "video"
            "worker"
            "xslt"
        ]

    let Request =
        Class "Request"
        |+> Static [
            Constructor (T<string> * !? RequestOptions)
            Constructor TSelf
        ]
        |+> Instance [
            "cache" =? Cache
            "credentials" =? Credentials
            "destination" =? RequestDestination
            "headers" =? Headers
            "integrity" =? T<string>
            "method" =? T<string>
            "mode" =? Mode
            "redirect" =? Redirect
            "referrer" =? Referrer
            "referrerPolicy" =? ReferrerPolicy
            "url" =? T<string>
            "clone" => T<unit> ^-> TSelf
        ]
        |+> Body

    let ResponseOptions =
        Pattern.Config "ResponseOptions" {
            Required = []
            Optional =
                [
                    "status", T<int>
                    "statusText", T<string>
                    "headers", Headers.Type
                ]
        }

    let ResponseType =
        Pattern.EnumStrings "ResponseType" [
            "basic"
            "cors"
            "error"
            "opaque"
            "opaqueredirect"
        ]

    let Response =
        let bodyTypes = File.Blob + FormData + Streamable.ReadableStream + URLSearchParams + T<string>
        Class "Response"
        |+> Static [
            Constructor (!?bodyTypes * !?ResponseOptions)
            "error" => T<unit> ^-> TSelf
            "redirect" => T<string>?url * T<int>?status ^-> TSelf
        ]
        |+> Instance [
            "headers" =? Headers
            "ok" =? T<bool>
            "redirected" =? T<bool>
            "status" =? T<int>
            "statusText" =? T<string>
            "type" =? ResponseType
            "url" =? T<string>
            "useFinalURL" =? T<bool>
            "clone" => T<unit> ^-> TSelf
        ]
        |+> Body

module General = 
    let BarProp =
        let BarProp = Class "BarProp"
        BarProp
        |+> Instance [
            "visible" =@ T<bool>
        ]

    let ScrollRestoration =
        Pattern.EnumStrings "ScrollRestoration" [
            "auto"
            "manual"
        ]
    
    let History =
        let History = Class "History"
        History
        |+> Instance [
            "length" =? T<int>
            "state" =? T<obj>
            "scrollRestoration" =@ ScrollRestoration
            "go" => T<unit> ^-> T<unit>
            "go" => T<int> ^-> T<unit>
            "back" => T<unit> ^-> T<unit>
            "forward" => T<unit> ^-> T<unit>
            "pushState" => T<obj> * T<string> ^-> T<unit>
            "pushState" => T<obj> * T<string> * T<string> ^-> T<unit>
            "replaceState" => T<obj> * T<string> ^-> T<unit>
            "replaceState" => T<obj> * T<string> * T<string> ^-> T<unit>
        ]

    let UndoManager =
        Class "UndoManager"
        |+> Instance [
            "length" =? T<int>
            "position" =? T<int>
            "undo" => T<unit> ^-> T<unit>
            "redo" => T<unit> ^-> T<unit>
            "remove" => T<int> ^-> T<unit>
            "clearUndo" => T<unit> ^-> T<unit> 
            "clearRedo" => T<unit> ^-> T<unit> 
        ]

    let MessagePort = Class "MessagePort"

    let ErrorEvent =
        Class "ErrorEvent"
        |=> Inherits Dom.Interfaces.Event
        |+> Instance [
            "message" =? T<string>
            "filename" =? T<string>
            "lineno" =? T<int>
            "colno" =? T<int>
            "error" =? T<obj>
        ]

    let MessageEvent =
        Class "MessageEvent"
        |=> Inherits Dom.Interfaces.Event
        |+> Static [
            Constructor (T<string> * !?T<obj>)
        ]
        |+> Instance [
            "data" =? T<obj>
            "origin" =? T<string>
            "lastEventId" =? T<string>
            "source" =? Dom.Interfaces.Window
            "ports" =? Type.ArrayOf(MessagePort)
            "initMessageEvent" => T<string> * T<bool> * T<bool> * T<obj> * T<string> * T<string> * Dom.Interfaces.Window * Type.ArrayOf(MessagePort) ^-> T<unit>
                |> Obsolete
        ]

    do
        MessagePort
        |+> Instance [
            "postMessage" => T<obj> * Type.ArrayOf(MessagePort) ^-> T<unit>
            "start" => T<unit> ^-> T<unit>
            "close" => T<unit> ^-> T<unit>
            "onmessage" =@ MessageEvent ^-> T<unit>
        ]
        |> ignore

    let Navigator =
        Class "Navigator" 
        |+> Instance [
            "geolocation" =? Geolocation.Geolocation
            "mediaDevices" =? Media.MediaDevices
        ]

    let MQL =
        Class "MediaQueryList"
        |=> Inherits Dom.Interfaces.EventTarget
        |+> Instance [
            "matches" =? T<bool>
            "media" =? T<string>
            "onchange" => Dom.Interfaces.Event ^-> T<unit>
        ]

    let CSSSD =
        Class "CSSStyleDeclaration"
        |+> Instance [
            "cssText" =@ T<string>
            "length" =? T<int>
            "getPropertyPriority" => T<string> ^-> T<string>
            "getPropertyValue" => T<string> ^-> T<string>
            "item" => T<int> ^-> T<string>
            "removeProperty" => T<string> ^-> T<string>
            "setProperty" => T<string>?propertyName * !? T<string>?value * !? T<string>?priority ^-> T<unit>
        ]

    let WindowOrWorkerGlobalScope =
        Class "WindowOrWorkerGlobalScope"
        |=> Inherits Dom.Interfaces.EventTarget
        |+> Instance [
            // "caches" =? CacheStorage
            // "indexedDB" =? IDBFactory
            "isSecureContext" =? T<bool>
            "origin" =? T<string>

            "atob" => T<string> ^-> T<string>
            "btoa" => T<string> ^-> T<string>

            "setInterval" => (T<unit> ^-> T<unit>)?callback * T<int>?delay ^-> T<obj>
            "setTimeout" => (T<unit> ^-> T<unit>)?callback * !?T<int>?delay ^-> T<obj>
            "clearInterval" => T<obj>?intervalId ^-> T<unit>
            "clearTimeout" => T<obj>?timeoutId ^-> T<unit>

            "fetch" => T<string> * !?Fetch.RequestOptions ^-> EcmaPromise.[Fetch.Response]
            "fetch" => Fetch.Request ^-> EcmaPromise.[Fetch.Response]
        ]

    let ElementDefinitionOptions =
        Pattern.Config "ElementDefinitionOptions" {
            Required = [
                "extends", T<string>
            ]
            Optional = []
        }

    let CustomElementConstructor = T<unit> ^-> Dom.Interfaces.Element

    let CustomElementsRegistry =
        Class "CustomElementsRegistry"
        |+> Instance [
            "define" => T<string>?name * CustomElementConstructor?constructor * ElementDefinitionOptions?options ^-> T<unit>
            "get" => T<string>?name ^-> (T<unit> + CustomElementConstructor)
            "whenDefined" => T<string>?name ^-> (EcmaPromise.[CustomElementConstructor])
            "upgrade" => Dom.Interfaces.Node ^-> T<unit>
        ]

    do
        let f = Dom.Interfaces.Event ^-> T<unit>
        Dom.Interfaces.Window
        |=> Inherits WindowOrWorkerGlobalScope
        |+> Static [
            "self" =? Dom.Interfaces.Window
            |> WithGetterInline "window"
            |> ObsoleteWithMessage "Use JS.Window instead."
        ]
        |+> Dom.Interfaces.QuerySelectorMixin
        |+> Instance [
            "history" =? History
            "document" =? Dom.Interfaces.Document
            "name" =@ T<string>
            "location" =? Dom.Interfaces.Location
            "undoManager" =? UndoManager

            "locationbar" =? BarProp
            "menubar" =? BarProp
            "personalbar" =? BarProp
            "scrollbars" =? BarProp
            "statusbar" =? BarProp
            "toolbar" =? BarProp

            "getComputedStyle" => Dom.Interfaces.Element * !? T<string>?pseudo ^-> CSSSD

            "matchMedia" => T<string> ^-> MQL

            "innerHeight" =? T<int>
            "innerWidth" =? T<int>
            "outerHeight" =? T<int>
            "outerWidth" =? T<int>

            "scrollX" =? T<float>
            "scrollY" =? T<float>
            "scroll" => T<int>?x * T<int>?y ^-> T<unit>
            "scrollBy" => T<int>?x * T<int>?y ^-> T<unit>
            "scrollTo" => T<int>?x * T<int>?y ^-> T<unit>

            "screenX" =? T<int>
            "screenY" =? T<int>

            "stop" => T<unit> ^-> T<unit>

            "moveBy" => T<int>?x * T<int>?y ^-> T<unit>
            "moveTo" => T<int>?x * T<int>?y ^-> T<unit>
            "resizeBy" => T<int>?x * T<int>?y ^-> T<unit>
            "resizeTo" => T<int>?oWidth * T<int>?oHeight ^-> T<unit>

            "requestAnimationFrame" => (T<float -> unit>) ^-> T<int>

            "blur" => T<unit> ^-> T<unit>
                |> WithComment "Shifts focus away from the window"
            "focus" => T<unit> ^-> T<unit>
                |> WithComment "Makes a request to bring the window to the front"

            "frames" =? Dom.Interfaces.Window
            "length" =? T<int>
            "top" =? Dom.Interfaces.Window
            "opener" =? Dom.Interfaces.Window
            "parent" =? Dom.Interfaces.Window
            "frameElement" =? Dom.Interfaces.Element
            "open" => (T<string> * T<string> * T<string> * T<string>) ^->  Dom.Interfaces.Window
            "open" => (T<string> * T<string> * T<string>) ^->  Dom.Interfaces.Window
            "open" => (T<string> * T<string>) ^->  Dom.Interfaces.Window
            "open" => (T<string>) ^->  Dom.Interfaces.Window
            "open" => (T<unit>) ^->  Dom.Interfaces.Window
            "close" => T<unit> ^-> T<unit>

            "navigator" =? Navigator
            "crypto" =? T<obj>
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

            "customElements" =? CustomElementsRegistry

            "onabort" =@ f
            "onauxclick" =@ f
            "onafterprint" =@ f
            "onbeforeprint" =@ f
            "onbeforeunload" =@ f
            "onblur" =@ f
            "oncanplay" =@ f
            "oncancel" =@ f
            "oncanplaythrough" =@ f
            "onchange" =@ f
            "onclick" =@ f
            "oncontextmenu" =@ f

            "oncuechange" =@ f

            "ondblclick" =@ f
            "ondrag" =@ f
            "ondragend" =@ f
            "ondragexit" =@ f
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
            "onformchange" =@ f |> Obsolete
            "onforminput" =@ f |> Obsolete
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
            "onloadend" =@ f
            "onmessage" =@ f
            "onmousedown" =@ f
            "onmousemove" =@ f
            "onmouseout" =@ f
            "onmouseover" =@ f
            "onmouseup" =@ f
            "onmouseenter" =@ f
            "onmouseleave" =@ f
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
            "onrejectionhandled" =@ f
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
            "ontoggle" =@ f
            "onundo" =@ f
            "onunhandledrejection" =@ f
            "onunload" =@ f
            "onvolumechange" =@ f
            "onwaiting" =@ f

            "NaN" =? T<double> |> WithGetterInline "NaN"
            "Infinity" =? T<double> |> WithGetterInline "Infinity"
            "undefined" =? T<obj> |> WithGetterInline "undefined"
            "eval" => T<string>?expr ^-> T<obj> |> WithInline "eval($expr)"
            "parseInt" => T<string>?str * !?T<int>?radix ^-> T<int> |> WithInline "parseInt($str, $radix)"
            "parseFloat" => T<string>?str ^-> T<double> |> WithInline "parseFloat($str)"
            "isNaN" => T<obj>?number ^-> T<bool> |> WithInline "isNaN($number)"
            "isFinite" => (T<int> + T<float>)?number ^-> T<bool> |> WithInline "isFinite($number)"
            "decodeURI" => T<string>?str ^-> T<string> |> WithInline "decodeURI($str)"
            "decodeURIComponent" => T<string>?str ^-> T<string> |> WithInline "decodeURIComponent($str)"
            "encodeURI" => T<string>?str ^-> T<string> |> WithInline "encodeURI($str)"
            "encodeURIComponent" => T<string>?str ^-> T<string> |> WithInline "encodeURIComponent($str)"
        ]
        |> ignore

module WebWorkers =

    let WorkerType =
        Pattern.EnumStrings "WorkerType" ["classic"; "module"]

    let WorkerOptions =
        Pattern.Config "WorkerOptions" {
            Required = []
            Optional =
                [
                    "type", WorkerType.Type
                    "credentials", T<string>
                    "name", T<string>
                ]
        }

    let WorkerNavigator =
        Class "WorkerNavigator"
        |+> Instance [
            "hardwareConcurrency" =? T<int>
            "language" =? T<string>
            "languages" =? Type.ArrayOf T<string>
            "onLine" =? T<bool>
            "userAgent" =? T<string>
        ]

    let WorkerUtils =
        Class "WorkerUtils"
        |+> Static [
            "importScripts" => (!+ T<string>) ^-> T<unit>
            "navigator" =? WorkerNavigator
        ]

    let WorkerLocation =
        Class "WorkerLocation"
        |+> Instance [
            "href" =? T<string>
            "protocol" =? T<string>
            "host" =? T<string>
            "hostname" =? T<string>
            "origin" =? T<string>
            "port" =? T<string>
            "pathname" =? T<string>
            "search" =? T<string>
            "hash" =? T<string>
        ]

    let WorkerGlobalScope = 
        Class "WorkerGlobalScope"
        |=> Inherits General.WindowOrWorkerGlobalScope
        |+> Instance [
            "navigator" =? WorkerNavigator
            "self" =? TSelf
            "location" =? WorkerLocation
            "onerror" =? General.ErrorEvent ^-> T<unit>
            "onoffline" =? Dom.Interfaces.Event ^-> T<unit>
            "ononline" =? Dom.Interfaces.Event ^-> T<unit>
            "onlanguagechange" =? Dom.Interfaces.Event ^-> T<unit>
            "importScripts" => !+T<string> ^-> T<unit>
        ]

    let DedicatedWorkerGlobalScope =
        Class "DedicatedWorkerGlobalScope"
        |=> Inherits WorkerGlobalScope
        |+> Instance [
            "onmessage" =@ General.MessageEvent ^-> T<unit>
            "onmessageerror" =@ General.MessageEvent ^-> T<unit>
            "close" => T<unit> ^-> T<unit>
            "postMessage" => !?T<obj>?message * !?T<obj[]>?transferList ^-> T<unit>
        ]

    let SharedWorkerGlobalScope =
        Class "SharedWorkerGlobalScope"
        |=> Inherits WorkerGlobalScope
        |+> Instance [
            "name" =? T<string>
            "applicationCache" =? AppCache.ApplicationCache
            "onconnect" =@ Dom.Interfaces.Event ^-> T<unit>
            "close" => T<unit> ^-> T<unit>
        ]

    let AbstractWorker =
        Interface "AbstractWorker"
        |+> [
            "onerror" =@ General.ErrorEvent ^-> T<unit>
        ]

    let Worker =
        Class "Worker"
        |=> Inherits Dom.Interfaces.EventTarget
        |=> Implements [AbstractWorker]
        |+> Static [
            Constructor (T<string>?url * !?WorkerOptions)
            Constructor (DedicatedWorkerGlobalScope ^-> T<unit>)
            |> WithMacro typeof<WebSharper.Core.Macros.WebWorker>
            |> WithComment "Create a Web Worker with the given expression as entry point. \
                A bundled JavaScript file named <assemblyname>.worker.js is automatically compiled for it."
            Constructor (T<string>?name * (DedicatedWorkerGlobalScope ^-> T<unit>))
            |> WithMacro typeof<WebSharper.Core.Macros.WebWorker>
            |> WithComment "Create a Web Worker with the given expression as entry point. \
                A bundled JavaScript file named <assemblyname>.<name>.js is automatically compiled for it."
            Constructor (T<string>?name * T<bool>?includeJsExports * (DedicatedWorkerGlobalScope ^-> T<unit>))
            |> WithMacro typeof<WebSharper.Core.Macros.WebWorker>
            |> WithComment "Create a Web Worker with the given expression as entry point. \
                A bundled JavaScript file named <assemblyname>.<name>.js is automatically compiled for it.
                If includeJsExports is true, values marked [JavaScriptExport] are included in the bundle."
        ]
        |+> Instance [
            "onmessage" =@ General.MessageEvent ^-> T<unit>
            "onmessageerror" =@ General.MessageEvent ^-> T<unit>
            "postMessage" => !?T<obj>?message * !?T<obj[]>?transferList ^-> T<unit>
            "terminate" => T<unit> ^-> T<unit>
        ]

    let SharedWorker =
        Class "SharedWorker"
        |=> Inherits Dom.Interfaces.EventTarget
        |=> Implements [AbstractWorker]
        |+> Static [
            Constructor (T<string>?url * !?WorkerOptions)
        ]
        |+> Instance [
            "port" =? General.MessagePort
        ]

module WebGL =

    let RenderingContext = Class "RenderingContext"
    let ContextAttributes = Class "ContextAttributes"
    let Object = Interface "Object"
    let Buffer = Interface "Buffer"
    let Framebuffer = Interface "Framebuffer"
    let Program = Interface "Program"
    let Renderbuffer = Interface "Renderbuffer"
    let Shader = Interface "Shader"
    let Texture = Interface "Texture"
    let UniformLocation = Interface "UniformLocation"
    let ActiveInfo = Class "ActiveInfo"
    let Enum = Class "Enum"
    let DataView = Class "DataView"

    let EnumClass =
        Enum

    let DataViewClass =
        DataView
        |=> Inherits TypedArrays.ArrayBufferView
        |+> Static [
                Constructor (TypedArrays.ArrayBuffer * T<int> * T<int>)
                Constructor (TypedArrays.ArrayBuffer * T<int>)
                Constructor TypedArrays.ArrayBuffer
            ]
        |+> Instance
            [
                "buffer" =? TypedArrays.ArrayBuffer
                "byteOffset" =? T<uint64>
                "byteLength" =? T<uint64>
                "getInt8" => T<int> ^-> T<int>
                "getUint8" => T<int> ^-> T<int>
                "getInt16" => T<int> * T<bool> ^-> T<int>
                "getInt16" => T<int> ^-> T<int>
                "getUint16" => T<int> * T<bool> ^-> T<int>
                "getUint16" => T<int> ^-> T<int>
                "getInt32" => T<int> * T<bool> ^-> T<int>
                "getInt32" => T<int> ^-> T<int>
                "getUint32" => T<int> * T<bool> ^-> T<int>
                "getUint32" => T<int> ^-> T<int>
                "getFloat32" => T<int> * T<bool> ^-> T<int>
                "getFloat32" => T<int> ^-> T<int>
                "getFloat64" => T<int> * T<bool> ^-> T<int>
                "getFloat64" => T<int> ^-> T<int>
                "setInt8" => T<int> * T<int> ^-> T<int>
                "setUint8" => T<int> * T<int> ^-> T<int>
                "setInt16" => T<int> * T<int> * T<bool> ^-> T<int>
                "setInt16" => T<int> * T<int> ^-> T<int>
                "setUint16" => T<int> * T<int> * T<bool> ^-> T<int>
                "setUint16" => T<int> * T<int> ^-> T<int>
                "setInt32" => T<int> * T<int> * T<bool> ^-> T<int>
                "setInt32" => T<int> * T<int> ^-> T<int>
                "setUint32" => T<int> * T<int> * T<bool> ^-> T<int>
                "setUint32" => T<int> * T<int> ^-> T<int>
                "setFloat32" => T<int> * T<int> * T<bool> ^-> T<int>
                "setFloat32" => T<int> * T<int> ^-> T<int>
                "setFloat64" => T<int> * T<float> * T<bool> ^-> T<int>
                "setFloat64" => T<int> * T<float> ^-> T<int>
            ]

    let RenderingContextClass =
        RenderingContext
        |+> Instance
            (List.ofArray [|
                // GLEnum constants
                "DEPTH_BUFFER_BIT" =? T<int>
                "STENCIL_BUFFER_BIT" =? T<int>
                "COLOR_BUFFER_BIT" =? T<int>
                "POINTS" =? Enum
                "LINES" =? Enum
                "LINE_LOOP" =? Enum
                "LINE_STRIP" =? Enum
                "TRIANGLES" =? Enum
                "TRIANGLE_STRIP" =? Enum
                "TRIANGLE_FAN" =? Enum
                "ZERO" =? Enum
                "ONE" =? Enum
                "SRC_COLOR" =? Enum
                "ONE_MINUS_SRC_COLOR" =? Enum
                "SRC_ALPHA" =? Enum
                "ONE_MINUS_SRC_ALPHA" =? Enum
                "DST_ALPHA" =? Enum
                "ONE_MINUS_DST_ALPHA" =? Enum
                "DST_COLOR" =? Enum
                "ONE_MINUS_DST_COLOR" =? Enum
                "SRC_ALPHA_SATURATE" =? Enum
                "FUNC_ADD" =? Enum
                "BLEND_EQUATION" =? Enum
                "BLEND_EQUATION_RGB" =? Enum
                "BLEND_EQUATION_ALPHA" =? Enum
                "FUNC_SUBSTRACT" =? Enum
                "FUNC_REVERSE_SUBSTRACT" =? Enum
                "BLEND_DST_RGB" =? Enum
                "BLEND_SRC_RGB" =? Enum
                "BLEND_DST_ALPHA" =? Enum
                "BLEND_SRC_ALPHA" =? Enum
                "CONSTANT_COLOR" =? Enum
                "ONE_MINUS_CONSTANT_COLOR" =? Enum
                "CONSTANT_ALPHA" =? Enum
                "ONE_MINUS_CONSTANT_ALPHA" =? Enum
                "BLEND_COLOR" =? Enum
                "ARRAY_BUFFER" =? Enum
                "ELEMENT_ARRAY_BUFFER" =? Enum
                "ARRAY_BUFFER_BINDING" =? Enum
                "ELEMENT_ARRAY_BUFFER_BINDING" =? Enum
                "STREAM_DRAW" =? Enum
                "STATIC_DRAW" =? Enum
                "DYNAMIC_DRAW" =? Enum
                "BUFFER_SIZE" =? Enum
                "BUFFER_USAGE" =? Enum
                "CURRENT_VERTEX_ATTRIB" =? Enum
                "FRONT" =? Enum
                "BACK" =? Enum
                "FRONT_AND_BACK" =? Enum
                "CULL_FACE" =? Enum
                "BLEND" =? Enum
                "DITHER" =? Enum
                "STENCIL_TEST" =? Enum
                "DEPTH_TEST" =? Enum
                "SCISSOR_TEST" =? Enum
                "POLYGON_OFFSET_FILL" =? Enum
                "SAMPLE_ALPHA_TO_COVERAGE" =? Enum
                "SAMPLE_COVERAGE" =? Enum
                "NO_ERROR" =? Enum
                "INVALID_ENUM" =? Enum
                "INVALID_VALUE" =? Enum
                "INVALID_OPERATION" =? Enum
                "OUT_OF_MEMORY" =? Enum
                "CW" =? Enum
                "CCW" =? Enum
                "LINE_WIDTH" =? Enum
                "ALIASED_POINT_SIZE_RANGE" =? Enum
                "ALIASED_LINE_WIDTH_RANGE" =? Enum
                "CULL_FACE_MODE" =? Enum
                "FRONT_FACE" =? Enum
                "DEPTH_RANGE" =? Enum
                "DEPTH_WRITEMASK" =? Enum
                "DEPTH_CLEAR_VALUE" =? Enum
                "DEPTH_FUNC" =? Enum
                "STENCIL_CLEAR_VALUE" =? Enum
                "STENCIL_FUNC" =? Enum
                "STENCIL_FAIL" =? Enum
                "STENCIL_PASS_DEPTH_FAIL" =? Enum
                "STENCIL_PASS_DEPTH_PASS" =? Enum
                "STENCIL_REF" =? Enum
                "STENCIL_VALUE_MASK" =? Enum
                "STENCIL_WRITEMASK" =? Enum
                "STENCIL_BACK_FUNC" =? Enum
                "STENCIL_BACK_FAIL" =? Enum
                "STENCIL_BACK_PASS_DEPTH_FAIL" =? Enum
                "STENCIL_BACK_PASS_DEPTH_PASS" =? Enum
                "STENCIL_BACK_REF" =? Enum
                "STENCIL_BACK_VALUE_MASK" =? Enum
                "STENCIL_BACK_WRITEMASK" =? Enum
                "VIEWPORT" =? Enum
                "SCISSOR_BOX" =? Enum
                "COLOR_CLEAR_VALUE" =? Enum
                "COLOR_WRITEMASK" =? Enum
                "UNPACK_ALIGNMENT" =? Enum
                "PACK_ALIGNMENT" =? Enum
                "MAX_TEXTURE_SIZE" =? Enum
                "MAX_VIEWPORT_DIMS" =? Enum
                "SUBPIXEL_BITS" =? Enum
                "RED_BITS" =? Enum
                "GREEN_BITS" =? Enum
                "BLUE_BITS" =? Enum
                "ALPHA_BITS" =? Enum
                "DEPTH_BITS" =? Enum
                "STENCIL_BITS" =? Enum
                "POLYGON_OFFSET_UNITS" =? Enum
                "POLYGON_OFFSET_FACTOR" =? Enum
                "TEXTURE_BINDING_2D" =? Enum
                "SAMPLE_BUFFERS" =? Enum
                "SAMPLES" =? Enum
                "SAMPLE_COVERAGE_VALUE" =? Enum
                "SAMPLE_COVERAGE_INVERT" =? Enum
                "NUM_COMPRESSED_TEXTURE_FORMATS" =? Enum
                "COMPRESSED_TEXTURE_FORMATS" =? Enum
                "DONT_CARE" =? Enum
                "FASTEST" =? Enum
                "NICEST" =? Enum
                "GENERATE_MIPMAP_HINT" =? Enum
                "BYTE" =? Enum
                "UNSIGNED_BYTE" =? Enum
                "SHORT" =? Enum
                "UNSIGNED_SHORT" =? Enum
                "INT" =? Enum
                "UNSIGNED_INT" =? Enum
                "FLOAT" =? Enum
                "DEPTH_COMPONENT" =? Enum
                "ALPHA" =? Enum
                "RGB" =? Enum
                "RGBA" =? Enum
                "LUMINANCE" =? Enum
                "LUMINANCE_ALPHA" =? Enum
                "UNSIGNED_SHORT_4_4_4_4" =? Enum
                "UNSIGNED_SHORT_5_5_5_1" =? Enum
                "UNSIGNED_SHORT_5_6_5" =? Enum
                "FRAGMENT_SHADER" =? Enum
                "VERTEX_SHADER" =? Enum
                "MAX_VERTEX_ATTRIBS" =? Enum
                "MAX_VERTEX_UNIFORM_VECTORS" =? Enum
                "MAX_VARYING_VECTORS" =? Enum
                "MAX_COMBINED_TEXTURE_IMAGE_UNITS" =? Enum
                "MAX_VERTEX_TEXTURE_IMAGE_UNITS" =? Enum
                "MAX_TEXTURE_IMAGE_UNITS" =? Enum
                "MAX_FRAGMENT_UNIFORM_VECTORS" =? Enum
                "SHADER_TYPE" =? Enum
                "DELETE_STATUS" =? Enum
                "LINK_STATUS" =? Enum
                "VALIDATE_STATUS" =? Enum
                "ATTACHED_SHADERS" =? Enum
                "ACTIVE_UNIFORMS" =? Enum
                "ACTIVE_ATTRIBUTES" =? Enum
                "SHADING_LANGUAGE_VERSION" =? Enum
                "CURRENT_PROGRAM" =? Enum
                "NEVER" =? Enum
                "LESS" =? Enum
                "EQUAL" =? Enum
                "LEQUAL" =? Enum
                "GREATER" =? Enum
                "NOTEQUAL" =? Enum
                "GEQUAL" =? Enum
                "ALWAYS" =? Enum
                "KEEP" =? Enum
                "REPLACE" =? Enum
                "INCR" =? Enum
                "DECR" =? Enum
                "INVERT" =? Enum
                "INCR_WRAP" =? Enum
                "DECR_WRAP" =? Enum
                "VENDOR" =? Enum
                "RENDERER" =? Enum
                "VERSION" =? Enum
                "NEAREST" =? Enum
                "LINEAR" =? Enum
                "NEAREST_MIPMAP_NEAREST" =? Enum
                "LINEAR_MIPMAP_NEAREST" =? Enum
                "NEAREST_MIPMAP_LINEAR" =? Enum
                "LINEAR_MIPMAP_LINEAR" =? Enum
                "TEXTURE_MAG_FILTER" =? Enum
                "TEXTURE_MIN_FILTER" =? Enum
                "TEXTURE_WRAP_S" =? Enum
                "TEXTURE_WRAP_T" =? Enum
                "TEXTURE_2D" =? Enum
                "TEXTURE" =? Enum
                "TEXTURE_CUBE_MAP" =? Enum
                "TEXTURE_BINDING_CUBE_MAP" =? Enum
                "TEXTURE_CUBE_MAP_POSITIVE_X" =? Enum
                "TEXTURE_CUBE_MAP_NEGATIVE_X" =? Enum
                "TEXTURE_CUBE_MAP_POSITIVE_Y" =? Enum
                "TEXTURE_CUBE_MAP_NEGATIVE_Y" =? Enum
                "TEXTURE_CUBE_MAP_POSITIVE_Z" =? Enum
                "TEXTURE_CUBE_MAP_NEGATIVE_Z" =? Enum
                "MAX_CUBE_MAP_TEXTURE_SIZE" =? Enum
                "TEXTURE0" =? Enum
                "TEXTURE1" =? Enum
                "TEXTURE2" =? Enum
                "TEXTURE3" =? Enum
                "TEXTURE4" =? Enum
                "TEXTURE5" =? Enum
                "TEXTURE6" =? Enum
                "TEXTURE7" =? Enum
                "TEXTURE8" =? Enum
                "TEXTURE9" =? Enum
                "TEXTURE10" =? Enum
                "TEXTURE11" =? Enum
                "TEXTURE12" =? Enum
                "TEXTURE13" =? Enum
                "TEXTURE14" =? Enum
                "TEXTURE15" =? Enum
                "TEXTURE16" =? Enum
                "TEXTURE17" =? Enum
                "TEXTURE18" =? Enum
                "TEXTURE19" =? Enum
                "TEXTURE20" =? Enum
                "TEXTURE21" =? Enum
                "TEXTURE22" =? Enum
                "TEXTURE23" =? Enum
                "TEXTURE24" =? Enum
                "TEXTURE25" =? Enum
                "TEXTURE26" =? Enum
                "TEXTURE27" =? Enum
                "TEXTURE28" =? Enum
                "TEXTURE29" =? Enum
                "TEXTURE30" =? Enum
                "TEXTURE31" =? Enum
                "ACTIVE_TEXTURE" =? Enum
                "REPEAT" =? Enum
                "CLAMP_TO_EDGE" =? Enum
                "MIRRORED_REPEAT" =? Enum
                "FLOAT_VEC2" =? Enum
                "FLOAT_VEC3" =? Enum
                "FLOAT_VEC4" =? Enum
                "INT_VEC2" =? Enum
                "INT_VEC3" =? Enum
                "INT_VEC4" =? Enum
                "BOOL" =? Enum
                "BOOL_VEC2" =? Enum
                "BOOL_VEC3" =? Enum
                "BOOL_VEC4" =? Enum
                "FLOAT_MAT2" =? Enum
                "FLOAT_MAT3" =? Enum
                "FLOAT_MAT4" =? Enum
                "SAMPLER_2D" =? Enum
                "SAMPLER_CUBE" =? Enum
                "VERTEX_ATTRIB_ARRAY_ENABLED" =? Enum
                "VERTEX_ATTRIB_ARRAY_SIZE" =? Enum
                "VERTEX_ATTRIB_ARRAY_STRIDE" =? Enum
                "VERTEX_ATTRIB_ARRAY_TYPE" =? Enum
                "VERTEX_ATTRIB_ARRAY_NORMALIZED" =? Enum
                "VERTEX_ATTRIB_ARRAY_POINTER" =? Enum
                "VERTEX_ATTRIB_ARRAY_BUFFER_BINDING" =? Enum
                "COMPILE_STATUS" =? Enum
                "LOW_FLOAT" =? Enum
                "MEDIUM_FLOAT" =? Enum
                "HIGH_FLOAT" =? Enum
                "LOW_INT" =? Enum
                "MEDIUM_INT" =? Enum
                "HIGH_INT" =? Enum
                "FRAMEBUFFER" =? Enum
                "RENDERBUFFER" =? Enum
                "RGBA4" =? Enum
                "RGB5_A1" =? Enum
                "RGB565" =? Enum
                "DEPTH_COMPONENT16" =? Enum
                "STENCIL_INDEX" =? Enum
                "STENCIL_INDEX8" =? Enum
                "DEPTH_STENCIL" =? Enum
                "RENDERBUFFER_WIDTH" =? Enum
                "RENDERBUFFER_HEIGHT" =? Enum
                "RENDERBUFFER_INTERNAL_FORMAT" =? Enum
                "RENDERBUFFER_RED_SIZE" =? Enum
                "RENDERBUFFER_GREEN_SIZE" =? Enum
                "RENDERBUFFER_BLUE_SIZE" =? Enum
                "RENDERBUFFER_ALPHA_SIZE" =? Enum
                "RENDERBUFFER_DEPTH_SIZE" =? Enum
                "RENDERBUFFER_STENCIL_SIZE" =? Enum
                "FRAMEBUFFER_ATTACHMENT_OBJECT_TYPE" =? Enum
                "FRAMEBUFFER_ATTACHMENT_OBJECT_NAME" =? Enum
                "FRAMEBUFFER_ATTACHMENT_TEXTURE_LEVEL" =? Enum
                "FRAMEBUFFER_ATTACHMENT_TEXTURE_CUBE_MAP_FACE" =? Enum
                "COLOR_ATTACHMENT0" =? Enum
                "DEPTH_ATTACHMENT" =? Enum
                "STENCIL_ATTACHMENT" =? Enum
                "DEPTH_STENCIL_ATTACHMENT" =? Enum
                "NONE" =? Enum
                "FRAMEBUFFER_COMPLETE" =? Enum
                "FRAMEBUFFER_INCOMPLETE_ATTACHMENT" =? Enum
                "FRAMEBUFFER_INCOMPLETE_MISSING_ATTACHMENT" =? Enum
                "FRAMEBUFFER_INCOMPLETE_DIMENSIONS" =? Enum
                "FRAMEBUFFER_UNSUPPORTED" =? Enum
                "FRAMEBUFFER_BINDING" =? Enum
                "RENDERBUFFER_BINDING" =? Enum
                "MAX_RENDERBUFFER_SIZE" =? Enum
                "INVALID_FRAMEBUFFER_OPERATION" =? Enum

                // WebGL-specific GLEnums
                "UNPACK_FLIP_Y_WEBGL" =? Enum
                "UNPACK_PREMATURELY_ALPHA_WEBGL" =? Enum
                "CONTEXT_LOST_WEBGL" =? Enum
                "UNPACK_COLORSPACE_CONVERSION_WEBGL" =? Enum
                "BROWSER_DEFAULT_WEBGL" =? Enum

                "canvas" =? Dom.Interfaces.Element
                "drawingBufferWidth" =? T<int>
                "drawingBufferHeight" =? T<int>

                "getContextAttributes" => T<unit> ^-> ContextAttributes
                "isContextLost" => T<unit> ^-> T<bool>
                "getSupportedExtensions" => T<unit> ^-> (Type.ArrayOf T<string>)
                "getExtension" => T<string>?name ^-> T<obj>
                "activeTexture" => Enum?texture ^-> T<unit>
                "attachShader" => Program?program * Shader?shader ^-> T<unit>
                "bindAttribLocation" => Program?program * T<int>?index * T<string>?name ^-> T<unit>
                "bindBuffer" => Enum?target * Buffer?buffer ^-> T<unit>
                "bindFramebuffer" => Enum?target * Framebuffer?framebuffer ^-> T<unit>
                "bindRenderbuffer" => Enum?target * Renderbuffer?renderbuffer ^-> T<unit>
                "bindTexture" => Enum?target * Texture?texture ^-> T<unit>
                "blendColor" => T<float>?red * T<float>?green * T<float>?blue * T<float>?alpha ^-> T<unit>
                "blendEquation" => Enum?mode ^-> T<unit>
                "blendEquationSeparate" => Enum?modeRGB * Enum?modeAlpha ^-> T<unit>
                "blendFunc" => Enum?sfactor * Enum?dfactor ^-> T<unit>
                "blendFuncSeparate" => Enum?srcRGB * Enum?dstRGB * Enum?srcAlpha * Enum?dstAlpha ^-> T<unit>
                "bufferData" => Enum?target * T<int>?size * Enum?usage ^-> T<unit>
                "bufferData" => Enum?target * TypedArrays.ArrayBufferView?data * Enum?usage ^-> T<unit>
                "bufferData" => Enum?target * TypedArrays.ArrayBuffer?data * Enum?usage ^-> T<unit>
                "bufferSubData" => Enum?target * T<int>?offset * TypedArrays.ArrayBufferView?data ^-> T<unit>
                "bufferSubData" => Enum?target * T<int>?offset * TypedArrays.ArrayBuffer?data ^-> T<unit>
                "checkFramebufferStatus" => Enum?target ^-> Enum
                "clear" => T<int>?mask ^-> T<unit>
                "clearColor" => T<float>?red * T<float>?green * T<float>?blue * T<float>?alpha ^-> T<unit>
                "clearDepth" => T<float>?depth ^-> T<unit>
                "clearStencil" => T<int>?s ^-> T<unit>
                "colorMask" => T<bool>?red * T<bool>?green * T<bool>?blue * T<bool>?alpha ^-> T<unit>
                "compileShader" => Shader?shader ^-> T<unit>
                "copyTexImage2D" => Enum?target * T<int>?level * Enum?internalformat * T<int>?x * T<int>?y * T<int>?width * T<int>?height * T<int>?border ^-> T<unit>
                "copyTexSubImage2D" => Enum?target * T<int>?level * T<int>?xoffset * T<int>?yoffset * T<int>?x * T<int>?y * T<int>?width * T<int>?height ^-> T<unit>
                "createBuffer" => T<unit> ^-> Buffer
                "createFramebuffer" => T<unit> ^-> Framebuffer
                "createProgram" => T<unit> ^-> Program
                "createRenderbuffer" => T<unit> ^-> Renderbuffer
                "createShader" => Enum?typ ^-> Shader
                "createTexture" => T<unit> ^-> Texture
                "cullFace" => Enum?mode ^-> T<unit>
                "deleteBuffer" => Buffer?buffer ^-> T<unit>
                "deleteFramebuffer" => Framebuffer?framebuffer ^-> T<unit>
                "deleteProgram" => Program?program ^-> T<unit>
                "deleteRenderbuffer" => Renderbuffer?renderbuffer ^-> T<unit>
                "deleteShader" => Shader?shader ^-> T<unit>
                "deleteTexture" => Texture?texture ^-> T<unit>
                "depthFunc" => Enum?func ^-> T<unit>
                "depthMask" => T<bool>?flag ^-> T<unit>
                "depthRange" => T<float>?zNear * T<float>?zFar ^-> T<unit>
                "detachShader" => Program?program * Shader?shader ^-> T<unit>
                "disable" => Enum?cap ^-> T<unit>
                "disableVertexAttribArray" => T<int>?index ^-> T<unit>
                "drawArrays" => Enum?mode * T<int>?first * T<int>?count ^-> T<unit>
                "drawElements" => Enum?mode * T<int>?count * Enum?typ * T<int>?offset ^-> T<unit>
                "enable" => Enum?cap ^-> T<unit>
                "enableVertexAttribArray" => T<int>?index ^-> T<unit>
                "finish" => T<unit> ^-> T<unit>
                "flush" => T<unit> ^-> T<unit>
                "framebufferRenderbuffer" => Enum?target * Enum?attachment * Enum?renderbuffertarget * Renderbuffer?renderbuffer ^-> T<unit>
                "framebufferTexture2D" => Enum?target * Enum?attachment * Enum?textarget * Texture?texture * T<int>?level ^-> T<unit>
                "frontFace" => Enum?mode ^-> T<unit>
                "generateMipmap" => Enum?target ^-> T<unit>
                "getActiveAttrib" => Program?program * T<int>?index ^-> ActiveInfo
                "getActiveUniform" => Program?program * T<int>?index ^-> ActiveInfo
                "getAttachedShaders" => Program?program ^-> (Type.ArrayOf Shader)
                "getAttribLocation" => Program?program * T<string>?name ^-> T<int>
                "getParameter" => Enum?pname ^-> T<obj>
                "getBufferParameter" => Enum?target * Enum?pname ^-> T<obj>
                "getError" => T<unit> ^-> Enum
                "getFramebufferAttachmentParameter" => Enum?target * Enum?attachment * Enum?pname ^-> T<obj>
                "getProgramParameter" => Program?program * Enum?pname ^-> T<obj>
                "getProgramInfoLog" => Program?program ^-> T<string>
                "getRenderbufferParameter" => Enum?target * Enum?pname ^-> T<obj>
                "getShaderParameter" => Shader?shader * Enum?pname ^-> T<obj>
                "getShaderInfoLog" => Shader?shader ^-> T<string>
                "getShaderSource" => Shader?shader ^-> T<string>
                "getTexParameter" => Enum?target * Enum?pname ^-> T<obj>
                "getUniform" => Program?program * UniformLocation?location ^-> T<obj>
                "getUniformLocation" => Program?program * T<string>?name ^-> UniformLocation
                "getVertexAttrib" => T<int>?index * Enum?pname ^-> T<obj>
                "getVertexAttribOffset" => T<int>?index * Enum?pname ^-> T<int>
                "hint" => Enum?target * Enum?mode ^-> T<unit>
                "isBuffer" => Buffer?buffer ^-> T<bool>
                "isEnabled" => Enum?cap ^-> T<bool>
                "isFramebuffer" => Framebuffer?framebuffer ^-> T<bool>
                "isProgram" => Program?program ^-> T<bool>
                "isRenderbuffer" => Renderbuffer?renderbuffer ^-> T<bool>
                "isShader" => Shader?shader ^-> T<bool>
                "isTexture" => Texture?texture ^-> T<bool>
                "lineWidth" => T<float>?width ^-> T<unit>
                "linkProgram" => Program?program ^-> T<unit>
                "pixelStorei" => Enum?pname * T<int>?param ^-> T<unit>
                "polygonOffset" => T<float>?factor * T<float>?units ^-> T<unit>
                "readPixels" => T<int>?x * T<int>?y * T<int>?width * T<int>?height * Enum?format * Enum?typ * TypedArrays.ArrayBufferView?pixels ^-> T<unit>
                "renderbufferStorage" => Enum?target * Enum?internalformat * T<int>?width * T<int>?height ^-> T<unit>
                "sampleCoverage" => T<float>?value * T<bool>?invert ^-> T<unit>
                "scissor" => T<int>?x * T<int>?y * T<int>?width * T<int>?height ^-> T<unit>
                "shaderSource" => Shader?shader * T<string>?source ^-> T<unit>
                "stencilFunc" => Enum?func * T<int>?ref * T<int>?mask ^-> T<unit>
                "stencilFuncSeparate" => Enum?face * Enum?func * T<int>?ref * T<int>?mask ^-> T<unit>
                "stencilMask" => T<int>?mask ^-> T<unit>
                "stencilMaskSeparate" => Enum?face * T<int>?mask ^-> T<unit>
                "stencilOp" => Enum?fail * Enum?zfail * Enum?zpass ^-> T<unit>
                "stencilOpSeparate" => Enum?face * Enum?fail * Enum?zfail * Enum?zpass ^-> T<unit>
                "texImage2D" => Enum?target * T<int>?level * Enum?internalformat * T<int>?width * T<int>?height * T<int>?border * Enum?format * Enum?typ * TypedArrays.ArrayBufferView?pixels ^-> T<unit>
                "texImage2D" => Enum?target * T<int>?level * Enum?internalformat * Enum?format * Enum?typ * Canvas.ImageData?pixels ^-> T<unit>
                "texImage2D" => Enum?target * T<int>?level * Enum?internalformat * Enum?format * Enum?typ * Dom.Interfaces.Element?image ^-> T<unit>
                "texParameterf" => Enum?target * Enum?pname * T<float>?param ^-> T<unit>
                "texParameteri" => Enum?target * Enum?pname * T<int>?param ^-> T<unit>
                "texParameteri" => Enum?target * Enum?pname * Enum?param ^-> T<unit>
                "texSubImage2D" => Enum?target * T<int>?level * T<int>?xoffset * T<int>?yoffset * T<int>?width * T<int>?height * Enum?format * Enum?typ * TypedArrays.ArrayBufferView?pixels ^-> T<unit>
                "texSubImage2D" => Enum?target * T<int>?level * T<int>?xoffset * T<int>?yoffset * Enum?format * Enum?typ * Canvas.ImageData?pixels ^-> T<unit>
                "texSubImage2D" => Enum?target * T<int>?level * T<int>?xoffset * T<int>?yoffset * Enum?format * Enum?typ * Dom.Interfaces.Element?image ^-> T<unit>
                "uniform1f" => UniformLocation?location * T<float>?x ^-> T<unit>
                "uniform1fv" => UniformLocation?location * TypedArrays.Float32Array?v ^-> T<unit>
                "uniform1fv" => UniformLocation?location * (Type.ArrayOf T<float>)?v ^-> T<unit>
                "uniform1i" => UniformLocation?location * T<int>?x ^-> T<unit>
                "uniform1iv" => UniformLocation?location * TypedArrays.Int32Array?v ^-> T<unit>
                "uniform1iv" => UniformLocation?location * (Type.ArrayOf T<int>)?v ^-> T<unit>
                "uniform2f" => UniformLocation?location * T<float>?x * T<float>?y ^-> T<unit>
                "uniform2fv" => UniformLocation?location * TypedArrays.Float32Array?v ^-> T<unit>
                "uniform2fv" => UniformLocation?location * (Type.ArrayOf T<float>)?v ^-> T<unit>
                "uniform2i" => UniformLocation?location * T<int>?x * T<int>?y ^-> T<unit>
                "uniform2iv" => UniformLocation?location * TypedArrays.Int32Array?v ^-> T<unit>
                "uniform2iv" => UniformLocation?location * (Type.ArrayOf T<int>)?v ^-> T<unit>
                "uniform3f" => UniformLocation?location * T<float>?x * T<float>?y * T<float>?z ^-> T<unit>
                "uniform3fv" => UniformLocation?location * TypedArrays.Float32Array?v ^-> T<unit>
                "uniform3fv" => UniformLocation?location * (Type.ArrayOf T<float>)?v ^-> T<unit>
                "uniform3i" => UniformLocation?location * T<int>?x * T<int>?y * T<int>?z ^-> T<unit>
                "uniform3iv" => UniformLocation?location * TypedArrays.Int32Array?v ^-> T<unit>
                "uniform3iv" => UniformLocation?location * (Type.ArrayOf T<int>)?v ^-> T<unit>
                "uniform4f" => UniformLocation?location * T<float>?x * T<float>?y * T<float>?z * T<float>?w ^-> T<unit>
                "uniform4fv" => UniformLocation?location * TypedArrays.Float32Array?v ^-> T<unit>
                "uniform4fv" => UniformLocation?location * (Type.ArrayOf T<float>)?v ^-> T<unit>
                "uniform4i" => UniformLocation?location * T<int>?x * T<int>?y * T<int>?z * T<int>?w ^-> T<unit>
                "uniform4iv" => UniformLocation?location * TypedArrays.Int32Array?v ^-> T<unit>
                "uniform4iv" => UniformLocation?location * (Type.ArrayOf T<int>)?v ^-> T<unit>
                "uniformMatrix2fv" => UniformLocation?location * T<bool>?transpose * TypedArrays.Float32Array?value ^-> T<unit>
                "uniformMatrix2fv" => UniformLocation?location * T<bool>?transpose * (Type.ArrayOf T<float>)?value ^-> T<unit>
                "uniformMatrix3fv" => UniformLocation?location * T<bool>?transpose * TypedArrays.Float32Array?value ^-> T<unit>
                "uniformMatrix3fv" => UniformLocation?location * T<bool>?transpose * (Type.ArrayOf T<float>)?value ^-> T<unit>
                "uniformMatrix4fv" => UniformLocation?location * T<bool>?transpose * TypedArrays.Float32Array?value ^-> T<unit>
                "uniformMatrix4fv" => UniformLocation?location * T<bool>?transpose * (Type.ArrayOf T<float>)?value ^-> T<unit>
                "useProgram" => Program?program ^-> T<unit>
                "validateProgram" => Program?program ^-> T<unit>
                "vertexAttrib1f" => T<int>?indx * T<float>?x ^-> T<unit>
                "vertexAttrib1fv" => T<int>?indx * TypedArrays.Float32Array?values ^-> T<unit>
                "vertexAttrib1fv" => T<int>?indx * (Type.ArrayOf T<float>)?values ^-> T<unit>
                "vertexAttrib2f" => T<int>?indx * T<float>?x * T<float>?y ^-> T<unit>
                "vertexAttrib2fv" => T<int>?indx * TypedArrays.Float32Array?values ^-> T<unit>
                "vertexAttrib2fv" => T<int>?indx * (Type.ArrayOf T<float>)?values ^-> T<unit>
                "vertexAttrib3f" => T<int>?indx * T<float>?x * T<float>?y * T<float>?z ^-> T<unit>
                "vertexAttrib3fv" => T<int>?indx * TypedArrays.Float32Array?values ^-> T<unit>
                "vertexAttrib3fv" => T<int>?indx * (Type.ArrayOf T<float>)?values ^-> T<unit>
                "vertexAttrib4f" => T<int>?indx * T<float>?x * T<float>?y * T<float>?z * T<float>?w ^-> T<unit>
                "vertexAttrib4fv" => T<int>?indx * TypedArrays.Float32Array?values ^-> T<unit>
                "vertexAttrib4fv" => T<int>?indx * (Type.ArrayOf T<float>)?values ^-> T<unit>
                "vertexAttribPointer" => T<int>?indx * T<int>?size * Enum?typ * T<bool>?normalized * T<int>?stride * T<int>?offset ^-> T<unit>
                "viewport" => T<int>?x * T<int>?y * T<int>?width * T<int>?height ^-> T<unit>
            |])

    let ContextAttributesClass =
        ContextAttributes
        |+> Pattern.OptionalFields [
                "alpha", T<bool>
                "depth", T<bool>
                "stencil", T<bool>
                "antialias", T<bool>
                "premultipliedAlpha", T<bool>
                "preserveDrawingBuffer", T<bool>
            ]

    let ObjectClass =
        Object

    let BufferClass =
        Buffer

    let FramebufferClass =
        Framebuffer

    let ProgramClass =
        Program

    let RenderbufferClass =
        Renderbuffer

    let ShaderClass =
        Shader

    let TextureClass =
        Texture

    let UniformLocationClass =
        UniformLocation

    let ActiveInfoClass =
        ActiveInfo
        |+> Instance
            [
                "size" =? T<int>
                "type" =? Enum
                "name" =? T<string>
            ]

module WebSockets =

    let ReadyState =
        Pattern.EnumInlines "WebSocketReadyState" [
            "Connecting", "0"
            "Open", "1"
            "Closing", "2"
            "Closed", "3"
        ]

    let WebSocket =
        Class "WebSocket"
        |+> Instance
            [
                "readyState" =? ReadyState
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
                "send" => File.Blob ^-> T<unit>
            ]
        |+> Static [
                Constructor T<string>
                Constructor (T<string> * T<string[]>)
            ]

module EventSource =
    
    let ReadyState =
        Pattern.EnumInlines "ReadyState" [
            "connecting", "0"
            "open", "1"
            "closed", "2"
        ]

    let EventSourceOptions =
        Pattern.Config "EventSourceOptions" {
            Required = []
            Optional = [
                "withCredentials", T<bool>
            ]
        }

    let EventSource =
        Class "EventSource"
        |=> Inherits Dom.Interfaces.EventTarget
        |+> Static [
            Constructor (T<string>?url * !? EventSourceOptions?options)
        ]
        |+> Instance [
            // Properties
            "readyState" =? ReadyState
            "url" =? T<string>
            "withCredentials" =? T<bool>

            // Event handlers
            "onerror" =@ General.ErrorEvent ^-> T<unit>
            "onmessage" =@ General.MessageEvent ^-> T<unit>
            "onopen" =@ Dom.Interfaces.Event ^-> T<unit>

            // Methods
            "close" => T<unit> ^-> T<unit>
        ]

module Definition =

    let Namespaces =
        [
            Namespace "WebSharper.JavaScript" [
                AudioVideoCommon.MediaError
                AudioVideoCommon.MediaPreload
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
                Canvas.TextDirection
                Canvas.TextMetrics
                Canvas.OffscreenCanvasClass
                Canvas.PredefinedColorSpace
                Canvas.CanvasState
                Canvas.CanvasTransform
                Canvas.CanvasCompositing
                Canvas.CanvasImageSmoothing
                Canvas.CanvasFillStrokeStyles
                Canvas.CanvasShadowStyles
                Canvas.CanvasFilters
                Canvas.CanvasRect
                Canvas.CanvasDrawPath
                Canvas.CanvasUserInterface
                Canvas.CanvasText
                Canvas.CanvasDrawImage
                Canvas.CanvasImageData
                Canvas.CanvasPathDrawingStyles
                Canvas.CanvasTextDrawingStyles
                Canvas.CanvasPath
                Canvas.Path2D
                Canvas.CanvasFillRule
                Canvas.ImageDataSettings
                Canvas.ImageSmoothingQuality
                Canvas.CanvasTextRendering
                Canvas.CanvasFontVariantCaps
                Canvas.CanvasFontKerning
                Canvas.CanvasFontStretch

                Decl.SVGImageElementClass
                Decl.HTMLImageElementClass

                Geometry.DOMMatrixInit
                Geometry.DOMMatrix2DInit
                Geometry.DOMMatrixReadOnly
                Geometry.DOMMatrix
                Geometry.DOMQuadInit
                Geometry.DOMQuad
                Geometry.DOMRectInit
                Geometry.DOMRectReadOnly
                Geometry.DOMRect
                Geometry.DOMPointInit
                Geometry.DOMPointReadOnly
                Geometry.DOMPoint

                Elements.HTMLElement
                Elements.CanvasElement
                Elements.OldCanvasElement
                Elements.HTMLAudioElement
                Elements.HTMLVideoElement
                Elements.HTMLMediaElement
                Elements.HTMLButtonElement
                Elements.HTMLDataListElement
                Elements.HTMLDetailsElement
                Elements.HTMLDialogElement
                Elements.HTMLFieldSetElement
                Elements.HTMLFormControlsCollection
                Elements.HTMLFormElement
                Elements.HTMLInputElement
                Elements.HTMLLabelElement
                Elements.HTMLLegendElement
                Elements.HTMLMenuElement
                Elements.HTMLMenuItemElement
                Elements.HTMLMeterElement
                Elements.HTMLOptGroupElement
                Elements.HTMLOptionElement
                Elements.HTMLOptionsCollection
                Elements.HTMLOutputElement
                Elements.HTMLProgressElement
                Elements.HTMLSelectElement
                Elements.HTMLTextAreaElement
                Dom.Interfaces.AssignedNodesOptions
                Dom.Interfaces.HTMLSlotElement
                Dom.Interfaces.HTMLTemplateEvent
                Elements.SelectionMode
                EventSource.ReadyState
                EventSource.EventSourceOptions
                EventSource.EventSource
                File.BinaryFileReader
                File.Blob
                File.BlobPropertyBag
                File.File
                File.FileList
                File.FileReader
                File.FileReaderReadyState
                File.ProgressEvent
                File.TextFileReader
                Fetch.XMLHttpRequest
                Fetch.XMLHttpRequestEventTarget
                Fetch.XMLHttpRequestResponseType
                Fetch.XMLHttpRequestUpload
                Fetch.FormData
                Fetch.URL
                Fetch.URLSearchParams
                Fetch.Headers
                Fetch.Mode
                Fetch.Credentials
                Fetch.Cache
                Fetch.Redirect
                Fetch.Referrer
                Fetch.ReferrerPolicy
                Fetch.RequestOptions
                Fetch.RequestDestination
                Fetch.Request
                Fetch.ResponseOptions
                Fetch.ResponseType
                Fetch.Response
                General.BarProp
                General.History
                General.ErrorEvent
                Dom.Interfaces.Location
                General.MessageEvent
                General.MessagePort
                General.Navigator
                General.ScrollRestoration
                General.UndoManager
                General.WindowOrWorkerGlobalScope
                Dom.Interfaces.Window
                General.CSSSD
                General.MQL
                General.CustomElementsRegistry
                General.ElementDefinitionOptions

                Media.MediaStream
                Media.MediaStreamTrack
                Media.CanvasCaptureMediaStreamTrack
                Media.ConstrainBooleanParameters
                Media.ConstrainDOMStringParameters
                Media.ConstrainDoubleRange
                Media.ConstrainULongRange
                Media.InputDeviceInfo
                Media.MediaDeviceInfo
                Media.MediaDeviceKind
                Media.MediaStreamTrackContentHint
                Media.MediaDevices
                Media.MediaStreamTrackEvent
                Media.MediaStreamTrackState
                Media.ULongRange
                Media.DoubleRange
                Media.MediaTrackSupportedConstraints
                Media.MediaTrackSettings
                Media.MediaTrackCapabilities
                Media.MediaTrackConstraints
                Media.MediaTrackConstraintSet

                Streamable.ReadableStream
                Streamable.WritableStream
                Streamable.QueuingStrategy
                Streamable.QueuingStrategyInit
                Streamable.ByteLengthQueuingStrategy
                Streamable.CountQueuingStrategy
                Streamable.StreamPipeOptions
                Streamable.ReadableStreamDefaultReader
                Streamable.ReadableByteStreamController
                Streamable.ReadableStreamBYOBReader
                Streamable.ReadableStreamBYOBRequest
                Streamable.ReadableStreamDefaultController
                Streamable.ReadableStreamGenericReader
                Streamable.ReadableStreamGetReaderOptions
                Streamable.ReadableStreamReaderMode
                Streamable.ReadableStreamResult
                Streamable.ReadableWritablePair
                Streamable.WritableStreamDefaultController
                Streamable.WritableStreamDefaultWriter

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
                WebSockets.ReadyState
                WebStorage.Storage
                WebStorage.StorageEvent
                WebWorkers.WorkerType
                WebWorkers.WorkerOptions
                WebWorkers.WorkerNavigator
                WebWorkers.WorkerUtils
                WebWorkers.WorkerLocation
                WebWorkers.WorkerGlobalScope
                WebWorkers.DedicatedWorkerGlobalScope
                WebWorkers.SharedWorkerGlobalScope
                WebWorkers.AbstractWorker
                WebWorkers.Worker
                WebWorkers.SharedWorker
            ]
            Namespace "WebSharper.JavaScript.Geolocation" [
                Geolocation.Coordinates
                Geolocation.Geolocation
                Geolocation.Position
                Geolocation.PositionError
                Geolocation.PositionOptions
            ]
            Namespace "WebSharper.JavaScript.WebGL" [
                WebGL.RenderingContextClass
                WebGL.ContextAttributesClass
                WebGL.ObjectClass
                WebGL.BufferClass
                WebGL.FramebufferClass
                WebGL.ProgramClass
                WebGL.RenderbufferClass
                WebGL.ShaderClass
                WebGL.TextureClass
                WebGL.UniformLocationClass
                WebGL.ActiveInfoClass
                WebGL.EnumClass
                WebGL.DataViewClass
            ]
        ]
