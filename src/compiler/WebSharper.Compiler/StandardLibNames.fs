﻿// $begin{copyright}
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

module WebSharper.Compiler.StandardLibNames

let Set =
    System.Collections.Generic.HashSet [|
        "undefined"
        "Boolean"
        "JSON"
        "Date"
        "Math"
        "Number"
        "String"
        "RegExp"
        "InternalError"
        "AggregateError"
        "EvalError"
        "RangeError"
        "TypeError"
        "URIError"
        "ArrayBuffer"
        "Int8Array"
        "Uint8Array"
        "Int16Array"
        "Uint16Array"
        "Int32Array"
        "Uint32Array"
        "Float32Array"
        "Float64Array"
        "Uint8ClampedArray"
        "BigInt64Array"
        "BigUint64Array"
        "BigInt"
        "Proxy"
        "WeakMap"
        "Set"
        "DataView"
        "Symbol"
        "Intl"
        "Reflect"
        "WeakSet"
        "Atomics"
        "ReadableStream"
        "ByteLengthQueuingStrategy"
        "CountQueuingStrategy"
        "WebAssembly"
        "FinalizationRegistry"
        "WeakRef"
        "NaN"
        "Infinity"
        "isNaN"
        "isFinite"
        "parseFloat"
        "parseInt"
        "escape"
        "unescape"
        "decodeURI"
        "encodeURI"
        "decodeURIComponent"
        "encodeURIComponent"
        "SVGCircleElement"
        "CanvasPattern"
        "MimeType"
        "SecurityPolicyViolationEvent"
        "HTMLSourceElement"
        "CSSPageRule"
        "CanvasRenderingContext2D"
        "HTMLOptGroupElement"
        "WebGLShaderPrecisionFormat"
        "Screen"
        "TimeRanges"
        "SVGForeignObjectElement"
        "PopupBlockedEvent"
        "SVGFEDistantLightElement"
        "RTCDataChannelEvent"
        "SVGFEDisplacementMapElement"
        "SVGDefsElement"
        "CSSMediaRule"
        "CharacterData"
        "HTMLAllCollection"
        "ConstantSourceNode"
        "HTMLFieldSetElement"
        "ScreenOrientation"
        "FontFace"
        "SVGNumber"
        "TextTrack"
        "SpeechSynthesisEvent"
        "SourceBufferList"
        "DocumentTimeline"
        "History"
        "WebGLSync"
        "ProgressEvent"
        "SVGFECompositeElement"
        "SVGGradientElement"
        "HTMLTableSectionElement"
        "SVGFETileElement"
        "DOMPointReadOnly"
        "WheelEvent"
        "FileReader"
        "AudioBufferSourceNode"
        "SVGDescElement"
        "TextMetrics"
        "SVGFEFuncBElement"
        "CSSMozDocumentRule"
        "Request"
        "HTMLBRElement"
        "DOMRequest"
        "ServiceWorker"
        "HTMLDetailsElement"
        "AudioListener"
        "IDBOpenDBRequest"
        "HTMLParagraphElement"
        "SVGAnimatedRect"
        "SVGAnimatedLength"
        "DataTransfer"
        "Cache"
        "MessageChannel"
        "WebGLProgram"
        "DOMRectReadOnly"
        "URLSearchParams"
        "HTMLImageElement"
        "SVGUseElement"
        "MediaMetadata"
        "MediaRecorder"
        "RTCCertificate"
        "PopStateEvent"
        "SVGFEMergeElement"
        "RTCIceCandidate"
        "SVGFEFuncRElement"
        "HTMLAudioElement"
        "StorageEvent"
        "NodeList"
        "HTMLCollection"
        "mozRTCIceCandidate"
        "HTMLOptionElement"
        "SVGFEImageElement"
        "AudioProcessingEvent"
        "XPathResult"
        "HTMLTemplateElement"
        "AudioDestinationNode"
        "SVGFEPointLightElement"
        "SVGStyleElement"
        "AbortController"
        "Comment"
        "DOMQuad"
        "HTMLModElement"
        "SVGMPathElement"
        "WebSocket"
        "RTCTrackEvent"
        "HTMLFrameElement"
        "Navigator"
        "SVGImageElement"
        "HTMLMapElement"
        "MutationRecord"
        "SVGAnimatedPreserveAspectRatio"
        "MediaStreamEvent"
        "ImageBitmap"
        "StaticRange"
        "CSSImportRule"
        "MutationEvent"
        "SVGFEMorphologyElement"
        "CSSFontFeatureValuesRule"
        "SVGLength"
        "OfflineAudioContext"
        "DOMStringList"
        "HTMLObjectElement"
        "WebGLTransformFeedback"
        "IDBKeyRange"
        "SVGFEComponentTransferElement"
        "TimeEvent"
        "ChannelMergerNode"
        "SVGAnimatedTransformList"
        "VTTCue"
        "RTCDtlsTransport"
        "WebGLVertexArrayObject"
        "webkitURL"
        "RTCPeerConnection"
        "DOMMatrix"
        "CSSRule"
        "SVGAElement"
        "HTMLTrackElement"
        "MediaKeySession"
        "XSLTProcessor"
        "CSSStyleDeclaration"
        "IDBObjectStore"
        "AudioNode"
        "SVGFEColorMatrixElement"
        "DOMParser"
        "SpeechSynthesisVoice"
        "SVGGeometryElement"
        "AudioContext"
        "SpeechSynthesisErrorEvent"
        "CanvasCaptureMediaStream"
        "ServiceWorkerRegistration"
        "PaintRequest"
        "SVGAnimatedBoolean"
        "Headers"
        "PushSubscriptionOptions"
        "PannerNode"
        "SVGFEFuncAElement"
        "HTMLAnchorElement"
        "MediaRecorderErrorEvent"
        "WebGL2RenderingContext"
        "HTMLCanvasElement"
        "WebGLBuffer"
        "SVGSymbolElement"
        "DOMTokenList"
        "DOMRect"
        "WebKitCSSMatrix"
        "IDBTransaction"
        "AudioBuffer"
        "PerformanceObserverEntryList"
        "SVGAnimateTransformElement"
        "CSSRuleList"
        "SVGFESpotLightElement"
        "MediaSource"
        "AnimationPlaybackEvent"
        "ScrollAreaEvent"
        "HTMLTextAreaElement"
        "FormDataEvent"
        "HTMLBaseElement"
        "SVGStringList"
        "SVGClipPathElement"
        "AbortSignal"
        "MediaCapabilities"
        "Option"
        "HTMLMenuElement"
        "BroadcastChannel"
        "DOMImplementation"
        "FileSystem"
        "PerformanceEntry"
        "HTMLTableRowElement"
        "SVGPolylineElement"
        "CSSSupportsRule"
        "SVGLinearGradientElement"
        "FontFaceSetLoadEvent"
        "HTMLSlotElement"
        "IIRFilterNode"
        "PerformanceNavigationTiming"
        "VTTRegion"
        "TextDecoder"
        "HTMLTableElement"
        "SVGAnimateMotionElement"
        "BaseAudioContext"
        "SVGTextElement"
        "PerformanceEventTiming"
        "DelayNode"
        "DOMPoint"
        "DeviceOrientationEvent"
        "IDBMutableFile"
        "SpeechSynthesisUtterance"
        "Notification"
        "MediaStreamTrack"
        "TransitionEvent"
        "SVGAnimatedString"
        "SVGFEDiffuseLightingElement"
        "HTMLPictureElement"
        "SVGTransform"
        "MediaEncryptedEvent"
        "HTMLQuoteElement"
        "HTMLInputElement"
        "WebGLContextEvent"
        "SVGFEGaussianBlurElement"
        "MediaQueryListEvent"
        "XMLDocument"
        "HTMLSpanElement"
        "WebGLRenderbuffer"
        "TextEncoder"
        "RTCDTMFSender"
        "HTMLDivElement"
        "ImageBitmapRenderingContext"
        "SVGAnimatedEnumeration"
        "WebGLQuery"
        "CSS"
        "CanvasGradient"
        "WebGLActiveInfo"
        "FontFaceSet"
        "ScriptProcessorNode"
        "SVGAnimatedNumberList"
        "SVGFEConvolveMatrixElement"
        "HTMLDListElement"
        "WebGLTexture"
        "CSSTransition"
        "HTMLLinkElement"
        "TextTrackCue"
        "SVGLengthList"
        "XPathEvaluator"
        "ShadowRoot"
        "IntersectionObserverEntry"
        "MouseEvent"
        "Geolocation"
        "NamedNodeMap"
        "DocumentType"
        "HTMLTableCellElement"
        "HTMLProgressElement"
        "IDBCursorWithValue"
        "SVGMatrix"
        "PerformanceNavigation"
        "PaintRequestList"
        "SVGAnimateElement"
        "Range"
        "SVGAnimatedLengthList"
        "TextTrackList"
        "FileList"
        "AbstractRange"
        "DynamicsCompressorNode"
        "IDBRequest"
        "FileSystemDirectoryReader"
        "MediaStreamTrackAudioSourceNode"
        "WebGLShader"
        "HTMLFormElement"
        "CompositionEvent"
        "Directory"
        "PerformancePaintTiming"
        "RTCRtpSender"
        "HTMLHeadingElement"
        "ValidityState"
        "GainNode"
        "HTMLMetaElement"
        "DocumentFragment"
        "SVGPathSegList"
        "EventSource"
        "HTMLIFrameElement"
        "PointerEvent"
        "SVGTSpanElement"
        "BiquadFilterNode"
        "Storage"
        "SVGPathElement"
        "SVGGElement"
        "AnalyserNode"
        "WebGLRenderingContext"
        "IdleDeadline"
        "SVGPreserveAspectRatio"
        "CSSConditionRule"
        "MediaQueryList"
        "HTMLOptionsCollection"
        "HTMLDataListElement"
        "MediaKeyMessageEvent"
        "StyleSheetList"
        "HTMLUListElement"
        "PerformanceMark"
        "TextTrackCueList"
        "HTMLLIElement"
        "HTMLTableCaptionElement"
        "GeolocationPositionError"
        "SharedWorker"
        "NodeIterator"
        "HTMLOListElement"
        "XMLSerializer"
        "RTCRtpTransceiver"
        "HTMLLegendElement"
        "CacheStorage"
        "NodeFilter"
        "SVGAnimatedInteger"
        "HTMLHtmlElement"
        "WebGLUniformLocation"
        "HTMLMarqueeElement"
        "SVGPatternElement"
        "IDBCursor"
        "HTMLFormControlsCollection"
        "HTMLHeadElement"
        "HTMLSelectElement"
        "MediaElementAudioSourceNode"
        "Blob"
        "SVGFilterElement"
        "HTMLMeterElement"
        "SVGSetElement"
        "SVGGraphicsElement"
        "File"
        "CSSStyleRule"
        "IDBFileRequest"
        "SVGLineElement"
        "PushManager"
        "TrackEvent"
        "PeriodicWave"
        "RadioNodeList"
        "XPathExpression"
        "MediaKeyStatusMap"
        "IDBDatabase"
        "DeviceMotionEvent"
        "MediaKeyError"
        "AudioScheduledSourceNode"
        "MessagePort"
        "SVGAnimatedNumber"
        "SVGMarkerElement"
        "TreeWalker"
        "FileSystemDirectoryEntry"
        "Permissions"
        "SVGFESpecularLightingElement"
        "SVGAnimatedAngle"
        "SVGSwitchElement"
        "IDBFactory"
        "SVGPolygonElement"
        "SVGFEBlendElement"
        "Animation"
        "CSSFontFaceRule"
        "HTMLLabelElement"
        "SVGRect"
        "PerformanceMeasure"
        "SVGNumberList"
        "ResizeObserverSize"
        "HTMLAreaElement"
        "OscillatorNode"
        "SVGStopElement"
        "SVGViewElement"
        "RTCSessionDescription"
        "MediaKeySystemAccess"
        "SVGElement"
        "Text"
        "mozRTCSessionDescription"
        "HTMLButtonElement"
        "SVGFEDropShadowElement"
        "AnimationEvent"
        "Image"
        "WaveShaperNode"
        "MediaCapabilitiesInfo"
        "HTMLTableColElement"
        "KeyEvent"
        "DOMMatrixReadOnly"
        "CDATASection"
        "SVGTextContentElement"
        "PluginArray"
        "XMLHttpRequest"
        "HashChangeEvent"
        "DataTransferItemList"
        "AnimationTimeline"
        "ChannelSplitterNode"
        "MediaStream"
        "HTMLMediaElement"
        "Crypto"
        "CSSKeyframesRule"
        "FileSystemFileEntry"
        "Response"
        "XMLHttpRequestEventTarget"
        "HTMLScriptElement"
        "HTMLTitleElement"
        "KeyboardEvent"
        "Plugin"
        "HTMLOutputElement"
        "MediaSession"
        "DragEvent"
        "FormData"
        "PromiseRejectionEvent"
        "DOMStringMap"
        "InputEvent"
        "CSSStyleSheet"
        "SpeechSynthesis"
        "CSSAnimation"
        "Audio"
        "VisualViewport"
        "PerformanceResourceTiming"
        "RTCStatsReport"
        "SVGPoint"
        "StereoPannerNode"
        "WebGLFramebuffer"
        "SVGComponentTransferFunctionElement"
        "RTCRtpReceiver"
        "SVGFETurbulenceElement"
        "Path2D"
        "CSSGroupingRule"
        "PushSubscription"
        "URL"
        "CustomElementRegistry"
        "MathMLElement"
        "SVGScriptElement"
        "SVGTextPositioningElement"
        "MediaStreamAudioSourceNode"
        "HTMLHRElement"
        "SVGEllipseElement"
        "WebGLSampler"
        "CSSCounterStyleRule"
        "RTCDTMFToneChangeEvent"
        "SourceBuffer"
        "SVGTextPathElement"
        "AudioParam"
        "MediaList"
        "SVGRectElement"
        "SVGAngle"
        "HTMLPreElement"
        "Selection"
        "SVGUnitTypes"
        "PermissionStatus"
        "BlobEvent"
        "RTCPeerConnectionIceEvent"
        "MediaStreamTrackEvent"
        "Attr"
        "MediaKeys"
        "CustomEvent"
        "SVGFEMergeNodeElement"
        "VideoPlaybackQuality"
        "Worker"
        "ClipboardEvent"
        "HTMLEmbedElement"
        "PerformanceObserver"
        "HTMLDirectoryElement"
        "HTMLFrameSetElement"
        "HTMLVideoElement"
        "MutationObserver"
        "SVGFEFloodElement"
        "CloseEvent"
        "IDBIndex"
        "IDBFileHandle"
        "DataTransferItem"
        "CSSKeyframeRule"
        "SVGRadialGradientElement"
        "SVGAnimationElement"
        "MimeTypeArray"
        "CaretPosition"
        "SVGMaskElement"
        "MediaError"
        "BarProp"
        "ResizeObserver"
        "SVGTitleElement"
        "SVGPointList"
        "HTMLStyleElement"
        "UIEvent"
        "DOMException"
        "ImageData"
        "XMLHttpRequestUpload"
        "CSS2Properties"
        "IDBVersionChangeEvent"
        "ResizeObserverEntry"
        "mozRTCPeerConnection"
        "FocusEvent"
        "HTMLDataElement"
        "FileSystemEntry"
        "SVGFEFuncGElement"
        "MessageEvent"
        "SVGFEOffsetElement"
        "ProcessingInstruction"
        "RTCDataChannel"
        "AnimationEffect"
        "MediaStreamAudioDestinationNode"
        "StyleSheet"
        "SubmitEvent"
        "ConvolverNode"
        "IntersectionObserver"
        "SVGSVGElement"
        "OfflineAudioCompletionEvent"
        "KeyframeEffect"
        "MouseScrollEvent"
        "CSSNamespaceRule"
        "HTMLFontElement"
        "ErrorEvent"
        "HTMLParamElement"
        "SVGTransformList"
        "HTMLUnknownElement"
        "HTMLTimeElement"
        "SVGMetadataElement"
        "Function"
        "Object"
        "eval"
        "EventTarget"
        "Window"
        "close"
        "stop"
        "focus"
        "blur"
        "open"
        "alert"
        "confirm"
        "prompt"
        "print"
        "postMessage"
        "captureEvents"
        "releaseEvents"
        "getSelection"
        "getComputedStyle"
        "matchMedia"
        "moveTo"
        "moveBy"
        "resizeTo"
        "resizeBy"
        "scroll"
        "scrollTo"
        "scrollBy"
        "requestAnimationFrame"
        "cancelAnimationFrame"
        "getDefaultComputedStyle"
        "scrollByLines"
        "scrollByPages"
        "sizeToContent"
        "updateCommands"
        "find"
        "dump"
        "setResizable"
        "requestIdleCallback"
        "cancelIdleCallback"
        "btoa"
        "atob"
        "setTimeout"
        "clearTimeout"
        "setInterval"
        "clearInterval"
        "queueMicrotask"
        "createImageBitmap"
        "fetch"
        "self"
        "name"
        "history"
        "customElements"
        "locationbar"
        "menubar"
        "personalbar"
        "scrollbars"
        "statusbar"
        "toolbar"
        "status"
        "closed"
        "event"
        "frames"
        "length"
        "opener"
        "parent"
        "frameElement"
        "navigator"
        "clientInformation"
        "external"
        "screen"
        "innerWidth"
        "innerHeight"
        "scrollX"
        "pageXOffset"
        "scrollY"
        "pageYOffset"
        "screenLeft"
        "screenTop"
        "screenX"
        "screenY"
        "outerWidth"
        "outerHeight"
        "performance"
        "mozInnerScreenX"
        "mozInnerScreenY"
        "devicePixelRatio"
        "scrollMaxX"
        "scrollMaxY"
        "fullScreen"
        "ondevicemotion"
        "ondeviceorientation"
        "onabsolutedeviceorientation"
        "content"
        "InstallTrigger"
        "sidebar"
        "onvrdisplayconnect"
        "onvrdisplaydisconnect"
        "onvrdisplayactivate"
        "onvrdisplaydeactivate"
        "onvrdisplaypresentchange"
        "visualViewport"
        "crypto"
        "onabort"
        "onblur"
        "onfocus"
        "onauxclick"
        "onbeforeinput"
        "oncanplay"
        "oncanplaythrough"
        "onchange"
        "onclick"
        "onclose"
        "oncontextmenu"
        "oncuechange"
        "ondblclick"
        "ondrag"
        "ondragend"
        "ondragenter"
        "ondragexit"
        "ondragleave"
        "ondragover"
        "ondragstart"
        "ondrop"
        "ondurationchange"
        "onemptied"
        "onended"
        "onformdata"
        "oninput"
        "oninvalid"
        "onkeydown"
        "onkeypress"
        "onkeyup"
        "onload"
        "onloadeddata"
        "onloadedmetadata"
        "onloadend"
        "onloadstart"
        "onmousedown"
        "onmouseenter"
        "onmouseleave"
        "onmousemove"
        "onmouseout"
        "onmouseover"
        "onmouseup"
        "onwheel"
        "onpause"
        "onplay"
        "onplaying"
        "onprogress"
        "onratechange"
        "onreset"
        "onresize"
        "onscroll"
        "onseeked"
        "onseeking"
        "onselect"
        "onstalled"
        "onsubmit"
        "onsuspend"
        "ontimeupdate"
        "onvolumechange"
        "onwaiting"
        "onselectstart"
        "ontoggle"
        "onpointercancel"
        "onpointerdown"
        "onpointerup"
        "onpointermove"
        "onpointerout"
        "onpointerover"
        "onpointerenter"
        "onpointerleave"
        "ongotpointercapture"
        "onlostpointercapture"
        "onmozfullscreenchange"
        "onmozfullscreenerror"
        "onanimationcancel"
        "onanimationend"
        "onanimationiteration"
        "onanimationstart"
        "ontransitioncancel"
        "ontransitionend"
        "ontransitionrun"
        "ontransitionstart"
        "onwebkitanimationend"
        "onwebkitanimationiteration"
        "onwebkitanimationstart"
        "onwebkittransitionend"
        "onerror"
        "speechSynthesis"
        "onafterprint"
        "onbeforeprint"
        "onbeforeunload"
        "onhashchange"
        "onlanguagechange"
        "onmessage"
        "onmessageerror"
        "onoffline"
        "ononline"
        "onpagehide"
        "onpageshow"
        "onpopstate"
        "onrejectionhandled"
        "onstorage"
        "onunhandledrejection"
        "onunload"
        "ongamepadconnected"
        "ongamepaddisconnected"
        "localStorage"
        "origin"
        "crossOriginIsolated"
        "isSecureContext"
        "indexedDB"
        "caches"
        "sessionStorage"
        "window"
        "document"
        "location"
        "top"
        "netscape"
        "Node"
        "Document"
        "HTMLDocument"
        "EventCounts"
        "Map"
        "Performance"
        "Event"
        "Location"
        "PerformanceTiming"
        "Promise"
        "console"
        "PageTransitionEvent"
        "NotifyPaintEvent"
        "DOMRectList"
        "Element"
        "HTMLElement"
        "HTMLBodyElement"
        "globalThis"
        "Error"
        "ReferenceError"
        "BeforeUnloadEvent"
        "Array"
        "SyntaxError"
    |]