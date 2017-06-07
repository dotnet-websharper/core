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

namespace WebSharper.JQuery

open WebSharper.JavaScript
open WebSharper.InterfaceGenerator

module Definition =

    let JQ = Class "jQuery"

    let Event =
        Class "jQuery.Event"
        |+> Instance [
            "AsDomEvent" =? T<Dom.Event>
            |> WithGetterInline "$0"
            |> WithComment "Casts this JQuery.Event to a Dom.Event"
            "currentTarget" =? T<Dom.Element>
            "data" =? T<obj>
            "delegateTarget" =? T<Dom.Element>
            "isDefaultPrevented" => T<unit> ^-> T<bool>
            "isImmediatePropagationStopped" => T<unit> ^-> T<bool>
            "isPropagationStopped" => T<unit> ^-> T<bool>
            "metaKey" =? T<bool>
            "namespace" =? T<string>
            "pageX" =? T<int>
            "pageY" =? T<int>
            "preventDefault" => T<unit> ^-> T<unit>
            "stopImmediatePropagation" => T<unit> ^-> T<unit>
            "stopPropagation" => T<unit> ^-> T<unit>
            "result" =? T<obj>
            "relatedTarget" =? T<Dom.Element>
            "target" =? T<Dom.Element>
            "which" =? T<int>
            "timeStamp" =? T<int>
            "type" =? T<string>
            "altKey" =? T<bool>
            "bubbles" =? T<bool>
            "button" =? T<int>
            "cancelable" =? T<bool>
            "char" =? T<char>
            "charCode" =? T<int>
            "clientX" =? T<int>
            "clientY" =? T<int>
            "ctrlKey" =? T<bool>
            "detail" =? T<obj>
            "eventPhase" =? T<int>
            "offsetX" =? T<int>
            "offsetY" =? T<int>
            "originalTarget" =? T<Dom.Element>
            "screenX" =? T<int>
            "screenY" =? T<int>
            "shiftKey" =? T<bool>
            "view" =? T<obj>
        ]

    let Error =
        Class "Error"
    
    let Promise =
        Class "Promise"
        |+> Instance [
            "always" => (!+ (!+ T<obj> ^-> T<unit>)) ^-> TSelf
            "done" => (!+ (!+ T<obj> ^-> T<unit>)) ^-> TSelf
            "fail" => (!+ (!+ T<obj> ^-> T<unit>)) ^-> TSelf
            "progress" => (!+ (!+ T<obj> ^-> T<unit>)) ^-> TSelf
            "state" => T<unit -> string>
            "then" => T<unit -> unit> * !? T<unit -> unit> * !? T<unit -> unit> ^-> TSelf
        ]

    let DeferredState = 
        "pending resolved rejected".Split(' ')
        |> Pattern.EnumStrings "DeferredState"

    let Deferred =
        let func = T<unit> ^-> T<unit>
        let funcorfuncs = func + Type.ArrayOf func
        Class "jQuery.Deferred"
        |+> Instance [
            "always" => func ^-> TSelf
            "always" => func *+ func ^-> TSelf
            "catch" => func ^-> Promise
            "done" => func ^-> TSelf
            "done" => func *+ func ^-> TSelf
            "fail" => func ^-> TSelf
            "fail" => func *+ func ^-> TSelf
            "notifiy" => T<obj> ^-> TSelf
            "notifiyWith" => T<obj> * !?T<obj []> ^-> TSelf
            "progressCallbacks" => func ^-> TSelf
            "progressCallbacks" => func *+ func ^-> TSelf
            "promise" => !?T<obj> ^-> Promise
            "reject" => !?T<obj> ^-> TSelf
            "rejectWith" => T<obj> * !?T<obj []> ^-> TSelf
            "resolve" => !?T<obj> ^-> TSelf
            "resolveWith" => T<obj> * !?T<obj []> ^-> TSelf
            "state" => T<unit> ^-> DeferredState
            "then" => funcorfuncs * funcorfuncs * !?funcorfuncs ^-> Promise
        ]

    let JqXHR =
        Class "jQuery.jqXHR"
        |=> Inherits Deferred
        |+> Instance [
            "readyState" =? T<int>
            "status" =? T<int>
            "statusText" =? T<string>
            "setRequestHeader" => T<string>?name * T<string>?value ^-> T<unit>
            "getResponseHeader" => T<string> ^-> T<string>
            "getAllResponseHeaders" => T<unit> ^-> T<string>
            "responseText" =? T<string>
            "responseXML" =? T<Dom.Document>
            "abort" => T<unit> ^-> T<unit>
        ]

    let DataType =
        "xml html script json jsonp text".Split(' ')
        |> Pattern.EnumStrings "DataType"

    let RequestType =
        "GET POST PUT DELETE".Split(' ')
        |> Pattern.EnumStrings "RequestType"

    let AjaxSettings =
        Pattern.Config "AjaxSettings" {
            Required = []
            Optional =
                [
                    "accepts", T<obj>
                    "async", T<bool>
                    "beforeSend", JqXHR * TSelf ^-> T<unit>
                    "cache", T<bool>
                    "complete", JqXHR * T<string> ^-> T<unit>
                    "contentType", (T<string> + T<bool>)
                    "context", T<obj>
                    "converters", T<obj>
                    "crossDomain", T<bool>
                    "data", (T<obj> + T<string> + T<obj []>)
                    "dataFilter", T<string> * DataType ^-> T<obj>
                    "dataType", DataType.Type
                    "error", JqXHR * T<string> * T<string> ^-> T<unit>
                    "global", T<bool>
                    "headers", T<obj>
                    "ifModified", T<bool>
                    "isLocal", T<bool>
                    "jsonp", (T<string> + T<bool>)
                    "jsonpCallback", (T<string> + (T<unit> ^-> T<unit>))
                    "method", RequestType.Type
                    "mimeType", T<string>
                    "password", T<string>
                    "processData", T<bool>
                    "scriptCharset", T<string>
                    "statusCode", T<obj>
                    "success", T<obj> * T<string> * JqXHR ^-> T<unit>
                    "timeout", T<double>
                    "traditional", T<bool>
                    "type", RequestType.Type
                    "url", T<string>
                    "username", T<string>
                    "xhr", T<unit> ^-> T<unit>
                    "xhrFields", T<obj>
                ]
        }


    let AjaxHandler =
        T<Dom.Element> -* Event * JqXHR * AjaxSettings ^-> T<unit>

    let AjaxSuccessHandler =
        T<Dom.Element> -* Event * JqXHR * AjaxSettings * T<obj> ^-> T<unit>
    
    let AjaxErrorHandler =
        T<Dom.Element> -* Event * JqXHR * AjaxSettings * T<string> ^-> T<unit>

    let Context = T<Dom.Element> + JQ

    let Callbacks =
        let cbf = T<unit> ^-> T<unit>
        Class "Callbacks"
        |+> Instance [
            "add" => (cbf + Type.ArrayOf cbf) ^-> TSelf
            "disable" => T<unit> ^-> T<unit>
            "disabled" => T<unit> ^-> T<bool>
            "empty" => T<unit> ^-> TSelf
            "fire" => T<obj> ^-> TSelf
            "fired" => T<unit> ^-> T<bool>
            "fireWith" => !?T<obj> * !?(T<obj> + T<obj []>) ^-> TSelf
            "has" => !?(cbf) ^-> T<bool>
            "lock" => T<unit> ^-> TSelf
            "locked" => T<unit> ^-> T<bool>
            "remove" => (cbf + Type.ArrayOf cbf) ^-> TSelf
        ]

    let Position =
        Pattern.Config "Position" {
            Required = []
            Optional = 
                [
                    "top", T<float>
                    "left", T<float>
                ]
        }

    let AnimateSettings =
        Pattern.Config "AnimateSettings" {
            Required = []
            Optional =
                [
                    "duration" , T<int> + T<string>
                    "easing" , T<string>
                    "queue" , T<bool> + T<string>
                    "specialEasing" , T<Object<string>>
                    "step" , T<int> * T<obj> ^-> T<unit>
                    "progress" , Promise * T<int> * T<int> ^-> T<unit>
                    "complete" , T<unit> ^-> T<unit>
                    "start" , Promise ^-> T<unit>
                    "done" , Promise * T<bool> ^-> T<unit>
                    "fail" , Promise * T<bool> ^-> T<unit>
                    "always" , Promise * T<bool> ^-> T<unit>
                ]
        }

    let Speed =
        Pattern.Config "Speed" {
            Required = []
            Optional = 
                [
                    "duration", T<int> + T<string>
                    "easing", T<string>
                    "complete", T<unit> ^-> T<unit>
                ]
        }

    let FX =
        Class "jQuery.fx"
        |+> Static [
            "off" =@ T<bool>
            |> WithComment "Globally disable all animations"

            "extend" => T<obj> ^-> T<obj>
            |> WithComment "Merge the contents of an object onto the jQuery prototype to provide new jQuery instance methods."
        ]

    let JQueryClass =
        let EH = T<Dom.Element> -* Event ^-> T<unit>
        let Content = T<string> + T<Dom.Node> + Type.ArrayOf T<Dom.Node> + TSelf
        let Func = T<int> ^-> Content
        let FuncWithHTML = T<int> * T<string> ^-> Content
        JQ
        |+> Instance [
            "ignore" =? T<unit>
            |> WithGetterInline "$this"

            // Removed
            // Error
            "error" => EH?handler ^-> JQ
            |> WithComment "Bind an event handler to the \"error\" JavaScript event."
            |> WithInteropInline (fun tr -> "$this.on(\"error\", " + tr "handler" + ")")
            |> ObsoleteWithMessage "Use .On(\"error\", eventHandler) instead"

            // Error
            "error" => T<Object<string>>?data * EH?handler ^-> JQ
            |> WithInteropInline (fun tr -> "$this.on(\"error\", null, $data, " + tr "handler" + ")")
            |> ObsoleteWithMessage "Use .On(\"error\", null, data, eventHandler) instead"

            // Deprecated
            "bind" => T<string> * !?T<Object<string>> * EH ^-> TSelf |> ObsoleteWithMessage "Use .On() instead"
            "bind" => T<string> * !?T<Object<string>> * !?T<bool> ^-> TSelf |> ObsoleteWithMessage "Use .On() instead"
            "bind" => T<obj> ^-> TSelf |> ObsoleteWithMessage "Use .On() instead"
            "bindFalse" => T<string>?event * T<Object<string>>?eventData ^-> JQ
            |> WithInline "$this.bind($event, $eventData, false)"
            |> ObsoleteWithMessage "Use .On() and event.PreventDefault() instead"
            "delegate" => T<string>?selector * Event?eventType * !?T<Object<string>>?eventData * EH?handler ^-> JQ
            |> WithComment "Attach a handler to one or more events for all elements that match the selector, now or in the future, based on a specific set of root elements."
            |> ObsoleteWithMessage "Use .On() instead"
            "unbind" => T<unit> ^-> JQ |> ObsoleteWithMessage "Use .Off() instead"
            "unbind" => Event ^-> JQ |> ObsoleteWithMessage "Use .Off() instead"
            "unbind" => T<string> * !?EH ^-> JQ |> ObsoleteWithMessage "Use .Off() instead"
            "unbindFalse" => T<string>?event * T<Object<string>>?eventData ^-> JQ
            |> WithInline "$this.unbind($event, $eventData, false)"
            |> ObsoleteWithMessage "Use .Off() instead"
            "undelegate" => T<unit> ^-> JQ |> ObsoleteWithMessage "Use .Off() instead"
            "undelegate" => T<string> * T<string> * !?EH ^-> JQ |> ObsoleteWithMessage "Use .Off() instead"

            // Ajax related instance methods
            "ajaxComplete" => AjaxHandler ^-> TSelf
            "ajaxError" => AjaxErrorHandler ^-> TSelf
            "ajaxSend" => AjaxHandler ^-> TSelf
            "ajaxStart" => (T<unit> ^-> T<unit>) ^-> TSelf
            "ajaxStop" => (T<unit> ^-> T<unit>) ^-> TSelf
            "ajaxSuccess" => AjaxSuccessHandler ^-> TSelf
            "load" => (T<string> * !?(T<obj> + T<string>) * !?(T<string> * T<string> * JqXHR ^-> T<unit>)) ^-> TSelf
            "serialize" => T<unit> ^-> T<string>
            "serializeArray" => T<unit> ^-> T<obj []>

            // Attribute related instance methods
            "addClass" => T<string> ^-> TSelf
            "addClass" => (T<int> * T<string> ^-> T<string>) ^-> TSelf
            "attr" => T<string> ^-> T<string>
            "attr" => T<string> * T<string> ^-> TSelf
            "attr" => T<obj> ^-> TSelf
            "attr" => T<string> * ((T<int> * T<string> ^-> (T<string> + T<float>))) ^-> TSelf
            "hasClass" => T<string> ^-> T<bool>
            "html" => T<unit> ^-> T<string>
            "html" => T<string> ^-> TSelf
            "html" => (T<int> * T<string> ^-> T<string>) ^-> TSelf
            Generic - fun t -> "prop" => T<string> ^-> t
            Generic - fun t -> "prop" => T<string> * t ^-> TSelf
            "prop" => T<obj> ^-> TSelf
            "prop" => T<string> * (T<int> * T<obj> ^-> T<obj>) ^-> TSelf
            "removeAttr" => T<string> ^-> TSelf
            "removeClass" => T<string> ^-> TSelf
            "removeClass" => (T<int> * T<obj> ^-> T<string>) ^-> TSelf
            "removeProp" => T<string> ^-> TSelf
            "toggleClass" => T<string> ^-> TSelf
            "toggleClass" => T<string> * T<bool> ^-> TSelf
            "toggleClass" => (T<int> * T<string> * T<bool> ^-> T<string>) * !?T<bool> ^-> TSelf
            "val" => T<unit> ^-> T<obj>
            "val" => (T<string> + T<float> + T<obj []>) ^-> TSelf
            "val" => (T<int> * T<string> ^-> T<string>) ^-> TSelf

            // CSS related instance methods
            "css" => T<unit> ^-> T<string>
            "css" => (T<string> + T<string []>) ^-> T<string>
            "css" => T<string> * T<string> ^-> TSelf
            "css" => T<string> * (T<int> * T<string> ^-> T<string>) ^-> TSelf
            "css" => T<Object<string>> ^-> TSelf
            "height" => T<unit> ^-> T<int>
            "height" => T<int> ^-> TSelf
            "height" => (T<int> * T<int> ^-> T<int>) ^-> TSelf
            "innerHeight" => T<unit> ^-> T<int>
            "innerHeight" => T<int> ^-> TSelf
            "innerHeight" => (T<int> * T<int> ^-> T<int>) ^-> TSelf
            "outerHeight" => !?T<bool> ^-> T<int>
            "outerHeight" => T<int> ^-> TSelf
            "outerHeight" => (T<int> * T<int> ^-> T<int>) ^-> TSelf
            "width" => T<unit> ^-> T<int>
            "width" => T<int> ^-> TSelf
            "width" => (T<int> * T<int> ^-> T<int>) ^-> TSelf
            "innerWidth" => T<unit> ^-> T<int>
            "innerWidth" => T<int> ^-> TSelf
            "innerWidth" => (T<int> * T<int> ^-> T<int>) ^-> TSelf
            "outerWidth" => !?T<bool> ^-> T<int>
            "outerWidth" => T<int> ^-> TSelf
            "outerWidth" => (T<int> * T<int> ^-> T<int>) ^-> TSelf
            "offset" => T<unit> ^-> Position
            "offset" => Position ^-> TSelf
            "offset" => (T<int> * Position ^-> Position) ^-> TSelf
            "position" => T<unit> ^-> Position
            "scrollLeft" => T<unit> ^-> T<int>
            "scrollLeft" => T<int> ^-> TSelf
            "scrollTop" => T<unit> ^-> T<int>
            "scrollTop" => T<int> ^-> TSelf

            // Data related instance methods
            "clearQueue" => !?T<string> ^-> TSelf
            "dequeue" => !?T<string> ^-> TSelf
            "data" => T<string> * T<obj> ^-> TSelf
            "data" => T<obj> ^-> TSelf
            "data" => T<string> ^-> T<obj>
            "data" => T<unit> ^-> T<obj>
            "queue" => !?T<string> ^-> T<obj []>
            "queue" => !?T<string> * (T<obj []> + ((T<unit> ^-> T<unit>) ^-> T<unit>)) ^-> TSelf
            "removeData" => (T<string> + T<string []>) ^-> TSelf

            // Deferred
            "promise" => !?T<string> * !?T<obj> ^-> Promise

            // Effects
            "animate" => T<Object<string>>?properties * AnimateSettings?options ^-> TSelf
            "animate" => T<Object<string>>?properties * (T<int> + T<string>)?duration ^-> TSelf
            "animate" => T<Object<string>>?properties * (T<int> + T<string>)?duration * T<string>?easing ^-> TSelf
            "animate" => T<Object<string>>?properties * (T<int> + T<string>)?duration * (T<unit> ^-> T<unit>)?complete ^-> TSelf
            "animate" => T<Object<string>>?properties * (T<int> + T<string>)?duration * T<string>?easing * (T<unit> ^-> T<unit>)?complete ^-> TSelf
            "delay" => T<int>?duration * !?T<string>?queuename ^-> TSelf
            "fadeIn" => AnimateSettings ^-> TSelf
            "fadeIn" => !?(T<int> + T<string>) * !?(T<unit> ^-> T<unit>) ^-> TSelf
            "fadeIn" => (T<int> + T<string>) * T<string> * !?(T<unit> ^-> T<unit>) ^-> TSelf
            "fadeOut" => AnimateSettings ^-> TSelf
            "fadeOut" => !?(T<int> + T<string>) * !?(T<unit> ^-> T<unit>) ^-> TSelf
            "fadeOut" => (T<int> + T<string>) * T<string> * !?(T<unit> ^-> T<unit>) ^-> TSelf
            "fadeTo" => (T<int> + T<string>) * T<int> * T<string> * !?(T<unit> ^-> T<unit>) ^-> TSelf
            "fadeTo" => (T<int> + T<string>) * T<int> * !?(T<unit> ^-> T<unit>) ^-> TSelf
            "fadeToggle" => AnimateSettings ^-> TSelf
            "fadeToggle" => !?(T<int> + T<string>) * !?(T<unit> ^-> T<unit>) ^-> TSelf
            "fadeToggle" => (T<int> + T<string>) * T<string> * !?(T<unit> ^-> T<unit>) ^-> TSelf
            "finish" => !?T<string> ^-> TSelf
            "hide" => (T<int> + T<string>) * (T<unit> ^-> T<unit>) ^-> TSelf
            "hide" => AnimateSettings ^-> TSelf
            "hide" => !?(T<int> + T<string>) * !?T<string> * !?(T<unit> ^-> T<unit>) ^-> TSelf
            "show" => AnimateSettings ^-> TSelf
            "show" => (T<int> + T<string>) * (T<unit> ^-> T<unit>) ^-> TSelf
            "show" => !?(T<int> + T<string>) * !?T<string> * !?(T<unit> ^-> T<unit>) ^-> TSelf
            "slideDown" => AnimateSettings ^-> TSelf
            "slideDown" => !?(T<int> + T<string>) * !?(T<unit> ^-> T<unit>) ^-> TSelf
            "slideDown" => (T<int> + T<string>) * T<string> * !?(T<unit> ^-> T<unit>) ^-> TSelf
            "slideToggle" => AnimateSettings ^-> TSelf
            "slideToggle" => !?(T<int> + T<string>) * !?(T<unit> ^-> T<unit>) ^-> TSelf
            "slideToggle" => (T<int> + T<string>) * T<string> * !?(T<unit> ^-> T<unit>) ^-> TSelf
            "slideUp" => AnimateSettings ^-> TSelf
            "slideUp" => !?(T<int> + T<string>) * !?(T<unit> ^-> T<unit>) ^-> TSelf
            "slideUp" => (T<int> + T<string>) * T<string> * !?(T<unit> ^-> T<unit>) ^-> TSelf
            "stop" => !?T<bool> * !?T<bool> ^-> TSelf
            "stop" => T<string> * !?T<bool> * !?T<bool> ^-> TSelf
            "toggle" => T<bool> ^-> TSelf
            "toggle" => AnimateSettings ^-> TSelf
            "toggle" => !?(T<int> + T<string>) * !?(T<unit> ^-> T<unit>) ^-> TSelf
            "toggle" => (T<int> + T<string>) * T<string> * !?(T<unit> ^-> T<unit>) ^-> TSelf

            // Events
            "blur" => !?T<obj> * EH ^-> TSelf
            "blur" => T<unit> ^-> TSelf
            "change" => !?T<obj> * EH ^-> TSelf
            "change" => T<unit> ^-> TSelf
            "click" => !?T<obj> * EH ^-> TSelf
            "click" => T<unit> ^-> TSelf
            "contextmenu" => !?T<obj> * EH ^-> TSelf
            "contextmenu" => T<unit> ^-> TSelf
            "dblclick" => !?T<obj> * EH ^-> TSelf
            "dblclick" => T<unit> ^-> TSelf
            "focus" => !?T<obj> * EH ^-> TSelf
            "focus" => T<unit> ^-> TSelf
            "focusin" => !?T<obj> * EH ^-> TSelf
            "focusin" => T<unit> ^-> TSelf
            "focusout" => !?T<obj> * EH ^-> TSelf
            "focusout" => T<unit> ^-> TSelf
            "hover" => EH ^-> TSelf
            "hover" => EH?handlerIn * EH?handlerOut ^-> TSelf
            "keydown" => !?T<obj> * EH ^-> TSelf
            "keydown" => T<unit> ^-> TSelf
            "keypress" => !?T<obj> * EH ^-> TSelf
            "keypress" => T<unit> ^-> TSelf
            "keyup" => !?T<obj> * EH ^-> TSelf
            "keyup" => T<unit> ^-> TSelf
            "load" => EH?eventHandler ^-> TSelf
            |> WithInteropInline (fun tr -> "$this.on(\"load\", " + tr "eventHandler" + ")")
            |> ObsoleteWithMessage "Use .On(\"load\", eventHandler) instead" 
            "unload" => EH?eventHandler ^-> TSelf
            |> WithInteropInline (fun tr -> "$this.on(\"unload\", " + tr "eventHandler" + ")")
            |> ObsoleteWithMessage "Use .On(\"load\", eventHandler) instead" 
            "mousedown" => !?T<obj> * EH ^-> TSelf
            "mousedown" => T<unit> ^-> TSelf
            "mouseenter" => !?T<obj> * EH ^-> TSelf
            "mouseenter" => T<unit> ^-> TSelf
            "mouseleave" => !?T<obj> * EH ^-> TSelf
            "mouseleave" => T<unit> ^-> TSelf
            "mousemove" => !?T<obj> * EH ^-> TSelf
            "mousemove" => T<unit> ^-> TSelf
            "mouseout" => !?T<obj> * EH ^-> TSelf
            "mouseout" => T<unit> ^-> TSelf
            "mouseover" => !?T<obj> * EH ^-> TSelf
            "mouseover" => T<unit> ^-> TSelf
            "mouseup" => !?T<obj> * EH ^-> TSelf
            "mouseup" => T<unit> ^-> TSelf
            "off" => T<unit> ^-> TSelf
            "off" => T<string> * !?T<string> * !?EH ^-> TSelf
            "off" => T<Object<_>>.[EH] * !?T<string> ^-> TSelf
            "off" => Event ^-> TSelf
            "on" => T<string> * !?T<string> * !?T<obj> * EH ^-> TSelf
            "on" => T<Object<_>>.[EH] * !?T<string> * !?T<obj> ^-> TSelf
            "one" => T<string> * !?T<string> * !?T<obj> * EH ^-> TSelf
            "one" => T<Object<_>>.[EH] * !?T<string> * !?T<obj> ^-> TSelf
            "ready" => (T<unit> ^-> T<unit>) ^-> TSelf
            "resize" => !?T<obj> * EH ^-> TSelf
            "resize" => T<unit> ^-> TSelf
            "scroll" => !?T<obj> * EH ^-> TSelf
            "scroll" => T<unit> ^-> TSelf
            "select" => !?T<obj> * EH ^-> TSelf
            "select" => T<unit> ^-> TSelf
            "size" => T<unit> ^-> T<int>
            |> WithInline "$this.length"
            "submit" => !?T<obj> * EH ^-> TSelf
            "submit" => T<unit> ^-> TSelf
            "trigger" => T<string> * !?(Type.ArrayOf T<obj>) ^-> TSelf
            "triggerHandler" => T<string> * !?(Type.ArrayOf T<obj>) ^-> TSelf

            // Internals
            "jquery" =? T<string>
            "pushStack" => (Type.ArrayOf T<Dom.Node>) ^-> TSelf
            "pushStack" => (Type.ArrayOf T<Dom.Node>) * T<string> * (Type.ArrayOf T<obj>) ^-> TSelf

            // Manipulation
            "after" => Content *+ T<obj> ^-> TSelf 
            "after" => Func ^-> TSelf
            "after" => FuncWithHTML ^-> TSelf
            "append" => Content *+ T<obj> ^-> TSelf
            "append" => FuncWithHTML ^-> TSelf
            "appendTo" => Content ^-> TSelf
            "before" => Content *+ T<obj> ^-> TSelf
            "before" => Func ^-> TSelf
            "before" => FuncWithHTML ^-> TSelf
            "clone" => T<unit> ^-> TSelf
            "clone" => T<bool>?withDataAndEvents ^-> TSelf
            "clone" => T<bool>?withDataAndEvents * T<bool>?deepWithDataAndEvents ^-> TSelf
            "detach" => !?T<string> ^-> TSelf
            "empty" => T<unit> ^-> TSelf
            "insertAfter" => Content ^-> TSelf
            "insertBefore" => Content ^-> TSelf
            "prepend" => Content *+ T<obj> ^-> TSelf
            "prepend" => FuncWithHTML ^-> TSelf
            "prependTo" => Content ^-> TSelf
            "remove" => !?T<string> ^-> TSelf
            "replaceAll" => Content ^-> TSelf
            "replaceWith" => Content ^-> TSelf
            "replaceWith" => (T<Dom.Element> -* Type.Parameters.Empty ^-> Content) ^-> TSelf
            "text" => T<unit> ^-> T<string>
            "text" => T<string> ^-> TSelf
            "text" => ((T<int> + T<string>) ^-> T<string>) ^-> TSelf
            "unwrap" => T<unit> ^-> TSelf
            "unwrap" => T<string> ^-> TSelf
            "wrap" => (T<string> + T<Dom.Element> + TSelf) ^-> TSelf
            "wrap" => (T<int> ^-> TSelf) ^-> TSelf
            "wrap" => (T<int> ^-> T<string>) ^-> TSelf
            "wrapAll" => (T<string> + T<Dom.Element> + TSelf) ^-> TSelf
            "wrapAll" => (T<unit> ^-> TSelf) ^-> TSelf
            "wrapAll" => (T<unit> ^-> T<string>) ^-> TSelf
            "wrapInner" => (T<string> + T<Dom.Element> + TSelf) ^-> TSelf
            "wrapInner" => (T<int> ^-> T<string>) ^-> TSelf

            // Miscellaneous
            "each" => (T<int> * T<Dom.Element> ^-> T<unit>) ^-> TSelf
            "get" => T<unit> ^-> Type.ArrayOf T<Dom.Element>
            "get" => T<int> ^-> T<Dom.Element>
            "index" => (T<unit> + T<string> + T<Dom.Element> + TSelf) ^-> T<int>
            "toArray" => T<unit> ^-> Type.ArrayOf T<Dom.Element>

            // Offset
            "offsetParent" => T<unit> ^-> TSelf

            // Properties
            "length" =? T<int>

            // Traversing
            "add" => T<string> + T<Dom.Node> + Type.ArrayOf T<Dom.Node> + TSelf ^-> TSelf
            "add" => T<string> * T<Dom.Node> ^-> TSelf
            "addBack" => !?T<string> ^-> TSelf
            "children" => !?T<string> ^-> TSelf
            "closest" => T<string> * !?T<Dom.Element> ^-> TSelf
            "closest" => T<Dom.Element> ^-> TSelf
            "closest" => TSelf ^-> TSelf
            "contents" => T<unit> ^-> TSelf
            "end" => T<unit> ^-> TSelf
            "eq" => T<int> ^-> TSelf
            "filter" => T<string> ^-> TSelf
            "filter" => (T<int> * T<Dom.Element> ^-> T<bool>) ^-> TSelf
            "filter" => T<Dom.Element> ^-> TSelf
            "filter" => TSelf ^-> TSelf
            "find" => T<string> ^-> TSelf
            "find" => T<Dom.Node> ^-> TSelf
            "find" => TSelf ^-> TSelf
            "first" => T<unit> ^-> TSelf
            "has" => (T<string> + T<Dom.Node>) ^-> TSelf
            "is" => T<string> ^-> T<bool>
            "is" => (T<int> * T<Dom.Element> ^-> T<bool>) ^-> T<bool>
            "is" => T<Dom.Node> ^-> T<bool>
            "is" => TSelf ^-> T<bool>
            "last" => T<unit> ^-> TSelf
            Generic - (fun t  -> Method "map" ((T<int> * T<Dom.Element> ^-> t) ^-> TSelf))
            "next" => !?T<string> ^-> TSelf
            "nextAll" => !?T<string> ^-> TSelf
            "nextUntil" => T<unit> ^-> TSelf
            "nextUntil" => T<string> * !?T<string> ^-> TSelf
            "nextUntil" => (T<Dom.Node> + TSelf) * !?T<string> ^-> TSelf
            "not" => T<string> ^-> TSelf
            "not" => (T<int> * T<Dom.Element> ^-> T<bool>) ^-> TSelf
            "not" => T<Dom.Node> ^-> TSelf
            "not" => (Type.ArrayOf T<Dom.Node>) ^-> TSelf
            "not" => TSelf ^-> TSelf
            "parent" => !?T<string> ^-> TSelf
            "parents" => !?T<string> ^-> TSelf
            "parentsUntil" => T<unit> ^-> TSelf
            "parentsUntil" => T<string> * !?T<string> ^-> TSelf
            "parentsUntil" => (T<Dom.Node> + TSelf) * !?T<string> ^-> TSelf
            "prev" => !?T<string> ^-> TSelf
            "prevAll" => !?T<string> ^-> TSelf
            "prevUntil" => T<unit> ^-> TSelf
            "prevUntil" => T<string> * !?T<string> ^-> TSelf
            "prevUntil" => (T<Dom.Node> + TSelf) * !?T<string> ^-> TSelf
            "siblings" => !?T<string> ^-> TSelf
            "slice" => T<int> * !?T<int> ^-> TSelf
        ]
        |+> Static [
            // Core
            Constructor (T<string>?selector)
            |> WithInline "jQuery($0)"
            |> WithComment "Accepts a string containing a CSS selector which is then used to match a set of elements."

            Constructor (T<string>?selector * Context?context)
            |> WithInline "jQuery($0, $1)"
            |> WithComment "Accepts a string containing a CSS selector and a DOM Element, Document, or jQuery to use as context."

            Constructor (T<Dom.Node>?element)
            |> WithInline "jQuery($0)"
            |> WithComment "Accepts a DOM element to wrap in a jQuery object."

            Constructor (T<Dom.Node []>?elementArray)
            |> WithInline "jQuery($0)"
            |> WithComment "Accepts an array containing a set of DOM elements to wrap in a jQuery object."

            Constructor (T<obj>?``object``)
            |> WithInline "jQuery($0)"
            |> WithComment "Accepts a plain object to wrap in a jQuery object."

            Constructor (TSelf?selection)
            |> WithInline "jQuery($0)"
            |> WithComment "Accepts an existing jQuery object to clone."

            Constructor (T<unit>)
            |> WithInline "jQuery()"
            |> WithComment "This signature does not accept any arguments."

            Constructor (T<string>?html * T<Dom.Document>?ownerDocument)
            |> WithInline "jQuery($0, $1)"
            |> WithComment "Creates DOM elements on the fly from the provided string of raw HTML."

            Constructor (T<string>?html * T<obj>?attributes)
            |> WithInline "jQuery($0, $1)"
            |> WithComment "Creates DOM elements on the fly from the provided string of raw HTML."

            Constructor ((T<unit> ^-> T<unit>)?callback)
            |> WithInline "jQuery($0, $1)"
            |> WithComment "Binds a function to be executed when the DOM has finished loading."

            "of" => (T<string>?selector) ^-> TSelf
            |> WithInline "jQuery($0)"
            |> WithComment "Accepts a string containing a CSS selector which is then used to match a set of elements."

            "of" => (T<string>?selector * Context?context) ^-> TSelf
            |> WithInline "jQuery($0, $1)"
            |> WithComment "Accepts a string containing a CSS selector and a DOM Element, Document, or jQuery to use as context."

            "of" => (T<Dom.Node>?element) ^-> TSelf
            |> WithInline "jQuery($0)"
            |> WithComment "Accepts a DOM element to wrap in a jQuery object."

            "of" => (T<Dom.Node []>?elementArray) ^-> TSelf
            |> WithInline "jQuery($0)"
            |> WithComment "Accepts an array containing a set of DOM elements to wrap in a jQuery object."

            "of" => (T<obj>?``object``) ^-> TSelf
            |> WithInline "jQuery($0)"
            |> WithComment "Accepts a plain object to wrap in a jQuery object."

            "of" => (TSelf?selection) ^-> TSelf
            |> WithInline "jQuery($0)"
            |> WithComment "Accepts an existing jQuery object to clone."

            "of" => (T<unit>) ^-> TSelf
            |> WithInline "jQuery()"
            |> WithComment "This signature does not accept any arguments."

            "of" => (T<string>?html * T<Dom.Document>?ownerDocument) ^-> TSelf
            |> WithInline "jQuery($0, $1)"
            |> WithComment "Creates DOM elements on the fly from the provided string of raw HTML."

            "of" => (T<string>?html * T<obj>?attributes) ^-> TSelf
            |> WithInline "jQuery($0, $1)"
            |> WithComment "Creates DOM elements on the fly from the provided string of raw HTML."

            "of" => ((T<unit> ^-> T<unit>)?callback) ^-> TSelf
            |> WithInline "jQuery($0, $1)"
            |> WithComment "Binds a function to be executed when the DOM has finished loading."

            "holdReady" => T<bool> ^-> T<obj>
            "noConflict" => !?T<bool> ^-> T<obj>
            "readyException" => Error ^-> T<string>
            "when" => Deferred ^-> Promise

            // Ajax related static methods
            "ajax" => T<string> * !? AjaxSettings ^-> JqXHR
            "ajax" => !?AjaxSettings ^-> JqXHR
            "ajaxPrefilter" => !?DataType * (AjaxSettings * T<obj> * JqXHR ^-> T<unit>) ^-> T<obj> // returns undefined
            "ajaxSetup" => AjaxSettings ^-> T<unit>
            "ajaxTransport" => DataType * (AjaxSettings * T<obj> * JqXHR ^-> T<unit>) ^-> T<obj> // returns undefined
            "get" => (T<string> * !?(T<obj> + T<string>) * !? (T<obj> * T<string> * JqXHR ^-> T<unit>) * !? DataType) ^-> JqXHR
            "get" => !?AjaxSettings ^-> JqXHR
            "getJSON" => T<string> * !?((T<obj> * T<string> ^-> T<unit>) + (T<obj> * T<string> * JqXHR ^-> T<unit>)) ^-> JqXHR
            "getJSON" => T<string> * (T<obj> + T<string>) * !?((T<obj> * T<string> ^-> T<unit>) + (T<obj> * T<string> * JqXHR ^-> T<unit>)) ^-> JqXHR
            "getScript" => (T<string> * !? (T<string> * T<string> * JqXHR ^-> T<unit>)) ^-> JqXHR
            "param" => (T<obj> * T<obj []> * TSelf) * !?T<bool> ^-> T<string>
            "post" => (T<string> * !?(T<obj> + T<string>) * !?(T<obj> * T<string> * JqXHR ^-> T<unit>) * !? DataType) ^-> JqXHR
            "post" => !?AjaxSettings ^-> JqXHR

            // Callback related static method
            "Callbacks" => T<string> ^-> Callbacks

            // CSS related static methods/properties
            "cssHooks" =@ T<obj>
            "cssNumber" =@ T<Object<int>>
            "escapeSelector" => T<string> ^-> T<string>

            // Data related static methods
            "data" => T<Dom.Element> * T<string> * T<obj> ^-> TSelf
            "data" => T<Dom.Element>* T<obj> ^-> TSelf
            "data" => T<Dom.Element> * T<string> ^-> T<obj>
            "data" => T<Dom.Element> * T<obj>
            "dequeue" => T<Dom.Element> * !?T<string> ^-> TSelf
            "hasData" => T<Dom.Element> ^-> T<bool>
            "queue" => T<Dom.Element> * !?T<string> ^-> T<obj []>
            "queue" => T<Dom.Element> * T<string> * (T<obj []> + ((T<unit> ^-> T<unit>) ^-> T<unit>)) ^-> TSelf
            "removeData" => T<Dom.Element> * (T<string> + T<string []>) ^-> TSelf

            // Deferred
            "Deferred" => (T<unit> ^-> T<unit>) ^-> Deferred

            // Effects
            "speed" => (T<int> + T<string>) * Speed ^-> T<obj>
            "speed" => (T<int> + T<string>) * T<string> * (T<unit> ^-> T<unit>) ^-> T<obj>
            "speed" => Speed ^-> T<obj>

            "fx" =? FX

            // Events
            "proxy" => (T<unit> ^-> T<unit>) * T<obj> * !?(Type.ArrayOf T<obj>) ^-> (Event ^-> T<unit>)
            "proxy" => T<obj> * T<string> * !?(Type.ArrayOf T<obj>) ^-> (Event ^-> T<unit>)
        
            // Internals
            "error" => T<string> ^-> T<unit>

            // Manipulation
            "htmlPrefilter" => T<string> ^-> T<string>
            
            // Utilities
            "contains" => T<Dom.Element> * T<Dom.Element> ^-> T<bool>
            "each" => (Type.ArrayOf T<obj>) * (T<int> * T<obj> ^-> T<unit>) ^-> T<obj>
            "each" => (T<obj>) * (T<string> * T<obj> ^-> T<unit>) ^-> T<obj>
            "extend" => T<obj> *+ T<obj> ^-> T<obj>
            "extend" => T<bool> * T<obj> * T<obj> *+ T<obj> ^-> T<obj>
            "globalEval" => T<string> ^-> T<unit>
            |> WithComment "Execute some JavaScript code globally"
            "grep" => (T<obj> + Type.ArrayOf T<obj>) * (T<obj> * T<int> ^-> T<bool>) * T<bool> ^-> Type.ArrayOf T<obj>
            "inArray" => T<obj> * Type.ArrayOf T<obj> * !?T<int> ^-> T<int>
            "isEmptyObject" => T<obj> ^-> T<bool>
            "isFunction" => T<obj> ^-> T<bool>
            "isNumeric" => T<obj> ^-> T<bool>
            "isPlainObject" => T<obj> ^-> T<bool>
            "isWindow" => T<obj> ^-> T<bool>
            "isXMLDoc" => T<Dom.Element> ^-> T<bool>
            "makeArray" => T<obj> ^-> Type.ArrayOf T<obj>
            "map" => (Type.ArrayOf T<obj>) * (T<obj> * T<int> ^-> T<unit>) ^-> Type.ArrayOf T<obj>
            "map" => (T<obj>) * (T<string> * T<obj> ^-> T<unit>) ^-> Type.ArrayOf T<obj>
            "merge" => (T<obj> + Type.ArrayOf T<obj>) * (T<obj> + Type.ArrayOf T<obj>) ^-> Type.ArrayOf T<obj>
            "noop" => T<unit> ^-> T<obj>
            "now" => T<unit> ^-> T<int>
            "parseHTML" => T<string> ^-> Type.ArrayOf T<Dom.Node>
            "parseJSON" => T<string> ^-> T<obj> |> ObsoleteWithMessage "Use the native JSON.parse instead"
            "parseXML" => T<string> ^-> T<Dom.Document>
            "trim" => T<string> ^-> T<string>
            "type" => T<obj> ^-> T<string>
            "unique" => Type.ArrayOf T<Dom.Element> ^-> Type.ArrayOf T<Dom.Element> |> ObsoleteWithMessage "Use UniqueSort instead"
            "uniqueSort" => Type.ArrayOf T<Dom.Element> ^-> Type.ArrayOf T<Dom.Element>
        ]
        
        
    let Assembly =
        Assembly [
            Namespace "WebSharper.JQuery" [
                JQueryClass
                JqXHR
                Callbacks
                AjaxSettings
                RequestType
                DataType
                Promise
                Error
                Deferred
                DeferredState
                AnimateSettings
                Event
                FX
                Speed
                Position
            ]
            Namespace "WebSharper.JQuery.Resources" [
                Resource "JQuery" "https://code.jquery.com/jquery-3.1.1.min.js" |> AssemblyWide
            ]
        ]

[<Sealed>]
type JQueryExtension() =
    interface IExtension with
        member x.Assembly = Definition.Assembly

[<assembly: Extension(typeof<JQueryExtension>)>]
do ()
