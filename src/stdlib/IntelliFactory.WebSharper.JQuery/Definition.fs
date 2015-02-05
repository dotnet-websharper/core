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

namespace IntelliFactory.WebSharper.JQuery

open IntelliFactory.WebSharper.JavaScript
open IntelliFactory.WebSharper.InterfaceGenerator

module Definition =

    let JQ = Type.New()

    // TODO: Where to define??
    let Error = Type.New ()
    // TODO: Where to define???
    let XmlHttpRequest = Type.New ()

    // Event
    let Event = Type.New()
    let EventClass =
        Class "Event"
        |=> Event
        |+> Instance
                [
                    "currentTarget" =? T<Dom.Element>
                    "absurd" => T<Dom.Node> ^-> T<unit>
                    "delegateTarget" =? T<Dom.Element>
                    "data" =? T<obj>
                    "isDefaultPrevented" => T<unit->bool>
                    "isImmediatePropagationStopped" => T<unit->bool>
                    "isPropagationStopped" => T<unit->bool>
                    "namespace" =? T<string>
                    "pageX" =? T<int>
                    "pageY" =? T<int>
                    "preventDefault" => T<unit->unit>
                    "relatedTarget" =? T<Dom.Element>
                    "result" =? T<obj>
                    "stopImmediatePropagation" => T<unit->unit>
                    "stopPropagation" => T<unit->unit>
                    "target" =? T<Dom.Element>
                    "timeStamp" =? T<int>
                    "eventType" =? T<obj>
                    "which" =? T<int>
            ]

    /// Request type
    let RequestType = Type.New()

    let RequestTypeClass =
        "GET POST PUT DELETE".Split ' '
        |> Pattern.EnumStrings "RequestType"
        |=> RequestType

    /// Data type
    let DataType = Type.New()
    let DataTypeClass =
        "xml html script json jsonp text".Split ' '
        |> Pattern.EnumStrings "DataType"
        |=> DataType

    /// Support
    let Support = Type.New()
    let SupportClass =
        let fields =
            "boxModel cssFloat hrefNormalized htmlSerialize \
             leadingWhitespace noCloneEvent objectAll opacity \
             scriptEval style tbody".Split ' '
        let props =
            [for f in fields ->
                f =? T<bool> :> CodeModel.IClassMember
            ]
        Class "Support"
        |=> Support
        |+> Instance props

    let JqXHR = Type.New ()

    /// Ajax configuration
    let AjaxSettings =
        let AjaxSettings = Type.New ()
        
        Pattern.Config "AjaxSettings" {
            Required = []
            Optional =
                [
                    "accepts" , T<obj>
                    "async" , T<bool>
                    "beforeSend" ,  JqXHR * !? AjaxSettings ^-> T<bool>
                    "cache" , T<bool>
                    // 1.5 allows _also_ an array of functions. We can't have both so the array version is 
                    // preferred.
                    "complete" , JqXHR * T<string> ^-> T<unit>
                    "contents", T<Object<string>>
                    "contentType" ,  T<string>
                    "context" , T<obj>
                    "converters", T<Object<string -> obj>>
                    "crossDomain", T<bool>
                    "data" , T<Object<obj>>
                    "dataFilter" , T<string> * DataType ^-> T<obj>
                    "dataType" , DataType
                    // See complete's comment.
                    "error" , JqXHR * T<string> * T<string> ^-> T<unit>
                    "global" , T<bool>
                    "headers", T<Object<string>>
                    "ifModified" , T<bool>
                    "isLocal" , T<bool>
                    "jsonp" , T<string>
                    "jsonpCallback" , T<string>
                    "mimeType", T<string>
                    "password" , T<string>
                    "processData" , T<bool>
                    "scriptCharset" , T<string>
                    "statusCode", T<Object<unit -> unit>>
                    // See complete's comment.
                    "success" , T<obj> * T<string> * JqXHR ^-> T<unit>
                    "timeout" , T<int>
                    "traditional" , T<bool>
                    "type" , RequestType
                    "url" , T<string>
                    "username" , T<string>
                    "xhr" , T<unit> ^-> XmlHttpRequest
                ]
        }
        |=> AjaxSettings

    let StringMap = Type.New()

    // Position
    let Position = Type.New()
    let PositionClass =
        Pattern.Config "Position" {
            Required = []
            Optional =
                [
                    "top", T<int>
                    "left" , T<int>
                ]
        }
        |=> Position

    // Animate options
    let AnimateConfig = Type.New()
    let AnimateConfigClass =
        Pattern.Config "AnimateConfig" {
            Required = []
            Optional =
                [
                    "duration", (T<int> + T<string>)
                    "easing" , T<string>
                    "complete" , T<unit -> unit>
                    "step" , T<unit -> unit>
                    "queue" , T<bool>
                    "specialEasing" , StringMap
                ]
        }
        |=> AnimateConfig


    /// Abbreviations
    let Content = T<Dom.Element> + T<string> + JQ
    let IntString = T<int> + T<string>
    let FloatString = T<float> + T<string>
    let UnitCallback = (T<unit> ^-> T<unit>)
    let AjaxHandler =
        (
            T<Dom.Element> -*
            Event?event *

            XmlHttpRequest?request *
            AjaxSettings?config ^->
            T<unit>
        ) ^-> JQ

    let AjaxErrorHandler = Type.New ()
//    let AjaxErrorHandler =
//        (
//            T<Dom.Element> -*
//            Event?event *
//            XmlHttpRequest?request *
//            AjaxConfig?config *
//            !?Error?error ^->
//            T<unit>
//        ) ^-> JQ

    let EventHandler =
        (
            T<Dom.Element>  -*
            Event?event
        ) ^-> T<unit>

    let AddCmt = "Add elements to the set of matched elements."
    let AddClassCmt = "Adds the specified class(es) to each of the set of matched elements."
    let AfterCmt = "Insert content, specified by the parameter, after each element in the set of matched elements."
    let AjaxCompleteCmt = "Register a handler to be called when Ajax requests complete. This is an Ajax Event."
    let AnimateCmt = "Perform a custom animation of a set of CSS properties."
    let AppendCmt = "Insert content, specified by the parameter, to the end of each element in the set of matched elements."
    let BeforeCmt = " Insert content, specified by the parameter, before each element in the set of matched elements."
    let CssCmt = "Get the value of a style property for the first element in the set of matched elements."
    let DataCmt = "Store arbitrary data associated with the matched elements."
    let IndexCmt = "Search for a given element from among the matched elements."
    let FilterCmt = "Reduce the set of matched elements to those that match the selector or pass the function's test."
    let GetCmt = "Retrieve the DOM elements matched by the jQuery object."
    let HeightCmt = "Get the current computed height for the first element in the set of matched elements."

    let FX =
        Class "jQuery.fx"
        |+> Static [
            "off" =@ T<bool>
            |> WithComment "Globally disable all animations."

            "interval" =@ T<int>
            |> WithComment "The rate (in milliseconds) at which animations fire."
        ]
    
    let Promise =
        let Promise = Type.New ()

        Class "Promise"
        |+> Instance [
            "always" => (!+ (!+ T<obj> ^-> T<unit>)) ^-> Promise
            "done" => (!+ (!+ T<obj> ^-> T<unit>)) ^-> Promise
            "fail" => (!+ (!+ T<obj> ^-> T<unit>)) ^-> Promise
            "state" => T<unit -> string>
            "then" => T<unit -> unit> * !? T<unit -> unit> * !? T<unit -> unit> ^-> Promise
        ]

    let Deferred =
        let Deferred = Type.New ()

        Class "jQuery.Deferred"
        |+> Static [
            Constructor (!? Deferred ^-> T<unit>)
        ]
        |+> Instance [
            "always" => (!+ (!+ T<obj> ^-> T<unit>)) ^-> Deferred
            |> WithComment "Add handlers to be called when the Deferred object is either resolved or rejected."

            "done" => (!+ (!+ T<obj> ^-> T<unit>)) ^-> Deferred
            |> WithComment "Add handlers to be called when the Deferred object is resolved."

            "fail" => (!+ (!+ T<obj> ^-> T<unit>)) ^-> Deferred
            |> WithComment "Add handlers to be called when the Deferred object is rejected."

            "notify" => !+ T<obj> ^-> Deferred
            |> WithComment "Call the progressCallbacks on a Deferred object with the given args."

            "notifyWith" => T<obj> *+ T<obj> ^-> Deferred
            |> WithComment "Call the progressCallbacks on a Deferred object with the given context and args."

            "progress" => (!+ (!+ T<obj> ^-> T<unit>)) ^-> Deferred
            |> WithComment "Add handlers to be called when the Deferred object generates progress notifications."

            "promise" => !? T<obj> ^-> Promise
            |> WithComment "Return a Deferred's Promise object."

            "reject" => !+ T<obj> ^-> Deferred
            |> WithComment "Reject a Deferred object and call any failCallbacks with the given args."

            "rejectWith" => T<obj> *+ T<obj> ^-> Deferred
            |> WithComment "Reject a Deferred object and call any failCallbacks with the given context and args."

            "resolve" => !+ T<obj> ^-> Deferred
            |> WithComment "Resolve a Deferred object and call any doneCallbacks with the given args."

            "resolveWith" => T<obj> *+ T<obj> ^-> Deferred
            |> WithComment "Resolve a Deferred object and call any doneCallbacks with the given context and args."

            "state" => T<unit -> string>
            |> WithComment "Determine the current state of a Deferred object."

            "then" => T<unit -> unit> * !? T<unit -> unit> * !? T<unit -> unit> ^-> Promise
            |> WithComment "Add handlers to be called when the Deferred object is resolved, rejected, or still in progress."

            "when" => !+ T<obj> ^-> Promise
            |> WithComment "Provides a way to execute callback functions based on one or more objects, usually Deferred objects that represent asynchronous events."
        ]

    let JqXHRClass =
        Class "jqXHR"
        |=> Inherits Deferred
        |+> Instance [
            // The documentation isn't clear about the types of each of the functions.
            "readyState" =? T<bool> 
            "statusText" =? T<string>
            "responseXML" =? T<string> 
            "responseText" =? T<string>
            "setRequestHeader" => T<string> * T<obj> ^-> T<unit>
            "getAllResponseHeaders" => T<unit> ^-> T<obj>
            "getResponseHeader" => T<unit> ^-> T<obj>
            "abort" => T<unit> ^-> T<unit>
            "success" => (T<obj> * T<string> * XmlHttpRequest ^-> T<unit>) ^-> JqXHR
            "error" => (XmlHttpRequest * T<string> * Error ^-> T<unit>) ^-> JqXHR
            "complete" => (XmlHttpRequest * T<string> ^-> T<unit>) ^-> JqXHR

        ]
        |=> JqXHR

    /// JQuery class
    let JQueryClass =
        Class "jQuery"
        |=> JQ
        |+> Instance
            [
                "ignore" =? T<unit>
                |> WithGetterInline "$this"

                // Add (Tested)
                "add" => T<string> ^-> JQ
                |> WithComment AddCmt
                "add" => T<Dom.Element> ^-> JQ
                |> WithComment AddCmt
                "add" => Type.ArrayOf T<Dom.Element> ^-> JQ
                |> WithComment AddCmt

                // Add class (Tested)
                "addClass" => T<string>?className ^-> JQ
                |> WithComment AddClassCmt
                "addClass" => (T<int> *  T<string> ^-> T<string>)?nameGen ^-> JQ
                |> WithComment AddClassCmt

                // After (Tested)
                "after" => (Content + (T<int> ^-> T<string>)) ^-> JQ
                |> WithComment AfterCmt

                "ajaxComplete" => AjaxHandler ^-> JQ
                |> WithComment AjaxCompleteCmt

                "ajaxError" => AjaxErrorHandler ^-> JQ

                "ajaxSend" => AjaxHandler ^-> JQ

                "ajaxStart" => AjaxHandler ^-> JQ

                "ajaxSuccess" => AjaxHandler ^-> JQ

                "andSelf" => T<unit> ^-> JQ

                // Animate (Tested)
                "animate" => StringMap?properties * AnimateConfig?options ^-> JQ
                |> WithComment AnimateCmt
                "animate" => StringMap * IntString?duration ^-> JQ
                |> WithComment AnimateCmt
                "animate" => StringMap * IntString?duration * T<string>?easing ^-> JQ
                |> WithComment AnimateCmt
                "animate" => StringMap * IntString?duration * UnitCallback?callback ^-> JQ
                |> WithComment AnimateCmt
                "animate" => StringMap * IntString?duration * T<string>?easing * UnitCallback?callback ^-> JQ
                |> WithComment AnimateCmt

                // Append (Tested)
                "append" => Content?content ^-> JQ
                |> WithComment AppendCmt
                "append" => (T<int> * T<string> ^-> T<string>)?contentGen ^-> JQ
                |> WithComment AppendCmt


                // AppendTo (Tested)
                "appendTo" => Content ^-> JQ
                |> WithComment "Insert every element in the set of matched elements to the end of the target."

                // Attr (Tested)
                "attr" => T<string>?attributeName ^-> T<string>
                |> WithComment "Get the value of an attribute for the first element in the set of matched elements."
                
                "attr" => T<string>?attributeName * T<string>?value ^-> JQ
                |> WithComment "Set the value of an attribute for the first element in the set of matched elements."

                Generic -   ( fun t ->
                                Method "prop" (T<string>?propName ^-> t)
                                |> WithComment "Get the value of a property for the first element in the set of matched elements."
                            )

                "removeProp" => T<string>?propName * T<string>?value ^-> JQ
                |> WithComment "Remove a property for the set of matched elements."

                "removeProp" => T<string>?propName ^-> JQ
                |> WithComment "Remove a property for the set of matched elements."

                Generic -   ( fun t ->
                                Method "prop" (T<string>?attributeName * t?value ^-> JQ)
                                |> WithComment "Set the value of a property for the first element in the set of matched elements."
                            )
                // Before
                "before" => Content?content ^-> JQ
                |> WithComment BeforeCmt
                "before" => (T<unit> ^-> T<string>)?contentGen ^-> JQ
                |> WithComment BeforeCmt

                // Bind (Tested)
                "bind" => T<string>?event * !?StringMap?eventData * EventHandler?handler ^-> JQ
                |> WithComment "Attach a handler to an event for the elements."

                "bindFalse" => T<string>?event * StringMap?eventData ^-> JQ
                |> WithComment "Attach a handler to an event for the elements."
                |> WithInline "$this.bind($event, $eventData, false)"


                // Blur (Tested)
                "blur" => !?EventHandler?handler ^-> JQ
                |> WithComment "Bind an event handler to the \"blur\" JavaScript event, or trigger that event on an element."

                "blur" => StringMap?eventData * EventHandler?handler ^-> JQ
                |> WithComment "Bind an event handler to the \"blur\" JavaScript event, or trigger that event on an element."


                // Change
                "change" => !?EventHandler?handler ^-> JQ
                |> WithComment "Bind an event handler to the \"change\" JavaScript event, or trigger that event on an element."

                "change" => StringMap?eventData * EventHandler?handler ^-> JQ
                |> WithComment "Bind an event handler to the \"change\" JavaScript event, or trigger that event on an element."


                // Children (Tested)
                "children" => !?T<string>?selector ^-> JQ
                |> WithComment "Get the children of each element in the set of matched elements, optionally filtered by a selector."

                // ClearQueue
                "clearQueue" => !?T<string>?selector ^-> JQ
                |> WithComment "Remove from the queue all items that have not yet been run."

                // Click
                "click" => !?EventHandler?handler ^-> JQ
                |> WithComment "Bind an event handler to the \"click\" JavaScript event, or trigger that event on an element."

                "click" => StringMap?eventData * EventHandler?handler ^-> JQ
                |> WithComment "Bind an event handler to the \"click\" JavaScript event, or trigger that event on an element."

                // Clone
                "clone" => !?T<bool>?withDataAndEvents ^-> JQ
                |> WithComment "Create a copy of the set of matched elements."

                // Closest
                "closest" => T<string>?selector * !?T<Dom.Element>?context ^-> JQ
                |> WithComment "Get the first ancestor element that matches the selector, beginning at the current element and progressing up through the DOM tree."

                // Contents
                "contents" => T<unit> ^-> JQ
                |> WithComment "Get the children of each element in the set of matched elements, including text nodes."

                // Context (Tested)
                "context" =? T<Dom.Element>
                |> WithComment "The DOM node context originally passed to jQuery(); if none was passed then context will likely be the document."

                // Css (Tested)
                "css" => T<unit> ^-> T<string>
                |> WithComment CssCmt
                "css" => T<string> ^-> T<string>
                |> WithComment CssCmt                
                "css" => T<string>?propertyName * T<string>?propertyValue ^-> JQ
                |> WithComment CssCmt
                "css" => T<string>?propertyName * (T<int> * T<string> ^-> T<string>)?valGen ^-> JQ
                |> WithComment CssCmt
                "css" => StringMap?map ^-> JQ
                |> WithComment CssCmt

                // Data
                "data" => T<string>?key * T<obj>?value ^-> JQ
                |> WithComment DataCmt
                "data" => StringMap?updates ^-> JQ
                |> WithComment DataCmt
                "data" => T<string>?key ^-> T<obj>
                |> WithComment DataCmt

                // Dblclick
                "dblclick" => !?EventHandler?handler ^-> JQ
                |> WithComment "Bind an event handler to the \"dblclick\" JavaScript event, or trigger that event on an element"

                "dblclick" => StringMap?data * EventHandler?handler ^-> JQ
                |> WithComment "Bind an event handler to the \"dblclick\" JavaScript event, or trigger that event on an element"

                // Delay
                "delay" => T<int>?duration * !?T<string>?queueName ^-> JQ
                |> WithComment "Set a timer to delay execution of subsequent items in the queue"

                // Delegate
                "delegate" => T<string>?selector * Event?eventType * !?StringMap?eventData * EventHandler?handler ^-> JQ
                |> WithComment "Attach a handler to one or more events for all elements that match the selector, now or in the future, based on a specific set of root elements."

                // Dequeue
                "dequeue" => !?T<string>?queueName ^-> JQ
                |> WithComment "Execute the next function on the queue for the matched elements."

                // Detach
                "detach" => !?T<string>?selector ^-> JQ
                |> WithComment "Remove the set of matched elements from the DOM."

                // Each
                "each" => (T<Dom.Element> -* !?T<int> ^-> T<unit>)?handler ^-> JQ
                |> WithComment "Iterate over a jQuery object, executing a function for each matched element."

                // Empty
                "empty" => T<unit> ^-> JQ
                |> WithComment "Remove all child nodes of the set of matched elements from the DOM."

                // End
                "end" => T<unit> ^-> JQ
                |> WithComment "End the most recent filtering operation in the current chain and return the set of matched elements to its previous state."

                // Eq
                "eq" => T<int> ^-> JQ
                |> WithComment "Reduce the set of matched elements to the one at the specified index."

                // Error
                "error" => EventHandler?handler ^-> JQ
                |> WithComment "Bind an event handler to the \"error\" JavaScript event."

                // Error
                "error" => StringMap?data * EventHandler?handler ^-> JQ
                |> WithComment "Bind an event handler to the \"error\" JavaScript event."

                // Fade in
                "fadeIn"  => !?FloatString?duration * !?UnitCallback?callback ^-> JQ
                |> WithComment "Display the matched elements by fading them to opaque."

                "fadeIn"  => FloatString?duration * T<string>?easing * !?UnitCallback?callback ^-> JQ
                |> WithComment "Display the matched elements by fading them to opaque."

                // FadeOut
                "fadeOut"  => !?FloatString?duration * !?UnitCallback?callback ^-> JQ
                |> WithComment "Display the matched elements by fading them to transparent."

                "fadeOut"  => FloatString?duration * T<string>?easing * !?UnitCallback?callback ^-> JQ
                |> WithComment "Display the matched elements by fading them to transparent."

                // FadeTo
                "fadeTo" => FloatString?duration * T<float>?value * T<string>?easing * !?UnitCallback?callback ^-> JQ
                |> WithComment "Adjust the opacity of the matched elements."
                "fadeTo" => FloatString?duration * T<float>?value * !?UnitCallback?callback ^-> JQ
                |> WithComment "Adjust the opacity of the matched elements."

                // FadeToggle
                "fadeToggle" => !? FloatString?duration * !?UnitCallback?callback ^-> JQ
                |> WithComment "Display or hide the matched elements by animating their opacity."
                "fadeToggle" => FloatString?duration * T<string>?easing* !?UnitCallback?callback ^-> JQ
                |> WithComment "Display or hide the matched elements by animating their opacity."

                // Filter
                "filter" => T<string>?selector ^-> JQ
                |> WithComment FilterCmt
                "filter" => (T<Dom.Element> -* !?T<int> ^-> T<bool>)?predicate ^-> JQ
                |> WithComment FilterCmt

                // Find
                "find" => T<string> ^-> JQ
                |> WithComment "Get the descendants of each element in the current set of matched elements, filtered by a selector."

                // First
                "first" => T<unit> ^-> JQ
                |> WithComment "Reduce the set of matched elements to the first in the set."

                // Focus
                "focus" => !?EventHandler?handler ^-> JQ
                |> WithComment "Bind an event handler to the \"focus\" JavaScript event, or trigger that event on an element."
                "focus" => StringMap?data * EventHandler?handler ^-> JQ
                |> WithComment "Bind an event handler to the \"focus\" JavaScript event, or trigger that event on an element."


                // Focusin
                "focusin" => EventHandler?handler ^-> JQ
                |> WithComment "Bind an event handler to the \"focusin\" JavaScript event."
                "focusin" => StringMap?data * EventHandler?handler ^-> JQ
                |> WithComment "Bind an event handler to the \"focusin\" JavaScript event."

                // Focusout
                "focusout" => EventHandler?handler ^-> JQ
                |> WithComment "Bind an event handler to the \"focusout\" JavaScript event."
                "focusout" => StringMap?data * EventHandler?handler ^-> JQ
                |> WithComment "Bind an event handler to the \"focusout\" JavaScript event."

                // Get
                "get" => T<int>?index ^-> T<Dom.Node>
                |> WithComment GetCmt
                "get" => T<unit> ^-> Type.ArrayOf T<Dom.Node>
                |> WithComment GetCmt

                // Has
                "has" => (T<string>?selector * T<Dom.Element>?element) ^-> JQ
                |> WithComment "Reduce the set of matched elements to those that have a descendant that matches the selector or DOM element."

                // HasClass
                "hasClass" => T<string>?className ^-> T<bool>
                |> WithComment "Determine whether any of the matched elements are assigned the given class."

                // Height
                "height" => T<unit> ^-> T<int>
                |> WithComment HeightCmt
                "height" => T<int> ^-> JQ
                |> WithComment HeightCmt


                // Hide (Tested)
                "hide" => !?IntString?duration * !?(T<Dom.Element> -* T<unit> ^-> T<unit>)?handler  ^-> JQ
                |> WithComment "Hide the matched elements."
                "hide" => IntString?duration * T<string>?easing * !?(T<Dom.Element> -* T<unit> ^-> T<unit>)?handler  ^-> JQ
                |> WithComment "Hide the matched elements."

                // Hover
                "hover" => EventHandler?handlerIn * !? EventHandler?handlerOut ^-> JQ
                |> WithComment "Bind two handlers to the matched elements, to be executed when the mouse pointer enters and leaves the elements."

                // Html
                "html" => T<unit> ^-> T<string>
                "html" => T<string> ^-> JQ

                // Index
                "index" => T<unit> ^-> T<int>
                |> WithComment IndexCmt

                "index" => T<string>?selector ^-> T<int>
                |> WithComment IndexCmt

                "index" => T<Dom.Element>?element ^-> T<int>
                |> WithComment IndexCmt

                "innerHeight" => T<unit> ^-> T<int>

                "innerWidth" => T<unit> ^-> T<int>

                "insertAfter" => Content ^-> JQ

                "insertBefore" => Content ^-> JQ

                "is" => T<string> ^-> T<bool>

                "keydown" => !?EventHandler ^-> JQ
                "keydown" => StringMap?data * EventHandler ^-> JQ

                "keypress" => !?EventHandler ^-> JQ
                "keypress" => StringMap?data * EventHandler ^-> JQ

                "keyup" => !?EventHandler ^-> JQ
                "keyup" => StringMap?data * EventHandler ^-> JQ

                "last" => T<unit> ^-> JQ

                "length" =@ T<int>

                "load" => T<string>  * !?(T<string> + T<obj>) * !?(T<string> * T<string> * XmlHttpRequest ^-> T<unit>) ^-> JQ

                "load" => EventHandler ^-> JQ
                "load" => StringMap?data * EventHandler ^-> JQ

                Generic -   ( fun t ->
                                Method "map" ((T<int> * T<Dom.Element> ^-> t) ^-> JQ)
                            )
                
                "mousedown" => StringMap?data * EventHandler ^-> JQ
                "mousedown" => !?EventHandler ^-> JQ

                "mouseenter" => !?EventHandler ^-> JQ
                "mouseenter" => StringMap?data * EventHandler ^-> JQ
                
                "mouseleave" => !?EventHandler ^-> JQ
                "mouseleave" => StringMap?data * EventHandler ^-> JQ
                
                "mousemove" => !?EventHandler ^-> JQ
                "mousemove" => StringMap?data * EventHandler ^-> JQ
                
                "mouseout" => !?EventHandler ^-> JQ
                "mouseout" => StringMap?data * EventHandler ^-> JQ
                
                "mouseover" => !?EventHandler ^-> JQ
                "mouseover" => StringMap?data * EventHandler ^-> JQ

                "mouseup" => !?EventHandler ^-> JQ
                "mouseup" => StringMap?data * EventHandler ^-> JQ

                "next" => !?T<string> ^-> JQ

                "nextAll" => !?T<string> ^-> JQ

                "nextUntil" => !?T<string> ^-> JQ

                "not" => T<string> ^-> JQ
                "not" => T<Dom.Element> ^-> JQ
                "not" => Type.ArrayOf T<Dom.Element> ^-> JQ

                "not" => (T<Dom.Element> -* T<int> ^-> T<unit>) ^-> JQ

                "offset" => T<unit> ^-> Position
                "offset" => Position ^-> JQ

                "offsetParent" => T<unit> ^-> JQ

                "one" => T<string> * !?T<obj> * EventHandler ^-> JQ

                "outerHeight" => !?T<bool> ^-> T<int>

                "outerWidth" => !?T<bool> ^-> T<int>

                "parent" => !?T<string> ^-> JQ

                "parents" => !?T<string> ^-> JQ

                "parentsUntil" => !?T<string> ^-> JQ

                "position" => T<unit> ^-> Position

                "prepend" => Content ^-> JQ
                "prepend" => (T<int> * T<string> ^-> T<string>) ^-> JQ

                "prependTo" => Content ^-> JQ

                "prev" => !?T<string> ^-> JQ

                "prevUntil" => !?T<string> ^-> JQ

                "pushStack" => Type.ArrayOf T<Dom.Element> ^-> JQ
                "pushStack" => Type.ArrayOf T<Dom.Element> * T<string> * Type.ArrayOf T<obj> ^-> JQ

                "queue" => !?T<string> ^-> JQ
                "queue" => !?T<string> * Type.ArrayOf T<obj> ^-> JQ
                "queue" => !?T<string> * (T<obj> ^-> T<unit>) ^-> JQ

                "ready" => UnitCallback ^-> JQ

                "remove" => !?T<string> ^-> JQ

                "removeAttr" => T<string> ^-> JQ

                "removeClass" => T<string> ^-> JQ

                "removeData" => !?T<string> ^-> JQ
                "removeData" => T<string[]> ^-> JQ

                "replaceAll" => !?T<string> ^-> JQ

                "replaceWith" => Content ^-> JQ
                "replaceWith" => (T<Dom.Element> -* T<unit> ^-> T<string>) ^-> JQ
                "replaceWith" => (T<Dom.Element> -* T<unit> ^-> JQ) ^-> JQ
                "replaceWith" => (T<Dom.Element> -* T<unit> ^-> T<Dom.Element>) ^-> JQ

                "resize" => !?EventHandler ^-> JQ
                "resize" => StringMap?data * !?EventHandler ^-> JQ

                "scroll" => !?EventHandler ^-> JQ
                "scroll" => StringMap?data * !?EventHandler ^-> JQ

                "scrollLeft" => T<int> ^-> JQ
                "scrollLeft" => T<unit->int>

                "scrollTop" => T<int> ^-> JQ
                "scrollTop" => T<unit->int>

                "select" => !?EventHandler ^-> JQ
                "select" => StringMap?data * !?EventHandler ^-> JQ

                "serialize" => T<unit> ^-> T<string>

                "serializeArray" => T<unit> ^-> Type.ArrayOf T<obj>

                "show" => !?IntString * !?(T<Dom.Element> -* T<unit> ^-> T<unit>)  ^-> JQ
                "show" => IntString * T<string>?easing * !?(T<Dom.Element> -* T<unit> ^-> T<unit>)  ^-> JQ

                "siblings" => !?T<string> ^-> JQ

                "size" => T<unit> ^-> T<int>

                "slice" => T<int> * !?T<int> ^-> JQ

                "slideDown" => !?IntString * !?UnitCallback ^-> JQ
                "slideDown" => IntString * T<string>?easing * !?UnitCallback ^-> JQ

                "slideToggle" => !?IntString * !?UnitCallback ^-> JQ
                "slideToggle" => IntString * T<string>?easing * !?UnitCallback ^-> JQ

                "slideUp" => !?IntString * !?UnitCallback ^-> JQ
                "slideUp" => IntString * T<string>?easing * !?UnitCallback ^-> JQ

                "stop" => !?T<bool> * !?T<bool> ^-> JQ
                "stop" => T<string> * !?T<bool> * !?T<bool> ^-> JQ

                "submit" => !?EventHandler ^-> JQ
                "submit" => StringMap?data * !?EventHandler ^-> JQ

                "text" => T<unit> ^-> T<string>
                "text" => T<string> ^-> JQ

                "toArray" => T<unit> ^-> Type.ArrayOf T<Dom.Element>

                /// TODO: review
                "toggle" => !?T<int> * !?EventHandler ^-> JQ
                "toggle" => T<int> * T<string>?easing * !?EventHandler ^-> JQ
                "toggle" => T<bool>?showOrHide ^-> JQ

                "toggleClass" => T<string> * !?T<bool> ^-> JQ
                "toggleClass" => (T<int> * T<string> ^-> T<unit>)  * !?T<bool> ^-> JQ

                "trigger" => T<string> * !?(Type.ArrayOf T<obj>) ^-> JQ
                "triggerHandler" => T<string> * !?(Type.ArrayOf T<obj>) ^-> JQ

                "unbind" => T<unit> ^-> JQ
                "unbind" => Event ^-> JQ
                "unbind" => T<string> * !?EventHandler ^-> JQ
                "unbindFalse" => T<string>?event * StringMap?eventData ^-> JQ
                |> WithComment "Attach a handler to an event for the elements."
                |> WithInline "$this.bind($event, $eventData, false)"

                "undelegate" => T<unit> ^-> JQ
                "undelegate" => T<string> * T<string> * !?EventHandler ^-> JQ

                "unload" => EventHandler ^-> JQ
                "unload" => StringMap?data * EventHandler ^-> JQ

                "unwrap" => T<unit> ^-> JQ

                "val" => T<unit> ^-> T<obj>
                "val" => T<string> ^-> JQ

                "width" => T<unit> ^-> T<int>
                "width" => T<int> ^-> JQ

                "wrap" => Content ^-> JQ
                "wrap" => (T<unit> ^-> T<string>) ^-> JQ

                "wrapAll" => Content ^-> JQ

                "wrapInner" => Content ^-> JQ
                "wrapInner" => (T<unit> ^-> T<string>) ^-> JQ

                "on" =>
                    T<string>?events
                    * !?T<string>?selector
                    * !?T<obj>?data
                    * T<obj->bool>?handler ^-> T<unit>

                "off" =>
                    T<string>?events
                    * !?T<string>?selector
                    * !?T<obj->bool>?handler ^-> T<unit>

            ]
        |+> Static [
                "of" => T<string>?selector ^-> JQ
                |> WithInline "jQuery($0)"
                |> WithComment "Accepts a string containing a CSS selector which is then used to match a set of elements."

                "of" => T<string>?selector * Content?context ^-> JQ
                |> WithInline "jQuery($0,$1)"
                |> WithComment "Accepts a string containing a CSS selector and a DOM Element, Document, or jQuery to use as context."

                "of" => (Type.ArrayOf T<Dom.Node>)?elementArray ^-> JQ
                |> WithInline "jQuery($0)"
                |> WithComment "An array containing a set of DOM elements to wrap in a jQuery object."

                "of" => T<Dom.Node>?node^-> JQ
                |> WithInline "jQuery($0)"
                |> WithComment "DOM node to wrap in a jQuery object."

                "of" => T<string>?html * T<Dom.Document>?ownerDocument ^-> JQ
                |> WithInline "jQuery($0,$1)"
                |> WithComment "Creates DOM elements on the fly from the provided string of raw HTML."

                "of" => T<string>?html * StringMap?props ^-> JQ
                |> WithInline "jQuery($0,$1)"
                |> WithComment "Creates DOM elements on the fly from the provided string of raw HTML."

                "of" => (T<unit> ^-> T<unit>) ?callback ^-> JQ
                |> WithInline "jQuery($0)"
                |> WithComment "Binds a function to be executed when the DOM has finished loading."

                "ajax" => AjaxSettings ^-> JqXHR
                "ajax" => T<string> * AjaxSettings ^-> JqXHR


                "ajaxSetup" => AjaxSettings ^-> T<unit>

                "contains" =>
                    T<Dom.Element>?container * T<Dom.Node>?contained ^-> T<bool>

                "data" =>
                    (T<Dom.Element> * T<string> * T<obj> ^-> T<unit>)
                    + (T<Dom.Element> * T<string> ^-> T<obj>)
                    + (T<Dom.Element> ^-> T<obj>)

                "dequeue" => T<Dom.Element> * !?T<string>?queueName ^-> T<unit>

                Generic -   ( fun t ->
                                Method "each" (Type.ArrayOf t * (T<int> * t ^-> T<unit>) ^-> T<unit>)
                            )
                "each" => T<obj> * (T<obj> * T<obj> ^-> T<unit>) ^-> T<unit>

                "error" => T<string->unit>

                "extend" => !?T<bool>?deep * T<obj> *+ T<obj> ^-> T<obj>
                
                "fx" =? FX 
                
                "get" =>
                    T<string>?url *
                    !?(T<string> + T<obj>)?data *
                    !?(T<obj> * T<string> * XmlHttpRequest ^-> T<unit>)?fn *
                    !?DataType?dataType ^->
                    JqXHR

                "getJSON" =>
                    T<string>?url *
                    !?(T<obj*string->unit>)?callback ^->
                    JqXHR

                "getJSON" =>
                    T<string>?url *
                    (T<string> + T<obj>)?data *
                    !?(T<obj*string->unit>)?callback ^->
                    JqXHR

                "getScript" =>
                    T<string>?url *
                    !?(T<obj*string->unit>)?callback ^->
                    JqXHR

                "globalEval" => T<string->obj>

                Generic - ( fun t ->
                                Method "grep" (
                                    Type.ArrayOf t * (t * T<int> ^-> T<bool>) * !?T<bool>?invert ^-> Type.ArrayOf t
                                )
                          )


                "hasData" => T<Dom.Element -> bool>
                Generic -   (fun t ->
                                Method "inArray" (
                                    t * (Type.ArrayOf t)  ^-> T<int>
                                )
                            )

                "isWindow" => T<obj> ^-> T<bool>
                |> WithComment "This is used in a number of places in jQuery to determine if we're operating against a browser window (such as the current window or an iframe)."

                "isArray" => T<obj->bool>

                "isNumeric" => T<obj->bool>

                "isEmptyObject" => T<obj> ^-> T<bool>

                "isFunction" => T<obj> ^-> T<bool>

                "isPlainObject" => T<obj->bool>

                "isXMLDoc" => T<Dom.Node> ^-> T<bool>

                "makeArray" => T<obj> ^-> Type.ArrayOf T<obj>

                Generic -   ( fun t u ->
                                Method "map" (Type.ArrayOf t * (t * T<int> ^-> u ) ^-> Type.ArrayOf u)
                            )

                Generic -   (fun t ->
                                Method "merge" ((Type.ArrayOf t) * (Type.ArrayOf t) ^-> (Type.ArrayOf t))
                            )

                "noConflict" => !?T<bool>?removeAll ^-> T<unit>

                "noop" => T<unit->unit>

                "param" => T<obj> * !?T<bool>?traditional ^-> T<string>

                "parseJSON" => T<string->obj>
                
                "parseXML" => T<string -> Dom.Document>

                "post" =>
                    T<string>?url *
                    !?(T<string> + T<obj>)?data *
                    !?(T<obj> * T<string> * XmlHttpRequest ^-> T<unit>)?success *
                    !?DataType?dataType ^->
                    JqXHR

                "when" => !+ Deferred ^-> Deferred

                //TODO!!!!!
                "proxy" => (T<obj> * T<string> ^-> T<obj>)

                "queue" =>
                    T<Dom.Element> * !?T<string>?queueName *
                    !?(T<unit->unit> + T<(unit->unit)[]>)?callback ^-> T<int>

                "removeData" =>
                    T<Dom.Element> * !?T<string>?name ^-> T<unit>

                // TODO!!!
                // "support" =? Support
                "trim" => T<string->string>
                
                "type" => T<unit> ^-> T<string>
                |> WithComment "Determine the internal JavaScript [[Class]] of an object"
                
                "unique" =>
                    Type.ArrayOf T<Dom.Element> ^-> Type.ArrayOf T<Dom.Element>
            ]
    
    let Callbacks =
        let Callbacks = Type.New ()
        
        let constructorComment = "A multi-purpose callbacks list object that provides a powerful way to manage callback lists."

        let addComment = "Add a callback or a collection of callbacks to a callback list."

        let removeComment = "Remove a callback or a collection of callbacks from a callback list."

        Class "jQuery.Callbacks"
        |+> Static [
            Constructor T<unit>
            |> WithInline "jQuery.Callbacks()"
            |> WithComment constructorComment

            Constructor T<string>
            |> WithInline "jQuery.Callbacks($0)"
            |> WithComment constructorComment
        ]
        |+> Instance [
            "add" => (!+ T<obj> ^-> T<unit>) ^-> Callbacks
            |> WithComment addComment

            "add" => Type.ArrayOf (!+ T<obj> ^-> T<unit>) ^-> Callbacks
            |> WithComment addComment

            "disable" => T<unit> ^-> Callbacks
            |> WithComment "Disable a callback list from doing anything more."

            "disabled" => T<unit -> bool>
            |> WithComment "Determine if the callbacks list has been disabled."

            "empty" => T<unit> ^-> Callbacks
            |> WithComment "Remove all of the callbacks from a list."

            "fire" => !+ T<obj> ^-> Callbacks
            |> WithComment "Call all of the callbacks with the given arguments."

            "fired" => T<unit -> bool>
            |> WithComment "Determine if the callbacks have already been called at least once."

            "fireWith" => !? T<obj> * !? (T<obj> + T<obj array>) ^-> Callbacks
            |> WithComment "Call all callbacks in a list with the given context and arguments."

            "has" => !? (!+ T<obj> ^-> T<unit>) ^-> T<bool>
            |> WithComment "Determine whether or not the list has any callbacks attached. If a callback is provided as an argument, determine whether it is in a list."

            "lock" => T<unit> ^-> Callbacks
            |> WithComment "Lock a callback list in its current state."

            "locked" => T<unit -> bool>
            |> WithComment "Determine if the callbacks list has been locked."

            "remove" => (!+ T<obj> ^-> T<unit>) ^-> Callbacks
            |> WithComment removeComment

            "remove" => Type.ArrayOf (!+ T<obj> ^-> T<unit>) ^-> Callbacks
            |> WithComment removeComment
        ]
        
    let Assembly =
        Assembly [
            Namespace "IntelliFactory.WebSharper.JQuery" [
                 Callbacks
                 RequestTypeClass
                 DataTypeClass
                 Promise
                 Deferred
                 JqXHRClass
                 SupportClass
                 PositionClass
                 AnimateConfigClass
                 AjaxSettings
                 EventClass
                 JQueryClass
                 FX
            ]
            Namespace "IntelliFactory.WebSharper.JQuery.Resources" [
                Resource "JQuery" "http://code.jquery.com/jquery-1.11.1.min.js"
                |> fun r -> r.AssemblyWide()
            ]
        ]

[<Sealed>]
type JQueryExtension() =
    interface IExtension with
        member x.Assembly = Definition.Assembly

[<assembly: Extension(typeof<JQueryExtension>)>]
do ()
