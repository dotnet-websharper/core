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

namespace WebSharper.JavaScript.Dom

open WebSharper.InterfaceGenerator
module P = Pattern

[<AutoOpen>]
module private Util =
    let GetNS = T<string>?namespaceURI * T<string>?localName
    let SetNS = T<string>?namespaceURI * T<string>?qualifiedName

[<AutoOpen>]
module private Types =
    let DOMClass name = Class ("DOM" + name) |> WithSourceName name
    let DOMTimeStamp = T<System.DateTime>
    let DocumentType = Class "DocumentType"
    let Document = Class "Document"
    let NodeList = Class "NodeList"
    let NamedNodeMap = Class "NamedNodeMap"
    let Element = Class "Element"
    let TypeInfo = Class "TypeInfo"
    let Event = Class "Event"
    let AbstractView = Class "AbstractView"
    let NodeFilter = Class "NodeFilter"
    let NodeIterator = Class "NodeIterator"
    let ShadowRoot = Class "ShadowRoot"
    let TreeWalker = Class "TreeWalker"
    let DocumentFragment = Class "DocumentFragment"
    let Text = Class "Text"

[<AutoOpen>]
module private Enumerations =

    let private Prepare (suffix: string) (x: string) =
        x.Substring(0, x.Length - suffix.Length).Split('_')
        |> Seq.filter ((<>) "")
        |> Seq.map (fun x ->
            x.Substring(0, 1) + x.Substring(1).ToLower())
        |> String.concat ""

    let private Enum name prefix suffix (names: string) =
        names.Split(' ')
        |> Seq.filter ((<>) "")
        |> Seq.map (fun x ->
            (Prepare suffix x, prefix + x))
        |> P.EnumInlines name

    let DOMExceptionType =
        Enum "ExceptionType" "DOMException." "_ERR" "\
            INDEX_SIZE_ERR DOMSTRING_SIZE_ERR HIERARCHY_REQUEST_ERR
            WRONG_DOCUMENT_ERR INVALID_CHARACTER_ERR NO_DATA_ALLOWED_ERR \
            NO_MODIFICATION_ALLOWED_ERR NOT_FOUND_ERR NOT_SUPPORTED_ERR \
            INUSE_ATTRIBUTE_ERR INVALID_STATE_ERR SYNTAX_ERR \
            INVALID_MODIFICATION_ERR NAMESPACE_ERR INVALID_ACCESS_ERR \
            VALIDATION_ERR TYPE_MISMATCH_ERR"

    let NodeType =
        Enum "NodeType" "Node." "_NODE" "\
            ELEMENT_NODE ATTRIBUTE_NODE TEXT_NODE CDATA_SECTION_NODE \
            ENTITY_REFERENCE_NODE ENTITY_NODE PROCESSING_INSTRUCTION_NODE \
            COMMENT_NODE DOCUMENT_NODE DOCUMENT_TYPE_NODE \
            DOCUMENT_FRAGMENT_NODE NOTATION_NODE"

    let DocumentPosition =
        Enum "DocumentPosition" "Node.DOCUMENT_POSITION_" "" "\
            DISCONNECTED PRECEDING FOLLOWING CONTAINS \
            CONTAINED_BY IMPLEMENTATION_SPECIFIC"

    let DerivationMethod =
        Enum "DerivationMethod" "TypeInfo.DERIVATION_" "" "\
            RESTRICTION EXTENSION UNION LIST"

    let NodeOperation =
        Enum "NodeOperation" "UserDataHandler.NODE_" "" "\
            IMPORTED DELETED RENAMED ADOPTED"

    let ErrorSeverity =
        Enum "ErrorSeverity" "DOMError.SEVERITY_" "" "\
            WARNING ERROR FATAL_ERROR"

    let PhaseType =
        Enum "PhaseType" "Event." "" "\
            AT_TARGET BUBBLING_PHASE CAPTURING_PHASE"

    let DeltaModeCode =
        Enum "DeltaModeCode" "WheelEvent." "" "\
            DOM_DELTA_PIXEL DOM_DELTA_LINE DOM_DELTA_PAGE"

    let InputModeCode =
        Enum "InputModeCode" "TextEvent.DOM_INPUT_METHOD_" "" "\
            UNKNOWN KEYBOARD PASTE DROP IME OPTION \
            HANDWRITING VOICE MULTIMODAL SCRIPT"

    let KeyLocationCode =
        Enum "KeyLocationCode" "KeyboardEvent.DOM_KEY_LOCATION_" "" "\
            LEFT NUMPAD RIGHT STANDARD MOBILE JOYSTICK"

    let attrChangeType =
        Enum "attrChangeType" "MutationEvent." "" "\
            ADDITION MODIFICATION REMOVAL"

module Interfaces =

    let Window = Class "Window"

    let DOMException =
        DOMClass "Exception"
        |+> Static [
                "code" =? DOMExceptionType
                "message" =? T<string>
                "name" =? T<string>
            ]

    let DOMStringList =
        DOMClass "StringList"
        |+> Instance [
                "length" =? T<int>
                "contains" => T<string->bool>
                "item" => T<int->string>
            ]

    let NameList =
        Class "NameList"
        |+> Instance [
                "length" =? T<int>
                "getName" => T<int->string>
                "getNamespaceURI" => T<int->string>
                "contains" => T<string->bool>
                "containsNS" => T<string>?namespaceURI * T<string>?name ^-> T<bool>
            ]

    let DOMImplementation =
        DOMClass "Implementation"
        |+> Instance [
                "hasFeature" =>
                    T<string>?feature * T<string>?version ^-> T<bool>
                "createDocumentType" =>
                    T<string>?qualifiedName *
                    T<string>?publicId *
                    T<string>?systemId ^-> DocumentType
                "createDocument" =>
                    T<string>?namespaceURI *
                    T<string>?qualifiedName *
                    DocumentType ^-> Document
                "createHTMLDocument" =>
                    !?T<string>?title ^-> Document
            ]

    let DOMImplementationList =
        DOMClass "ImplementationList"
        |+> Instance [
                "item" => T<int> ^-> DOMImplementation
                "length" =? T<int>
            ]

    let DOMImpementationSource =
        DOMClass "ImplementationSource"
        |+> Instance [
                "getDOMImplementation" =>
                    T<string> ^-> DOMImplementation
                "getDOMImplementationList" =>
                    T<string> ^-> DOMImplementationList
            ]

    let DOMRect =
        DOMClass "Rect"
        |+> Instance [
                "x" =? T<double>
                "y" =? T<double>
                "width" =? T<double>
                "height" =? T<double>
                "top" =? T<double>
                "right" =? T<double>
                "bottom" =? T<double>
                "left" =? T<double>
            ]

    let EventTargetClass =
        Class "EventTarget"

    let AbortSignal =
        Class "AbortSignal"
        |=> Inherits EventTargetClass
        |+> Static [
            "abort" => T<obj>?reason ^-> TSelf
            "timeout" => T<int>?milliseconds ^-> TSelf
        ]
        |+> Instance [
            "aborted" =? T<bool>
            "reason" =? T<obj>
            "throwIfAborted" => T<unit> ^-> T<unit>
            "onabort" =@ !?(Event ^-> T<unit>)
            |> ObsoleteWithMessage "Use OnAbort instead"
            "onabort" =@ !?(Event ^-> T<unit>)
            |> WithSourceName "OnAbort"
        ]

    let EventListenerOptions =
        Pattern.Config "EventListenerOptions" {
            Required = []
            Optional = [
                "capture", T<bool>
            ]
        }

    let AddEventListenerOptions =
        Pattern.Config "AddEventListenerOptions" {
            Required = []
            Optional = [
                "capture", T<bool>
                "passive", T<bool>
                "once", T<bool>
                "signal", AbortSignal.Type
            ]
        }

    let EventTarget =
        let EventListener = (T<unit> + Event) ^-> T<unit>
        EventTargetClass
        |+> Static [
                Constructor T<unit>
            ]
        |+> Instance [
                "addEventListener" =>
                    T<string>?eventtype *
                    EventListener?listener *
                    !?T<bool>?useCapture ^-> T<unit>
                "addEventListener" =>
                    T<string>?eventtype *
                    EventListener?listener *
                    AddEventListenerOptions?options ^-> T<unit>
                "dispatchEvent" => Event ^-> T<bool>
                "removeEventListener" =>
                    T<string>?eventtype *
                    EventListener?listener *
                    !?T<bool>?useCapture ^-> T<unit>
                "removeEventListener" =>
                    T<string>?eventtype *
                    EventListener?listener *
                    EventListenerOptions?options ^-> T<unit>
            ]

    let QuerySelectorMixin =
        Instance [
            "querySelector" => T<string> ^-> Element
            "querySelectorAll" => T<string> ^-> NodeList
        ]

    let Node =
        Class "Node"
        |=> Inherits EventTarget
        |+> Static [
                "ELEMENT_NODE" =? T<int>
                "ATTRIBUTE_NODE" =? T<int>
                "TEXT_NODE" =? T<int>
                "CDATA_SECTION_NODE" =? T<int>
                "ENTITY_REFERENCE_NODE" =? T<int>
                "ENTITY_NODE" =? T<int>
                "PROCESSING_INSTRUCTION_NODE" =? T<int>
                "COMMENT_NODE" =? T<int>
                "DOCUMENT_NODE" =? T<int>
                "DOCUMENT_TYPE_NODE" =? T<int>
                "DOCUMENT_FRAGMENT_NODE" =? T<int>
                "NOTATION_NODE" =? T<int>

                "DOCUMENT_POSITION_DISCONNECTED" =? T<int>
                "DOCUMENT_POSITION_PRECEDING" =? T<int>
                "DOCUMENT_POSITION_FOLLOWING" =? T<int>
                "DOCUMENT_POSITION_CONTAINS" =? T<int>
                "DOCUMENT_POSITION_CONTAINED_BY" =? T<int>
                "DOCUMENT_POSITION_IMPLEMENTATION_SPECIFIC" =? T<int>
            ]
        |+> Instance [
                "baseURI" =? T<string>
                "childNodes" =? NodeList
                "firstChild" =? TSelf
                "isConnected" =? T<bool>
                "lastChild" =? TSelf
                "nextSibling" =? TSelf
                "nodeName" =? T<string>
                "nodeType" =? NodeType
                "nodeValue" =@ T<string>
                "ownerDocument" =? Document
                "parentNode" =? TSelf
                "parentElement" =? Element
                "previousSibling" =? TSelf
                "textContent" =@ T<string>
                "appendChild" => TSelf?newChild ^-> TSelf
                "cloneNode" => !?T<bool>?deep ^-> TSelf
                "contains" => TSelf ^-> T<bool>
                "compareDocumentPosition" => TSelf ^-> DocumentPosition
                "getRootNode" => !?T<obj>?options ^-> TSelf
                "hasAttributes" => T<unit->bool>
                "hasChildNodes" => T<unit->bool>
                "insertBefore" => TSelf?newChild * TSelf?refChild ^-> TSelf
                "isDefaultNamespace" => T<string->bool>
                "isEqualNode" => TSelf ^-> T<bool>
                "isSameNode" => TSelf ^-> T<bool>
                "lookupNamespaceURI" => T<string->string>
                "lookupPrefix" => T<string->string>
                "normalize" => T<unit->unit>
                "removeChild" => TSelf?oldChild ^-> TSelf
                "replaceChild" => TSelf?newChild * TSelf?oldChild ^-> TSelf
            ]

    let NodeList =
        NodeList
        |+> Instance [
                "item" => T<int>?index ^-> Node
                |> WithInline "$this[$index]"
                "length" =? T<int>
//                "entries" => T<unit -> unit> this returns an iterator same as values and keys
                "forEach" =>
                    (Node * T<int> * TSelf * T<obj> ^-> T<unit>) *
                    T<obj> ^-> T<unit>
            ]

    let DOMTokenList =
        DOMClass "TokenList"
        |+> Instance [
            "length" =? T<int>
            "item" => T<int> ^-> T<string>
            "contains" => T<string> ^-> T<bool>
            "add" => T<string> ^-> T<unit>
            "remove" => T<string> ^-> T<unit>
            "replace" => (T<string> * T<string>) ^-> T<unit>
            "toggle" => (T<string> * !?T<bool>) ^-> T<bool>
            "supports" => T<string> ^-> T<bool>
            "value" =? T<string>
        ]

    let Range =
        Class "Range"
        |+> Static [
            Constructor T<unit>
            "START_TO_START" =? T<int>
            "START_TO_END" =? T<int>
            "END_TO_END" =? T<int>
            "END_TO_START" =? T<int>
        ]
        |+> Instance [
            "startContainer" =? Node
            "startOffset" =? T<int>
            "endContainer" =? Node
            "endOffset" =? T<int>
            "collapsed" =? T<bool>
            "commonAncestorContainer" =? Node

            "setStart" => (Node * T<int>) ^-> T<unit>
            "setEnd" => (Node * T<int>) ^-> T<unit>
            "setStartBefore" => Node ^-> T<unit>
            "setStartAfter" => Node ^-> T<unit>
            "setEndBefore" => Node ^-> T<unit>
            "setEndAfter" => Node ^-> T<unit>
            // unit version defaults to false
            "collapse" => T<bool>?toStart ^-> T<unit>
            "collapse" => T<unit> ^-> T<unit>
                |> WithComment "The toStart parameter defaults to false"
            "selectNode" => Node ^-> T<unit>
            "selectNodeContents" => Node ^-> T<unit>

            "compareBoundaryPoints" => (T<int> * Node) ^-> T<int>

            "deleteContents" => T<unit> ^-> T<unit>
            "extractContents" => T<unit> ^-> DocumentFragment
            "cloneContents" => T<unit> ^-> DocumentFragment
            "insertNode" => Node ^-> T<unit>
            "surroundContents" => Node ^-> T<unit>

            "cloneRange" => T<unit> ^-> TSelf
            "detach" => T<unit> ^-> T<unit>

            "isPointInRange" => (Node * T<int>) ^-> T<bool>
            "comparePoint" => (Node * T<int>) ^-> T<int>

            "intersectsNode" => Node ^-> T<bool>
        ]

    let Attr =
        Class "Attr"
        |=> Inherits Node
        |+> Instance [
                "name" =? T<string>
                "specified" =? T<bool>
                "value" =@ T<string>
                "ownerElement" =? Element
                "namespaceUri" =? T<string>
                "localName" =? T<string>
                "prefix" =? T<string>
            ]

    let NamedNodeMap =
        NamedNodeMap
        |+> Instance [
                "length" =? T<int>
                "getNamedItem" => T<string> ^-> Attr
                "setNamedItem" => Attr ^-> Attr
                "removeNamedItem" => T<string> ^-> Attr
                "item" => T<int> ^-> Attr
                "getNamedItemNS" => GetNS ^-> Attr
                "setNamedItemNS" => Attr ^-> Attr
                "removeNamedItemNS" => GetNS ^-> Attr
            ]

    let ShadowRootMode =
        Pattern.EnumStrings "ShadowRootMode" [
            "open"
            "closed"
        ]

    let ShadowRootInit =
        Pattern.Config "ShadowRootInit" {
            Required = [ "mode", ShadowRootMode.Type ]
            Optional = []
        }

    let HTMLCollection = // LOOK AT THIS
        Class "HTMLCollection"
        |+> Instance [
            "length" =? T<int>
            "item" => T<int> ^-> Element
            "namedItem" => T<string> ^-> Element
        ]

    let ParentNodeMixin = 
        Instance [
            "childElementCount" =? T<int>
            "firstElementChild" =? !?Element
            "lastElementChild" =? !?Element
            "children" =? HTMLCollection

            "prepend" => !+(T<string> + Node) ^-> T<unit>
            "append" => !+(T<string> + Node) ^-> T<unit>
            "replaceChildren" => !+(T<string> + Node) ^-> T<unit>
        ]

    let NonDocumentTypeChildNodeMixin = 
        Instance [
            "nextElementSibling" =? !?Element 
            "previousElementSibling" =? !?Element
        ]

    let ChildNodeMixin = 
        Instance [
            "after" => !+(T<string> + Node) ^-> T<unit>
            "before" => !+(T<string> + Node) ^-> T<unit>
            "replaceWith" => !+(T<string> + Node) ^-> T<unit>
            "remove" => T<unit> ^-> T<unit>
        ]

    let AssignedNodesOptions = 
        Pattern.Config "AssignedNodesOptions"
            {
                Required = []
                Optional = [
                    "flatten", T<bool>
                ]
            }
         

    let HTMLSlotElement = 
        Class "HTMLSlotElement"
        |=> Inherits Element
        |+> Static [
            Constructor T<unit>
        ]
        |+> Instance [
            "name" =@ T<string>

            "assign" => !+Node ^-> T<unit>
            "assignedNodes" => !?AssignedNodesOptions?options ^-> !|Node
            "assignedElements" => !?AssignedNodesOptions?options ^-> !|Element
            "replaceWith" => !+(Element + Text) ^-> T<unit>

            "onslotchange" => Event ^-> T<unit>
            |> ObsoleteWithMessage "Use OnSlotChange instead"
            "onslotchange" =@ !?(Event ^-> T<unit>)
            |> WithSourceName "OnSlotChange"
        ]

    let HTMLTemplateEvent =
        Class "HTMLTemplateElement"
        |=> Inherits Element
        |+> Instance [
            "content" =? DocumentFragment
        ]
    let SlottableMixin = 
        Instance [
            "assignedSlot" => HTMLSlotElement
        ]

    let PointerEventsMixin = 
        Instance [
            "setPointerCapture" => T<int> ^-> T<unit>
            "releasePointerCapture" => T<int> ^-> T<unit>
            "hasPointerCapture" => T<int> ^-> T<bool>
            "requestPointerLock" => T<unit> ^-> T<unit> // Part of Pointer Lock 2.0 
        ]

    let FullscreenNavigationUI = 
        Pattern.EnumStrings "FullscreenNavigationUI"
            [
                "auto"
                "show"
                "hide"
            ]

    let FullscreenOptions = 
        Pattern.Config "FullscreenOptions"
            {
                Required = []
                Optional = [
                    "navigationUI", FullscreenNavigationUI.Type
                ]
            }

    let ScrollAnimateMode = 
        Pattern.EnumStrings "ScrollAnimateMode"
            [
                "auto"
                "smooth"
                "instant"
            ]

    let ScrollOptions = 
        Pattern.Config "ScrollOptions"
            {
                Required = [
                    "top", T<int>
                    "left", T<int>
                ]
                Optional = [
                    "behavior", ScrollAnimateMode.Type
                ]
            }

    let AnimationPlayState = 
        Pattern.EnumStrings "AnimationPlayState"
            [
                "idle"
                "running"
                "paused"
                "finished"
            ]

    let AnimationReplaceState = 
        Pattern.EnumStrings "AnimationReplaceState"
            [
                "active"
                "presisted"
                "removed"
            ]

    let TimelinePhase =
        Pattern.EnumStrings "TimelinePhase" 
            [
                "inactive"
                "before"
                "active"
                "after"
            ]

    let DocumentTimeLineOptions =
        Pattern.Config "DocumentTimeLineOptions"
            {
                Required = []
                Optional = [
                    "originTime", T<int>
                ]
            }

    let AnimationTimeLine =
        Class "AnimationTimeLine"
        |+> Instance [
            "currentTime" =? !?T<double>
            "phase" =? TimelinePhase
        ]

    let DocumentTimeLine =
        Class "DocumentTimeLine"
        |=> Inherits AnimationTimeLine
        |+> Static [
            Constructor T<unit>
            Constructor DocumentTimeLineOptions
        ]

    let Animation = 
        Class "Animation"
        |+> Static [
            Constructor T<unit>
        ]
        |+> Instance [
            "currentTime" =@ T<double>
            "effect" =@ T<obj>
            "finished" =? WebSharper.JavaScript.Ecma.Definition.EcmaPromise.[T<unit>]
            "id" =@ T<string>
            "oncancel" =@ T<unit> ^-> T<unit>
            |> ObsoleteWithMessage "Use OnCancel instead"
            "oncancel" =@ !?(Event ^-> T<unit>)
            |> WithSourceName "OnCancel"
            "onfinish" =@ T<unit> ^-> T<unit>
            |> ObsoleteWithMessage "Use OnFinish instead"
            "onfinish" =@ !?(Event ^-> T<unit>)
            |> WithSourceName "OnFinish"
            "onremove" =@ T<unit> ^-> T<unit>
            |> ObsoleteWithMessage "Use OnRemove instead"
            "onremove" =@ !?(Event ^-> T<unit>)
            |> WithSourceName "OnRemove"
            "pending" =? T<bool>
            "playbackRate" =@ T<double>
            "playState" =? AnimationPlayState.Type
            "ready" =? WebSharper.JavaScript.Ecma.Definition.EcmaPromise.[T<unit>]
            "replaceState" =? AnimationReplaceState.Type
            "startTime" =@ T<double>
            "timeline" =@ AnimationTimeLine

            "cancel" => T<unit> ^-> T<unit>
            "commitStyles" => T<unit> ^-> T<unit>
            "finish" => T<unit> ^-> T<unit>
            "pause" => T<unit> ^-> T<unit>
            "persist" => T<unit> ^-> T<unit>
            "play" => T<unit> ^-> T<unit>
            "reverse" => T<unit> ^-> T<unit>
            "updatePlaybackRate" => T<double> ^-> T<unit>
        ]

    let Element =
        Element
        |=> Inherits Node
        |+> QuerySelectorMixin
        |+> ParentNodeMixin
        |+> NonDocumentTypeChildNodeMixin
        |+> ChildNodeMixin
        |+> SlottableMixin
        |+> PointerEventsMixin
        |+> Instance [
                "schemaTypeInfo" =@ TypeInfo
                "tagName" =@ T<string>
                "attributes" =? NamedNodeMap
                "classList" =? DOMTokenList
                "className" =@ T<string>
                "id" =@ T<string>
                "innerHTML" =@ T<string>
                "localName" =? T<string>
                "namespaceURI" =? T<string>
                "outerHTML" =? T<string>
                "prefix" =? T<string>

                "slot" =@ T<string>

                "animate" => (T<obj> + !|T<obj>) * (T<int> + T<obj>) ^-> Animation
                "getAnimations" => T<unit> ^-> !|Animation

                // CSSOM
                "scrollTop" =@ T<double>
                "scrollLeft" =@ T<double>
                "scrollWidth" =? T<double>
                "scrollHeight" =? T<double>
                "clientTop" =? T<double>
                "clientLeft" =? T<double>
                "clientWidth" =? T<double>
                "clientHeight" =? T<double>
                
                "getClientRects" => T<unit> ^-> Type.ArrayOf DOMRect
                "getBoundingClientRect" => T<unit> ^-> DOMRect
                "scroll" => !?ScrollOptions?options ^-> T<unit>
                "scrollBy" => !?ScrollOptions?options ^-> T<unit>
                "scrollIntoView" => !?ScrollOptions?options ^-> T<unit>
                "scrollTo" => !?ScrollOptions ^-> T<unit>
                // CSSOM

                // FullScreen API
                "onfullscreenchange" =@ T<unit> ^-> T<unit>
                |> ObsoleteWithMessage "Use OnFullscreenChange instead"
                "onfullscreenchange" =@ !?(Event ^-> T<unit>)
                |> WithSourceName "OnFullscreenChange"
                "onfullscreenerror" =@ T<unit> ^-> T<unit>
                |> ObsoleteWithMessage "Use OnFullscreenError instead"
                "onfullscreenerror" =@ !?(Event ^-> T<unit>)
                |> WithSourceName "OnFullscreenError"

                "requestFullScreen" => !?FullscreenOptions?options ^-> WebSharper.JavaScript.Ecma.Definition.EcmaPromise.[T<unit>]
                // FullScreen API


                "hasAttributes" => T<unit> ^-> T<bool>
                "getAttributeNames" => T<unit> ^-> T<string[]>
                    |> WithComment "Warning: This method is not available in every browser, but it's part of the specification."
                "getAttribute" => T<string> ^-> T<string>
                "setAttribute" => T<string> * T<string> ^-> T<unit>
                "removeAttribute" => T<string->unit>
                "getAttributeNode" => T<string> ^-> Attr
                "setAttributeNode" => Attr ^-> Attr
                "removeAttributeNode" => Attr ^-> Attr
                "getElementsByTagName" => T<string> ^-> NodeList
                "getAttributeNS" => GetNS ^-> T<string>
                "setAttributeNS" => SetNS * T<string> ^-> T<unit>
                "removeAttributeNS" => GetNS ^-> T<unit>
                "getAttributeNodeNS" => GetNS ^-> Attr
                "setAttributeNodeNS" => Attr ^-> Attr
                "getElementsByTagNameNS" => GetNS ^-> NodeList
                "hasAttribute" => T<string->bool>
                "hasAttributeNS" => GetNS ^-> T<bool>
                "setIdAttribute" => T<string> * T<bool>?isId ^-> T<unit>
                "setIdAttributeNS" => GetNS * T<bool>?isId ^-> T<unit>
                "setIdAttributeNode" => Attr * T<bool>?isId ^-> T<unit>
                "getElementsByClassName" => T<string> ^-> NodeList

                "closest" => T<string>?selectors ^-> TSelf
                "matches" => T<string>?selectors ^-> T<bool>
                
                "attachShadow" => ShadowRootInit ^-> ShadowRoot
                "shadowRoot" =? ShadowRoot

                "insertAdjacentElement" => T<string>?where * Element?element ^-> !?Element
                "insertAdjacentText" => T<string>?where * T<string>?data ^-> T<unit>
                "insertAdjacentHTML" => T<string>?position * T<string>?text ^-> T<unit>
            ]

    let CharacterData =
        Class "CharacterData"
        |=> Inherits Node
        |+> ChildNodeMixin
        |+> NonDocumentTypeChildNodeMixin
        |+> Instance [
                "data" =@ T<string>
                "length" =? T<int>
                "substringData" =>
                    T<int>?offset * T<int>?count ^-> T<string>
                "appendData" => T<string->unit>
                "insertData" => T<int>?offset * T<string> ^-> T<unit>
                "deleteData" => T<int>?offset * T<int>?count ^-> T<unit>
                "replaceData" =>
                    T<int>?offset * T<int>?count * T<string> ^-> T<unit>
            ]

    let Text =
        Text
        |=> Inherits CharacterData
        |+> SlottableMixin
        |+> Static [
                Constructor T<unit>
                Constructor T<string>
            ]
        |+> Instance [
                "wholeText" =? T<string>

                "splitText" => T<int> ^-> TSelf
            ]

    let Comment =
        Class "Comment"
        |=> Inherits CharacterData

    let TypeInfo =
        TypeInfo
        |+> Instance [
                "typeName" =? T<string>
                "typeNamespace" =? T<string>
                "isDerivedFrom" =>
                    T<string>?typeNamespace *
                    T<string>?typeName *
                    DerivationMethod?derivationMethod ^-> T<bool>
            ]
        |> Obsolete

    let UserDataHandler =
        Class "UserDataHandler"
        |+> Instance [
                "handle" =>
                    NodeOperation * T<string>?key * T<obj>?data *
                    Node?src * Node?dst ^-> T<unit>
            ]
        |> Obsolete

    let DOMError =
        DOMClass "Error"
        |+> Instance [
                "name" =? T<string>
                "message" =? T<string>
            ]

    let DOMErrorHandler =
        DOMClass "ErrorHandler"
        |+> Instance [
                "handleError" => DOMError ^-> T<bool>
            ]
        |> Obsolete

    let DOMLocator =
        DOMClass "Locator"
        |> WithSourceName "Locator"
        |+> Instance [
                "lineNumber" =? T<int>
                "columnNumber" =? T<int>
                "byteOffset" =? T<int>
                "utf16Offset" =? T<int>
                "relatedNode" =? Node
                "uri" =? T<string>
            ]
        |> Obsolete

    let DOMConfiguration =
        DOMClass "Configuration"
        |+> Instance [
                "setParameter" => T<string*obj->unit>
                "getParameter" => T<string->obj>
                "canSetParameter" => T<string*obj->bool>
                "parameterNames" =@ DOMStringList
            ]

    let CDATASection =
        Class "CDATASection"
        |=> Inherits Text

    let DocumentType =
        DocumentType
        |=> Inherits Node
        |+> ChildNodeMixin
        |+> Instance [
                "name" =? T<string>
                "publicId" =? T<string>
                "systemId" =? T<string>
            ]

    let Notation =
        Class "Notation"
        |=> Inherits Node
        |+> Instance [
                "publicId" =? T<string>
                "systemId" =? T<string>
            ]
        |> Obsolete

    let Entity =
        Class "Entity"
        |=> Inherits Node
        |+> Instance [
                "publicId" =? T<string>
                "systemId" =? T<string>
                "notationName" =? T<string>
                "inputEncoding" =? T<string>
                "xmlEncoding" =? T<string>
                "xmlVersion" =? T<string>
            ]
        |> Obsolete

    let EntityReference =
        Class "EntityReference"
        |=> Inherits Node
        |> Obsolete

    let ProcessingInstruction =
        Class "ProcessingInstruction"
        |=> Inherits Node
        |+> Instance [
                "target" =? T<string>
                "data" =@ T<string>
            ]

    let DocumentFragment =
        DocumentFragment
        |=> Inherits Node
        |+> QuerySelectorMixin
        |+> ParentNodeMixin

    let ShadowRoot =
        ShadowRoot
        |=> Inherits DocumentFragment
        |+> Instance [
            "mode" =? ShadowRootMode
            "host" =? Element
        ]

    let NodeIterator =
        NodeIterator
        |+> Instance [
            "root" =? Node
            "referenceNode" =? Node
            "pointerBeforeReferenceNode" =? T<bool>
            "whatToShow" =? T<int>
            "filter" =? NodeFilter

            "nextNode" => T<unit> ^-> Node
            "previousNode" => T<unit> ^-> Node

            "detach" => T<unit> ^-> T<unit>
        ]

    let TreeWalker =
        let UTN = T<unit> ^-> Node
        TreeWalker
        |+> Instance [
            "root" =? Node
            "whatToShow" =? T<int>
            "filter" =? NodeFilter
            "currentNode" =@ Node

            "parentNode" => UTN
            "firstChild" => UTN
            "lastChild" => UTN
            "previousSibling" => UTN
            "nextSibling" => UTN
            "previousNode" => UTN
            "nextNode" => UTN
        ]

    let NodeFilter =
        NodeFilter
        |+> Static [
            // Constants for acceptNode
            "FILTER_ACCEPT" =? T<int>
            "FILTER_REJECT" =? T<int>
            "FILTER_SKIP" =? T<int>
            // Constants for whatToShow
            "SHOW_ALL" =? T<int>
            "SHOW_ELEMENT" =? T<int>
            "SHOW_ATTRIBUTE" =? T<int>
            "SHOW_TEXT" =? T<int>
            "SHOW_CDATA_SECTION" =? T<int>
            "SHOW_ENTITY_REFERENCE" =? T<int> |> Obsolete
            "SHOW_ENTITY" =? T<int> |> Obsolete
            "SHOW_PROCESSING_INSTRUCTION" =? T<int>
            "SHOW_COMMENT" =? T<int>
            "SHOW_DOCUMENT" =? T<int>
            "SHOW_DOCUMENT_TYPE" =? T<int>
            "SHOW_DOCUMENT_FRAGMENT" =? T<int>
            "SHOW_NOTATION" =? T<int>
        ]
        |+> Instance [
            "acceptNode" => Node ^-> T<int>
        ]

    let EventInit =
        Pattern.Config "EventInit" {
            Required = []
            Optional =
                [
                    "bubbles", T<bool>
                    "cancelable", T<bool>
                    "scoped", T<bool>
                    "composed", T<bool>
                ]
        }

    let Event =
        Event
        |+> Static [
                Constructor (T<string> * !? EventInit)
            ]
        |+> Instance [
                "bubbles" =? T<bool>
                "cancelable" =? T<bool>
                "composed" =? T<bool>
                "currentTarget" =? EventTarget
                "defaultPrevented" =? T<bool>
                "eventPhase" =? PhaseType
                "namespaceURI" =@ T<string>
                "target" =? EventTarget
                "timeStamp" =? DOMTimeStamp
                "type" =? T<string>
                "isTrusted" =? T<bool>
                "initEvent" =>
                    T<string>?eventTypeArg *
                    T<bool>?canBubbleArg *
                    T<bool>?cancelableArg ^-> T<unit>
                    |> Obsolete
                "initEventNS" =>
                    T<string>?namespaceURIArg *
                    T<string>?eventTypeArg *
                    T<bool>?canBubbleArg *
                    T<bool>?cancelableArg ^-> T<unit>
                    |> Obsolete
                "preventDefault" => T<unit->unit>
                "stopImmediatePropagation" => T<unit->unit>
                "stopPropagation" => T<unit->unit>
                "composedPath" => T<unit> ^-> !|EventTarget
            ]

    let CustomEvent  =
        Class "CustomEvent"
        |+> Instance [
                "detail" =? T<obj>
                "initCustomEvent" =>
                    T<string>?typeArg *
                    T<bool>?canBubbleArg *
                    T<bool>?cancelableArg *
                    T<obj>?detailArg ^-> T<unit>
                    |> Obsolete
                "initCustomEventNS" =>
                    T<string>?namespaceURIArg *
                    T<string>?typeArg *
                    T<bool>?canBubbleArg *
                    T<bool>?cancelableArg *
                    T<obj>?detailArg ^-> T<unit>
                    |> Obsolete
            ]

    let DocumentView =
        Class "DocumentView"
        |+> Instance [
                "defaultView" => AbstractView
            ]

    let AbstractView =
        AbstractView
        |+> Instance [
                "document" =@ DocumentView
            ]

    let UIEvent =
        Class "UIEvent"
        |=> Inherits Event
        |+> Instance [
                "detail" =? T<int>
                "view" =? AbstractView
                "initUIEvent" =>
                    T<string>?typeArg *
                    T<bool>?canBubbleArg *
                    T<bool>?cancelableArg *
                    AbstractView?viewArg *
                    T<int>?detailArg ^-> T<unit>
                    |> Obsolete
                "initUIEventNS" =>
                    T<string>?namespaceURIArg *
                    T<string>?typeArg *
                    T<bool>?canBubbleArg *
                    T<bool>?cancelableArg *
                    AbstractView?viewArg *
                    T<int>?detailArg ^-> T<unit>
                    |> Obsolete
            ]

    let FocusEvent =
        Class "FocusEvent"
        |=> Inherits UIEvent
        |+> Instance [
            "relatedTarget" =? EventTarget
        ]

    let MouseButtons =
        Class "MouseButtons"
        |+> Static [
            "none" =? T<int> |> WithGetterInline "0"
            "left" =? T<int> |> WithGetterInline "1"
            "right" =? T<int> |> WithGetterInline "2"
            "middle" =? T<int> |> WithGetterInline "4"
            "back" =? T<int> |> WithGetterInline "8"
            "forward" =? T<int> |> WithGetterInline "16"
        ]
        |> WithComment "Flags for the Buttons property of MouseEvent. Note that this is different from the Button property."

    let MouseEvent =
        Class "MouseEvent"
        |=> Inherits UIEvent
        |+> Instance [
                "altKey" =? T<bool>
                "button" =? T<int> // short
                "buttons" =? T<int>
                "clientX" =? T<int>
                "clientY" =? T<int>
                "ctrlKey" =? T<bool>
                "metaKey" =? T<bool>
                "relatedTarget" =? EventTarget
                "screenX" =? T<int>
                "screenY" =? T<int>
                "shiftKey" =? T<bool>
                "getModifierState" => T<string>?keyIdentifierArg ^-> T<bool>
                "initMouseEvent" =>
                    T<string>?typeArg *
                    T<bool>?canBubbleArg *
                    T<bool>?cancelableArg *
                    AbstractView?viewArg *
                    T<int>?detailArg *
                    T<int>?screenXArg *
                    T<int>?screenYArg *
                    T<int>?clientXArg *
                    T<int>?clientYArg *
                    T<bool>?ctrlKeyArg *
                    T<bool>?altKeyArg *
                    T<bool>?shiftKeyArg *
                    T<bool>?metaKeyArg *
                    T<int>?button *
                    Node?relatedTargetArg ^-> T<unit>
                    |> Obsolete
                "initMouseEventNS" =>
                    T<string>?namespaceURIArg *
                    T<string>?typeArg *
                    T<bool>?canBubbleArg *
                    T<bool>?cancelableArg *
                    AbstractView?viewArg *
                    T<int>?detailArg *
                    T<int>?screenXArg *
                    T<int>?screenYArg *
                    T<int>?clientXArg *
                    T<int>?clientYArg *
                    T<int>?button *
                    Node?relatedTargetArg *
                    T<string>?modifiersListArg ^-> T<unit>
                    |> Obsolete
            ]

    let MouseWheelEvent =
        Class "MouseWheelEvent"
        |=> Inherits MouseEvent
        |+> Instance [
                "wheelDelta" =@ T<int>
                "initMouseWheelEvent" =>
                    T<string>?typeArg *
                    T<bool>?canBubbleArg *
                    T<bool>?cancelableArg *
                    AbstractView?viewArg *
                    T<int>?detailArg *
                    T<int>?screenXArg *
                    T<int>?screenYArg *
                    T<int>?clientXArg *
                    T<int>?clientYArg *
                    T<int>?button *
                    Node?relatedTargetArg *
                    T<string>?modifiersListArg *
                    T<int>?wheelDeltaArg ^-> T<unit>
                "initMouseWheelEventNS" =>
                    T<string>?namespaceURIArg *
                    T<string>?typeArg *
                    T<bool>?canBubbleArg *
                    T<bool>?cancelableArg *
                    AbstractView?viewArg *
                    T<int>?detailArg *
                    T<int>?screenXArg *
                    T<int>?screenYArg *
                    T<int>?clientXArg *
                    T<int>?clientYArg *
                    T<int>?button *
                    Node?relatedTargetArg *
                    T<string>?modifiersListArg *
                    T<int>?wheelDeltaArg ^-> T<unit>
            ]
        |> Obsolete

    let WheelEvent =
        Class "WheelEvent"
        |=> Inherits MouseEvent
        |+> Instance [
                "deltaX" =? T<int>
                "deltaY" =? T<int>
                "deltaZ" =? T<int>
                "deltaMode" =? DeltaModeCode
                "initWheelEvent" =>
                    T<string>?typeArg *
                    T<bool>?canBubbleArg *
                    T<bool>?cancelableArg *
                    AbstractView?viewArg *
                    T<int>?detailArg *
                    T<int>?screenXArg *
                    T<int>?screenYArg *
                    T<int>?clientXArg *
                    T<int>?clientYArg *
                    T<int>?button *
                    Node?relatedTargetArg *
                    T<string>?modifiersListArg *
                    T<int>?wheelDeltaArg *
                    T<int>?deltaX *
                    T<int>?deltaY *
                    T<int>?deltaZ *
                    DeltaModeCode ^-> T<unit>
                    |> Obsolete  
                "initWheelEventNS" =>
                    T<string>?namespaceURIArg *
                    T<string>?typeArg *
                    T<bool>?canBubbleArg *
                    T<bool>?cancelableArg *
                    AbstractView?viewArg *
                    T<int>?detailArg *
                    T<int>?screenXArg *
                    T<int>?screenYArg *
                    T<int>?clientXArg *
                    T<int>?clientYArg *
                    T<int>?button *
                    Node?relatedTargetArg *
                    T<string>?modifiersListArg *
                    T<int>?wheelDeltaArg *
                    T<int>?deltaX *
                    T<int>?deltaY *
                    T<int>?deltaZ *
                    DeltaModeCode ^-> T<unit>
                    |> Obsolete
            ]

    let KeyboardEvent =
        Class "KeyboardEvent"
        |=> Inherits UIEvent
        |+> Instance [
                "altKey" =? T<bool>
                "charCode" =? T<int>
                |> WithComment "Get the character code of a keypress event."
                "code" =? T<string>
                |> WithComment "Get the key code of a keyboard event."
                "ctrlKey" =? T<bool>
                "isComposing" =? T<bool>
                "key" =? T<string>
                |> WithComment "Get the character code of a keyboard event."
                "keyCode" =? T<int>
                |> WithComment "Get the key code of a keydown / keyup event."
                "keyIdentifier" =? T<string> |> Obsolete
                "keyLocation" =@ KeyLocationCode |> Obsolete
                "location" =? T<int>
                "metaKey" =? T<bool>
                "repeat" =? T<bool>
                "shiftKey" =? T<bool>
                "getModifierState" => T<string>?keyIdentifierArg ^-> T<bool>
                "initKeyboardEvent" =>
                    T<string>?typeArg *
                    T<bool>?canBubbleArg *
                    T<bool>?cancelableArg *
                    AbstractView *
                    T<string>?keyIdentifierArg *
                    KeyLocationCode *
                    T<string>?modifiersListArg ^-> T<unit>
                    |> Obsolete
                "initKeyboardEventNS" =>
                    T<string>?namespaceURIArg *
                    T<string>?typeArg *
                    T<bool>?canBubbleArg *
                    T<bool>?cancelableArg *
                    AbstractView *
                    T<string>?keyIdentifierArg *
                    KeyLocationCode *
                    T<string>?modifiersListArg ^-> T<unit>
                    |> Obsolete
                "which" =? T<int>
                |> WithComment "Get the key code of a keydown / keyup event, \
                                or the character code of a keypress event."
            ]
    let CompositionEvent =
        Class "CompositionEvent"
        |=> Inherits UIEvent
        |+> Instance [
                "data" =? T<string>
                "locale" =? T<string>
                "initCompositionEvent" =>
                    T<string>?typeArg *
                    T<bool>?canBubbleArg *
                    T<bool>?cancelableArg *
                    AbstractView *
                    T<string>?dataArg ^-> T<unit>
                    |> Obsolete
                "initCompositionEventNS" =>
                    T<string>?namespaceURIArg *
                    T<string>?typeArg *
                    T<bool>?canBubbleArg *
                    T<bool>?cancelableArg *
                    AbstractView *
                    T<string>?dataArg ^-> T<unit>
                    |> Obsolete
            ]

    let MutationRecord =
        Class "MutationRecord"
        |+> Instance [
            "type" =? T<string>
            "target" =? Node
            "addedNodes" =? NodeList
            "removedNodes" =? NodeList
            "previousSibling" =? Node
            "nextSibling" =? Node
            "attributeName" =? T<string>
            "attributeNamespace" =? T<string>
            "oldValue" =? T<string>
        ]

    let MutationObserverInit =
        Pattern.Config "MutationObserverInit" {
            Required = []
            Optional = 
                [
                    "attributes", T<bool>
                    "characterData", T<bool>
                    "attributeOldValue", T<bool>
                    "characterDataOldValue", T<bool>
                    "attributeFilter", T<string []>
                    "subTree", T<bool>
                    "childList", T<bool>
                ]
        }

    let MutationObserver =
        Class "MutationObserver"
        |+> Instance [
            "disconnect" => T<unit> ^-> T<unit>
            "observe" => (Node * !?MutationObserverInit) ^-> T<unit>
            "takeRecords" => T<unit> ^-> Type.ArrayOf MutationRecord
        ]
        |+> Static [
            Constructor ((!|MutationRecord * TSelf ^-> T<unit>)?callback)
        ]

    let MutationEvent =
        Class "MutationEvent"
        |=> Inherits Event
        |+> Instance [
                "attrChange" =@ attrChangeType
                "attrName" =@ T<string>
                "newValue" =@ T<string>
                "prevValue" =@ T<string>
                "relatedNode" =@ Node
                "initMutationEvent" =>
                    T<string>?typeArg *
                    T<bool>?canBubbleArg *
                    T<bool>?cancelableArg *
                    Node?relatedNodeArg *
                    T<string>?prevValueArg *
                    T<string>?newValueArg *
                    T<string>?attrNameArg *
                    attrChangeType ^-> T<unit>
                "initMutationEventNS" =>
                    T<string>?namespaceURIArg *
                    T<string>?typeArg *
                    T<bool>?canBubbleArg *
                    T<bool>?cancelableArg *
                    Node?relatedNodeArg *
                    T<string>?prevValueArg *
                    T<string>?newValueArg *
                    T<string>?attrNameArg *
                    attrChangeType ^-> T<unit>
            ]
        |> Obsolete

    let MutationNameEvent =
        Class "MutationNameEvent"
        |=> Inherits Event
        |+> Instance [
                "prevNamespaceURI" =@ T<string>
                "prevNodeName" =@ T<string>
                "initMutationNameEvent" =>
                    T<string>?typeArg *
                    T<bool>?canBubbleArg *
                    T<bool>?cancelableArg *
                    Node?relatedNodeArg *
                    T<string>?prevNamespaceURIArg *
                    T<string>?prevNodeNameArg ^-> T<unit>
                "initMutationNameEventNS" =>
                    T<string>?namespaceURIArg *
                    T<string>?typeArg *
                    T<bool>?canBubbleArg *
                    T<bool>?cancelableArg *
                    Node?relatedNodeArg *
                    T<string>?prevNamespaceURIArg *
                    T<string>?prevNodeNameArg ^-> T<unit>
            ]

    let DocumentEvent =
        Class "DocumentEvent"
        |+> Instance [
                "canDispatch" => T<string>?namespaceURI * T<string>?eventtype ^-> T<bool>
                "createEvent" => T<string>?eventType ^-> Event // user needs to downcast to the right event
                // the following methods are non-standard
                "createCustomEvent" => T<unit> ^-> CustomEvent
                |> WithInline "$0.createEvent(\"CustomEvent\")"
                "createMouseEvent" => T<unit> ^-> MouseEvent
                |> WithInline "$0.createEvent(\"MouseEvent\")"
                "createMouseWheelEvent" => T<unit> ^-> MouseWheelEvent
                |> WithInline "$0.createEvent(\"MouseWheelEvent\")"
                "createKeyboardEvent" => T<unit> ^-> KeyboardEvent
                |> WithInline "$0.createEvent(\"TextEvent\")"
                "createCompositionEvent" => T<unit> ^-> CompositionEvent
                |> WithInline "$0.createEvent(\"CompositionEvent\")"
                "createMutationEvent" => T<unit> ^-> MutationEvent
                |> WithInline "$0.createEvent(\"MutationEvent\")"
                "createMutationNameEvent" => T<unit> ^-> MutationNameEvent
                |> WithInline "$0.createEvent(\"MutationNameEvent\")"
            ]

    let ElementCreateOptions =
        Pattern.Config "ElementCreateOptions" {
            Required = []
            Optional = [ "is", T<string> ]
        }

    let CompatMode = 
        Pattern.EnumStrings "CompatMode"
            [
                "BackCompat"
                "CSS1Compat"
            ]

    let DesignMode = 
        Pattern.EnumStrings "DesignMode"
            [
                "on"
                "off"
            ]

    let Location = 
        Class "Location" 
        |+> Instance [
            "href" =@ T<string>
            "assign" => T<string> ^-> T<unit> 
            "replace" => T<string> ^-> T<unit> 
            "reload" => T<unit> ^-> T<unit> 
            "toString" => T<unit> ^-> T<string>

                // URL decomposition IDL attributes 
            "protocol" =@ T<string>
            "host" =@ T<string>
            "hostname" =@ T<string>
            "port" =@ T<string>
            "pathname" =@ T<string>
            "search" =@ T<string>
            "hash" =@ T<string>
            "origin" =? T<string>
            "username" =@ T<string>
            "password" =@ T<string>

        ]

    let Document =
        Document
        |=> Inherits Node
        |+> QuerySelectorMixin
        |+> ParentNodeMixin
        |+> Instance [
                "activeElement" =? Element
                "characterSet" =? T<string>
                "compatMode" =? CompatMode.Type
                "contentType" =? T<string>
                "cookie" =@ T<string>
                "body" =@ Element
                "defaultView" =? Window
                "designMode" =@ DesignMode.Type
                "dir" =@ T<string>
                "doctype" =? DocumentType
                "documentElement" =? Element
                "documentURI" =? T<string>
                "domain" =@ T<string> |> Obsolete
                "embeds" =? NodeList
                "fonts" =? T<obj> // TODO
                "fullscreenElement" =? !?Element
                "forms" =? NodeList
                "head" =? Element
                "hidden" =? T<bool>
                "images" =? NodeList
                "implementation" =@ DOMImplementation
                "inputEncoding" =? T<string> |> Obsolete
                "lastModified" =? T<string>
                "links" =? NodeList
                "location" =@ Location
                "lastStyleSheetSet" =? T<string>
                "pictureInPictureElement" =? !?Element
                "pictureInPictureEnabled" =? T<bool>
                "pointerLockElement" =? !?Element
                "scrollingElement" =? !?Element
                "plugins" =? NodeList
                "preferredStyleSheetSet" =? T<string>
                "readyState" =? T<string>
                "referrer" =? T<string>
                "scripts" =? NodeList
                "selectedStyleSheetSet" =@ T<string>
                "strictErrorChecking" =@ T<bool>
                "styleSheets" =? T<obj> // StyleSheetList
                "styleSheetSets" =? T<obj>
                "timeline" =? DocumentTimeLine
                    |> WithComment "Warning: This method is not supported in every browser."
                "title" =@ T<string>
                "URL" =? T<string>
                "visibilityState" =? T<string>
                "xmlEncoding" =@ T<string> |> Obsolete
                "xmlStandalone" =@ T<bool> |> Obsolete
                "xmlVersion" =@ T<string> |> Obsolete
                "adoptNode" => Node ^-> Node
                "createAttribute" => T<string> ^-> Attr
                "createAttributeNS" =>
                    T<string>?namespaceURI *
                    T<string>?qualifiedName ^-> Attr
                "createCDATASection" => T<string> ^-> CDATASection
                "createComment" => T<string> ^-> Comment
                "createDocumentFragment" => T<unit> ^-> DocumentFragment
                "createElement" =>
                    T<string> *
                    !?ElementCreateOptions?elementCreationOptions ^-> Element
                "createElementNS" =>
                    T<string>?namespaceURI *
                    T<string>?qualifiedName *
                    !?ElementCreateOptions?elementCreationOptions ^-> Element
                "createEntityReference" => T<string> ^-> EntityReference |> Obsolete
                "createProcessingInstruction" =>
                    T<string>?target *
                    T<string>?data ^-> ProcessingInstruction
                "createTextNode" => T<string> ^-> Text
                "enableStyleSheetForSet" => T<string> ^-> T<unit>
                "getElementsByClassName" => T<string> ^-> NodeList
                "getElementById" => T<string>?id ^-> Element
                "getElementsByTagName" => T<string> ^-> NodeList
                "getElementsByTagNameNS" =>
                    T<string>?namespaceURI *
                    T<string>?localName ^-> NodeList
                "importNode" => Node?importedNode * T<bool>?deep ^-> Node
                "normalizeDocument" => T<unit> ^-> T<unit> |> Obsolete
                "renameNode" =>
                    Node *
                    T<string>?namespaceURI *
                    T<string>?qualifiedName ^-> Node

                "close" => T<unit> ^-> T<unit>
                "open" => T<unit> ^-> T<unit>
                "write" => T<string> ^-> T<unit>
                "writeln" => T<string> ^-> T<unit>

                "execCommand" => T<string> * T<bool> * T<string> ^-> T<bool> |> Obsolete
                "getElementsByName" => T<string> ^-> NodeList
                "getSelection" => T<unit> ^-> T<obj>
                "hasFocus" => T<unit> ^-> T<bool>
                "queryCommandEnabled" => T<string> ^-> T<bool> |> Obsolete
                "queryCommandSupported" => T<string> ^-> T<bool> |> Obsolete

                "createNodeIterator" => (Node * !?T<int> * !?NodeFilter) ^-> NodeIterator
                "createTreeWalker" => (Node * !?T<int> * !?NodeFilter) ^-> TreeWalker

                "createRange" => T<unit> ^-> Range
                "createEvent" => T<string> ^-> Event

                "elementFromPoint" => T<int>?x * T<int>?y ^-> Element
                "elementsFromPoint" => T<int>?x * T<int>?y ^-> !|Element

                "exitPictureInPicture" => T<unit> ^-> WebSharper.JavaScript.Ecma.Definition.EcmaPromise.[T<unit>]
                |> WithWarning "Warning: This method is not supported in every browser"

                "exitPointerLock" => T<unit> ^-> T<unit>

                "hasStorageAccess" => T<unit> ^-> WebSharper.JavaScript.Ecma.Definition.EcmaPromise.[T<bool>]
                |> WithComment "Warning: This method is not supported in every browser."
                "requestStorageAccess" => T<unit> ^-> WebSharper.JavaScript.Ecma.Definition.EcmaPromise.[T<unit>]
                |> WithComment "Warning: This method is not supported in every browser."

                // Event handlers
                "onvisibilitychange" => T<unit> ^-> T<unit>
                |> ObsoleteWithMessage "Use OnVisibilityChange instead"
                "onvisibilitychange" =@ !?(Event ^-> T<unit>)
                |> WithSourceName "OnVisibilityChange"
                "onreadystatechange" => T<unit> ^-> T<unit>
                |> ObsoleteWithMessage "Use OnReadyStateChange instead"
                "onreadystatechange" =@ !?(Event ^-> T<unit>)
                |> WithSourceName "OnReadyStateChange"

                "getAnimations" => T<unit> ^-> !|Animation
            ]
        |+> Static [
                "Current" =? Document
                |> WithGetterInline "document"
                |> ObsoleteWithMessage "Use JS.Document or JS.Window.Document instead."
            ]

module Definition =
    module E = Enumerations
    module I = Interfaces

    let Namespaces =
        [
            Namespace "WebSharper.JavaScript.Dom" [
                I.Attr
                I.CDATASection
                I.CharacterData
                I.Comment
                I.DOMConfiguration
                I.DOMError
                I.DOMErrorHandler
                I.DOMException
                I.DOMImpementationSource
                I.DOMImplementation
                I.DOMImplementationList
                I.DOMLocator
                I.DOMStringList
                I.DOMRect
                I.Document
                I.DocumentFragment
                I.DocumentType
                I.Element
                I.Entity
                I.EntityReference
                I.NameList
                I.NamedNodeMap
                I.Node
                I.NodeList
                I.Notation
                I.ProcessingInstruction
                I.Text
                I.TypeInfo
                I.UserDataHandler
                I.Event
                I.EventInit
                I.EventTarget
                I.CustomEvent
                I.FocusEvent
                I.DocumentEvent
                I.DocumentView
                I.AbstractView
                I.UIEvent
                I.MouseButtons
                I.MouseEvent
                I.MouseWheelEvent
                I.WheelEvent
                I.KeyboardEvent
                I.CompositionEvent
                I.MutationEvent
                I.MutationNameEvent
                I.ShadowRoot
                I.ShadowRootInit
                I.ShadowRootMode
                I.DOMTokenList
                I.ElementCreateOptions
                I.MutationObserver
                I.MutationObserverInit
                I.MutationRecord
                I.NodeFilter
                I.NodeIterator
                I.HTMLCollection
                I.TreeWalker
                I.Range
                I.FullscreenNavigationUI
                I.FullscreenOptions
                I.ScrollAnimateMode
                I.ScrollOptions
                I.Animation
                I.AnimationPlayState
                I.AnimationReplaceState
                I.CompatMode
                I.DesignMode
                I.TimelinePhase
                I.AnimationTimeLine
                I.DocumentTimeLine
                I.DocumentTimeLineOptions
                I.AddEventListenerOptions
                I.EventListenerOptions
                I.AbortSignal
                E.DOMExceptionType
                E.DerivationMethod
                E.DocumentPosition
                E.ErrorSeverity
                E.NodeOperation
                E.NodeType
                E.PhaseType
                E.DeltaModeCode
                E.InputModeCode
                E.KeyLocationCode
                E.attrChangeType
            ]
        ]
