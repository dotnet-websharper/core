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

namespace WebSharper.JavaScript.Xhr

open WebSharper.InterfaceGenerator
open WebSharper.JavaScript

module Definition =

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
            "send" => !?Dom.Interfaces.Document?body ^-> T<unit>
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

    let Namespaces = 
        [
            Namespace "WebSharper.JavaScript" [
                XMLHttpRequest
                XMLHttpRequestEventTarget
                XMLHttpRequestResponseType
                XMLHttpRequestUpload
            ]
        ]