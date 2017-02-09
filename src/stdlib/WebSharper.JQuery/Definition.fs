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

    let JqXHR =
        Class "jQuery.jqXHR"

    let DataType =
        "xml html script json jsonp text".Split(' ')
        |> Pattern.EnumStrings "DataType"

    let MethodType =
        "GET POST PUT DELETE".Split(' ')
        |> Pattern.EnumStrings "MethodType"

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
                    "method", MethodType.Type
                    "mimeType", T<string>
                    "password", T<string>
                    "processData", T<bool>
                    "scriptCharset", T<string>
                    "statusCode", T<obj>
                    "success", T<obj> * T<string> * JqXHR ^-> T<unit>
                    "timeout", T<double>
                    "traditional", T<bool>
                    "type", MethodType.Type
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

    let JQueryClass = 
        JQ
        |+> Instance [
            // Ajax related instance methods
            "ajaxComplete" => AjaxHandler ^-> TSelf
            "ajaxError" => AjaxErrorHandler ^-> TSelf
            "ajaxSend" => AjaxHandler ^-> TSelf
            "ajaxStart" => T<unit->unit> ^-> TSelf
            "ajaxStop" => T<unit->unit> ^-> TSelf
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
            "prop" => T<string> ^-> T<obj>
            "prop" => T<string> * T<obj> ^-> TSelf
            "prop" => T<obj> ^-> TSelf
            "prop" => T<string> * (T<int> * T<obj> ^-> T<obj>) ^-> TSelf
            "removeAttr" => T<string> ^-> TSelf
            "removeClass" => T<string> ^-> TSelf
            "removeClass" => (T<int> * T<obj> ^-> T<string>) ^-> TSelf
            "removeProp" => T<string> ^-> TSelf
            "toggleClass" => T<string> ^-> TSelf
            "toggleClass" => T<string> * T<bool> ^-> TSelf
            "toggleClass" => (T<int> * T<string> * T<bool> ^-> T<string>) * !?T<bool> ^-> TSelf
            "val" => T<unit> ^-> (T<string> + T<float> + T<obj []>)
            "val" => (T<string> + T<float> + T<obj []>) ^-> TSelf
            "val" => (T<int> * T<string> ^-> T<string>) ^-> TSelf
        ]
        |+> Static [
            // Ajax related static methods
            "add" => T<string> * !? AjaxSettings ^-> JqXHR
            "add" => !?AjaxSettings ^-> JqXHR
            "ajaxPrefilter" => !?DataType * (AjaxSettings * T<obj> * JqXHR ^-> T<unit>) ^-> T<obj> // returns undefined
            "ajaxSetup" => AjaxSettings ^-> T<unit>
            "ajaxTransport" => DataType * (AjaxSettings * T<obj> * JqXHR ^-> T<unit>) ^-> T<obj> // returns undefined
            "get" => (T<string> * !?(T<obj> + T<string>) * !? (T<obj> * T<string> * JqXHR ^-> T<unit>) * !? DataType) ^-> JqXHR
            "get" => !?AjaxSettings ^-> JqXHR
            "getJSON" => (T<string> * !?(T<obj> + T<string>) * !? (T<obj> * T<string> * JqXHR ^-> T<unit>)) ^-> JqXHR
            "getScript" => (T<string> * !? (T<obj> * T<string> * JqXHR ^-> T<unit>)) ^-> JqXHR
            "param" => (T<obj> * T<obj []> * TSelf) * !?T<bool> ^-> T<string>
            "get" => (T<string> * !?(T<obj> + T<string>) * !? (T<obj> * T<string> * JqXHR ^-> T<unit>) * !? DataType) ^-> JqXHR
            "get" => !?AjaxSettings ^-> JqXHR
        ]
        
    
        
    let Assembly =
        Assembly [
            Namespace "WebSharper.JQuery" [
            ]
            Namespace "WebSharper.JQuery.Resources" [
                Resource "JQuery" "http://code.jquery.com/jquery-1.11.2.min.js" |> AssemblyWide
            ]
        ]

[<Sealed>]
type JQueryExtension() =
    interface IExtension with
        member x.Assembly = Definition.Assembly

[<assembly: Extension(typeof<JQueryExtension>)>]
do ()
