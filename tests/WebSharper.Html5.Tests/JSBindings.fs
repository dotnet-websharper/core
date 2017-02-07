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

module WebSharper.Html5.Tests.JSBindings

open WebSharper
open WebSharper.JavaScript
open WebSharper.JQuery
open WebSharper.Testing


[<JavaScript>]
let Tests =
    TestCategory "JSBindings" {

        Test "Element" {
            let elem = JS.Document.CreateElement("div")
            equalMsg elem.OuterHTML "<div></div>" "OuterHTML checking"
            equalMsg elem.InnerHTML "" "InnerHTML checking for emptiness (no child)"
            elem.SetAttribute("height", "500")
            elem.SetAttribute("class", "example-class")
            elem.SetAttribute("id", "example-id")
            let child = JS.Document.CreateElement("div")
            child.SetAttribute("class", "child-example-class")
            elem.AppendChild(child) |> ignore
            notEqualMsg elem.InnerHTML "" "InnerHTML checking for emptiness (has child)"
            equalMsg elem.Attributes.Length 3 "Attribute count"
            isTrueMsg (elem.HasAttributes()) "Has attributes"
            notEqualMsg elem.ClassName "" "Class name checking for emptiness: "
            notEqualMsg elem.Id "" "Id checking for emptiness"
            isTrueMsg (elem.Prefix = null) "Prefix checking"
            equalMsg elem.LocalName "div" "Local name checking"
            isTrueMsg (elem.NamespaceURI <> "") "Checking namespace emptiness"
            notEqualMsg (elem.GetElementsByClassName("child-example-class").Length) 0 "Childs by name count"
            //        IE doesn't support the methods below
            equalMsg (elem.Closest("div")) elem "Checking closest div"
            equalMsg (elem.Closest("input")) null "Checking closest input"
            isTrueMsg (elem.Matches("div")) "Matching for div"
            isFalseMsg (elem.Matches("input")) "Matching for input"
        }

//        IE doesn't support constructing the object like this
        Test "Text" {
            let exampleText = Dom.Text("example-text")
            equalMsg (exampleText.WholeText) "example-text" "Check for initial value"
            let splitRemained = exampleText.SplitText(7)
            equalMsg (exampleText.WholeText) "example" "Check splitted value"
            equalMsg (splitRemained.WholeText) "-text" "Check remaining value after splitting"
        }


//        IE doesn't support constructing the object like this
        Test "Event" {
            let event = new Dom.Event("click")
            isFalseMsg event.Composed "Is composed (click)"
            isFalseMsg event.IsTrusted "Is trusted (click)"
            notEqualMsg event.CurrentTarget JS.Undefined "Checking current target"
            notEqualMsg event.Target JS.Undefined "Checking target"
            notEqualMsg event.TimeStamp JS.Undefined "Checking timestamp"
            equalMsg event.Type "click" "Checking type"
        }

        Test "Document" {
            let doc = JS.Document
            doc.Dir <- "rtl"
            notEqualMsg doc.Cookie JS.Undefined "Checking body"
            notEqualMsg doc.Body JS.Undefined "Checking body"
            notEqualMsg doc.Head JS.Undefined "Checking head"
            notEqualMsg doc.Dir "ltr" "Checking ltr (current rtl)"
            equalMsg doc.Dir "rtl" "Checking rtl (current rtl)"
            doc.Dir <- "ltr"
            notEqualMsg doc.Dir "rtl" "Checking rtl (current ltr)"
            equalMsg doc.Dir "ltr" "Checking ltr (current ltr)"
            notEqualMsg doc.Doctype JS.Undefined "Checking doctype"
            notEqualMsg doc.DocumentElement JS.Undefined "Checking documentElement"
            notEqualMsg doc.DocumentURI JS.Undefined "Checking documentURI"
            notEqualMsg doc.Domain JS.Undefined "Checking domain"
            notEqualMsg doc.Embeds JS.Undefined "Checking for embeds"
            notEqualMsg doc.Forms JS.Undefined "Checking for forms"
            notEqualMsg doc.Hidden JS.Undefined "Checking for hidden"
            notEqualMsg doc.Images JS.Undefined "Checking for images"
            notEqualMsg doc.Links JS.Undefined "Checking for links"
            notEqualMsg doc.LastModified JS.Undefined "Checking for lastModified"
            notEqualMsg doc.Plugins JS.Undefined "Checking for plugins"
            notEqualMsg doc.ReadyState JS.Undefined "Checking for readystate"
            notEqualMsg doc.Referrer JS.Undefined "Checking for referrer"
            notEqualMsg doc.Scripts JS.Undefined "Checking for scripts"
            notEqualMsg doc.Timeline JS.Undefined "Checking for timeline"
            notEqualMsg doc.Title JS.Undefined "Checking for title"
            notEqualMsg doc.URL JS.Undefined "Checking for URL"
            notEqualMsg doc.VisibilityState JS.Undefined "Checking for visibility"
        }

    }

