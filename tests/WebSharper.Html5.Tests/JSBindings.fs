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
open WebSharper.Testing


type Browser =
    | IE of string
    | Firefox of string
    | Edge of string
    | Chrome of string
    | Safari of string
    | Opera of string
    | Other of string

[<JavaScript>]
let BrowserVersion() =
    let ua = JS.Window.Navigator?userAgent
    let ua = ua.ToString()
    let regexstr = "(opera|chrome|safari|firefox|msie|trident(?=\/))\/?\s*(\d+)"
    let regexp = new RegExp(regexstr, "i")
    let m = regexp.Exec(ua)
    let m1, m2 =
        if Array.length m >= 3
            then m.[1], m.[2]
            else if Array.length m >= 2
                then m.[1], ""
                else "", ""
    if (new RegExp("trident", "i")).Test(m1) then
        let version = "\\brv[ :]+(\d+)"
        let iregexp = new RegExp(version, "g")
        let res = iregexp.Exec(ua)
        let ver = if Array.length res >= 2 then res.[1] else ""
        (Browser.IE ver)
    else if (m1 = "Chrome") then
        let iregexp = new RegExp("\\b(OPR|Edge)\/(\d+)", "")
        let im = iregexp.Exec(ua)
        match im with
        | null -> 
            (Browser.Chrome m2)
        | im ->
            let res = im.[1..]
            let br = res.[1]
            match br with
            | "OPR" -> (Browser.Opera res.[2])
            | "Edge" -> (Browser.Edge res.[2])
            | _ -> (Browser.Other ua)
    else if (m1 = "Firefox") then
        (Browser.Firefox m2)
    else
        (Browser.Other ua)
[<JavaScript>]
let isIE () =
    let bv = BrowserVersion()
    match bv with
    | (Browser.IE ver) -> true
    | _ -> false

[<JavaScript>]
let Tests =
    TestCategory "JSBindings" {

        Test "EcmaRegexp" {
            let regexp = new RegExp("a", "g")
            equalMsg regexp.Flags "g" ".flags"
            isFalseMsg regexp.Unicode ".unicode"
            isFalseMsg regexp.Sticky ".sticky"
            let teststr = "abacad"
            equalMsg (Array.length (regexp.Match(teststr))) 3 ".match()"
            equalMsg (regexp.Replace(teststr, "x")) "xbxcxd" ".replace()"
            equalMsg (regexp.Split(teststr)) [| ""; "b"; "c"; "d" |] ".split()"
            equalMsg (regexp.Search(teststr)) 0 ".search()"
        }

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
        }


//        do if not (isIE()) then
//            Test "Text" {
//                let exampleText = Dom.Text("example-text")
//                equalMsg (exampleText.WholeText) "example-text" "Check for initial value"
//                let splitRemained = exampleText.SplitText(7)
//                equalMsg (exampleText.WholeText) "example" "Check splitted value"
//                equalMsg (splitRemained.WholeText) "-text" "Check remaining value after splitting"
//            }
//
//            Test "Event" {
//                let event = new Dom.Event("click")
//                isFalseMsg event.Composed "Is composed (click)"
//                isFalseMsg event.IsTrusted "Is trusted (click)"
//                notEqualMsg event.CurrentTarget JS.Undefined "Checking current target"
//                notEqualMsg event.Target JS.Undefined "Checking target"
//                notEqualMsg event.TimeStamp JS.Undefined "Checking timestamp"
//                equalMsg event.Type "click" "Checking type"
//            }

        Test "Document" {
            let doc = JS.Document
            notEqualMsg doc.Cookie JS.Undefined "Checking body"
            notEqualMsg doc.Body JS.Undefined "Checking body"
            notEqualMsg doc.Head JS.Undefined "Checking head"
            notEqualMsg doc.Dir JS.Undefined "Checking dir"
            notEqualMsg doc.Doctype JS.Undefined "Checking doctype"
            notEqualMsg doc.DocumentElement JS.Undefined "Checking documentElement"
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
            notEqualMsg doc.Title JS.Undefined "Checking for title"
            notEqualMsg doc.URL JS.Undefined "Checking for URL"
            notEqualMsg doc.VisibilityState JS.Undefined "Checking for visibility"
        }

        Test "NodeIterator" {
            let doc = JS.Document
            let iterator = doc.CreateNodeIterator(doc)
            equalMsg (iterator.NextNode()) iterator.Root "Checking nextNode"
            equalMsg (iterator.PreviousNode()) iterator.Root "Checking previousNode"
        }

        Test "TreeWalker" {
            let doc = JS.Document
            let iterator = doc.CreateTreeWalker(doc)
            notEqualMsg (iterator.NextNode()) iterator.Root "Checking nextNode"
            equalMsg (iterator.PreviousNode()) iterator.Root "Checking previousNode"
            notEqualMsg (iterator.CurrentNode) JS.Undefined "Checking currentNode"
            notEqualMsg (iterator.NextSibling()) JS.Undefined "Checking nextSibling"
            notEqualMsg (iterator.PreviousSibling()) JS.Undefined "Checking previousSibling"
        }

//
//        Console.Log(BrowserVersion())
//        do if not (isIE()) then
//            Test "EcmaObject" {
//                let e = new JavaScript.Object()
//                let o1assign = New ( [ ("a",5 :> obj) ] )
//                let o2assign = New ( [ ("a",6 :> obj); ("b", 10 :> obj) ] )
//                let o1 = JavaScript.Object.Assign(e,o1assign)
//                isTrueMsg (JS.Undefined <> o1?a && o1?a = 5) "Checking assign without merging"
//                let e2 = new JavaScript.Object()
//                let o2 = JavaScript.Object.Assign(e2, o2assign, o1assign)
//                isTrueMsg (JS.Undefined <> o2?a && JS.Undefined <> o2?b && o2?a = 5 && o2?b = 10) "Checking assign with merging"
//                isFalseMsg (JavaScript.Object.Is(o1, o2)) "Checking equality with is() /not true "
//                isTrueMsg (JavaScript.Object.Is(o1, o1)) "Checking equality with is()"
//            }
//
//        do if not (isIE()) then
//                Test "EcmaMath" {
//                    equalMsg (Math.Cbrt 27.) 3. "Math.cbrt"
//                    equalMsg (Math.Clz32 1.) 31. "Math.clz32"
//                    equalMsg (Math.Expm1 0.) 0. "Math.expm1"
//                    equalMsg (Math.Hypot (3., 4.)) 5. "Math.hypot"
//                    equalMsg (Math.Imul (2., 4.)) 8. "Math.imul"
//                    equalMsg (Math.Fround(1.5)) 1.5 "Math.fround"
//                    equalMsg (Math.Atanh(1.)) JS.Infinity "Math.atanh"
//                    equalMsg (Math.Atanh(0.)) 0. "Math.atanh"
//                    equalMsg (Math.Asinh(0.)) (0.) "Math.asinh"
//                    equalMsg (Math.Log1p(Math.E - 1.)) 1. "Math.log1p"
//                    equalMsg (Math.Log10(1000.)) 3. "Math.log10"
//                    equalMsg (Math.Log2(16.)) 4. "Math.log2"
//                    equalMsg (Math.Sign(0.)) 0. "Math.sign of 0"
//                    equalMsg (Math.Sign(2.)) 1. "Math.sign of 2"
//                    equalMsg (Math.Sign(-5.)) -1. "Math.sign of -5"
//                    equalMsg (Math.Trunc(3.14)) 3. "Math.trunc"
//                }
//            else 
//                Test "EcmaMath" {
//                    equalMsg (Math.Trunc(3.14)) 3. "Math.trunc"
//                }
//
//        do if not (isIE()) then
//            Test "EcmaNumber" {
//                equalMsg (Number.ParseFloat("3.14")) 3.14 "Number.parseFloat"
//                equalMsg (Number.ParseInt("3.14")) 3 "Number.parseInt failed"
//                equalMsg (Number.ParseInt("3")) 3 "Number.parseInt"
//                isTrueMsg (Number.IsNaN(Number.ParseFloat("not-a-number"))) "Number.isNaN"
//                isTrueMsg (Number.IsFinite(5)) "Number.isFinite"
//                isFalseMsg (Number.IsFinite(JS.Infinity)) "Number.isFinite infinity"
//            }
//
//            Test "EcmaString" {
//                let exampleString = new String("example-string")
//                let repeatable = new String("rpt")
//                isTrueMsg (exampleString.EndsWith("in", 13)) "String.endsWith"
//                isFalseMsg (exampleString.EndsWith("asd")) "String.endsWith failed"
//                isTrueMsg (exampleString.Includes("ple")) "String.includes"
//                isFalseMsg (exampleString.Includes("ple", 8)) "String.includes"
//                equalMsg (repeatable.Repeat(3)) "rptrptrpt" "String.repeat"
//                equalMsg (exampleString.Substr(-5,3)) "tri" "String.substr"
//                isTrueMsg (exampleString.StartsWith("amp", 2)) "String.startsWith"
//            }


        //Test "Window" {
        //    let mywindow = JS.Window.Open("", "myWindow", "width=300, height=200")
        //    mywindow.Document.Write("<div style=\"height: 1900px\"></div>")
        //    mywindow.Scroll(0, 1200)
        //    equalMsg mywindow.ScrollY 1200. "scroll is working"
        //    let currentScreenX, currentScreenY = mywindow.ScreenX, mywindow.ScreenY
        //    mywindow.MoveBy(-50, -50)
        //    let targetY = if currentScreenY < 50 then 0 else currentScreenY - 50
        //    equalMsg mywindow.ScreenX (currentScreenX - 50) "moving horizontally is ok"
        //    equalMsg mywindow.ScreenY targetY "moving vertically is ok"
        //    mywindow.ResizeTo(500, 600)
        //    equalMsg mywindow.OuterWidth (500) "resizing horizontally is ok"
        //    equalMsg mywindow.OuterHeight (600) "resizing vertically is ok"
        //}
    }

