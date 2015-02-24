# WebSharper Extension for jQuery

|               |                       |
| --------------|-----------------------|
| Library Home  | [jquery.com][jq]      |
| Bound Version | 1.10.2                |
| NuGet Package | WebSharper            |
| Assembly      | WebSharper.JQuery.dll |
| Namespace     | WebSharper.JQuery     |

[jq]: http://jquery.com

Note: the jQuery extension is distributed and automatically included
with WebSharper.

The API is as far as possible a one-to-one mapping of the JavaScript
API, making it straightforward to convert existing jQuery code to F#.

## Selecting DOM Nodes

jQuery enables you to construct wrappers for DOM nodes or to create
new elements by supplying a string argument to the jQuery function:

    var itemElems = jQuery(".Item")
    var myNewElem = jQuery("<p>Foo</p>")

In the jQuery WebSharper extension, the same functionality is provided
by the static member `Of` on the `JQuery` class:

    let itemElems = JQuery.Of(".Item")
    let myNewElem = JQuery.Of("<p>Foo</p>")

## Methods

The return value of the `Of` method is an object of type `JQuery`,
containing all the familiar instance members.

The following JavaScript example shows how you can invoke the `ready`
function on the jQuery object:

    jQuery(document).ready(function(){
       // Your code here
    });
 
The equivallent code in WebSharper is:

    JQuery.Of(Dom.Document.Current).Ready(fun _ ->
        // Your code here
    )

In jQuery, functions are often flexible with regards to their input
parameters.  In F# this is represented by overloaded functions
corresponding to different ways to invoke a method.

For example the `fadeOut` function that hides an element after
applying a `fade-out` effect accepts various types of arguments, e.g:

    jQuery("#MyElem").fadeOut()
    jQuery("#MyElem").fadeOut("slow")
    jQuery("#MyElem").fadeOut(100, function () {alert("Faded out");})

In F#, you write:

    JQuery.Of("#MyElem").FadeOut()
    JQuery.Of("#MyElem").FadeOut("slow")
    JQuery.Of("#MyElem").FadeOut(100., fun () -> JavaScript.Alert "Faded out")

## Chaining

Just like in pure jQuery, __chaining__ of method invocations is
supported since the result type of most jQuery operations is another
jQuery object.  Here is an example of chaining in JavaScript:

    jQuery('#MyDiv').removeClass('Off').addClass('On')

And the corresponding code written in F#:

    JQuery.Of("#MyDiv").RemoveClass("Off").AddClass("On")

## Ignoring Return Values

For situations when the result of a method invocation can be ignored,
the extension provides an extra property `Ignore`, which simply
changes the return type to unit in F#:

    JQ.Of("#MyElem").FadeOut().Ignore

## Implicit Arguments

Callback functions in JavaScript are sometimes passed an implicit
argument - `this`.  jQuery makes heavy use of this idiom.  Here is an
example of the `each` function:

    jQuery("div").each(function () {
        jQuery(this).hide();
    });

The `this` object refers to the current element when traversing the
jQuery collection.

In the WebSharper extension `this` parameter is explicit.  The code is
written as:

    JQuery.Of("div").Each(fun el ->
        JQuery.of(el).Hide().Ignore
    ))

## DOM Manipulation

The following example changes the background of every second list item
in all the list with the ID `MyList`:

    [<JavaScript>]
    let ChangeBackground () =
        JQuery.Of("#MyList li").Each(fun (el: Dom.Element) ix ->
            if ix % 2 = 0 then
                JQuery.Of(el).Css("background-color", "red").Ignore
        )
        |> ignore

## Ajax

Here is an example using the `getJSON` function for fetching JSON data
from the server.

    [<JavaScriptType>]
    type Data =
        {
           Name : string
           Email : string
        }

    [<JavaScript>]
    let AjaxCall () =
        JQuery.GetJSON("data.json", fun (data, _) ->
            let data = As<Data> data
            let nameLabel =
                JQuery.Of("<div/>").Text("Name: " + data.Name)
            let descrLabel =
                JQuery.Of("<div/>").Text("Email: " + data.Email)
            JQuery.Of("<p/>").
                Append(nameLabel).
                Append(descrLabel).
                AppendTo("body").
                Ignore
        )

## Attaching Event Handlers

Below is an example of constructing a button and adding an event
handler for the click event:

    [<JavaScript>]
    let ButtonWithEvent () =
        JQuery.Of("<button/>")
            .Text("Click")
            .Click(fun _ _ ->
                Window.Alert("Button clicked"))
            .AppendTo("body")
            .Ignore
