# WebSharper UI.Next

> [See the C# version of this documentation here.](UINext-CSharp.md)

UI.Next is a client-side library providing a novel, pragmatic and convenient approach to UI reactivity. It includes:

* A [dataflow layer](#dataflow) for expressing user inputs and values computed from them as time-varying values. This approach is related to Functional Reactive Programming (FRP), but differs from it in significant ways discussed [here](https://github.com/intellifactory/websharper.ui.next/blob/master/docs/FRP.md).
* A reactive [DOM library](#dom) for displaying these time-varying values in a functional way. If you are familiar with Facebook React, then you will find some similarities with this approach: instead of explicitly inserting, modifying and removing DOM nodes, you return a value that represents a DOM tree based on inputs. The main difference is that these inputs are nodes of the dataflow layer, rather than a single state value associated with the component.
* A [declarative animation system](#animation) for the DOM layer.

A full documentation for UI.Next is available [on GitHub](https://github.com/intellifactory/websharper.ui.next/blob/master/README.md). Samples can be browsed [here](http://intellifactory.github.io/websharper.ui.next.samples/).

<a name="basic-example"></a>
## A basic example

To get a taste of UI.Next's concepts, here is a very basic example: an input box, and a label that is automatically updated with the uppercased content of the input box.

```fsharp
open WebSharper.UI.Next
open WebSharper.UI.Next.Html
open WebSharper.UI.Next.Client

let basicExample() =

    let rvContent = Var.Create ""
    let vUpperContent =
        rvContent.View
        |> View.Map (fun t -> t.ToUpper())

    div [
        Doc.Input [] rvContent
        label [textView vUpperContent]
    ]
```

* First, we create a *reactive variable* `rvContent`, of type `Var<string>`. It is initialized with an empty string, and will be updated whenever the user inputs text.

* Then, we create a *view* on `rvContent`, which we call `vUpperContent`. A view is a value that cannot be set explicitly, but instead is set automatically based on the current value of one or several other values. In this case, `vUpperContent` is updated whenever `rvContent` is updated by applying the method `ToUpper()`.

* Finally, we render the page. The functions `div` and `label` create HTML elements. `Doc.Input` creates an HTML input element whose value is always synchronized with the current value of `rvContent`. `textView` creates a text node whose value is always synchronized with the current value of `vUpperContent`.

<a name="dataflow"></a>
## The Dataflow layer

The flow of time-varying values in UI.Next is represented as a dataflow graph. When the value of an input node (aka `Var`) is set, it is propagated through the internal nodes (aka `View`s) down to the output node (aka `Sink`).

### Reactive variables: `Var<'T>`

A value of type **`Var<'T>`** is an input node to the dataflow graph. Its value can be imperatively read or set using the `Value` property, the functions `Var.Get` and `Var.Set`, and the `<-` operator. It can also be associated with an element in the [DOM layer](#dom). In the [basic example](#basic-example), `rvContent` is a `Var<string>`.

```fsharp
let rvContent = Var.Create "initial value"
rvContent.Value <- "second value"
let secondValue = rvContent.Value
Var.Set rvContent "third value"
let thirdValue = Var.Get rvContent
```

A `Var<'T>` is essentially equivalent to a classic F# `'T ref`, except for the fact that it can be reactively observed by `View`s.

### Reactive views: `View<'T>`

A value of type **`View<'T>`** is an internal node in the dataflow graph. It is not possible to explicitly get or set the value of a `View<'T>`. Instead, at any time its value is determined by the value of the nodes that precede it in the dataflow graph, and can be observed by other `View`s. In the [basic example](#basic-example), `vUpperContent` is a `View<string>`.

* The simplest way to create a `View<'T>` is by using the **`View`** property of a `Var<'T>`, which creates a view whose value is always the current value of the `Var<'T>`.

    ```fsharp
    let rvContent = Var.Create "initial value"
    let vContent = rvContent.View
    // vContent's current value is now "initial value"
    rvContent.Value <- "new value"
    // vContent's current value is now "new value"
    ```

* A `View<'T>` can also be created using one of the various functions in the `View` module:

    * **`View.Const : 'T -> View<'T>`** creates a view whose value never changes.

    ```fsharp
    let vThree = View.Const 3
    // vThree's value will always be 3
    ```

    * **`View.FromVar : rv:Var<'T> -> View<'T>`** creates a view whose value is always the current value of `rv`. It is equivalent to `rv.View`.

    ```fsharp
    let rvContent = Var.Create "initial value"
    let vContent = View.FromVar rvContent
    // vContent's current value is now "initial value"
    rvContent.Value <- "new value"
    // vContent's current value is now "new value"
    ```

    * **`View.Map : f:('A -> 'B) -> v:View<'A> -> View<'B>`** creates a view whose value is always the result of calling `f` on the current value of `v`.

    ```fsharp
    let rvContent = Var.Create "initial value"
    let vContent = rvContent.View |> View.Map (fun t -> t.ToUpper())
    // vContent's current value is now "INITIAL VALUE"
    ```

    * **`View.MapAsync : f:('A -> Async<'B>) -> v:View<'A> -> View<'B>`** creates a view whose value is always the result of calling `f` on the current value of `v`. Note that if `v` is updated before the previous asynchronous call returns, then this previous call is discarded.

    * **`View.Map2 : f:('A -> 'B -> 'C) -> va:View<'A> -> vb:View<'B> -> View<'C>`** creates a view whose value is always the result of calling `f` on the current values of `va` and `vb`.

    ```fsharp
    type Person = { Name: string; Age: int }

    let rvName = Var.Create "John Doe"
    let rvAge = Var.Create 42
    let vPerson = View.Map2 (fun n a -> { Name = n; Age = a }) rvName rvAge
    // vPerson's current value is now { Name = "John Doe"; Age = 42 }
    rvName.Value <- "Jane Doe"
    // vPerson's current value is now { Name = "Jane Doe"; Age = 42 }
    ```

    * **`View.Apply : vf:View<'A -> 'B> -> va:View<'A> -> View<'B>`** creates a view whose value is always the result of calling the current value of `vf` on the current value of `va`. It is particularly useful in combination with `View.Const` to do the same as `View.Map2` but with more than two views:

    ```fsharp
    type Person = { FirstName: string; LastName: string; Age: int }

    let rvFirstName = Var.Create "John"
    let rvLastName = Var.Create "Doe"
    let rvAge = Var.Create 42
    let (<*>) f x = View.Apply f x
    let vPerson =
        View.Const (fun f l a -> { FirstName = f; LastName = l; Age = a })
        <*> rvFirstName.View
        <*> rvLastName.View
        <*> rvAge.View
    ```

    * **`View.Bind : f:('A -> View<'B>) -> View<'A> -> View<'B>`** is an important function because it allows a subgraph to change depending on its inputs. For example, in the following code, when `rvIsEmail`'s value is true, the graph contains `rvEmail` as a node, and when it is false, it contains `rvUsername` as a node instead.

    ```fsharp
    type UserId =
        | Username of string
        | Email of string

    let rvIsEmail = Var.Create false
    let rvUsername = Var.Create ""
    let rvEmail = Var.Create ""
    let vUserId =
        rvIsEmail |> View.Bind (fun isEmail ->
            if isEmail then
                View.Map Email rvEmail.View
            else
                View.Map Username rvUsername.View
        )
    ```

    * **`View.Join : View<View<'A>> -> View<'A>`** "flattens" a view of a view. It can be used equivalently to `Bind`, as the following equalities hold:

    ```fsharp
    View.Bind f x = View.Join (View.Map f x)
	View.Join x = View.Bind id x
    ```

    Dynamic composition via `View.Bind` and `View.Join` should be used with some care. Whenever static composition (such as `View.Map2`) can do the trick, it should be preferred. One concern here is efficiency, and another is state, identity and sharing (see [Sharing](https://github.com/intellifactory/websharper.ui.next/blob/master/docs/Sharing.md) for a discussion).

    * **`View.SnapshotOn : init:'B -> va:View<'A> -> vb:View<'B> -> View<'B>`** produces a snapshot of `vb`: a `View` that has the same value as `vb`, except that it is only updated when `va` is updated. Before `va` is first updated, the result `View` has the value `init`.

    `SnapshotOn` is typically used to bring events such as submit buttons into the dataflow graph.

    ```fsharp
    type LoginData = { Username: string; Password: string }

    let rvSubmit = Var.Create ()
    let rvUsername = Var.Create ""
    let rvPassword = Var.Create ""
    let vLoginData =
        View.Const (fun u p -> Some { Username = u; Password = p })
        <*> rvUsername.View
        <*> rvPassword.View
        |> View.SnapshotOn () rvSubmit.View
    ```

    In the above example, `vLoginData` is initialized with `None`, and is updated with the current login data wrapped in `Some` whenever `rvSubmit` is updated. It is then possible to map a `View` or a `Sink` on `vLoginData` that performs the actual login.

    * **`View.Convert : f:('A -> 'B) -> v:View<seq<'A>> -> View<seq<'B>>`** maps views on sequences with "shallow" memoization. The process remembers inputs from the previous step, and reuses outputs from the previous step when possible instead of calling the converter function. Memory use is proportional to the longest sequence taken by the View. Since only one step of history is retained, there is no memory leak. Requires equality on `'A`.

    * **`View.ConvertBy : key:('A -> 'K) -> f:('A -> 'B) -> v:View<seq<'A>> -> View<seq<'B>>`** is a variant on `Convert` that uses a `key` function to determine identity on inputs, rather than an equality constraint on the type `'A` itself.

    * **`View.ConvertSeq : f:(View<'A> -> 'B) -> v:View<seq<'A>> -> View<seq<'B>>`** is an extended form of `Convert` where the conversion function accepts a reactive view. At every step, changes to inputs identified as being the same object are propagated via that view. Requires equality on `'A`.

    * **`View.ConvertSeqBy : key:('A -> 'K) -> f:(View<'A> -> 'B) -> v:View<seq<'A>> -> View<seq<'B>>`** is a variant on `ConvertSeq` that uses a `key` function to determine identity on inputs, rather than an equality constraint on the type `'A` itself.

### Sinks

Once a graph is built out of `Var`s and `View`s, it needs to be run to react to changes.

The function **`View.Sink : f:('T -> unit) -> v:View<'T> -> unit`** is the output node of the dataflow graph. This function calls `f` with the current value of `v` whenever it is updated. It is highly recommended to have a single `Sink` running per dataflow graph; memory leaks may happen if the application repeatedly spawns `Sink` processes that never get collected. See [Leaks](https://github.com/intellifactory/websharper.ui.next/blob/master/docs/Leaks.md) for more information.

```fsharp
type LoginData = { Username: string; Password: string }

let rvSubmit = Var.Create ()
let rvUsername = Var.Create ""
let rvPassword = Var.Create ""
do
    View.Const (fun u p -> Some { Username = u; Password = p })
    <*> rvUsername.View
    <*> rvPassword.View
    |> View.SnapshotOn () rvSubmit.View
    |> View.Sink (function
        | None -> ()
        | Some loginData ->
            Rpc.LoginUser loginData // An [<Rpc>] function that logs in the user
            |> Async.Start)
```

It is relatively rare to call `View.Sink` directly. Instead, views are generally connected to the [DOM layer](#dom), which itself calls `Sink` when inserted into the document.

<a name="dom"></a>
## The DOM layer

In UI.Next, the type `Doc` represents a DOM snippet, i.e. a sequence of HTML or SVG elements. A `Doc` can be empty, a single element or several elements. Unlike [WebSharper.Html.Client](HtmlCombinators.md), the `Doc` API is mostly generative: it is not advised to imperatively insert nodes or change their contents. Instead, dynamic nodes are generated based on a dataflow graph.

Most of the following functions are located in the namespace `WebSharper.UI.Next`. Convenience functions such as individual HTML elements and attributes require `WebSharper.UI.Next.Html` to be open. Dynamic functions that involve `Var`s, `View`s or `Dom.Element`s are under `WebSharper.UI.Next.Client`.

### Creating Docs

The following functions create Docs:

* **`Doc.TextNode : string -> Doc`** creates a `Doc` composed of a single text node with the given contents.

    ```fsharp
    let myDoc = Doc.TextNode "WebSharper"
    // myDoc HTML equivalent is now: WebSharper
    ```

	This is aliased as **`text`** when the namespace `WebSharper.UI.Next.Html` is open.

* **`Doc.TextView : View<string> -> Doc`** creates a `Doc` composed of a single text node whose contents is always equal to the value of the given `View`.

    ```fsharp
    let rvText = Var.Create "WebSharper"
    let myDoc = Doc.TextView rvText.View
    // myDoc HTML equivalent is now: WebSharper
    rvText.Value <- "UI.Next"
    // myDoc HTML equivalent is now: UI.Next
    ```

	This is aliased as **`textView`** when the namespace `WebSharper.UI.Next.Html` is open.

* **`Doc.Element : string -> seq<Attr> -> seq<Doc> -> Doc`** creates a `Doc` composed of a single HTML element with the given tag name, attributes and children.

    ```fsharp
    let myDoc = Doc.Element "a" [attr.href "http://websharper.com"] [textNode "WebSharper"]
    // myDoc HTML equivalent is now: <a href="http://websharper.com">WebSharper</a>
    ```

    * The namespace `WebSharper.UI.Next.Html` contains functions for all standard HTML tags. Each element comes in two flavors: **`div`** creates a div with child elements, and **`divAttr`** creates a div with attributes and child elements. A few of them need to be wrapped in backticks \`\`like so\`\` to avoid collisions with keywords such as `base` or `use`.

    ```fsharp
    let myDoc1 = span [textNode "WebSharper"]
    // myDoc1 HTML equivalent is now: <span>WebSharper</span>
    let myDoc = aAttr [attr.href "http://websharper.com"] [textNode "WebSharper"]
    // myDoc HTML equivalent is now: <a href="http://websharper.com">WebSharper</a>
    ```

* **`Doc.SvgElement : string -> seq<Attr> -> seq<Doc> -> Doc`** creates a `Doc` composed of a single SVG element with the given tag name, attributes and children.

    ```fsharp
    let myDoc =
        Doc.SvgElement "rect" [
            Attr.Create "width" "150"
            Attr.Create "height" "120"
        ] []
    // myDoc HTML equivalent is now: <svg:rect width="150" height="120" />
    ```

    * The modules `WebSharper.UI.Next.SvgElements` and `WebSharper.UI.Next.SvgAttributes` contain functions for all standard SVG tags and attributes.

    ```fsharp
    let myDoc =
        SvgElements.rectAttr [
            SvgAttributes.width "150"
            SvgAttributes.height "120"
        ] []
    // myDoc HTML equivalent is now: <svg:rect width="150" height="120" />
    ```

* **`Doc.Static : Dom.Element -> Doc`** creates a `Doc` composed of a single element.

    ```fsharp
    let myElt = JS.Document.CreateElement "div"
    let myDoc = Doc.Static myElt
    // myDoc HTML equivalent is now: <div></div>
    ```

* **`Doc.Empty : Doc`** creates an empty `Doc`, ie. a `Doc` composed of zero elements.

* **`Doc.Append : Doc -> Doc -> Doc`** creates a `Doc` composed of two `Doc`s in sequence.

    ```fsharp
    let myDoc =
        Doc.Append
            (textNode "Visit ")
            (aAttr [attr.href "http://websharper.com"] [textNode "WebSharper"])
    // myDoc HTML equivalent is now: Visit <a href="http://websharper.com">WebSharper</a>
    ```

* **`Doc.Concat : seq<Doc> -> Doc`** creates a `Doc` composed of several `Doc`s in sequence.

    ```fsharp
    let myDoc =
        Doc.Concat [
            textNode "Visit "
            aAttr [attr.href "http://websharper.com"] [textNode "WebSharper"]
            textNode "!"
        ]
    // myDoc HTML equivalent is now: Visit <a href="http://websharper.com">WebSharper</a>!
    ```

* **`Doc.EmbedView : View<Doc> -> Doc`** allows the actual DOM structure of a `Doc` to depend on the dataflow graph.

    ```fsharp
    let rvText = Var.Create (Some "some text")
    let myDoc =
        rvText.View
        |> View.Map (function
            | Some t -> b [textNode t]
            | None -> i [textNode "(no value)"])
        |> Doc.EmbedView
    // myDoc HTML equivalent is now: <b>some text</b>
    rvText.Value <- None
    // myDoc HTML equivalent is now: <i>(no value)</i>
    ```

For convenience, `View.Convert*` functions are specialied for the `Doc` type, so that the resulting sequence is passed to `Doc.Concat` and `Doc.EmbedView`.

* **`Doc.Convert : ('T -> Doc) -> View<seq<'T>> -> Doc`**

* **`Doc.ConvertBy : ('T -> 'K) -> ('T -> Doc) -> View<seq<'T>> -> Doc`**

* **`Doc.ConvertSeq : (View<'T> -> Doc) -> View<seq<'T>> -> Doc`**

* **`Doc.ConvertSeqBy : ('T -> 'K) -> (View<'T> -> Doc) -> View<seq<'T>> -> Doc`**

### Creating input elements

The following functions create elements which can be used to set the value of a `Var` based on user input.

* **`Doc.Input : seq<Attr> -> Var<string> -> Doc`** creates an `<input>` text box synchronized with the given `Var`. The `Var` is updated on user input, and the text box is updated when the `Var` changes.

    ```fsharp
    let rvText = Var.Create "initial value"
    let myDoc = Doc.Input rvText
    // myDoc HTML equivalent is now: <input type="text" value="initial value" />
    // User types "!"...
    // myDoc HTML equivalent is now: <input type="text" value="initial value!" />
    rvText.Value <- "new value"
    // myDoc HTML equivalent is now: <input type="text" value="new value" />
    ```

* **`Doc.InputArea : seq<Attr> -> Var<string> -> Doc`** is similar, but creates a `<textarea>`.

* **`Doc.PasswordBox : seq<Attr> -> Var<string> -> Doc`** is similar, but creates an `<input type="password">`.

* **`Doc.IntInput : seq<Attr> -> Var<int> -> Doc`** and **`Doc.FloatInput : seq<Attr> -> Var<float> -> Doc`** are similar, but create an `<input type="number">`.

* **`Doc.Button : caption: string -> seq<Attr> -> (unit -> unit) -> Doc`** creates a `<button>` that calls the given callback on click. If you have a `rvSubmit : Var<unit>`, then `Var.Set rvSubmit` is a callback that can be passed to `Doc.Button`.

    Here is the login box sample from `SnapshotOn` above, with a document to render it:

    ```fsharp
    type LoginData = { Username: string; Password: string }

    let rvSubmit = Var.Create ()
    let rvUsername = Var.Create ""
    let rvPassword = Var.Create ""
    let vLoginResult =
        View.Const (fun u p -> Some { Username = u; Password = p })
        <*> rvUsername.View
        <*> rvPassword.View
        |> View.SnapshotOn () rvSubmit.View
        |> View.MapAsync (function
            | None ->
                async { return () }
            | Some loginData ->
                Rpc.LoginUser loginData)
    div [
        Doc.Input [] rvUsername
        Doc.PasswordBox [] rvPassword
        Doc.Button "Log in" [] (Var.Set rvSubmit)
        vLoginResult |> View.Map (fun () -> Doc.Empty) |> Doc.EmbedView
    ]
    ```

* **`Doc.Link : caption: string -> seq<Attr> -> (unit -> unit) -> Doc`** is similar to `Button`, but creates an `<a>` link instead.

* **`Doc.CheckBox : seq<Attr> -> Var<bool> -> Doc`** creates a checkbox whose checked state reflects the value of the given boolean `Var`.

* **`Doc.CheckBoxGroup : seq<Attr> -> 'T -> Var<list<'T>> -> Doc`** creates a checkbox whose checked state reflects the presence or absence of the given value in the given list `Var`. For example, given `Doc.CheckBoxGroup [] "test" rvMyVar`, ticking the checkbox will add `"test"` to the list held by `rvMyVar` and unticking the checkbox will remove it. No ordering of values in the list is guaranteed.

* **`Doc.Select : seq<Attr> -> ('T -> string) -> list<'T> -> Var<'T> -> Doc`** creates a selection box from the given list. It requires a function to show each item, and a variable which is updated with the currently-selected item.

* **`Doc.Radio : seq<Attr> -> 'T -> Var<'T> -> Doc`** creates a radio button whose checked state reflects whether the given `Var`'s value is currently equal to the given value.

### Inserting Docs in a document

There are several ways to insert a `Doc` in a document:

* **`Doc.Run : Dom.Element -> Doc -> unit`** renders the `Doc` as the children of the given DOM element.

* **`Doc.RunById : string -> Doc -> unit`** is similar to `Doc.Run`, but takes an element identifier to locate the container element.

* The type `Doc` implements the interface **`WebSharper.Html.Client.IControlBody`**; it is therefore possible to use it as the body of a `Web.Control` or to pass it to the function `ClientSide` from `WebSharper.Html.Server`.

    ```fsharp
    open WebSharper
    open WebSharper.UI.Next
    open WebSharper.UI.Next.Html

    type MyControl() =
        inherit Web.Control()

        [<JavaScript>]
        override this.Body =
            let rvText = Var.Create ""
            Doc.Concat [
                Doc.Input rvText
                Label [] [Doc.TextView rvText.View]
            ] :> _
    ```

* **`Doc.AsPagelet : Doc -> Pagelet`** wraps a `Doc` in a div tag of type `WebSharper.Html.Client.Pagelet`, allowing it to be inserted in an `Html.Client`-based control.

### Attributes

Just like `Doc`, the `Attr` type is monoidal: it represents zero, one or many DOM attributes. DOM attributes can be created with the following functions:

* **`Attr.Create : name:string -> value:string -> Attr`** represents a single attribute with the given `name` and `value`.

    * For convenience, when `WebSharper.UI.Next.Html` is open, standard attributes such as `href` are available as **`attr.href`**. A few of them need to be wrapped in backticks \`\`like so\`\` to avoid collisions with keywords such as `class` or `default`.

* **`Attr.Dynamic : name:string -> value:View<string> -> Attr`** represents a single attribute with the given `name` and varying `value`.

    * For convenience, when `WebSharper.UI.Next.Html` and `WebSharper.UI.Next.Client` are open, standard attributes such as `href` are available as **`attr.hrefDyn`**.

* **`Attr.Handler : name:string -> callback:(Dom.Element -> #Dom.Event -> unit) -> Attr`** specifies a handler for a DOM event, such as click event for a button. The `name` of the event doesn't include the `on` prefix, for example: `Attr.Handler "click" callback`.

    * For convenience, when `WebSharper.UI.Next.Html` and `WebSharper.UI.Next.Client` are open, standard event handlers such as `onclick` are available as **`on.click`**.

* **`Attr.HandlerView : name:string -> View<'T> -> (Dom.Element -> #Dom.Event -> 'T -> unit) -> Attr`** specifies a handler for a DOM event, and additionally the handler receives the current value of the given view. The `name` of the event doesn't include the `on` prefix, for example: `Attr.Handler "click" callback`.

    * For convenience, when `WebSharper.UI.Next.Html` and `WebSharper.UI.Next.Client` are open, standard event handlers such as `onclick` are available as **`on.clickView`**.

* **`Attr.Append : Attr -> Attr -> Attr`** combines two collections of attributes into one.

* **`Attr.Concat : seq<Attr> -> Attr`** concatenates multiple collections of attributes into one.

* **`Attr.Empty : Attr`** is the empty collection of attributes.

* **`Attr.Class : name:string -> Attr`** specifies a class attribute. Classes are additive, so:

    ```fsharp
    Attr.Append (Attr.Class "a") (Attr.Class "b") = Attr.Create "class" "a b"
    ```

* **`Attr.DynamicClass : View<'T> -> pred:('T -> bool) -> Attr`** specifies a class that is added when `pred` is true and removed when `pred` is false.

* **`Attr.Style : name:string -> value:string -> Attr`** specifies a CSS style property, such as `Attr.Style "background-color" "black"`.

* **`Attr.DynamicStyle : string -> View<string> -> Attr`** specifies a varying CSS style property.

* **`Attr.DynamicPred : name: string -> pred:View<bool> -> value:View<string> -> Attr`** specifies an attribute with the given `name` and varying `value` when `pred` is true, and unsets it when `pred` is false.

    * For convenience, when `WebSharper.UI.Next.Html` and `WebSharper.UI.Next.Client` are open, standard attributes such as `href` are available as **`attr.hrefDynPred`**.

<a name="animation"></a>
## Animation

UI.Next allows you to create animations declaratively, which are then run when a `View` changes. There are three main types involved in animation:

* **`Anim<'T>`** is an animation for a type `'T`. It represents a function from time to `'T`.

* **`Trans<'T>`** defines a transition for a type `'T`. It indicates the `Anim<'T>` that should be played on enter, change and exit.

* And finally `Attr`, via the functions **`Attr.Animated`** and **`Attr.AnimatedStyle`**, attaches a transition to a given attribute or style of an element. For convenience, when `WebSharper.UI.Next.Html` and `WebSharper.UI.Next.Client` are open, standard attributes such as `href` are available as `attr.hrefAnim`.

Here is an example for an element that enters from the left and leaves to the right using a cubic animation, and otherwise moves linearly according to `rvLeftPos`:

```fsharp
let linearAnim = Anim.Simple Interpolation.Double (Easing.Custom id) 300.
let cubicAnim = Anim.Simple Interpolation.Double Easing.CubicInOut 300.
let swipeTransition =
    Trans.Create linearAnim
	|> Trans.Enter (fun x -> cubicAnim (x - 100.) x)
	|> Trans.Exit (fun x -> cubicAnim x (x + 100.))
let rvLeftPos = Var.Create 0.
divAttr [
	Attr.Style "position" "relative"
	Attr.AnimatedStyle "left" swipeTransition rvLeftPos.View (fun pos -> string pos + "%")
] [
	Doc.TextNode "content"
]
```
