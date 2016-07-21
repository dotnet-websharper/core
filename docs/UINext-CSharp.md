# WebSharper UI.Next

UI.Next is a client-side library providing a novel, pragmatic and convenient approach to UI reactivity. It includes:

* A [dataflow layer](#dataflow) for expressing user inputs and values computed from them as time-varying values. This approach is related to Functional Reactive Programming (FRP), but differs from it in significant ways discussed [here](https://github.com/intellifactory/websharper.ui.next/blob/master/docs/FRP.md).
* A reactive [DOM library](#dom) for displaying these time-varying values in a functional way. If you are familiar with Facebook React, then you will find some similarities with this approach: instead of explicitly inserting, modifying and removing DOM nodes, you return a value that represents a DOM tree based on inputs. The main difference is that these inputs are nodes of the dataflow layer, rather than a single state value associated with the component.
* A [declarative animation system](#animation) for the DOM layer.

A full documentation for UI.Next is available [on GitHub](https://github.com/intellifactory/websharper.ui.next/blob/master/README.md). Samples can be browsed [here](http://intellifactory.github.io/websharper.ui.next.samples/).

<a name="basic-example"></a>
## A basic example

To get a taste of UI.Next's concepts, here is a very basic example: an input box, and a label that is automatically updated with the uppercased content of the input box.

```csharp
using WebSharper;
using WebSharper.UI.Next;
using WebSharper.UI.Next.Client;
using WebSharper.UI.Next.CSharp;
using static WebSharper.UI.Next.CSharp.Client.Html;

namespace MyWebsite
{
    public class MyWebsite
    {
        [SPAEntryPoint]
        public static void Main()
        {
            Var<string> rvContent = Var.Create("");
            View<string> vUpperContent = rvContent.View.Map(t => t.ToUpper());

            div(
                input(rvContent),
                label(vUpperContent)
            )
                .RunById("main");
        }
    }
}
```

* First, we create a *reactive variable* `rvContent`, of type `Var<string>`. It is initialized with an empty string, and will be updated whenever the user inputs text.

* Then, we create a *view* on `rvContent`, which we call `vUpperContent`. A view is a value that cannot be set explicitly, but instead is set automatically based on the current value of one or several other values. In this case, `vUpperContent` is updated whenever `rvContent` is updated by applying the method `ToUpper()`.

* Finally, we render the page. The functions `div` and `label` create HTML elements. `Doc.Input` creates an HTML input element whose value is always synchronized with the current value of `rvContent`. `textView` creates a text node whose value is always synchronized with the current value of `vUpperContent`.

<a name="dataflow"></a>
## The Dataflow layer

The flow of time-varying values in UI.Next is represented as a dataflow graph. When the value of an input node (aka `Var`) is set, it is propagated through the internal nodes (aka `View`s) down to the output node (aka `Sink`).

The following classes and methods require the following `using` statements:

```csharp
using WebSharper.UI.Next;
using WebSharper.UI.Next.CSharp;
```

### Reactive variables: `Var<T>`

A value of type **`Var<T>`** is an input node to the dataflow graph. Its value can be imperatively read or set using the `Value` property, or the functions `Var.Get` and `Var.Set`. It can also be associated with an element in the [DOM layer](#dom). In the [basic example](#basic-example), `rvContent` is a `Var<string>`.

```csharp
Var<string> rvContent = Var.Create("initial value")
rvContent.Value = "second value";
string secondValue = rvContent.Value;
Var.Set(rvContent, "third value");
string thirdValue = Var.Get rvContent;
```

A `Var<T>` is essentially equivalent to a classic F# `'T ref`, except for the fact that it can be reactively observed by `View`s.

### Reactive views: `View<T>`

A value of type **`View<T>`** is an internal node in the dataflow graph. It is not possible to explicitly get or set the value of a `View<T>`. Instead, at any time its value is determined by the value of the nodes that precede it in the dataflow graph, and can be observed by other `View`s. In the [basic example](#basic-example), `vUpperContent` is a `View<string>`.

* The simplest way to create a `View<T>` is by using the **`View`** property of a `Var<T>`, which creates a view whose value is always the current value of the `Var<T>`.

    ```csharp
    Var<string> rvContent = Var.Create("initial value");
    View<string> vContent = rvContent.View;
    // vContent's current value is now "initial value"
    rvContent.Value = "new value";
    // vContent's current value is now "new value"
    ```

* A `View<T>` can also be created using one of the various static methods in the `View` class:

    * **`View<T> Const(T value)`** creates a view whose value never changes.

    ```csharp
    View<int> vThree = View.Const(3);
    // vThree's value will always be 3
    ```

    * **`View<T> FromVar(Var<T> rv)`** creates a view whose value is always the current value of `rv`. It is equivalent to `rv.View`.

    ```csharp
    Var<string> rvContent = Var.Create("initial value");
    View<string> vContent = View.FromVar(rvContent);
    // vContent's current value is now "initial value"
    rvContent.Value = "new value";
    // vContent's current value is now "new value"
    ```

    * **`View<U> Map(this View<T> v, Func<T, U> f)`** creates a view whose value is always the result of calling `f` on the current value of `v`.

    ```csharp
    Var<string> rvContent = Var.Create("initial value");
    View<string> vContent = rvContent.View.Map(t => t.ToUpper());
    // vContent's current value is now "INITIAL VALUE"
    ```

    * **`View<U> MapAsync(this View<T> v, Func<T, Task<U>> f)`** creates a view whose value is always the result of calling `f` on the current value of `v`. Note that if `v` is updated before the previous asynchronous call returns, then this previous call is discarded.

    * **`View<V> Map2(this View<T> v1, View<U> v2, Func<T, U, V> f)`** creates a view whose value is always the result of calling `f` on the current values of `v1` and `v2`.

    ```csharp
    class Person
    {
        public string Name { get; set; }
        public int Age { get; set; }
    }

    Var<string> rvName = Var.Create("John Doe");
    Var<int> rvAge = Var.Create(42);
    View<Person> vPerson = rvName.View.Map2(rvAge.View,
        (n, a) => new Person { Name = n, Age = a });
    // vPerson's current value is now { Name = "John Doe", Age = 42 }
    rvName.Value = "Jane Doe";
    // vPerson's current value is now { Name = "Jane Doe", Age = 42 }
    ```

    * **`View<U> Bind(this View<T> v, Func<T, View<U>> f)`** is an important function because it allows a subgraph to change depending on its inputs. For example, in the following code, when `rvIsEmail`'s value is true, the graph contains `rvEmail` as a node, and when it is false, it contains `rvUsername` as a node instead.

    ```csharp
    abstract class UserId { }

    class UserName : UserId
    {
        public string Name { get; set; }
    }

    class Email : UserId
    {
        public string Value { get; set; }
    }

    Var<bool> rvIsEmail = Var.Create(false);
    Var<UserId> rvEmail = Var.Create<UserId>(new Email { Value = "" });
    Var<UserId> rvUsername = Var.Create<UserId>(new UserName { Name = "" });

    View<UserId> vUserId = rvIsEmail.View.Bind(isEmail =>
        isEmail ? rvEmail.View : rvUsername.View
    );
    ```

    * **`View<T> Join(this View<View<T>> v)`** "flattens" a view of a view. It can be used equivalently to `Bind`, as the following equalities hold:

    ```csharp
    v.Bind(f) == v.Map(f).Join()
    v.Join() == v.Bind(x => x)
    ```

    Dynamic composition via `View.Bind` and `View.Join` should be used with some care. Whenever static composition (such as `View.Map2`) can do the trick, it should be preferred. One concern here is efficiency, and another is state, identity and sharing (see [Sharing](https://github.com/intellifactory/websharper.ui.next/blob/master/docs/Sharing.md) for a discussion).

    * **`Submitter<T> Submitter.Create(View<T> v, T init)`** creates a `Submitter`, a related and useful class. `Submitter` has a `.Trigger()` method and an output `.View` which, at any point in time, has the value that `v` had the last time it was `Trigger`ed. When it has never been `Trigger`ed yet, the output `View`'s value is `init`.
    
    `Submitter`s are typically used to bring events such as submit buttons into the dataflow graph.

    ```csharp
    public class LoginData
    {
        public string Username { get; set; }
        public string Password { get; set; }
    }

    var rvUsername = Var.Create("");
    var rvPassword = Var.Create("");
    var vLoginData =
        rvUsername.View.Map2(rvPassword.View, (username, password) =>
            new LoginData { Username = username, Password = password });
    var sLoginData = Submitter.Create(vLoginData, null);
    var vSubmittedLoginData = sLoginData.View;
    ```

    In the above example, `vSubmittedLoginData` is initialized with `null`, and is updated with the current login data whenever `sLoginData.Trigger()` is called. It is then possible to map a `View` or a `Sink` on `vSubmittedLoginData` that performs the actual login.

    * **`View<IEnumerable<U>> MapSeqCached(this View<IEnumerable<T>> v, Func<T, U> f)`** maps views on sequences with "shallow" memoization. The process remembers inputs from the previous step, and reuses outputs from the previous step when possible instead of calling the converter function. Memory use is proportional to the longest sequence taken by the View. Since only one step of history is retained, there is no memory leak. Requires equality on `T`.

    * **`View<IEnumerable<U>> MapSeqCached(this View<IEnumerable<T>> v, Func<T, K> key, Func<T, U> f)`** is a variant on `MapSeqCached` above that uses a `key` function to determine identity on inputs, rather than an equality constraint on the type `T` itself.

    * **`View<IEnumerable<U>> MapSeqCached(this View<IEnumerable<T>> v, Func<View<T>, U> f)`** is an extended form of `MapSeqCached` where the conversion function accepts a reactive view. At every step, changes to inputs identified as being the same object are propagated via that view. Requires equality on `T`.

    * **`View<IEnumerable<U>> MapSeqCached(this View<IEnumerable<T>> v, Func<T, K> key, Func<View<T>, U> f)`** is a variant on `MapSeqCached` above that uses a `key` function to determine identity on inputs, rather than an equality constraint on the type `T` itself.

### Sinks

Once a graph is built out of `Var`s and `View`s, it needs to be run to react to changes.

The function **`void Sink(this View<T> v, Action<T> f)`** is the output node of the dataflow graph. This function calls `f` with the current value of `v` whenever it is updated. It is highly recommended to have a single `Sink` running per dataflow graph; memory leaks may happen if the application repeatedly spawns `Sink` processes that never get collected. See [Leaks](https://github.com/intellifactory/websharper.ui.next/blob/master/docs/Leaks.md) for more information.

```csharp
public class LoginData
{
    public string Username { get; set; }
    public string Password { get; set; }
}

var rvUsername = Var.Create("");
var rvPassword = Var.Create("");
var vLoginDataInput =
    rvUsername.View.Map2(rvPassword.View, (username, password) =>
        new LoginData { Username = username, Password = password });
var sLoginData = Submitter.Create(vLoginData, null);
var vSubmittedLoginData = sLoginData.View;
vSubmitted.Sink(async login =>
{
    await Rpc.Login(login); // An [Rpc] function that logs in the user
})
```

It is relatively rare to call `View.Sink` directly. Instead, views are generally connected to the [DOM layer](#dom), which itself calls `Sink` when inserted into the document.

<a name="dom"></a>
## The DOM layer

In UI.Next, the type `Doc` represents a DOM snippet, i.e. a sequence of HTML or SVG elements, with possibly reactive content. A `Doc` can be empty, a single element or several elements. The `Doc` API is mostly generative: it is not advised to imperatively insert nodes or change their contents. Instead, dynamic nodes are generated based on a dataflow graph.

Most of the following functions are located in the namespace `WebSharper.UI.Next`. Convenience functions such as individual HTML elements and attributes require `using static WebSharper.UI.Next.CSharp.Client.Html;`. Dynamic functions that involve `Var`s, `View`s or `Dom.Element`s are under `WebSharper.UI.Next.Client`.

### Creating Docs

The main way to create `Doc`s is to use the static methods from `WebSharper.UI.Next.CSharp.Client.Html` named after HTML elements. These functions take any number of arguments which will be inserted as attributes and child elements on the currently created element. The arguments can have the following types:

* **`string`**: this will insert a text node.

* **`Doc`**: this will insert a reactive node.

* **`Attr`**: this will insert an attribute.

* **`View<T>`** or **`Var<T>`**: this will insert a node, whose value will be live-updated from the given View or Var. When that value is a `string`, `Doc`, `View<T>` or `Var<T>`, it gets inserted accordingly, for all other types (including an `Attr`), its `.ToString()` method called and inserted as a text node.

* any other object will have its `.ToString()` method called and inserted as a text node.

You can find SVG elements as static members of `WebSharper.UI.Next.CSharp.Client.Html.SvgElement`.

There are more strongly typed `Doc` construction options which do not use client-side type checking, but requires a longer syntax.
These are either members of `WebSharper.UI.Next.Doc` for unvarying Docs that ate usable on both client and server side, and `WebSharper.UI.Next.Client.Doc` for reactive Docs.
The following static methods in the `Doc` class create Docs:

* **`text(string text)`** creates a `Doc` composed of a single text node with the given contents.
* **`text(View<string> v)`** creates a `Doc` composed of a single text node whose contents is always equal to the value of the given `View`.
* **`Doc.Element(string tag, IEnumerable<Attr> attrs, IEnumerable<Doc> children)`** creates a `Doc` composed of a single HTML element with the given tag name, attributes and children.
* **`Doc.SvgElement(string tag, IEnumerable<Attr> attrs, IEnumerable<Doc> children)`** creates a `Doc` composed of a single SVG element with the given tag name, attributes and children.
* **`doc(Dom.Element el)`** creates a `Doc` composed of a single element.
* **`Doc.Empty`** creates an empty `Doc`, ie. a `Doc` composed of zero elements.
* **`doc(Doc doc1, Doc doc2)`** creates a `Doc` composed of two `Doc`s in sequence.
* **`Doc.Concat(IEnumerable<Doc> docs)`** creates a `Doc` composed of several `Doc`s in a sequence.
* **`doc(View<Doc> view)`** allows the actual DOM structure of a `Doc` to depend on the dataflow graph.

### Creating input elements

The following static methods of `WebSharper.UI.Next.CSharp.Client.Html` create elements which can be used to set the value of a `Var` based on user input.

* **`input(IRef<string> var)** creates an `<input>` text box synchronized with the given `IRef`. The `Var` is updated on user input, and the text box is updated when the `Var` changes. You can also specify any number of attributes as a `params` argument. Overloads exist for `Var<int>` and `Var<double>` for creating number imputs. One overload does not take an `IRef`, this is not a auto-synchronizing input.

    ```csharp
    var rvText = Var.Create("initial value");
    var myDoc = input(rvText);
    // myDoc HTML equivalent is now: <input type="text" value="initial value" />
    // User types "!"...
    // myDoc HTML equivalent is now: <input type="text" value="initial value!" />
    rvText.Value = "new value";
    // myDoc HTML equivalent is now: <input type="text" value="new value" />
    ```

* **`textarea(IRef<string> var)`** and its overloads are similar, but creates a `<textarea>`.

* **`passwordBox(IRef<string> var)`** is similar, but creates an `<input type="password">`.

* **`button(string caption, Action callback, params Attr[] attrs)`** creates a `<button>` that calls the given callback on click. If you have a `Submitter<T>`, then its `.Trigger` method is a suitable callback to make a button click capture a current state that you want the button click handler to operate with.

    Here is the login box sample from `SnapshotOn` above, with a document to render it:

    ```csharp
    var rvUsername = Var.Create("");
    var rvPassword = Var.Create("");
    var vLoginData =
        rvUsername.View.Map2(rvPassword.View, (username, password) =>
            new LoginData { Username = username, Password = password });
    var sLoginData = Submitter.Create(vLoginData, null);
    var vLoginResult = sLoginData.View.MapAsync(async login => await Rpc.Login(login));
    div(
        input(rvUsername),
        passwordBox(rvPassword),
        button("Log in", sLoginData.Trigger),
        vLoginResult.Map(res => Doc.Empty)
    );
    ```

* **`link(string caption, Action callback, params Attr[] attrs)`** is similar to `Button`, but creates an `<a>` link instead.

* **`checkbox(IRef<bool> var, params Attr[] attrs)`** creates a checkbox whose checked state reflects the value of the given boolean `Var`.

* **`checkbox(T value, IRef<IEnumerable<T>> var, params Attr[] attrs)`** creates a checkbox whose checked state reflects the presence or absence of the given value in the given enumerable `Var`. For example, given `checkbox("test",rvMyVar)`, ticking the checkbox will add `"test"` to the list held by `rvMyVar` and unticking the checkbox will remove it. No ordering of values in the list is guaranteed.

* **`select(IRef<T> var, IEnumerable<T> options, Func<T, string> text, params Attr[] attrs)`** creates a selection box from the given enumerable. It requires a function to get the text for each item, and a variable which is updated with the currently-selected item. A dynamic version is available, taking a `View<IEnumerable<T>>` for `options` argument. Overloads exist that also take a placeholder text. 

* **`radio(IRef<T> var, T value, params Attr[] attrs)`** creates a radio button whose checked state reflects whether the given `Var`'s value is currently equal to the given value.

### Inserting Docs in a document

There are several ways to insert a `Doc` in a document by extension methods available with `WebSharper.UI.Next.Client`:

* **`.Run(Dom.Element el)`** extension method renders the `Doc` as the children of the given DOM element.

* **`.RunById(string id)`** is similar to `Doc.Run`, but takes an element identifier to locate the container element.

* The type `Doc` implements the interface **`WebSharper.Html.Client.IControlBody`**; it is therefore possible to use it as the body of a `Web.Control` or to pass it to the function `ClientSide` from `WebSharper.Html.Server`.

    ```csharp
    using WebSharper;
    using WebSharper.UI.Next;
    using WebSharper.UI.Next.Html;

    public class MyControl : Web.Control
    {
        public override IControlBody Body
        {
            get
            {
                var rvText = Var.Create("");
                return doc(
                    input(rvText),
                    label(rvText)
                );
            }
        }
    }
    ```
    
### Attributes

Just like `Doc`, the `Attr` type is monoidal: it represents zero, one or many DOM attributes. Client-side only (reactive) static methods presented here assumes having `using CAttr = WebSharper.UI.Next.Client.Attr;`. DOM attributes can be created with the following functions:

* **`attrib(string name, string value)`** represents a single attribute with the given `name` and `value`.

    * For convenience, standard attributes such as `href` are available as **`attr.href`** when having `using static WebSharper.UI.Next.CSharp.Client.Html`.  A few of them need to be prepended with a `@` character to avoid collisions with keywords such as `class` or `default`.

* **`attrib(string name, View<string> value)`** represents a single attribute with the given `name` and varying `value`.

    * For convenience, when `using WebSharper.UI.Next.CSharp.Html` and `using WebSharper.UI.Next.CSharp.Client`, standard attributes such as `href` are available as **`attr.hrefDyn`**.

* **`handler(string name, Action<Dom.Element,Dom.Event> callback)`** specifies a handler for a DOM event, such as click event for a button. The `name` of the event doesn't include the `on` prefix, for example: `handler("click", (el, ev) => { })`.

    * For convenience, when `using WebSharper.UI.Next.CSharp.Html` and `using WebSharper.UI.Next.CSharp.Client.Html`, standard event handlers such as `onclick` are available as **`on.click`**.

* **`handler(string name, View<'T> view, Action<Dom.Element,Dom.Event,T> callback)`** specifies a handler for a DOM event, and additionally the handler receives the current value of the given view. The `name` of the event doesn't include the `on` prefix, for example: `Attr.Handler "click" callback`.

    * Standard event handlers are also available with this method signature on the `WebSharper.UI.Next.CSharp.Client.Html.on` static class.

* **`attrib(params Attr[] attrs)`** concatenates multiple collections of attributes into one.

* **`Attr.Empty`** is the empty collection of attributes.

* **`class(string name)`** specifies a class attribute. Classes are additive, so:

    ```csharp
    attrib(class("a"), class("b")) == attrib("class", "a b")
    ```

* **`class(string name, View<T> view, Predicate<T> pred)`** specifies a class that is added when `pred` is true and removed when `pred` is false.

* **`style(string name, string value)`** specifies a CSS style property, such as `style("background-color", "black")`.

* **`style(string name, View<string> value)`** specifies a varying CSS style property.

* **`attrib(string name, View<string> view, Predicate<T> pred)`** specifies an attribute with the given `name` and varying `value` when `pred` is true, and unsets it when `pred` is false.

<a name="animation"></a>
## Animation

UI.Next allows you to create animations declaratively, which are then run when a `View` changes. There are three main types involved in animation:

* **`Anim<'T>`** is an animation for a type `'T`. It represents a function from time to `'T`.

* **`Trans<'T>`** defines a transition for a type `'T`. It indicates the `Anim<'T>` that should be played on enter, change and exit.

* And finally `Attr`, via the functions **`Attr.Animated`** and **`Attr.AnimatedStyle`**, attaches a transition to a given attribute or style of an element. For convenience, when having `using static WebSharper.UI.Next.CSharp.Client.Html`, standard attributes has overloads that take a `Converter` delegate to define an animation on the attribute.

Here is an example for an element that enters from the left and leaves to the right using a cubic animation, and otherwise moves linearly according to `rvLeftPos`:

```csharp
// define animations that can be parametrized by start and end times
Func<double, double, Anim<double>> linearAnim = 
    (start, end) => Anim.Simple(Interpolation.Double, new Easing(x => x), 300, start, end);
Func<double, double, Anim<double>> cubicAnim =
    (start, end) => Anim.Simple(Interpolation.Double, Easing.CubicInOut, 300, start, end);
// define the transition with a cubic in and out and linear in between
var swipeTransition =
    new Trans<double>(linearAnim, x => cubicAnim(x - 100, x), x => cubicAnim(x, x + 100));

var rvLeftPos = Var.Create<double>(0);
var animatedDoc =
    div(
        style("position", "relative"),
        style("left", swipeTransition, rvLeftPos.View, pos => pos + "%"),
        "content"
    );
```
