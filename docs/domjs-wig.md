Tutorial by Istvan Gansperger

## Introduction

When writing client-side applications it is indispensable to interface with existing JavaScript libraries.
JavaScript being dynamically typed, it is, however, often not straightforward for F# consumption.
It is close to impossible – and usually not worth the hassle – to generate usable typed interfaces for an untyped language.
For this very reason one may have to manually or semi-automatically transcribe those APIs into typed F#, annotating with
types. This is what the WebSharper Interface Generator (WIG) can help you with. WIG has an F# embedded domain specific language
that makes it easy to express complex APIs in a straightforward manner. By transliterating your JavaScript APIs into F# with WIG, either
by hand, based on your library's documentation and samples, or by clever WIG code generation tactics, based on some other source
(like an XML file that contains all the type information), you can obtain a WebSharper-friendly extension that
enables you to address your JavaScript library in F#.


## Scope of the Tutorial

In this tutorial we will be focusing on the workflow of writing extensions in WIG and not so much on all the features it offers. I will throughly explain the code I'm writing and every piece I'm using from the API, but for a complete list of features you should consult the [documentation of WIG](http://websharper.com/docs/wig).

I will be using the [Dom.js library](https://github.com/dkraczkowski/dom.js) as the API we want to generate an interface for. It is not a huge library but a very practical one to show off the most common patterns you will be using in WIG.


## Getting your Project set up

For this tutorial I will use Visual Studio but the process is very similar in [CloudSharper](http://cloudsharper.com) or [MonoDevelop](http://monodevelop.com/). If you have [WebSharper](http://websharper.com) installed you can just go ahead and make a new `Extension` project (you find WebSharper templates under `Templates > Visual F# > WebSharper`). If you do not yet have the extension installed, you can just grab it from our [Download page](http://websharper.com/downloads).

When you open the project you are presented with the default template that showcases the syntax and usage of WIG. It also has some boilerplate code which is necessary for generating the interfaces. For the time being, ignore `type Extension` on the bottom of the file. It's just some boilerplate code whihc is necessary for the compilation.
This is what you should be seeing at this point:

```fsharp
namespace WIGTutorial

open WebSharper
open WebSharper.JavaScript
open WebSharper.InterfaceGenerator

module Definition =

    let I1 =
        Interface "I1"
        |+> [
                "test1" => T<string> ^-> T<string>
                "radius1" =@ T<float>
            ]

    let I2 =
        Generic - fun t1 t2 ->
            Interface "I2"
            |+> [
                    Generic - fun m1 -> "foo" => m1 * t1 ^-> t2
                ]

    let C1 =
        Class "C1"
        |+> Instance [
                "foo" =@ T<int>
            ]
        |+> Static [
                Constructor (T<unit> + T<int>)
                "mem"   => (T<unit> + T<int> ^-> T<unit>)
                "test2" => (TSelf -* T<int> ^-> T<unit>) * T<string> ^-> T<string>
                "radius2" =? T<float>
                |> WithSourceName "R2"
            ]

    let Assembly =
        Assembly [
            Namespace "Extension1" [
                 I1
                 I2
                 C1
            ]
        ]

[<Sealed>]
type Extension() =
    interface IExtension with
        member ext.Assembly =
            Definition.Assembly

[<assembly: Extension(typeof<Extension>)>]
do ()

```

This is probably a bit scary at first, but we really don't need much of this. First you should just remove `I1` , `I2` and `C1`. I will be explaining the syntax in detail as we move on, so there is no need to try to proccess all that information just now.
The code should look like this:

```fsharp
namespace WIGTutorial

open WebSharper
open WebSharper.JavaScript
open WebSharper.InterfaceGenerator

module Definition =

    let Assembly =
        Assembly [
            Namespace "Extension1" [
            ]
        ]

[<Sealed>]
type Extension() =
    interface IExtension with
        member ext.Assembly =
            Definition.Assembly

[<assembly: Extension(typeof<Extension>)>]
do ()
```

From this point on, I will just focus on the `Definition` module because that's all we need to modify. The most essential part of this module is the `Assembly` let binding. This describes the namespaces and classes that will be included in the produced dll. Since we are binding `DOM.js` I will go with the name `WebSharper.DomJs` for the namespace. That's how we usually name the extensions we provide for WebSharper; obviously you do not have to follow this.

```fsharp
module Definition =

    let Assembly =
        Assembly [
            Namespace "WebSharper.DomJs" []
        ]
```


## Adding the Resource

You have basically two options for adding JavaScript resources. One is to use a CDN and the other is to include the source of the library in the generated dll. The CDN one is nice because you will be able to update the used version by just modifying one line of code (if there is no change in the API, of course) and you won't have to include the file with all your projects. The other way is to include it as an embedded resource, meaning you will ship the resource with your code. It is up to you which one you opt for. For example if you are writing a library that you plan to use in an offline mobile application it is not very practical to use a CDN, since you don't assume internet connection on the device. If you cannot redistribute the JavaScript code because of license issues, but a CDN is offered, you need to go for the CDN solution obviously.

Let's take a look at the CDN one first. (DOM.js does not seem to have a CDN so we will just use the GitHub link.)

```fsharp
module Res =
    let DomJs = 
        (Resource "DomJs" "https://raw.githubusercontent.com/dkraczkowski/dom.js/master/src/dom.min.js").AssemblyWide()
```

And let's put it in the `Assembly`.

```fsharp
module Definition =
        
    let Assembly =
        Assembly [
            Namespace "WebSharper.DomJs.Resources" [
                Res.DomJs
            ]
            Namespace "WebSharper.DomJs" [
                 
            ]
        ]
```
`res.AssemblyWide()` here specifies that the the resource is to be included if anything is used from the API. In this case this is enough, but there are some very important concepts when dealing with resources that I haven't mentioned here. For a complete guide, visit the [resources documentation](http://websharper.com/docs/resources).

Now, we will include the JavaScript file as an embedded resource. For this only the `Res` module needs to be changed a bit, and you need to add the JS files to the project.

```fsharp
module Res =
    let DomJs = 
        Resource "DomJs" "dom.min.js" |> AssemblyWide
```

This means that WebSharper will try to find the resource named `dom.min.js`, and if it finds the file, it includes it and if it doesn't it, will just insert the path as is into the `<head>` tag of the html file (or in case of a Single Page Application or Bundle, it will insert the code into the generated JS file).
Now to add the resource you should grab the [JS file](https://raw.githubusercontent.com/dkraczkowski/dom.js/master/src/dom.min.js) from the [github repo](https://github.com/dkraczkowski/dom.js) and add it to the solution by right-clicking on the project in the Solution Explorer and choosing `Add > Existing item`. You can then click on the file and in the `Properties` view, set `Build Action` to `EmbeddedResource`. 

That's all for preparing the project. Now we can get to work.


## Writing the API Binding

We are going to be working off the [API reference on GitHub](https://github.com/dkraczkowski/dom.js#api-reference), since there does not seem to be anything else we could use to generate the code. This part is not particularly exciting, you just have to look at the API and transform it to WIG. It may not be straightforward at first, but once you have done it a couple of times, it becomes second nature.

We are going to be doing it method by method from the top. Here's the code for the first couple of functions:

```fsharp
module Definition =      
    let O = T<unit>
    let S = T<string>  
    let E = T<WebSharper.JavaScript.Dom.Element>

    let Offset = Class "Offset"
    let Dom = Class "Dom"

    Offset
    |+> Instance [
        "top" =? T<int>
        "right" =? T<int>
        "bottom" =? T<int>
        "left" =? T<int>
        "width" =? T<int>
        "height" =? T<int>
    ] |> ignore

    Dom
    |+> Static [
        "find" => S?selector ^-> !| E
        |> WithComment "Finds HTMLElements that match css pattern."

        "id" => S?id ^-> E
        |> WithComment "Returns HTMLElement with given id."

        "findByTagName" => S?tagName ^-> !| E
        |> WithComment "Finds HTMLElements that match given tag name."

        "findByClass" => S?className ^-> !| E
        |> WithComment "Finds HTMLElements that match given class name."

        "parent" => E?element ^-> E
        |> WithComment "Gets the parent of the html element."

        "children" => E?element * S?tag ^-> !| E
        |> WithComment "Gets children elements of the html element."

        "next" => E?element ^-> E
        |> WithComment "Gets following sibling element of the HTMLElement."

        "previous" => E?element ^-> E
        |> WithComment "Gets previous sibling element of the HTMLElement."

        "offset" => E?element ^-> Offset
        |> WithComment "Returns current coordinates of the element, relative to the document."
    ] |> ignore

    let Assembly =
        Assembly [
            Namespace "WebSharper.DomJs.Resources" [
                Res.DomJs
            ]
            Namespace "WebSharper.DomJs" [
                Offset
                Dom
            ]
        ]
```

So there are quite a few new things here. This is the DSL of WIG, the embedded language it uses to describe interfaces.

Let's focus on the first three lines in `Definition`:

```fsharp
let O = T<unit>
let S = T<string>  
let E = T<WebSharper.JavaScript.Dom.Element>
```

The first thing of importance is `T<'a>`. `T` is basically a value that holds a desired type for us. For example if we want something to be of type string we can't just write `string` because that would be an actual type in F#. We need to wrap our types coming from the outside in `T`. We will be mostly using `Dom.Element` from `WebSharper`, which is the type of a simple DOM node, so we create a shorthand for it. Strings and units will be rather frequent as well, so we declare a shorthand for those two too.
The next step is to create an empty definition :

```fsharp
let Offset = Class "Offset"
let Dom = Class "Dom"
```
This is not necessary but is a good practice. In F# you can only use variables you declared prior to the place of use, so we would be in some trouble if we needed mutual references. Also you might need to self-references the type of the object you are describing, which would not be possible either, without using these empty types.

You can define a class with the `Class` function. The parameter is the generated name of the class in the assembly. The name of the let binding will not affect the generated code; we will use that to tell `Assembly` at the end what to include in the assembly.

Now we can go onto actually getting some work done. Let's take a look at the `DomClass` first:

```fsharp
Dom
|+> Static [
    "find" => S?selector ^-> !| E
    |> WithComment "Finds HTMLElements that match css pattern."

    "id" => S?id ^-> E
    |> WithComment "Returns HTMLElement with given id."

    "findByTagName" => S?tagName ^-> !| E
    |> WithComment "Finds HTMLElements that match given tag name."

    "findByClass" => S?className ^-> !| E
    |> WithComment "Finds HTMLElements that match given class name."

    "parent" => E?element ^-> E
    |> WithComment "Gets the parent of the html element."

    "children" => E?element * S?tag ^-> !| E
    |> WithComment "Gets children elements of the html element."

    "next" => E?element ^-> E
    |> WithComment "Gets following sibling element of the HTMLElement."

    "previous" => E?element ^-> E
    |> WithComment "Gets previous sibling element of the HTMLElement."

    "offset" => E?element ^-> Offset
    |> WithComment "Returns current coordinates of the element, relative to the document."
] |> ignore
```

The JS library has a `Dom` object which has a couple of static methods. We can add these with the `|+>` operator to our class, using the `Static` helper on a list of method definitions.

There are a couple of operators depending on what kind of member you want to describe. We only have functions here, so we will use the `=>` operator. This needs a name (the name of the function) and the type of the function. Again we cannot use standard F# types here so we need some more operators for describing the semantics in .NET. With functions the most important one is `^->` which is the same as `->` in F# only it operates on on our `Type` instead of real types. WIG does not support curried functions (you can express them but they will not compile to the JavaScript call you would expect), so it is highly advised to use tupled arguments. You can achive this with he familiar `*` operator as you can see in the binding of the `children` function. Another interesting thing to note here is the `?` in the argument list. This is just an operator all the same, which says that the parameter being specified should have the name standing after it in the generated assembly. For example `"find" ^-> S?selector ^-> !| E` will generate a function equivalent to `let find (selector : string): Dom.Element[]` in F# (the `!|` operator creates a one-dimensional array type). For  One more thing here is the `WithComment` function. I don't think this one needs much explaining, it just appends a comment that will show up in IntelliSense when usig the generated code.

Also note that the `|+>` operator added members to the class definition desctructively but is also returning the value so you can define class properties and member lists one in a chained way. So in the end we ignore the result. (General rule is: modifying type definitions are mutable operations while modifying member declarations are always immutable.) Another way would be to have this member additions inside the `Namespace "WebSharper.DomJs" [ ... ]` definition.

The `offset` function does not return a primitive or an existing type, isntead it returns an `Offset` object. Objects in JavaScript can be expressed with classes in .NET, so we will create an `Offset` class:

```fsharp
Offset
|+> Instance [
    "top" =? T<int>
    "right" =? T<int>
    "bottom" =? T<int>
    "left" =? T<int>
    "width" =? T<int>
    "height" =? T<int>
] |> ignore
```
Usually these kinds of objects do not need a constructor as we will not ahve to create them ourselves, we just receive them from a funtion. This class only needs some members as it's a plain JavaScript dictionary. Here `Protocol` means that the following list will contain instance members and not static ones. The `=?` oeprator means that the given property should be read-only (only getter is generated). Other such operators are `=!` which specifies a write-only proeprty and `=@` which creates one with both setter and getter. We need these properties to be read-only so we go with `=?` here.

An interesting thing can come up here: we know that `parent`, `next` and `previous` functions may return null. We don't really like dealing with nulls in F# (or in any language in particular) so we can do a neat trick here that would get us an `option` which would be `None` if we have a null and `Some x` if we have a non null value. For this we need to know how WebSharper treats F#'s optional values.
 
WebSharper compiles `option` to a specific JavaScript object. Fore example `Some 3` becomes `{$: 1, $0: 3}` in the generated JS code, and `None` becomes `{$: 0}`. Knowing that we only need a wrapper in JavaScript around those three functions. We can use the `WithInterop` WIG function for this purpose. This can create a type value that automatically applies transformations in the generated JavaScript inline. And this is how we use it:

```fsharp
let EOpt =
    !? E |> WithInterop {
        In = fun o -> o + ".$0"
        Out = fun e -> "(e = " + e + ", e?{$: 1, $0: e}:{$: 0})"
    }
```

Now you will be able to use `EOpt` instead of `E` when you want the method result returned to WebSharper code as an option value as the `!?` operator creates an otion type.

Most of the remaining functions are analogous to the previous ones so I will not go into detail about those. I will, instead, show some of the more interesitng ones. Let's take a look at `Dom.requestAnimationFrame`:

```fsharp
let Handle = Class "Handle"

//...
Dom 
|+> Static [

        //...

        "requestAnimationFrame" => (O ^-> O)?callback ^-> Handle
        "cancelAnimationFrame" => Handle?handle ^-> O
```

Here `Handle` is just an empty container that will carry the reference of the registered callback which can later be cancelled. Having an empty type is convinient because these objects can only come from `requestAnimationFrame` and can only be passed to `cancelAnimationFrame`. We do not care what their internals are, they are just tokens to be used with those two functions. `requestAnimatonFrame` expects a callback as its first argument. Describing this is evry easy as arguments of functions can be otehr functions. `(O ^-> O)` here means that the first argument is a function from `unit` to `unit`.

Let's check out the `Dom.draggable` function:

```fsharp
let AxesEnum = 
    Pattern.EnumStrings "axes" [ "x"; "y" ]

let DraggableCfg =
    Class "DraggableCfg"
    |+> Pattern.OptionalFields [
        "axis", AxesEnum.Type
        "grid", !| T<int>
        "handler", E
        "onDragStart", T<WebSharper.Dom.MouseEvent> ^-> O
        "onDragMove", T<WebSharper.Dom.MouseEvent> ^-> O
        "onDragEnd", T<WebSharper.Dom.MouseEvent> ^-> O
        "constrain", E
    ]

//...
Dom 
|+> Static [

    //...

    "draggable" => E?element * !?DraggableCfg?options ^-> O
```
There are quite a number of new things in this snippet. First there are those `Pattern` stuff that need some explaining. `Pattern.EnumString` is a handy way to express JS enums which are plain strings most of the time with a finite number of possible values. You just give the enum a name and specify the possible values, and WIG will generate the right class. `Pattern.OptionalFields` is fairly simple as well. We often use config objects in JavaScript to pass to constructors and functions, which are basically just dictionaries with the configuration we want. 

The last new thing here is the `!?` operator that specifies that a function argument is optional.

One more convininent WIG feature is to concisely define overloads for functions.

```fsharp
"onClick" => (E + !| E)?element ^-> (T<WebSharper.Dom.MouseEvent> ^-> O)?listener ^-> O
```

Note the `+` sign between `E` and `!| E`: this tells WIG that `element` can be of either type, so it can generate the appropriate overloads for the function.

And don't forget to add these new classes and patterns to the assembly:

```fsharp
let Assembly =
    Assembly [
        Namespace "WebSharper.DomJs.Resources" [
            Res.DomJs
        ]
        Namespace "WebSharper.DomJs" [
            Offset
            Dom
            Handle
            AxesEnum
            DraggableCfg
        ]
    ]
```        

## Using your Binding

All that's left now is including your bidning in a WebSharper project to be able to use it. Obviously these are JavaScript libraries so they will only work on the client side. Using it is very simple: just hit `Ctrl+Shift+B` or (whatever invokes MSBuild). This generates a dll file in the output directory of your project. All you have to do is include this as a reference in another application or, if you will, make a NuGet package out of it.


## Afterword

That covers the basics of binding JavaScript libraries. At this point you should feel confident enough to start doing your own interface for an API, and with the the help of the [WIG documentation](http://websharper.com/docs/wig) it should be pretty simple. In this tutorial you learned how interface bindings are structured, how to address resources, how to embed them or use CDNs and how to describe the interface itself. WIG is a very small – but a rather important – part of WebSharper. Without JavaScript libraries a project that compiles F# code to JavaScript would be just a toy, it would not be capable of any serious development. WebSharper already provides bindings for a handful of popular JavaScript libraries, and now you can also make your own if you find that the existing ones do not cover your needs. As I said the tutorial didn't cover everything you need to know about WIG, in fact I couldn't mention a lot of important things like interfaces, generics and constructors, but they should be intuitive enough to be learned from the [WIG documentation](http://websharper.com/docs/wig). Once again the goal of this tutorial wasn't to teach you everything but to give you an idea on how to get started and not get lost.