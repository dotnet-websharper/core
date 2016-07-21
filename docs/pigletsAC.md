## Suggestion Box Piglet

Often enough we desire to provide the user with an input box where he
can type a keyword and provide him with suggestions based on the
keyword from which he can choose an option.

Implementing such functionality with Piglets is easy and there are
several approaches to achieve it. Nevertheless, the main objective is
sticking to the Piglet philosophy of staying UI agnostic and not to
repeat yourself.

The following post explains a simple approach to achieve this
functionality with the Piglet philosophy. Although the ideas are
simple, the type signatures of Piglets can be scary at first so
walking through a tutorial can be a quick way to understand the core
concepts behind Piglets.

### A Datatype for a Suggestion Box

The first step is to define the datatype that the Piglet will be
using. To keep things simple, lets assume the user can choose from a
specific list of string say "Church, Chompsky, Curry, Turing,
Trovalds". Normally we would expect the Piglet to have type
Piglet<string,v> but in this case we must add some extra streams to the
Piglet to produce the suggestion list, so the Piglet needs a stream
with an array of possible alternatives and an additional string which
will contain the final result once the user picks one of the
options. We define the following type:

    type InputWithSuggestions =
        {
            //Field to collect user input
            query       : string
            //Field to provide suggestions    
            suggestions : string []
            //Field to collect the option selected by the user
            result      : string
        }

The corresponding Piglet definition is very simple:

    let InputWithSuggestionsPiglet init =
        (fun query suggestions result ->
            {
                query        = query
                suggestions  = suggestions
                result       = result
            })
        <*> Piglet.Yield init.query
        <*> Piglet.Yield init.suggestions
        <*> Piglet.Yield init.result

### Search Box View

For this example, the
[Sencha Touch](http://www.sencha.com/products/touch) bindings for
WebSharper will be used to render the output. Piglets are UI agnostic
and can be rendered by many different approaches achieving similar
levels of interactivity and retaining the business logic. The
following code defines our rendering function:

    let SuggestionsBox query suggestions result =
        let elements : Ext.Component [] =
            [|
                Ext.field.Text() |> Reactive.Text.WithText query;
                Ext.field.Select()
                |> SenchaTouch.Reactive.Select.WithSelect(As,As,result)
                |> SenchaTouch.Reactive.Select.WithOptions
                    (Array.map (fun l -> New[("text",As l);("value",As l)]),suggestions);
                ExtCfg.Label(Html="").Create()
                |> Reactive.Label.WithLabelGen ((fun o -> "You selected: "+o.ToString()),result |> As<Reader<obj>>)
            |]
        ExtCfg.Container(Items=elements,Layout="hbox").Create()

What happened here is the following. The Piglet will provide us with
three steams, namely query, suggestions and result. The types are:

    val query : Stream<string>

    val suggestions : Stream<string []>

    val result : Stream<string>

Note that they correspond to the members of the `InputWithSuggestions`
type but they are a stream instead of just a plain type. Now the
function `Reactive.Text.WithText` takes a `Stream<string>` and a text
field and returns a text field that is updated every time the stream
produces a new value. The new text field also writes its value to the
stream every time it is changed.

The function `Reactive.Select.WithSelect` takes a `Stream<obj>` The `As` function is
also provided two times. The role of the function is to convert the
value from the stream into an obj type and back. Since a string is
also of type obj, As can simply be provided to cast the string up and
down to and from obj.

The function `Reactive.Select.WithOptions` behaves a bit different. It
takes as an argument a `Reader<obj>`, a conversion function
`(obj->obj[])` and a Select field. This function awaits the Reader to
produce a new value and applies the conversion function to the value
and uses the result to update the option list. The select boxes in
Sencha Touch expect a JSON object with the structure
`{text:text,value:value}` to populate the select boxes. In this case
we are providing the function with a `Stream<string []>` so the only
necessary conversion is converting the strings into the appropriate JSON.

Note that the select field is connected to two streams, namely the
suggestions and the result stream. This is perfectly acceptable and
the reason it is so is that the suggestions stream is responsible for
updating the option list shown on the field and the result stream is
responsible for collection the final choice of the user.

### Higher Order Signal Functions

Currently all visual elements are in place as well as most of the
logic, but if anything is typed in the input box, no suggestions will
be provided. This is due to the fact that the suggestions stream is
never provided with data.

We require a mechanism to collect the data from the input, produce
suggestions and feed that data into the stream. Anyone familiar with
traditional Functional Reactive Programming (FRP) frameworks is aware
of higher order functions such as `map` and `fold` for signals which
might seem to be useful in this case.

Unfortunately, Piglets currently do not provide such functions but it
is easy to define them. In our toy example, suggestions are embedded in
the page but in most cases they will be generated asynchronously in
some server backend. Therefore we define `MapAsync` as follows:

    let MapAsync  (f : 'a->Async<'b>) (r : Reader<'a>) (w :Writer<'b>) : unit =
        r.Subscribe
            (function 
                | Success x -> 
                    async{
                        let! res = f x
                        res |> Success |> w.Trigger
                        return ()
                    } |> Async.Start
                | Failure msgs -> Failure msgs |> w.Trigger)
        |> ignore

We introduce the usage of two functions `Reader.Subscribe` and
`Writer.Trigger`. The `Reader.Subscribe` function will register a
callback that will be executed every time the reader has a new
value. The `Writer.Trigger` function will update the current value of
the stream. This function, whenever the reader produces a new value,
it applies the asynchronous function which when finished triggers its
result into the writer.

###Connecting Everything

We can now introduce a function to produce suggestions
asynchronously. This is easily achieved using the Async implementation
from WebSharper and the following code simulates a delayed server query:

    let Options = 
        [|
            "Church"
            "Chompsky"
            "Turing"
            "Torvalds"
            "Curry"
        |]

    let Suggest input =
        let f (cb : string [] -> unit) =
            Options 
            |> Array.filter (fun e -> e.Contains input)
            |> cb
        async{
            let! res = Async.FromContinuations (fun (ok,_,_) ->
                JavaScript.SetTimeout (fun () -> f ok) 2000 |> ignore)
            return res
        }

Now we can use the MapAsync function defined earlier to produce
suggestions and provide them to the select box by calling the
function inside the `SuggestionsBox` function as follows:

    MapAsync Suggest query suggestions

It is irrelevant where the call is performed since this function
operates over streams and once the elements are rendered they will
continue updating themselves as new values arrive from the streams. 

###An Html View

Using the WebSharper HTML markup we can easily render the Piglet as
well without having to modify the existing code. Following is the
view:

    let RadioSelector (options : Reader<string []>) (output : Stream<string>) =
        let child = Controls.Radio output [] |> ref
        let sel = Div [!child]
        options.Subscribe(
            function 
                | Failure _ -> ()
                | Success a -> 
                    child.Value.Remove ()
                    child := a |> Array.map (fun e -> (e,e)) |> Controls.Radio result
                    !child |> sel.Append |> ignore
        ) |> ignore
        sel

    let SuggestionBoxHtml query (suggestions : Stream<string []>) result =
        MapAsync Suggest query suggestions
        Div
            [
                Controls.Input query
                RadioSelector suggestions result
                Span [] |> Controls.ShowString result (fun r -> "You selected: "+r)
            ]

Piglet currently doesn't offer a mechanism to automatically generate
suggestions out of a stream since that is not the objective of the
Piglet framework but we can easily define one as done with the
`RadioSelector` function.


###Summary

We learned how to define a simple Piglet with some additional helper
fields to enhance the interactive behavior of the application. While
doing so we remained completely UI agnostic since all the
control/logic code is either inside the Piglet or implemented as a
function over streams. Both of these constructs are designed to work
with any UI. During the course of this example we explored some
low-level stream functions such as Trigger and Subscribe which allow
the developer to further customize a Piglet.

Although the code might be be considerably larger than automatically
rendering a similar element using another framework. Piglets offer
unmatched flexibility and safety. After writing the Piglet once,
additional views can be defined with minimal effort. A tradeoff must
be made to offer this flexibility but for a system with multiple UIs,
Piglets will reduce code repetition avoiding errors and increasing modularity.

The full example is available at: [https://gist.github.com/netogallo/6219579](https://gist.github.com/netogallo/6219579).
