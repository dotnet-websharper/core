# [alpha] LiveData: propagating database changes to the client

WebSharper.LiveData is a library that allows you to reactively synchronize collections between the server-side database and the client-side display and logic. It includes:

* Live synchronization of client-side collections from the server-side database using the WebSockets-based [DDP protocol from Meteor](https://www.meteor.com/ddp).
* Mapping of these live-synchronized collections to [UI.Next](UINext.md) list models.
* Insertion, update and deletion operations from the client side with latency compensation: changes are reflected immediately on the UI and then corrected based on the server's response.

It features a flexible database connector back-end, although there is currently only one connector for MongoDB.

This tutorial will teach you how to create a simple chat application using WebSharper.LiveData.

![Mini-chat screenshot](http://websharper.com/img/tuto-chat.png)

## Setting up the project

For this tutorial, you will need a self-hosted Client-Server application. The recommended way to create such a project is through the Visual Studio extension available [on the downloads page](http://websharper.com/downloads), or the Xamarin Studio addin available in the Add-in Manager.

You will need to add the following NuGet packages to your project:

* `WebSharper.LiveData`
* `WebSharper.LiveData.MongoDB`

These packages are on alpha version, so you need to enable installing pre-release packages. Make sure that all the `WebSharper.*` NuGet packages are at least on version 3.6.

You will also need to install and run MongoDB. Just follow the instructions [on the official website](https://www.mongodb.org/downloads).

## The project structure

The project initially contains three files called `Remoting.fs`, `Client.fs` and `Main.fs`. Let's rename `Remoting.fs` to `Db.fs`, and use these files as follows:

* `Db.fs` will contain the definition of the database and its collections.
* `Client.fs` will contain the client-side code, ie. the dynamic user interface.
* `Main.fs` will contain the web server code.

In the database, we will store messages as simple objects with three fields: `author` and `text` of type string, and `date` of type `DateTime`.

## Defining the database collections

Let's start by defining the database collections. Replace the contents of `Db.fs` with the following:

```fsharp
namespace LiveDataTutorial

open System
open WebSharper
open WebSharper.LiveData
open WebSharper.LiveData.Client

[<JavaScript>]
module Db =

    let Agent = Database.Agent.MongoDB "mongodb://localhost:27017"

    type Message = { author: string; text: string; date: DateTime }

    let Messages = PublicCollection<Message>.Create "mydb.messages" Agent
```

First, we define the agent that connects to the database. The URI is the default URI where the locally installed MongoDB runs.

Then, we define the record type for collection items.

Finally, we create a value of type `PublicCollection<Message>` that represents the collection of chat messages in the database. This tells the agent that the collection will be accessible from the client through LiveData. It also gives us a strongly-typed value that we will use from the client side to have a typed list model.

`Public` means that methods to insert, update and delete elements will be directly accessible from the client side. Anyone who can connect to the application will be able to post, modify and delete messages. This is acceptable for small prototypes like this, but obviously not secure for real-world applications. We will see later how to create private collections with methods that can do checks and rectify data before performing database operations.

Note that the `Db` module is annotated with a `[<JavaScript>]` attribute, meaning that it is compiled to client-side code, despite describing server-side operations. What happens is that methods like `Database.Agent.MongoDB` and `PublicCollection<_>.Create` use WebSharper features such as [inlines](http://websharper.com/docs/translation#heading-1-2) and [macros](http://websharper.com/docs/translation#heading-3-2) to run completely different code on the server side and on the client side.

## The list model and the user interface

Let's move on to `Client.fs`.

On the client side, the WebSocket connection to the LiveData service is represented by a value of type `DDP.Server`. Using it and the description of the database collection from above, we can create a `PublicSyncedListModel<Db.Message>`. This is a UI.Next list model which is automatically synchronized with the server-side database. The `initLimit` argument specifies that we want to initialize it with the 100 top items ordered by the `date` field. It is an optional argument, and if omitted, all items are retrieved from the database.

```fsharp
namespace LiveDataTutorial

open System
open WebSharper
open WebSharper.LiveData
open WebSharper.LiveData.Client
open WebSharper.UI.Next
open WebSharper.UI.Next.Html
open WebSharper.UI.Next.Client

[<JavaScript>]
module Client =

    let CreateListModel ddpServer =
        Db.Messages.CreatePublicSyncedListModel(
            ddpServer,
            initLimit = (100, "date")
        )
```

Adding items to a public list model is done using the `Add` method. This performs a latency-compensated insertion:

1. The item is inserted in the local list model and marked as not synchronized yet.
2. The item is sent through the WebSocket to be inserted in the database.
3. When receiving confirmation from the server that the item was inserted, it is marked as synchronized.

Let's post a "joined" message on startup using `Add` and the `onReady` optional argument to `CreatePublicSyncedListModel`:

```fsharp
    let PostMessage (messages: PublicSyncedListModel<Db.Message>) username text =
        messages.Add {
            author = username
            text = text
            date = DateTime.UtcNow
        }
        |> Async.Start

    let CreateListModel ddpServer username =
        Db.Messages.CreatePublicSyncedListModel(
            ddpServer,
            initLimit = (100, "date"),
            onReady = (fun messages ->
                PostMessage messages username "joined the conversation")
        )
```

Now we can create the user interface. For simplicity we will use a table for the messages, with one line per message.

```fsharp
    let ShowMessage (vMessage: View<SyncItem<Db.Message>>) =
        tr [
            td [textView (vMessage.Map (fun m -> m.Item.date.ToLongTimeString()))]
            td [textView (vMessage.Map (fun m -> "<" + m.Item.author + ">"))]
            td [textView (vMessage.Map (fun m -> m.Item.text))]
        ]
```

Messages have type `SyncItem<Db.Message>`. This is a record with three fields:

* `Item` contains the actual `Db.Message`;
* `LocalKey` contains a unique identifier local to the client side;
* `DbState` contains the synchronization status of the item, including its database identifier if it is synchronized.

For the user input, we will simply use a `Var<string>` for the input text.

```fsharp
    let InputBox (messages: PublicSyncedListModel<Db.Message>) username =
        let inputMessage = Var.Create ""
        label [
            text ("Post as " + username + ": ")
            Doc.Input [] inputMessage
            Doc.Button "Send" [] (fun () ->
                PostMessage messages username inputMessage.Value
                inputMessage.Value <- "")
        ]
```

And finally, we can put the UI together:

```fsharp
    let Main (ddpServer: DDP.Server) (username: string) =
        let messages = CreateListModel ddpServer username
        Doc.Concat [
            table [
                messages.View.DocSeqCached(messages.Key, fun _ vMessage ->
                    ShowMessage vMessage
                )
            ]
            div [ InputBox messages username ]
        ]
```

## The web server

Let's finish up the application by building up the server. We are going to create a single-page sitelet, and serve it with Microsoft.Owin.

First, the sitelet. We create it with `Application.SinglePage`. The whole interface is client-side, so the whole `Body` of the `Page` is just a call to the client-side control. The user is assigned a random username of the form `user1234`.

```fsharp
namespace LiveDataTutorial

open WebSharper
open WebSharper.Sitelets
open WebSharper.LiveData
open WebSharper.UI.Next
open WebSharper.UI.Next.Html

module Site =

    let Main ddpServer =
        Application.SinglePage (fun ctx ->
            let username = "user" + System.Random().Next(10000).ToString()
            Content.Page(
                Body = [ client <@ Client.Main ddpServer username @> ]
            )
        )
```

We also need to add a DDP server to the instantiation of the self-hosting web server. Since we are serving more than just a Sitelet, we replace the call to `UseSitelet` with the more customizable `UseWebSharper`.

```fsharp
module SelfHostedServer =
    open global.Owin
    open Microsoft.Owin.Hosting
    open Microsoft.Owin.StaticFiles
    open Microsoft.Owin.FileSystems
    open WebSharper.Owin

    [<EntryPoint>]
    let Main args =
        let rootDirectory, url =
            match args with
            | [| rootDirectory; url |] -> rootDirectory, url
            | [| url |] -> "..", url
            | [| |] -> "..", "http://localhost:9000/"
            | _ -> eprintfn "Usage: LiveDataTutorial ROOT_DIRECTORY URL"; exit 1
        use server = WebApp.Start(url, fun appB ->
            let ddpServer = DDP.Server.Create appB "/websocket" Db.Agent
            appB.UseStaticFiles(
                    StaticFileOptions(
                        FileSystem = PhysicalFileSystem(rootDirectory)))
                .UseWebSharper(
                    WebSharperOptions(
                        ServerRootDirectory = rootDirectory,
                        Sitelet = Some (Site.Main ddpServer))
                        .WithDDPServer(ddpServer))
            |> ignore)
        stdout.WriteLine("Serving {0}", url)
        stdin.ReadLine() |> ignore
        0
```

And there we have it! The mini-chat is ready to run. Just start the application and browse [localhost:9000](http://localhost:9000/).

You can get the project in this state [from the WebSharper.LiveData repository](https://github.com/intellifactory/websharper.livedata/tree/master/WebSharper.LiveData.MiniChat_1).

## Remote Methods

As mentioned earlier, public collections are generally unsafe: any connected client can insert, update or delete any item in the collection. For real-life applications, we need more control over what operations clients are able to perform.

Private collections, simply called `Collection<_>`, do not provide default `Add`, `Update` and `Delete` methods. Instead, you must register your own methods. Methods are declared as top-level values using a method from the `RegisterMethod` family. These methods on the `Collection` type take as arguments a string, which is the unique name that identifies the method on the wire, and a function which is the callback invoked on the server side when the method is called from the client side.

Among these methods, `RegisterAddMethod`, `RegisterUpdateMethod` and `RegisterDeleteMethod` provide the same latency compensation as the public collection methods. Let's replace our `PublicCollection` with a `Collection` and declare a method to post a message in `Db.fs`.

```fsharp
    let Messages = Collection<Message>.Create "mydb.messages" Agent

    let PostMessage = Messages.RegisterAddMethod("postMessage", fun ctx msg ->
        async {
            if msg.text.Length > 1000 then
                return None
            else
                return Some { msg with date = DateTime.UtcNow }
        }
    )
```

Here we perform both checking and rectification on the input message, ensuring that the message is not too long and that its date is current. With latency compensation, a user posting a message will immediately see it appear, and then when the server has completed the method call, it will either have its `DbState` set to `Error` (if it was rejected) or have its date fixed (if it was accepted).

Now, switching to `Client.fs`, you will notice that `Db.Messages.CreatePublicSyncedListModel` doesn't compile anymore, since `Db.Messages` is now a private collection. So let's replace all references to `PublicSyncedListModel` to `SyncedListModel`, which is the private variant. The `Add` method we used in `Client.PostMessage` is also specific to public list models, we can use `Db.PostMessage` instead.

```fsharp
    let PostMessage (messages: SyncedListModel<Db.Message>) username text =
        Db.PostMessage messages {
            author = username
            text = text
            date = DateTime.UtcNow
        }
        |> Async.Start

    let CreateListModel ddpServer username =
        Db.Messages.CreateSyncedListModel(
            ddpServer,
            initLimit = (100, "date"),
            onReady = (fun messages ->
                PostMessage messages username "joined the conversation")
        )
```

Now the application compiles and runs, and exposes a much safer API to connected clients.

There's one last thing that we need to change in the user interface. To see it, try to post a message longer than the 1000 characters limits we set in `Db.PostMessage`. From the poster's perspective, it looks like the message was sent correctly! Indeed, the fact that the message was rejected is reflected on the item's `DbState`, but we are not doing anything with it currently. Let's fix this by changing the text color based on the state:

```fsharp
    let ShowMessage (vMessage: View<SyncItem<Db.Message>>) =
        trAttr [
            Attr.DynamicStyle "color" (vMessage.Map (fun m ->
                match m.DbState with
                | Synced _ -> "black"
                | NotSynced _ -> "gray"
                | Error _ -> "red"
            ))
        ] [
            td [textView (vMessage.Map (fun m -> m.Item.date.ToLongTimeString()))]
            td [textView (vMessage.Map (fun m -> "<" + m.Item.author + ">"))]
            td [textView (vMessage.Map (fun m -> m.Item.text))]
        ]
```

That's better. Now messages are shown in gray while they're being sent, black when successful and red if rejected.

You can get the project in this state [from the WebSharper.LiveData repository](https://github.com/intellifactory/websharper.livedata/tree/master/WebSharper.LiveData.MiniChat_2).

![Mini-chat screenshot with rejected message](http://websharper.com/img/tuto-chat-reject.png)

## Going further

There are many enhancements that can be done to this application. Here are a few suggestions you can play with:

* Allow editing and deleting messages. You can create latency-compensated update and delete methods using `Collection.RegisterUpdateMethod` and `Collection.RegisterDeleteMethod`.

* Allow the user to choose their username.

* Use user sessions to authenticate users. You can access the user session from within a registered method using the `ctx` argument.

Have fun!
