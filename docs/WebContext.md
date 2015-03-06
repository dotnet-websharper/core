# Web Context

Both in Sitelets and Rpc functions, WebSharper provides a value of type `WebSharper.Web.IContext` that gives some contextual information about the current request.

## Retrieving the context

### Sitelets

In Sitelets, the context provided by [content-generating functions](Sitelets.md#content) such as `PageContent` or `CustomContent` implements `Web.IContext`, so you can use it directly.

### Rpc functions

In Rpc functions, the context can be retrieved using the function `WebSharper.Web.Remoting.GetContext()`. Be careful to only call it from the thread from which your function was called. A typical Rpc has the following structure:

```fsharp
open WebSharper
open WebSharper.Web

[<Rpc>]
let MyRpcFunction () =
    // Retrieve the context outside of the async {} expression.
    let context = Remoting.GetContext()
    async {
        // Once retrieved, use the context at will here.
        return System.IO.File.ReadAllText(context.RootFolder + "/someContent.txt")
    }
```

<a name="user-sessions"></a>
## User Sessions

The main reason to use the context is to manage user sessions. The member `UserSession` has the following members:

* `LoginUser : username: string * ?persistent: bool -> Async<unit>`

    Logs in the user with the given username. This sets a cookie that is uniquely associated with this username. Set `persistent` to `true` if the user session should last beyond the user's current browser session.

* `GetLoggedInUser : unit -> Async<string option>`

    Retrieves the currently logged in user's username, or `None` if the user is not logged in.

* `Logout : unit -> unit`

    Logs the user out.

The implementation of these functions relies on cookies and thus requires that the browser has enabled cookies.

## Other Context functionality

* `context.RootFolder` returns the physical folder on the server machine from which the application is running.