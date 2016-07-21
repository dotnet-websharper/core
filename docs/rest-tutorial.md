# Implementing a REST API

Thanks to [Sitelets](../docs/Sitelets.md), it is easy to implement a REST API with WebSharper. This tutorial will guide you through the steps to create such an API.

Here is the format of the API we will create:

```
# Request:                              # Response:

GET /api/person?id=42

                                        { "result": "success",
                                          "firstName": "Alan",
                                          "lastName": "Turing",
                                          "born": "1912-06-23",
                                          "died": "1954-06-07" }

# Request:                              # Response:

POST /api/person

{ "firstName": "Noam",                  { "result": "success",
  "lastName": "Chomsky",                  "id": 43 }
  "born": "1928-12-07" }

# Request:                              # Response:

PUT /api/person?id=44

{ "firstName": "Bertrand",              { "result": "error",
  "lastName": "Russell",                  "message": "User id does not exist" }
  "born": "1872-05-18",
  "died": "1970-02-02" }

# Request:                              # Response:

DELETE /api/person?id=43

                                        { "result": "success" }
```

Here are the steps to define a REST API with sitelets:

* [Define the action type](#action), ie. the shape of the URLs and the JSON request content.
* [Define the sitelet response types](#response), ie. the shape of the JSON response content.
* [Implement the application logic](#logic), ie. the functionality invoked by each action.
* [Implement the sitelet itself](#sitelet), ie. mapping from actions to application logic and returning the response as JSON.

<a name="action"></a>
## Defining the action type

Our API has four different cases, so we can define it as a union type:

```fsharp
type ApiAction =
  | GetPerson
  | PostPerson
  | PutPerson
  | DeletePerson
```

Then, we can associate each case with the HTTP method it responds to using the attribute `Method`:

```fsharp
type ApiAction =
  | [<Method "GET">]
    GetPerson
  | [<Method "POST">]
    PostPerson
  | [<Method "PUT">]
    PutPerson
  | [<Method "DELETE">]
    DeletePerson
```

By default, an action responds to a URL fragment equal to its name, for example here `/GetPerson`. To change this, we use the attribute `CompiledName`:

```fsharp
type ApiAction =
  | [<Method "GET"; CompiledName "person">]
    GetPerson
  | [<Method "POST"; CompiledName "person">]
    PostPerson
  | [<Method "PUT"; CompiledName "person">]
    PutPerson
  | [<Method "DELETE"; CompiledName "person">]
    DeletePerson
```

Now, we can add parameters. All parameters can be passed as argument to the union cases. Which part of the request they are parsed from depends on attributes:

* If there is an attribute `Query "name"` where `"name"` is the name of an argument, then this argument is passed via the query string.
* If there is an attribute `Json "name"` where `"name"` is the name of an argument,
then this argument is passed as JSON content. There can only be one `Json` argument per case.
* Arguments that are neither `Query` nor `Json` are passed via the URL path, like so: `/person/42`.

```fsharp
type ApiAction =
  | [<Method "GET"; CompiledName "person"; Query "id">]
    GetPerson of id: int
  | [<Method "POST"; CompiledName "person"; Json "personData">]
    PostPerson of personData: PersonData
  | [<Method "PUT"; CompiledName "person"; Query "id"; Json "personData">]
    PutPerson of id: int * personData: PersonData
  | [<Method "DELETE"; CompiledName "person"; Query "id">]
    DeletePerson of id: int

and PersonData =
  {
    firstName: string
    lastName: string
    [<DateTimeFormat "yyyy-MM-dd">] born: System.DateTime
    [<DateTimeFormat "yyyy-MM-dd">] died: option<System.DateTime>
  }
```

You can refer to the [full Sitelets documentation](../docs/Sitelets.md#sitelet-infer) for more information on the URL parsing, and the [JSON documentation](../docs/Json.md) for a detailed description of the JSON formatting.

<a name="response"></a>
## Defining the JSON response types

The JSON response types are defined similarly to the JSON request types such as `PersonData` here.

A good idea is to define a generic type for results, that indicates success or returns an error message in case of failure.

```fsharp
[<NamedUnionCases "result">]
type Result<'T> =
  | [<CompiledName "success">] Success of 'T
  | [<CompiledName "failure">] Failure of message: string
```

The above definition creates a type that is translated to JSON as follows:

```
Success { id = 12 }      // {"result": "success", id: 12 }
Failure "Error message"  // {"result": "failure", "message": "Error message"}
```

With this defined, here are the response types for the 4 actions:

* `GetPerson` returns `Result<PersonData>`.
* `PostPerson` returns `Result<PersonId>`, where `type PersonId = { id: int }`.
* `PutPerson` and `DeletePerson` both return `Result<option<unit>>`. On success, we'll return `Success None`, which results in no extra JSON field added next to `"result": "success"`.

<a name="logic"></a>
## Implementing the logic

Now, let's implement the application logic. For this simple example, we'll store the data in-memory in a hash table:

```fsharp
module ApplicationLogic =
  open System.Collections.Generic

  let people = new Dictionary<int, PersonData>()
  let lastId = ref 0
```

`lastId` stores the highest id we've used so far, to determine what should be the id of the next `POST`ed value. We'll use [`lock`](https://msdn.microsoft.com/en-us/library/ee370413.aspx) to ensure that only one function can access the database at once.

```fsharp
  let getPerson (id: int) =
    lock people <| fun () ->
      match people.TryGetValue(id) with
      | true, person -> Success person
      | false, _ -> Failure "Person not found."

  let postPerson (data: PersonData) =
    lock people <| fun () ->
      incr lastId
      people.[!lastId] <- data
      Success { id = !lastId }

  let putPerson (id: int) (data: PersonData) =
    lock people <| fun () ->
      match people.TryGetValue(id) with
      | true, _ ->
        people.[id] <- data
        Success None
      | false, _ -> Failure "Person not found."

  let deletePerson (id: int) =
    lock people <| fun () ->
      match people.TryGetValue(id) with
      | true, _ ->
        people.Remove(id) |> ignore
        Success None
      | false, _ -> Failure "Person not found."
```

<a name="sitelet"></a>
## Implement the sitelet

It's time to put all of this together! Let's write a function that takes an `ApiAction`, calls the appropriate implementation, and returns the result as JSON content:

```fsharp
let ApiContent (action: ApiAction) =
  match action with
  | GetPerson id ->
    Content.JsonContent <| fun ctx ->
      getPerson id
  | PostPerson personData ->
    Content.JsonContent <| fun ctx ->
      postPerson personData
  | PutPerson (id, personData) ->
    Content.JsonContent <| fun ctx ->
      putPerson id personData
  | DeletePerson id ->
    Content.JsonContent <| fun ctx ->
      deletePerson id
```


We can now put our action in the context of a larger application, and tie it all up in a sitelet:

```fsharp
type Action =
  | [<CompiledName "api">] Api of ApiAction
  | [<CompiledName "">] Home
  // Other website actions...

let MainSitelet =
  Sitelet.Infer <| function
    | Api apiAction -> ApiContent apiAction
    | Home -> Content.NotFound // Put the content of your home page here
    // Other website actions...
```

And we're good to go!

You can check the full code for this tutorial [here](https://github.com/intellifactory/websharper/blob/master/tests/WebSharper.Sitelets.Tests/Api.fs), with the main sitelet defined [here](https://github.com/intellifactory/websharper/blob/master/tests/WebSharper.Sitelets.Tests/SampleSite.fs).