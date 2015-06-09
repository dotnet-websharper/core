# Server-side JSON API

WebSharper Sitelets can automatically [parse JSON requests](Sitelets.md#json-request), and [reply with JSON content](Sitelets.md#json-response). The structure of the JSON is inferred from the type, and can be customized using attributes.

## Base types

The following base types are handled:

* Integers: `int8`, `int16`, `int32` (aka `int`), `int64`

```fsharp
Content.JsonContent <| fun ctx ->
    12y

// Output: 12
```

* Unsigned integers: `uint8` (aka `byte`), `uint16`, `uint32`, `uint64`

```fsharp
Content.JsonContent <| fun ctx ->
    12ul

// Output: 12
```

* Floats: `single`, `double` (aka `float`)

```fsharp
Content.JsonContent <| fun ctx ->
    12.34

// Output: 12.34
```

* Decimals: `decimal`

```fsharp
Content.JsonContent <| fun ctx ->
    12.34m

// Output: 12.34
```

* Strings: `string`

```fsharp
Content.JsonContent <| fun ctx ->
    """A string with some "content" inside"""

// Output: "A string with some \"content\" inside"
```

* Booleans: `bool`

```fsharp
Content.JsonContent <| fun ctx ->
    true

// Output: true
```

## Collections

Values of type `list<'T>`, `'T[]` and `Set<'T>` are represented as JSON arrays:

```fsharp
Content.JsonContent <| fun ctx ->
    [|"a string"; "another string"|]

// Output: ["a string", "another string"]

Content.JsonContent <| fun ctx ->
    Set ["a string"; "another string"]

// Output: ["another string", "a string"]
```

Values of type `Map<string, 'T>` and `System.Collections.Generic.Dictionary<string, 'T>` are represented as flat JSON objects:

```fsharp
Content.JsonContent <| fun ctx ->
    Map [("somekey", 12); ("otherkey", 34)]

// Output: {"somekey": 12, "otherkey": 34}
```

## Records

F# records are represented as flat JSON objects. The attribute `[<Name "name">]` can be used to customize the field name:

```fsharp
type Name =
    {
        [<Name "first-name">] FirstName: string
        LastName: string
    }

type User =
    {
        name: Name
        age: int
    }

Content.JsonContent <| fun ctx ->
    {name = {FirstName = "John"; LastName = "Doe"}; age = 42}

// Output: {"name": {"first-name": "John", "LastName": "Doe"}, "age": 42}
```

## Unions

Union types intended for use in JSON serialization should bear the attribute `NamedUnionCases`. There are two ways to use it.

### Explicit discriminator

With `[<NamedUnionCases "field">]`, the union value is represented as a JSON object with a field called `"field"`, whose value is the name of the union case, and as many other fields as the union case has arguments. You can use `[<Name "name">]` to customize the name of a union case.

```fsharp
[<NamedUnionCases "kind">]
type Contact =
    | [<Name "address">]
        Address of street: string * zip: string * city: string
    | Email of email: string

Content.JsonContent <| fun ctx ->
    [
        Address("12 Random St.", "15243", "Unknownville")
        Email "john.doe@example.com"
    ]

// Output: [
//           {"kind": "address",
//            "street": "12 Random St.",
//            "zip": "15243",
//            "city": "Unknownville"},
//           {"kind": "Email",
//            "email": "john.doe@example.com"}
//         ]
```

Unnamed fields receive the names `Item1`, `Item2`, etc.

### Implicit discriminator

With an argumentless `[<NamedUnionCases>]`, no extra field is added to determine the union case; instead, it is inferred from the names of the fields present. This means that each case must have at least one mandatory field that no other case in the same type has, or a compile-time error will be thrown.

```fsharp
[<NamedUnionCases>]
type Contact =
    | Address of street: string * zip: string * city: string
    | Email of email: string

Content.JsonContent <| fun ctx ->
    [
        Address("12 Random St.", "15243", "Unknownville")
        Email "john.doe@example.com"
    ]

// Output: [
//           {"street": "12 Random St.",
//            "zip": "15243",
//            "city": "Unknownville"},
//           {"email": "john.doe@example.com"}
//         ]
```

### Record inside union

As a special case, if a union case has a single, unnamed argument which is a record, then the fields of this record are used as the fields of the output object.

```fsharp
type Address = { street: string; zip: string; city: string }

[<NamedUnionCases>]
type Contact =
    | Address of Address
    | Email of email: string

Content.JsonContent <| fun ctx ->
    [
        Address {
            street = "12 Random St."
            zip = "15243"
            city = "Unknownville"
        }
        Email "john.doe@example.com"
    ]

// Output: [
//           {"street": "12 Random St.",
//            "zip": "15243",
//            "city": "Unknownville"},
//           {"email": "john.doe@example.com"}
//         ]
```

## Optional fields

Fields with type `option<'T>` are represented as a field that may or may not be there. This is the case both for unions and records.

```fsharp
[<NamedUnionCases>]
type Contact =
    | Address of street: string * zip: string * city: string option
    | Email of email: string

type User =
    {
        fullName: string
        age: int option
        contact: Contact
    }

Content.JsonContent <| fun ctx ->
    [
        {
            fullName = "John Doe"
            age = Some 42
            contact = Address("12 Random St.", "15243", Some "Unknownville")
        }
        {
            fullName = "Jane Doe"
            age = None
            contact = Address("53 Alea St.", "51423", None)
        }
    ]

// Output: [
//           {"fullName": "John Doe",
//            "age": 42,
//            "contact":{"street": "12 Random St.",
//                       "zip": "15243",
//                       "city": "Unknownville"}},
//           {"fullName": "Jane Doe",
//            "contact":{"street": "53 Alea St.",
//                       "zip": "51423"}}
//         ]
```

When parsing JSON, `null` is also accepted as a `None` value.

## DateTimes

Values of type `System.DateTime` are encoded using an ISO 8601 round-trip format string:

```fsharp
Content.JsonContent <| fun ctx ->
    System.DateTime.UtcNow

// Output: "2015-03-06T17:05:19.2077851Z"
```

The format can be customized with the attribute `[<DateTimeFormat>]`. This attribute can be placed either on a record field of type `System.DateTime` or `option<System.DateTime>`, or on a union case with an argument of one of these types.

```fsharp
type Action =
    {
        [<DateTimeFormat "yyyy-MM-dd">] dateOnly: System.DateTime
    }

Content.JsonContent <| fun ctx ->
    { dateOnly = System.DateTime.UtcNow }

// Output: { dateOnly: "2015-03-24" }

[<NamedUnionCases>]
type Action =
    | [<DateTimeFormat("time", "HH.mm.ss")>] A of time: System.DateTime

// Output: { dateOnly: "15.03.32" }
```
