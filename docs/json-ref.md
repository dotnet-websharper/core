# Server-side JSON Cheat Sheet

Here is a quick reference for the JSON format used by `[<Json>]`-annotated [inferred sitelets](Sitelets.md#json-request), [Content.JsonContent](Sitelets.md#json-response) and `WebSharper.Json`.

## Base types

Encoded as JSON primitive types.

</td></tr>
<tr><th>Type</th><th>Value and corresponding encoded JSON</th></tr>
<tr><td>

```fsharp
type Body = string
```
</td><td>

```fsharp
"home"

// "home"

"This is \"home\"!"

// "This is \"home\"!"
```
</td></tr>
<tr><td>

```fsharp
type Body = int
```
</td><td>

```
2

// 2
```
</td></tr>
<tr><td>

```fsharp
type Body = float
```
</td><td>

```fsharp
1.2345

// 1.2345
```
</td></tr>
<tr><td colspan="2">And every standard .NET number type: int8, uint8, int16, uint16, int32, uint32, int64, uint64, single, double, decimal.</td></tr>
<tr><td colspan="2">
## Records

Encoded as a JSON object.

</td></tr>
<tr><th>Type</th><th>Value and corresponding encoded JSON</th></tr>
<tr><td>

```fsharp
type Action =
  { x: string
    y: int }
```
</td><td>

```fsharp
{ x = "test"
  y = 1 }

// {"x":"test","y":1}
```
</td></tr>
<tr><td>

```fsharp
type Action =
  { x: string
    y: Sub }
and Sub =
  { z: int
    t: int }
```
</td><td>

```fsharp
{ x = "test"
  y =
    { z = 1
      t = 2 } }

// {"x":"test","y":{"z":1,"t":2}}
```
</td></tr>
<tr><td colspan="2">
## Unions

Encoded as a JSON object. Use `[<NamedUnionCases(fieldname)>]` to store the case name in a field, and `[<Name(name)>]` for the name of each field.

</td></tr>
<tr><th>Type</th><th>Value and corresponding encoded JSON</th></tr>
<tr><td>

```fsharp
[<NamedUnionCases "result">]
type Body =
  | [<Name "success">]
    Success of value: int
  | [<Name "error">]
    Error of message: string
```
</td><td>

```fsharp
Success 42

// {"result":"success","value":42}

Error "Wrong value."

// {"result":"error","message":"Wrong value."}
```
</td></tr>
<tr><td colspan="2">

Use `[<NamedUnionCases>]` to infer the union case based on the present field names.

</td></tr>
<tr><td>

```fsharp
[<NamedUnionCases>]
type Body =
  | Success of value: int
  | Error of error: string
```
</td><td>

```fsharp
Success 42

// {"value":42}

Error "Incorrect value."

// {"error":"Incorrect value."}
```

</td></tr>
<tr><td colspan="2">

A single unnamed record argument is treated as if its fields were the arguments.

</td></tr>
<tr><td>

```fsharp
[<NamedUnionCases "result">]
type Body =
  | [<Name "success">]
    Success of Value
  | [<Name "error">]
    Error of message: string

and Value =
  { id: int
    name: string }
```
</td><td>

```fsharp
Success { id = 1; name = "abc" }

// {"result":"success","id":1,"name":"abc"}
```

</td></tr>
<tr><td colspan="2">

Use `[<Constant>]` to represent an argument-less union case as a string, int, float or bool instead of an object.

</td></tr>
<tr><td>

```fsharp
type Body =
  | [<Constant "red">] Red
  | [<Constant true>] Green
  | [<Constant 3>] Blue
```
</td><td>

```fsharp
[Red; Green; Blue]

// ["red",true,3]
```

</td></tr>
<tr><td colspan="2">
## Options

Union case arguments of type option become present/absent fields.

</td></tr>
<tr><th>Type</th><th>Value and corresponding encoded JSON</th></tr>
<tr><td>

```fsharp
[<NamedUnionCases>]
type Body =
  | Success of value: int
             * remark: string option
  | Error of error: string
```
</td><td>

```fsharp
Success(41, Some "Almost there...")

// {"value":41,"remark":"Almost there..."}

Success(42, None)

// {"value":42}
```

</td></tr>
<tr><td colspan="2">

Record fields of type option become present/absent fields.

</td></tr>
<tr><td>

```fsharp
type Body =
  { value: int
    remark: string option }
```
</td><td>

```fsharp
{ value = 41; remark = Some "Almost there..." }

// {"value":41,"remark":"Almost there..."}

{ value = 42; remark = None }

// {"value":42}
```

</td></tr>
<tr><td colspan="2">
## Collections

Arrays, lists and sets are represented as JSON arrays.

</td></tr>
<tr><th>Type</th><th>Value and corresponding encoded JSON</th></tr>
<tr><td>

```fsharp
type Body = int[]
```
</td><td>

```fsharp
[| 4; 8; 15; 16; 23; 42 |]

// [4,8,15,16,23,42]
```
</td></tr>
<tr><td>

```fsharp
type Body = Person list

and Person =
  { name: string }
```
</td><td>

```fsharp
[
  { name = "John" }
  { name = "Bob" }
]

// [{"name":"John"},{"name":"Bob"}]
```
</td></tr>
<tr><td>

```fsharp
type Body = Set<string>
```
</td><td>

```fsharp
Set [ "fsharp"; "websharper" ]

// ["fsharp","websharper"]
```
</td></tr>
<tr><td colspan="2">

Maps and dictionaries with string keys are represented as JSON objects.

</td></tr>
<tr><td>

```fsharp
type Body = Map<string, int>
```
</td><td>

```fsharp
Map [
  "John", 38
  "Bob", 46
]

// {"John":38,"Bob":46}
```
</td></tr>
<tr><td>

```fsharp
open System.Collections.Generic

type Body = Dictionary<string, Person>

and Person =
  { age: int }
```
</td><td>

```fsharp
let dict = Dictionary()
dict.["John"] <- { age = 38 }
dict.["Bob"] <- { age = 46 }

// {"John":{"age":38},"Bob":{"age":46}}
```
</td></tr>
<tr><td colspan="2">
## Datetime

Encoded as a string. Default format is ISO-8601 round-trip format (`"o"`).

</td></tr>
<tr><th>Type</th><th>Value and corresponding encoded JSON</th></tr>
<tr><td>

```fsharp
type Action = System.DateTime
```
</td><td>

```fsharp
System.DateTime.UtcNow

// "2015-04-15T15:37:23.0000000"
```
</td></tr>
<tr><td colspan="2">

Setting the format of a DateTime union case argument with `[<DateTimeFormat(argname, format)>]`.

</td></tr>
<tr><td>

```fsharp
type Body =
  | [<DateTimeFormat("date", "yyyy-MM-dd")>]
    Article of id: int * date: DateTime
```
</td><td>

```fsharp
Article(43, System.DateTime.Now)

// {"id":43,"date":"2015-04-15"}
```
</td></tr>
<tr><td colspan="2">

Setting the format of a DateTime record field with `[<DateTimeFormat(format)>]`.

</td></tr>
<tr><td>

```fsharp
type Body =
  { id: int
    [<DateTimeFormat("yyyy-MM-dd")>]
    date: DateTime }
```
</td><td>

```fsharp
{ id = 43; date = System.DateTime.Now }

// {"id":43,"date":"2015-04-15"}
```
</td></tr>
<tr><td colspan="2">

Both `DateTimeFormat` uses also work on `DateTime option`.

</td></tr>
<tr><td>

```fsharp
[<NamedUnionCases>]
type Body =
  | [<DateTimeFormat("time", "HH:mm:ss")>]
    Body of time: DateTime option * child: Child

and Child =
  { [<DateTimeFormat("yyyy-MM-dd")>]
    date: DateTime option }
```
</td><td>

```fsharp
Body(Some System.DateTime.Now, { date = None })

// {"time":"15:37:23","child":{}}

Body(None, { date = Some System.DateTime.Now })

// {"child":{"date":"2015-04-15"}}
```
</td></tr>
</table>