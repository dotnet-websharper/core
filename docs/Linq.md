# Using LINQ on the client-side

WebSharper provides support to process `IEnumerable` sequences using LINQ
queries on the client side.

## Simple queries

You can use LINQ query syntax in client-side code like you would in .NET.

```csharp
using System;
using System.Linq;
using System.Collections.Generic;
using WebSharper;

namespace TestLinq
{
    [JavaScript]
    public class App
    {
        public static void Main()
        {
            var people = new[] { "Don", "Simon", "Rich", "Joe", "Martin" };

            var oddPeople =
                from person in people
                where person.Length % 2 == 1
                orderby person.Length
                select person;

            Console.Log("People whose names have an odd length:");
            foreach (var person in oddPeople)
            {
                Console.Log("  " + person);
            }
            // prints:
            // People whose names have an odd length:
            //   Don
            //   Simon
            //   Joe
        }
    }
}
```

Of course, you can use explicit LINQ extension methods if you prefer.

```csharp
        public static void Main()
        {
            var people = new[] { "Don", "Simon", "Rich", "Joe", "Martin" };

            var oddPeople = people
                .Where(person => person.Length % 2 == 1)
                .OrderBy(person => person.Length);

            Console.Log("People whose names have an odd length:");
            foreach (var person in oddPeople)
            {
                Console.Log("  " + person);
            }
            // prints:
            // People whose names have an odd length:
            //   Don
            //   Simon
            //   Joe
        }
```

## Other Linq methods

WebSharper provides proxies for all `IEnumerable` extension methods from
`System.Linq`, such as `.Concat()`, `.ToArray()` or `.ElementAt()`:

```csharp
        public static void Main()
        {
            var people = new[] { "Don", "Simon", "Rich", "Joe", "Martin" };
            var morePeople = people.Concat(new[] { "Xavier", "Evan" }).ToArray();

            Console.Log("The sixth person is", morePeople.ElementAt(5));
            // prints:
            // The sixth person is Xavier
        }
```

## Using custom comparers

Advanced uses of Linq methods are possible, such as custom comparers. Here is
an example where we make a sequence's items distinct by their length using an
`EqualityComparer`:

```csharp
        public static void Main()
        {
            var people = new[] { "Don", "Simon", "Rich", "Joe", "Martin" };

            var distinctPeople = people.Distinct(new ByLength());

            foreach (var person in distinctPeople)
            {
                Console.Log(person);
            }
            // prints:
            //   Don
            //   Simon
            //   Rich
            //   Martin
        }

        public class ByLength : EqualityComparer<string>
        {
            override public bool Equals(string x, string y)
            {
                return x.Length == y.Length;
            }

            public override int GetHashCode(string x)
            {
                return x.Length;
            }
        }
```

## Methods dealing with default values

Some Linq methods use the default value of the element type. For example,
`.FirstOrDefault()` returns the first element of the `IEnumerable`, or a
default type if it is empty. This default value depends on the type: for
example, if the element type is `int`, then the default value is `0`, whereas
if the element type is a class, then the default value is `null`. WebSharper
can provide the correct default value, on the condition that the element type
is known statically. For example, the following will run correctly:

```csharp
        public static void Main()
        {
            var people = new string[] { };
            var onePerson = people.FirstOrDefault();
            Console.Log(onePerson);
            // prints:
            // null
        }
```

However, the following will fail to compile because the element type is not
known statically:

```csharp
        public static T GetDefaultPerson<T>(IEnumerable<T> people)
        {
            return people.FirstOrDefault();
            // compile error:
            // Macro 'WebSharper.MacroModule+DefaultOf' requires a resolved type argument.
        }
```
