using System;
using System.Collections;
using System.Collections.Generic;
using System.Linq;
using System.Runtime.CompilerServices;
using System.Threading.Tasks;
using WebSharper.Testing;

namespace WebSharper.CSharp.Tests
{
    [JavaScript, Test("C# 15 syntax")]
    public class CSharp15 : TestCategory
    {
        [Test]
        public void CollectionExpressionArguments()
        {
            List<int> list = [with(capacity: 12), 1, 2, .. new[] { 3, 4 }];
            Equal(list.ToArray(), new[] { 1, 2, 3, 4 });

            HashSet<string> set = [with(new CSharp15StringComparer()), "Hello", "HELLO", "other"];
            Equal(set.Count, 2);
            IsTrue(set.Contains("hello"));

            ArgumentCollection items = [with(offset: 10, capacity: 4), 1, .. new[] { 2, 3 }];
            Equal(items.Offset, 10);
            Equal(items.Capacity, 4);
            Equal(items.ToArray(), new[] { 11, 12, 13 });

            ArgumentCollection empty = [with(7)];
            Equal(empty.Offset, 7);
            Equal(empty.Capacity, 8, "optional constructor argument");
            Equal(empty.Count(), 0);
        }

        [Test]
        public void CollectionArgumentEvaluationOrder()
        {
            var events = new List<int>();
            int Mark(int value) { events.Add(value); return value; }
            ArgumentCollection items = [with(capacity: Mark(4), offset: Mark(10)), Mark(1), .. new[] { Mark(2), Mark(3) }];
            Equal(events.ToArray(), new[] { 4, 10, 1, 2, 3 });
            Equal(items.ToArray(), new[] { 11, 12, 13 });
        }

        [Test]
        public void CollectionBuilderArguments()
        {
            BuiltCollection items = [with(offset: 10), 1, .. new[] { 2, 3 }];
            Equal(items.ToArray(), new[] { 11, 12, 13 });
            BuiltCollection ordinary = [4, .. new[] { 5, 6 }];
            Equal(ordinary.ToArray(), new[] { 4, 5, 6 });
            ReadOnlySpan<int> span = [7, 8];
            var copy = span.ToArray();
            copy[0] = 99;
            Equal(span.ToArray(), new[] { 7, 8 }, "span ToArray returns a copy");
        }

        [Test]
        public void UnionConversionsAndPatterns()
        {
            NumberOrText number = 42;
            NumberOrText text = "hello";
            int Length(NumberOrText value) => value switch
            {
                int n => n,
                string s => s.Length,
            };
            Equal(Length(number), 42);
            Equal(Length(text), 5);
            Equal((int)number.Value, 42);
            Equal(new NumberOrText("test").Value, "test");
            Equal(((NumberOrText)21).Value, 21, "explicit case-to-union conversion");
            IsTrue(number is int n && n == 42);
            IsTrue(text is string { Length: 5 });
            IsTrue(number is 42);
            IsTrue(number is > 40 and < 50);
            NumberOrText numericText = "42";
            IsFalse(numericText is 42, "constant patterns retain the case type");
            IsFalse(numericText is > 40, "relational patterns retain the case type");
            IsTrue(numericText is "42");
            IsTrue(text is not int);
            IsTrue(number is var whole && (int)whole.Value == 42);
            IsTrue(default(NumberOrText) is null);
            IUnion union = text;
            Equal(union.Value, "hello");
        }

        [Test]
        public void UnionRecordsAndGenerics()
        {
            CSharp15Pet pet = new CSharp15Dog("Rex");
            Equal(pet switch
            {
                CSharp15Cat(var name) => name,
                CSharp15Dog { Name: var name } => name,
            }, "Rex");
            IsTrue(pet is CSharp15Dog and { Name: "Rex" });
            CSharp15Result<int> result = 7;
            Equal(result.GetValue(), 7);
            result = "error";
            Equal(result.GetValue(), -1);
            NumberOrText? optional = 9;
            IsTrue(optional is int n && n == 9);
            optional = null;
            IsTrue(optional is null);
            CustomCSharp15Union custom = 12;
            IsTrue(custom is int value && value == 12);
            custom = "abc";
            IsTrue(custom is string { Length: 3 });
            custom = null;
            IsTrue(custom is null);
        }

        [Test]
        public void ExtensionIndexers()
        {
            IEnumerable<int> numbers = new[] { 10, 20, 30 };
            Equal(numbers[1], 20, "expression-bodied indexer");
            Equal(numbers[index: 2], 30, "named index argument");
            var box = new IndexerBox();
            box[1, 2] = 5;
            Equal(box[1, 2], 5, "block-bodied getter and setter");
            Equal(box[1, 2] = 7, 7, "assignment result");
            Equal(box[1, 2] += 3, 10, "compound assignment");
            Equal(box[1, 2]++, 10, "postfix increment");
            Equal(++box[1, 2], 12, "prefix increment");
            Equal(box[1, 2], 12);
            IndexerBox absent = null;
            Equal(absent?[1, 2], null);
            box["abc"] = 17;
            Equal(box.Items[3], 17, "setter-only indexer");
            IEnumerable<string> words = new[] { "one", "two" };
            Equal(words[1], "two", "generic extension indexer");
            Equal(box[1, 2, 3, 4], 10, "params index arguments");
            box[true] ??= 5;
            Equal(box[true], 5);
            box[true] ??= 7;
            Equal(box[true], 5);
        }

        [Test]
        public void ExtensionIndexerEvaluationOrder()
        {
            var box = new IndexerBox();
            var events = new List<int>();
            IndexerBox Receiver() { events.Add(1); return box; }
            int Index(int value) { events.Add(value); return value; }
            Receiver()[Index(2), Index(3)] += Index(4);
            Equal(events.ToArray(), new[] { 1, 2, 3, 4 });
            Equal(box[2, 3], 4);
            events.Clear();
            Equal(Receiver()[column: Index(3), row: Index(2)] += Index(4), 8);
            Equal(events.ToArray(), new[] { 1, 3, 2, 4 }, "named arguments preserve source order");
            events.Clear();
            Equal(Receiver()[Index(2), Index(3)]++, 8);
            Equal(events.ToArray(), new[] { 1, 2, 3 }, "increment evaluates the receiver and indices once");
        }

        [Test]
        public void LabeledForBreakAndContinue()
        {
            var values = new List<int>();
            int Value(int row, int column) => row * 10 + column;
            outer: for (int i = 0; i < 4; i++)
            {
                for (int j = 0; j < 3; j++)
                {
                    if (i == 2) break outer;
                    values.Add(Value(i, j));
                    if (j == 1) continue outer;
                }
            }
            Equal(values.ToArray(), new[] { 0, 1, 10, 11 });
        }

        [Test]
        public void LabeledWhileAndDoWhile()
        {
            var i = 0;
            var count = 0;
            outer: while (++i <= 3)
            {
                for (int j = 0; j < 3; j++)
                {
                    count++;
                    if (j == 0) continue outer;
                }
            }
            Equal(count, 3);
            i = 0;
            count = 0;
            again: do
            {
                i++;
                for (int j = 0; j < 3; j++)
                {
                    count++;
                    if (i == 3) break again;
                    if (j == 0) continue again;
                }
            } while (i < 4);
            Equal(i, 3);
            Equal(count, 3);
        }

        [Test]
        public void LabeledForeachAndFinally()
        {
            var count = 0;
            var disposed = 0;
            outer: foreach (var i in new[] { 1, 2, 3 })
            {
                try
                {
                    foreach (var j in new[] { 1, 2 })
                    {
                        count++;
                        if (i == 3) break outer;
                        if (j == 1) continue outer;
                    }
                }
                finally { disposed++; }
            }
            Equal(count, 3);
            Equal(disposed, 3);
        }

        [Test]
        public void LabeledSwitch()
        {
            var count = 0;
            target: switch (1)
            {
                case 1:
                    for (int i = 0; i < 3; i++)
                    {
                        count++;
                        if (i == 0) break target;
                    }
                    count += 10;
                    break;
            }
            Equal(count, 1);
        }

        private static IEnumerable<int> LabeledIterator()
        {
            outer: for (int i = 0; i < 4; i++)
            {
                for (int j = 0; j < 3; j++)
                {
                    if (i == 2) break outer;
                    yield return i * 10 + j;
                    if (j == 1) continue outer;
                }
            }
        }

        [Test]
        public void LabeledIteratorJumps()
        {
            Equal(LabeledIterator().ToArray(), new[] { 0, 1, 10, 11 });
        }

        [Test]
        public void LabeledJumpsAndOrdinaryContinue()
        {
            var count = 0;
            outer: do
            {
                count++;
                if (count == 1) continue;
                for (int i = 0; i < 2; i++)
                {
                    if (i == 0) break outer;
                }
            } while (count < 1);
            Equal(count, 1, "ordinary continue checks the do-while condition");

            var events = new List<int>();
            loop: for (int i = 0; i < 2; events.Add(10), i++)
            {
                try
                {
                    if (i == 0) continue;
                    if (i == 1) break loop;
                }
                finally { events.Add(i); }
            }
            Equal(events.ToArray(), new[] { 0, 10, 1 }, "finally runs before the for increment");
        }

        [Test]
        public async Task LabeledAsyncJumps()
        {
            var count = 0;
            outer: for (int i = 0; i < 4; i++)
            {
                for (int j = 0; j < 3; j++)
                {
                    await Task.Delay(1);
                    if (i == 2) break outer;
                    count++;
                    if (j == 0) continue outer;
                }
            }
            Equal(count, 2);
        }

        [Test]
        public async Task LabeledAsyncFinallyJumps()
        {
            var count = 0;
            var cleanup = 0;
            outer: for (int i = 0; i < 4; i++)
            {
                try
                {
                    for (int j = 0; j < 3; j++)
                    {
                        await Task.Delay(1);
                        if (i == 2) break outer;
                        count++;
                        if (j == 0) continue outer;
                    }
                }
                finally { cleanup++; }
            }
            Equal(count, 2);
            Equal(cleanup, 3, "finally executes once when leaving each iteration");
        }
    }

    [JavaScript]
    public class CSharp15StringComparer : IEqualityComparer<string>
    {
        public bool Equals(string x, string y) => x.ToLower() == y.ToLower();
        public int GetHashCode(string value) => value.ToLower().GetHashCode();
    }

    [JavaScript]
    public class ArgumentCollection : IEnumerable<int>
    {
        private readonly List<int> items = new();
        public ArgumentCollection(int offset, int capacity = 8)
        {
            Offset = offset;
            Capacity = capacity;
        }
        public int Offset { get; }
        public int Capacity { get; }
        public void Add(int item) => items.Add(item + Offset);
        public IEnumerator<int> GetEnumerator() => items.GetEnumerator();
        IEnumerator IEnumerable.GetEnumerator() => GetEnumerator();
    }

    [JavaScript, CollectionBuilder(typeof(CSharp15CollectionBuilder), nameof(CSharp15CollectionBuilder.Create))]
    public class BuiltCollection(int[] items) : IEnumerable<int>
    {
        public IEnumerator<int> GetEnumerator() => ((IEnumerable<int>)items).GetEnumerator();
        IEnumerator IEnumerable.GetEnumerator() => GetEnumerator();
    }

    [JavaScript]
    public static class CSharp15CollectionBuilder
    {
        public static BuiltCollection Create(ReadOnlySpan<int> items) => new(items.ToArray());
        public static BuiltCollection Create(int offset, ReadOnlySpan<int> items)
            => new(items.ToArray().Select(x => x + offset).ToArray());
    }

    [JavaScript] public union NumberOrText(int, string);
    [JavaScript] public record CSharp15Cat(string Name);
    [JavaScript] public record CSharp15Dog(string Name);
    [JavaScript] public union CSharp15Pet(CSharp15Cat, CSharp15Dog);
    [JavaScript]
    public union CSharp15Result<T>(T, string)
    {
        public int GetValue() => this switch { int n => n, _ => -1 };
    }

    [JavaScript, Union]
    public sealed class CustomCSharp15Union : IUnion
    {
        public CustomCSharp15Union(int value) { Value = value; }
        public CustomCSharp15Union(string value) { Value = value; }
        public object Value { get; }
    }

    [JavaScript]
    public class IndexerBox
    {
        public int[] Items = Enumerable.Repeat(0, 20).ToArray();
        public int? Optional;
    }

    [JavaScript]
    public static class CSharp15Indexers
    {
        extension(IEnumerable<int> sequence)
        {
            public int this[int index] => sequence.ElementAt(index);
        }

        extension(IndexerBox box)
        {
            public int this[params int[] indexes] => indexes.Sum();
            public int this[int row, int column]
            {
                get { return box.Items[row * 4 + column]; }
                set { box.Items[row * 4 + column] = value; }
            }
            public int this[string key]
            {
                set { box.Items[key.Length] = value; }
            }
            public int? this[bool key]
            {
                get => box.Optional;
                set => box.Optional = value;
            }
        }
    }

    [JavaScript]
    public static class GenericCSharp15Indexers
    {
        extension<T>(IEnumerable<T> sequence)
        {
            public T this[int index] => sequence.ElementAt(index);
        }
    }
}
