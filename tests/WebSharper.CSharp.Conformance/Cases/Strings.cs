// $begin{copyright}
//
// This file is part of WebSharper
//
// Copyright (c) 2008-2026 IntelliFactory
//
// Licensed under the Apache License, Version 2.0 (the "License"); you
// may not use this file except in compliance with the License.  You may
// obtain a copy of the License at
//
//     http://www.apache.org/licenses/LICENSE-2.0
//
// Unless required by applicable law or agreed to in writing, software
// distributed under the License is distributed on an "AS IS" BASIS,
// WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or
// implied.  See the License for the specific language governing
// permissions and limitations under the License.
//
// $end{copyright}
using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using WebSharper;
using static Conformance.H;

namespace Conformance
{
    [JavaScript]
    public static class Strings
    {
        public static List<(string, Func<string>)> Cases() => new List<(string, Func<string>)>
        {
            C("split.basic", () => J("a,b,,c".Split(','))),
            C("split.removeempty", () => J("a,b,,c".Split(',', StringSplitOptions.RemoveEmptyEntries))),
            C("split.trimentries", () => J(" a , b ,c ".Split(',', StringSplitOptions.TrimEntries))),
            C("split.chars", () => J("a b;c".Split(new[] { ' ', ';' }))),
            C("split.string", () => J("a::b::c".Split("::"))),
            C("split.count", () => J("a,b,c,d".Split(',', 2))),
            C("trim.chars", () => "xxhixx".Trim('x') + "|" + "  hi ".TrimStart() + "|" + "  hi ".TrimEnd() + "|"),
            C("pad", () => "7".PadLeft(3, '0') + "|" + "ab".PadRight(4) + "|"),
            C("substring", () => "hello".Substring(1, 3) + "|" + "hello".Substring(2)),
            C("substring.outofrange", () => "hello".Substring(3, 5)),
            C("indexof", () => $"{"hello".IndexOf('l')},{"hello".LastIndexOf('l')},{"hello".IndexOf("lo")},{"hello".IndexOf('z')}"),
            C("indexof.startindex", () => $"{"abcabc".IndexOf('b', 2)}"),
            C("contains.comparison", () => B("Hello".Contains("hell", StringComparison.OrdinalIgnoreCase))),
            C("startswith.endswith", () => $"{B("Hello".StartsWith("He"))},{B("Hello".EndsWith("LO", StringComparison.OrdinalIgnoreCase))}"),
            C("equals.ignorecase", () => B(string.Equals("abc", "ABC", StringComparison.OrdinalIgnoreCase))),
            C("compare.ordinal", () => $"{Math.Sign(string.CompareOrdinal("a", "B"))},{Math.Sign(string.Compare("a", "b"))}"),
            C("upper.lower.invariant", () => "MiXeD".ToUpperInvariant() + "|" + "MiXeD".ToLowerInvariant()),
            C("replace", () => "a-b-c".Replace("-", "+") + "|" + "a-b-c".Replace('-', '_')),
            C("insert.remove", () => "hello".Insert(2, "XX") + "|" + "hello".Remove(1, 2) + "|" + "hello".Remove(3)),
            C("join.concat", () => string.Join("-", new[] { 1, 2, 3 }) + "|" + string.Concat("a", 1, true)),
            C("join.null", () => string.Join(",", new string[] { "a", null, "b" })),
            C("isnullorwhitespace", () => $"{B(string.IsNullOrWhiteSpace(" \t"))},{B(string.IsNullOrEmpty(""))}"),
            C("char.indexer", () => { var s = "abc"; return $"{s[1]},{s[^1]}"; }),
            C("string.range", () => "hello world"[6..] + "|" + "hello"[1..^1]),
            C("string.ctor.repeat", () => new string('=', 5)),
            C("tochararray.reverse", () => new string("hello".ToCharArray().Reverse().ToArray())),
            C("string.equality.ref", () => { var a = "ab"; var b = string.Concat("a", "b"); return B(a == b) + "," + B((object)a == (object)b) + "," + B(a.Equals(b)); }),
            C("stringbuilder", () => { var sb = new StringBuilder(); sb.Append("a").Append(1).AppendLine("b").Append('c'); sb.Insert(0, ">"); return sb.ToString().Replace("\n", "\\n").Replace("\r", "\\r") + "|" + sb.Length; }),
            C("stringbuilder.remove.replace", () => { var sb = new StringBuilder("hello world"); sb.Remove(0, 6).Replace('o', '0'); return sb.ToString(); }),
            C("interp.nested", () => { var xs = new[] { 1, 2 }; return $"{xs.Length} items: {string.Join(";", xs.Select(x => $"<{x}>"))}"; }),
            C("interp.null", () => { string s = null; object o = null; return $"[{s}][{o}]"; }),
            C("interp.adjacent.numbers", () => { int a = 1, b = 2; return $"{a}{b}"; }),
            C("interp.single.number.is.string", () => { object o = $"{7}"; return B(o is string); }),
            C("interp.bool", () => $"{true}|{false}"),
            C("raw.literal", () => """a "quoted" {x}"""),
            C("verbatim", () => @"c:\path\""q"""),
            C("escape.unicode", () => "\u00e9\t|".Length.ToString()),
            C("char.tostring.concat", () => 'a' + "b" + 'c'),
            C("char.plus.char", () => $"{'a' + 'b'}"),
            C("format.string.braces", () => string.Format("{{{0}}}", 5)),
            C("string.compareto.culture", () => $"{Math.Sign("apple".CompareTo("Banana"))}"),
            C("tostring.object.null", () => { object o = null; return o?.ToString() ?? "null"; }),
            C("string.split.empty", () => $"{"".Split(',').Length}"),
            C("trim.unicode.ws", () => $"[{"\u00a0x\u00a0".Trim()}]"),
            C("lastindexof.string", () => $"{"a.b.c".LastIndexOf(".")}"),
            C("string.contains.char", () => B("abc".Contains('b'))),
            C("string.equals.null", () => { string s = null; return B(s == null) + "," + B("a" != null); }),
        };
    }
}
