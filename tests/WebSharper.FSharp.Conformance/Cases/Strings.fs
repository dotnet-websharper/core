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
namespace Conformance

open System
open System.Text
open WebSharper
open Conformance.H

[<JavaScript>]
module Strings =
    let private s (x: string) = x
    let private n (x: int) = x

    let Cases () = [
        C "string.length" (fun () -> string (s "héllo").Length)
        C "string.index" (fun () -> string (s "abc").[1])
        C "string.index.out.of.range" (fun () -> string (s "abc").[5])
        C "string.slice" (fun () -> (s "abcdef").[1..3] + "," + (s "abcdef").[..1] + "," + (s "abcdef").[4..])
        C "string.slice.out.of.range" (fun () -> (s "abc").[1..10])
        C "string.substring" (fun () -> (s "abcdef").Substring(2, 3))
        C "string.substring.bad" (fun () -> (s "abc").Substring(2, 5))
        C "string.concat.null" (fun () -> s "a" + (null: string) + "b")
        C "string.of.null" (fun () -> "[" + string (null: string) + "]")
        C "string.of.option" (fun () -> string (Some 1) + "|" + string (None: int option))
        C "string.of.unit" (fun () -> "[" + string () + "]")
        C "string.of.bool" (fun () -> string true + "," + string false)
        C "string.of.char" (fun () -> string 'x')
        C "string.of.list" (fun () -> string [1; 2; 3])
        C "string.of.tuple" (fun () -> string (1, "a"))
        C "string.of.list.long" (fun () -> string [ 1 .. 10 ] + "|" + string ([]: int list))
        C "string.of.list.nested" (fun () -> string [ "a"; "b" ] + "|" + string [ true ] + "|" + string [ Some 1 ] + "|" + string [ [ 1 ]; [ 2 ] ] + "|" + string [ (null: string); "a" ] + "|" + string [ (None: int option) ])
        C "string.of.option.nested" (fun () -> string (Some (Some 1)) + "|" + string (Some true) + "|" + string (Some [ 1; 2 ]) + "|" + string (Some (1, 2)) + "|" + string (Some (null: string)) + "|" + string (Some (None: int option)))
        C "string.of.tuple.nested" (fun () -> string (1, true, 2.5) + "|" + string (1, (2, 3)) + "|" + string (Some 1, (None: int option)) + "|" + string ((null: string), 1))
        C "string.of.obj" (fun () -> string (box true) + "|" + string (null: obj) + "|" + string (box 3))
        C "string.split" (fun () -> J ((s "a,b,,c").Split(',')))
        C "string.split.options" (fun () -> J ((s "a,b,,c").Split([| ',' |], StringSplitOptions.RemoveEmptyEntries)))
        C "string.split.string" (fun () -> J ((s "a--b--c").Split([| "--" |], StringSplitOptions.None)))
        C "string.trim" (fun () -> "[" + (s "  x  ").Trim() + "][" + (s "xxaxx").Trim('x') + "][" + (s "  x").TrimStart() + "]")
        C "string.pad" (fun () -> (s "7").PadLeft(3, '0') + "," + (s "ab").PadRight(4, '.'))
        C "string.replace" (fun () -> (s "a.b.c").Replace(".", "--") + "," + (s "aaa").Replace('a', 'b'))
        C "string.indexof" (fun () -> string ((s "hello").IndexOf "l") + "," + string ((s "hello").LastIndexOf 'l') + "," + string ((s "hello").IndexOf "z"))
        C "string.indexof.start" (fun () -> string ((s "hello").IndexOf("l", 3)))
        C "string.contains" (fun () -> B ((s "hello").Contains "ell") + "," + B ((s "hello").StartsWith "he") + "," + B ((s "hello").EndsWith "lo"))
        C "string.case" (fun () -> (s "Hello").ToUpper() + "," + (s "Hello").ToLower())
        C "string.case.invariant" (fun () -> (s "Hello").ToUpperInvariant() + "," + (s "Hello").ToLowerInvariant())
        C "string.compare.ordinal" (fun () -> string (sign (String.CompareOrdinal(s "a", s "B"))))
        C "string.compare.fsharp" (fun () -> string (sign (compare (s "a") (s "B"))) + "," + string (sign (compare (s "apple") (s "Apple"))))
        C "string.sort.fsharp" (fun () -> J (List.sort [ s "b"; "B"; "a"; "A" ]))
        C "string.equals.ignorecase" (fun () -> B (String.Equals(s "abc", s "ABC", StringComparison.OrdinalIgnoreCase)))
        C "string.isnullorempty" (fun () -> B (String.IsNullOrEmpty null) + "," + B (String.IsNullOrWhiteSpace " "))
        C "string.join" (fun () -> String.Join("-", [| "a"; "b"; "c" |]) + "," + String.Join(",", [ 1; 2 ]))
        C "string.concat.array" (fun () -> String.Concat([| "a"; "b" |]))
        C "string.reverse" (fun () -> String(Array.rev ((s "abc").ToCharArray())))
        C "string.ctor.repeat" (fun () -> String('x', n 3))
        C "string.insert.remove" (fun () -> (s "abc").Insert(1, "XY") + "," + (s "abcdef").Remove(1, 2))
        C "string.equality" (fun () -> B (s "abc" = "a" + "bc") + "," + B (obj.ReferenceEquals(s "abc", "a" + s "bc")))
        C "string.module.concat" (fun () -> String.concat ", " [ "a"; "b" ])
        C "string.module.map" (fun () -> String.map Char.ToUpper (s "abc"))
        C "string.module.collect" (fun () -> String.collect (fun c -> string c + string c) (s "ab"))
        C "string.module.init" (fun () -> String.init 3 (fun i -> string i))
        C "string.module.replicate" (fun () -> String.replicate 3 (s "ab"))
        C "string.module.forall" (fun () -> B (String.forall Char.IsDigit (s "123")) + "," + B (String.exists Char.IsUpper (s "abC")))
        C "string.module.filter" (fun () -> String.filter Char.IsLetter (s "a1b2"))
        C "string.module.length.null" (fun () -> string (String.length null))
        C "string.seq" (fun () -> J (s "abc" |> Seq.map string |> Seq.rev))
        C "string.toCharArray" (fun () -> string ((s "abc").ToCharArray().Length))
        C "char.tests" (fun () -> B (Char.IsDigit '5') + "," + B (Char.IsLetter 'é') + "," + B (Char.IsWhiteSpace '\t') + "," + B (Char.IsUpper 'A'))
        C "char.case" (fun () -> string (Char.ToUpper 'a') + string (Char.ToLower 'Q') + string (Char.ToUpperInvariant 'é') + "," + B (Char.IsLower 'é') + B (Char.IsDigit '٣'))
        C "char.unicode.upper" (fun () -> string (Char.ToUpper 'ß') + "," + (s "straße").ToUpper())
        C "char.surrogate.length" (fun () -> string (s "😀").Length)
        C "stringbuilder" (fun () -> let sb = StringBuilder() in sb.Append("a").Append(1).Append('c') |> ignore; sb.ToString())
        C "stringbuilder.appendline" (fun () -> let sb = StringBuilder() in sb.AppendLine("a") |> ignore; sb.ToString())
        C "interp.basic" (fun () -> let x = n 3 in $"x={x}")
        C "interp.adjacent" (fun () -> let a, b = n 1, n 2 in $"{a}{b}")
#if !JS
        C "interp.typed" (fun () -> let x = n 3 in $"%d{x}|%5d{x}|%-3d{x}|")
#else
        C "interp.typed" (fun () -> "WS-COMPILE-ERROR")
#endif
#if !JS
        C "interp.format" (fun () -> let x = 3.14159 in $"{x:F2}")
#else
        C "interp.format" (fun () -> "WS-COMPILE-ERROR")
#endif
        C "interp.float" (fun () -> let x = 2.0 in $"{x}")
        C "interp.string" (fun () -> let x = s "q" in $"[%s{x}]")
        C "interp.verbatim" (fun () -> let x = n 1 in $"""{x} "quoted" """)
        C "escape.sequences" (fun () -> string (s "a\tb\n").Length)
        C "triple.quoted" (fun () -> """a"b""")
    ]

[<JavaScript>]
module Formatting =
    type private P = { Name: string; Age: int }
    type private U = UA | UB of int | UC of string * float

    let private n (x: int) = x

    let Cases () = [
        C "sprintf.d" (fun () -> sprintf "%d|%i|%5d|%-5d|%05d" (n 42) (n 42) (n 42) (n 42) (n 42))
        C "sprintf.d.negative" (fun () -> sprintf "%05d|%+d" (n -42) (n 42))
        C "sprintf.s" (fun () -> sprintf "[%s][%5s][%-5s]" "ab" "ab" "ab")
        C "sprintf.s.null" (fun () -> sprintf "[%s]" (null: string))
        C "sprintf.f" (fun () -> sprintf "%f|%.2f|%8.3f|%-8.1f|" 3.14159 3.14159 3.14159 3.14159)
        C "sprintf.f.rounding" (fun () -> sprintf "%.0f|%.0f|%.1f" 0.5 1.5 0.25)
        C "sprintf.e" (fun () -> sprintf "%e|%.2e" 12345.678 12345.678)
#if !JS
        C "sprintf.g" (fun () -> sprintf "%g|%g|%g" 0.0001 123456789.0 1.5)
#else
        C "sprintf.g" (fun () -> "WS-COMPILE-ERROR")
#endif
        C "sprintf.x" (fun () -> sprintf "%x|%X|%04x|%o" (n 255) (n 255) (n 255) (n 8))
        C "sprintf.x.negative" (fun () -> sprintf "%x" (n -1))
        C "sprintf.c" (fun () -> sprintf "%c%c" 'o' 'k')
        C "sprintf.b" (fun () -> sprintf "%b|%b" true false)
        C "sprintf.percent" (fun () -> sprintf "100%%")
        C "sprintf.star" (fun () -> sprintf "%*d|%.*f" 5 (n 42) 2 3.14159)
        C "sprintf.O" (fun () -> sprintf "%O|%O" (n 1) "s")
        C "sprintf.A.int" (fun () -> sprintf "%A" (n 42))
        C "sprintf.A.string" (fun () -> sprintf "%A" "hi")
        C "sprintf.A.float" (fun () -> sprintf "%A|%A|%A" 1.0 1.5 nan)
        C "sprintf.A.list" (fun () -> sprintf "%A" [ 1; 2; 3 ])
        C "sprintf.A.array" (fun () -> sprintf "%A" [| 1; 2 |])
        C "sprintf.A.tuple" (fun () -> sprintf "%A" (1, "a", true))
        C "sprintf.A.option" (fun () -> sprintf "%A|%A" (Some 1) (None: int option))
        C "sprintf.A.record" (fun () -> sprintf "%A" { Name = "Ann"; Age = 30 })
        C "sprintf.A.union" (fun () -> sprintf "%A|%A|%A" UA (UB 1) (UC ("x", 2.5)))
        C "sprintf.A.nested" (fun () -> sprintf "%A" [ Some (1, "a") ])
        C "sprintf.A.char" (fun () -> sprintf "%A" 'c')
        C "sprintf.A.unit" (fun () -> sprintf "%A" ())
        C "sprintf.A.map" (fun () -> sprintf "%A" (Map.ofList [ 1, "a" ]))
        C "sprintf.partial" (fun () -> let f = sprintf "%d-%s" in f 1 "a" + "," + f 2 "b")
        C "sprintf.int64" (fun () -> sprintf "%d" 9007199254740993L)
        C "sprintf.u" (fun () -> sprintf "%u" 4294967295u)
#if !JS
        C "sprintf.M" (fun () -> sprintf "%M" 1.50M)
#else
        C "sprintf.M" (fun () -> "WS-COMPILE-ERROR")
#endif
        C "failwithf" (fun () -> try failwithf "bad %d" (n 7) with e -> e.Message)
        C "string.format" (fun () -> String.Format("{0}-{1}", n 1, "a"))
        C "string.format.align" (fun () -> String.Format("[{0,5}][{0,-5}]", n 42))
        C "string.format.spec" (fun () -> String.Format("{0:F1}|{0:N0}|{1:X2}", 1234.56, n 10))
        C "string.format.escape" (fun () -> String.Format("{{{0}}}", n 1))
        C "record.tostring" (fun () -> string { Name = "Ann"; Age = 30 })
        C "union.tostring" (fun () -> string (UB 1) + "|" + string UA)
    ]
