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

[<WebSharper.Name "Strings">]
[<WebSharper.Proxy "Microsoft.FSharp.Core.StringModule, FSharp.Core">]
module private WebSharper.StringProxy

open WebSharper.JavaScript

module M = WebSharper.Core.Macros

let Compare (x: string) (y: string) = compare x y

let CopyTo (s: string) (o: int) (d: char []) (off: int) (ct: int) =
    Array.blit (s.ToCharArray()) o d off ct

[<Direct "$x.substring($x.length - $s.length) == $s">]
let EndsWith (x: string) (s: string) = X<bool>

[<Direct "$s.indexOf($c,$i)">]
let IndexOf (s: string) (c: char) (i: int) = X<int>

[<Direct "$x.substring(0,$index-1)+$s+$x.substring($index)">]
let Insert (x: string) (index: int) (s: string) = X<string>

[<Direct "$x == null || $x == ''">]
let IsNullOrEmpty (x: string) = X<bool>

[<Direct """$x == null || /^\s*$/.test($x)""">]
let IsNullOrWhiteSpace (x: string) = X<bool>

[<Direct "$s.lastIndexOf($c,$i)">]
let LastIndexOf (s: string) (c: char) (i: int) = X<int>

[<Direct "$n>$s.length?Array($n-$s.length+1).join($c)+$s:$s">]
let PadLeftWith (s: string) (n: int) (c: char) = X<string>

let PadLeft (s: string) (n: int) =
    PadLeftWith s n ' '

[<Direct "$n>$s.length?$s+Array($n-$s.length+1).join($c):$s">]
let PadRightWith (s: string) (n: int) (c: char) = X<string>

let PadRight (s: string) (n: int) =
    PadRightWith s n ' '

[<Direct "$x.substring(0,$ix) + $x.substring($ix+$ct)">]
let Remove (x: string) (ix: int) (ct: int) = X<string>

[<Direct "$string.replace($search,$replace)">]
let ReplaceOnce string search replace = X<string>

let Replace (subject: string) (search: string) (replace: string) =
    let rec replaceLoop (subj: string) =
        let index = subj.IndexOf(search)
        if index <> -1 then
            let replaced = ReplaceOnce subj search replace
            let nextStartIndex = index + replace.Length
            (replaced.Substring(0, index + replace.Length)) +
                (replaceLoop (replaced.Substring(nextStartIndex)))
        else subj
    replaceLoop subject

let ReplaceChar (s: string) (oldC: char) (newC: char) =
    Replace s (string oldC) (string newC)

[<Direct "$s.substr($ix,$ct)">]
let Substring (s: string) (ix: int) (ct: int) = X<string>

/// String.Substring(startIndex, length): out of range arguments throw, like .NET
let SubstringChecked (s: string) (ix: int) (ct: int) =
    if ix < 0 || ct < 0 || ix + ct > s.Length then raise (System.ArgumentOutOfRangeException("length"))
    Substring s ix ct

[<Direct "$t.substring(0,$s.length) == $s">]
let StartsWith (t: string) (s: string) = X<bool>

let ToCharArray (s: string) = Array.init s.Length (fun x -> s.[x])

let ToCharArrayRange (s: string) (startIndex: int) (length: int) =
    Array.init length (fun i -> s.[startIndex + i])

[<Direct @"$s.replace(/^\s+/,'').replace(/\s+$/,'')">]
let Trim (s: string) = X<string>

[<Direct @"$s.replace(/^\s+/,'')">]
let TrimStartWS (s: string) = X<string>

let TrimStart (s: string) (t: char[]) =
    if t = null || Array.isEmpty t then
        TrimStartWS s
    else
        let mutable i = 0
        let mutable go = true
        while i < s.Length && go do
            let c = s.[i]
            if t |> Array.exists ((=) c) then
                i <- i + 1 
            else go <- false
        s.Substring(i)

[<Direct @"$s.replace(/\s+$/,'')">]
let TrimEndWS (s: string) = X<string>

let TrimEnd (s: string) (t: char[]) =
    if t = null || Array.isEmpty t then
        TrimEndWS s
    else 
        let mutable i = s.Length - 1
        let mutable go = true
        while i >= 0 && go do
            let c = s.[i]
            if t |> Array.exists ((=) c) then
                i <- i - 1 
            else go <- false
        s.Substring(0, i + 1)

[<Direct "$values.join($sep)">]
let Join (sep: string) (values: string []) = X<string>

[<Direct "$str.split($pat)">]
let SplitWith (str: string) (pat: obj) = X<string[]>

[<Inline "new RegExp($pat)">]
let MakeRegexp (pat: string) = X<obj>

[<Direct @"$s.replace(/[-\/\\^$*+?.()|[\]{}]/g, '\\$&')">]
let RegexEscape (s: string) = X<string>

let Split (s: string) (pat: obj) (opts: System.StringSplitOptions) =
    let res = SplitWith s pat
    // TrimEntries (2) trims each part, RemoveEmptyEntries (1) then drops the empty ones
    let res = if JS.Inline("($0 & 2) !== 0", opts) then Array.map (fun (x: string) -> x.Trim()) res else res
    if JS.Inline("($0 & 1) !== 0", opts) then
        Array.filter (fun x -> x !==. "") res
    else
        res

let SplitChars (s: string) (sep: char[]) (opts: System.StringSplitOptions) =
    let re = "[" + RegexEscape (new System.String(sep)) + "]"
    Split s (MakeRegexp re) opts

let SplitStrings (s: string) (sep: string[]) (opts: System.StringSplitOptions) =
    let re = String.concat "|" (Array.map RegexEscape sep)
    Split s (MakeRegexp re) opts

/// StringComparison values with an odd number ignore case (CurrentCultureIgnoreCase = 1, InvariantCultureIgnoreCase = 3,
/// OrdinalIgnoreCase = 5); comparisons are ordinal otherwise
[<Inline "$c % 2 === 1">]
let IgnoresCase (c: System.StringComparison) = X<bool>

let ContainsCmp (s: string) (sub: string) (c: System.StringComparison) =
    if IgnoresCase c then s.ToLower().IndexOf(sub.ToLower()) <> -1 else s.IndexOf(sub) <> -1

let StartsWithCmp (s: string) (t: string) (c: System.StringComparison) =
    if IgnoresCase c then StartsWith (s.ToLower()) (t.ToLower()) else StartsWith s t

let EndsWithCmp (s: string) (t: string) (c: System.StringComparison) =
    if IgnoresCase c then EndsWith (s.ToLower()) (t.ToLower()) else EndsWith s t

let EqualsCmp (a: string) (b: string) (c: System.StringComparison) =
    if isNull a || isNull b then a ===. b
    elif IgnoresCase c then a.ToLower() = b.ToLower()
    else a = b

/// String.Split with a count: at most `count` parts, the last one holds the rest of the string
let SplitCount (s: string) (seps: string[]) (count: int) (opts: System.StringSplitOptions) =
    let parts = SplitStrings s seps opts
    if count <= 0 then [||]
    elif parts.Length <= count then parts
    else
        // find the position after the (count - 1)th separator in s
        let mutable pos = 0
        let mutable i = 0
        while i < count - 1 do
            let mutable best = -1
            let mutable bestLen = 0
            for sep in seps do
                let j = s.IndexOf(sep, pos)
                if j <> -1 && (best = -1 || j < best) then
                    best <- j
                    bestLen <- sep.Length
            pos <- best + bestLen
            i <- i + 1
        Array.append (Array.sub parts 0 (count - 1)) [| s.Substring(pos) |]

let Filter f (s: string) =
    System.String.Concat(s |> Seq.choose (fun c -> if f c then Some (string c) else None) |> Array.ofSeq)

[<Inline "$text.replace($pattern, $replace)">]
let ReplaceString (pattern: RegExp) (replace: 'obj) (text: string) = X<string>

let SFormat (format: string) (args: obj[]) =
    let pattern = RegExp("{{|}}|{(0|[1-9]\d*)(?:,(-?[1-9]\d*|0))?(?::(.*?))?}", "g")
    format
    |> ReplaceString pattern (FuncWithArgs(fun (m: string, i, w, f) ->
        if m = "{{" then "{"
        elif m = "}}" then "}"
        else
            let r = WebSharper.Formatting.FormatValue args.[JS.Plus i] (if f = JS.Undefined then "" else As<string> f)
            if w <> JS.Undefined then
                let w1 = JS.Plus w
                let w2 = abs w1
                if w2 > r.Length then
                    if w1 > 0 then r.PadLeft(w2)
                    else r.PadRight(w2)
                else r
            else r
    ))

[<Proxy(typeof<string>)>]
[<Type "string">]
type private StringProxy =

    [<Inline "''">]
    new () = {}

    [<Inline "$chars.join('')">]
    new (chars: char []) = {}

    [<Inline>]
    static member CtorProxy(ch: char, n: int) = String.replicate n (string ch)

    [<Inline "$chars.slice($i, $i + $j).join('')">]
    new (chars: char [], i: int, j: int) = {}

    member this.Chars  with [<Inline "$this[$pos]">]
                            get (pos: int) = X<char>

    [<Inline "$this">]
    member this.Clone() = this :> obj

    [<Inline "$this">]
    member this.Copy() = this

    [<Inline>]
    static member Compare(x: string, y: string) =
        Unchecked.compare x y

    [<Inline>]
    static member Compare(x: string, y: string, b: bool) =
        if b then
            Unchecked.compare (x.ToLower()) (y.ToLower())
        else
        Unchecked.compare x y

    [<Inline>]
    member this.CompareTo(s: string) =
        Unchecked.compare (this :> obj) (s :> obj)

    [<Inline>]
    member this.CompareTo(s: obj) =
        Unchecked.compare (this :> obj) s

    [<Inline>]
    static member Concat(strings: string seq) =
        Join "" (Array.ofSeq strings)

    [<Inline>]
    static member Concat<'T>(objs: 'T seq) : string =
        Join "" (Array.ofSeq (objs |> Seq.map (fun o -> WebSharper.Formatting.FormatValue (box o) "")))

    [<Inline>]
    static member Concat(s1: string, s2: string) = s1 + s2

    [<Inline>]
    static member Concat(s1: string, s2: string, s3: string) = s1 + s2 + s3

    [<Inline>]
    static member Concat(s1: string, s2: string, s3: string, s4: string) = s1 + s2 + s3 + s4

    [<Inline>]
    static member Concat(o1: obj) = WebSharper.Formatting.FormatValue o1 ""

    [<Inline>]
    static member Concat(o1: obj, o2: obj) =
        WebSharper.Formatting.FormatValue o1 "" + WebSharper.Formatting.FormatValue o2 ""

    [<Inline>]
    static member Concat(o1: obj, o2: obj, o3: obj) =
        WebSharper.Formatting.FormatValue o1 "" + WebSharper.Formatting.FormatValue o2 "" + WebSharper.Formatting.FormatValue o3 ""

    [<Inline "$strings.join('')">]
    static member Concat([<System.ParamArray>] strings: string[]) = X<string>

    [<Inline "$strings.join('')">]
    static member Concat([<System.ParamArray>] strings: ReadOnlySpanProxy<string>) = X<string>

    [<Inline>]
    static member Concat([<System.ParamArray>] objs: obj[]) =
        Join "" (objs |> Array.map (fun o -> WebSharper.Formatting.FormatValue o ""))

    [<Inline>]
    static member Concat([<System.ParamArray>] objs: ReadOnlySpanProxy<obj>) = 
        Join "" (As<obj[]> objs |> Array.map (fun o -> WebSharper.Formatting.FormatValue o ""))

    [<Inline "$this.indexOf($s) != -1">]
    member this.Contains(s: string) = X<bool>

    [<Inline "$this.indexOf($c) != -1">]
    member this.Contains(c: char) = X<bool>

    [<Inline>]
    member this.Contains(s: string, comparisonType: System.StringComparison) = ContainsCmp (As this) s comparisonType

    [<Inline>]
    member this.StartsWith(s: string, comparisonType: System.StringComparison) = StartsWithCmp (As this) s comparisonType

    [<Inline>]
    member this.EndsWith(s: string, comparisonType: System.StringComparison) = EndsWithCmp (As this) s comparisonType

    [<Inline>]
    member this.Equals(s: string, comparisonType: System.StringComparison) = EqualsCmp (As this) s comparisonType

    [<Inline>]
    static member Equals(a: string, b: string, comparisonType: System.StringComparison) = EqualsCmp a b comparisonType

    /// ordinal (UTF-16 code unit) comparison, the sign of .NET's result
    [<Inline "$a < $b ? -1 : $a > $b ? 1 : 0">]
    static member CompareOrdinal(a: string, b: string) = X<int>

    [<Inline "$this.slice(0, $startIndex) + $value + $this.slice($startIndex)">]
    member this.Insert(startIndex: int, value: string) = X<string>

    [<Inline "$this.toUpperCase()">]
    member this.ToUpperInvariant() = X<string>

    [<Inline "$this.toLowerCase()">]
    member this.ToLowerInvariant() = X<string>

    [<Inline>]
    member this.CopyTo(s: int, d: char [], off: int, ct: int) =
        CopyTo (As this) s d off ct

    static member Empty with [<Inline "''">] get () = X<string>

    [<Inline>]
    member this.EndsWith(other: string) = EndsWith (As this) other

    [<Inline "$x == $y">]
    static member Equals(x: string, y: string) = X<bool>

    [<Inline "$this == $s">]
    member this.Equals(s: string) = X<bool>

    [<Inline "$this === $s">]
    override this.Equals(s: obj) = X<bool>

    [<Inline>]
    override this.GetHashCode() = hash this

    [<Inline>]
    member this.GetEnumerator() = Enumerator.Get (unbox<seq<char>> this) |> As<System.CharEnumerator>

    [<Inline "$this.indexOf($s)">]
    member this.IndexOf(s: string) = X<int>

    [<Inline "$this.indexOf($c)">]
    member this.IndexOf(c: char) = X<int>

    [<Inline "$this.indexOf($s,$i)">]
    member this.IndexOf(s: string, i: int) = X<int>

    [<Inline>]
    member this.IndexOf(c: char, i: int) = IndexOf (As this) c i

    [<Inline>]
    static member IsNullOrEmpty(x: string) = IsNullOrEmpty x

    [<Inline>]
    static member IsNullOrWhiteSpace(x: string) = IsNullOrWhiteSpace x

    member this.Item
        with    [<Inline "$this[$pos]">]
                get (pos: int) = X<char>

    [<Inline>]
    static member Join(sep: string, values: string seq) =
        Join sep (Array.ofSeq values)

    /// String.Join<T>: each value's ToString, null as ""
    [<Inline>]
    static member Join<'T>(sep: string, values: seq<'T>) =
        Join sep (values |> Seq.map (fun x -> WebSharper.Formatting.FormatValue (box x) "") |> Array.ofSeq)

    /// String.Join(string, params object[]): each value as .NET formats it, null as ""
    [<Inline>]
    static member Join(sep: string, [<System.ParamArray>] values: obj[]) =
        Join sep (values |> Array.map (fun x -> WebSharper.Formatting.FormatValue x ""))

    [<Inline>]
    static member Join(sep: string, [<System.ParamArray>] values: ReadOnlySpanProxy<obj>) =
        Join sep (As<obj[]> values |> Array.map (fun x -> WebSharper.Formatting.FormatValue x ""))

    [<Inline>]
    static member Join(sep: string, [<System.ParamArray>] values: ReadOnlySpanProxy<string>) =
        Join sep (As<string[]> values)

    [<Inline>]
    static member Join(sep: string, [<System.ParamArray>] values: string[]) =
        Join sep values

    [<Inline "$this.lastIndexOf($s)">]
    member this.LastIndexOf(s: string) = X<int>

    [<Inline "$this.lastIndexOf($c)">]
    member this.LastIndexOf(c: char) = X<int>

    [<Inline "$this.lastIndexOf($s,$i)">]
    member this.LastIndexOf(s: string, i: int) = X<int>

    [<Inline>]
    member this.LastIndexOf(c: char, i: int) =
        LastIndexOf (As this) c i

    member this.Length with [<Inline "$this.length">]
                            get () = X<int>

    [<Inline>]
    member this.PadLeft(i: int) =
        PadLeft (As this) i

    [<Inline>]
    member this.PadLeft(i: int, c: char) =
        PadLeftWith (As this) i c

    [<Inline>]
    member this.PadRight(i: int) =
        PadRight (As this) i

    [<Inline>]
    member this.PadRight(i: int, c: char) =
        PadRightWith (As this) i c

    [<Inline "$this.substring(0,$ix)">]
    member this.Remove(ix: int) = X<string>

    [<Inline>]
    member this.Remove(ix: int, count: int) = Remove (As this) ix count

    [<Inline>]
    member this.ToCharArray() = ToCharArray (As this)

    [<Inline>]
    member this.Replace(subj: string, repl: string) =
        Replace (As this) subj repl

    [<Inline>]
    member this.Replace(subj: char, repl: char) =
        ReplaceChar (As this) subj repl

    [<Inline>]
    member this.Split([<System.ParamArray>] sep: char[]) =
        SplitChars (As this) sep  System.StringSplitOptions.None

    [<Inline>]
    member this.Split(sep: char[], opts: System.StringSplitOptions) =
        SplitChars (As this) sep opts

    [<Inline>]
    member this.Split(sep: char, opts: System.StringSplitOptions) =
        SplitChars (As this) [| sep |] opts

    [<Inline>]
    member this.Split(sep: string[], opts: System.StringSplitOptions) =
        SplitStrings (As this) sep opts

    [<Inline>]
    member this.Split(sep: string, opts: System.StringSplitOptions) =
        SplitStrings (As this) [| sep |] opts

    [<Inline>]
    member this.Split(sep: char, count: int, opts: System.StringSplitOptions) =
        SplitCount (As this) [| string sep |] count opts

    [<Inline>]
    member this.Split(sep: char[], count: int) =
        SplitCount (As this) (Array.map string sep) count System.StringSplitOptions.None

    [<Inline>]
    member this.Split(sep: string, count: int, opts: System.StringSplitOptions) =
        SplitCount (As this) [| sep |] count opts

    [<Inline>]
    member this.StartsWith(s: string) =
        StartsWith (As this) s

    [<Inline "$this.substring($ix)">]
    member this.Substring(ix: int) = X<string>

    [<Inline>]
    member this.Substring(ix: int, ct: int) =
        SubstringChecked (As this) ix ct

    [<Inline>]
    member this.ToCharArray(i: int, l: int) =
        ToCharArrayRange (As this) i l

    [<Inline "$this">]
    override this.ToString() = X<string>
    
    [<Inline "$this.toLowerCase()">]
    member this.ToLower() = X<string>

    [<Inline "$this.toUpperCase()">]
    member this.ToUpper() = X<string>

    [<Inline>]
    member this.Trim() = Trim (As this)

    [<Inline>]
    member this.Trim(t: char) = TrimEnd (TrimStart (As this) [| t |]) [| t |]

    [<Inline>]
    member this.Trim(t: char[]) = TrimEnd (TrimStart (As this) t) t

    [<Inline>]
    member this.TrimStart() = TrimStart (As this) null

    [<Inline>]
    member this.TrimStart(t: char) = TrimStart (As this) [| t |]

    [<Inline>]
    member this.TrimStart(t: char[]) = TrimStart (As this) t

    [<Inline>]
    member this.TrimEnd() = TrimEnd (As this) null

    [<Inline>]
    member this.TrimEnd(t: char) = TrimEnd (As this) [| t |]
    
    [<Inline>]
    member this.TrimEnd(t: char[]) = TrimEnd (As this) t

    [<Inline "$a + $b">]
    static member (+) (a: string, b: string) = X<string>

    [<Inline>]
    static member (+) (a: obj, b: string) = string a + b 

    [<Inline>]
    static member (+) (a: string, b: obj) = a + string b

    [<Inline "$a == $b">]
    static member op_Equality(a: string, b: string) = X<bool>

    [<Inline "$a != $b">]
    static member op_Inequality(a: string, b: string) = X<bool>

    [<Macro(typeof<M.StringFormat>)>]
    [<Inline>]
    static member Format(format: string, [<System.ParamArray>] arguments: obj []) = SFormat format arguments

    [<Macro(typeof<M.StringFormat>)>]
    [<Inline>]
    static member Format(format: string, [<System.ParamArray>] arguments: ReadOnlySpanProxy<obj>) = SFormat format (As arguments)

    [<Macro(typeof<M.StringFormat>)>]
    [<Inline>]
    static member Format(format: string, arg0: obj): string = SFormat format [|arg0|]

    [<Macro(typeof<M.StringFormat>)>]
    [<Inline>]
    static member Format(format: string, arg0: obj, arg1: obj): string = SFormat format [|arg0; arg1|]

    [<Macro(typeof<M.StringFormat>)>]
    [<Inline>]
    static member Format(format: string, arg0: obj, arg1: obj, arg2: obj): string = SFormat format [|arg0; arg1; arg2|]

let protect (s : string) =
    if s = null then "" else s

[<Inline "$strings.join($sep)">]
let join (strings: string[]) (sep: string) = X<string>

[<Name "collect">]
let Collect (f: char -> string) (s: string) : string =
    System.String.Concat(Array.init s.Length (fun i -> f s.[i]))

[<Name "concat">]
let Concat (separator: string) (strings: seq<string>) : string =
    join (Seq.toArray strings) separator

[<Name "exists">]
let Exists (f: char -> bool) (s: string) : bool =
    Seq.exists f (protect s)

[<Name "forall">]
let ForAll (f: char -> bool) (s: string) : bool =
    Seq.forall f (protect s)

[<Name "init">]
let Initialize (count: int) (f: int -> string) : string =
    System.String.Concat(Array.init count f)

[<Name "iter">]
let Iterate (f: char -> unit) (s: string) : unit =
    Seq.iter f (protect s)

[<Name "iteri">]
let IterateIndexed (f: int -> char -> unit) (s: string) : unit =
    Seq.iteri f (protect s)

[<Name "length">]
let Length (s: string) : int =
    (protect s).Length

[<Name "map">]
let Map (f: char -> char) (s: string) : string =
    Collect (fun x -> string (f x)) (protect s)

[<Name "mapi">]
let MapIndexed (f: int -> char -> char) (s: string) : string =
    System.String.Concat (Seq.toArray (Seq.mapi (fun i x -> string (f i x)) s))

[<Name "replicate">]
let Replicate (count: int) (s: string) : string =
    System.String.Concat(Array.create count s)
