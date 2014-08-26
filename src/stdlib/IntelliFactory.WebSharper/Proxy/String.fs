// $begin{copyright}
//
// This file is part of WebSharper
//
// Copyright (c) 2008-2013 IntelliFactory
//
// GNU Affero General Public License Usage
// WebSharper is free software: you can redistribute it and/or modify it under
// the terms of the GNU Affero General Public License, version 3, as published
// by the Free Software Foundation.
//
// WebSharper is distributed in the hope that it will be useful, but WITHOUT
// ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
// FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License
// for more details at <http://www.gnu.org/licenses/>.
//
// If you are unsure which license is appropriate for your use, please contact
// IntelliFactory at http://intellifactory.com/contact.
//
// $end{copyright}

[<IntelliFactory.WebSharper.Core.Attributes.Name "Strings">]
[<IntelliFactory.WebSharper.Core.Attributes.Proxy
    "Microsoft.FSharp.Core.StringModule, \
     FSharp.Core, Culture=neutral, \
     PublicKeyToken=b03f5f7f11d50a3a">]
module private IntelliFactory.WebSharper.StringProxy

[<JavaScript>]
let Compare (x: string) (y: string) = compare x y

[<JavaScript>]
let CopyTo (s: string) (o: int) (d: char []) (off: int) (ct: int) =
    Array.blit (s.ToCharArray()) o d off ct

[<Direct "$x.substring($x.length - $s.length) == $s">]
let EndsWith (x: string) (s: string) = X<bool>

[<Direct "$s.indexOf(String.fromCharCode($c),$i)">]
let IndexOf (s: string) (c: char) (i: int) = X<int>

[<Direct "$x.substring(0,$index-1)+$s+$x.substring($index)">]
let Insert (x: string) (index: int) (s: string) = X<string>

[<Direct "$x == null || $x == ''">]
let IsNullOrEmpty (x: string) = X<bool>

[<Direct "$s.lastIndexOf(String.fromCharCode($c),$i)">]
let LastIndexOf (s: string) (c: char) (i: int) = X<int>

[<Inline "Array($n-$s.length+1).join(String.fromCharCode($c))+$s">]
let PadLeftWith (s: string) (n: int) (c: char) = X<string>

[<JavaScript>]
let PadLeft (s: string) (n: int) =
    PadLeftWith s n ' '

[<Inline "$s+Array($n-$s.length+1).join(String.fromCharCode($c))">]
let PadRightWith (s: string) (n: int) (c: char) = X<string>

[<JavaScript>]
let PadRight (s: string) (n: int) =
    PadRightWith s n ' '

[<Direct "$x.substring(0,$ix) + $x.substring($ix+$ct)">]
let Remove (x: string) (ix: int) (ct: int) = X<string>

[<Direct "$string.replace($search,$replace)">]
let ReplaceOnce string search replace = X<string>

[<JavaScript>]
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

[<JavaScript>]
let ReplaceChar (s: string) (oldC: char) (newC: char) =
    Replace s (string oldC) (string newC)

[<Direct "$s.substr($ix,$ct)">]
let Substring (s: string) (ix: int) (ct: int) = X<string>

[<Direct "$t.substring(0,$s.length) == $s">]
let StartsWith (t: string) (s: string) = X<bool>

[<JavaScript>]
let ToCharArray (s: string) = Array.init s.Length (fun x -> s.[x])

[<JavaScript>]
let ToCharArrayRange (s: string) (startIndex: int) (length: int) =
    Array.init length (fun i -> s.[startIndex + i])

[<Direct @"$s.replace(/^\s+/,'').replace(/\s+$/,'')">]
let Trim (s: string) = X<string>

[<Direct "$values.join($sep)">]
let Join (sep: string) (values: string []) = X<string>

[<Direct "$str.split($pat)">]
let SplitWith (str: string) (pat: obj) = X<string[]>

[<Inline "new RegExp($pat)">]
let MakeRegexp (pat: string) = X<obj>

[<Direct @"$s.replace(/[-\/\\^$*+?.()|[\]{}]/g, '\\$&')">]
let RegexEscape (s: string) = X<string>

[<JavaScript>]
let Split (s: string) (pat: obj) (opts: System.StringSplitOptions) =
    let res = SplitWith s pat
    if opts ===. System.StringSplitOptions.RemoveEmptyEntries then
        Array.filter (fun x -> x !==. "") res
    else
        res

[<JavaScript>]
let SplitChars (s: string) (sep: char[]) (opts: System.StringSplitOptions) =
    let re = "[" + RegexEscape (new System.String(sep)) + "]"
    Split s (MakeRegexp re) opts

[<JavaScript>]
let SplitStrings (s: string) (sep: string[]) (opts: System.StringSplitOptions) =
    let re = String.concat "|" (Array.map RegexEscape sep)
    Split s (MakeRegexp re) opts

[<Proxy(typeof<string>)>]
type private StringProxy =

    [<Inline "''">]
    new () = {}

    [<Inline "String.fromCharCode.apply(undefined,$chars)">]
    new (chars: char []) = {}

    member this.Chars  with [<Inline "$this.charCodeAt($pos)">]
                            get (pos: int) = X<char>

    [<Inline "$this">]
    member this.Clone() = this

    [<Inline>]
    [<JavaScript>]
    static member Compare(x: string, y: string) =
        Unchecked.compare x y

    [<Inline>]
    [<JavaScript>]
    member this.CompareTo(s: string) =
        Unchecked.compare (this :> obj) (s :> obj)

    [<Inline>]
    [<JavaScript>]
    static member Concat(strings: string seq) =
        Join "" (Array.ofSeq strings)

    [<Inline "$strings.join('')">]
    static member Concat([<System.ParamArray>] strings: string[]) = X<string>

    [<Inline "$this.indexOf($s) != -1">]
    member this.Contains(s: string) = X<bool>

    [<Inline>]
    [<JavaScript>]
    member this.CopyTo(s: int, d: char [], off: int, ct: int) =
        CopyTo (As this) s d off ct

    static member Empty with [<Inline "''">] get () = X<string>

    [<Inline>]
    [<JavaScript>]
    member this.EndsWith(other: string) = EndsWith (As this) other

    [<Inline "$x === $y">]
    static member Equals(x: string, y: string) = X<bool>

    [<Inline>]
    [<JavaScript>]
    member this.GetEnumerator() = Enumerator.Get this

    [<Inline "$this.indexOf($s)">]
    member this.IndexOf(s: string) = X<int>

    [<Inline "$this.indexOf(String.fromCharCode($c))">]
    member this.IndexOf(c: char) = X<int>

    [<Inline "$this.indexOf($s,$i)">]
    member this.IndexOf(s: string, i: int) = X<int>

    [<Inline>]
    [<JavaScript>]
    member this.IndexOf(c: char, i: int) = IndexOf (As this) c i

    [<Inline>]
    [<JavaScript>]
    static member IsNullOrEmpty(x: string) = IsNullOrEmpty x

    member this.Item
        with    [<Inline "$this.charCodeAt($pos)">]
                get (pos: int) = X<char>

    [<Inline>]
    [<JavaScript>]
    static member Join(sep: string, values: string seq) =
        Join sep (Array.ofSeq values)

    [<Inline>]
    [<JavaScript>]
    static member Join(sep: string, [<System.ParamArray>] values: string[]) =
        Join sep values

    [<Inline "$this.lastIndexOf($s)">]
    member this.LastIndexOf(s: string) = X<int>

    [<Inline "$this.lastIndexOf(String.fromCharCode($c))">]
    member this.LastIndexOf(c: char) = X<int>

    [<Inline "$this.lastIndexOf($s,$i)">]
    member this.LastIndexOf(s: string, i: int) = X<int>

    [<Inline>]
    [<JavaScript>]
    member this.LastIndexOf(c: char, i: int) =
        LastIndexOf (As this) c i

    member this.Length with [<Inline "$this.length">]
                            get () = X<int>

    [<Inline>]
    [<JavaScript>]
    member this.PadLeft(i: int) =
        PadLeft (As this) i

    [<Inline>]
    [<JavaScript>]
    member this.PadLeft(i: int, c: char) =
        PadLeftWith (As this) i c

    [<Inline>]
    [<JavaScript>]
    member this.PadRight(i: int) =
        PadRight (As this) i

    [<Inline>]
    [<JavaScript>]
    member this.PadRight(i: int, c: char) =
        PadRightWith (As this) i c

    [<Inline "$this.substring(0,$ix)">]
    member this.Remove(ix: int) = X<string>

    [<Inline>]
    [<JavaScript>]
    member this.Remove(ix: int, count: int) = Remove (As this) ix count

    [<Inline>]
    [<JavaScript>]
    member this.ToCharArray() = ToCharArray (As this)

    [<Inline>]
    [<JavaScript>]
    member this.Replace(subj: string, repl: string) =
        Replace (As this) subj repl

    [<Inline>]
    [<JavaScript>]
    member this.Replace(subj: char, repl: char) =
        ReplaceChar (As this) subj repl

    [<Inline>]
    [<JavaScript>]
    member this.Split([<System.ParamArray>] sep: char[]) =
        SplitChars (As this) sep  System.StringSplitOptions.RemoveEmptyEntries

    [<Inline>]
    [<JavaScript>]
    member this.Split(sep: char[], opts: System.StringSplitOptions) =
        SplitChars (As this) sep opts

    [<Inline>]
    [<JavaScript>]
    member this.Split(sep: string[], opts: System.StringSplitOptions) =
        SplitStrings (As this) sep opts

    [<Inline>]
    [<JavaScript>]
    member this.StartsWith(s: string) =
        StartsWith (As this) s

    [<Inline "$this.substring($ix)">]
    member this.Substring(ix: int) = X<string>

    [<Inline>]
    [<JavaScript>]
    member this.Substring(ix: int, ct: int) =
        Substring (As this) ix ct

    [<Inline>]
    [<JavaScript>]
    member this.ToCharArray(i: int, l: int) =
        ToCharArrayRange (As this) i l

    [<Inline "$this.toLowerCase()">]
    member this.ToLower() = X<string>

    [<Inline "$this.toUpperCase()">]
    member this.ToUpper() = X<string>

    [<Inline>]
    [<JavaScript>]
    member this.Trim() = Trim (As this)

[<JavaScript>]
let protect (s : string) =
    if s = null then "" else s

[<Inline "$strings.join($sep)">]
let join (strings: string[]) (sep: string) = X<string>

[<JavaScript>]
[<Name "collect">]
let Collect (f: char -> string) (s: string) : string =
    System.String.Concat(Array.init s.Length (fun i -> f s.[i]))

[<JavaScript>]
[<Name "concat">]
let Concat (separator: string) (strings: seq<string>) : string =
    join (Seq.toArray strings) separator

[<JavaScript>]
[<Name "exists">]
let Exists (f: char -> bool) (s: string) : bool =
    Seq.exists f (protect s)

[<JavaScript>]
[<Name "forall">]
let ForAll (f: char -> bool) (s: string) : bool =
    Seq.forall f (protect s)

[<JavaScript>]
[<Name "init">]
let Initialize (count: int) (f: int -> string) : string =
    System.String.Concat(Array.init count f)

[<JavaScript>]
[<Name "iter">]
let Iterate (f: char -> unit) (s: string) : unit =
    Seq.iter f (protect s)

[<JavaScript>]
[<Name "iteri">]
let IterateIndexed (f: int -> char -> unit) (s: string) : unit =
    Seq.iteri f (protect s)

[<JavaScript>]
[<Name "length">]
let Length (s: string) : int =
    (protect s).Length

[<JavaScript>]
[<Name "map">]
let Map (f: char -> char) (s: string) : string =
    Collect (fun x -> string (f x)) (protect s)

[<JavaScript>]
[<Name "mapi">]
let MapIndexed (f: int -> char -> char) (s: string) : string =
    System.String.Concat (Seq.toArray (Seq.mapi (fun i x -> string (f i x)) s))

[<JavaScript>]
[<Name "replicate">]
let Replicate (count: int) (s: string) : string =
    Initialize count (fun _ -> s)