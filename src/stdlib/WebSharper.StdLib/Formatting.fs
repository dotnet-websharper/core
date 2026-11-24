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

/// .NET (invariant culture) string conversion and format strings for values in JavaScript,
/// used by String.Format, interpolated strings and the numeric ToString(format) overloads.
[<WebSharper.JavaScript>]
module WebSharper.Formatting

open WebSharper.JavaScript

[<Inline "typeof $x">]
let private typeOf (x: obj) = X<string>

[<Inline "$x == null">]
let private isNull (x: obj) = X<bool>

[<Inline "String($x)">]
let private str (x: obj) = X<string>

[<Inline "$s.length">]
let private len (s: string) = X<int>

[<Inline "$s.charAt($i)">]
let private charAt (s: string) (i: int) = X<string>

[<Inline "$s.substring($a, $b)">]
let private sub (s: string) (a: int) (b: int) = X<string>

[<Inline "$s.substring($a)">]
let private subFrom (s: string) (a: int) = X<string>

[<Inline "$s.indexOf($t)">]
let private indexOf (s: string) (t: string) = X<int>

[<Inline "$s.toUpperCase()">]
let private upper (s: string) = X<string>

[<Inline "$s.toLowerCase()">]
let private lower (s: string) = X<string>

[<Inline "$s.repeat($n)">]
let private repeat (s: string) (n: int) = X<string>

[<Inline "Math.abs($x)">]
let private abs (x: float) = X<float>

[<Inline "Number($x)">]
let private toNumber (x: obj) = X<float>

[<Inline "$x.toFixed($d)">]
let private toFixed (x: float) (d: int) = X<string>

[<Inline "$x.toExponential($d)">]
let private toExponential (x: float) (d: int) = X<string>

[<Inline "$x.toExponential()">]
let private toExponentialShortest (x: float) = X<string>

[<Inline "$x.toString(16)">]
let private toHex (x: obj) = X<string>

[<Inline "parseInt($s, 10)">]
let private parseIntStr (s: string) = X<int>

[<Inline "isFinite($x)">]
let private isFinite (x: float) = X<bool>

[<Inline "isNaN($x)">]
let private isNaN (x: float) = X<bool>

[<Inline "Object.is($x, -0)">]
let private isNegZero (x: float) = X<bool>

let private padStart (s: string) (n: int) (c: string) =
    if len s >= n then s else repeat c (n - len s) + s

/// Inserts "," group separators into a string of integer digits.
let private group (digits: string) =
    let mutable res = ""
    let mutable i = len digits
    while i > 3 do
        res <- "," + sub digits (i - 3) i + res
        i <- i - 3
    sub digits 0 i + res

/// .NET exponent notation: "1.5e-7" -> "1.5E-07" (at least 2 exponent digits, sign always shown).
let private dotnetExponent (s: string) (minDigits: int) (e: string) =
    let i = indexOf s "e"
    if i < 0 then s else
    let mant = sub s 0 i
    let rest = subFrom s (i + 1)
    let sign, digits =
        if charAt rest 0 = "-" then "-", subFrom rest 1
        elif charAt rest 0 = "+" then "+", subFrom rest 1
        else "+", rest
    mant + e + sign + padStart digits minDigits "0"

/// Shortest round-trip representation of a double, as .NET's default ToString() writes it.
let private general (x: float) =
    if isNaN x then "NaN"
    elif not (isFinite x) then (if x > 0.0 then "Infinity" else "-Infinity")
    elif isNegZero x then "-0"
    else
        let s = str x
        // .NET switches to exponent notation from 1E+15 and below 1E-05
        let a = abs x
        if a <> 0.0 && (a >= 1e15 || a < 1e-5) && indexOf s "e" < 0 then
            dotnetExponent (toExponentialShortest x) 2 "E"
        else dotnetExponent s 2 "E"

let private fixedDigits (x: float) (decimals: int) =
    // toFixed switches to exponent notation from 1e21
    if abs x >= 1e21 then str x else
    // .NET rounds a value exactly halfway between two results to even (2.25 -> "2.2"), toFixed rounds it up
    let scaled = x * JS.Inline("Math.pow(10, $0)", decimals)
    let isMidpoint : bool = JS.Inline("Number.isInteger($0 * 2) && !Number.isInteger($0)", scaled)
    if isMidpoint && decimals <= 15 then
        let down : float = JS.Inline("Math.floor($0)", scaled)
        let even = if JS.Inline("$0 % 2 === 0", down) then down else down + 1.0
        toFixed (even / JS.Inline("Math.pow(10, $0)", decimals)) decimals
    else toFixed x decimals

let private standardNumber (x: obj) (isInt: bool) (f: string) (precision: int option) : string option =
    let n = toNumber x
    let neg (s: string) = if n < 0.0 && charAt s 0 <> "-" then "-" + s else s
    let res =
      match f with
      | "D" | "d" when isInt ->
          let digits = str (if typeOf x = "bigint" then x else box (abs n))
          let digits = if charAt digits 0 = "-" then subFrom digits 1 else digits
          Some ((if n < 0.0 then "-" else "") + padStart digits (defaultArg precision 0) "0")
      | "X" | "x" when isInt ->
          let h =
              if typeOf x = "bigint" then
                  if n < 0.0 then JS.Inline("BigInt.asUintN(64, $0).toString(16)", x) else toHex x
              elif n < 0.0 then JS.Inline("($0 >>> 0).toString(16)", x)
              else toHex x
          let h = padStart h (defaultArg precision 0) "0"
          Some (if f = "X" then upper h else h)
      | "F" | "f" -> Some (fixedDigits n (defaultArg precision 2))
      | "N" | "n" ->
          let s = fixedDigits (abs n) (defaultArg precision 2)
          let i = indexOf s "."
          let intPart, frac = if i < 0 then s, "" else sub s 0 i, subFrom s i
          Some (group intPart + frac)
      | "P" | "p" ->
          let s = fixedDigits (abs (n * 100.0)) (defaultArg precision 2)
          let i = indexOf s "."
          let intPart, frac = if i < 0 then s, "" else sub s 0 i, subFrom s i
          Some ((if n < 0.0 then "-" else "") + group intPart + frac + " %")
      | "E" | "e" ->
          let s = toExponential n (defaultArg precision 6)
          Some (dotnetExponent s 3 (if f = "E" then "E" else "e"))
      | "C" | "c" ->
          let s = fixedDigits (abs n) (defaultArg precision 2)
          let i = indexOf s "."
          let intPart, frac = if i < 0 then s, "" else sub s 0 i, subFrom s i
          Some ((if n < 0.0 then "-" else "") + "¤" + group intPart + frac)
      | "G" | "g" | "R" | "r" ->
          match precision with
          | Some p when p > 0 && not isInt ->
              let s = JS.Inline("Number($0.toPrecision($1)).toString()", n, p)
              Some (dotnetExponent s 2 (if f = "g" then "e" else "E"))
          | _ ->
              if isInt then Some (str x) else Some (general n)
      | _ -> None
    Option.map neg res

/// Custom numeric format strings: 0 # . , % and literals ('...' or "..." quoted, \ escaped), with ; sections.
let private customNumber (x: float) (format: string) =
    let sections : string[] = JS.Inline("$0.split(';')", format)
    let section, x =
        if x < 0.0 && sections.Length > 1 && len sections.[1] > 0 then sections.[1], abs x
        elif x = 0.0 && sections.Length > 2 && len sections.[2] > 0 then sections.[2], x
        else sections.[0], x
    // scan the pattern
    let mutable intZeros = 0
    let mutable fracZeros = 0
    let mutable fracHashes = 0
    let mutable inFrac = false
    let mutable grouping = false
    let mutable percent = false
    let mutable firstPlaceholder = -1
    let mutable lastPlaceholder = -1
    let mutable i = 0
    while i < len section do
        let c = charAt section i
        if c = "'" || c = "\"" then
            let close = JS.Inline("$0.indexOf($1, $2 + 1)", section, c, i)
            i <- if close < 0 then len section else close + 1
        elif c = "\\" then i <- i + 2
        else
            if c = "0" || c = "#" then
                if firstPlaceholder < 0 then firstPlaceholder <- i
                lastPlaceholder <- i
                if inFrac then
                    if c = "0" then fracZeros <- fracZeros + 1 + fracHashes; fracHashes <- 0
                    else fracHashes <- fracHashes + 1
                elif c = "0" then intZeros <- intZeros + 1
            elif c = "." then inFrac <- true
            elif c = "," && firstPlaceholder >= 0 && not inFrac then grouping <- true
            elif c = "%" then percent <- true
            i <- i + 1
    let x = if percent then x * 100.0 else x
    let maxFrac = fracZeros + fracHashes
    let s = fixedDigits (abs x) maxFrac
    let dot = indexOf s "."
    let mutable intPart = if dot < 0 then s else sub s 0 dot
    let mutable frac = if dot < 0 then "" else subFrom s (dot + 1)
    // trim optional (#) fraction digits
    while len frac > fracZeros && charAt frac (len frac - 1) = "0" do
        frac <- sub frac 0 (len frac - 1)
    if intPart = "0" && intZeros = 0 then intPart <- ""
    intPart <- padStart intPart intZeros "0"
    if grouping then intPart <- group intPart
    let number = intPart + (if len frac > 0 then "." + frac else "")
    // literals before the first and after the last placeholder
    let literal (from: int) (until: int) =
        let mutable res = ""
        let mutable j = from
        while j < until do
            let c = charAt section j
            if c = "'" || c = "\"" then
                let close = JS.Inline("$0.indexOf($1, $2 + 1)", section, c, j)
                let close = if close < 0 then len section else close
                res <- res + sub section (j + 1) close
                j <- close + 1
            elif c = "\\" then
                res <- res + charAt section (j + 1)
                j <- j + 2
            else
                if c <> "0" && c <> "#" && c <> "." && c <> "," then res <- res + c
                j <- j + 1
        res
    let prefix = if firstPlaceholder < 0 then literal 0 (len section) else literal 0 firstPlaceholder
    let suffix = if lastPlaceholder < 0 then "" else literal (lastPlaceholder + 1) (len section)
    let sign = if x < 0.0 && sections.Length = 1 && (len number > 0 && JS.Inline("/[1-9]/.test($0)", number)) then "-" else ""
    sign + prefix + number + suffix

let private isStandardFormat (f: string) =
    JS.Inline("/^[A-Za-z]\\d{0,9}$/.test($0)", f)

/// Formats a number with a .NET numeric format string.
let FormatNumber (x: obj) (format: string) : string =
    let isInt =
        typeOf x = "bigint" || JS.Inline("Number.isInteger($0)", x)
    if isNull format || format = "" then
        if typeOf x = "bigint" then str x
        elif isNegZero (toNumber x) then "-0"
        elif isInt && abs (toNumber x) < 1e15 then str x
        else general (toNumber x)
    elif isStandardFormat format then
        let precision = if len format > 1 then Some (parseIntStr (subFrom format 1)) else None
        match standardNumber x isInt (charAt format 0) precision with
        | Some s -> s
        | None -> customNumber (toNumber x) format
    else customNumber (toNumber x) format

/// Converts a value to string like .NET's String.Format and interpolated strings do:
/// null is "", booleans are "True"/"False", numbers use .NET's format and the optional format string.
let FormatValue (x: obj) (format: string) : string =
    if isNull x then ""
    else
        match typeOf x with
        | "boolean" -> if As<bool> x then "True" else "False"
        | "number" | "bigint" -> FormatNumber x format
        | "string" -> As<string> x
        | _ -> str x

[<Inline "(() => { const r = []; for (let i = 0; i < $names.length; i += 2) r.push([$names[i], $names[i + 1]]); return r.sort((a, b) => Number(b[0]) - Number(a[0])); })()">]
let private flagPairs (names: obj[]) = X<obj[][]>

[<Inline "$parts.join(', ')">]
let private joinParts (parts: string[]) = X<string>

/// Enum.ToString: `names` is [| value0; name0; value1; name1; ... |] in declaration order.
/// A defined value gives its name; a [Flags] combination of defined values gives "A, B"; otherwise the number.
let FormatEnum (value: obj) (names: obj[]) (isFlags: bool) : string =
    let mutable res : string = null
    let mutable i = 0
    while isNull (As<obj> res) && i < names.Length do
        if JS.Inline("$0 == $1", names.[i], value) then res <- As<string> names.[i + 1]
        i <- i + 2
    if not (isNull (As<obj> res)) then res
    elif not isFlags then str value
    else
        let v = toNumber value
        if v = 0.0 then str value else
        // greedy, from the largest defined flag, like .NET
        let pairs = flagPairs names
        let mutable rest = v
        let mutable parts : string[] = [||]
        for p in pairs do
            let f = toNumber p.[0]
            if f <> 0.0 && JS.Inline("($0 & $1) === $1", rest, f) then
                rest <- JS.Inline("$0 & ~$1", rest, f)
                parts <- JS.Inline("[$1].concat($0)", parts, p.[1])
        if rest <> 0.0 || parts.Length = 0 then str value
        else joinParts parts

/// Enum.Parse / TryParse: a name, a number, or for [Flags] a comma separated list of names; null when not valid.
let TryParseEnum (s: string) (names: obj[]) (ignoreCase: bool) : obj =
    if isNull (As<obj> s) then null else
    let find (n: string) : obj =
        let n : string = JS.Inline("$0.trim()", n)
        if JS.Inline("/^[+-]?\\d+$/.test($0)", n) then box (toNumber n)
        else
            let mutable res : obj = null
            let mutable i = 0
            while isNull res && i < names.Length do
                let name = As<string> names.[i + 1]
                if name = n || ignoreCase && lower name = lower n then res <- names.[i]
                i <- i + 2
            res
    let parts : string[] = JS.Inline("$0.split(',')", s)
    let mutable acc = 0.0
    let mutable ok = parts.Length > 0
    for p in parts do
        let v = find p
        if isNull v then ok <- false
        else acc <- JS.Inline("$0 | $1", acc, toNumber v)
    if ok then box acc else null

/// Enum.Parse: like TryParseEnum, throws ArgumentException when the string is not valid.
let ParseEnum (s: string) (names: obj[]) (ignoreCase: bool) : obj =
    match TryParseEnum s names ignoreCase with
    | null -> raise (System.ArgumentException("Requested value '" + s + "' was not found."))
    | v -> v

[<Inline "new Date($ms)">]
let private date (ms: float) = X<Date>

let private pad2 (x: float) = (if x < 10.0 then "0" else "") + str x

/// TimeSpan.ToString() (a TimeSpan is milliseconds): .NET's constant format [-][d.]hh:mm:ss[.fffffff]
let FormatTimeSpan (value: float) : string =
    let neg = value < 0.0
    let a = if neg then -value else value
    let totalSeconds : float = JS.Inline("Math.floor($0 / 1000)", a)
    let ticks : float = JS.Inline("Math.round(($0 - $1 * 1000) * 1E4)", a, totalSeconds)
    let d : float = JS.Inline("Math.floor($0 / 86400)", totalSeconds)
    let h : float = JS.Inline("Math.floor($0 / 3600) % 24", totalSeconds)
    let m : float = JS.Inline("Math.floor($0 / 60) % 60", totalSeconds)
    let s : float = JS.Inline("$0 % 60", totalSeconds)
    (if neg then "-" else "")
    + (if d > 0.0 then str d + "." else "")
    + pad2 h + ":" + pad2 m + ":" + pad2 s
    + (if ticks > 0.0 then "." + padStart (str ticks) 7 "0" else "")

/// TimeSpan.ToString(format): the standard "c"/"t"/"T", "g" and "G" formats, and custom formats
/// (d..dddddddd, h, hh, m, mm, s, ss, f..fffffff, F..FFFFFFF, '...'/"..." literals, \\ escapes, %x).
let FormatTimeSpanWith (value: float) (format: string) : string =
    if isNull format || format = "" || format = "c" || format = "t" || format = "T" then FormatTimeSpan value else
    let neg = value < 0.0
    let a = if neg then -value else value
    let totalSeconds : float = JS.Inline("Math.floor($0 / 1000)", a)
    let ticks : float = JS.Inline("Math.round(($0 - $1 * 1000) * 1E4)", a, totalSeconds)
    let d : float = JS.Inline("Math.floor($0 / 86400)", totalSeconds)
    let h : float = JS.Inline("Math.floor($0 / 3600) % 24", totalSeconds)
    let m : float = JS.Inline("Math.floor($0 / 60) % 60", totalSeconds)
    let s : float = JS.Inline("$0 % 60", totalSeconds)
    let fraction (n: int) = sub (padStart (str ticks) 7 "0") 0 n
    let trimmed (n: int) : string = JS.Inline("$0.replace(/0+$/, '')", fraction n)
    match format with
    | "g" ->
        (if neg then "-" else "") + (if d > 0.0 then str d + ":" else "") + str h + ":" + pad2 m + ":" + pad2 s
        + (if ticks > 0.0 then "." + trimmed 7 else "")
    | "G" ->
        (if neg then "-" else "") + str d + ":" + pad2 h + ":" + pad2 m + ":" + pad2 s + "." + fraction 7
    | _ ->
    let format = if len format = 2 && charAt format 0 = "%" then subFrom format 1 else format
    let mutable res = ""
    let mutable i = 0
    while i < len format do
        let c = charAt format i
        let mutable n = 1
        while i + n < len format && charAt format (i + n) = c do n <- n + 1
        match c with
        | "d" -> res <- res + padStart (str d) n "0"; i <- i + n
        | "h" -> res <- res + (if n >= 2 then pad2 h else str h); i <- i + n
        | "m" -> res <- res + (if n >= 2 then pad2 m else str m); i <- i + n
        | "s" -> res <- res + (if n >= 2 then pad2 s else str s); i <- i + n
        | "f" -> res <- res + fraction n; i <- i + n
        | "F" -> res <- res + trimmed n; i <- i + n
        | "\\" ->
            res <- res + charAt format (i + 1)
            i <- i + 2
        | "'" | "\"" ->
            let close : int = JS.Inline("$0.indexOf($1, $2)", format, c, i + 1)
            if close < 0 then raise (System.FormatException("Input string was not in a correct format."))
            res <- res + sub format (i + 1) close
            i <- close + 1
        | _ -> raise (System.FormatException("Input string was not in a correct format."))
    res

/// DateTime.ToString() (a DateTime is milliseconds): .NET's invariant culture "G" format, MM/dd/yyyy HH:mm:ss
let FormatDateTime (value: float) : string =
    let d = date value
    pad2 (float (d.GetMonth() + 1)) + "/" + pad2 (float (d.GetDate())) + "/" + str (d.GetFullYear())
    + " " + pad2 (float (d.GetHours())) + ":" + pad2 (float (d.GetMinutes())) + ":" + pad2 (float (d.GetSeconds()))

/// DateOnly.ToString() (a DateOnly is the milliseconds of local midnight): invariant culture, MM/dd/yyyy
let FormatDateOnly (value: float) : string =
    let d = date value
    pad2 (float (d.GetMonth() + 1)) + "/" + pad2 (float (d.GetDate())) + "/" + str (d.GetFullYear())
