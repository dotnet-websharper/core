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
        if typeOf x = "bigint" || isInt && abs (toNumber x) < 1e15 then str x else general (toNumber x)
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
