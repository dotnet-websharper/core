// $begin{copyright}
//
// This file is part of WebSharper
//
// Copyright (c) 2008-2018 IntelliFactory
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

/// Defines operators and functions that are automatically available whenever
/// `WebSharper` is open.
[<AutoOpen>]
module WebSharper.JavaScript.Pervasives

open System.Runtime.CompilerServices

open WebSharper
module M = WebSharper.Core.Macros

/// Casts an object to the desired type.
[<Inline "$x">]
let As<'T> (x: obj) = X<'T>

[<Inline "$x * $y">]
let ( *. ) x y = X<obj>

[<Inline "$x / $y">]
let ( /. ) x y = X<obj>

[<Inline "$x % $y">]
let ( %. ) x y = X<obj>

[<Inline "$x + $y">]
let ( +. ) x y = X<obj>

[<Inline "$x - $y">]
let ( -. ) x y = X<obj>

[<Inline "$x << $y">]
let ( <<. ) x y = X<obj>

[<Inline "$x >> $y">]
let ( >>. ) x y = X<obj>

[<Inline "$x >>> $y">]
let ( >>>. ) x y = X<obj>

[<Inline "$x < $y">]
let ( <. ) x y = X<bool>

[<Inline "$x > $y">]
let ( >. ) x y = X<bool>

[<Inline "$x >= $y">]
let ( >=. ) x y = X<bool>

[<Inline "$x <= $y">]
let ( <=. ) x y = X<bool>

[<Inline "$x == $y">]
let ( ==. ) x y = X<bool>

[<Inline "$x === $y">]
let ( ===. ) x y = X<bool>

[<Inline "$x != $y">]
let ( !=. ) x y = X<bool>

[<Inline "$x !== $y">]
let ( !==. ) x y = X<bool>

[<Inline "$x | $y">]
let ( |. ) x y = X<obj>

[<Inline "$x & $y">]
let ( &. ) x y = X<obj>

[<Inline "$x ^ $y">]
let ( ^. ) x y = X<obj>

[<Inline "$obj[$field]">]
let ( ? ) (obj: obj) (field: string) = X<'T>

[<Inline "void ($obj[$key] = $value)">]
let ( ?<- ) (obj: obj) (key: string) (value: obj) = X<unit>

[<Inline "[$x,$y]">]
let ( => ) (x: string) (y: obj) = (x, y)

[<JavaScript>]
let private NewFromSeq<'T> (fields: seq<string * obj>) : 'T =
    let r = JS.Inline "{}"
    for (k, v) in fields do
        (?<-) r k v
    As r

/// Constructs a new object as if an object literal was used.
[<Macro(typeof<M.New>); Inline>]
let New<'T> (fields: seq<string * obj>) = NewFromSeq<'T> fields

/// Constructs an proxy to a remote object instance.
[<Constant null>]
let Remote<'T> = X<'T>

/// Gets JavaScript properties in sequence dynamically from an object.
[<JavaScript; Macro(typeof<M.GetJS>)>]
let GetJS<'T> (x: obj) (items: seq<string>) =
    let mutable x = x
    for i in items do
        x <- x?(i)
    As<'T> x

/// Erases generic parameters inside this expression during WebSharper translation.
/// You can get use this to translate `defaultof` inside a generic function.
[<Macro(typeof<M.DefaultToUndefined>); MethodImpl(MethodImplOptions.NoInlining)>]
let DefaultToUndefined<'T> (x: 'T) = x

module Optional =
    /// Converts an F# option value to a JavaScript erased option
    [<Inline>]
    let ofOption x =
        match x with
        | None -> Undefined
        | Some v -> Defined v

    /// Converts a JavaScript erased option to an F# option value
    [<Inline>]
    let toOption x =
        match x with
        | Undefined -> None
        | Defined v -> Some v

    [<Inline "$x !== undefined">]
    let isDefined x =
        match x with
        | Undefined -> false
        | Defined _ -> true

    [<Inline "$x === undefined">]
    let isUndefined x =
        match x with
        | Undefined -> true
        | Defined _ -> false

[<JavaScript>]
module DateTime =
    open WebSharper.JavaScript

    [<Inline "new Date($x)">]
    let asJSDate (x: System.DateTime) = X<Date>

    let private shortDays =
        [
            "Sun"; "Mon"; "Tue"; "Wed"; "Thu"; "Fri"; "Sat"
        ]

    let private longDays =
        [
            "Sunday"; "Monday"; "Tuesday"; "Wednesday"; "Thursday"; "Friday"; "Saturday"
        ]

    let private shortMonths =
        [
            "Jan"; "Feb"; "Mar"; "Apr"; "May"; "Jun"; "Jul"; "Aug"; "Sep"; "Oct"; "Nov"; "Dec"
        ]

    let private longMonths =
        [
            "January"; "February"; "March"; "April"; "May"; "June"; "July"; "August"; "September"; "October"; "November"; "December"
        ]

    let private padLeft (minLength: int) (x: string) =
        if x.Length < minLength then
            String.replicate (minLength - x.Length) "0" + x
        else
            x

    let private padRight (minLength: int) (x: string) =
        if x.Length < minLength then
            x + String.replicate (minLength - x.Length) "0"
        else
            x

    let private dateOffsetString (d : Date) =
        // Should this -60000.0 be applied by the caller instead?
        let offset = d.GetTimezoneOffset() * -60000.0
        let isMinus = offset < 0
        let offset = abs offset
        let hours = int (offset / 3600000.0) |> string |> padLeft 2
        let minutes = int ((offset % 3600000.0) / 60000.0) |> string |> padLeft 2
        let sign = if isMinus then "-" else "+"
        
        sign + hours + ":" + minutes

    let private parseRepeatToken(format : string) (pos : int) (patternChar : char) =
        let mutable tokenLength = 0
        let mutable internalPos = pos
        while internalPos < format.Length && format.[internalPos] = patternChar do
            internalPos <- internalPos + 1
            tokenLength <- tokenLength + 1

        tokenLength

    let private parseNextChar (format: string) (pos: int) =
        if pos >= format.Length - 1 then
            None
        else
            format.[pos + 1] |> Some

    let private parseQuotedString (format: string) (pos: int) =
        let beginPos = pos
        // Get the character used to quote the string
        let quoteChar = format.[pos]

        let mutable result = ""
        let mutable foundQuote = false
        let mutable pos = pos
        let mutable earlyBreak = false

        while (pos < format.Length && not earlyBreak) do
            pos <- pos + 1
            let currentChar = format.[pos]
            if currentChar = quoteChar then
                foundQuote <- true
                earlyBreak <- true
            elif currentChar = '\\' then
                if pos < format.Length then
                    pos <- pos + 1
                    result <- result + string format.[pos]
                else
                    // This means that '\' is the last character in the string.
                    failwith "Invalid string format"
            else
                result <- result + string currentChar

        if not foundQuote then
            // We could not find the matching quote
            failwith $"Invalid string format could not find matching quote for {quoteChar}"

        (result, pos - beginPos + 1)

    let rec dateToStringWithCustomFormat (d : Date) (format : string) =
        let mutable cursorPos = 0
        let mutable tokenLength = 0
        let mutable result = ""

        let appendToResult (s: string) =
            result <- result + s

        while cursorPos < format.Length do
            let token = format.[cursorPos]

            match token with
            | 'd' ->
                tokenLength <- parseRepeatToken format cursorPos 'd'
                cursorPos <- cursorPos + tokenLength

                match tokenLength with
                | 1 -> d.GetDate() |> string |> appendToResult
                | 2 -> d.GetDate() |> string |> padLeft 2 |> appendToResult
                | 3 -> shortDays.[d.GetDay()] |> string |> appendToResult
                | 4 | _ -> longDays.[d.GetDay()] |> string |> appendToResult
            | 'f' ->
                tokenLength <- parseRepeatToken format cursorPos 'f'
                cursorPos <- cursorPos + tokenLength

                match tokenLength with
                | 1 | 2 | 3 ->
                    let precision = 10.0 ** (3.0 - float tokenLength) |> int
                    let ms = d.GetMilliseconds()
                    (ms / precision) |> string |> padLeft tokenLength |> appendToResult
                | 4 | 5 | 6 | 7 ->
                    let ms = d.GetMilliseconds()
                    ms |> string |> padRight tokenLength |> appendToResult
                | _ -> failwith "Input string was not in a correct format."
            | 'F' ->
                tokenLength <- parseRepeatToken format cursorPos 'F'
                cursorPos <- cursorPos + tokenLength

                match tokenLength with
                | 1 | 2 | 3 ->
                    let precision = 10.0 ** (3.0 - float tokenLength) |> int 
                    let value = d.GetMilliseconds() / precision
                    if value <> 0 then
                        value |> string |> padLeft tokenLength |> appendToResult
                | 4 | 5 | 6 | 7 ->
                    let value = d.GetMilliseconds()
                    if value <> 0 then
                        value |> string |> padLeft 3 |> appendToResult
                | _ -> failwith "Input string was not in a correct format."
            | 'g' ->
                tokenLength <- parseRepeatToken format cursorPos 'g'
                cursorPos <- cursorPos + tokenLength

                // Behave like CultureInfo.InvariantCulture
                "A.D." |> appendToResult
            
            | 'h' ->
                tokenLength <- parseRepeatToken format cursorPos 'h'
                cursorPos <- cursorPos + tokenLength

                let hours = d.GetHours() % 12
                
                match tokenLength with
                | 1 -> 
                    if hours = 0 then
                        "12" 
                    else
                        hours |> string
                | 2 | _ -> 
                    if hours = 0 then
                        "12"
                    else
                        hours |> string |> padLeft 2
                |> appendToResult

            | 'H' ->
                tokenLength <- parseRepeatToken format cursorPos 'H'
                cursorPos <- cursorPos + tokenLength

                let hours = d.GetHours()
                match tokenLength with
                | 1 -> hours |> string
                | 2 | _ -> hours |> string |> padLeft 2
                |> appendToResult

            | 'K' -> 
                tokenLength <- parseRepeatToken format cursorPos 'K'
                cursorPos <- cursorPos + tokenLength

                dateOffsetString d
                |> String.replicate tokenLength
                |> appendToResult
                
            | 'm' ->
                tokenLength <- parseRepeatToken format cursorPos 'm'
                cursorPos <- cursorPos + tokenLength

                match tokenLength with
                | 1 -> d.GetMinutes() |> string
                | 2 | _ -> d.GetMinutes() |> string |> padLeft 2
                |> appendToResult

            | 'M' ->
                tokenLength <- parseRepeatToken format cursorPos 'M'
                cursorPos <- cursorPos + tokenLength

                match tokenLength with
                | 1 -> d.GetMonth() + 1 |> string
                | 2 -> d.GetMonth() + 1 |> string |> padLeft 2
                | 3 -> shortMonths.[d.GetMonth()] |> string
                | 4 | _ -> longMonths.[d.GetMonth()] |> string
                |> appendToResult

            | 's' ->
                tokenLength <- parseRepeatToken format cursorPos 's'
                cursorPos <- cursorPos + tokenLength

                match tokenLength with
                | 1 -> d.GetSeconds() |> string
                | 2 | _ -> d.GetSeconds() |> string |> padLeft 2
                |> appendToResult

            | 't' ->
                tokenLength <- parseRepeatToken format cursorPos 't'
                cursorPos <- cursorPos + tokenLength

                match tokenLength with
                | 1 -> if d.GetHours() < 12 then "A" else "P"
                | 2 | _ -> if d.GetHours() < 12 then "AM" else "PM"
                |> appendToResult

            | 'y' ->
                tokenLength <- parseRepeatToken format cursorPos 'y'
                cursorPos <- cursorPos + tokenLength

                match tokenLength with
                | 1 -> d.GetFullYear() % 100 |> string
                | 2 -> d.GetFullYear() % 100 |> string |> padLeft 2
                | _ -> d.GetFullYear() |> string |> padLeft tokenLength
                |> appendToResult
            | 'z' ->        
                tokenLength <- parseRepeatToken format cursorPos 'z'
                cursorPos <- cursorPos + tokenLength

                // WebSharper only support DateTimeKind.Local code to get the offset 
                // should be adapted if more DateTimeKind are supported in the future
                let utcOffsetText = dateOffsetString d

                let sign = utcOffsetText.Substring(0, 1)
                // Hours and minutes are always 2 digits (02, 13, etc.)
                let hours = utcOffsetText.Substring(1, 2)
                let minutes = utcOffsetText.Substring(4, 2)

                match tokenLength with
                | 1 -> 
                    // Remove leading zero if present
                    let nonLeadingZero = 
                        if hours.StartsWith("0") then hours.Substring(1) else hours

                    sign + nonLeadingZero
                | 2 -> sign + hours
                | _ -> sign + hours + ":" + minutes
                |> appendToResult

            | ':' -> 
                cursorPos <- cursorPos + 1
                ":" |> appendToResult
            | '/' -> 
                cursorPos <- cursorPos + 1
                "/" |> appendToResult
            | '\''
            | '"' -> 
                let quotedString, quotedStringLenght = parseQuotedString format cursorPos
                cursorPos <- cursorPos + quotedStringLenght
                quotedString |> appendToResult
            | '%' ->
                // Using a if statement make the code a bit more readable
                // compared to using a match statement
                let nextChar = parseNextChar format cursorPos
                if nextChar.IsSome && nextChar.Value <> '%' then
                    cursorPos <- cursorPos + 2
                    dateToStringWithCustomFormat d (string nextChar.Value) |> appendToResult
                else
                    failwith "Invalid format string"
            | '\\' ->
                match parseNextChar format cursorPos with
                | Some nextChar2 ->
                    cursorPos <- cursorPos + 2
                    string nextChar2 |> appendToResult
                | None ->
                    failwith "Invalid format string"
            | _ ->
                token |> string |> appendToResult
                cursorPos <- cursorPos + 1

        result            
    

    let DateFormatter (date: System.DateTime) (format: string) =
        let d = asJSDate date

        match format with 
        | "D" -> 
            (longDays.[d.GetDay()] |> string)
            + ", "
            + (d.GetDate() |> string |> padLeft 2)
            + " "
            + (longMonths.[d.GetMonth()] |> string)
            + " "
            + (d.GetFullYear() |> string)
        | "d" -> 
            (d.GetMonth() + 1 |> string |> padLeft 2)
            + "/"
            + (d.GetDate() |> string |> padLeft 2)
            + "/"
            + (d.GetFullYear() |> string)
        | "T" ->
            (d.GetHours() |> string |> padLeft 2)
            + ":"
            + (d.GetMinutes() |> string |> padLeft 2)
            + ":"
            + (d.GetSeconds() |> string |> padLeft 2)
        | "t" ->
            (d.GetHours() |> string |> padLeft 2)
            + ":"
            + (d.GetMinutes() |> string |> padLeft 2)
        | "O" | "o" -> 
            (d.GetFullYear() |> string)
            + "-"
            + ((d.GetMonth() + 1) |> string |> padLeft 2)
            + "-"
            + (d.GetDate() |> string |> padLeft 2)
            + "T"
            + (d.GetHours() |> string |> padLeft 2)
            + ":"
            + (d.GetMinutes() |> string |> padLeft 2)
            + ":"
            + (d.GetSeconds() |> string |> padLeft 2)
            + "."
            + (d.GetMilliseconds() |> string |> padLeft 3)
            + dateOffsetString d
        | _ -> dateToStringWithCustomFormat d format

module Union =
// {{ generated by genInterop.fsx, do not modify
    /// Converts an F# Choice value to a JavaScript erased union
    [<Inline "$x.$1">]
    let ofChoice2 (x: Choice<'T1, 'T2>) = X<Union<'T1, 'T2>>
    /// Converts a JavaScript erased union to an F# Choice value
    [<Inline>]
    let toChoice2 x =
        match x with
        | Union1Of2 v -> Choice1Of2 v
        | Union2Of2 v -> Choice2Of2 v
    /// Converts an F# Choice value to a JavaScript erased union
    [<Inline "$x.$1">]
    let ofChoice3 (x: Choice<'T1, 'T2, 'T3>) = X<Union<'T1, 'T2, 'T3>>
    /// Converts a JavaScript erased union to an F# Choice value
    [<Inline>]
    let toChoice3 x =
        match x with
        | Union1Of3 v -> Choice1Of3 v
        | Union2Of3 v -> Choice2Of3 v
        | Union3Of3 v -> Choice3Of3 v
    /// Converts an F# Choice value to a JavaScript erased union
    [<Inline "$x.$1">]
    let ofChoice4 (x: Choice<'T1, 'T2, 'T3, 'T4>) = X<Union<'T1, 'T2, 'T3, 'T4>>
    /// Converts a JavaScript erased union to an F# Choice value
    [<Inline>]
    let toChoice4 x =
        match x with
        | Union1Of4 v -> Choice1Of4 v
        | Union2Of4 v -> Choice2Of4 v
        | Union3Of4 v -> Choice3Of4 v
        | Union4Of4 v -> Choice4Of4 v
    /// Converts an F# Choice value to a JavaScript erased union
    [<Inline "$x.$1">]
    let ofChoice5 (x: Choice<'T1, 'T2, 'T3, 'T4, 'T5>) = X<Union<'T1, 'T2, 'T3, 'T4, 'T5>>
    /// Converts a JavaScript erased union to an F# Choice value
    [<Inline>]
    let toChoice5 x =
        match x with
        | Union1Of5 v -> Choice1Of5 v
        | Union2Of5 v -> Choice2Of5 v
        | Union3Of5 v -> Choice3Of5 v
        | Union4Of5 v -> Choice4Of5 v
        | Union5Of5 v -> Choice5Of5 v
    /// Converts an F# Choice value to a JavaScript erased union
    [<Inline "$x.$1">]
    let ofChoice6 (x: Choice<'T1, 'T2, 'T3, 'T4, 'T5, 'T6>) = X<Union<'T1, 'T2, 'T3, 'T4, 'T5, 'T6>>
    /// Converts a JavaScript erased union to an F# Choice value
    [<Inline>]
    let toChoice6 x =
        match x with
        | Union1Of6 v -> Choice1Of6 v
        | Union2Of6 v -> Choice2Of6 v
        | Union3Of6 v -> Choice3Of6 v
        | Union4Of6 v -> Choice4Of6 v
        | Union5Of6 v -> Choice5Of6 v
        | Union6Of6 v -> Choice6Of6 v
    /// Converts an F# Choice value to a JavaScript erased union
    [<Inline "$x.$1">]
    let ofChoice7 (x: Choice<'T1, 'T2, 'T3, 'T4, 'T5, 'T6, 'T7>) = X<Union<'T1, 'T2, 'T3, 'T4, 'T5, 'T6, 'T7>>
    /// Converts a JavaScript erased union to an F# Choice value
    [<Inline>]
    let toChoice7 x =
        match x with
        | Union1Of7 v -> Choice1Of7 v
        | Union2Of7 v -> Choice2Of7 v
        | Union3Of7 v -> Choice3Of7 v
        | Union4Of7 v -> Choice4Of7 v
        | Union5Of7 v -> Choice5Of7 v
        | Union6Of7 v -> Choice6Of7 v
        | Union7Of7 v -> Choice7Of7 v
// }}

/// The computation expression for JavaScript Promises.
[<Inline>]
let promise = Promise.Builder()
