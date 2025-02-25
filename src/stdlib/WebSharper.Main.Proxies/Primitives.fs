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

namespace WebSharper

open WebSharper.JavaScript

module M = WebSharper.Core.Macros

[<JavaScript>]
[<Name "Numeric">]
type internal N =
    static member Parse<'T>(s: string, min: 'T, max: 'T, overflowMsg) =
        let x : float = JS.Plus s
        if x !==. (x -. (x %. 1)) then
            raise (System.FormatException "Input string was not in a correct format.")
        elif (x <. min) || (x >. max) then
            raise (System.OverflowException overflowMsg)
        else As<'T> x

    static member TryParse<'T>(s: string, min: 'T, max: 'T, r: byref<'T>) =
        let x : float = JS.Plus s
        let ok = x ===. (x -. (x %. 1)) && (x >=. min) && (x <=. max)
        if ok then r <- As<'T> x
        ok

    static member ParseBigInt<'T>(s: string, min: 'T, max: 'T, overflowMsg) =
        let x = bigint.Parse s
        if x !==. (x -. (x %. 1L)) then
            raise (System.FormatException "Input string was not in a correct format.")
        elif (x <. min) || (x >. max) then
            raise (System.OverflowException overflowMsg)
        else As<'T> x

    static member TryParseBigInt<'T>(s: string, min: 'T, max: 'T, r: outref<'T>) =
        match bigint.TryParse s with
        | true, x ->
            let ok = x ===. (x -. (x %. 1L)) && (x >=. min) && (x <=. max)
            if ok then r <- As<'T> x
            ok
        | _ -> false

    static member ParseBool(s: string) =
        match s.ToLower() with
        | "true" -> true
        | "false" -> false
        | _ -> raise (System.FormatException "String was not recognized as a valid Boolean.")

    static member TryParseBool(s: string, r: byref<bool>) =
        match s.ToLower() with
        | "true" -> r <- true; true
        | "false" -> r <- false; true
        | _ -> false

[<Macro(typeof<M.NumericMacro>)>]
[<Proxy(typeof<System.Byte>)>]
type internal NB =

    [<Name "WebSharper.Numeric.ParseByte">]
    static member Parse(s: string) : System.Byte =
        N.Parse(s, System.Byte.MinValue, System.Byte.MaxValue, "Value was either too large or too small for an unsigned byte.")

    [<Name "WebSharper.Numeric.TryParseByte">]
    static member TryParse(s: string, r: byref<System.Byte>) : bool =
        N.TryParse(s, System.Byte.MinValue, System.Byte.MaxValue, &r)

[<Macro(typeof<M.NumericMacro>)>]
[<Proxy(typeof<System.SByte>)>]
type internal NSB =

    [<Name "WebSharper.Numeric.ParseSByte">]
    static member Parse(s: string) : System.SByte =
        N.Parse(s, System.SByte.MinValue, System.SByte.MaxValue, "Value was either too large or too small for a signed byte.")

    [<Name "WebSharper.Numeric.TryParseSByte">]
    static member TryParse(s: string, r: byref<System.SByte>) : bool =
        N.TryParse(s, System.SByte.MinValue, System.SByte.MaxValue, &r)

[<Macro(typeof<M.NumericMacro>)>]
[<Proxy(typeof<System.Int16>)>]
[<Name "Int16">]
type internal NI16 =

    [<Name "WebSharper.Numeric.ParseInt16">]
    static member Parse(s: string) : System.Int16 =
        N.Parse(s, System.Int16.MinValue, System.Int16.MaxValue, "Value was either too large or too small for an Int16.")

    [<Name "WebSharper.Numeric.TryParseInt16">]
    static member TryParse(s: string, r: byref<System.Int16>) : bool =
        N.TryParse(s, System.Int16.MinValue, System.Int16.MaxValue, &r)

[<Macro(typeof<M.NumericMacro>)>]
[<Proxy(typeof<System.Int32>)>]
[<Name "Int32">]
type internal NI32 =

    [<Name "WebSharper.Numeric.ParseInt32">]
    static member Parse(s: string) : System.Int32 =
        N.Parse(s, System.Int32.MinValue, System.Int32.MaxValue, "Value was either too large or too small for an Int32.")

    [<Name "WebSharper.Numeric.TryParseInt32">]
    static member TryParse(s: string, r: byref<System.Int32>) : bool =
        N.TryParse(s, System.Int32.MinValue, System.Int32.MaxValue, &r)

[<Macro(typeof<M.NumericMacro>)>]
[<Proxy(typeof<System.UInt16>)>]
[<Name "UInt16">]
type internal NUI16 =

    [<Name "WebSharper.Numeric.ParseUInt16">]
    static member Parse(s: string) : System.UInt16 =
        N.Parse(s, System.UInt16.MinValue, System.UInt16.MaxValue, "Value was either too large or too small for an UInt16.")

    [<Name "WebSharper.Numeric.TryParseUInt16">]
    static member TryParse(s: string, r: byref<System.UInt16>) : bool =
        N.TryParse(s, System.UInt16.MinValue, System.UInt16.MaxValue, &r)

[<Macro(typeof<M.NumericMacro>)>]
[<Proxy(typeof<System.UInt32>)>]
[<Name "UInt32">]
type internal NUI32 =

    [<Name "WebSharper.Numeric.ParseUInt32">]
    static member Parse(s: string) : System.UInt32 =
        N.Parse(s, System.UInt32.MinValue, System.UInt32.MaxValue, "Value was either too large or too small for an UInt32.")

    [<Name "WebSharper.Numeric.TryParseUInt32">]
    static member TryParse(s: string, r: byref<System.UInt32>) : bool =
        N.TryParse(s, System.UInt32.MinValue, System.UInt32.MaxValue, &r)

[<Macro(typeof<M.NumericMacro>)>]
[<Proxy(typeof<System.Int64>)>]
[<Name "Int64">]
type internal NI64 =

    [<Name "WebSharper.Numeric.ParseInt64">]
    static member Parse(s: string) : System.Int64 =
        N.ParseBigInt(s, System.Int64.MinValue, System.Int64.MaxValue, "Value was either too large or too small for an Int64.")

    [<Name "WebSharper.Numeric.TryParseInt64">]
    static member TryParse(s: string, r: outref<System.Int64>) : bool =
        N.TryParseBigInt(s, System.Int64.MinValue, System.Int64.MaxValue, &r)

[<Macro(typeof<M.NumericMacro>)>]
[<Proxy(typeof<System.UInt64>)>]
[<Name "UInt64">]
type internal NUI64 =

    [<Name "WebSharper.Numeric.ParseUInt64">]
    static member Parse(s: string) : System.UInt64 =
        N.ParseBigInt(s, System.UInt64.MinValue, System.UInt64.MaxValue, "Value was either too large or too small for an UInt64.")

    [<Name "WebSharper.Numeric.TryParseUInt64">]
    static member TryParse(s: string, r: outref<System.UInt64>) : bool =
        N.TryParseBigInt(s, System.UInt64.MinValue, System.UInt64.MaxValue, &r)

[<Macro(typeof<M.NumericMacro>)>]
[<Proxy(typeof<System.Single>)>]
type internal NS =

    [<Inline "Math.abs($0) === Infinity">]
    static member IsInfinity(f: single) = X<bool>

    [<Inline "isNaN($0)">]
    static member IsNaN(f: single) = X<bool>

    [<Inline "$0 === -Infinity">]
    static member IsNegativeInfinity (f: single) = X<bool>

    [<Inline "$0 === Infinity">]
    static member IsPositiveInfinity (f: single) = X<bool>

    [<Macro(typeof<M.NumericMacro>)>]
    static member Parse(x: string) = X<System.Single>

    [<Macro(typeof<M.NumericMacro>)>]
    static member TryParse(x: string, r: byref<System.Single>) = X<bool>

    [<Inline "$0 / $1">]
    static member DivideByInt(a: single, b: int) = X<single>

[<Macro(typeof<M.NumericMacro>)>]
[<Proxy(typeof<System.Double>)>]
type internal ND =

    [<Inline "Math.abs($0) === Infinity">]
    static member IsInfinity(f: double) = X<bool>

    [<Inline "isNaN($0)">]
    static member IsNaN(f: double) = X<bool>

    [<Inline "$0 === -Infinity">]
    static member IsNegativeInfinity (f: double) = X<bool>

    [<Inline "$0 === Infinity">]
    static member IsPositiveInfinity (f: double) = X<bool>

    [<Macro(typeof<M.NumericMacro>)>]
    static member Parse(x: string) = X<System.Double>

    [<Macro(typeof<M.NumericMacro>)>]
    static member TryParse(x: string, r: byref<System.Double>) = X<bool>

    [<Inline "$0 / $1">]
    static member DivideByInt(a: double, b: int) = X<double>

[<Proxy(typeof<System.Boolean>)>]
type internal B = 
    [<Inline>]
    static member op_LogicalNot(a: bool) = not a

    [<Inline "$this == $x">]
    member this.Equals(x: bool) = X<bool>

    [<Inline "$this === $x">]
    override this.Equals(x: obj) = X<bool>

    [<Inline "$a == $b">]
    static member op_Equality(a: bool, b: bool) = X<bool>

    [<Inline "$a != $b">]
    static member op_Inequality(a: bool, b: bool) = X<bool>

    [<Inline>]
    override this.GetHashCode() = hash this

    [<Constant "true">]
    static member TrueString = X<string>

    [<Constant "false">]
    static member FalseString = X<string>

    [<Inline>]
    override this.ToString() = string this

    [<Inline>]
    member this.CompareTo(x: bool) =
        Unchecked.compare (this :> obj) (x :> obj)

    [<Inline>]
    member this.CompareTo(x: obj) =
        Unchecked.compare (this :> obj) x

    [<Inline>]
    static member Parse(x: string) =
        N.ParseBool x

    [<Inline>]
    static member TryParse(x: string, r: byref<bool>) =
        N.TryParseBool(x, &r)
