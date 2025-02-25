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

open System.Runtime.InteropServices
open WebSharper
open WebSharper.JavaScript
open WebSharper.MathJS

module M = WebSharper.Core.Macros

[<Require(typeof<WebSharper.MathJS.Resources.Js>)>]
module internal Decimal =
    [<JavaScript>]
    let WSDecimalMath: MathJS.MathInstance =
        MathJS.Math.Create(Config(Number = "BigNumber", Precision = 29., Predictable = true, Epsilon = 1e-60))

    [<JavaScript>]
    let CreateDecimal(lo: int32, mid: int32, hi: int32, isNegative: bool, scale: byte) : decimal =
        let n(x:int) = (WSDecimalMath.Bignumber x) |> As<decimal>
        if lo = 0 && hi = 0 && mid = 0 then
            n 0
        else
            let uint_sup =
                System.Decimal.Add(System.Decimal.Multiply((n 429496729), (n 10)), (n 6))
            let reinterpret (x: int) = 
                if x >= 0 then
                    n(x)
                else
                    uint_sup + (n x)
            let quotient =
                WSDecimalMath.Pow((n 10 |> As<MathNumber>), WSDecimalMath.UnaryMinus((n <| int scale )|>As<MathNumber>)) |> As<decimal>
            let value =
                (((reinterpret hi) * uint_sup + reinterpret mid) * uint_sup + reinterpret lo)
            let sign = if isNegative then (n -1) else (n 1)
            sign * value * quotient

    [<JavaScript>]
    let CreateDecimalBits (bits : int32[]) =
        if bits.Length = 4 then
            let sign = (bits.[3] &&& 0x80000000) <> 0
            let scale = As<byte> ((bits.[3] >>> 16) &&& 0x7F)
            CreateDecimal(bits.[0], bits.[1], bits.[2], sign, scale) 
        else
            invalidArg "bits" "The length of the bits array is not 4"

open Decimal

[<Require(typeof<WebSharper.MathJS.Resources.Js>)>]
[<Proxy(typeof<System.Decimal>)>]
[<Prototype(false)>]
type internal DecimalProxy =

    [<Inline>]
    static member CtorProxy(lo: int32, mid: int32, hi: int32, isNegative: bool, scale: byte) : decimal =
        CreateDecimal(lo, mid, hi, isNegative, scale) 

    [<Inline>]
    static member CtorProxy(bits: int32[]): decimal =
        CreateDecimalBits bits

    [<Inline>]
    static member private mathn (v: decimal): MathNumber = As<MathNumber> v

    [<Inline>]
    static member private un (op: MathNumber -> MathNumber) (v: decimal) = 
        DecimalProxy.mathn v
        |> op
        |> As<decimal>

    [<Inline>]
    static member private bin (op: (MathNumber * MathNumber) -> MathNumber) (v1: decimal) (v2: decimal) = 
        op (DecimalProxy.mathn v1, DecimalProxy.mathn v2)
        |> As<decimal>

    [<Inline>]
    static member private mul (op: (MathNumber * MathNumber * MathNumber []) -> MathNumber) (v1: decimal) (v2: decimal) = 
        op (DecimalProxy.mathn v1, DecimalProxy.mathn v2, [||])
        |> As<decimal>

    [<Inline>]
    static member CtorProxy(v : double) : decimal = WSDecimalMath.Bignumber(MathNumber(v)) |> As<decimal>

    [<Inline>]
    static member CtorProxy(v : int32) : decimal = WSDecimalMath.Bignumber(MathNumber(v)) |> As<decimal>

    [<Inline>]
    static member CtorProxy(v : int64) : decimal = WSDecimalMath.Bignumber(MathNumber(v.ToString())) |> As<decimal>

    [<Inline>]
    static member CtorProxy(v : single) : decimal = WSDecimalMath.Bignumber(MathNumber(v)) |> As<decimal>

    [<Inline>]
    static member CtorProxy(v : uint32) : decimal = WSDecimalMath.Bignumber(MathNumber(v)) |> As<decimal>

    [<Inline>]
    static member CtorProxy(v : uint64) : decimal = WSDecimalMath.Bignumber(MathNumber(v.ToString()) |> As<decimal>

    [<Inline>]
    static member internal Abs(n : decimal) : decimal = DecimalProxy.un WSDecimalMath.Abs n

    [<Inline>]
    static member Add(n1 : decimal, n2 : decimal) : decimal = DecimalProxy.mul WSDecimalMath.Add n1 n2

    [<Inline>]
    static member Ceiling(d: decimal) : decimal = DecimalProxy.un WSDecimalMath.Ceil d

    [<Inline>]
    static member Compare(n1 : decimal, n2 : decimal) : int = DecimalProxy.bin WSDecimalMath.Compare n1 n2 |> float |> As<int>

    [<Inline>]
    member this.CompareTo(n : decimal) : int = DecimalProxy.bin WSDecimalMath.Compare (this |> As<decimal>) n |> float |> As<int>

    [<Inline>]
    member this.CompareTo(n : obj) : int = DecimalProxy.bin WSDecimalMath.Compare (this |> As<decimal>) (n |> As<decimal>) |> float |> As<int>

    [<Inline>]
    static member Divide(n1 : decimal, n2 : decimal): decimal = DecimalProxy.bin WSDecimalMath.Divide n1 n2

    [<Inline>]
    member this.Equals(n : decimal): bool = DecimalProxy.bin WSDecimalMath.Equal (this |> As<decimal>) n |> As<bool>

    [<Inline>]
    static member Equals(a: decimal, b : decimal): bool = DecimalProxy.bin WSDecimalMath.Equal a b |> As<bool>

    [<Inline>]
    static member GreaterThan(a: decimal, b: decimal): bool = DecimalProxy.bin WSDecimalMath.Larger a b |> As<bool>

    [<Inline>]
    static member LessThan(a: decimal, b: decimal): bool = DecimalProxy.bin WSDecimalMath.Smaller a b |> As<bool>

    [<Inline>]
    static member Floor(d: decimal) : decimal = DecimalProxy.un WSDecimalMath.Floor d

    [<Inline>]
    static member internal Max(n1 : decimal, n2 : decimal): decimal =
        if n1 >= n2 then
            n1
        else
            n2

    [<Inline>]
    static member internal Min(n1 : decimal, n2 : decimal): decimal =
        if n1 <= n2 then
            n1
        else
            n2

    [<Inline>]
    static member Multiply(n1 : decimal, n2 : decimal): decimal = DecimalProxy.mul WSDecimalMath.Multiply n1 n2

    [<Inline>]
    static member Pow(n1 : decimal, n2 : decimal): decimal = DecimalProxy.bin WSDecimalMath.Pow n1 n2

    [<Inline>]
    static member Parse(s : string) = WSDecimalMath.Bignumber(MathNumber(s)) |> As<decimal>

    [<Inline>]
    static member TryParse(s: string, [<Out>] v: decimal byref) =
        try
            let x = WSDecimalMath.Bignumber(MathNumber(s)) |> As<decimal>
            v <- x
            true
        with _ ->
            false

    [<Inline>]
    static member Remainder(n1 : decimal, n2 : decimal): decimal = DecimalProxy.bin WSDecimalMath.Mod n1 n2

    [<Inline>]
    static member Subtract(n1 : decimal, n2 : decimal): decimal = DecimalProxy.bin WSDecimalMath.Subtract n1 n2

    [<Inline>]
    member internal this.Sign = DecimalProxy.un WSDecimalMath.Sign (this |> As<decimal>) |> float |> As<int>

    [<Inline>]
    static member Zero: decimal = 0m
    
    [<Inline>]
    static member op_Addition(n1 : decimal, n2 : decimal): decimal = DecimalProxy.Add (n1, n2)

    [<Inline>]
    static member op_Division(n1 : decimal, n2 : decimal): decimal = DecimalProxy.bin WSDecimalMath.Divide n1 n2

    [<Inline>]
    static member op_Equality(n1 : decimal, n2 : decimal): bool = DecimalProxy.Equals (n1,n2)

    [<Inline>]
    static member op_Inequality(n1 : decimal, n2 : decimal): bool = not <| DecimalProxy.Equals (n1,n2)

    [<Inline>]
    static member op_GreaterThan(n1:decimal, n2:decimal): bool = DecimalProxy.GreaterThan (n1, n2)

    [<Inline>]
    static member op_LessThan(n1:decimal, n2:decimal): bool = DecimalProxy.LessThan (n1, n2)

    [<Inline>]
    static member op_GreaterThanOrEqual(n1:decimal, n2:decimal): bool = not <| DecimalProxy.LessThan (n1, n2)

    [<Inline>]
    static member op_LessThanOrEqual(n1:decimal, n2:decimal): bool = not <| DecimalProxy.GreaterThan (n1, n2)

    [<Inline>]
    static member op_Modulus(n1 : decimal, n2 : decimal): decimal = DecimalProxy.bin WSDecimalMath.Mod n1 n2

    [<Inline>]
    static member op_Multiply(n1 : decimal, n2 : decimal): decimal = DecimalProxy.Multiply (n1, n2)

    [<Inline>]
    static member op_Exponentiation(n1 : decimal, n2 : decimal): decimal = DecimalProxy.Pow (n1, n2)

    [<Inline>]
    static member op_Subtraction(n1 : decimal, n2 : decimal) = DecimalProxy.Subtract (n1,n2)

    [<Inline>]
    static member op_UnaryNegation(n : decimal): decimal = DecimalProxy.un WSDecimalMath.UnaryMinus n

    [<Inline>]
    static member op_UnaryPlus(n : decimal) : decimal = DecimalProxy.un WSDecimalMath.UnaryPlus n

[<Proxy "Microsoft.FSharp.Core.LanguagePrimitives+IntrinsicFunctions, FSharp.Core">]
module internal IntrinsicFunctionProxy =

    [<Inline>]
    let MakeDecimal lo med hi isNegative scale = System.Decimal(lo,med,hi,isNegative,scale)

[<Proxy(typeof<System.Math>)>]
type private MathProxyForDecimals =

    [<Inline>]
    static member Abs(value: decimal) = DecimalProxy.Abs value

    [<Inline>]
    static member Sign(value: decimal) = (As<DecimalProxy> value).Sign

    [<Inline>]
    static member Max(n1 : decimal, n2 : decimal): decimal = DecimalProxy.Max(n1, n2)

    [<Inline>]
    static member Min(n1 : decimal, n2 : decimal): decimal = DecimalProxy.Min(n1, n2)

    [<Inline>]
    static member Ceiling(d: decimal) = DecimalProxy.Ceiling d

    [<Inline>]
    static member Floor(d: decimal) = DecimalProxy.Floor d
