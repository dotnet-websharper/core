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

open WebSharper
open WebSharper.JavaScript

[<JavaScript>]
module internal BigIntProxyHelpers =
    let ToBin (n: byte) =
        [0 .. 7]
        |> List.fold (fun (num, binstr) i ->
            if num / (int)(Math.Pow((float)2, (float)(7 - i))) = 1 then
                num % (int)(Math.Pow((float)2, (float)(7 - i))), binstr + "1"
            else
                num, binstr + "0"
        ) (int n, "")
        |> snd

    let ToBinaryStr (arr: byte[]) =
        arr
        |> Array.rev
        |> Array.map(ToBin)
        |> String.concat ""
        |> fun x -> "0b" + x

[<Proxy(typeof<System.Numerics.BigInteger>)>]
[<Type "BigInt">]
[<Prototype(false)>]
type private BigIntegerProxy =     

    [<Inline "$v"; Pure>]
    new (v: int64) = {}

    [<Inline "BigInt($v)"; Pure>]
    new (v: int32) = {}

    [<Inline "$v"; Pure>]
    new (v: uint64) = {}

    [<Inline "BigInt($v)"; Pure>]
    new (v: uint32) = {}

    [<Inline "BigInt($v)"; Pure>]
    new (v: float) = {}

    [<Inline "BigInt(0)"; Pure>]
    new () = {}

    [<Inline; Pure>]
    static member CtorProxy (v: byte[]) =
        let binString = BigIntProxyHelpers.ToBinaryStr v
        As<BigIntegerProxy> (WebSharper.JavaScript.BigInt binString)
            
    [<Inline "BigInt($s)"; Pure>]
    static member Parse(s: string) = X<bigint>

    [<Inline>]
    static member TryParse(s: string, r: byref<bigint>) : bool =
        try
            r <- bigint.Parse s
            true
        with _ -> false
    
    [<Inline "$n1 + $n2">]
    static member op_Addition(n1 : bigint, n2 : bigint) = X<bigint>

    [<Inline "$n1 * $n2">]
    static member op_Multiply(n1 : bigint, n2 : bigint) = X<bigint>

    [<Inline "$n1 - $n2">]
    static member op_Subtraction(n1 : bigint, n2 : bigint) = X<bigint>

    [<Inline "$n1 % $n2">]
    static member op_Modulus(n1 : bigint, n2 : bigint) = X<bigint>

    [<Inline "$n1 / $n2">]
    static member op_Division(n1 : bigint, n2 : bigint) = X<bigint>

    [<Inline "$n1 << $n2">]
    static member op_LeftShift(n1 : bigint, n2 : int) = X<bigint>

    [<Inline "$n1 >> $n2">]
    static member op_RightShift(n1 : bigint, n2 : int) = X<bigint>

    [<Inline "$n1 > $n2">]
    static member op_GreaterThan(n1 : bigint, n2 : bigint) = X<bool>

    [<Inline "$n1 < $n2">]
    static member op_LessThan(n1 : bigint, n2 : bigint) = X<bool>

    [<Inline "$n1 >= $n2">]
    static member op_GreaterThanOrEqual(n1 : bigint, n2 : bigint) = X<bool>

    [<Inline "$n1 <= $n2">]
    static member op_LessThanOrEqual(n1 : bigint, n2 : bigint) = X<bool>

    [<Inline "$n1 ** BigInt($n2)"; Pure>]
    static member op_Exponentiation(n1 : bigint, n2 : int) = X<bigint>

    [<Inline "$n1 ** BigInt($n2)"; Pure>]
    static member Pow(n1 : bigint, n2 : int) = X<bigint>

[<Proxy "Microsoft.FSharp.Core.NumericLiterals+NumericLiteralI, FSharp.Core">]
module private NumericLiteralIProxy =
    
    [<Inline "BigInt(0)"; Pure>]
    let FromZero<'T>() = X<'T>

    [<Inline "BigInt(1)"; Pure>]
    let FromOne<'T>() = X<'T>

    [<Inline "BigInt($v)"; Pure>]
    let FromInt32<'T>(v: int32) = X<'T>

    [<Inline "BigInt($v)"; Pure>]
    let FromInt64<'T>(v: int64) = X<'T>

    [<Inline "BigInt($v)"; Pure>]
    let FromString<'T>(v: string) = X<'T>

    [<Inline "BigInt($v)"; Pure>]
    let FromInt64Dynamic(v: int64) = X<obj>

    [<Inline "BigInt($v)"; Pure>]
    let FromStringDynamic(v: string) = X<obj>