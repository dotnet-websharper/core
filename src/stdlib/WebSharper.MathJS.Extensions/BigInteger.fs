// $begin{copyright}
//
// This file is part of WebSharper
//
// Copyright (c) 2008-2016 IntelliFactory
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
open WebSharper.MathJS

[<Require(typeof<WebSharper.MathJS.Resources.Js>)>]
[<Proxy(typeof<System.Numerics.BigInteger>)>]
type private BigIntegerProxy =

    [<Inline "math.bignumber($v)">]
    new (v : byte[]) = {}

    [<Inline "math.bignumber($v)">]
    new (v : decimal) = {}

    [<Inline "math.bignumber($v)">]
    new (v : double) = {}

    [<Inline "math.bignumber($v)">]
    new (v : int32) = {}

    [<Inline "math.bignumber($v)">]
    new (v : int64) = {}

    [<Inline "math.bignumber($v)">]
    new (v : single) = {}

    [<Inline "math.bignumber($v)">]
    new (v : uint32) = {}

    [<Inline "math.bignumber($v)">]
    new (v : uint64) = {}

    [<Inline "math.abs($n)">]
    static member Abs(n : bigint) = X<bigint>

    [<Inline "math.add($n1, $n2)">]
    static member Add(n1 : bigint, n2 : bigint) = X<bigint>

    [<Inline "math.compare($n1, $n2)">]
    static member Compare(n1 : bigint, n2 : bigint) = X<int>

    [<Inline "math.compare($this, $n)">]
    member this.CompareTo(n : bigint) = X<int>

    [<Inline "math.compare($this, $n)">]
    member this.CompareTo(n : int64) = X<int>

    [<Inline "math.compare($this, $n)">]
    member this.CompareTo(n : obj) = X<int>

    [<Inline "math.compare($this, $n)">]
    member this.CompareTo(n : uint64) = X<int>

    [<Inline "math.divide($n1, $n2)">]
    static member Divide(n1 : bigint, n2 : bigint) = X<bigint>

    [<Inline "math.equal($this, $n)">]
    member this.Equals(n : bigint) = X<bool>
        
    [<Inline "math.equal($this, $n)">]
    member this.Equals(n : int64) = X<bool>
        
    [<Inline "math.equal($this, $n)">]
    member this.Equals(n : obj) = X<bool>
        
    [<Inline "math.equal($this, $n)">]
    member this.Equals(n : uint64) = X<bool>

    [<Inline "math.gcd($n1, $n2)">]
    static member GreatestCommonDivisor(n1 : bigint, n2 : bigint) = X<bigint>

    [<Inline "math.log($n)">]
    static member Log(n : bigint) = X<float>

    [<Inline "math.log($n, $b)">]
    static member Log(n : bigint, b : float) = X<float>

    [<Inline "math.log10($n)">]
    static member Log10(n : bigint) = X<float>

    [<Inline "math.max($n1, $n2)">]
    static member Max(n1 : bigint, n2 : bigint) = X<bigint>

    [<Inline "math.min($n1, $n2)">]
    static member Min(n1 : bigint, n2 : bigint) = X<bigint>

    [<Inline "math.mod(math.pow($v, $e), $m)">]
    static member ModPow(v : bigint, e : bigint, m : bigint) = X<bigint>

    [<Inline "math.multiply($n1, $n2)">]
    static member Multiply(n1 : bigint, n2 : bigint) = X<bigint>

    [<Inline "math.bignumber($s)">]
    static member Parse(s : string) = X<bigint>

    [<Inline "math.pow($n1, $n2)">]
    static member Pow(n1 : bigint, n2 : int32) = X<bigint>

    [<Inline "math.mod($n1, $n2)">]
    static member Remainder(n1 : bigint, n2 : bigint) = X<bigint>

    [<Inline "math.subtract($n1, $n2)">]
    static member Subtract(n1 : bigint, n2 : bigint) = X<bigint>

    [<Inline "math.format($this)">]
    member this.ToString() = X<string>
        
    [<Inline "math.add($n1, $n2)">]
    static member op_Addition(n1 : bigint, n2 : bigint) = X<bigint>

    [<Inline "math.bitAnd($n1, $n2)">]
    static member op_BitwiseAnd(n1 : bigint, n2 : bigint) = X<bigint>

    [<Inline "math.bitOr($n1, $n2)">]
    static member op_BitwiseOr(n1 : bigint, n2 : bigint) = X<bigint>

    [<Inline "math.divide($n1, $n2)">]
    static member op_Division(n1 : bigint, n2 : bigint) = X<bigint>

    [<Inline "math.equal($n1, $n2)">]
    static member op_Equality(n1 : bigint, n2 : bigint) = X<bool>

    [<Inline "math.equal($n1, $n2)">]
    static member op_Equality(n1 : bigint, n2 : int64) = X<bool>

    [<Inline "math.equal($n1, $n2)">]
    static member op_Equality(n1 : bigint, n2 : uint64) = X<bool>

    [<Inline "math.equal($n1, $n2)">]
    static member op_Equality(n1 : int64, n2 : bigint) = X<bool>

    [<Inline "math.equal($n1, $n2)">]
    static member op_Equality(n1 : uint64, n2 : bigint) = X<bool>

    [<Inline "math.bitxor($n1, $n2)">]
    static member op_ExclusiveOr(n1 : bigint, n2 : bigint) = X<bigint>

    [<Inline "!math.equal($n1, $n2)">]
    static member op_Inequality(n1 : bigint, n2 : bigint) = X<bool>

    [<Inline "!math.equal($n1, $n2)">]
    static member op_Inequality(n1 : bigint, n2 : int64) = X<bool>

    [<Inline "!math.equal($n1, $n2)">]
    static member op_Inequality(n1 : bigint, n2 : uint64) = X<bool>

    [<Inline "!math.equal($n1, $n2)">]
    static member op_Inequality(n1 : int64, n2 : bigint) = X<bool>

    [<Inline "!math.equal($n1, $n2)">]
    static member op_Inequality(n1 : uint64, n2 : bigint) = X<bool>

    [<Inline "math.mod($n1, $n2)">]
    static member op_Modulus(n1 : bigint, n2 : bigint) = X<bigint>

    [<Inline "math.multiply($n1, $n2)">]
    static member op_Multiply(n1 : bigint, n2 : bigint) = X<bigint>

    [<Inline "math.subtract($n1, $n2)">]
    static member op_Subtraction(n1 : bigint, n2 : bigint) = X<bigint>

    [<Inline "math.unaryMinus($n)">]
    static member op_UnaryNegation(n : bigint) = X<bigint>

    [<Inline "math.unaryPlus($n)">]
    static member op_UnaryPlus(n : bigint) = X<bigint>
