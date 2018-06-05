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
[<Proxy(typeof<System.Numerics.Complex>)>]
type ComplexProxy =

    [<Inline "math.complex($r, $i)">]
    new (r : float, i : float) = {}

    member this.Imaginary
        with [<Inline "$this.im">] get () = X<float>

    //member this.Magnitude
    //    with [<Inline>] get () = Math.Abs(this)

    member this.Magnitude
        with [<Inline "math.abs($this)">] get () = X<float>

    member this.Phase
        with [<Inline "math.atan2($this.im, $this.re)">] get () = X<float>

    member this.Real
        with [<Inline "$this.re">] get () = X<float>

    [<Inline "math.abs($c)">]
    static member Abs(c : Complex) = X<float>

    [<Inline "math.acos($c)">]
    static member Acos(c : Complex) = X<Complex>

    [<Inline "math.add($c1, $c2)">]
    static member Add(c1 : Complex, c2 : Complex) = X<Complex>

    [<Inline "math.asin($c)">]
    static member Asin(c : Complex) = X<Complex>

    [<Inline "math.atan($c)">]
    static member Atan(c : Complex) = X<Complex>

    [<Inline "math.conj($c)">]
    static member Conjugate(c : Complex) = X<Complex>

    [<Inline "math.cos($c)">]
    static member Cos(c : Complex) = X<Complex>

    [<Inline "math.cosh($c)">]
    static member Cosh(c : Complex) = X<Complex>

    [<Inline "math.divide($c1, $c2)">]
    static member Divide(c1 : Complex, c2 : Complex) = X<Complex>

    [<Inline "$this.equals($c)">]
    static member Equals(c : Complex) = X<bool>

    [<Inline "math.exp(c)">]
    static member Exp(c : Complex) = X<Complex>

    [<Inline "math.complex.fromPolar($r, $i)">]
    static member FromPolarCoordinates(r : float, i : float) = X<Complex>

    [<Inline "math.log($c)">]
    static member Log(c : Complex) = X<Complex>

    [<Inline "math.log($c, $b)">]
    static member Log(c : Complex, b : float) = X<Complex>

    [<Inline "math.log10($c)">]
    static member Log10(c : Complex) = X<Complex>

    [<Inline "math.multiply($c1, $c2)">]
    static member Multiply(c1 : Complex, c2 : Complex) = X<Complex>

    [<Inline "math.unaryMinus($c)">]
    static member Negate(c : Complex) = X<Complex>

    [<Inline "math.pow($c1, $c2)">]
    static member Pow(c1 : Complex, c2 : Complex) = X<Complex>

    [<Inline "math.pow($c, $d)">]
    static member Pow(c : Complex, d : float) = X<Complex>

    [<Inline "math.sin($c)">]
    static member Sin(c : Complex) = X<Complex>

    [<Inline "math.sinh($c)">]
    static member Sinh(c : Complex) = X<Complex>

    [<Inline "math.sqrt($c)">]
    static member Sqrt(c : Complex) = X<Complex>

    [<Inline "math.subtract($c1, $c2)">]
    static member Subtract(c1 : Complex, c2 : Complex) = X<Complex>

    [<Inline "math.tan($c)">]
    static member Tan(c : Complex) = X<Complex>

    [<Inline "math.tanh($c)">]
    static member Tanh(c : Complex) = X<Complex>

    [<Inline "$this.toString()">]
    static member ToString() = X<string>

    [<Inline "$this.format($n)">]
    member x.ToString(n : string) = X<string>

    [<Inline "math.complex(0, 1)">]
    static member ImaginaryOne = X<Complex>

    [<Inline "math.complex(1, 0)">]
    static member One = X<Complex>

    [<Inline "math.complex(0, 0)">]
    static member Zero = X<Complex>

    [<Inline "math.add($c1, $c2)">]
    static member op_Addition(c1 : Complex, c2 : Complex) = X<Complex>

    [<Inline "math.divide($c1, $c2)">]
    static member op_Division(c1 : Complex, c2 : Complex) = X<Complex>

    [<Inline "$c1.equals($c2)">]
    static member op_Equality(c1 : Complex, c2 : Complex) = X<bool>

    [<Inline "math.complex($i)">]
    static member op_Explicit(i : bigint) = X<Complex>

    [<Inline "!$c1.equals($c2)">]
    static member op_Inequality(c1 : Complex, c2 : Complex) = X<bool>

    [<Inline "math.multiply($c1, $c2)">]
    static member op_Multiply(c1 : Complex, c2 : Complex) = X<Complex>

    [<Inline "math.subtract($c1, $c2)">]
    static member op_Subtraction(c1 : Complex, c2 : Complex) = X<Complex>

    [<Inline "math.unaryMinus($c)">]
    static member op_UnaryNegation(c : Complex) = X<Complex>

