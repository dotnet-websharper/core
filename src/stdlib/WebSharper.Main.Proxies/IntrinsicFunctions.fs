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

[<WebSharper.Proxy
    "Microsoft.FSharp.Core.LanguagePrimitives+IntrinsicFunctions, \
     FSharp.Core, Culture=neutral, \
     PublicKeyToken=b03f5f7f11d50a3a">]
module private WebSharper.IntrinsicFunctionProxy

open System
open WebSharper.JavaScript
module M = WebSharper.Core.Macros

[<Inline "$value">]
let UnboxGeneric<'T> (value: obj) = X<'T>

[<Inline "$value">]
let UnboxFast<'T> (value: obj) = X<'T>

[<Macro(typeof<M.TypeTest>)>]
let TypeTestGeneric<'T> (value: obj) = X<bool>

[<Macro(typeof<M.TypeTest>)>]
let TypeTestFast<'T> (value: obj) = X<bool>

[<Inline "$arr.length"; Pure>]
let GetArray2DLength1 (arr: 'T[,]) = X<int>

[<Inline "$arr.length ? $arr[0].length : 0"; Pure>]
let GetArray2DLength2 (arr: 'T[,]) =  X<int>

[<Name "checkBounds">]
let checkBounds (arr: 'T[]) (n: int) =
    if n < 0 || n >= Array.length arr then
        failwith "Index was outside the bounds of the array."

[<Name "checkBounds2D">]
let checkBounds2D<'T> (arr: 'T[,]) (n1: int) (n2: int) =
    if n1 < 0 || n2 < 0 || n1 >= GetArray2DLength1 arr
        || n2 >= GetArray2DLength2 arr then
        raise (new IndexOutOfRangeException())

[<Name "checkRange">]

let checkRange (arr: 'T []) (start: int) (size: int) : unit =
    if (size < 0) || (start < 0) || (Array.length arr < start + size) then
        failwith "Index was outside the bounds of the array."

[<Inline "$arr[$n]"; Pure>]
let GetArrayInternal<'T> (arr: 'T[]) (n:int) = X<'T>

[<Inline "void ($arr[$n] = $x)">]
let SetArrayInternal<'T> (arr: 'T[]) (n:int) (x:'T) = ()

[<Name "set">]
let SetArray<'T> (arr: 'T[]) (n: int) (x: 'T) =
    checkBounds arr n
    SetArrayInternal arr n x

[<Inline "$s[$ix]"; Pure>]
[<Name "WebSharper.Strings.get">]
let GetString (s: string) (ix: int) = X<char>

[<Name "get"; Pure>]
let GetArray<'T> (arr: 'T[]) (n: int) =
    checkBounds arr n
    GetArrayInternal arr n

[<Inline "$x.slice($start,$start+$length)">]
let private subArray (x: 'T) start length = X<'T>

[<Name "sub"; Pure>]
let GetArraySub<'T> (arr: 'T[]) start length =
    checkRange arr start length
    subArray arr start length

[<Name "setSub" >]
let SetArraySub<'T> (arr: 'T[]) start len (src: 'T[]) =
    for i = 0 to len - 1 do
        arr.[start+i] <- src.[i]

[<Inline "$arr[$n1][$n2]"; Pure>]
let GetArray2DInternal (arr: 'T[,]) (n1:int) (n2:int) = X<'T>

[<Name "get2D"; Pure >]
let GetArray2D (arr: 'T[,]) (n1: int) (n2: int) =
    checkBounds2D arr n1 n2
    GetArray2DInternal arr n1 n2

[<Inline "void ($arr[$n1][$n2] = $x)">]
let SetArray2DInternal (arr: 'T[,]) (n1:int) (n2:int) (x:'T) = ()


[<Name "set2D" >]
let SetArray2D (arr: 'T[,]) (n1: int) (n2: int) (x: 'T) =
    checkBounds2D arr n1 n2
    SetArray2DInternal arr n1 n2 x

[<Name "zeroCreate2D"; Pure >]
let Array2DZeroCreate<'T> (n:int) (m:int) =
    let arr = As<'T[,]>(Array.init n (fun _ -> Array.zeroCreate m))
    arr?dims <- 2
    arr

[<Name "sub2D"; Pure >]
let GetArray2DSub<'T> (src: 'T[,]) src1 src2 len1 len2 =
    let len1 = (if len1 < 0 then 0 else len1)
    let len2 = (if len2 < 0 then 0 else len2)
    let dst = Array2DZeroCreate len1 len2
    for i = 0 to len1 - 1 do
        for j = 0 to len2 - 1 do
            dst.[i,j] <- src.[src1 + i, src2 + j]
    dst

[<Name "setSub2D" >]
let SetArray2DSub<'T> (dst: 'T[,]) src1 src2 len1 len2 (src: 'T[,]) =
    for i = 0 to len1 - 1 do
        for j = 0 to len2 - 1 do
            dst.[src1+i, src2+j] <- src.[i, j]

[<Name "length"; Pure >]
let GetLength<'T> (arr: System.Array) =
    match arr?dims with
    | 2 -> GetArray2DLength1 (As arr) * GetArray2DLength1 (As arr)
    | _ -> Array.length (As arr)

[<Name "checkThis">]
let CheckThis (this: 'T) =
    if this = null then
        invalidOp "The initialization of an object or value resulted in an object or value being accessed recursively before it was fully initialized."
    else this
