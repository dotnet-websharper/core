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

[<IntelliFactory.WebSharper.Core.Attributes.Proxy
    "Microsoft.FSharp.Core.LanguagePrimitives+IntrinsicFunctions, \
     FSharp.Core, Culture=neutral, \
     PublicKeyToken=b03f5f7f11d50a3a">]
module private IntelliFactory.WebSharper.IntrinsicFunctionProxy

[<Inline "$value">]
let UnboxGeneric<'T> (value: obj) = X<'T>

[<Inline "$arr[$n]">]
let GetArray<'T> (arr: 'T[]) (n:int) = X<'T>

[<Inline "void ($arr[$n] = $x)">]
let SetArray<'T> (arr: 'T[]) (n:int) (x:'T) = ()

[<Inline "$s.charCodeAt($ix)">]
let GetString (s: string) (ix: int) = X<char>

[<JavaScript>]
let GetArraySub<'T> (arr: 'T[]) start len =
    let dst = Array.zeroCreate len   
    for i = 0 to len - 1 do 
        dst.[i] <- arr.[start + 1]
    dst

[<JavaScript>]
let SetArraySub<'T> (arr: 'T[]) start len (src: 'T[]) =
    for i = 0 to len - 1 do 
        arr.[start+i] <- src.[i]

[<Inline "$arr[$n1][$n2]">]
let GetArray2D (arr: 'T[,]) (n1:int) (n2:int) = X<'T>

[<Inline "void ($arr[$n1][$n2] = $x)">]
let SetArray2D (arr: 'T[,]) (n1:int) (n2:int) (x:'T) = ()

[<Inline "$arr.length">]
let GetArray2DLength1 (arr: 'T[,]) = X<int>

[<Inline "$arr.length ? $arr[0].length : 0">]
let GetArray2DLength2 (arr: 'T[,]) =  X<int>  

[<JavaScript>]
let Array2DZeroCreate<'T> (n:int) (m:int) =
    let arr = As<'T[,]>(Array.init n (fun _ -> Array.zeroCreate m))
    arr?dims <- 2
    arr

[<JavaScript>]
let GetArray2DSub<'T> (src: 'T[,]) src1 src2 len1 len2 =
    let len1 = (if len1 < 0 then 0 else len1)
    let len2 = (if len2 < 0 then 0 else len2)
    let dst = Array2DZeroCreate len1 len2
    for i = 0 to len1 - 1 do
        for j = 0 to len2 - 1 do
            dst.[i,j] <- src.[src1 + i, src2 + j]
    dst

[<JavaScript>]
let SetArray2DSub<'T> (dst: 'T[,]) src1 src2 len1 len2 (src: 'T[,]) =
    for i = 0 to len1 - 1 do
        for j = 0 to len2 - 1 do
            dst.[src1+i, src2+j] <- src.[i, j]

[<JavaScript>]
let GetLength<'T> (arr: System.Array) =
    match arr?dims with
    | 2 -> GetArray2DLength1 (As arr) * GetArray2DLength1 (As arr)
    | _ -> Array.length (As arr)    
