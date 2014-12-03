// $begin{copyright}
//
// This file is part of WebSharper
//
// Copyright (c) 2008-2014 IntelliFactory
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

[<IntelliFactory.WebSharper.Core.Attributes.Name "OperatorIntrinsics">]
[<IntelliFactory.WebSharper.Core.Attributes.Proxy
    "Microsoft.FSharp.Core.Operators+OperatorIntrinsics, \
     FSharp.Core, Culture=neutral, \
     PublicKeyToken=b03f5f7f11d50a3a">]
module private IntelliFactory.WebSharper.OperatorIntrinsicsProxy 

[<Inline "$s.slice($st,$e)">]
let Slice (s: 'T) (st: int) (e: int) = X<'T>

[<Inline "$s.slice($st)">]
let SliceStart (s: 'T) (st: int) = X<'T>

[<JavaScript>]
let GetStringSlice (source: string) (start: int option) (finish: int option) =
    match start, finish with
    | Some s, Some f -> Slice source s (f + 1)
    | Some s, None -> SliceStart source s
    | None, Some f -> Slice source 0 (f + 1)
    | _ -> ""

[<JavaScript>]
let GetArraySlice<'T> (source: 'T[]) (start: int option) (finish: int option) =
    match start, finish with
    | Some s, Some f -> Slice source s (f + 1)
    | Some s, None -> SliceStart source s
    | None, Some f -> Slice source 0 (f + 1)
    | _ -> [||]

module F = IntelliFactory.WebSharper.IntrinsicFunctionProxy

[<JavaScript>]
let SetArraySlice (dst: _[]) start finish (src:_[]) = 
    let start  = (match start with None -> 0 | Some n -> n) 
    let finish = (match finish with None -> dst.Length - 1 | Some n -> n) 
    F.SetArraySub dst start (finish - start + 1) src

[<JavaScript>]
let GetArraySlice2D (arr: _[,]) start1 finish1 start2 finish2 = 
    let start1  = (match start1 with None -> 0 | Some n -> n) 
    let start2  = (match start2 with None -> 0 | Some n -> n) 
    let finish1 = (match finish1 with None -> F.GetArray2DLength1 arr - 1 | Some n -> n) 
    let finish2 = (match finish2 with None -> F.GetArray2DLength2 arr - 1 | Some n -> n) 
    let len1 = (finish1 - start1 + 1)
    let len2 = (finish2 - start2 + 1)
    F.GetArray2DSub arr start1 start2 len1 len2

[<JavaScript>]
let GetArraySlice2DFixed1 (arr: _[,]) fixed1 start2 finish2 = 
    let start2  = (match start2 with None -> 0 | Some n -> n) 
    let finish2 = (match finish2 with None -> F.GetArray2DLength2 arr - 1 | Some n -> n) 
    let len2 = (finish2 - start2 + 1)
    let dst = Array.zeroCreate len2
    for j = 0 to len2 - 1 do 
        F.SetArray dst j (F.GetArray2D arr fixed1 (start2+j))
    dst

[<JavaScript>]
let GetArraySlice2DFixed2 (arr: _[,]) start1 finish1 fixed2 = 
    let start1  = (match start1 with None -> 0 | Some n -> n) 
    let finish1 = (match finish1 with None -> F.GetArray2DLength1 arr - 1 | Some n -> n) 
    let len1 = (finish1 - start1 + 1)
    let dst = Array.zeroCreate len1
    for i = 0 to len1 - 1 do 
        F.SetArray dst i (F.GetArray2D arr (start1+i) fixed2)
    dst

[<JavaScript>]
let SetArraySlice2DFixed1 (dst: _[,]) fixed1 start2 finish2 (src:_[]) = 
    let start2  = (match start2 with None -> 0 | Some n -> n) 
    let finish2 = (match finish2 with None -> F.GetArray2DLength2 dst - 1 | Some n -> n) 
    let len2 = (finish2 - start2 + 1)
    for j = 0 to len2 - 1 do
        F.SetArray2D dst fixed1 (start2+j) (F.GetArray src j)

[<JavaScript>]
let SetArraySlice2DFixed2 (dst: _[,]) start1 finish1 fixed2 (src:_[]) = 
    let start1  = (match start1 with None -> 0 | Some n -> n) 
    let finish1 = (match finish1 with None -> F.GetArray2DLength1 dst - 1 | Some n -> n) 
    let len1 = (finish1 - start1 + 1)
    for i = 0 to len1 - 1 do
        F.SetArray2D dst (start1+i) fixed2 (F.GetArray src i)

[<JavaScript>]
let SetArraySlice2D (dst: _[,]) start1 finish1 start2 finish2 (src:_[,]) = 
    let start1  = (match start1 with None -> 0 | Some n -> n) 
    let start2  = (match start2 with None -> 0 | Some n -> n) 
    let finish1 = (match finish1 with None -> F.GetArray2DLength1 dst - 1 | Some n -> n) 
    let finish2 = (match finish2 with None -> F.GetArray2DLength2 dst - 1 | Some n -> n) 
    F.SetArray2DSub dst start1 start2 (finish1 - start1 + 1) (finish2 - start2 + 1) src
