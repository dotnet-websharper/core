// $begin{copyright}
//
// This file is part of WebSharper
//
// Copyright (c) 2008-2015 IntelliFactory
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

[<WebSharper.Core.Attributes.Name "Arrays2D">]
[<WebSharper.Core.Attributes.Proxy
    "Microsoft.FSharp.Collections.Array2DModule, \
     FSharp.Core, Culture=neutral, \
     PublicKeyToken=b03f5f7f11d50a3a">]
module private WebSharper.Array2DModuleProxy

open WebSharper.JavaScript
module F = WebSharper.IntrinsicFunctionProxy

[<JavaScript>]
[<Inline>]
let Length1 (arr: 'T[,]) = F.GetArray2DLength1 arr

[<Inline>]
[<JavaScript>]
let Length2 (arr: 'T[,]) = F.GetArray2DLength2 arr

[<Inline>]
[<JavaScript>]
let Get (array: 'T[,]) (n:int) (m:int) = F.GetArray2D array n m

[<Inline>]
[<JavaScript>]
let Set (array: 'T[,]) (n:int) (m:int) (x:'T) = F.SetArray2D array n m x

[<JavaScript>]
[<Inline>]
let ZeroCreate (n:int) (m:int) = F.Array2DZeroCreate n m
    
[<Inline>]
[<JavaScript>]
let Create n m (x:'T) =
    let arr = As<'T[,]>(Array.init n (fun _ -> Array.create m x))
    arr?dims <- 2
    arr
     
[<JavaScript>]
[<Name "init">]
let Initialize n m f = 
    let array = ZeroCreate n m : 'T[,]  
    for i = 0 to n - 1 do 
        for j = 0 to m - 1 do 
            array.[i, j] <- f i j
    array

[<JavaScript>]
[<Name "iter">]
let Iterate f array = 
    let count1 = F.GetArray2DLength1 array 
    let count2 = F.GetArray2DLength2 array 
    for i = 0 to count1 - 1 do 
      for j = 0 to count2 - 1 do 
        f array.[i,j]

[<JavaScript>]
[<Name "iteri">]
let IterateIndexed (f : int -> int -> 'T -> unit) (array:'T[,]) =
    let count1 = F.GetArray2DLength1 array 
    let count2 = F.GetArray2DLength2 array 
    for i = 0 to count1 - 1 do 
      for j = 0 to count2 - 1 do 
        f i j array.[i,j]

[<JavaScript>]
[<Name "map">]
let Map f array = 
    Initialize (F.GetArray2DLength1 array) (F.GetArray2DLength2 array) (fun i j -> f array.[i,j])

[<JavaScript>]
[<Name "mapi">]
let MapIndexed f array = 
    Initialize (F.GetArray2DLength1 array) (F.GetArray2DLength2 array) (fun i j -> f i j array.[i,j])

[<JavaScript>]
[<Name "copy">]
let Copy array = 
    Initialize (F.GetArray2DLength1 array) (F.GetArray2DLength2 array) (fun i j -> array.[i,j])
