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

[<IntelliFactory.WebSharper.Core.Attributes.Name "Arrays2D">]
[<IntelliFactory.WebSharper.Core.Attributes.Proxy
    "Microsoft.FSharp.Collections.Array2DModule, \
     FSharp.Core, Culture=neutral, \
     PublicKeyToken=b03f5f7f11d50a3a">]
module private IntelliFactory.WebSharper.Array2DModuleProxy

module F = IntelliFactory.WebSharper.IntrinsicFunctionProxy

[<JavaScript>]
[<Inline>]
[<Name "length1">]
let Length1 (arr: 'T[,]) = F.GetArray2DLength1 arr

[<Inline>]
[<JavaScript>]
[<Name "length2">]
let Length2 (arr: 'T[,]) = F.GetArray2DLength2 arr

[<Inline>]
[<JavaScript>]
[<Name "zeroCreate">]
let ZeroCreate (n:int) (m:int) = F.Array2DZeroCreate n m
    
[<Inline>]
[<JavaScript>]
[<Name "create">]
let Create n m (x:'T) =
    let arr = As<'T[,]>(Array.init n (fun _ -> Array.create m x))
    arr?dims <- 2
    arr
     