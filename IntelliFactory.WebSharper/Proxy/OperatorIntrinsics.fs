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
