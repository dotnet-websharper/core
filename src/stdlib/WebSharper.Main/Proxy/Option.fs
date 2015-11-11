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

namespace WebSharper

open WebSharper.JavaScript
module M = WebSharper.Core.Macros
module Q = WebSharper.Core.Quotations
module J = WebSharper.Core.JavaScript.Core
module R = WebSharper.Core.Reflection

type private OptionMacro() =
    let str x = !~(J.String x)
    let int x = !~(J.Integer (int64 x))
    let (==) x y = J.Binary(x, J.BinaryOperator.``==``, y)
    let (?) x f = J.FieldGet(x, str f)
    interface M.IMacro with
        member this.Translate(q, tr) =
            let t = R.TypeDefinition.FromType typeof<OptionProxy<_>>
            let ctor name g args =
                let uc : Q.Concrete<_> =
                    { Entity = R.UnionCase.Create t name; Generics = g }
                tr (Q.NewUnionCase(uc, args))
            match q with
            | Q.CallOrCallModule ({Entity = m; Generics = g}, args) ->
                match m.Name with
                | "Some" -> ctor "Some" g args
                | "get_None" -> ctor "None" g args
                | "get_IsSome" -> (tr args.[0])?("$") == int 1
                | "get_IsNone" -> (tr args.[0])?("$") == int 0
                | _ -> tr q
            | Q.PropertyGet ({Entity = p; Generics = g}, args) ->
                match p.Name with
                | "None" -> ctor "None" g args
                | "IsSome" -> (tr args.[0])?("$") == int 1
                | "IsNone" -> (tr args.[0])?("$") == int 0
                | _ -> tr q
            | q -> tr q

and [<Proxy(typeof<option<_>>)>]
    [<Name "WebSharper.Option.T">]
    [<Macro(typeof<OptionMacro>)>]
    [<RequireQualifiedAccess>]
    private OptionProxy<'T> =
    | None
    | Some of 'T

    member this.Value with [<Inline "$this.$0">] get () = X<'T>
