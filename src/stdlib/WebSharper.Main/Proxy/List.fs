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

type private ListMacro() =
    let str x = !~(J.String x)
    let int x = !~(J.Integer (int64 x))
    let (==) x y = J.Binary(x, J.BinaryOperator.``==``, y)
    let (?) x f = J.FieldGet(x, str f)
    interface M.IMacro with
        member this.Translate(q, tr) =
            let ctor name g args =
                let t = R.TypeDefinition.FromType typeof<ListProxy<_>>
                let uc : Q.Concrete<_> =
                    { Entity = R.UnionCase.Create t name; Generics = g }
                tr (Q.NewUnionCase(uc, args))
            match q with
            | Q.CallOrCallModule ({Entity = m; Generics = g}, args) ->
                match m.Name with
                | "Cons" -> ctor "Cons" g args
                | "get_Empty" -> ctor "Empty" g args
                | "get_IsEmpty" -> (tr args.[0])?("$") == int 0
                | _ -> tr q
            | Q.PropertyGet ({Entity = p; Generics = g}, args) ->
                match p.Name with
                | "Empty" -> ctor "Empty" g args
                | "IsEmpty" -> (tr args.[0])?("$") == int 0
                | _ -> tr q
            | q -> tr q

and [<Name "WebSharper.List.T">]
    [<Proxy(typeof<list<_>>)>]
    [<Macro(typeof<ListMacro>)>]
    [<RequireQualifiedAccess>]
    private ListProxy<'T> =
    | Empty
    | Cons of 'T * List<'T>

    member this.Head    with [<Inline "$this.$0">] get ()     = X<'T>
    member this.Tail    with [<Inline "$this.$1">] get ()     = X<list<'T>>

    [<JavaScript>]
    member this.Length with get () = Seq.length (As this)

    [<JavaScript>]
    member this.Item with get (x: int) : 'T = Seq.nth x (As this)

    [<JavaScript>]
    member this.GetEnumerator() =
        let data = As<list<'T>> this
        Enumerator.New data (fun e ->
            match e.State with
            | x :: xs ->
                e.Current <- x
                e.State <- xs
                true
            | [] ->
                false)

    [<JavaScript>]
    member this.GetSlice(start, finish) : list<'T> =
        match start, finish with
        | None, None -> As this
        | Some i, None -> As this |> CollectionInternals.ListSkip i
        | None, Some j -> As this |> Seq.take (j + 1) |> List.ofSeq  
        | Some i, Some j -> As this |> CollectionInternals.ListSkip i |> Seq.take (j - i + 1) |> List.ofSeq        
