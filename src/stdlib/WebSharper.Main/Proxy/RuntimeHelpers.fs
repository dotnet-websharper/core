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

[<WebSharper.Core.Attributes.Proxy
    "Microsoft.FSharp.Core.CompilerServices.RuntimeHelpers, \
     FSharp.Core, Culture=neutral, \
     PublicKeyToken=b03f5f7f11d50a3a">]
module private WebSharper.RuntimeHelpersProxy

#nowarn "40"

open WebSharper.JavaScript

type IE<'T> = System.Collections.Generic.IEnumerator<'T>

[<JavaScript>]
[<Name "WebSharper.Seq.enumFinally">]
let EnumerateThenFinally (s: seq<'T>) (f: unit -> unit) : seq<'T> =
    Enumerable.Of (fun () ->
        let e = try Enumerator.Get s with e -> f(); raise e
        Enumerator.New () (fun x ->
            try
                if e.MoveNext() then
                    x.Current <- e.Current
                    true
                else
                    f ()
                    false
            with e ->
                f ()
                raise e))

[<JavaScript>]
[<Name "WebSharper.Seq.enumUsing">]
let EnumerateUsing<'T1,'T2,'T3 when 'T1 :> System.IDisposable
                                and 'T2 :> seq<'T3>>
        (x: 'T1) (f: 'T1 -> 'T2) : seq<'T3> =
    f x :> _

[<JavaScript>]
[<Name "WebSharper.Seq.enumWhile">]
let EnumerateWhile (f: unit -> bool) (s: seq<'T>) : seq<'T> =
    Enumerable.Of (fun () ->
        let rec next (en: Enumerator.T<option<IE<_>>,'T>) =
            match en.State with
            | None ->
                if f () then
                    en.State <- Some (Enumerator.Get s)
                    next en
                else
                    false
            | Some e ->
                if e.MoveNext() then
                    en.Current <- e.Current
                    true
                else
                    en.State <- None
                    next en
        Enumerator.New None next)

[<JavaScript>]
[<Name "WebSharper.Control.createEvent">]
let CreateEvent<'A, 'D when 'D : delegate<'A, unit> and 'D :> System.Delegate> 
        (add: 'D -> unit) 
        (remove: 'D -> unit)
        (create: (obj -> 'A -> unit) -> 'D) : IEvent<'D, 'A> =
    New [
        "AddHandler" => add
        "RemoveHandler" => remove
        "Subscribe" => 
            fun (r: System.IObserver<'A>) ->
                let h = create (fun _ args -> r?OnNext(args))
                add h
                New [ "Dispose" => fun () -> remove h ] 
    ]
    
