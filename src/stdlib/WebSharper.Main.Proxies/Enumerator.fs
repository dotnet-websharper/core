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

/// Provides an `IEnumerator` implementation.
module internal WebSharper.Enumerator

open WebSharper.JavaScript
type IE<'T> = System.Collections.Generic.IEnumerator<'T>

/// Represents an unfolding enumerator.
[<Sealed; JavaScript>]
type T<'S,'T> (s: 'S, c: 'T, n: T<'S,'T> -> bool, d: T<'S,'T> -> unit) =
    let mutable e = 0
    [<Inline>]
    member this.State 
        with get() = s 
        and set (v: 'S) = this?s <- v
    [<Inline>]
    member this.Current 
        with get() = c 
        and set (v: 'T) = this?c <- v

    interface System.Collections.IEnumerator with
        member this.MoveNext() = 
            let m = n this 
            e <- if m then 1 else 2
            m
        member this.Current = 
            (As<System.Collections.Generic.IEnumerator<obj>> this).Current  
        [<JavaScript(false)>]
        member this.Reset() = ()

    interface System.Collections.Generic.IEnumerator<'T> with
        member this.Current with get() = 
            if e = 1 then
                c
            elif e = 0 then
                failwith "Enumeration has not started. Call MoveNext."
            else 
                failwith "Enumeration already finished."

    interface System.IDisposable with
        member this.Dispose() = if As d then d this

/// Constructs a new `IEnumerator` by unfolding a function.
[<Inline>]
[<JavaScript>]
let New<'S,'T> (state: 'S) (next: T<'S,'T> -> bool) =
    As<IE<'T>> (new T<'S,'T>(state, As null, next, As JS.Undefined)) 

[<Inline>]
[<JavaScript>]
let NewDisposing<'S,'T> (state: 'S) dispose (next: T<'S,'T> -> bool) =
    As<IE<'T>> (new T<'S,'T>(state, As null, next, dispose))

[<JavaScript>]
let ArrayEnumerator (s: obj[]) =
    New 0 (fun e ->
        let i = e.State
        if i < s.Length then
            e.Current <- As s.[i]
            e.State <- i + 1
            true
        else
            false)

[<JavaScript>]
let StringEnumerator (s: string) =
    New 0 (fun e ->
        let i = e.State
        if i < s.Length then
            e.Current <- As s.[i]
            e.State <- i + 1
            true
        else
            false)

[<JavaScript(JavaScriptOptions.NoDefaultInterfaceImplementation)>]
let Get (x: seq<'T>) : IE<'T> =
    if x :? System.Array then
        ArrayEnumerator (As x)
    elif JS.TypeOf x = JS.String then
        StringEnumerator (As x)
    else
        x.GetEnumerator()

[<JavaScript(JavaScriptOptions.NoDefaultInterfaceImplementation)>]
let Get0 (x: System.Collections.IEnumerable) : System.Collections.IEnumerator =
    if x :? System.Array then
        As (ArrayEnumerator (As x))
    elif JS.TypeOf x = JS.String then
        As (StringEnumerator (As x))
    elif JS.In "GetEnumerator0" x then
        x.GetEnumerator()
    else
        (As<seq<obj>> x).GetEnumerator()

[<JavaScript(JavaScriptOptions.NoDefaultInterfaceImplementation)>]
let Reset (x: System.Collections.IEnumerator) =
    if JS.In "Reset" x then
        x.Reset()
    else
        failwith "IEnumerator.Reset not supported"

[<JavaScript(JavaScriptOptions.NoDefaultInterfaceImplementation)>]
let Count (x: System.Collections.Generic.ICollection<'T>) = 
    if x :? System.Array then
        (As<obj[]> x).Length
    else 
        x.Count

[<JavaScript(JavaScriptOptions.NoDefaultInterfaceImplementation)>]
let Count0 (x: System.Collections.ICollection) = 
    if x :? System.Array then
        (As<obj[]> x).Length
    elif JS.In "Count0" x then
        x.Count
    else 
        (As<System.Collections.Generic.ICollection<obj>> x).Count

[<JavaScript>]
let ArrayCopyTo(x: System.Array) (array: System.Array) (index: int) =
    if x.Length + index < array.Length then raise (System.ArgumentException("array"))
    Array.blit (As<obj[]> x) 0 (As<obj[]> array) index x.Length

[<JavaScript(JavaScriptOptions.NoDefaultInterfaceImplementation)>]
let CopyTo (x: System.Collections.Generic.ICollection<'T>) (array: 'T[]) (index: int) =
    if x :? System.Array then
        ArrayCopyTo (As<System.Array> x) array index
    else
        x.CopyTo(array, index)

[<JavaScript(JavaScriptOptions.NoDefaultInterfaceImplementation)>]
let CopyTo0 (x: System.Collections.ICollection) (array: System.Array) (index: int) =
    if x :? System.Array then
        ArrayCopyTo (As<System.Array> x) array index
    elif JS.In "CopyTo0" x then
        x.CopyTo(array, index)
    else
        (As<System.Collections.Generic.ICollection<obj>> x).CopyTo(As<obj[]> array, index)

[<Inline>]
let IsResizable(x: obj) = JS.In "resizable" x

[<Inline>]
let IsJSReadOnly(x: obj) = JS.In "readonly" x

[<JavaScript(JavaScriptOptions.NoDefaultInterfaceImplementation)>]
let IsReadOnly (x: System.Collections.Generic.ICollection<'T>) = 
    if x :? System.Array then
        not (IsResizable x)
    else 
        x.IsReadOnly

[<JavaScript>]
let FailReadOnly() =
    failwith "Collection is read-only."

[<JavaScript(JavaScriptOptions.NoDefaultInterfaceImplementation)>]
let Add (x: System.Collections.Generic.ICollection<'T>) (item: 'T) =
    if x :? System.Array then
        if IsResizable x then
            (As<JavaScript.Array<'T>> x).Push(item) |> ignore
        else
            FailReadOnly()
    else
        if (JS.In "Add" x) then
            x.Add(item)
        else
            FailReadOnly()

[<JavaScript(JavaScriptOptions.NoDefaultInterfaceImplementation)>]
let Clear (x: System.Collections.Generic.ICollection<'T>) =
    if x :? System.Array then
        if IsResizable x then
            (As<'T[]> x).JS.Splice(0, (As<'T[]> x).Length) |> ignore   
        else
            FailReadOnly()
    else
        if (JS.In "Clear" x) then
            x.Clear()
        else
            FailReadOnly()

[<JavaScript(JavaScriptOptions.NoDefaultInterfaceImplementation)>]
let Contains (x: System.Collections.Generic.ICollection<'T>) (item: 'T) =
    if x :? System.Array then
        Array.contains (As<int> item) (As<int[]> x) // using int so that 'T is not constrained, it's erased anyways
    else
        x.Contains(item)

[<JavaScript(JavaScriptOptions.NoDefaultInterfaceImplementation)>]
let Remove (x: System.Collections.Generic.ICollection<'T>) (item: 'T) =
    if x :? System.Array then
        if IsResizable x then
            match System.Array.IndexOf(As<'T[]> x, item) with
            | -1 -> false
            | n -> (As<'T[]> x).JS.Splice(n, 1) |> ignore; true
        else
            FailReadOnly()
    else
        if (JS.In "Remove" x) then
            x.Remove(item)
        else
            FailReadOnly()

[<JavaScript(JavaScriptOptions.NoDefaultInterfaceImplementation)>]
let IsFixedSize (x: System.Collections.IList) = 
    if x :? System.Array then
        not (IsResizable x)
    else 
        x.IsReadOnly

[<JavaScript(JavaScriptOptions.NoDefaultInterfaceImplementation)>]
let LIsReadOnly (x: System.Collections.IList) = 
    if x :? System.Array then
        if IsJSReadOnly x then 
            true 
        else 
            false
    else 
        x.IsReadOnly

[<JavaScript(JavaScriptOptions.NoDefaultInterfaceImplementation)>]
let LItem0Get (x: System.Collections.IList) (index: int) = 
    if x :? System.Array then
        (As<obj[]> x)[index]
    else 
        x[index]

[<JavaScript(JavaScriptOptions.NoDefaultInterfaceImplementation)>]
let LItem0Set (x: System.Collections.IList) (index: int) (value: obj) = 
    if x :? System.Array then
        if IsJSReadOnly x then 
            FailReadOnly()
        else
            (As<obj[]> x)[index] <- value
    else 
        x[index] <- value    

[<JavaScript(JavaScriptOptions.NoDefaultInterfaceImplementation)>]
let LAdd (x: System.Collections.IList) (item: obj) : int = 
    if x :? System.Array then
        if IsResizable x then
            As<obj[]>(x).JS.Push(item) |> ignore
            As<obj[]>(x).Length - 1            
        else
            FailReadOnly()
    else 
        x.Add(item)

[<JavaScript(JavaScriptOptions.NoDefaultInterfaceImplementation)>]
let LClear (x: System.Collections.IList) = 
    if x :? System.Array then
        if IsResizable x then
            (As<obj[]> x).JS.Splice(0, (As<obj[]> x).Length) |> ignore   
        elif IsJSReadOnly x then 
            FailReadOnly()
        else
            for i = 0 to (As<obj[]> x).Length - 1 do
                (As<obj[]> x)[i] <- null
    else 
        x.Clear()

[<JavaScript(JavaScriptOptions.NoDefaultInterfaceImplementation)>]
let LContains (x: System.Collections.IList) (item: obj) = 
    if x :? System.Array then
        Array.contains (As<int> item) (As<int[]> x) // using int so that 'T is not constrained, it's erased anyways
    else 
        x.Contains(item)

[<JavaScript(JavaScriptOptions.NoDefaultInterfaceImplementation)>]
let LIndexOf0 (x: System.Collections.IList) (item: obj) = 
    if x :? System.Array then
        System.Array.IndexOf (As<obj[]> x, item)
    else 
        x.IndexOf(item)

[<JavaScript(JavaScriptOptions.NoDefaultInterfaceImplementation)>]
let LInsert0 (x: System.Collections.IList) (index: int) (value: obj) = 
    if x :? System.Array then
        if IsResizable x then
            (As<obj[]> x).JS.Splice(index, 0, value) |> ignore
        else
            FailReadOnly()
    else 
        x.Insert(index, value)

[<JavaScript(JavaScriptOptions.NoDefaultInterfaceImplementation)>]
let LRemove0 (x: System.Collections.IList) (item: obj) = 
    if x :? System.Array then
        if IsResizable x then
            match System.Array.IndexOf(As<obj[]> x, item) with
            | -1 -> ()
            | n -> (As<obj[]> x).JS.Splice(n, 1) |> ignore
        else
            FailReadOnly()
    else 
        x.Remove(item)

[<JavaScript(JavaScriptOptions.NoDefaultInterfaceImplementation)>]
let LRemoveAt0 (x: System.Collections.IList) (index: int) = 
    if x :? System.Array then
        if IsResizable x then
            (As<obj[]> x).JS.Splice(index, 1) |> ignore
        else
            FailReadOnly()
    else 
        x.RemoveAt(index)

[<JavaScript(JavaScriptOptions.NoDefaultInterfaceImplementation)>]
let LItemGet (x: System.Collections.Generic.IList<'T>) (index: int) = 
    if x :? System.Array then
        As<'T> ((As<obj[]> x)[index])
    else 
        x[index] 

[<JavaScript(JavaScriptOptions.NoDefaultInterfaceImplementation)>]
let LItemSet (x: System.Collections.Generic.IList<'T>) (index: int) (value: 'T) = 
    if x :? System.Array then
        if IsJSReadOnly x then 
            FailReadOnly()
        else
            (As<obj[]> x)[index] <- box value
    else 
        x[index] <- value    

[<JavaScript(JavaScriptOptions.NoDefaultInterfaceImplementation)>]
let LIndexOf (x: System.Collections.Generic.IList<'T>) (item: 'T) = 
    if x :? System.Array then
        System.Array.IndexOf (As<obj[]> x, item)
    else 
        x.IndexOf(item)

[<JavaScript(JavaScriptOptions.NoDefaultInterfaceImplementation)>]
let LInsert (x: System.Collections.Generic.IList<'T>) (index: int) (value: 'T) = 
    if x :? System.Array then
        if IsResizable x then
            (As<'T[]> x).JS.Splice(index, 0, value) |> ignore
        else
            FailReadOnly()
    else 
        x.Insert(index, value)

[<JavaScript(JavaScriptOptions.NoDefaultInterfaceImplementation)>]
let LRemoveAt (x: System.Collections.Generic.IList<'T>) (index: int) = 
    if x :? System.Array then
        if IsResizable x then
            (As<'T[]> x).JS.Splice(index, 1) |> ignore
        else
            FailReadOnly()
    else 
        x.RemoveAt(index)
