// $begin{copyright}
//
// This file is part of WebSharper
//
// Copyright (c) 2008-2026 IntelliFactory
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

open System
open WebSharper.JavaScript

[<Name "WebSharper.Delegate">]
[<Proxy(typeof<Delegate>)>]
[<Type "Function">]
type internal DelegateProxy =

    [<Inline "$wsruntime.DelegateEqual($0, $1)">]
    override this.Equals(x: obj) = X<bool>

    [<Inline "$wsruntime.DelegateEqual($0, $1)">]
    static member op_Equality(a: Delegate, b: Delegate) = X<bool>

    [<Inline "!$wsruntime.DelegateEqual($0, $1)">]
    static member op_Inequality(a: Delegate, b: Delegate) = X<bool>

    [<Inline>]
    override this.GetHashCode() = hash this

    [<Inline "$this.apply(null, $args)">]
    member this.DynamicInvoke(args: obj[]) = X<obj>

    [<Direct "$0.$Invokes || [$0]">]
    static member InvocationList(del: Delegate) = X<Delegate[]> 
    [<Inline>]
    member this.GetInvocationList() : Delegate[] =
        DelegateProxy.InvocationList (As this)
    
    static member DelegateTarget(del) =
        if (JS.Not del) then null
        elif (JS.In "$Target" del) then del?``$Target``
        elif (JS.In "$Invokes" del) then 
            let inv = del?``$Invokes`` : (_ * _)[]
            snd inv.[inv.Length - 1]
        else null

    [<Inline>]
    member this.Target =
        DelegateProxy.DelegateTarget this

    [<Inline "$wsruntime.CreateDelegate($0)">]
    static member JSCreateDelegate(invokes: Delegate[]) = X<Delegate>

    [<Inline "$wsruntime.CombineDelegates([$0, $1])">]
    static member Combine(a: Delegate, b: Delegate) = X<Delegate>

    [<Inline "$wsruntime.CombineDelegates($0)">]
    static member Combine(delegates: Delegate[]) = X<Delegate>

    [<Inline "$wsruntime.DelegateEqual($0, $1)">]
    static member DelegateEqual(d1: Delegate, d2: Delegate) = X<bool>
                           
    /// Index of the last occurrence of `valueInv` as a contiguous part of `sourceInv`, or -1
    static member LastIndexOfInvocations(sourceInv: Delegate[], valueInv: Delegate[]) =
        let mutable res = -1
        let mutable i = sourceInv.Length - valueInv.Length
        while res = -1 && i >= 0 do
            let mutable j = 0
            while j < valueInv.Length && DelegateProxy.DelegateEqual(sourceInv.[i + j], valueInv.[j]) do
                j <- j + 1
            if j = valueInv.Length then
                res <- i
            i <- i - 1
        res

    static member Remove(source:Delegate, value: Delegate) =
        if isNull source then null
        elif isNull value then source
        else
            let sourceInv = DelegateProxy.InvocationList source
            let valueInv = DelegateProxy.InvocationList value
            let i = DelegateProxy.LastIndexOfInvocations(sourceInv, valueInv)
            if i = -1 then source
            else
                let resInv = Array.append (sourceInv.JS.Slice(0, i)) (sourceInv.JS.Slice(i + valueInv.Length))
                DelegateProxy.JSCreateDelegate(resInv)

    static member RemoveAll(source:Delegate, value: Delegate) =
        if isNull source then null
        elif isNull value then source
        else
            let valueInv = DelegateProxy.InvocationList value
            let mutable res = source
            let mutable inv = DelegateProxy.InvocationList source
            let mutable i = DelegateProxy.LastIndexOfInvocations(inv, valueInv)
            while i <> -1 do
                inv <- Array.append (inv.JS.Slice(0, i)) (inv.JS.Slice(i + valueInv.Length))
                res <- DelegateProxy.JSCreateDelegate(inv)
                i <- DelegateProxy.LastIndexOfInvocations(inv, valueInv)
            res

[<Proxy(typeof<MulticastDelegate>)>]
type internal MulticastDelegateProxy =
    
    [<Inline "$wsruntime.DelegateEqual($0, $1)">]
    override this.Equals(x: obj) = X<bool>

    [<Inline "$wsruntime.DelegateEqual($0, $1)">]
    static member op_Equality(a: MulticastDelegate, b: MulticastDelegate) = X<bool>

    [<Inline "!$wsruntime.DelegateEqual($0, $1)">]
    static member op_Inequality(a: MulticastDelegate, b: MulticastDelegate) = X<bool>

    [<Inline>]
    override this.GetHashCode() = hash this

    [<Inline>]
    member this.GetInvocationList() : Delegate[] =
        DelegateProxy.InvocationList (As this)
