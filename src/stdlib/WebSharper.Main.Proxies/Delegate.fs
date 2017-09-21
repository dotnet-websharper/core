// $begin{copyright}
//
// This file is part of WebSharper
//
// Copyright (c) 2008-2016 IntelliFactory
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
type internal DelegateProxy =

    [<Inline "$wsruntime.DelegateEqual($0, $1)">]
    member this.Equals(x: obj) = X<bool>

    [<Inline "$wsruntime.DelegateEqual($0, $1)">]
    static member op_Equality(a: Delegate, b: Delegate) = X<bool>

    [<Inline "!$wsruntime.DelegateEqual($0, $1)">]
    static member op_Inequality(a: Delegate, b: Delegate) = X<bool>

    [<Inline>]
    member this.GetHashCode() = hash this

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
                           
    static member Remove(source:Delegate, value: Delegate) =
        let sourceInv = source.GetInvocationList()
        if value.GetInvocationList().Length > 1 then
            failwith "TODO: Remove multicast delegates"
        let resInv = [||]
        let mutable found = false
        for i = sourceInv.Length - 1 downto 0 do
            let it = sourceInv.[i]
            if not found && DelegateProxy.DelegateEqual(it, value) then
                found <- true
            else
                resInv.JS.Unshift(it) |> ignore
        DelegateProxy.JSCreateDelegate(resInv)         

    static member RemoveAll(source:Delegate, value: Delegate) =
        let sourceInv = source.GetInvocationList()
        if value.GetInvocationList().Length > 1 then
            failwith "TODO: Remove multicast delegates"
        DelegateProxy.JSCreateDelegate(sourceInv |> Array.filter (fun i -> not (i.Equals(value))))         

[<Proxy(typeof<MulticastDelegate>)>]
type internal MulticastDelegateProxy =
    
    [<Inline "$wsruntime.DelegateEqual($0, $1)">]
    member this.Equals(x: obj) = X<bool>

    [<Inline "$wsruntime.DelegateEqual($0, $1)">]
    static member op_Equality(a: Delegate, b: Delegate) = X<bool>

    [<Inline "!$wsruntime.DelegateEqual($0, $1)">]
    static member op_Inequality(a: Delegate, b: Delegate) = X<bool>

    [<Inline>]
    member this.GetHashCode() = hash this

    [<Inline>]
    member this.GetInvocationList() : Delegate[] =
        DelegateProxy.InvocationList (As this)
