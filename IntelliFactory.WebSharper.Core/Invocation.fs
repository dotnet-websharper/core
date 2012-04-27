// $begin{copyright}
//
// This file is part of WebSharper
//
// Copyright (c) 2008-2012 IntelliFactory
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

module IntelliFactory.WebSharper.Core.Invocation

type Func               = list<obj> -> option<obj>

type F<'R>              = delegate of unit -> 'R
type F<'T1,'R>          = delegate of 'T1 -> 'R
type F<'T1,'T2,'R>      = delegate of 'T1 * 'T2 -> 'R
type F<'T1,'T2,'T3,'R>  = delegate of 'T1 * 'T2 * 'T3 -> 'R

type A                  = delegate of unit -> unit
type A<'T1>             = delegate of 'T1 -> unit
type A<'T1,'T2>         = delegate of 'T1 * 'T2 -> unit
type A<'T1,'T2,'T3>     = delegate of 'T1 * 'T2 * 'T3 -> unit

type I =
    abstract member Init : System.Delegate -> unit
    abstract member Func : Func

[<Sealed>]
type FX<'R>() =
    let mutable f = Unchecked.defaultof<F<'R>>
    interface I with
        override this.Init d = f <- d :?> _
        override this.Func =
            function
            | [] -> Some (f.Invoke() :> obj)
            | _  -> None

[<Sealed>]
type FX<'T1,'R>() =
    let mutable f = Unchecked.defaultof<F<'T1,'R>>
    interface I with
        override this.Init d = f <- d :?> _
        override this.Func = function
            | [:? 'T1 as x] -> Some (f.Invoke(x) :> obj)
            | _ -> None

[<Sealed>]
type FX<'T1,'T2,'R>() =
    let mutable f = Unchecked.defaultof<F<'T1,'T2,'R>>
    interface I with
        override this.Init d = f <- d :?> _
        override this.Func = function
            | [:? 'T1 as x; :? 'T2 as y] -> Some (f.Invoke(x, y) :> obj)
            | _ -> None

[<Sealed>]
type FX<'T1,'T2,'T3,'R>() =
    let mutable f = Unchecked.defaultof<F<'T1,'T2,'T3,'R>>
    interface I with
        override this.Init d = f <- d :?> _
        override this.Func = function
            | [:? 'T1 as x; :? 'T2 as y; :? 'T3 as z] ->
                Some (f.Invoke(x, y, z) :> obj)
            | _ -> None

[<Sealed>]
type AX() =
    let mutable f = Unchecked.defaultof<A>
    interface I with
        override this.Init d = f <- d :?> _
        override this.Func = function
            | [] -> f.Invoke(); Some null
            | _ -> None

[<Sealed>]
type AX<'T1>() =
    let mutable f = Unchecked.defaultof<A<'T1>>
    interface I with
        override this.Init d = f <- d :?> _
        override this.Func = function
            | [:? 'T1 as x] -> f.Invoke x; Some null
            | _ -> None

[<Sealed>]
type AX<'T1,'T2>() =
    let mutable f = Unchecked.defaultof<A<'T1,'T2>>
    interface I with
        override this.Init d = f <- d :?> _
        override this.Func = function
            | [:? 'T1 as x; :? 'T2 as y] ->
                f.Invoke(x, y); Some null
            | _ -> None

[<Sealed>]
type AX<'T1,'T2,'T3>() =
    let mutable f = Unchecked.defaultof<A<'T1,'T2,'T3>>
    interface I with
        override this.Init d = f <- d :?> _
        override this.Func = function
            | [:? 'T1 as x; :? 'T2 as y; :? 'T3 as z] ->
                f.Invoke(x, y, z); Some null
            | _ -> None

/// Compiles a method to a fast invoke function.
let Compile (info: System.Reflection.MethodInfo) : obj -> obj [] -> option<obj> =
    let ts =
        [
            if not info.IsStatic then
                yield info.DeclaringType
            for p in info.GetParameters() do
                yield p.ParameterType
        ]
    let r = info.ReturnType
    let types =
        if r = typeof<System.Void> || r = typeof<unit> then
            match ts with
            | [] ->
                let dT  = typeof<A>
                let iT  = typedefof<AX>
                Some (iT, dT)
            | [t1] ->
                let dT  = typedefof<A<_>>.MakeGenericType(t1)
                let iT  = typedefof<AX<_>>.MakeGenericType(t1)
                Some (iT, dT)
            | [t1; t2] ->
                let dT  = typedefof<A<_,_>>.MakeGenericType(t1, t2)
                let iT  = typedefof<AX<_,_>>.MakeGenericType(t1, t2)
                Some (iT, dT)
            | [t1; t2; t3] ->
                let dT  = typedefof<A<_,_,_>>.MakeGenericType(t1, t2, t3)
                let iT  = typedefof<AX<_,_,_>>.MakeGenericType(t1, t2, t3)
                Some (iT, dT)
            | _ ->
                None
        else
            match ts with
            | [] ->
                let dT  = typedefof<F<_>>.MakeGenericType [| r |]
                let iT  = typedefof<FX<_>>.MakeGenericType [| r |]
                Some (iT, dT)
            | [t1] ->
                let dT  = typedefof<F<_,_>>.MakeGenericType(t1, r)
                let iT  = typedefof<FX<_,_>>.MakeGenericType(t1, r)
                Some (iT, dT)
            | [t1; t2] ->
                let dT  = typedefof<F<_,_,_>>.MakeGenericType(t1, t2, r)
                let iT  = typedefof<FX<_,_,_>>.MakeGenericType(t1, t2, r)
                Some (iT, dT)
            | [t1; t2; t3] ->
                let dT  = typedefof<F<_,_,_,_>>.MakeGenericType(t1, t2, t3, r)
                let iT  = typedefof<FX<_,_,_,_>>.MakeGenericType(t1, t2, t3, r)
                Some (iT, dT)
            | _ ->
                None
    match types with
    | Some (iT, dT) ->
        let i = System.Activator.CreateInstance iT :?> I
        i.Init (System.Delegate.CreateDelegate(dT, info))
        if info.IsStatic then
            fun _ args -> i.Func (List.ofArray args)
        else
            fun t args -> i.Func (t :: List.ofArray args)
    | _ ->
        let k = ts.Length
        fun t xs -> try Some (info.Invoke(t, xs)) with _ -> None
