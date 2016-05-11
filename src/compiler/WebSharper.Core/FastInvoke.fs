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

namespace WebSharper.Core

open System
open System.Reflection

/// Splits invocation of methods represented as `System.Reflection.MethodInfo`
/// into the static and dynamic phase, doing the binding work at the static phase.
/// A pre-bound method can be 100-1000x faster to invoke compared to using `.Invoke`
/// every time, which has to repeatedly do method binding.
module internal FastInvoke =

    let fail () =
        invalidOp "Bad arguments given to a FastInvoke method"

    [<AbstractClass>]
    type FastMethod() =
        abstract Invoke0 : unit -> obj
        abstract Invoke1 : obj -> obj
        abstract Invoke2 : obj * obj -> obj
        abstract Invoke3 : obj * obj * obj -> obj
        abstract Invoke4 : obj * obj * obj * obj -> obj
        abstract Invoke5 : obj * obj * obj * obj * obj -> obj
        abstract Invoke6 : obj * obj * obj * obj * obj * obj -> obj
        abstract Invoke7 : obj * obj * obj * obj * obj * obj * obj -> obj
        abstract InvokeN : [<ParamArray>] args: obj [] -> obj

        override this.Invoke0() = fail ()
        override this.Invoke1(_) = fail ()
        override this.Invoke2(_, _) = fail ()
        override this.Invoke3(_, _, _) = fail ()
        override this.Invoke4(_, _, _, _) = fail ()
        override this.Invoke5(_, _, _, _, _) = fail ()
        override this.Invoke6(_, _, _, _, _, _) = fail ()
        override this.Invoke7(_, _, _, _, _, _, _) = fail ()

        override this.InvokeN(xs) =
            match xs.Length with
            | 0 -> this.Invoke0()
            | 1 -> this.Invoke1(xs.[0])
            | 2 -> this.Invoke2(xs.[0], xs.[1])
            | 3 -> this.Invoke3(xs.[0], xs.[1], xs.[2])
            | 4 -> this.Invoke4(xs.[0], xs.[1], xs.[2], xs.[3])
            | 5 -> this.Invoke5(xs.[0], xs.[1], xs.[2], xs.[3], xs.[4])
            | 6 -> this.Invoke6(xs.[0], xs.[1], xs.[2], xs.[3], xs.[4], xs.[5])
            | 7 -> this.Invoke7(xs.[0], xs.[1], xs.[2], xs.[3], xs.[4], xs.[5], xs.[6])
            | _ -> fail ()

    [<AutoOpen>]
    module Implementation =

        let inline (!) x = unbox x

        type F<'R> = delegate of unit -> 'R
        type F<'T1,'R>  = delegate of 'T1 -> 'R
        type F<'T1,'T2,'R> = delegate of 'T1 * 'T2 -> 'R
        type F<'T1,'T2,'T3,'R> = delegate of 'T1 * 'T2 * 'T3 -> 'R
        type F<'T1,'T2,'T3,'T4,'R> = delegate of 'T1 * 'T2 * 'T3 * 'T4 -> 'R
        type F<'T1,'T2,'T3,'T4,'T5,'R> = delegate of 'T1 * 'T2 * 'T3  * 'T4 * 'T5 -> 'R
        type F<'T1,'T2,'T3,'T4,'T5,'T6,'R> = delegate of 'T1 * 'T2 * 'T3 * 'T4 * 'T5 * 'T6 -> 'R
        type F<'T1,'T2,'T3,'T4,'T5,'T6,'T7,'R> = delegate of 'T1 * 'T2 * 'T3 * 'T4 * 'T5 * 'T6 * 'T7 -> 'R

        type A = delegate of unit -> unit
        type A<'T1> = delegate of 'T1 -> unit
        type A<'T1,'T2> = delegate of 'T1 * 'T2 -> unit
        type A<'T1,'T2,'T3> = delegate of 'T1 * 'T2 * 'T3 -> unit
        type A<'T1,'T2,'T3,'T4> = delegate of 'T1 * 'T2 * 'T3 * 'T4 -> unit
        type A<'T1,'T2,'T3,'T4,'T5> = delegate of 'T1 * 'T2 * 'T3 * 'T4 * 'T5 -> unit
        type A<'T1,'T2,'T3,'T4,'T5,'T6> = delegate of 'T1 * 'T2 * 'T3 * 'T4 * 'T5 * 'T6 -> unit
        type A<'T1,'T2,'T3,'T4,'T5,'T6,'T7> = delegate of 'T1 * 'T2 * 'T3 * 'T4 * 'T5 * 'T6 * 'T7 -> unit

        [<AbstractClass>]
        type Factory() =
            abstract Prepare : obj -> FastMethod

        [<AbstractClass>]
        type Factory<'T>() =
            inherit Factory()
            abstract Prepare : 'T -> FastMethod

            override this.Prepare(x: obj) =
                this.Prepare(x :?> 'T)

        [<Sealed>]
        type Func0<'R>() =
            inherit Factory<F<'R>>()

            override this.Prepare(d: F<'R>) =
                {
                    new FastMethod() with
                        override this.Invoke0() =
                            box (d.Invoke())
                }

        [<Sealed>]
        type Func1<'T1,'R>() =
            inherit Factory<F<'T1,'R>>()

            override this.Prepare(d: F<'T1,'R>) =
                {
                    new FastMethod() with
                        override this.Invoke1(x1) =
                            box (d.Invoke(!x1))
                }

        [<Sealed>]
        type Func2<'T1,'T2,'R>() =
            inherit Factory<F<'T1,'T2,'R>>()

            override this.Prepare(d: F<'T1,'T2,'R>) =
                {
                    new FastMethod() with
                        override this.Invoke2(x1, x2) =
                            box (d.Invoke(!x1, !x2))
                }

        [<Sealed>]
        type Func3<'T1,'T2,'T3,'R>() =
            inherit Factory<F<'T1,'T2,'T3,'R>>()

            override this.Prepare(d: F<'T1,'T2,'T3,'R>) =
                {
                    new FastMethod() with
                        override this.Invoke3(x1, x2, x3) =
                            box (d.Invoke(!x1, !x2, !x3))
                }

        [<Sealed>]
        type Func4<'T1,'T2,'T3,'T4,'R>() =
            inherit Factory<F<'T1,'T2,'T3,'T4,'R>>()

            override this.Prepare(d: F<'T1,'T2,'T3,'T4,'R>) =
                {
                    new FastMethod() with
                        override this.Invoke4(x1, x2, x3, x4) =
                            box (d.Invoke(!x1, !x2, !x3, !x4))
                }

        [<Sealed>]
        type Func5<'T1,'T2,'T3,'T4,'T5,'R>() =
            inherit Factory<F<'T1,'T2,'T3,'T4,'T5,'R>>()

            override this.Prepare(d: F<'T1,'T2,'T3,'T4,'T5,'R>) =
                {
                    new FastMethod() with
                        override this.Invoke5(x1, x2, x3, x4, x5) =
                            box (d.Invoke(!x1, !x2, !x3, !x4, !x5))
                }

        [<Sealed>]
        type Func6<'T1,'T2,'T3,'T4,'T5,'T6,'R>() =
            inherit Factory<F<'T1,'T2,'T3,'T4,'T5,'T6,'R>>()

            override this.Prepare(d: F<'T1,'T2,'T3,'T4,'T5,'T6,'R>) =
                {
                    new FastMethod() with
                        override this.Invoke6(x1, x2, x3, x4, x5, x6) =
                            box (d.Invoke(!x1, !x2, !x3, !x4, !x5, !x6))
                }

        [<Sealed>]
        type Func7<'T1,'T2,'T3,'T4,'T5,'T6,'T7,'R>() =
            inherit Factory<F<'T1,'T2,'T3,'T4,'T5,'T6,'T7,'R>>()

            override this.Prepare(d: F<'T1,'T2,'T3,'T4,'T5,'T6,'T7,'R>) =
                {
                    new FastMethod() with
                        override this.Invoke7(x1, x2, x3, x4, x5, x6, x7) =
                            box (d.Invoke(!x1, !x2, !x3, !x4, !x5, !x6, !x7))
                }

        [<Sealed>]
        type Act0() =
            inherit Factory<A>()

            override this.Prepare(d: A) =
                {
                    new FastMethod() with
                        override this.Invoke0() =
                            d.Invoke(); null
                }

        [<Sealed>]
        type Act1<'T1>() =
            inherit Factory<A<'T1>>()

            override this.Prepare(d: A<'T1>) =
                {
                    new FastMethod() with
                        override this.Invoke1(x1) =
                            d.Invoke(!x1); null
                }

        [<Sealed>]
        type Act2<'T1,'T2>() =
            inherit Factory<A<'T1,'T2>>()

            override this.Prepare(d: A<'T1,'T2>) =
                {
                    new FastMethod() with
                        override this.Invoke2(x1, x2) =
                            d.Invoke(!x1, !x2); null
                }

        [<Sealed>]
        type Act3<'T1,'T2,'T3>() =
            inherit Factory<A<'T1,'T2,'T3>>()

            override this.Prepare(d: A<'T1,'T2,'T3>) =
                {
                    new FastMethod() with
                        override this.Invoke3(x1, x2, x3) =
                            d.Invoke(!x1, !x2, !x3); null
                }

        [<Sealed>]
        type Act4<'T1,'T2,'T3,'T4>() =
            inherit Factory<A<'T1,'T2,'T3,'T4>>()

            override this.Prepare(d: A<'T1,'T2,'T3,'T4>) =
                {
                    new FastMethod() with
                        override this.Invoke4(x1, x2, x3, x4) =
                            d.Invoke(!x1, !x2, !x3, !x4); null
                }

        [<Sealed>]
        type Act5<'T1,'T2,'T3,'T4,'T5>() =
            inherit Factory<A<'T1,'T2,'T3,'T4,'T5>>()

            override this.Prepare(d: A<'T1,'T2,'T3,'T4,'T5>) =
                {
                    new FastMethod() with
                        override this.Invoke5(x1, x2, x3, x4, x5) =
                            d.Invoke(!x1, !x2, !x3, !x4, !x5); null
                }

        [<Sealed>]
        type Act6<'T1,'T2,'T3,'T4,'T5,'T6>() =
            inherit Factory<A<'T1,'T2,'T3,'T4,'T5,'T6>>()

            override this.Prepare(d: A<'T1,'T2,'T3,'T4,'T5,'T6>) =
                {
                    new FastMethod() with
                        override this.Invoke6(x1, x2, x3, x4, x5, x6) =
                            d.Invoke(!x1, !x2, !x3, !x4, !x5, !x6); null
                }

        [<Sealed>]
        type Act7<'T1,'T2,'T3,'T4,'T5,'T6,'T7>() =
            inherit Factory<A<'T1,'T2,'T3,'T4,'T5,'T6,'T7>>()

            override this.Prepare(d: A<'T1,'T2,'T3,'T4,'T5,'T6,'T7>) =
                {
                    new FastMethod() with
                        override this.Invoke7(x1, x2, x3, x4, x5, x6, x7) =
                            d.Invoke(!x1, !x2, !x3, !x4, !x5, !x6, !x7); null
                }

    /// Generates dynamic methods for methods with a large number of arguments (N >= 8).
    module CodeGen =
        open System.Reflection.Emit

        type F = delegate of obj [] -> obj

        let Generate (m: MethodInfo) : FastMethod =
            let r = DynamicMethod("FastInvoke:" + m.Name, typeof<obj>, [| typeof<obj[]> |], true)
            let g = r.GetILGenerator()
            [|
                if not m.IsStatic then
                    yield m.DeclaringType
                for p in m.GetParameters() do
                    yield p.ParameterType
            |]
            |> Array.iteri (fun i p ->
                g.Emit(OpCodes.Ldarg_0)
                g.Emit(OpCodes.Ldc_I4, i)
                g.Emit(OpCodes.Ldelem, typeof<obj>)
                g.Emit(OpCodes.Unbox_Any, p))
            let ct = if m.IsVirtual then OpCodes.Callvirt else OpCodes.Call
            g.Emit(ct, m)
            if m.ReturnType = typeof<Void> then
                g.Emit(OpCodes.Ldnull)
                g.Emit(OpCodes.Ret)
            else
                g.Emit(OpCodes.Box, m.ReturnType)
                g.Emit(OpCodes.Ret)
            let d = r.CreateDelegate(typeof<F>) :?> F
            {
                new FastMethod() with
                    override this.InvokeN(ps) = d.Invoke(ps)
            }

    /// Compiles a method to a fast invoke function.
    let Compile (info: MethodInfo) : FastMethod =
        let ts =
            [|
                if not info.IsStatic then
                    yield info.DeclaringType
                for p in info.GetParameters() do
                    yield p.ParameterType
            |]
        let r = info.ReturnType
        let isAction = r = typeof<Void>
        let types =
            if isAction then
                match ts.Length with
                | 0 -> Some (typedefof<Act0>, typedefof<A>)
                | 1 -> Some (typedefof<Act1<_>>, typedefof<A<_>>)
                | 2 -> Some (typedefof<Act2<_,_>>, typedefof<A<_,_>>)
                | 3 -> Some (typedefof<Act3<_,_,_>>, typedefof<A<_,_,_>>)
                | 4 -> Some (typedefof<Act4<_,_,_,_>>, typedefof<A<_,_,_,_>>)
                | 5 -> Some (typedefof<Act5<_,_,_,_,_>>, typedefof<A<_,_,_,_,_>>)
                | 6 -> Some (typedefof<Act6<_,_,_,_,_,_>>, typedefof<A<_,_,_,_,_,_>>)
                | 7 -> Some (typedefof<Act7<_,_,_,_,_,_,_>>, typedefof<A<_,_,_,_,_,_,_>>)
                | _ -> None
            else
                match ts.Length with
                | 0 -> Some (typedefof<Func0<_>>, typedefof<F<_>>)
                | 1 -> Some (typedefof<Func1<_,_>>, typedefof<F<_,_>>)
                | 2 -> Some (typedefof<Func2<_,_,_>>, typedefof<F<_,_,_>>)
                | 3 -> Some (typedefof<Func3<_,_,_,_>>, typedefof<F<_,_,_,_>>)
                | 4 -> Some (typedefof<Func4<_,_,_,_,_>>, typedefof<F<_,_,_,_,_>>)
                | 5 -> Some (typedefof<Func5<_,_,_,_,_,_>>, typedefof<F<_,_,_,_,_,_>>)
                | 6 -> Some (typedefof<Func6<_,_,_,_,_,_,_>>, typedefof<F<_,_,_,_,_,_,_>>)
                | 7 -> Some (typedefof<Func7<_,_,_,_,_,_,_,_>>, typedefof<F<_,_,_,_,_,_,_,_>>)
                | _ -> None
        match types with
        | Some (iT, dT) ->
            let ts = if isAction then ts else Array.append ts [| r |]
            let dT, iT =
                if ts.Length > 0
                then dT.MakeGenericType(ts), iT.MakeGenericType(ts)
                else dT, iT
            let factory = Activator.CreateInstance iT :?> Factory
            factory.Prepare(Delegate.CreateDelegate(dT, info))
        | _ ->
            CodeGen.Generate info
