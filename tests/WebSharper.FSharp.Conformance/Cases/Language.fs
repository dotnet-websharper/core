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
namespace Conformance

open System
open System.Collections.Generic
open WebSharper
open Conformance.H

exception MyError of string * int

[<Measure>] type M

[<JavaScript>]
type IShape =
    abstract Area: unit -> float
    abstract Name: string

[<JavaScript>]
[<AbstractClass>]
type Animal(name: string) =
    abstract Speak: unit -> string
    default this.Speak() = name + " makes a sound"
    member _.Name = name
    override this.ToString() = "Animal " + name

[<JavaScript>]
type Dog(name) =
    inherit Animal(name)
    override this.Speak() = name + " barks; " + base.Speak()

[<JavaScript>]
type Counter() =
    let mutable count = 0
    static let mutable created = 0
    do created <- created + 1
    member _.Incr() = count <- count + 1; count
    member _.Count with get () = count and set v = count <- v
    static member Created = created

[<JavaScript>]
type Vec(x: float, y: float) =
    member _.X = x
    member _.Y = y
    static member (+) (a: Vec, b: Vec) = Vec(a.X + b.X, a.Y + b.Y)
    static member (*) (k: float, v: Vec) = Vec(k * v.X, k * v.Y)
    override _.ToString() = "(" + string x + "," + string y + ")"

[<JavaScript>]
type Overloads() =
    member _.F(x: int) = "int"
    member _.F(x: string) = "string"
    member _.F(x: int, y: int) = "two"
    member _.G(x: int, ?y: int) = x + defaultArg y 10

[<JavaScript>]
type Disposable(log: ResizeArray<string>) =
    interface IDisposable with
        member _.Dispose() = log.Add "disposed"

[<JavaScript>]
type MaybeBuilder() =
    member _.Bind(x, f) = Option.bind f x
    member _.Return x = Some x
    member _.ReturnFrom x = x
    member _.Zero() = None

[<JavaScript>]
module Language =
    let maybe = MaybeBuilder()
    let private n (x: int) = x
    let rec private fact n = if n <= 1 then 1 else n * fact (n - 1)
    let rec private even n = if n = 0 then true else odd (n - 1)
    and private odd n = if n = 0 then false else even (n - 1)
    let rec private loop acc n = if n = 0 then acc else loop (acc + 1) (n - 1)
#if !JS
    let inline twice (x: ^a) = x + x
#endif
    let private generic (x: 'T) = [ x; x ]
    let private curry a b c = a * 100 + b * 10 + c
    let private bump (y: byref<int>) = y <- y + 1
    let private shape name area =
        { new IShape with
            member _.Area() = area
            member _.Name = name }

    let Cases () = [
        C "closure.capture" (fun () -> let fs = [ for i in 1 .. 3 -> fun () -> i ] in Ji (List.map (fun f -> f ()) fs))
        C "closure.mutable.ref" (fun () -> let r = ref 0 in let f () = r.Value <- r.Value + 1 in f (); f (); string r.Value)
        C "closure.loop.mutable" (fun () -> let fs = ResizeArray() in (for i in 0 .. 2 do fs.Add(fun () -> i * 10)); Ji (fs |> Seq.map (fun f -> f ())))
        C "currying" (fun () -> let f = curry 1 in let g = f 2 in string (g 3))
        C "composition" (fun () -> let f = ((+) 1) >> ((*) 2) in let g = ((+) 1) << ((*) 2) in string (f 3) + "," + string (g 3))
        C "pipes" (fun () -> string ((1, 2) ||> (+)) + "," + string ((1, 2, 3) |||> fun a b c -> a + b + c))
        C "recursion" (fun () -> string (fact 10) + "," + B (even 11))
        C "tail.recursion.deep" (fun () -> string (loop 0 1000000))
        C "recursion.deep.nontail" (fun () -> let rec f n = if n = 0 then 0 else 1 + f (n - 1) in string (f 10000))
#if !JS
        C "inline.srtp" (fun () -> string (twice 3) + "," + twice "ab" + "," + string (twice 1.5))
#else
        C "inline.srtp" (fun () -> "WS-COMPILE-ERROR")
#endif
        C "generic.function" (fun () -> string (List.length (generic 1)) + J (generic "a"))
        C "mutable.loop" (fun () -> let mutable s = 0 in (for i in 1 .. 10 do s <- s + i); string s)
        C "for.downto" (fun () -> let mutable s = "" in (for i = 3 downto 1 do s <- s + string i); s)
        C "while.loop" (fun () -> let mutable i = 0 in (while i < 5 do i <- i + 2); string i)
        C "for.range.step" (fun () -> let mutable s = "" in (for i in 0 .. 5 .. 20 do s <- s + string i + " "); s)
        C "if.unit" (fun () -> let mutable x = 0 in (if n 1 > 0 then x <- 1); string x)
#if !JS
        C "exception.failwith" (fun () -> try failwith "boom" with Failure m -> "failure:" + m)
#else
        C "exception.failwith" (fun () -> "WS-COMPILE-ERROR")
#endif
#if !JS
        C "exception.invalidarg" (fun () -> try invalidArg "x" "bad" with :? ArgumentException as e -> e.ParamName)
#else
        C "exception.invalidarg" (fun () -> "WS-COMPILE-ERROR")
#endif
        C "exception.invalidop" (fun () -> try invalidOp "nope" with :? InvalidOperationException as e -> e.Message)
        C "exception.custom" (fun () -> try raise (MyError ("m", 7)) with MyError (s, i) -> s + string i)
        C "exception.reraise" (fun () -> try (try failwith "inner" with _ -> reraise ()) with e -> e.Message)
        C "exception.finally" (fun () -> let mutable log = "" in (try (try failwith "x" finally log <- log + "F") with _ -> log <- log + "C"); log)
        C "exception.message.division" (fun () -> try string (1 / n 0) with e -> e.Message)
        C "exception.nullref" (fun () -> let s : string = null in string s.Length)
        C "exception.tostring" (fun () -> (ArgumentException "x").Message)
        C "exception.inner" (fun () -> let e = Exception("outer", Exception "inner") in e.InnerException.Message)
        C "use.dispose" (fun () -> let log = ResizeArray() in (using (new Disposable(log)) (fun _ -> log.Add "body")); J log)
        C "use.binding" (fun () -> let log = ResizeArray() in let f () = (use d = new Disposable(log) in log.Add "body") in f (); J log)
        C "class.members" (fun () -> let c = Counter() in c.Incr() |> ignore; c.Incr() |> ignore; c.Count <- c.Count * 10; string c.Count)
        C "class.static.let" (fun () -> let before = Counter.Created in Counter() |> ignore; string (Counter.Created - before))
        C "inheritance" (fun () -> let d = Dog "Rex" :> Animal in d.Speak())
        C "inheritance.tostring" (fun () -> string (Dog "Rex"))
        C "object.expression" (fun () -> let s = shape "sq" 2.0 in s.Name + string (s.Area()))
#if !JS
        C "object.expression.override" (fun () -> let o = { new Object() with override _.ToString() = "custom" } in string o)
#else
        C "object.expression.override" (fun () -> "WS-COMPILE-ERROR")
#endif
        C "interface.cast" (fun () -> let o = shape "x" 1.0 in match box o with :? IShape as s -> s.Name | _ -> "no")
        C "interface.cast.fail" (fun () -> let o = box (Counter()) in try (o :?> IShape).Name with :? InvalidCastException -> "cast")
        C "downcast.fail" (fun () -> let o = box "s" in try string (unbox<int> o) with :? InvalidCastException -> "cast")
        C "operator.overload" (fun () -> string (Vec(1.0, 2.0) + 2.0 * Vec(1.0, 1.0)))
        C "method.overload" (fun () -> let o = Overloads() in o.F(1) + o.F("s") + o.F(1, 2))
        C "optional.args" (fun () -> let o = Overloads() in string (o.G 1) + "," + string (o.G(1, 2)))
        C "named.args" (fun () -> let o = Overloads() in string (o.G(y = 5, x = 1)))
        C "lazy" (fun () -> let mutable n = 0 in let l = lazy (n <- n + 1; n) in string l.Value + string l.Value + B l.IsValueCreated)
        C "lazy.exception" (fun () -> let l = lazy (failwith "lz" : int) in let a = (try string l.Value with e -> e.Message) in a + "," + (try string l.Value with e -> e.Message))
        C "computation.option" (fun () -> string (maybe { let! a = Some 1 in let! b = Some 2 in return a + b }) + "," + string (maybe { let! a = None in return a + 1 }))
        C "units.of.measure" (fun () -> string (3.0<M> * 2.0))
        C "ref.cell" (fun () -> let r = ref "a" in r.Value <- r.Value + "b"; r.Value)
        C "byref" (fun () -> let mutable x = 1 in bump &x; string x)
        C "shadowing" (fun () -> let x = 1 in let x = x + 1 in string x)
        C "tuple.return" (fun () -> let divrem a b = a / b, a % b in let q, r = divrem 17 5 in string q + string r)
        C "outref.tryparse" (fun () -> let ok, v = Double.TryParse "2.5" in B ok + string v)
        C "id.ignore" (fun () -> ignore (n 5); string (id 3))
        C "nameof" (fun () -> nameof Counter + "," + nameof n)
#if !JS
        C "typeof.name" (fun () -> typeof<int>.Name)
#else
        C "typeof.name" (fun () -> "WS-COMPILE-ERROR")
#endif
        C "box.unbox" (fun () -> let o = box 3 in string (unbox<int> o + 1))
        C "isnull" (fun () -> B (isNull (null: string)) + "," + B (isNull (box 0)))
        C "default.values" (fun () -> string (Unchecked.defaultof<int>) + "," + B (isNull (Unchecked.defaultof<string>)) + "," + string (Unchecked.defaultof<float>))
        C "event" (fun () -> let e = Event<int>() in let mutable s = 0 in e.Publish.Add(fun x -> s <- s + x); e.Trigger 2; e.Trigger 3; string s)
        C "delegate" (fun () -> let d = Func<int, int>(fun x -> x * 3) in string (d.Invoke 4))
        C "action.delegate" (fun () -> let mutable s = "" in let a = Action<string>(fun x -> s <- s + x) in a.Invoke "x"; a.Invoke "y"; s)
    ]
