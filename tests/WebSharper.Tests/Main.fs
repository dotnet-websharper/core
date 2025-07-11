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

module WebSharper.Tests.Main

open WebSharper
open WebSharper.Testing

[<assembly: JavaScript "WebSharper.Tests.Basis+TestOptionals">] // test for JavaScript "TypeName"
do()

[<JavaScript>]
let RunTests runServerSide autoStart =
    if not autoStart then
        JavaScript.JS.Inline "QUnit.config.autostart = false";
    Test "non-categorized test" {
        System.Console.Error.WriteLine("Hello World")
        System.Console.Out.WriteLine("Console.Out test")
        System.Console.Error.WriteLine("Console.Error num: {0}", 52)
        System.Console.Out.WriteLine("out hello {0} {1} {2} {3}", 52,55,58,63)
        System.Console.Error.WriteLine("Error record {0}", {| field1 = "SampleText"; field_2 = 128.35|})
        System.Console.Error.WriteLine(true)
        equal 1 1
    }
    Runner.RunTests [|
        AddressOf.Tests
        Array.Tests
        Array2D.Tests
        Async.Tests
        Basis.Tests
        Char.Tests
        Conversions.Tests
        DateTime.Tests
        DateTime.NativeTests
        Delegate.Tests
        Double.Tests
        BigInt.Tests
        Enum.Tests
        Event.Tests
        Exception.Tests
        Inheritance.Tests
        Integers.Tests
        Interop.Tests
        KeyValuePair.Tests
        Lazy.Tests
        List.Tests
        Macro.Tests
        Math.Tests
        Nullable.Tests
        Object.Tests
        ObjExpr.Tests
        Operators.Tests
        Option.Tests
        Optimizations.Tests
        Promise.Tests
        Proxy.Tests
        Queue.Tests
        Query.Tests
        Random.Tests
        Ref.Tests
        Reflected.Tests
        Regression.Tests
        Result.Tests
        Seq.Tests
        Stack.Tests
        String.Tests
        StringBuilder.Tests
        Task.Tests
        TimeSpan.Tests
        ValueOption.Tests
        Printf.Tests
        Tupled.Tests
        WIG.Tests
        (if runServerSide then Compiler.Tests else TestCategory "Compiler" { do () })
    |]
