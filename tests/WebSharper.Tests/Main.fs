﻿// $begin{copyright}
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

module WebSharper.Tests.Main

open WebSharper
open WebSharper.Testing

[<assembly: JavaScript "WebSharper.Tests.Basis+TestOptionals">] // test for JavaScript "TypeName"
do()

[<JavaScript>]
let RunTests() =
    Test "non-categorized test" {
        equal 1 1
    }
    Runner.RunTests [|
        AddressOf.Tests
        Array.Tests
        Array2D.Tests
        Async.Tests
        Basis.Tests
        Char.Tests
        DateTime.Tests
        DateTime.NativeTests
        Delegate.Tests
        Double.Tests
        Enum.Tests
        Event.Tests
        Exception.Tests
        Inheritance.Tests
        Integers.Tests
        KeyValuePair.Tests
        Lazy.Tests
        List.Tests
        Macro.Tests
        Math.Tests
        MathJS.Tests
        Nullable.Tests
        Object.Tests
        ObjExpr.Tests
        Operators.Tests
        Option.Tests
        Optimizations.Tests
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
        Task.Tests
        TimeSpan.Tests
        Printf.Tests
        Tupled.Tests
        WIG.Tests
        Cookies.Tests
#if NET461
        Compiler.Tests
#endif
    |]
