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

module IntelliFactory.WebSharper.Tests.WIG

open IntelliFactory.WebSharper
open IntelliFactory.WebSharper.JavaScript
open IntelliFactory.WebSharper.Testing
open IntelliFactory.WebSharper.InterfaceGenerator.Tests

[<JavaScript>]
let Tests =
    Section "Interface generator"

    Test "Functions" {
        WIGtest.ArgsFuncIn(fun (a, b) -> a + b) =? 3    
        WIGtest.ArgsFuncOut()(1, 2) =? 3
        let x = JustX(5)       
        WIGtest.GetGetThis().Bind(x)() =? x
        WIGtest.FuncInWithThis(FuncWithThis(fun (t: JustX) () -> string t.X)) =? "0"
        WIGtest.ArgFuncInWithThis(FuncWithThis(fun (t: JustX) a -> string t.X + string a)) =? "01"
        WIGtest.ArgsFuncInWithThis(FuncWithThis(fun (t: JustX) (a, b) -> string t.X + string a + string b)) =? "012"
        WIGtest.TupledFuncInWithThis(FuncWithThis(fun (t: JustX) (a, b) -> string t.X + string a + string b)) =? "012"
    }
