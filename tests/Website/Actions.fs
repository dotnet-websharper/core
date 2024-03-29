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

/// Declares parts of the website that can be linked to.
module WebSharper.Tests.Website.Actions

open WebSharper

type Action =
    | [<CompiledName "/">] Home
    | [<CompiledName "/tests">] Tests
    | [<CompiledName "/consoletests">] ConsoleTests
    | [<EndPoint "/testQuery?{a}&{b}">] TestQuery of a: int * b: string
    | [<EndPoint "/testQuery2?what={w}&qwe={q}">] TestQuery2 of w: int * q: string


