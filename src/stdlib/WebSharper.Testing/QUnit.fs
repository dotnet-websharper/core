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

open WebSharper.JavaScript

[<Require(typeof<Testing.Resources.QUnit>)>]
module QUnit =

    [<Stub>]
    type Asserter =

        [<Stub; Name "ok">]
        member this.Ok(value: bool) = X<unit>

        [<Stub; Name "ok">]
        member this.Ok(value: bool, message: string) = X<unit>

        [<Stub; Name "notOk">]
        member this.NotOk(value: bool) = X<unit>

        [<Stub; Name "notOk">]
        member this.NotOk(value: bool, message: string) = X<unit>

        [<Stub; Name "equal">]
        member this.Equal<'T>(actual: 'T, expected: 'T) = X<unit>

        [<Stub; Name "equal">]
        member this.Equal<'T>(actual: 'T, expected: 'T, message: string) = X<unit>

        [<Stub; Name "notEqual">]
        member this.NotEqual<'T>(actual: 'T, expected: 'T) = X<unit>

        [<Stub; Name "notEqual">]
        member this.NotEqual<'T>(actual: 'T, expected: 'T, message: string) = X<unit>

        [<Stub; Name "deepEqual">]
        member this.DeepEqual<'T>(actual: 'T, expected: 'T) = X<unit>

        [<Stub; Name "deepEqual">]
        member this.DeepEqual<'T>(actual: 'T, expected: 'T, message: string) = X<unit>

        [<Stub; Name "notDeepEqual">]
        member this.NotDeepEqual<'T>(actual: 'T, expected: 'T) = X<unit>

        [<Stub; Name "notDeepEqual">]
        member this.NotDeepEqual<'T>(actual: 'T, expected: 'T, message: string) = X<unit>

        [<Stub; Name "strictEqual">]
        member this.StrictEqual<'T>(actual: 'T, expected: 'T) = X<unit>

        [<Stub; Name "strictEqual">]
        member this.StrictEqual<'T>(actual: 'T, expected: 'T, message: string) = X<unit>

        [<Stub; Name "notStrictEqual">]
        member this.NotStrictEqual<'T>(actual: 'T, expected: 'T) = X<unit>

        [<Stub; Name "notStrictEqual">]
        member this.NotStrictEqual<'T>(actual: 'T, expected: 'T, message: string) = X<unit>

        [<Stub; Name "propEqual">]
        member this.PropEqual<'T>(actual: 'T, expected: 'T) = X<unit>

        [<Stub; Name "propEqual">]
        member this.PropEqual<'T>(actual: 'T, expected: 'T, message: string) = X<unit>

        [<Stub; Name "notPropEqual">]
        member this.NotPropEqual<'T>(actual: 'T, expected: 'T) = X<unit>

        [<Stub; Name "notPropEqual">]
        member this.NotPropEqual<'T>(actual: 'T, expected: 'T, message: string) = X<unit>

        [<Stub; Name "expect">]
        member this.Expect(assertionCount: int) = X<unit>

        [<Stub; Name "async">]
        member this.Async() = X<unit -> unit>

        [<Stub; Name "push">]
        member this.Push(result: bool, actual: 'T, expected: 'T, message: string) = X<unit>

        [<Stub; Name "push">]
        member this.Push(result: bool, actual: 'T, expected: 'T) = X<unit>

    // Unlike the methods above, the functions below must not be implemented as X<_>.
    // They are meant to be called from the top-level, which means they will be called
    // from the server side too. Since X<_>'s .NET implementation throws an exception,
    // it is not suitable in this case.

    [<Inline "QUnit.test($name, $callback)">]
    let Test (name: string) (callback: Asserter -> unit) = ()

    [<Inline "QUnit.skip($name, $callback)">]
    let Skip (name: string) (callback: Asserter -> unit) = ()

    [<Inline "QUnit.todo($name, $callback)">]
    let Todo (name: string) (callback: Asserter -> unit) = ()

    [<Inline "QUnit.module($name)">]
    let Module (name: string) = ()
