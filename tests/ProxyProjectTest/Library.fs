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

module ProxyProjectTest.Functions

let add x y = x + y

type IInterfaceTest =
    abstract Add: int * int -> int

let isJS() =
#if JAVASCRIPT
    true
#else
    false
#endif

/// #1623: inline members with `try`/`finally` and `try`/`with` in expression
/// position, compiled in the proxy project and inlined into another assembly.
type ResourceBuilder() =
    member inline _.Return(x: 'T) : Result<'T, string> = Ok x

    member inline _.Delay([<InlineIfLambda>] f: unit -> Result<'T, string>) = f

    member inline _.Run([<InlineIfLambda>] f: unit -> Result<'T, string>) = f ()

    member inline this.TryFinally([<InlineIfLambda>] body: unit -> Result<'T, string>, [<InlineIfLambda>] compensation: unit -> unit) =
        try
            this.Run body
        finally
            compensation ()

    member inline this.TryWith([<InlineIfLambda>] body: unit -> Result<'T, string>, [<InlineIfLambda>] handler: exn -> Result<'T, string>) =
        try
            this.Run body
        with e ->
            handler e

    member inline this.Using(resource: #System.IDisposable, binder: _ -> Result<'T, string>) =
        this.TryFinally(
            (fun () -> binder resource),
            (fun () -> if not (obj.ReferenceEquals(resource, null)) then resource.Dispose())
        )

let resource = ResourceBuilder()
