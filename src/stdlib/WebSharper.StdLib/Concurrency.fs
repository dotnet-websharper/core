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

/// Implements concurrency primitives.
module WebSharper.Concurrency

// This function has an optimized client implementation in ../WebSharper.StdLib.Proxies/Concurrency.fs.
// It is made public here so that libraries (such as UI) can call it directly.
/// Schedule the given function to be run asynchronously.
let Schedule (f: unit -> unit) : unit =
    async { return f() } |> Async.Start
