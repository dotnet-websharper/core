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

module Website.Controls

open WebSharper
open WebSharper.Testing
open WebSharper.Html.Client

[<Sealed>]
type EntryPoint() =
    inherit Web.Control()

    [<JavaScript>]
    override this.Body =
        Client.EntryPoint()
        Span [] :> _

[<Sealed>]
type Tests() =
    inherit Web.Control()

    [<JavaScript>]
    override this.Body =
        RunTests [
            WebSharper.Collections.Tests.Dictionary.Tests
            WebSharper.Collections.Tests.Set.Tests
            WebSharper.Collections.Tests.Map.Tests
            WebSharper.Collections.Tests.ResizeArray.Tests
            WebSharper.Collections.Tests.LinkedList.Tests
            WebSharper.Collections.Tests.HashSet.Tests
            WebSharper.Tests.Array.Tests
            WebSharper.Tests.Array2D.Tests
            WebSharper.Tests.Async.Tests
            WebSharper.Tests.Basis.Tests
            WebSharper.Tests.Char.Tests
            WebSharper.Tests.DateTime.Tests
            WebSharper.Tests.DateTime.NativeTests
            WebSharper.Tests.Double.Tests
            WebSharper.Tests.Exception.Tests
            WebSharper.Tests.Int32.Tests
            WebSharper.Tests.KeyValuePair.Tests
            WebSharper.Tests.Lazy.Tests
            WebSharper.Tests.List.Tests
            WebSharper.Tests.Macro.Tests
            WebSharper.Tests.Math.Tests
            WebSharper.Tests.Object.Tests
            WebSharper.Tests.Operators.Tests
            WebSharper.Tests.Option.Tests
            WebSharper.Tests.Proxy.Tests
            WebSharper.Tests.Queue.Tests
            WebSharper.Tests.Random.Tests
            WebSharper.Tests.Ref.Tests
            WebSharper.Tests.Regression.Tests
            WebSharper.Tests.Seq.Tests
            WebSharper.Tests.Stack.Tests
            WebSharper.Tests.String.Tests
            WebSharper.Tests.TimeSpan.Tests
            WebSharper.Tests.Printf.Tests
            WebSharper.Tests.Tupled.Tests
            WebSharper.Tests.WIG.Tests
            WebSharper.Web.Tests.Remoting.Tests
            WebSharper.Html5.Tests.Tests
        ]
        Span [] :> _
