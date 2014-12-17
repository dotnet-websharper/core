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

open IntelliFactory.WebSharper
open IntelliFactory.WebSharper.Pagelets.Html

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
        do IntelliFactory.WebSharper.Collections.Tests.Dictionary.Tests
        do IntelliFactory.WebSharper.Tests.Regression.Tests
        Span [] :> _
