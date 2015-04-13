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

namespace WebSharper

[<Proxy(typeof<Choice<_,_>>)>]
[<Name "WebSharper.Choice2">]
type private ChoiceProxy<'T1,'T2> =
    | Choice1Of2 of 'T1
    | Choice2Of2 of 'T2

[<Proxy(typeof<Choice<_,_,_>>)>]
[<Name "WebSharper.Choice3">]
type private ChoiceProxy<'T1,'T2,'T3> =
    | Choice1Of3 of 'T1
    | Choice2Of3 of 'T2
    | Choice3Of3 of 'T3

[<Proxy(typeof<Choice<_,_,_,_>>)>]
[<Name "WebSharper.Choice4">]
type private ChoiceProxy<'T1,'T2,'T3,'T4> =
    | Choice1Of4 of 'T1
    | Choice2Of4 of 'T2
    | Choice3Of4 of 'T3
    | Choice4Of4 of 'T4

[<Proxy(typeof<Choice<_,_,_,_,_>>)>]
[<Name "WebSharper.Choice5">]
type private ChoiceProxy<'T1,'T2,'T3,'T4,'T5> =
    | Choice1Of5 of 'T1
    | Choice2Of5 of 'T2
    | Choice3Of5 of 'T3
    | Choice4Of5 of 'T4
    | Choice5Of5 of 'T5

[<Proxy(typeof<Choice<_,_,_,_,_,_>>)>]
[<Name "WebSharper.Choice6">]
type private ChoiceProxy<'T1,'T2,'T3,'T4,'T5,'T6> =
    | Choice1Of6 of 'T1
    | Choice2Of6 of 'T2
    | Choice3Of6 of 'T3
    | Choice4Of6 of 'T4
    | Choice5Of6 of 'T5
    | Choice6Of6 of 'T6

[<Proxy(typeof<Choice<_,_,_,_,_,_,_>>)>]
[<Name "WebSharper.Choice7">]
type private ChoiceProxy<'T1,'T2,'T3,'T4,'T5,'T6,'T7> =
    | Choice1Of7 of 'T1
    | Choice2Of7 of 'T2
    | Choice3Of7 of 'T3
    | Choice4Of7 of 'T4
    | Choice5Of7 of 'T5
    | Choice6Of7 of 'T6
    | Choice7Of7 of 'T7
