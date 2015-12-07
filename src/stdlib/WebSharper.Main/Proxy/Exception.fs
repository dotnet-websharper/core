// $begin{copyright}
//
// This file is part of WebSharper
//
// Copyright (c) 2008-2015 IntelliFactory
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

[<Name "Exception">]
[<Proxy(typeof<System.Exception>)>]
type private ExceptionProxy [<Direct "new Error($message)">] (message: string) =

    [<JavaScript>]
    new () = ExceptionProxy "Exception of type 'System.Exception' was thrown."

    member this.Message with [<Inline "$this.message">] get () = X<string>

[<Proxy(typeof<MatchFailureException>)>]
[<Name "MatchFailureException">]
[<JavaScript>]
type private MatchFailureExceptionProxy (message: string, line: int, column: int) =
    inherit ExceptionProxy (message + " at " + string line + ":" + string column)

[<Proxy(typeof<System.IndexOutOfRangeException>)>]
[<Name "IndexOutOfRangeException">]
[<JavaScript>]
type private IndexOutOfRangeExceptionProxy(message: string) =
    inherit ExceptionProxy(message)

    new () = IndexOutOfRangeExceptionProxy "Index was outside the bounds of the array."

[<Proxy(typeof<System.OperationCanceledException>)>]
[<Name "OperationCanceledException">]
[<JavaScript>]
type private OperationCanceledExceptionProxy(message: string) =
    inherit ExceptionProxy(message)

    new () = OperationCanceledExceptionProxy "The operation was canceled."

[<Proxy(typeof<System.ArgumentException>)>]
[<Name "ArgumentException">]
[<JavaScript>]
type private ArgumentExceptionProxy(message: string) =
    inherit ExceptionProxy(message)
    
    new () = ArgumentExceptionProxy "Value does not fall within the expected range."

[<Proxy(typeof<System.InvalidOperationException>)>]
[<Name "InvalidOperationException">]
[<JavaScript>]
type private InvalidOperationExceptionProxy(message: string) =
    inherit ExceptionProxy(message)
    
    new () = InvalidOperationExceptionProxy "Operation is not valid due to the current state of the object."

[<Proxy(typeof<System.AggregateException>)>]
[<Name "AggregateException">]
[<JavaScript>]
type private AggregateExceptionProxy(message: string, innerExceptions: exn[]) =
    inherit ExceptionProxy(message)

    new (innerExceptions: exn[]) = AggregateExceptionProxy("One or more errors occurred.", innerExceptions)

    member this.InnerExceptions 
        with [<Inline "$this.InnerExceptions">] get() = X<System.Collections.ObjectModel.ReadOnlyCollection<exn>>

[<Proxy(typeof<System.TimeoutException>)>]
[<Name "TimeoutException">]
type private TimeoutExceptionProxy(message: string) =
    inherit ExceptionProxy(message)
    
    new () = TimeoutExceptionProxy "The operation has timed out."
