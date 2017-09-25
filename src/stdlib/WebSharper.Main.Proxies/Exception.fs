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

[<Name "Exception">]
[<Proxy(typeof<System.Exception>)>]
type private ExceptionProxy(message, inner: exn) =
    inherit Error(message)

    new (message: string) = ExceptionProxy (message, null)

    new () = ExceptionProxy ("Exception of type 'System.Exception' was thrown.", null)

    member this.Message with [<Inline "$this.message">] get () = X<string>
    member this.InnerException with get () = inner

[<Proxy(typeof<MatchFailureException>)>]
[<Name "MatchFailureException">]
type private MatchFailureExceptionProxy (message: string, line: int, column: int) =
    inherit exn(message + " at " + string line + ":" + string column)

[<Proxy(typeof<System.IndexOutOfRangeException>)>]
[<Name "IndexOutOfRangeException">]
type private IndexOutOfRangeExceptionProxy(message: string) =
    inherit exn(message)

    new () = IndexOutOfRangeExceptionProxy "Index was outside the bounds of the array."

[<Proxy(typeof<System.OperationCanceledException>)>]
[<Name "OperationCanceledException">]
type private OperationCanceledExceptionProxy(message: string, inner: exn, ct: CT) =
    inherit exn(message, inner)

    new (ct) = OperationCanceledExceptionProxy ("The operation was canceled.", null, ct)
    
    [<Inline>]
    new () = OperationCanceledExceptionProxy (CT.None)
    [<Inline>]
    new (message) = OperationCanceledExceptionProxy (message, null, CT.None)
    [<Inline>]
    new (message, ct) = OperationCanceledExceptionProxy (message, null, ct)
    [<Inline>]
    new (message, inner) = OperationCanceledExceptionProxy (message, inner, CT.None)

    [<Inline>]
    member this.CancellationToken = ct

[<Proxy(typeof<System.ArgumentException>)>]
[<Name "ArgumentException">]
type private ArgumentExceptionProxy(message: string) =
    inherit exn(message)
    
    new () = ArgumentExceptionProxy "Value does not fall within the expected range."

    new (argumentName: string, message: string) =
        ArgumentExceptionProxy (message + "\nParameter name: " + argumentName)

[<Proxy(typeof<System.ArgumentOutOfRangeException>)>]
[<Name "ArgumentOutOfRangeException">]
type private ArgumentOutOfRangeExceptionProxy =
    inherit exn

    new () =
        { inherit exn("Specified argument was out of the range of valid values.") }

    new (argumentName: string) =
        new ArgumentOutOfRangeExceptionProxy(argumentName, "Specified argument was out of the range of valid values.")

    new (argumentName: string, message: string) =
        { inherit exn(message + "\nParameter name: " + argumentName) }

[<Proxy(typeof<System.ArgumentNullException>)>]
[<Name "ArgumentNullException">]
type private ArgumentNullExceptionProxy =
    inherit exn

    new () =
        { inherit exn("Value cannot be null.") }

    new (argumentName: string) =
        new ArgumentNullExceptionProxy(argumentName, "Value cannot be null.")

    new (argumentName: string, message: string) =
        { inherit exn(message + "\nParameter name: " + argumentName) }

[<Proxy(typeof<System.InvalidOperationException>)>]
[<Name "InvalidOperationException">]
type private InvalidOperationExceptionProxy(message: string, innerExn: exn) =
    inherit exn(message, innerExn)
    
    new () = InvalidOperationExceptionProxy "Operation is not valid due to the current state of the object."

    new (message) =
        new InvalidOperationExceptionProxy(message, null)

[<Proxy(typeof<System.AggregateException>)>]
[<Name "AggregateException">]
type private AggregateExceptionProxy(message: string, innerExceptions: exn[]) =
    inherit exn(message)

    new (innerExceptions: exn[]) = AggregateExceptionProxy("One or more errors occurred.", innerExceptions)

    new (innerExceptions: seq<exn>) = AggregateExceptionProxy("One or more errors occurred.", Array.ofSeq innerExceptions)

    new (message, innerExceptions: seq<exn>) = AggregateExceptionProxy(message, Array.ofSeq innerExceptions)

    new (message, innerException: exn) = AggregateExceptionProxy(message, [| innerException |])

    [<Inline>]
    member this.InnerExceptions 
        with get() = As<System.Collections.ObjectModel.ReadOnlyCollection<exn>> innerExceptions

[<Proxy(typeof<System.TimeoutException>)>]
[<Name "TimeoutException">]
type private TimeoutExceptionProxy(message: string) =
    inherit exn(message)
    
    new () = TimeoutExceptionProxy "The operation has timed out."

[<Proxy(typeof<System.FormatException>)>]
[<Name "FormatException">]
type private FormatException(message: string) =
    inherit exn(message)

    new () = FormatException "One of the identified items was in an invalid format."

[<Proxy(typeof<System.OverflowException>)>]
[<Name "OverflowException">]
type private OverflowException(message: string) =
    inherit exn(message)

    new () = OverflowException "Arithmetic operation resulted in an overflow."
