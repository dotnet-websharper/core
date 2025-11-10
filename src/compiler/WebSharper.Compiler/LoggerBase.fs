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

namespace WebSharper.Compiler

[<AbstractClass>]
#if DEBUG
// Persisting logger as static for assembly resolution logging in when debugging the compiler
type LoggerBase() as self =
    static let mutable currentLogger = Unchecked.defaultof<LoggerBase>
    do currentLogger <- self
#else
/// Abstract base class for WebSharper compilation times and F# source generator logging.
type LoggerBase() =
#endif
    
    let mutable timeStamps = [ System.DateTime.Now ]

    member _.Indent (s: string) =
        String.replicate (timeStamps.Length - 1) "  " + s

    abstract Error : string -> unit
    abstract Out : string -> unit
    
    member x.EnterContext() =
        timeStamps <- timeStamps.Head :: timeStamps
        
    member x.ExitContext() =
        timeStamps <- timeStamps.Tail

    member x.TimedStage name =
        let now = System.DateTime.Now
        let lastTimestamp = timeStamps.Head
        sprintf "%s: %O" name (now - lastTimestamp)
        |> x.Out
        timeStamps <- now :: timeStamps.Tail        

    [<System.Diagnostics.Conditional "DEBUG">]
    member x.DebugWrite s =
        System.Diagnostics.Debug.WriteLine (x.Indent s)

#if DEBUG
    static member Current
        with get() = currentLogger
        and set l = currentLogger <- l
#endif

/// WebSharper logger printing to console out and error.
type ConsoleLogger() =
    inherit LoggerBase()
    override x.Error s =
        System.Console.Error.WriteLine(x.Indent s)

    override x.Out s =
        System.Console.Out.WriteLine(x.Indent s)

/// WebSharper logger that ignores all messages.
type EmptyLogger() =
    inherit LoggerBase()

    override this.Out (arg: string) = ()
    override this.Error (arg: string) = ()
