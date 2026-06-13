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

open System.Diagnostics
open System
open System.Globalization
open System.IO
open System.Text

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
    
    let mutable timeStamps = [ Stopwatch.GetTimestamp() ]
    let mutable projectFile = ""
    let mutable timingLogPath = ""
    static let timingLock = obj()

    static let jsonString (value: string) =
        if isNull value then
            "null"
        else
            let b = StringBuilder()
            b.Append('"') |> ignore
            for c in value do
                match c with
                | '"' -> b.Append("\\\"") |> ignore
                | '\\' -> b.Append("\\\\") |> ignore
                | '\b' -> b.Append("\\b") |> ignore
                | '\f' -> b.Append("\\f") |> ignore
                | '\n' -> b.Append("\\n") |> ignore
                | '\r' -> b.Append("\\r") |> ignore
                | '\t' -> b.Append("\\t") |> ignore
                | c when int c < 32 -> b.Append("\\u").Append((int c).ToString("x4", CultureInfo.InvariantCulture)) |> ignore
                | c -> b.Append(c) |> ignore
            b.Append('"').ToString()

    let writeTimingRecord (stage: string) (elapsed: TimeSpan) (depth: int) =
        if not (String.IsNullOrWhiteSpace timingLogPath) then
            try
                let dir = Path.GetDirectoryName timingLogPath
                if not (String.IsNullOrWhiteSpace dir) then
                    Directory.CreateDirectory dir |> ignore
                let project =
                    if String.IsNullOrWhiteSpace projectFile then ""
                    else Path.GetFileNameWithoutExtension projectFile
                let json =
                    sprintf
                        """{"t":%s,"project":%s,"stage":%s,"ms":%s,"depth":%d}"""
                        (jsonString (DateTimeOffset.UtcNow.ToString("O", CultureInfo.InvariantCulture)))
                        (jsonString project)
                        (jsonString stage)
                        (elapsed.TotalMilliseconds.ToString("0.###", CultureInfo.InvariantCulture))
                        depth
                lock timingLock (fun () -> File.AppendAllText(timingLogPath, json + Environment.NewLine))
            with _ ->
                ()

    member _.Indent (s: string) =
        String.replicate (timeStamps.Length - 1) "  " + s

    member _.ProjectFile
        with get() = projectFile
        and set value = projectFile <- value

    member _.TimingLogPath
        with get() = timingLogPath
        and set value = timingLogPath <- value

    abstract Error : string -> unit
    abstract Out : string -> unit
    
    member x.EnterContext() =
        timeStamps <- timeStamps.Head :: timeStamps
        
    member x.ExitContext() =
        timeStamps <- timeStamps.Tail

    member x.TimedStage name =
        let now = Stopwatch.GetTimestamp()
        let lastTimestamp = timeStamps.Head
        let elapsed = TimeSpan.FromSeconds (float (now - lastTimestamp) / float Stopwatch.Frequency)
        sprintf "%s: %O" name elapsed
        |> x.Out
        writeTimingRecord name elapsed (timeStamps.Length - 1)
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
