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


/// Statistics of the booster: counters and timings of the messages it handles, its compilations, and the agent
/// tools, its own and those of extensions. They stay on this machine: in memory, in a file next to the booster's
/// record (see save), and for `dotnet ws stats` through the `stats` message. They hold no source, symbol names,
/// paths or project names, only names chosen by the code that records them.
module WebSharper.FSharp.Service.Stats

open System
open System.IO
open System.Threading
open System.Text.Json.Nodes
open System.Collections.Concurrent

/// How many recent samples of a timing are kept for its percentiles.
let recentSamples = 1000

/// A timing: count, total and maximum since the booster started, and percentiles of the recent samples.
type Timing() =
    let recent = Array.zeroCreate<float> recentSamples
    let mutable count = 0L
    let mutable total = 0.
    let mutable maxValue = 0.
    member this.Add(value: float) =
        lock this (fun () ->
            recent.[int (count % int64 recentSamples)] <- value
            count <- count + 1L
            total <- total + value
            if value > maxValue then maxValue <- value)
    member this.Count = lock this (fun () -> count)
    member this.ToJson() =
        lock this (fun () ->
            let n = int (min count (int64 recentSamples))
            let sorted = Array.sub recent 0 n |> Array.sort
            let pct (p: float) = if n = 0 then 0. else sorted.[min (n - 1) (int (Math.Ceiling(p * float n)) - 1 |> max 0)]
            let round (v: float) = Math.Round(v, 2)
            JsonObject(dict [
                "count", JsonValue.Create(count) :> JsonNode
                "total", JsonValue.Create(round total) :> JsonNode
                "mean", JsonValue.Create(if count = 0L then 0. else round (total / float count)) :> JsonNode
                "p50", JsonValue.Create(round (pct 0.5)) :> JsonNode
                "p90", JsonValue.Create(round (pct 0.9)) :> JsonNode
                "p99", JsonValue.Create(round (pct 0.99)) :> JsonNode
                "max", JsonValue.Create(round maxValue) :> JsonNode
            ]))

/// Where statistics go: the booster's store, or in `wsfscservice --mcp` a queue sent to the booster.
type ISink =
    abstract Count : name: string * by: int64 -> unit
    /// A value of a distribution: milliseconds for durations (names ending with _ms), else the unit in the name.
    abstract Value : name: string * value: float -> unit

/// The statistics of a booster.
type Store() =
    let counters = ConcurrentDictionary<string, int64 ref>(StringComparer.Ordinal)
    let timings = ConcurrentDictionary<string, Timing>(StringComparer.Ordinal)
    let mutable changed = 0
    member _.Changed = changed <> 0
    member _.MarkSaved() = changed <- 0
    member _.CounterValue(name: string) = match counters.TryGetValue name with | true, c -> c.Value | _ -> 0L
    member _.TimingCount(name: string) = match timings.TryGetValue name with | true, t -> t.Count | _ -> 0L
    interface ISink with
        member _.Count(name, by) =
            let c = counters.GetOrAdd(name, fun _ -> ref 0L)
            Interlocked.Add(&c.contents, by) |> ignore
            changed <- 1
        member _.Value(name, value) =
            if not (Double.IsNaN value || Double.IsInfinity value) then
                timings.GetOrAdd(name, fun _ -> Timing()).Add value
                changed <- 1
    member this.ToJson() =
        let counterJson = JsonObject()
        for KeyValue(k, v) in counters |> Seq.sortBy (fun kv -> kv.Key) do counterJson.[k] <- JsonValue.Create(v.Value)
        let timingJson = JsonObject()
        for KeyValue(k, v) in timings |> Seq.sortBy (fun kv -> kv.Key) do timingJson.[k] <- v.ToJson()
        JsonObject(dict [ "counters", counterJson :> JsonNode; "timings", timingJson :> JsonNode ])

/// The statistics of this process.
let store = Store()

/// The sink of this process: the store in the booster, a queue in `wsfscservice --mcp` (see Forwarding).
let mutable sink = store :> ISink

let count name = sink.Count(name, 1L)
let countBy name by = sink.Count(name, by)
let value name v = sink.Value(name, v)
let ms name (elapsed: TimeSpan) = sink.Value(name, elapsed.TotalMilliseconds)

/// A name made of fixed text and a short value that is safe to store (a tool name, an error code, a stage):
/// lower case letters, digits, '_', '-' and '.', other characters replaced by '_', at most 48 characters.
let part (s: string) =
    if String.IsNullOrEmpty s then "none" else
    let chars = s.ToLowerInvariant().ToCharArray() |> Array.map (fun c -> if Char.IsAsciiLetterOrDigit c || c = '_' || c = '-' || c = '.' then c else '_')
    let t = String(chars).Trim('_')
    let t = if t.Length > 48 then t.Substring(0, 48) else t
    if t = "" then "none" else t

/// A compilation stage name as LoggerBase.TimedStage prints it, without the names and numbers some stages add (file,
/// bundle and assembly names, paths, counts): it ends before " to ", " from ", ":" and the first word with a '.', a
/// path separator or a digit. "Writing code files to /a/b.dll" is writing_code_files, "Writing App.Client.js"
/// writing, "Parsing with FCS (3 of 7 files from the read cache)" parsing_with_fcs.
let stageName (stage: string) =
    if String.IsNullOrWhiteSpace stage then "none" else
    let cut (sep: string) (s: string) = match s.IndexOf sep with | -1 -> s | i -> s.Substring(0, i)
    let words =
        (stage |> cut " to " |> cut " from " |> cut ":").Replace('(', ' ').Replace(')', ' ').Replace("#", "")
            .Split(' ', StringSplitOptions.RemoveEmptyEntries)
        |> Array.takeWhile (fun w -> not (w |> Seq.exists (fun c -> c = '.' || c = '/' || c = '\\' || Char.IsDigit c || c = '%')))
    let name = part (String.Join("_", words))
    let collapsed = Text.RegularExpressions.Regex.Replace(name, "_+", "_")
    if collapsed = "" then "none" else collapsed

// ---------------------------------------------------------------------------------------------
// Statistics of extensions: names must be lower case and fixed by the extension's code, values numbers or booleans.

let private validName = Text.RegularExpressions.Regex(@"^[a-z0-9][a-z0-9_.-]{0,63}$")

/// The statistics an extension records, under "ext.<extension name>.".
type IAgentStats =
    /// Counts an event.
    abstract Count : name: string * ?by: int -> unit
    /// Times what runs until the result is disposed: `use _ = stats.Time "sidecar.read"`.
    abstract Time : name: string -> IDisposable
    /// A value of a distribution, for example a size.
    abstract Value : name: string * value: float -> unit
    /// Counts an event and records its fields: numbers as distributions (name.field), booleans as counters
    /// (name.field.true / name.field.false). Fields of other types are dropped.
    abstract Event : name: string * fields: (string * obj) list -> unit

/// The IAgentStats of an extension: its names are checked (lower case, fixed text), invalid ones are dropped and
/// counted as stats.dropped.
let forExtension (extensionName: string) =
    let prefix = "ext." + part extensionName + "."
    let check (name: string) f =
        if validName.IsMatch name then f (prefix + name) else count "stats.dropped"
    { new IAgentStats with
        member _.Count(name, ?by) = check name (fun n -> countBy n (int64 (defaultArg by 1)))
        member _.Time(name) =
            let sw = Diagnostics.Stopwatch.StartNew()
            { new IDisposable with member _.Dispose() = check name (fun n -> ms (n + "_ms") sw.Elapsed) }
        member _.Value(name, v) = check name (fun n -> value n v)
        member _.Event(name, fields) =
            check name (fun n ->
                count n
                for (field, v) in fields do
                    if not (validName.IsMatch field) then count "stats.dropped" else
                    match v with
                    | :? bool as b -> count (sprintf "%s.%s.%b" n field b)
                    | :? int as i -> value (n + "." + field) (float i)
                    | :? int64 as i -> value (n + "." + field) (float i)
                    | :? float as f -> value (n + "." + field) f
                    | :? float32 as f -> value (n + "." + field) (float f)
                    | :? TimeSpan as t -> value (n + "." + field + "_ms") t.TotalMilliseconds
                    | _ -> count "stats.dropped")
    }

// ---------------------------------------------------------------------------------------------
// `wsfscservice --mcp` sends what it records to the booster, which keeps them with its own.

/// What `wsfscservice --mcp` queues: [ "c", name, by ] or [ "v", name, value ].
type Forwarding() =
    let queue = ConcurrentQueue<JsonNode>()
    let maxQueued = 10000
    let add (node: JsonNode) = if queue.Count < maxQueued then queue.Enqueue node
    interface ISink with
        member _.Count(name, by) = add (JsonArray(JsonValue.Create "c", JsonValue.Create name, JsonValue.Create by))
        member _.Value(name, v) = add (JsonArray(JsonValue.Create "v", JsonValue.Create name, JsonValue.Create v))
    /// The queued events, removed from the queue.
    member _.Take() =
        let items = JsonArray()
        let mutable item = null
        while queue.TryDequeue(&item) do items.Add item
        items
    /// Puts events back when they could not be sent.
    member _.Return(items: JsonArray) =
        for i in items |> Seq.toArray do
            items.Remove i |> ignore
            add i

/// Applies the events a `wsfscservice --mcp` sent (Forwarding.Take) to this process's sink.
let apply (events: JsonArray) =
    for e in events do
        try
            match e.[0].GetValue<string>() with
            | "c" -> sink.Count(e.[1].GetValue<string>(), e.[2].GetValue<int64>())
            | "v" -> sink.Value(e.[1].GetValue<string>(), e.[2].GetValue<float>())
            | _ -> ()
        with _ -> count "stats.dropped"

// ---------------------------------------------------------------------------------------------
// The booster's statistics with its state, and their file.

let private started = DateTime.UtcNow

/// The statistics of this booster with its state: version, start, uptime, memory, and what `health` adds.
let snapshot (version: string) (health: (string * JsonNode) list) =
    let o = store.ToJson()
    use proc = Diagnostics.Process.GetCurrentProcess()
    let h =
        JsonObject(dict ([
            "version", JsonValue.Create(version) :> JsonNode
            "started", JsonValue.Create(started.ToString("o")) :> JsonNode
            "uptime_s", JsonValue.Create(Math.Round((DateTime.UtcNow - started).TotalSeconds)) :> JsonNode
            "working_set_mb", JsonValue.Create(Math.Round(float proc.WorkingSet64 / 1048576., 1)) :> JsonNode
            "managed_heap_mb", JsonValue.Create(Math.Round(float (GC.GetTotalMemory false) / 1048576., 1)) :> JsonNode
        ] @ health))
    o.["booster"] <- h
    o

/// The file of the statistics of the booster running from a folder: in a folder of their own next to the boosters'
/// records (dotnet ws reads every .json file of the records' folder as a record).
let pathFor (folder: string) =
    Path.Combine(WebSharper.Compiler.WsFscServiceCommon.boosterRecordFolder (), "stats",
                 Path.GetFileNameWithoutExtension(WebSharper.Compiler.WsFscServiceCommon.boosterRecordPath folder) + ".json")

/// Writes the statistics to their file, if they changed. Best effort.
let save (folder: string) (getSnapshot: unit -> JsonObject) =
    if store.Changed then
        try
            store.MarkSaved()
            let path = pathFor folder
            Directory.CreateDirectory(Path.GetDirectoryName path) |> ignore
            let tmp = sprintf "%s.%i.tmp" path Environment.ProcessId
            File.WriteAllText(tmp, (getSnapshot ()).ToJsonString(Text.Json.JsonSerializerOptions(WriteIndented = true)))
            File.Move(tmp, path, true)
        with _ -> ()
