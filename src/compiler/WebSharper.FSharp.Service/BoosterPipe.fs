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

/// One request to a running booster over its named pipe, as `--mcp` front ends and compilers send them: the
/// message, then the booster's j: (answer text), t: (its time in ms) and x: (exit code) replies. Compiled into
/// wsfscservice and linked into wscscservice.
#if CSHARP_BOOSTER
module WebSharper.CSharp.Service.BoosterPipe
#else
module WebSharper.FSharp.Service.BoosterPipe
#endif

open System
open System.IO
open System.IO.Pipes
open System.Runtime.InteropServices
open WebSharper.Compiler.WsFscServiceCommon

/// Whether a booster listens on the pipe: without one, connecting would only wait for the timeout. On Linux and
/// macOS the pipe is a socket file that stays when the booster is killed, so a refused connection tells.
let exists (pipeName: string) =
    try
        if RuntimeInformation.IsOSPlatform(OSPlatform.Windows) then File.Exists(@"\\.\pipe\" + pipeName)
        elif not (File.Exists pipeName) then false
        else
            use probe = new System.Net.Sockets.Socket(System.Net.Sockets.AddressFamily.Unix, System.Net.Sockets.SocketType.Stream, System.Net.Sockets.ProtocolType.Unspecified)
            try
                probe.Connect(System.Net.Sockets.UnixDomainSocketEndPoint(pipeName))
                true
            with :? System.Net.Sockets.SocketException -> false
    with _ -> true

/// Why a request was not answered.
type SendError =
    /// No booster listens on the pipe, with the reason.
    | NotRunning of string
    /// The booster closed the connection without an exit code.
    | NoAnswer of string

/// Sends one message: Ok (exit code, the text of the j: replies, the t: time in ms), or why not.
let send (pipeName: string) (args: string[]) (connectTimeoutMs: int) : Result<int * string * float option, SendError> =
    if not (exists pipeName) then Error (NotRunning "its pipe does not exist") else
    use pipe = new NamedPipeClientStream(".", pipeName, PipeDirection.InOut, PipeOptions.Asynchronous)
    let connectError =
        try
            pipe.Connect(connectTimeoutMs)
            if pipe.IsConnected then None else Some "could not connect"
        with
        | :? TimeoutException -> Some "no answer on its pipe"
        | e -> Some e.Message
    match connectError with
    | Some reason -> Error (NotRunning reason)
    | None ->
    try
        pipe.ReadMode <- PipeTransmissionMode.Byte
        let options = System.Text.Json.JsonSerializerOptions()
        options.Encoder <- System.Text.Encodings.Web.JavaScriptEncoder.UnsafeRelaxedJsonEscaping
        let bytes = System.Text.Json.JsonSerializer.Serialize({ args = args }, options) |> System.Text.Encoding.UTF8.GetBytes
        pipe.Write(BitConverter.GetBytes(bytes.Length), 0, 4) // prepend with message length
        pipe.Write(bytes, 0, bytes.Length)
        pipe.Flush()
        if RuntimeInformation.IsOSPlatform(OSPlatform.Windows) then
            pipe.WaitForPipeDrain()
        let text = System.Text.StringBuilder()
        let mutable boosterMs = None
        let handle (message: obj) =
            async {
                let s = (message :?> System.Text.Json.JsonElement).GetString()
                if s.StartsWith "j: " then
                    text.Append(s.Substring 3) |> ignore
                    return None
                elif s.StartsWith "t: " then
                    match Double.TryParse(s.Substring 3, Globalization.NumberStyles.Float, Globalization.CultureInfo.InvariantCulture) with
                    | true, v -> boosterMs <- Some v
                    | _ -> ()
                    return None
                elif s.StartsWith "x: " then
                    return Some (int (s.Substring 3))
                else
                    return None
            }
        let disconnected () = if not pipe.IsConnected then raise (PipeException ())
        match Async.RunSynchronously (readingMessages pipe handle disconnected) with
        | Some code -> Ok (code, text.ToString(), boosterMs)
        | None -> Error (NoAnswer "The WebSharper booster closed the connection without answering.")
    with _ ->
        Error (NoAnswer "The WebSharper booster closed the connection without answering, it may have stopped.")
