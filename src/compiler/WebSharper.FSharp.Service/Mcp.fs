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

/// A minimal Model Context Protocol server over stdio (newline-delimited JSON-RPC 2.0), for
/// `wsfscservice --mcp`: AI agents start it as an MCP server, it forwards tool calls to the running
/// booster of the same installation over its named pipe.
module WebSharper.FSharp.Service.Mcp

open System
open System.IO
open System.Text.Json
open System.Text.Json.Nodes

/// Protocol versions this server implements; the newest is offered when the client asks for another.
let supportedProtocolVersions = [ "2025-11-25"; "2025-06-18"; "2025-03-26"; "2024-11-05" ]

let serverName = "websharper-booster"

let private jsonOptions =
    let o = JsonSerializerOptions()
    o.Encoder <- System.Text.Encodings.Web.JavaScriptEncoder.UnsafeRelaxedJsonEscaping
    o

let private response (id: JsonNode) (result: JsonNode) =
    JsonObject(dict [ "jsonrpc", JsonValue.Create("2.0") :> JsonNode; "id", (match id with null -> null | i -> i.DeepClone()); "result", result ])

let private errorResponse (id: JsonNode) (code: int) (message: string) =
    let err = JsonObject(dict [ "code", JsonValue.Create(code) :> JsonNode; "message", JsonValue.Create(message) :> JsonNode ])
    JsonObject(dict [ "jsonrpc", JsonValue.Create("2.0") :> JsonNode; "id", (match id with null -> null | i -> i.DeepClone()); "error", err :> JsonNode ])

let private textResult (text: string) (isError: bool) =
    let content = JsonArray(JsonObject(dict [ "type", JsonValue.Create("text") :> JsonNode; "text", JsonValue.Create(text) :> JsonNode ]))
    JsonObject(dict [ "content", content :> JsonNode; "isError", JsonValue.Create(isError) :> JsonNode ])

/// Handles one JSON-RPC message, returns the response to send, None for notifications.
/// callTool gets the tool name and its arguments as JSON text.
let handle (version: string) (instructions: string) (toolsJson: unit -> JsonArray) (callTool: string -> string -> Result<string, string>) (line: string) : JsonNode option =
    let msg =
        try Some (JsonNode.Parse line)
        with _ -> None
    match msg with
    | None | Some null -> Some (errorResponse null -32700 "Parse error")
    | Some msg ->
    let id = match msg with :? JsonObject as o when o.ContainsKey "id" -> o.["id"] | _ -> null
    let isRequest = match msg with :? JsonObject as o -> o.ContainsKey "id" | _ -> false
    let methodName = try msg.["method"].GetValue<string>() with _ -> null
    let pars = try msg.["params"] with _ -> null
    match methodName with
    | null ->
        // a response from the client (we send no requests) or an invalid message
        if isRequest && not (isNull (try msg.["result"] with _ -> null)) then None
        elif isRequest then Some (errorResponse id -32600 "Invalid request")
        else None
    | _ when not isRequest ->
        // notifications: notifications/initialized, notifications/cancelled, ...
        None
    | "initialize" ->
        let requested = try pars.["protocolVersion"].GetValue<string>() with _ -> null
        let protocolVersion =
            if not (isNull requested) && List.contains requested supportedProtocolVersions then requested
            else List.head supportedProtocolVersions
        let result =
            JsonObject(dict [
                "protocolVersion", JsonValue.Create(protocolVersion) :> JsonNode
                "capabilities", JsonObject(dict [ "tools", JsonObject(dict [ "listChanged", JsonValue.Create(false) :> JsonNode ]) :> JsonNode ]) :> JsonNode
                "serverInfo", JsonObject(dict [ "name", JsonValue.Create(serverName) :> JsonNode; "version", JsonValue.Create(version) :> JsonNode ]) :> JsonNode
                "instructions", JsonValue.Create(instructions) :> JsonNode
            ])
        Some (response id result)
    | "ping" ->
        Some (response id (JsonObject()))
    | "tools/list" ->
        Some (response id (JsonObject(dict [ "tools", toolsJson () :> JsonNode ])))
    | "tools/call" ->
        let name = try pars.["name"].GetValue<string>() with _ -> null
        if isNull name then
            Some (errorResponse id -32602 "Missing tool name")
        else
            let arguments = match (try pars.["arguments"] with _ -> null) with null -> "" | a -> a.ToJsonString()
            let result =
                try callTool name arguments
                with e -> Error (sprintf "%s failed: %s" name e.Message)
            match result with
            | Ok text -> Some (response id (textResult text false))
            | Error text -> Some (response id (textResult text true))
    | m ->
        Some (errorResponse id -32601 (sprintf "Method not found: %s" m))

/// Serves MCP over the given reader and writer until the input ends.
let serve (version: string) (instructions: string) (toolsJson: unit -> JsonArray) (callTool: string -> string -> Result<string, string>) (input: TextReader) (output: TextWriter) =
    let mutable line = input.ReadLine()
    while not (isNull line) do
        if not (String.IsNullOrWhiteSpace line) then
            match handle version instructions toolsJson callTool line with
            | Some r ->
                // one message per line: the serializer writes no line breaks
                output.Write(r.ToJsonString(jsonOptions))
                output.Write('\n')
                output.Flush()
            | None -> ()
        line <- input.ReadLine()
