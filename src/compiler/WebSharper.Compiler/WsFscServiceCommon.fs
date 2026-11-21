module WebSharper.Compiler.WsFscServiceCommon

open System.IO.Pipes
open System.Text

type ArgsType = {args: string array}

let md5 = System.Security.Cryptography.MD5.Create()

let hashPath (fullPath: string) =
    let data =
        fullPath.ToLower()
        |> Encoding.UTF8.GetBytes
        |> md5.ComputeHash
    (System.Text.StringBuilder(), data)
    ||> Array.fold (fun sb b -> sb.Append(b.ToString("x2")))
    |> string

/// The pipe of the booster running from a folder: a hash of the folder on Windows, a file in the temp folder elsewhere.
let boosterPipeName (folder: string) =
    let raw = System.IO.Path.Combine(folder, "WsFscServicePipe") |> hashPath
    if System.Runtime.InteropServices.RuntimeInformation.IsOSPlatform(System.Runtime.InteropServices.OSPlatform.Windows) then
        raw
    else
        System.IO.Path.Combine(System.IO.Path.GetTempPath(), raw)

/// A running booster registers itself for dotnet ws, which can't tell a booster started as
/// `dotnet wsfscservice.dll` from any other dotnet process. One file per booster folder,
/// written on start and removed on exit, see boosterRecordPath.
[<CLIMutable>]
type BoosterRecord =
    {
        Pid: int
        /// The WebSharper package version, e.g. 11.0.0.756-preview.1
        Version: string
        /// The folder of wsfscservice.dll
        Folder: string
        /// The UTC start time of the process, to tell it from a later process with the same pid
        Started: System.DateTime
        Pipe: string
    }

let boosterRecordFolder () =
    System.IO.Path.Combine(System.IO.Path.GetTempPath(), "websharper-boosters")

let boosterRecordPath (folder: string) =
    System.IO.Path.Combine(boosterRecordFolder (), hashPath (System.IO.Path.Combine(folder, "WsFscServicePipe")) + ".json")

exception PipeException of unit

let readingMessages (pipe: PipeStream) (handleMessage: obj -> Async<'a option>) (clientError: unit -> unit) = 
    let readBytes (stream: Stream) (count: int) =
       let buffer = Array.zeroCreate<byte> count
       let rec readLoop offset remaining =
           async {
               if remaining = 0 then
                   return buffer
               else
                   let! bytesRead = stream.ReadAsync(buffer, offset, remaining) |> Async.AwaitTask
                   if bytesRead = 0 then
                       return null
                   else
                       return! readLoop (offset + bytesRead) (remaining - bytesRead)
            }
       readLoop 0 count
    let rec readingMessage() =
        async {
            try
                let readMessage () =
                    async {
                        let! lengthBuffer = readBytes pipe 4
                        if isNull lengthBuffer then
                            return null
                        else
                            let messageLength = System.BitConverter.ToInt32(lengthBuffer, 0)
                            // Read the actual message based on the length
                            let! messageBuffer = readBytes pipe messageLength
                            // Convert the byte array to a string (assuming UTF-8 encoding)
                            let message = Encoding.UTF8.GetString(messageBuffer)
                            return message
                    }
                clientError ()
                let! byteArr = readMessage ()
                match byteArr with
                | "" | null ->
                    do! Async.Sleep 1000
                    return! readingMessage()
                | _ ->
                    let deserializedMessage = System.Text.Json.JsonSerializer.Deserialize(byteArr)
                    let! finish = handleMessage deserializedMessage
                    match finish with
                    | Some _ -> return finish
                    | None -> return! readingMessage()
            with
            | :? System.Runtime.Serialization.SerializationException as ex->
                return None
            | PipeException _ as ex ->
                raise ex |> ignore
                return None
            | ex ->
                return! readingMessage()
        }
    readingMessage ()

