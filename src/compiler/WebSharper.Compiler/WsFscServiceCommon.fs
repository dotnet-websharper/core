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

