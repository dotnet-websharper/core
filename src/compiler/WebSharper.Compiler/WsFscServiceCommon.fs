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

let readingMessages (pipe: PipeStream) (handleMessage: obj -> Async<'a option>) = 
    let rec readingMessage() =
        async {
            try
                let readMessage () =
                    async {
                        let sb = new StringBuilder()
                        let mutable buffer = Array.zeroCreate<byte> 5
                        let bytesRead = pipe.Read(buffer, 0, buffer.Length)
                        sb.Append(System.Text.Encoding.UTF8.GetString(buffer)) |> ignore
                        buffer <- Array.zeroCreate<byte> 5
                        while (not pipe.IsMessageComplete) do
                            let bytesRead = pipe.Read(buffer, 0, buffer.Length)
                            sb.Append(System.Text.Encoding.UTF8.GetString(buffer)) |> ignore
                            buffer <- Array.zeroCreate<byte> 5
                        let res = sb.ToString()
                        return res.Replace("\x00", "")
                    }
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
            | ex ->
                return! readingMessage()
        }
    readingMessage ()

