module WebSharper.Compiler.WsFscServiceCommon

open System.IO.Pipes

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

let readingMessages (pipe: PipeStream) handleMessage = 
    // keep reading incoming messages asynchronously
    // notify the message triggering the OnMessageReceived event
    let buffer = Array.zeroCreate<byte> 0x1000
    let bufferResizable = new ResizeArray<byte>()    
    let rec readingMessage() =
        async {
            let! bytesRead = pipe.AsyncRead(buffer)
            if bytesRead = 0 then
                if pipe.IsConnected then
                    // bytesRead = 0 should signal closed connection.
                    // if connection is up, retry reading anyway.
                    // without if pipe.IsConnected, it would be infinite loop when a connection is closed.
                    // let! bytesRead = pipe.AsyncRead(buffer) doesn't block.
                    return! readingMessage()
                else
                    return None
            else 
                // add the bytes read to a "Resizable" collection 
                bufferResizable.AddRange(buffer.[0..bytesRead - 1])

                if pipe.IsMessageComplete then 
                    // if the message is completed fire OnMessageReceived event
                    // including the total bytes part of the message
                    let message = bufferResizable.ToArray()
                    let! finish = handleMessage message
                    // clear the resizable collection to be ready for the next income message
                    bufferResizable.Clear()
                    match finish with
                    | Some _ -> return finish
                    | None -> return! readingMessage()
                else
                    // the message is not completed, keep reading
                    return! readingMessage ()
        }
    readingMessage ()

