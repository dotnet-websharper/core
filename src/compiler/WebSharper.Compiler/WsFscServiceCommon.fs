module WebSharper.Compiler.WsFscServiceCommon

open System.IO.Pipes

type ArgsType = {args: string array}

let hashPipeName (fullPath: string) =
    fullPath.Replace("/", "$").Replace("\\", "$")

let readingMessages (pipe: PipeStream) = 
    // partial function (byte array -> Async<unit>)
    // keep reading incoming messages asynchronously
    // notify the message triggering the OnMessageReceived event
    let innerReadingMessages = 
        let bufferResizable = new ResizeArray<byte>()                                            
        let rec readingMessage (buffer: byte array) (handleMessage: byte[] -> Async<unit>) = async {
            let! bytesRead = pipe.AsyncRead(buffer, 0, buffer.Length)
            if bytesRead = 0 then
                ()
            else 
                // add the bytes read to a "Resizable" collection 
                bufferResizable.AddRange(buffer.[0..bytesRead - 1])
              
                if pipe.IsMessageComplete then 
                    // if the message is completed fire OnMessageReceived event
                    // including the total bytes part of the message
                    let message = bufferResizable |> Seq.toArray
                    do! handleMessage message
                    // clear the resizable collection to be ready for the next income message
                    bufferResizable.Clear()
                    do! readingMessage buffer handleMessage
                else
                    // the message is not completed, keep reading
                    do! readingMessage buffer handleMessage
            }
        readingMessage
    innerReadingMessages (Array.zeroCreate<byte> 0x1000)

