module WebSharper.Compiler.WsFscServiceCommon

open System.IO.Pipes
open System.Runtime.Serialization.Formatters.Binary

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
            let bf = new BinaryFormatter()
            try
                let deserializedMessage = bf.Deserialize(pipe)
                let! finish = handleMessage deserializedMessage
                match finish with
                | Some _ -> return finish
                | None -> return! readingMessage()
            with
            | :? System.Runtime.Serialization.SerializationException ->
                return None
            | _ ->
                return! readingMessage()
        }
    readingMessage ()

