module Compiler.BoosterTests

open System.IO
open NUnit.Framework
open FsUnit
open WebSharper.Compiler

type CollectingLogger() =
    inherit LoggerBase()
    let errors = ResizeArray<string>()
    member this.Errors = List.ofSeq errors
    override this.Error s = errors.Add s
    override this.Out _ = ()

// The booster used to send two exit codes when the options failed to parse (the error code, then 0),
// and dropped the error message.
[<Test>]
let ``Booster reports an options error once and returns one exit code`` () =
    let dir = Directory.CreateTempSubdirectory("ws-booster-test")
    try
        let config = Path.Combine(dir.FullName, "wsconfig.json")
        File.WriteAllText(config, "{ not json")
        let logger = CollectingLogger()
        match WebSharper.FSharp.Service.Program.parseCompileOptions [| "fsc.exe"; "--wsconfig:" + config |] logger with
        | Error exitCode -> exitCode |> should equal 1
        | Ok _ -> Assert.Fail "Expected the options to be rejected"
        logger.Errors |> should haveLength 1
        logger.Errors.Head |> should haveSubstring "WS9001"
        logger.Errors.Head |> should haveSubstring "Failed to parse wsconfig.json, not a valid json."
    finally
        dir.Delete(true)
