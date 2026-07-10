// $begin{copyright}
//
// This file is part of WebSharper
//
// Copyright (c) 2008-2018 IntelliFactory
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

module WebSharper.Core.JavaScript.Test.ParallelCompilationTests

open NUnit.Framework
open WebSharper.Compiler.CommandTools

[<Test>]
let Run () =

    Section "parallelCompilation / verifyRPCSignatures / externalFSharpCompiler"

    Test "WsConfig defaults parallelCompilation to false" {
        WsConfig.Empty.ParallelCompilation =? false
    }

    Test "AddJson parses parallelCompilation" {
        WsConfig.Empty.AddJson("""{ "parallelCompilation": true }""", "wsconfig.json", "Release").ParallelCompilation =? true
    }

    Test "RecognizeWebSharperArg parses --parallelcompilation+/- flags" {
        (RecognizeWebSharperArg "--parallelcompilation+" WsConfig.Empty |> Option.map (fun c -> c.ParallelCompilation)) =? Some true
        (RecognizeWebSharperArg "--parallelcompilation-" { WsConfig.Empty with ParallelCompilation = true } |> Option.map (fun c -> c.ParallelCompilation)) =? Some false
    }

    Test "WsConfig defaults verifyRPCSignatures to false" {
        WsConfig.Empty.VerifyRPCSignatures =? false
    }

    Test "AddJson parses verifyRPCSignatures" {
        WsConfig.Empty.AddJson("""{ "verifyRPCSignatures": true }""", "wsconfig.json", "Release").VerifyRPCSignatures =? true
    }

    Test "RecognizeWebSharperArg parses --verifyrpcsignatures+/- flags" {
        (RecognizeWebSharperArg "--verifyrpcsignatures+" WsConfig.Empty |> Option.map (fun c -> c.VerifyRPCSignatures)) =? Some true
        (RecognizeWebSharperArg "--verifyrpcsignatures-" { WsConfig.Empty with VerifyRPCSignatures = true } |> Option.map (fun c -> c.VerifyRPCSignatures)) =? Some false
    }

    Test "WsConfig defaults externalFSharpCompiler to false" {
        WsConfig.Empty.ExternalFSharpCompiler =? false
    }

    Test "RecognizeWebSharperArg parses --externalfsharpcompiler+/- flags" {
        (RecognizeWebSharperArg "--externalfsharpcompiler+" WsConfig.Empty |> Option.map (fun c -> c.ExternalFSharpCompiler)) =? Some true
        (RecognizeWebSharperArg "--externalfsharpcompiler-" { WsConfig.Empty with ExternalFSharpCompiler = true } |> Option.map (fun c -> c.ExternalFSharpCompiler)) =? Some false
    }

    let rejectsExternalFSharpCompilerInJson =
        try
            WsConfig.Empty.AddJson("""{ "externalFSharpCompiler": true }""", "wsconfig.json", "Release") |> ignore
            false
        with _ ->
            true

    Test "externalFSharpCompiler is MSBuild-only: AddJson rejects it in wsconfig.json" {
        rejectsExternalFSharpCompilerInJson =? true
    }

    Test "WsConfig defaults awaitDll to false" {
        WsConfig.Empty.AwaitDll =? false
    }

    Test "RecognizeWebSharperArg parses --awaitdll+/- flags" {
        (RecognizeWebSharperArg "--awaitdll+" WsConfig.Empty |> Option.map (fun c -> c.AwaitDll)) =? Some true
        (RecognizeWebSharperArg "--awaitdll-" { WsConfig.Empty with AwaitDll = true } |> Option.map (fun c -> c.AwaitDll)) =? Some false
    }

    let rejectsAwaitDllInJson =
        try
            WsConfig.Empty.AddJson("""{ "awaitDll": true }""", "wsconfig.json", "Release") |> ignore
            false
        with _ ->
            true

    Test "awaitDll is MSBuild-only: AddJson rejects it in wsconfig.json" {
        rejectsAwaitDllInJson =? true
    }
