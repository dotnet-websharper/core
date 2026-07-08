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

// UseLocalMacros is intentionally exercised below to verify its deprecated alias behavior.
#nowarn "44"

module WebSharper.Core.JavaScript.Test.LocalMetaprogrammingTests

open NUnit.Framework
open WebSharper.Core.AST
open WebSharper.Compiler
open WebSharper.Compiler.CommandTools

let private mentionsSetting (err: CompilationError) =
    (string err).Contains("localMetaprogramming")

/// Runs f against a fresh Compilation with the given LocalMetaprogramming value
/// and returns the compiled errors recorded on it.
let private errorsAfter lmp (f: Compilation -> unit) =
    let c = Compilation(WebSharper.Core.Metadata.Info.Empty, true)
    c.AssemblyName <- "CurrentAsm"
    c.LocalMetaprogramming <- lmp
    f c
    c.Errors

let private currentAsmMacro = TypeDefinition { Assembly = "CurrentAsm"; FullName = "Test.Macro" }
let private refAsmMacro = TypeDefinition { Assembly = "OtherAsm"; FullName = "Test.Macro" }
let private currentAsmGen = TypeDefinition { Assembly = "CurrentAsm"; FullName = "Test.Generator" }
let private refAsmGen = TypeDefinition { Assembly = "OtherAsm"; FullName = "Test.Generator" }

[<Test>]
let Run () =

    Section "LocalMetaprogramming"

    Test "WsConfig defaults localMetaprogramming to false" {
        WsConfig.Empty.LocalMetaprogramming =? false
    }

    Test "AddJson parses localMetaprogramming" {
        WsConfig.Empty.AddJson("""{ "localMetaprogramming": true }""", "wsconfig.json", "Release").LocalMetaprogramming =? true
    }

    Test "RecognizeWebSharperArg parses --localmetaprogramming+ flag" {
        (RecognizeWebSharperArg "--localmetaprogramming+" WsConfig.Empty |> Option.map (fun c -> c.LocalMetaprogramming)) =? Some true
    }

    Test "RecognizeWebSharperArg parses --localmetaprogramming- flag" {
        (RecognizeWebSharperArg "--localmetaprogramming-" { WsConfig.Empty with LocalMetaprogramming = true } |> Option.map (fun c -> c.LocalMetaprogramming)) =? Some false
    }

    Test "UseLocalMacros is a deprecated alias for LocalMetaprogramming (get)" {
        let c = Compilation(WebSharper.Core.Metadata.Info.Empty, true)
        c.LocalMetaprogramming <- true
        c.UseLocalMacros =? true
    }

    Test "UseLocalMacros is a deprecated alias for LocalMetaprogramming (set)" {
        let c = Compilation(WebSharper.Core.Metadata.Info.Empty, true)
        c.UseLocalMacros <- true
        c.LocalMetaprogramming =? true
    }

    Test "Current-project macro lookup returns None whether gated or reflection-loaded" {
        (Compilation(WebSharper.Core.Metadata.Info.Empty, true, AssemblyName = "CurrentAsm")).GetMacroInstance(currentAsmMacro) =? None
    }

    Test "Current-project macro is gated with a directed error when localMetaprogramming is off" {
        (errorsAfter false (fun c -> c.GetMacroInstance(currentAsmMacro) |> ignore) |> List.map (snd >> mentionsSetting)) =? [ true ]
    }

    Test "Current-project macro is not gated when localMetaprogramming is on" {
        // reaches the reflection-load path instead, which fails to find the (nonexistent) test type
        (errorsAfter true (fun c -> c.GetMacroInstance(currentAsmMacro) |> ignore) |> List.map (snd >> mentionsSetting)) =? [ false ]
    }

    Test "Referenced-assembly macro load failure is silent when localMetaprogramming is off" {
        errorsAfter false (fun c -> c.GetMacroInstance(refAsmMacro) |> ignore) =? []
    }

    Test "Referenced-assembly macro load failure errors when localMetaprogramming is on" {
        (errorsAfter true (fun c -> c.GetMacroInstance(refAsmMacro) |> ignore) |> List.map (snd >> mentionsSetting)) =? [ false ]
    }

    Test "Current-project generator lookup returns None whether gated or reflection-loaded" {
        (Compilation(WebSharper.Core.Metadata.Info.Empty, true, AssemblyName = "CurrentAsm")).GetGeneratorInstance(currentAsmGen) =? None
    }

    Test "Current-project generator is gated with a directed error when localMetaprogramming is off" {
        (errorsAfter false (fun c -> c.GetGeneratorInstance(currentAsmGen) |> ignore) |> List.map (snd >> mentionsSetting)) =? [ true ]
    }

    Test "Current-project generator is not gated when localMetaprogramming is on" {
        (errorsAfter true (fun c -> c.GetGeneratorInstance(currentAsmGen) |> ignore) |> List.map (snd >> mentionsSetting)) =? [ false ]
    }

    Test "Referenced-assembly generator load failure is silent when localMetaprogramming is off" {
        errorsAfter false (fun c -> c.GetGeneratorInstance(refAsmGen) |> ignore) =? []
    }

    Test "Referenced-assembly generator load failure errors when localMetaprogramming is on" {
        (errorsAfter true (fun c -> c.GetGeneratorInstance(refAsmGen) |> ignore) |> List.map (snd >> mentionsSetting)) =? [ false ]
    }
