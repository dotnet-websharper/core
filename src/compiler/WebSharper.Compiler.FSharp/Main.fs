namespace WebSharper.Compiler.FSharp

open Microsoft.FSharp.Compiler.SourceCodeServices
open Microsoft.FSharp.Compiler.SimpleSourceCodeServices

open System.IO

module M = WebSharper.Core.Metadata

type internal FSIFD = FSharpImplementationFileDeclaration

type WebSharperFSharpCompiler(logger) =
    let checker = FSharpChecker.Create(keepAssemblyContents = true)
    let service = SimpleSourceCodeServices()

    member this.Compile (prevMeta, argv, path: string, aR : IntelliFactory.Core.AssemblyResolution.AssemblyResolver) =

        let started = System.DateTime.Now

        let projectOptions =
            try
                checker.GetProjectOptionsFromCommandLineArgs(path, argv)
            with e ->
//                printfn "Error: %s" e.Message
                failwithf "Error reading project options: %s" path

//        System.IO.File.AppendAllLines (
//            @"C:\repo\websharper.csharp\projectoptions.txt",
//            [|
//                "projectOptionsFromArgs:"
//                sprintf "%A" projectOptions
//                ""
//            |]
//        )

//        let projectOptions =
////            checker.GetProjectOptionsFromProjectFile path
//            FSharpChecker.Create().GetProjectOptionsFromProjectFileWithoutReferences path
//
//        System.IO.File.AppendAllLines (
//            @"C:\repo\websharper.csharp\projectoptions.txt",
//            [|
//                "projectOptionsFromProject:"
//                sprintf "%A" projectOptions
//                ""
//            |]
//        )

//        let ended = System.DateTime.Now
//        logger <| sprintf "Creating project options: %A" (ended - started)
//        let started = ended 
//

//        printfn "Done. Checking..."

        let checkFileResults = 
            projectOptions
            |> checker.ParseAndCheckProject 
            |> Async.RunSynchronously

        let ended = System.DateTime.Now
        logger <| sprintf "Checking project: %A" (ended - started)
        let started = ended 

    
//        printfn "Done."

        if checkFileResults.HasCriticalErrors then
            for err in checkFileResults.Errors do
                printfn "%s" err.Message
            failwith "FSharp compilation error"
    
//        printfn "No errors. Parsing..."

//        checker.StartBackgroundCompile projectOptions

        let refMeta =   
            match prevMeta with
            | None -> M.empty
            | Some dep -> dep  
        
        let comp = 
            WebSharper.Compiler.FSharp.Translator.transformAssembly refMeta
                (Path.GetFileNameWithoutExtension path)
                checkFileResults

        let ended = System.DateTime.Now
        logger <| sprintf "Parsing with FCS: %A" (ended - started)
        let started = ended 

//        let comp =
//            match prevMeta with
//            | None -> comp
//            | Some dep -> WebSharper.Core.Metadata.union dep comp  


        let ended = System.DateTime.Now
        logger <| sprintf "Metadata union: %A" (ended - started)
        let started = ended 

//        printfn "Done. Compiling..."
//        checker.WaitForBackgroundCompile()

//        aR.Wrap <| fun () -> WebSharper.Compiler.ToJavaScript.ToJavaScript.CompileFull comp
        WebSharper.Compiler.ToJavaScript.ToJavaScript.CompileFull comp
            
        let ended = System.DateTime.Now
        logger <| sprintf "Transforming: %A" (ended - started)
        let started = ended 

        comp

//    member this.CompileWithArgs (prevMeta, argv, path: string) =
//
//        let started = System.DateTime.Now
//
////        let projectOptions =
////            checker.GetProjectOptionsFromProjectFileWithoutReferences path
//
//        let projectOptions =
//            try
//                checker.GetProjectOptionsFromCommandLineArgs(path, argv)
//            with e ->
////                printfn "Error: %s" e.Message
//                failwithf "Error reading project options: %s" path
//
//        let ended = System.DateTime.Now
//        logger <| sprintf "Creating project options: %A" (ended - started)
//        let started = ended 
//
//
////        printfn "Done. Checking..."
//
//        let checkFileResults = 
//            projectOptions
//            |> checker.ParseAndCheckProject 
//            |> Async.RunSynchronously
//
//        let ended = System.DateTime.Now
//        logger <| sprintf "Checking project: %A" (ended - started)
//        let started = ended 
//
//    
////        printfn "Done."
//
//        if checkFileResults.HasCriticalErrors then
//            for err in checkFileResults.Errors do
//                printfn "%s" err.Message
//            failwith "FSharp compilation error"
//    
////        printfn "No errors. Parsing..."
//
////        checker.StartBackgroundCompile projectOptions
//
//        let refMeta =   
//            match prevMeta with
//            | None -> M.empty
//            | Some dep -> dep  
//        
//        let comp = 
//            WebSharper.Compiler.FSharp.Translator.transformAssembly refMeta
//                (Path.GetFileNameWithoutExtension path)
//                checkFileResults
//
//        let ended = System.DateTime.Now
//        logger <| sprintf "Parsing with FCS: %A" (ended - started)
//        let started = ended 
//
////        let comp =
////            match prevMeta with
////            | None -> comp
////            | Some dep -> WebSharper.Core.Metadata.union dep comp  
//
//
//        let ended = System.DateTime.Now
//        logger <| sprintf "Metadata union: %A" (ended - started)
//        let started = ended 
//
////        printfn "Done. Compiling..."
////        checker.WaitForBackgroundCompile()
//
//        WebSharper.Compiler.ToJavaScript.ToJavaScript.CompileFull comp
//            
//        let ended = System.DateTime.Now
//        logger <| sprintf "Transforming: %A" (ended - started)
//        let started = ended 
//
//        comp
//
//    member this.CompileFSharp (argv, path: string) =
////        use proc =
////            new System.Diagnostics.Process(
////                StartInfo = 
////                    System.Diagnostics.ProcessStartInfo(
////                        @"C:\Program Files (x86)\Microsoft SDKs\F#\3.0\Framework\v4.0\fsc.exe", 
////                        argv |> Seq.map (fun a -> "\"" + a + "\"") |> String.concat " "
////                    )
////            )
////        proc.Start() |> ignore
////        proc.WaitForExit()
////        if proc.ExitCode <> 0 then
////            failwith "F# compilation error"
//        let dir = Path.GetDirectoryName path
//        Directory.CreateDirectory dir |> ignore
//        let messages, res = service.Compile(Array.append [| "fsc.exe" |] argv)
//        let errors = messages |> Array.filter (fun e -> e.Severity = Microsoft.FSharp.Compiler.FSharpErrorSeverity.Error)
//        if errors.Length > 0 then
//            eprintfn "%A" errors
//            failwith "F# compilation error"



//        let projectOptions =
//            try
//                checker.GetProjectOptionsFromCommandLineArgs(path, args)
//            with e ->
////                printfn "Error: %s" e.Message
//                failwithf "Error reading project options: %s" path
//
//        let checkFileResults = 
//            projectOptions
//            |> checker.ParseAndCheckProject 
//            |> Async.RunSynchronously
//    
////        printfn "Done."
//
//        if checkFileResults.HasCriticalErrors then
//            for err in checkFileResults.Errors do
//                printfn "%s" err.Message
//            failwith "FSharp compilation error"
//    
////        printfn "No errors. Parsing..."
//
//        checker.StartBackgroundCompile projectOptions
//
////        printfn "Done. Compiling..."
//
//        checker.WaitForBackgroundCompile()
