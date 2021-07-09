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

namespace WebSharper.Compiler

module C = Commands

module HtmlCommand =

    type Mode =
        | Debug
        | Release

    type Config =
        {
            MainAssemblyPath : string
            Mode : Mode
            OutputDirectory : string
            ProjectDirectory : string
            ReferenceAssemblyPaths : list<string>
            UnpackSourceMap : bool
            UnpackTypeScript : bool
            DownloadResources: bool
        }

        static member Create(mainAssemblyPath) =
            {
                MainAssemblyPath = mainAssemblyPath
                Mode = Debug
                OutputDirectory = "."
                ProjectDirectory = "."
                ReferenceAssemblyPaths = []
                UnpackSourceMap = false
                UnpackTypeScript = false
                DownloadResources = false
            }

    exception BadOptions of string

    let GetErrors config =
        [
            if C.NoDir config.ProjectDirectory then
                yield "-project parameter invalid (not such directory): " + string config.ProjectDirectory
            if C.NoFile config.MainAssemblyPath then
                yield "-site parameter invalid (no such file)"
            for r in config.ReferenceAssemblyPaths do
                if C.NoFile r then
                    yield "-ref parameter invalid (no such file): " + string r
        ]

    type IHtmlCommand =
        abstract Execute : C.Environment * Config -> C.Result

    let mutable implementationInstance = None : IHtmlCommand option
    
    let Exec env (config: Config) =
        
        //let assemblyResolveHandler = 
        //    System.ResolveEventHandler(fun _ e ->

        //        let assemblyName = AssemblyName(e.Name).Name
        //         These are the dependencies of WebSharper.Offline.Sitelets
        //         They must be exact versions by strict nuget dependency
        //        match assemblyName with 
        //        | "WebSharper.JavaScript" 
        //        | "WebSharper.Main" 
        //        | "WebSharper.Collections"
        //        | "WebSharper.Web"
        //        | "WebSharper.Sitelets"
        //            ->
        //            let path =
        //                config.ReferenceAssemblyPaths |> List.tryFind (fun r ->
        //                    Path.GetFileNameWithoutExtension(r) = assemblyName
        //                )   
        //            match path with
        //            | Some p ->
        //                printfn "Loaded from project refs: %s" p
        //                System.Reflection.Assembly.Load(File.ReadAllBytes(p))
        //                System.Reflection.Assembly.LoadFile(p)

        //            | None ->
        //                failwithf "Assembly not referenced, needed for Html projects: %s" assemblyName
        //        | _ -> 
        //            let p = Path.Combine(compilerDir, assemblyName + ".dll")
        //            if File.Exists(p) then
        //                printfn "Loaded from combpiler folder: %s" p
        //                System.Reflection.Assembly.LoadFile(p)
        //            else
        //                printfn "Tried to load but not found: %s" p
        //                null
        //    )

        let cmd =
            match implementationInstance with
            | Some cmd -> cmd
            | None ->
        
                let referencedAsmNames =
                    config.ReferenceAssemblyPaths
                    |> Seq.map (fun i -> 
                        let n = Path.GetFileNameWithoutExtension(i)
                        n, i
                    )
                    |> dict
                
                let asmList = System.AppDomain.CurrentDomain.GetAssemblies()
                printfn "In main context: %A" (asmList |> Array.map (fun a -> a.FullName))

                [|
                    "WebSharper.JavaScript" 
                    "WebSharper.Main" 
                    "WebSharper.Collections"
                    "WebSharper.Web"
                    "WebSharper.Sitelets"
                |]
                |> Array.iter (fun assemblyName ->
                    match referencedAsmNames.TryGetValue assemblyName with
                    | true, p ->
                        printfn "Loaded from project refs: %s" p
                        //System.Reflection.Assembly.Load(File.ReadAllBytes(p)) |> ignore
                        System.Reflection.Assembly.LoadFile(p) |> ignore   
                    | _ ->
                        failwithf "Assembly not referenced, needed for Html projects: %s" assemblyName   
                )

                //let thisAsm = typeof<IHtmlCommand>.Assembly
                //let compilerDir = Path.GetDirectoryName(thisAsm.Location)

                let cmdAssemblyPath =
                    let thisPath = typeof<IHtmlCommand>.Assembly.Location
                    Path.Combine(Path.GetDirectoryName(thisPath), "WebSharper.Sitelets.Offline.dll")
                let asm = System.Reflection.Assembly.LoadFile(cmdAssemblyPath)

                //let offlineSiteletsAsm =
                //    let n = thisAsm.GetName()
                //    n.Name <- "WebSharper.Sitelets.Offline"
                //    n.FullName

                //let asm = System.Reflection.Assembly.Load(offlineSiteletsAsm)
                let tN = "WebSharper.Sitelets.Offline.HtmlCommand"
                let t = asm.GetType(tN, throwOnError = true)
                    
                let cmd = Activator.CreateInstance(t) :?> IHtmlCommand

                let asmList = System.AppDomain.CurrentDomain.GetAssemblies()
                printfn "And now: %A" (asmList |> Array.map (fun a -> a.FullName))

                implementationInstance <- Some cmd

                cmd

        //System.AppDomain.CurrentDomain.add_AssemblyResolve(assemblyResolveHandler)

        let res = cmd.Execute(env, config)   
        
        //System.AppDomain.CurrentDomain.remove_AssemblyResolve(assemblyResolveHandler)

        res

    let Parse (args: list<string>) =
        let trim (s: string) =
            let s = s.Trim()
            if s.StartsWith "\"" && s.EndsWith "\"" then
                s.Substring(1, s.Length - 2).Trim()
            else
                s
        let setMode opts (mode: string) =
            let m = mode.Trim().ToLower()
            let oMode =
                if m.StartsWith("debug") then
                    Debug
                elif m.StartsWith("release") then
                    Release
                else
                    BadOptions ("Invalid mode option: " + mode +
                        ". User either debug or release.")
                    |> raise
            { opts with Mode = oMode}
        let setOutputDirectory opts path =
            { opts with OutputDirectory = path }
        let setProjectDirectory opts path =
            { opts with ProjectDirectory = path }
        let addReferenceFile opts path =
            { opts with ReferenceAssemblyPaths = path :: opts.ReferenceAssemblyPaths }
        let setSourceAssembly opts path =
            { opts with MainAssemblyPath = path}
        let rec proc opts xs =
            match xs with
            | [] ->
                opts
            | "-project" :: d :: xs ->
                proc (setProjectDirectory opts (trim d)) xs
            | "-mode" :: f :: xs ->
                proc (setMode opts (trim f)) xs
            | "-ref" :: f :: xs ->
                proc (addReferenceFile opts (trim f)) xs
            | "-out" :: f :: xs ->
                proc (setOutputDirectory opts (trim f)) xs
            | "-site" ::f :: xs ->
                proc (setSourceAssembly opts (trim f)) xs
            | "-sm" :: xs ->
                proc { opts with UnpackSourceMap = true } xs
            | "-dts" :: xs ->
                proc { opts with UnpackTypeScript = true } xs
            | x :: xs ->
                let x =
                    match x with
                    | "-o" -> "-out"
                    | "-s" -> "-site"
                    | _ -> raise (BadOptions <| "Invalid parameter: " + x)
                proc opts (x :: xs)
        match args with
        | "sitelets" :: args
        | "html" :: args ->
            try
                let def = Config.Create("")
                let cfg = proc def args
                match GetErrors cfg with
                | [] -> C.Parsed cfg
                | errors -> C.ParseFailed errors
            with BadOptions reason ->
                C.ParseFailed [reason]
        | _ -> C.NotRecognized

    let Description =
        "generates static sites from sitelet assemblies"

    let Usage =
        [
            "Usage: WebSharper.exe html [OPTIONS] [INPUTS]"
            "-mode <mode>    Either Debug([-].*)? or Release([-].*)? (defaults to Debug)."
            "-ref <file>     Path to a reference assembly."
            "-out <dir>      Path to the output directory. Short form: -o"
            "-project <dir>  Path to the project directory."
            "-site <file>    Path to the assembly containing the web site. Short form: -s"
            "-sm             Unpack source maps and source files"
            //"-dts            Unpack TypeScript declaration files"
        ]
        |> String.concat System.Environment.NewLine

    let Instance =
        C.DefineCommand<Config> "html" Description Usage Parse Exec
