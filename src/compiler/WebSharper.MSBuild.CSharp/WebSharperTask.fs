// $begin{copyright}
//
// This file is part of WebSharper
//
// Copyright (c) 2008-2016 IntelliFactory
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

namespace WebSharper.MSBuild.CSharp

open System
open System.Diagnostics
open System.IO
open System.Reflection
open Microsoft.Build.Framework
open Microsoft.Build.Utilities

[<Sealed>]
type WebSharperTask() =
    inherit ToolTask()

    member val EmbeddedResources : ITaskItem [] = Array.empty with get, set
    member val Configuration = "" with get, set
    member val OutputAssembly : ITaskItem = null with get, set
    member val References : ITaskItem [] = Array.empty with get, set
    member val KeyOriginatorFile : string = null with get, set
    member val MSBuildProjectFullPath : string = null with get, set
    member val MSBuildProjectDirectory : string = null with get, set
    member val Name = "" with get, set
    member val OutputPath : string = null with get, set
    member val WebProjectOutputDir : string = null with get, set
    member val WebSharperBundleOutputDir : string = null with get, set
    member val WebSharperHtmlDirectory : string = null with get, set
    member val WebSharperProject : string = null with get, set
    member val WebSharperSourceMap = "" with get, set
    member val WebSharperTypeScriptDeclaration = "" with get, set
    member val WebSharperErrorsAsWarnings = "" with get, set
    member val WebSharperDeadCodeElimination = "" with get, set
    member val WebSharperDownloadResources = "" with get, set
    member val WebSharperAnalyzeClosures : string = null with get, set
    member val DocumentationFile = "" with get, set
    member val ZafirToolPath = "ZafirCs.exe" with get, set
    member val DefineConstants = "" with get, set
    member val NoStandardLib = "" with get, set
    member val Sources : ITaskItem [] = Array.empty with get, set
    member val TargetType = "" with get, set 
    member val NoConfig = "" with get, set 
    member val DebugType = "" with get, set 
    member val SubsystemVersion = "" with get, set 
    member val LangVersion = "" with get, set 

    override this.ToolName = "ZafirCs.exe"

    override this.GenerateFullPathToTool() = this.ZafirToolPath

    member this.WriteAtFileName(filename: string) =
        use f = File.OpenWrite(filename)
        use w = new StreamWriter(f)
                
        if bool.TryParse this.NoConfig ||> (&&) then
            w.WriteLine "/noconfig"

        if bool.TryParse this.NoStandardLib ||> (&&) then
            w.WriteLine "/nostdlib+"

        let writeStringIfSet (name: string) (value: string) =
            if not (String.IsNullOrEmpty value) then
                w.WriteLine(name + value)

        let writeItemIfSet (name: string) (value: ITaskItem) =
            match value with
            | null -> ()
            | value -> writeStringIfSet name value.ItemSpec

        writeStringIfSet "/target:" this.TargetType

        writeStringIfSet "/debug:" this.DebugType

        writeStringIfSet "/subsystemversion:" this.SubsystemVersion

        writeStringIfSet "/langVersion:" this.LangVersion

        writeStringIfSet "/doc:" this.DocumentationFile

        writeItemIfSet "/out:" this.OutputAssembly

        writeStringIfSet "--ws:" this.WebSharperProject

        writeStringIfSet "--project:" this.MSBuildProjectFullPath

        writeStringIfSet "/keyfile:" this.KeyOriginatorFile

        writeStringIfSet "--wsoutput:" this.WebProjectOutputDir
        writeStringIfSet "--wsoutput:" this.WebSharperBundleOutputDir
        writeStringIfSet "--wsoutput:" this.WebSharperHtmlDirectory

        if bool.TryParse this.WebSharperErrorsAsWarnings ||> (&&) then
            w.WriteLine "--wswarnonly"

        match bool.TryParse this.WebSharperDeadCodeElimination with
        | true, false ->
            w.WriteLine "--dce-"
        | _ -> ()

        if bool.TryParse this.WebSharperDownloadResources ||> (&&) then
            w.WriteLine "--dlres"

        writeStringIfSet "--closures:" this.WebSharperAnalyzeClosures

        if bool.TryParse this.WebSharperSourceMap ||> (&&) then
            w.WriteLine "--jsmap"

        if this.WebProjectOutputDir <> null && this.WebSharperProject = null then
            w.WriteLine "--site"

        writeStringIfSet "/define:" this.DefineConstants

        for r in this.References do
            writeItemIfSet "/reference:" r
            
        for s in this.Sources do
            writeItemIfSet "" s

    override this.GenerateCommandLineCommands() =
        let atFileName = Path.Combine(Path.GetTempPath(), Path.GetTempFileName())
        this.WriteAtFileName atFileName
        let builder = CommandLineBuilder()
        builder.AppendFileNameIfNotNull ("@" + atFileName)
        string builder
