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
open WebSharper
open WebSharper.Compiler
module FE = FrontEnd

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
    member val DocumentationFile = "" with get, set
    member val ZafirToolPath = "ZafirCs.exe" with get, set
    member val DefineConstants = "" with get, set
    member val NoStandardLib = "" with get, set
    member val Sources : ITaskItem [] = Array.empty with get, set
    member val TargetType = "" with get, set 
    member val NoConfig = "" with get, set 
    member val DebugType = "" with get, set 
    member val SubsystemVersion = "" with get, set 

    override this.ToolName = "ZafirCs.exe"

    override this.GenerateFullPathToTool() = this.ZafirToolPath

    override this.GenerateCommandLineCommands() =
        let builder = CommandLineBuilder()
                
        if bool.TryParse this.NoConfig ||> (&&) then
            builder.AppendSwitch "/noconfig"

        if bool.TryParse this.NoStandardLib ||> (&&) then
            builder.AppendSwitch "/nostdlib+"

        builder.AppendSwitchIfNotNull("/target:", this.TargetType) 

        builder.AppendSwitchIfNotNull("/debug:", this.DebugType) 

        builder.AppendSwitchIfNotNull("/subsystemversion:", this.SubsystemVersion) 

        builder.AppendSwitchIfNotNull("/doc:", this.DocumentationFile) 

        builder.AppendSwitchIfNotNull("/out:", this.OutputAssembly) 

        builder.AppendSwitchIfNotNull("--ws:", this.WebSharperProject)

        builder.AppendSwitchIfNotNull("--project:", this.MSBuildProjectFullPath)

        builder.AppendSwitchIfNotNull("/keyfile:", this.KeyOriginatorFile)

        builder.AppendSwitchIfNotNull("--wsoutput:", this.WebProjectOutputDir)
        builder.AppendSwitchIfNotNull("--wsoutput:", this.WebSharperBundleOutputDir)
        builder.AppendSwitchIfNotNull("--wsoutput:", this.WebSharperHtmlDirectory)

        if bool.TryParse this.WebSharperErrorsAsWarnings ||> (&&) then
            builder.AppendSwitch "--wswarnonly"

        match bool.TryParse this.WebSharperDeadCodeElimination with
        | true, false ->
            builder.AppendSwitch "--dce-"
        | _ -> ()

        if bool.TryParse this.WebSharperSourceMap ||> (&&) then
            builder.AppendSwitch "--jsmap"

        if this.WebProjectOutputDir <> null && this.WebSharperProject = null then
            builder.AppendSwitch("--site")

        builder.AppendSwitchIfNotNull("/define:", this.DefineConstants)

        for r in this.References do
            builder.AppendSwitchIfNotNull("/reference:", r)
            
        for s in this.Sources do
            builder.AppendFileNameIfNotNull s
         
        string builder
