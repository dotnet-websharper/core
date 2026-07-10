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

using System;
using System.Diagnostics;
using System.IO;
using System.Reflection;
using Microsoft.Build.Framework;
using Microsoft.Build.Utilities;

namespace WebSharper.MSBuild.FSharp
{
    public sealed class WebSharperTask : ToolTask, IWebSharperTaskArgs
    {
        public ITaskItem[] FscCommandLineArgs { get; set; } = new ITaskItem[] { };
        public string Configuration { get; set; } = "";
        public string MSBuildProjectFullPath { get; set; }
        public string WebProjectOutputDir { get; set; }
        public string WebSharperConfigFile { get; set; }
        public string WebSharperBundleOutputDir { get; set; }
        public string WebSharperHtmlDirectory { get; set; }
        public string WebSharperProject { get; set; }
        public string WebSharperSourceMap { get; set; } = "";
        public string WebSharperTypeScriptDeclaration { get; set; } = "";
        public string WebSharperTypeScriptOutput { get; set; } = "";
        public string WebSharperErrorsAsWarnings { get; set; } = "";
        public string WebSharperDeadCodeElimination { get; set; } = "";
        public string WebSharperDownloadResources { get; set; } = "";
        public string WebSharperAnalyzeClosures { get; set; }
        public string WebSharperJsOutput { get; set; }
        public string WebSharperMinJsOutput { get; set; }
        public string WebSharperTimingLog { get; set; }
        public string WebSharperToolPath { get; set; } = "wsfsc.exe";
        public string WebSharperStandalone { get; set; } = "";
        public string WebSharperUseLocalReflection { get; set; } = "";
        public string WebSharperVerifyRPCSignatures { get; set; } = "";
        public string WebSharperParallelCompilation { get; set; } = "";
        public string WebSharperExternalFSharpCompiler { get; set; } = "";

        protected override string ToolName => Path.GetFileName(WebSharperToolPath);

        protected override string GenerateFullPathToTool()
        {
            return WebSharperToolPath;
        }

        protected override string GenerateCommandLineCommands()
        {
            var atFileName = Path.Combine(Path.GetTempPath(), Path.GetTempFileName());
            WebSharperArgsWriter.WriteAtFileName(this, atFileName, forceAwaitDll: false, forceExternalFSharpCompiler: false);
            var builder = new CommandLineBuilder();
            builder.AppendFileNameIfNotNull("@" + atFileName);
            return builder.ToString();
        }

        protected override bool HandleTaskExecutionErrors()
        {
            var dllOutput = WebSharperArgsWriter.GetExpectedAssemblyOutput(FscCommandLineArgs, MSBuildProjectFullPath);
            WebSharperArgsWriter.MarkOutputFailed(dllOutput);
            return base.HandleTaskExecutionErrors();
        }
    }
}
