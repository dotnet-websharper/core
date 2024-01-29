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

namespace WebSharper.MSBuild.CSharp
{
    public sealed class WebSharperTask : ToolTask
    {
        public ITaskItem[] EmbeddedResources { get; set; } = new ITaskItem[] { };
        public string Configuration { get; set; } = "";
        public ITaskItem OutputAssembly { get; set; }
        public ITaskItem[] References { get; set; } = new ITaskItem[] { };
        public string KeyOriginatorFile { get; set; }
        public string MSBuildProjectFullPath { get; set; }
        public string MSBuildProjectDirectory { get; set; }
        public string Name { get; set; } = "";
        public string OutputPath { get; set; }
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
        public string DocumentationFile { get; set; } = "";
        public string WebSharperToolPath { get; set; } = "wscsc.exe";
        public string DefineConstants { get; set; } = "";
        public string NoStandardLib { get; set; } = "";
        public ITaskItem[] Sources { get; set; } = new ITaskItem[] { };
        public string TargetType { get; set; } = "";
        public string NoConfig { get; set; } = "";
        public string DebugType { get; set; } = "";
        public string SubsystemVersion { get; set; } = "";
        public string LangVersion { get; set; } = "";

        protected override string ToolName => Path.GetFileName(WebSharperToolPath);

        protected override string GenerateFullPathToTool()
        {
            return WebSharperToolPath;
        }

        private void WriteAtFileName(string filename)
        {
            using (FileStream f = File.OpenWrite(filename))
            using (StreamWriter w = new StreamWriter(f))
            {
                if (TryParseBool(NoConfig))
                    w.WriteLine("/noconfig");

                if (TryParseBool(NoStandardLib))
                    w.WriteLine("/nostdlib+");

                WriteIfSet(w, "/target:", TargetType);

                WriteIfSet(w, "/debug:", DebugType);

                WriteIfSet(w, "/subsystemversion:", SubsystemVersion);

                WriteIfSet(w, "/langVersion:", LangVersion);

                WriteIfSet(w, "/doc:", DocumentationFile);

                WriteIfSet(w, "/out:", OutputAssembly);

                WriteIfSet(w, "--ws:", WebSharperProject);

                WriteIfSet(w, "--project:", MSBuildProjectFullPath);

                WriteIfSet(w, "/keyfile:", KeyOriginatorFile);

                WriteIfSet(w, "--wsoutput:", WebProjectOutputDir);
                WriteIfSet(w, "--wsoutput:", WebSharperBundleOutputDir);
                WriteIfSet(w, "--wsoutput:", WebSharperHtmlDirectory);

                if (bool.TryParse(WebSharperDeadCodeElimination, out bool parsed) && !parsed)
                    w.WriteLine("--dce-");

                if (TryParseBool(WebSharperDownloadResources))
                    w.WriteLine("--dlres");

                WriteIfSet(w, "--closures:", WebSharperAnalyzeClosures);

                WriteIfSet(w, "--jsoutput:", WebSharperJsOutput);
                WriteIfSet(w, "--minjsoutput:", WebSharperMinJsOutput);

                if (TryParseBool(WebSharperSourceMap))
                    w.WriteLine("--jsmap");

                if (TryParseBool(WebSharperTypeScriptDeclaration))
                    w.WriteLine("--dts");

                if (TryParseBool(WebSharperTypeScriptOutput))
                    w.WriteLine("--ts");

                if (TryParseBool(WebSharperErrorsAsWarnings))
                    w.WriteLine("--wswarnonly");

                if (WebProjectOutputDir != null && WebSharperProject == null)
                    w.WriteLine("--site");

                WriteIfSet(w, "--wsconfig:", WebSharperConfigFile);

                WriteIfSet(w, "/define:", DefineConstants);

                foreach (var r in References)
                    WriteIfSet(w, "/reference:", r);

                foreach (var s in Sources)
                    WriteIfSet(w, "", s);
            }
        }

        protected override string GenerateCommandLineCommands()
        {
            var atFileName = Path.Combine(Path.GetTempPath(), Path.GetTempFileName());
            WriteAtFileName(atFileName);
            var builder = new CommandLineBuilder();
            builder.AppendFileNameIfNotNull("@" + atFileName);
            return builder.ToString();
        }

        private static bool TryParseBool(string s)
        {
            return bool.TryParse(s, out bool parsed) && parsed;
        }

        private void WriteIfSet(TextWriter w, string name, string value)
        {
            if (!string.IsNullOrEmpty(value))
                w.WriteLine(name + value);
        }

        private void WriteIfSet(TextWriter w, string name, ITaskItem value)
        {
            if (value != null)
                WriteIfSet(w, name, value.ItemSpec);
        }

        protected override bool HandleTaskExecutionErrors()
        {
            var dllOutput = Path.Combine(Path.GetDirectoryName(MSBuildProjectFullPath).Trim(), OutputAssembly.ItemSpec.Trim());
            if (File.Exists(dllOutput))
            {
                if (File.Exists(dllOutput + ".failed"))
                {
                    File.Delete(dllOutput + ".failed");
                }
                File.Move(dllOutput, dllOutput + ".failed");
            }
            return base.HandleTaskExecutionErrors();
        }
    }
}
