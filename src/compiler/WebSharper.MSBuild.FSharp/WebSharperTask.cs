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
    public sealed class WebSharperTask : ToolTask
    {
        public ITaskItem[] FscCommandLineArgs { get; set; } = new ITaskItem[] { };
        public string MSBuildProjectFullPath { get; set; }
        public string WebProjectOutputDir { get; set; }
        public string WebSharperConfigFile { get; set; }
        public string WebSharperBundleOutputDir { get; set; }
        public string WebSharperHtmlDirectory { get; set; }
        public string WebSharperProject { get; set; }
        public string WebSharperSourceMap { get; set; } = "";
        public string WebSharperErrorsAsWarnings { get; set; } = "";
        public string WebSharperDeadCodeElimination { get; set; } = "";
        public string WebSharperDownloadResources { get; set; } = "";
        public string WebSharperAnalyzeClosures { get; set; }
        public string WebSharperJsOutput { get; set; }
        public string WebSharperMinJsOutput { get; set; }
        public string WebSharperToolPath { get; set; } = "wsfsc.exe";
        public string WebSharperStandalone { get; set; } = "";

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
                if (FscCommandLineArgs.Length == 0)
                {
                    throw new Exception("Expecting FscCommandLineArgs set, needs ProvideCommandLineArgs=True");
                }

                foreach (var a in FscCommandLineArgs)
                    WriteIfSet(w, "", a);

                WriteIfSet(w, "--ws:", WebSharperProject);

                WriteIfSet(w, "--project:", MSBuildProjectFullPath);

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

                if (TryParseBool(WebSharperErrorsAsWarnings))
                    w.WriteLine("--wswarnonly");

                if (TryParseBool(WebSharperStandalone))
                    w.WriteLine("--standalone");

                if (WebProjectOutputDir != null && WebSharperProject == null)
                    w.WriteLine("--site");

                WriteIfSet(w, "--wsconfig:", WebSharperConfigFile);
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
    }
}
