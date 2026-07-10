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
using Microsoft.Build.Framework;
using Microsoft.Build.Utilities;

namespace WebSharper.MSBuild.FSharp
{
    /// Combined mode (WebSharperExternalFSharpCompiler + WebSharperParallelCompilation): starts
    /// wsfsc --awaitdll --externalfsharpcompiler+ as a detached process and returns immediately,
    /// without waiting for it to finish. Must not be a ToolTask (which blocks in Execute() until
    /// the tool process exits) - the whole point is that CoreCompile (the real fsc) gets to run
    /// concurrently with this launched wsfsc. WebSharperJoinTask waits for it later.
    public sealed class WebSharperLaunchTask : Task, IWebSharperTaskArgs
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

        [Output]
        public int LaunchedProcessId { get; set; }

        public override bool Execute()
        {
            try
            {
                var atFileName = Path.Combine(Path.GetTempPath(), Path.GetTempFileName());
                WebSharperArgsWriter.WriteAtFileName(this, atFileName, forceAwaitDll: true, forceExternalFSharpCompiler: true);

                // Delete any stale output from a previous build so the launched wsfsc's
                // --awaitdll wait loop can never mistake it for a freshly-produced assembly.
                var dllOutput = WebSharperArgsWriter.GetExpectedAssemblyOutput(FscCommandLineArgs, MSBuildProjectFullPath);
                if (dllOutput != null && File.Exists(dllOutput))
                    File.Delete(dllOutput);

                var logPath = WebSharperArgsWriter.GetLaunchLogPath(dllOutput);
                var logWriter = new StreamWriter(logPath, append: false) { AutoFlush = true };

                var startInfo = new ProcessStartInfo(WebSharperToolPath, "@" + atFileName)
                {
                    UseShellExecute = false,
                    CreateNoWindow = true,
                    WindowStyle = ProcessWindowStyle.Hidden,
                    RedirectStandardOutput = true,
                    RedirectStandardError = true,
                };
                var proc = new Process { StartInfo = startInfo, EnableRaisingEvents = true };
                DataReceivedEventHandler onData = (s, e) =>
                {
                    if (e.Data != null)
                    {
                        lock (logWriter)
                            logWriter.WriteLine(e.Data);
                    }
                };
                proc.OutputDataReceived += onData;
                proc.ErrorDataReceived += onData;
                // Deliberately never explicitly disposes logWriter: Process.Exited can fire
                // before all pending OutputDataReceived/ErrorDataReceived callbacks for the
                // last lines of output (e.g. an unhandled exception dump right before exit)
                // have actually been dispatched, so closing the writer there raced with - and
                // sometimes silently dropped - exactly the output most worth keeping. AutoFlush
                // already makes every line durable on write; the handle lives until this
                // MSBuild node process itself exits, which is an acceptable, bounded leak in
                // exchange for never losing a launched process's error output - confirmed via
                // combined-mode end-to-end testing (this exact race was hiding a real failure).
                proc.Start();
                proc.BeginOutputReadLine();
                proc.BeginErrorReadLine();
                LaunchedProcessId = proc.Id;
                WebSharperLaunchedProcesses.Register(proc);
                return true;
            }
            catch (Exception e)
            {
                Log.LogErrorFromException(e, true);
                return false;
            }
        }
    }
}
