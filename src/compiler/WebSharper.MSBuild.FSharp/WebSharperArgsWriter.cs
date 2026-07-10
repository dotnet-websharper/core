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
using System.IO;
using Microsoft.Build.Framework;

namespace WebSharper.MSBuild.FSharp
{
    /// Common property surface shared by WebSharperTask (blocking, ToolTask) and
    /// WebSharperLaunchTask (non-blocking, plain Task) so both can write the same
    /// wsfsc @file argument format without a shared base class (ToolTask can't be
    /// mixed with a non-blocking task base).
    public interface IWebSharperTaskArgs
    {
        ITaskItem[] FscCommandLineArgs { get; set; }
        string Configuration { get; set; }
        string MSBuildProjectFullPath { get; set; }
        string WebProjectOutputDir { get; set; }
        string WebSharperConfigFile { get; set; }
        string WebSharperBundleOutputDir { get; set; }
        string WebSharperHtmlDirectory { get; set; }
        string WebSharperProject { get; set; }
        string WebSharperSourceMap { get; set; }
        string WebSharperTypeScriptDeclaration { get; set; }
        string WebSharperTypeScriptOutput { get; set; }
        string WebSharperErrorsAsWarnings { get; set; }
        string WebSharperDeadCodeElimination { get; set; }
        string WebSharperDownloadResources { get; set; }
        string WebSharperAnalyzeClosures { get; set; }
        string WebSharperJsOutput { get; set; }
        string WebSharperMinJsOutput { get; set; }
        string WebSharperTimingLog { get; set; }
        string WebSharperToolPath { get; set; }
        string WebSharperStandalone { get; set; }
        string WebSharperUseLocalReflection { get; set; }
        string WebSharperVerifyRPCSignatures { get; set; }
        string WebSharperParallelCompilation { get; set; }
        string WebSharperExternalFSharpCompiler { get; set; }
    }

    public static class WebSharperArgsWriter
    {
        private static bool TryParseBool(string s)
        {
            return bool.TryParse(s, out bool parsed) && parsed;
        }

        private static void WriteIfSet(TextWriter w, string name, string value)
        {
            if (!string.IsNullOrEmpty(value))
                w.WriteLine(name + value);
        }

        private static void WriteIfSet(TextWriter w, string name, ITaskItem value)
        {
            if (value != null)
                WriteIfSet(w, name, value.ItemSpec);
        }

        /// forceAwaitDll/forceExternalFSharpCompiler: launched (non-blocking) mode always
        /// implies both, regardless of what's separately configured on the task.
        public static void WriteAtFileName(IWebSharperTaskArgs t, string filename, bool forceAwaitDll, bool forceExternalFSharpCompiler)
        {
            using (FileStream f = File.OpenWrite(filename))
            using (StreamWriter w = new StreamWriter(f))
            {
                if (t.FscCommandLineArgs == null || t.FscCommandLineArgs.Length == 0)
                {
                    throw new Exception("Expecting FscCommandLineArgs set, needs ProvideCommandLineArgs=True");
                }

                foreach (var a in t.FscCommandLineArgs)
                    WriteIfSet(w, "", a);

                WriteIfSet(w, "--ws:", t.WebSharperProject);

                WriteIfSet(w, "--configuration:", t.Configuration);
                WriteIfSet(w, "--project:", t.MSBuildProjectFullPath);

                WriteIfSet(w, "--wsoutput:", t.WebProjectOutputDir);
                WriteIfSet(w, "--wsoutput:", t.WebSharperBundleOutputDir);
                WriteIfSet(w, "--wsoutput:", t.WebSharperHtmlDirectory);

                if (bool.TryParse(t.WebSharperDeadCodeElimination, out bool parsed))
                    w.WriteLine("--dce" + (parsed ? "+" : "-"));

                if (bool.TryParse(t.WebSharperUseLocalReflection, out bool ulr))
                    w.WriteLine("--uselocalreflection" + (ulr ? "+" : "-"));

                if (bool.TryParse(t.WebSharperVerifyRPCSignatures, out bool vrpc))
                    w.WriteLine("--verifyrpcsignatures" + (vrpc ? "+" : "-"));

                if (forceAwaitDll || TryParseBool(t.WebSharperParallelCompilation))
                    w.WriteLine("--parallelcompilation+");
                else
                    w.WriteLine("--parallelcompilation-");

                if (forceExternalFSharpCompiler || TryParseBool(t.WebSharperExternalFSharpCompiler))
                    w.WriteLine("--externalfsharpcompiler+");
                else
                    w.WriteLine("--externalfsharpcompiler-");

                if (forceAwaitDll)
                    w.WriteLine("--awaitdll+");

                if (TryParseBool(t.WebSharperDownloadResources))
                    w.WriteLine("--dlres");

                WriteIfSet(w, "--closures:", t.WebSharperAnalyzeClosures);

                WriteIfSet(w, "--jsoutput:", t.WebSharperJsOutput);
                WriteIfSet(w, "--minjsoutput:", t.WebSharperMinJsOutput);
                WriteIfSet(w, "--wstimings:", t.WebSharperTimingLog);

                if (TryParseBool(t.WebSharperSourceMap))
                    w.WriteLine("--jsmap");

                if (TryParseBool(t.WebSharperTypeScriptDeclaration))
                    w.WriteLine("--dts");

                if (TryParseBool(t.WebSharperTypeScriptOutput))
                    w.WriteLine("--ts");

                if (TryParseBool(t.WebSharperErrorsAsWarnings))
                    w.WriteLine("--wswarnonly");

                // Launched (--awaitdll) mode always forces --standalone: the wsfscservice
                // booster's named-pipe client only catches its own PipeException type around
                // sendCompileCommand, letting any other exception during a long (up to 10 min)
                // --awaitdll wait propagate unhandled and crash silently from this process's
                // perspective - confirmed via combined-mode end-to-end testing. The booster's
                // warm-FCS-checker reuse also isn't relevant to this non-interactive, one-shot
                // launched process anyway.
                if (forceAwaitDll || TryParseBool(t.WebSharperStandalone))
                    w.WriteLine("--standalone");

                if (t.WebProjectOutputDir != null && t.WebSharperProject == null)
                    w.WriteLine("--site");

                WriteIfSet(w, "--wsconfig:", t.WebSharperConfigFile);
            }
        }

        /// Mirrors WebSharperTask.HandleTaskExecutionErrors: on failure, rename the
        /// output assembly (if any) to "<path>.failed" so a stale successful-looking
        /// assembly isn't left behind, and a later build's --wstimings/log inspection
        /// can find the failed attempt. dllOutput is the intermediary assembly path
        /// wsfsc/the real fsc was writing to.
        public static void MarkOutputFailed(string dllOutput)
        {
            if (string.IsNullOrEmpty(dllOutput) || !File.Exists(dllOutput))
                return;
            var failedOutput = dllOutput + ".failed";
            if (File.Exists(failedOutput))
                File.Delete(failedOutput);
            File.Move(dllOutput, failedOutput);
        }

        /// The first FscCommandLineArgs item is assumed to be "-o:<relative assembly path>",
        /// resolved against the project's directory - the same assumption
        /// WebSharperTask.HandleTaskExecutionErrors already relied on.
        public static string GetExpectedAssemblyOutput(ITaskItem[] fscCommandLineArgs, string msBuildProjectFullPath)
        {
            if (fscCommandLineArgs == null || fscCommandLineArgs.Length == 0 || string.IsNullOrEmpty(msBuildProjectFullPath))
                return null;
            return Path.Combine(Path.GetDirectoryName(msBuildProjectFullPath).Trim(), fscCommandLineArgs[0].ItemSpec.Replace("-o:", "").Trim());
        }

        /// Combined mode's launched wsfsc has no console (CreateNoWindow) and isn't waited on
        /// by the launching task, so its stdout/stderr are captured here instead - both so a
        /// failure is diagnosable and so the OS pipe buffer backing the redirected streams can
        /// never fill up and block the launched process.
        public static string GetLaunchLogPath(string dllOutput)
        {
            if (string.IsNullOrEmpty(dllOutput))
                return Path.Combine(Path.GetTempPath(), "wsfsc-launch-" + Path.GetRandomFileName() + ".log");
            return dllOutput + ".launch.log";
        }
    }
}
