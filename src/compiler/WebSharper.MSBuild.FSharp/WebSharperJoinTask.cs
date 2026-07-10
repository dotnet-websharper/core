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
using Microsoft.Build.Framework;
using Microsoft.Build.Utilities;

namespace WebSharper.MSBuild.FSharp
{
    /// Combined mode: waits for the process WebSharperLaunchTask started (by PID) to finish,
    /// and surfaces its result. A thin wait, not a second compile - the real work already
    /// happened concurrently with CoreCompile in the launched wsfsc process.
    public sealed class WebSharperJoinTask : Task
    {
        [Required]
        public int LaunchedProcessId { get; set; }

        /// Same items the real (post-CoreCompile) WebSharperTask invocation would receive -
        /// used only to compute the ".failed" marker path on a launched-process failure,
        /// the same way WebSharperTask.HandleTaskExecutionErrors does.
        public ITaskItem[] FscCommandLineArgs { get; set; } = new ITaskItem[] { };
        public string MSBuildProjectFullPath { get; set; }

        public override bool Execute()
        {
            // Prefer the actual Process object WebSharperLaunchTask created (see
            // WebSharperLaunchedProcesses): a fresh Process.GetProcessById(pid) lookup fails
            // outright once the launched process has already exited, which is a likely, benign
            // outcome (it can legitimately finish close to when the real fsc does).
            Process proc;
            bool isReusedStartedProcess = WebSharperLaunchedProcesses.TryTake(LaunchedProcessId, out proc);
            if (!isReusedStartedProcess)
            {
                try
                {
                    proc = Process.GetProcessById(LaunchedProcessId);
                }
                catch (ArgumentException)
                {
                    Log.LogError("Could not find the launched WebSharper compiler process (PID {0}); it may have exited unexpectedly before this build could join it.", LaunchedProcessId);
                    return false;
                }
            }

            // The parameterless WaitForExit() overload has a known issue for Process objects
            // obtained via GetProcessById (as opposed to Process.Start): it can leave internal
            // state such that a subsequent ExitCode read throws "Process was not started by
            // this object". The timeout overload uses a simpler native-handle wait and doesn't
            // have this problem - found via combined-mode end-to-end testing.
            proc.WaitForExit(System.Threading.Timeout.Infinite);
            if (isReusedStartedProcess)
            {
                // Per the documented WaitForExit(int) caveat: it does not guarantee redirected
                // output events have finished dispatching, only that the process itself has
                // exited. A trailing parameterless WaitForExit() (returns immediately here,
                // since the process has already exited) is Microsoft's documented way to also
                // wait for that - only safe for the object Process.Start() actually created,
                // not the GetProcessById fallback (that's the known issue noted above).
                proc.WaitForExit();
            }

            if (proc.ExitCode != 0)
            {
                var dllOutput = WebSharperArgsWriter.GetExpectedAssemblyOutput(FscCommandLineArgs, MSBuildProjectFullPath);
                WebSharperArgsWriter.MarkOutputFailed(dllOutput);
                var logPath = WebSharperArgsWriter.GetLaunchLogPath(dllOutput);
                Log.LogError("WebSharper compilation (launched process, PID {0}) failed with exit code {1}. See {2} for details.", LaunchedProcessId, proc.ExitCode, logPath);
                return false;
            }

            return true;
        }
    }
}
