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

using System.Collections.Concurrent;
using System.Diagnostics;

namespace WebSharper.MSBuild.FSharp
{
    /// Hands off the Process object WebSharperLaunchTask created to WebSharperJoinTask within
    /// the same MSBuild node process. Process.GetProcessById(pid).WaitForExit()/.ExitCode is
    /// unreliable once the process has already exited by the time the join task runs (a real,
    /// likely outcome - the launched wsfsc can legitimately finish very close to when the real
    /// fsc does, and GetProcessById throws once the OS has no record of the PID left). A Process
    /// object obtained from Process.Start(), by contrast, correctly retains its exit code
    /// regardless of what's happened to the OS-level process table by the time it's queried, as
    /// long as the *same* object is reused - found via combined-mode end-to-end testing.
    public static class WebSharperLaunchedProcesses
    {
        private static readonly ConcurrentDictionary<int, Process> processes = new ConcurrentDictionary<int, Process>();

        public static void Register(Process p)
        {
            processes[p.Id] = p;
        }

        /// Removes on lookup so a long-lived MSBuild node doesn't accumulate entries across
        /// many builds.
        public static bool TryTake(int pid, out Process p)
        {
            return processes.TryRemove(pid, out p);
        }
    }
}
