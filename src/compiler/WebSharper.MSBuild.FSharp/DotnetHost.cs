// $begin{copyright}
//
// This file is part of WebSharper
//
// Copyright (c) 2008-2026 IntelliFactory
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
using System.Runtime.InteropServices;

namespace WebSharper.MSBuild
{
    /// Runs a portable compiler (WebSharperToolPath ending in .dll) with the dotnet host.
    internal static class DotnetHost
    {
        public static bool IsPortable(string toolPath) =>
            !string.IsNullOrEmpty(toolPath) && toolPath.EndsWith(".dll", StringComparison.OrdinalIgnoreCase);

        /// The host of the SDK running this build ($(DOTNET_HOST_PATH)), so the compiler runs on a
        /// runtime installed next to it, then dotnet from PATH.
        public static string Find(string dotnetHostPath)
        {
            if (!string.IsNullOrEmpty(dotnetHostPath) && File.Exists(dotnetHostPath))
                return dotnetHostPath;
            var fromEnv = Environment.GetEnvironmentVariable("DOTNET_HOST_PATH");
            if (!string.IsNullOrEmpty(fromEnv) && File.Exists(fromEnv))
                return fromEnv;
            var exeName = RuntimeInformation.IsOSPlatform(OSPlatform.Windows) ? "dotnet.exe" : "dotnet";
            var path = Environment.GetEnvironmentVariable("PATH") ?? "";
            foreach (var dir in path.Split(Path.PathSeparator))
            {
                try
                {
                    var candidate = Path.Combine(dir.Trim('"'), exeName);
                    if (File.Exists(candidate))
                        return candidate;
                }
                catch (ArgumentException) { }
            }
            return exeName;
        }

        public static string Quote(string path) => "\"" + path + "\"";
    }
}
