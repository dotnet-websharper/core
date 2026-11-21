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
using System.Linq;
using System.Text.RegularExpressions;
using Microsoft.Build.Utilities;

namespace WebSharper.MSBuild
{
    /// Finds an installed WebSharper.FSharp.Booster / WebSharper.CSharp.Booster .NET tool: the ReadyToRun
    /// compiler for the current platform, used instead of the portable compiler of the package when its
    /// version is exactly the version of the referenced WebSharper package.
    ///
    /// A local tool counts only if dotnet, run from the project folder, would find it: the first manifest
    /// listing the tool decides, looking from the project folder up (.config/dotnet-tools.json, then
    /// dotnet-tools.json, stopping at "isRoot": true). A global tool counts only if no such manifest lists it.
    internal sealed class BoosterLocator
    {
        public string PackageId;       // WebSharper.FSharp.Booster
        public string EntryAssembly;   // wsfsc.dll
        public string Version;         // $(WebSharperCompilerVersion)
        public string ProjectDirectory;
        public string NuGetPackageRoot;
        public string[] RuntimeIdentifiers; // $(NETCoreSdkRuntimeIdentifier), $(NETCoreSdkPortableRuntimeIdentifier)
        public TaskLoggingHelper Log;

        /// The path of the Booster's entry assembly, or null to use the portable compiler.
        public string Locate()
        {
            if (string.IsNullOrEmpty(Version) || string.IsNullOrEmpty(ProjectDirectory))
                return null;
            var id = PackageId.ToLowerInvariant();

            var manifest = FindManifest(id, out var listedVersion);
            if (manifest != null)
            {
                if (!SameVersion(listedVersion, Version))
                {
                    Log.LogWarning(null, "WSB9101", null, manifest, 0, 0, 0, 0,
                        "{0} is listed at version {1} in this tool manifest, but the referenced WebSharper is {2}. " +
                        "Building with the portable compiler. Run: dotnet tool update {0} --version {2}",
                        PackageId, listedVersion, Version);
                    return null;
                }
                var root = PackageRoot();
                var local = FindInPackageFolder(root, id, listedVersion);
                if (local == null)
                {
                    Log.LogWarning(null, "WSB9102", null, manifest, 0, 0, 0, 0,
                        "{0} {1} is listed in this tool manifest, but it is not restored for this platform ({2}) in {3}. " +
                        "Building with the portable compiler. Run: dotnet tool restore",
                        PackageId, listedVersion, string.Join(", ", RuntimeIdentifiers), root);
                    return null;
                }
                Log.LogMessage("Using the {0} {1} tool from {2}: {3}", PackageId, listedVersion, manifest, local);
                return local;
            }

            var global = FindGlobal(id);
            if (global != null)
            {
                Log.LogMessage("Using the global {0} {1} tool: {2}", PackageId, Version, global);
                return global;
            }

            Log.LogMessage("Using the portable WebSharper compiler: no tool manifest lists {0} and it is not installed globally at version {1}.", PackageId, Version);
            return null;
        }

        static bool SameVersion(string a, string b) =>
            string.Equals((a ?? "").Trim(), (b ?? "").Trim(), StringComparison.OrdinalIgnoreCase);

        string FindManifest(string id, out string listedVersion)
        {
            listedVersion = null;
            var toolPattern = new Regex(
                "\"" + Regex.Escape(id) + "\"\\s*:\\s*\\{[^{}]*?\"version\"\\s*:\\s*\"([^\"]*)\"",
                RegexOptions.IgnoreCase | RegexOptions.Singleline);
            var isRoot = new Regex("\"isRoot\"\\s*:\\s*true", RegexOptions.IgnoreCase);
            var dir = new DirectoryInfo(Path.GetFullPath(ProjectDirectory));
            while (dir != null)
            {
                foreach (var candidate in new[] {
                    Path.Combine(dir.FullName, ".config", "dotnet-tools.json"),
                    Path.Combine(dir.FullName, "dotnet-tools.json") })
                {
                    if (!File.Exists(candidate))
                        continue;
                    string text;
                    try { text = File.ReadAllText(candidate); }
                    catch (IOException) { continue; }
                    var m = toolPattern.Match(text);
                    if (m.Success)
                    {
                        listedVersion = m.Groups[1].Value;
                        return candidate;
                    }
                    if (isRoot.IsMatch(text))
                        return null;
                }
                dir = dir.Parent;
            }
            return null;
        }

        string PackageRoot()
        {
            var fromEnv = Environment.GetEnvironmentVariable("NUGET_PACKAGES");
            if (!string.IsNullOrEmpty(fromEnv))
                return fromEnv;
            if (!string.IsNullOrEmpty(NuGetPackageRoot))
                return NuGetPackageRoot;
            return Path.Combine(Environment.GetFolderPath(Environment.SpecialFolder.UserProfile), ".nuget", "packages");
        }

        /// <root>/<id>.<rid>/<version>/tools/<tfm>/<rid>/<entry>
        string FindInPackageFolder(string root, string id, string version)
        {
            foreach (var rid in RuntimeIdentifiers.Where(r => !string.IsNullOrEmpty(r)).Distinct())
            {
                var path = FindEntry(Path.Combine(root, id + "." + rid, version.ToLowerInvariant(), "tools"), rid);
                if (path != null)
                    return path;
            }
            return null;
        }

        /// <CLI home>/.dotnet/tools/.store/<id>/<version>/<id>.<rid>/<version>/tools/<tfm>/<rid>/<entry>
        string FindGlobal(string id)
        {
            var home = Environment.GetEnvironmentVariable("DOTNET_CLI_HOME");
            if (string.IsNullOrEmpty(home))
                home = Environment.GetFolderPath(Environment.SpecialFolder.UserProfile);
            var store = Path.Combine(home, ".dotnet", "tools", ".store", id);
            var version = Version.ToLowerInvariant();
            foreach (var rid in RuntimeIdentifiers.Where(r => !string.IsNullOrEmpty(r)).Distinct())
            {
                var path = FindEntry(Path.Combine(store, version, id + "." + rid, version, "tools"), rid);
                if (path != null)
                    return path;
            }
            return null;
        }

        string FindEntry(string toolsDir, string rid)
        {
            if (!Directory.Exists(toolsDir))
                return null;
            foreach (var tfmDir in Directory.GetDirectories(toolsDir).OrderByDescending(d => d, StringComparer.OrdinalIgnoreCase))
            {
                var entry = Path.Combine(tfmDir, rid, EntryAssembly);
                if (File.Exists(entry))
                    return entry;
            }
            return null;
        }
    }
}
