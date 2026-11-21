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

/// Starts a booster process detached from the compiler that starts it. Compiled into wsfsc and linked into wscsc
/// (WebSharper.Compiler targets netstandard2.0, which lacks some of the APIs used here).
module WebSharper.Compiler.BoosterProcess

open System.Text
open System.Diagnostics
open System.Runtime.InteropServices
open System.IO

/// Starts the booster on Windows without inheriting any of this process's handles.
/// Process.Start with UseShellExecute=false always passes inheritable handles to the child,
/// so a booster started that way keeps the stdout/stderr pipes of this client open. When the
/// client runs inside an MSBuild ToolTask, MSBuild waits for those pipes to close: forever
/// before MSBuild 18.6 (the build hangs), 30 seconds after that.
module DetachedProcess =

    [<Struct; StructLayout(LayoutKind.Sequential, CharSet = CharSet.Unicode)>]
    type StartupInfo =
        val mutable cb: int
        val mutable lpReserved: nativeint
        val mutable lpDesktop: nativeint
        val mutable lpTitle: nativeint
        val mutable dwX: int
        val mutable dwY: int
        val mutable dwXSize: int
        val mutable dwYSize: int
        val mutable dwXCountChars: int
        val mutable dwYCountChars: int
        val mutable dwFillAttribute: int
        val mutable dwFlags: int
        val mutable wShowWindow: int16
        val mutable cbReserved2: int16
        val mutable lpReserved2: nativeint
        val mutable hStdInput: nativeint
        val mutable hStdOutput: nativeint
        val mutable hStdError: nativeint

    [<Struct; StructLayout(LayoutKind.Sequential)>]
    type ProcessInformation =
        val mutable hProcess: nativeint
        val mutable hThread: nativeint
        val mutable dwProcessId: int
        val mutable dwThreadId: int

    [<DllImport("kernel32.dll", EntryPoint = "CreateProcessW", CharSet = CharSet.Unicode, SetLastError = true)>]
    extern bool CreateProcess(
        string lpApplicationName, StringBuilder lpCommandLine,
        nativeint lpProcessAttributes, nativeint lpThreadAttributes,
        bool bInheritHandles, uint32 dwCreationFlags, nativeint lpEnvironment,
        string lpCurrentDirectory, StartupInfo& lpStartupInfo, ProcessInformation& lpProcessInformation)

    [<DllImport("kernel32.dll", SetLastError = true)>]
    extern bool CloseHandle(nativeint hObject)

    let CREATE_NO_WINDOW = 0x08000000u

    let start (exePath: string) (args: string list) (workingDir: string) =
        let quote (s: string) = "\"" + s + "\""
        let commandLine = StringBuilder(exePath :: args |> List.map quote |> String.concat " ")
        let mutable startupInfo = StartupInfo()
        startupInfo.cb <- Marshal.SizeOf<StartupInfo>()
        let mutable processInfo = ProcessInformation()
        // a bare name (dotnet) is looked up on PATH from the command line
        let appName = if Path.IsPathRooted exePath then exePath else null
        if not (CreateProcess(appName, commandLine, 0n, 0n, false, CREATE_NO_WINDOW, 0n, workingDir, &startupInfo, &processInfo)) then
            raise (System.ComponentModel.Win32Exception(Marshal.GetLastWin32Error()))
        CloseHandle processInfo.hThread |> ignore
        CloseHandle processInfo.hProcess |> ignore

/// The dotnet host to start the booster with when it has no runnable apphost: the host running
/// this process (the portable compiler runs as dotnet wsfsc.dll), then $(DOTNET_HOST_PATH) set by
/// MSBuild, then dotnet from PATH.
let dotnetHost () =
    let isDotnet (p: string) =
        not (isNull p) && Path.GetFileNameWithoutExtension(p).Equals("dotnet", System.StringComparison.OrdinalIgnoreCase) && File.Exists p
    let processPath = System.Environment.ProcessPath
    let hostPath = System.Environment.GetEnvironmentVariable "DOTNET_HOST_PATH"
    if isDotnet processPath then processPath
    elif isDotnet hostPath then hostPath
    else "dotnet"

/// The booster's apphost, if it can be started directly. Packages extracted on Linux and macOS
/// may not have the executable bit set, those boosters run with the dotnet host.
let tryAppHost (location: string) (name: string) =
    let isWindows = RuntimeInformation.IsOSPlatform(OSPlatform.Windows)
    let path = Path.Combine(location, if isWindows then name + ".exe" else name)
    if not (File.Exists path) then None
    elif isWindows then Some path
    else
        try
            if File.GetUnixFileMode(path).HasFlag(UnixFileMode.UserExecute) then Some path else None
        with _ -> None

/// Starts a detached booster (name: wsfscservice, wscscservice) from a folder.
let start (location: string) (name: string) (logArgs: string list) =
    let fileName, args =
        match tryAppHost location name with
        | Some appHost -> appHost, logArgs
        | None -> dotnetHost (), Path.Combine(location, name + ".dll") :: logArgs
    if RuntimeInformation.IsOSPlatform(OSPlatform.Windows) then
        DetachedProcess.start fileName args location
    else
        // the booster's output goes to /dev/null, so it doesn't keep our pipes open
        let startInfo = ProcessStartInfo("/bin/sh")
        startInfo.ArgumentList.Add "-c"
        startInfo.ArgumentList.Add "nohup \"$0\" \"$@\" </dev/null >/dev/null 2>&1 &"
        startInfo.ArgumentList.Add fileName
        for a in args do startInfo.ArgumentList.Add a
        startInfo.WorkingDirectory <- location
        startInfo.CreateNoWindow <- true
        startInfo.UseShellExecute <- false
        use p = Process.Start(startInfo)
        p.WaitForExit()
