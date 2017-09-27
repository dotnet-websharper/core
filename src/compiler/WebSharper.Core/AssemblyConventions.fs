// $begin{copyright}
//
// This file is part of WebSharper
//
// Copyright (c) 2008-2017 IntelliFactory
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

module WebSharper.Core.AssemblyConventions

open System
open System.Collections.Generic
open System.IO
open System.Reflection

let NetStandardName = "netstandard"

let NetStandardAssembly =
    Path.Combine(Path.GetDirectoryName(Assembly.GetExecutingAssembly().Location), "ref", "netstandard.dll")
    |> Mono.Cecil.AssemblyDefinition.ReadAssembly

let NetStandardFullName = NetStandardAssembly.FullName

let IsDefinedInOrForwardedFrom (asm: Mono.Cecil.AssemblyDefinition) =
    let exported = HashSet()
    for e in asm.MainModule.ExportedTypes do
        exported.Add(e.FullName) |> ignore
    fun (fullName: string) ->
        let n = fullName.Replace('+', '/')
        exported.Contains(n)
        ||
        match asm.MainModule.GetType(n) with
        | null -> false
        | _ -> true

let isInNS = IsDefinedInOrForwardedFrom NetStandardAssembly

let IsNetStandardType (fullName: string) =
    isInNS fullName

let StandardAssemblyNameForTypeNamed (fullName: string) =
    if IsNetStandardType fullName
    then Some NetStandardName
    else None

let StandardAssemblyFullNameForTypeNamed (fullName: string) =
    if IsNetStandardType fullName
    then Some NetStandardFullName
    else None
