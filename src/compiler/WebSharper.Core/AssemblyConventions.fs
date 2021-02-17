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

let NetStandardFullName, nsTypes =
    let types = HashSet<string>()
    let thisAsm = Assembly.GetExecutingAssembly()
    let s = thisAsm.GetManifestResourceStream("WebSharper.Core.netstandardtypes.txt")
    use r = new StreamReader(s)
    let fullName = r.ReadLine()
    let rec read() =
        match r.ReadLine() with
        | null -> ()
        | s -> types.Add s |> ignore; read()
    read()
    fullName, types

let isInNS t = nsTypes.Contains t

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