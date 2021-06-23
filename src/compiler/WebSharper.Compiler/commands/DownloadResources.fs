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

namespace WebSharper.Compiler

module Re = WebSharper.Core.Resources 

module DownloadResources =

    let private localResTyp = typeof<Re.IDownloadableResource>

    let private printError (e: exn) =
                let rec messages (e: exn) =
                    seq {
                        yield e.Message
                        match e with
                        | :? System.Reflection.ReflectionTypeLoadException as e ->
                            yield! Seq.collect messages e.LoaderExceptions
                        | e when isNull e.InnerException -> ()
                        | e -> yield! messages e.InnerException
                    }
                String.concat " - " (messages e)

    let DownloadResource (path: string) (root: string) =
        let errors = ResizeArray()
        let name = Path.GetFileNameWithoutExtension path
        try
            let asm = 
                if name.Contains "System." 
                    || name.Contains "Microsoft." 
                    || name = "WebSharper.Sitelets" 
                    || name = "WebSharper.AspNetCore" 
                then
                    null
                else
                    try
                        System.Reflection.Assembly.Load (Path.GetFileNameWithoutExtension path)
                    with e ->
                        if e.HResult <> 0x80131058 then
                            errors.Add <| sprintf "Failed to load assembly for unpacking local resources: %s - %s" path (printError e)     
                        // else this is a reference assembly, so it's ok not to load it.
                        null
            if not (isNull asm) then
                for t in asm.GetTypes() do
                    let isDownloadableResource =
                        try
                            t.GetInterfaces() |> Array.contains localResTyp && t.GetConstructor([||]) |> isNull |> not
                        with _ -> false
                    if isDownloadableResource then
                        try
                            let res = Activator.CreateInstance(t) :?> Re.IDownloadableResource
                            res.Unpack(root)
                        with e ->
                            errors.Add <| sprintf "Failed to unpack local resource: %s - %s" t.FullName (printError e)   
        with e ->
            errors.Add <| sprintf "Failed to unpack local resources from %s: %s" name (printError e)
