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

open System
open System.IO
module CT = WebSharper.Core.ContentTypes
module M = WebSharper.Core.Metadata

module internal Utility =

    /// Reads a manifest resource from an assembly that defines the given type.
    let ReadResourceFromAssembly (ty: Type) (name: string) =
        ty.Assembly.GetManifestResourceNames()
        |> Seq.tryFind (fun x -> x.Contains(name))
        |> Option.bind (fun name ->
            use s = ty.Assembly.GetManifestResourceStream(name)
            use r = new StreamReader(s)
            Some (r.ReadToEnd()))

    let IsWebResourceAttribute (fullName: string) =
        fullName = "WebSharper.WebResourceAttribute"

    /// Like `ReadResourceFromAssembly`, but checks if it is marked as WebResource.
    /// First consults if the resource has been marked with WebResourceAttribute, and if yes,
    /// uses the annotation to determine content type.
    /// Otherwise, tries to guess content-type from filename.
    let ReadWebResource (ty: Type) (name: string) =
        try
            let content = defaultArg (ReadResourceFromAssembly ty name) ""
            let contentType =
                let explicit =
                    CustomAttributeData.GetCustomAttributes(ty.Assembly)
                    |> Seq.tryPick (fun attr ->
                        if IsWebResourceAttribute attr.Constructor.DeclaringType.FullName then
                            match [for a in attr.ConstructorArguments -> a.Value] with
                            | [(:? string as n); (:? string as contentType)] ->
                                if n.Contains(name) // TODO: better checking here.
                                    then Some (CT.Parse contentType)
                                    else None
                            | _ -> None
                        else None)
                match explicit with
                | None ->
                    match CT.TryGuessByFileName name with
                    | None -> CT.Text.Plain
                    | Some cT -> cT
                | Some cT -> cT
            (content, contentType)
        with e ->
            ("", CT.Text.Plain)
