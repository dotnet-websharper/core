// $begin{copyright}
//
// This file is part of WebSharper
//
// Copyright (c) 2008-2016 IntelliFactory
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

namespace WebSharper

module PathConventions =
    open System
    open System.IO
    open System.Reflection

    type AssemblyId =
        {
            ShortName : string
        }

        static member Create(s: string) =
            { ShortName = 
                match s.IndexOf ',' with
                | -1 -> s
                | i -> s.Substring(0, i)
            }

        static member Create(a: Assembly) =
            AssemblyId.Create(a.GetName())

        static member Create(n: AssemblyName) =
            { ShortName = n.Name }

        static member Create(t: Type) =
            AssemblyId.Create(t.Assembly)

    type ResourceKind =
        | ContentResource
        | ScriptResource

        static member Content = ContentResource
        static member Script = ScriptResource

    type EmbeddedResource =
        {
            Id : AssemblyId
            Kind : ResourceKind
            Name : string
        }

        static member Create(kind, id, name) =
            {
                Id = id
                Kind = kind
                Name = name
            }

    let private joinWithSlash (a: string) (b: string) =
        let startsWithSlash (s: string) =
            s.Length > 0
            && s.[0] = '/'
        let endsWithSlash (s: string) =
            s.Length > 0
            && s.[s.Length - 1] = '/'
        match endsWithSlash a, startsWithSlash b with
        | true, true -> a + b.Substring(1)
        | false, false -> a + "/" + b
        | _ -> a + b

    [<Sealed>]
    type PathUtility(root: string, combine: string -> string -> string) =
        let ( ++ ) = combine
        let content = root ++  "Content" ++ "WebSharper"
        let scripts = root ++ "Scripts" ++ "WebSharper"

        member p.ContentBasePath = content

        member p.ScriptBasePath = scripts

        member p.JavaScriptFileName(a) =
            a.ShortName + ".js"

        member p.MinifiedJavaScriptFileName(a) =
            a.ShortName + ".min.js"

        member p.TypeScriptDefinitionsFileName(a) =
            a.ShortName + ".d.ts"

        member p.JavaScriptPath(a) =
            scripts ++ p.JavaScriptFileName(a)

        member p.MapFileName(a) =
            a.ShortName + ".map"

        member p.MapFilePath(a) =
            scripts ++ p.MapFileName(a)

        member p.MinifiedJavaScriptPath(a) =
            scripts ++ p.MinifiedJavaScriptFileName(a)

        member p.MinifiedMapFileName(a) =
            a.ShortName + ".min.map"

        member p.MinifiedMapFilePath(a) =
            scripts ++ p.MinifiedMapFileName(a)

        member p.TypeScriptDefinitionsPath(a) =
            scripts ++ p.TypeScriptDefinitionsFileName(a)

        member p.EmbeddedResourceKey(r) =
            r.Id.ShortName + "/" + r.Name

        member p.EmbeddedPath(r) =
            match r.Kind with
            | ContentResource -> content ++ r.Id.ShortName ++ r.Name
            | ScriptResource -> scripts ++ r.Id.ShortName ++ r.Name

        static member FileSystem(root) =
            PathUtility(root, fun a b -> Path.Combine(a, b))

        static member VirtualPaths(root) =
            let root =
                match root with
                | "" | null -> "/"
                | _ -> root
            PathUtility(root, joinWithSlash)
