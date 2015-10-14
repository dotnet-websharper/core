// $begin{copyright}
//
// This file is part of WebSharper
//
// Copyright (c) 2008-2015 IntelliFactory
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

//module M = WebSharper.Core.Metadata
//module Re = WebSharper.Core.Reflection
//
//type Context =
//    {
//        Code : IDictionary<Re.AssemblyName, Assembly>
//        Infos : list<M.AssemblyInfo>
//        Metas : list<Metadata.T>
//    }
//
//    member this.AssemblyInfos = this.Infos
//    member this.MetadataRecords = this.Metas
//
//    member this.CreateMetadataInfo() =
//        M.Info.Create this.Infos
//
//    member this.LookupAssembly(name: Re.AssemblyName) =
//        match this.Code.TryGetValue(name) with
//        | true, a -> Some a
//        | _ -> None
//
//    member this.LookupAssemblyCode(debug: bool, name: Re.AssemblyName) =
//        match this.Code.TryGetValue(name) with
//        | true, a -> if debug then a.ReadableJavaScript else a.CompressedJavaScript
//        | _ -> None
//
//    static member Get(assemblies: list<Assembly>) =
//        let assemblies =
//            assemblies
//            |> Seq.distinctBy (fun a -> a.Raw.Name.Name)
//            |> Seq.toList
//        let cM = assemblies |> List.choose (fun a -> Metadata.ReadFromCecilAssembly a.Raw)
//        let rM = assemblies |> List.choose (fun a -> Utility.ReadAssemblyInfo a.Raw)
//        let code =
//            dict [|
//                for a in assemblies do
//                    yield (Re.AssemblyName.Parse(a.FullName), a)
//            |]
//        { Metas = cM; Infos = rM; Code = code }
