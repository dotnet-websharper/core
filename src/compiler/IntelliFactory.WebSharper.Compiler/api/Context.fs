// $begin{copyright}
//
// This file is part of WebSharper
//
// Copyright (c) 2008-2014 IntelliFactory
//
// GNU Affero General Public License Usage
// WebSharper is free software: you can redistribute it and/or modify it under
// the terms of the GNU Affero General Public License, version 3, as published
// by the Free Software Foundation.
//
// WebSharper is distributed in the hope that it will be useful, but WITHOUT
// ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
// FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License
// for more details at <http://www.gnu.org/licenses/>.
//
// If you are unsure which license is appropriate for your use, please contact
// IntelliFactory at http://intellifactory.com/contact.
//
// $end{copyright}

namespace IntelliFactory.WebSharper.Compiler

module M = IntelliFactory.WebSharper.Core.Metadata
module Re = IntelliFactory.WebSharper.Core.Reflection

type Context =
    {
        Code : IDictionary<Re.AssemblyName, Assembly>
        Infos : list<M.AssemblyInfo>
        Metas : list<Metadata.T>
    }

    member this.AssemblyInfos = this.Infos
    member this.MetadataRecords = this.Metas

    member this.CreateMetadataInfo() =
        M.Info.Create this.Infos

    member this.LookupAssembly(name: Re.AssemblyName) =
        match this.Code.TryGetValue(name) with
        | true, a -> Some a
        | _ -> None

    member this.LookupAssemblyCode(debug: bool, name: Re.AssemblyName) =
        match this.Code.TryGetValue(name) with
        | true, a -> if debug then a.ReadableJavaScript else a.CompressedJavaScript
        | _ -> None

    static member Get(assemblies: list<Assembly>) =
        let assemblies =
            assemblies
            |> Seq.distinctBy (fun a -> a.Raw.Name.Name)
            |> Seq.toList
        let cM = assemblies |> List.choose (fun a -> Metadata.ReadFromCecilAssembly a.Raw)
        let rM = assemblies |> List.choose (fun a -> Utility.ReadAssemblyInfo a.Raw)
        let code =
            dict [|
                for a in assemblies do
                    yield (Re.AssemblyName.Parse(a.FullName), a)
            |]
        { Metas = cM; Infos = rM; Code = code }
