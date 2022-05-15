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

module WebSharper.Sitelets.Loading

open System
open System.Reflection

open WebSharper.Constants

type private BF = BindingFlags

/// Looks up assembly-wide Website attribute and runs it if present
let private TryLoadSiteA (assembly: Assembly) =
    let aT = typeof<WebsiteAttribute>
    match Attribute.GetCustomAttribute(assembly, aT) with
    | :? WebsiteAttribute as attr ->
        attr.Run () |> Some
    | _ -> None

/// Searches for static property with Website attribute and loads it if found
let private TryLoadSiteB (assembly: Assembly) =
    let aT = typeof<WebsiteAttribute>
    assembly.GetModules(false)
    |> Seq.collect (fun m ->
        try m.GetTypes() |> Seq.ofArray
        with
        | :? ReflectionTypeLoadException as e ->
            e.Types |> Seq.filter (fun t -> not (obj.ReferenceEquals(t, null)))
        | _ -> Seq.empty
    )
    |> Seq.tryPick (fun ty ->
        ty.GetProperties(BF.Static ||| BF.Public ||| BF.NonPublic)
        |> Array.tryPick (fun p ->
            match Attribute.GetCustomAttribute(p, aT) with
            | :? WebsiteAttribute ->
                try
                    let sitelet = p.GetGetMethod().Invoke(null, [||])
                    let upcastSitelet =
                        sitelet.GetType()
                            .GetMethod("Box", BF.Instance ||| BF.Public)
                            .Invoke(sitelet, [||])
                            :?> Sitelet<obj>
                    Some (upcastSitelet, [])
                with e ->
                    raise <| exn("Failed to initialize sitelet definition: " + ty.FullName + "." + p.Name, e)  
            | _ -> None
        )
    )

/// Try to find a sitelet defined in given assembly.
let DiscoverSitelet(assembly: Assembly) =
    TryLoadSiteA assembly 
    |> Option.orElseWith (fun () -> TryLoadSiteB assembly)
    |> Option.map fst       
