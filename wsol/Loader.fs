// $begin{copyright}
// 
// This file is part of WebSharper
// 
// Copyright (c) 2008-2011 IntelliFactory
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

/// Assembly loader utilities.
module private IntelliFactory.WebSharper.Sitelets.Offline.Loader

type D<'T1,'T2> = System.Collections.Generic.Dictionary<'T1,'T2>

/// A synchronization object.
let private syncRoot = obj ()

/// A mapping for all located assemblies.
let private paths = D()

/// Adding a custom assembly resolver.
let private handler =
    System.ResolveEventHandler(fun _ a ->
        let name = System.Reflection.AssemblyName(a.Name).Name
        lock syncRoot <| fun () ->
            match paths.TryGetValue name with
            | (true, file)  -> System.Reflection.Assembly.LoadFile file
            | _             -> null)

do System.AppDomain.CurrentDomain.add_AssemblyResolve handler
do System.AppDomain.CurrentDomain.add_ReflectionOnlyAssemblyResolve handler

/// Adds a search path to the assembly loader of the current domain.
/// Caution: assemblies cannot be unloaded.
let AddSearchPath (path: string) =
    lock syncRoot <| fun () ->
        let proc pattern =
            for file in System.IO.Directory.GetFiles(path, pattern) do
                let name = System.IO.Path.GetFileNameWithoutExtension file
                if not (paths.ContainsKey name) then
                    paths.[name] <- file
        proc "*.dll"
        proc "*.exe"
