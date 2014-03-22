// $begin{copyright}
//
// This file is part of WebSharper
//
// Copyright (c) 2008-2013 IntelliFactory
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

namespace IntelliFactory.WebSharper.Core.Plugins

open System
open System.Configuration
open System.Collections.Generic
open IntelliFactory.Core

/// Classifies results of plugin execution.
type Result =
    | Error
    | Pass
    | Success

/// Provides environment facilities for the execution of plugins.
type IEnvironment =

    /// Assembly resolver utility.
    abstract AssemblyResolver : AssemblyResolver

    /// The command-line arguments.
    abstract CommandLineArgs : string []

/// An interface to implement by plugins.
type IPlugin =

    /// Called once for every plugin per execution of WebSharper.
    abstract Run : IEnvironment -> Result

/// Used by the configuration infrastructure.
[<Sealed>]
type PluginElement() =
    inherit ConfigurationElement()

    [<ConfigurationProperty("key", IsRequired=true, IsKey=true)>]
    member this.Key
        with get () = this.["key"] :?> string
        and set (x: string) = this.["key"] <- x

    [<ConfigurationProperty("implementation", IsRequired=true)>]
    member this.Implementation
        with get () = this.["implementation"] :?> string
        and set (x: string) = this.["implementation"] <- x

/// Used by the configuration infrastructure.
[<Sealed>]
type PluginsCollection() =
    inherit ConfigurationElementCollection()
    let elements = List()

    member this.Elements =
        elements :> seq<_>

    override this.CollectionType =
        ConfigurationElementCollectionType.AddRemoveClearMap

    override this.CreateNewElement() =
        let element = PluginElement()
        elements.Add(element)
        element :> _

    override this.GetElementKey(element) =
        (element :?> PluginElement).Key :> _

/// Used by the configuration infrastructure.
[<Sealed>]
type PluginsSection() =
    inherit ConfigurationSection()

    [<ConfigurationProperty("plugins", IsDefaultCollection=false)>]
    [<ConfigurationCollection(typeof<PluginsCollection>,
        AddItemName = "add",
        ClearItemsName = "clear",
        RemoveItemName = "remove")>]
    member this.Plugins =
        base.["plugins"] :?> PluginsCollection

/// Uses application configuration to get the plugins.
module Configuration =

    let private tryLoadPlugin (fullName: string) =
        match Type.GetType(fullName) with
        | null -> None
        | t ->
            let instance =
                try Some (Activator.CreateInstance(t))
                with _ -> None
            instance
            |> Option.bind (function
                | :? IPlugin as self -> Some self
                | _ -> None)

    let private getKnownPlugins () =
        [
            "IntelliFactory.WebSharper.Sitelets.Plugin, \
                IntelliFactory.WebSharper.Sitelets"
        ]
        |> Seq.choose tryLoadPlugin

    /// Loads configured plugins.
    let GetPlugins () =
        let key = "IntelliFactory.WebSharper.Plugins"
        match ConfigurationManager.GetSection(key) with
        | :? PluginsSection as s ->
            s.Plugins.Elements
            |> Seq.map (fun e -> e.Implementation)
            |> Seq.choose tryLoadPlugin
            |> Seq.append (getKnownPlugins ())
        | _ ->
            getKnownPlugins ()
