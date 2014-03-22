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

/// Constructs proxy reports in XML format.
module internal IntelliFactory.WebSharper.ProxyReporter

//open System.Collections.Generic
//open System.IO
//open System.Reflection
//open System.Text.RegularExpressions
//open System.Xml
//module M    = IntelliFactory.WebSharper.Metadata
//module AL   = IntelliFactory.PowerPack.AssemblyLoading
//module RH   = IntelliFactory.PowerPack.ReflectionHelpers
//module MI   = IntelliFactory.PowerPack.MetaId
//
///// Runs the report construction.
//let Run (outFile: FileInfo) (depFiles: seq<FileInfo>) =
//    let out = File.CreateText outFile.FullName
//    use xml = XmlTextWriter.Create(out, XmlWriterSettings(Indent=true))
//    let metadata =
//        M.Info.Create [
//            for f in depFiles ->
//                M.AssemblyInfo.ForAssemblyFile f.FullName
//        ]
//    let getName (info: MemberInfo) =
//        string (MI.MemberId.Of info)
//    let reportConstructor info =
//        xml.WriteStartElement "Member"
//        xml.WriteAttributeString("Kind", "Constructor")
//        xml.WriteAttributeString("Name", getName info)
//        xml.WriteEndElement()
//    let reportMethod (info: MethodInfo) =
//        xml.WriteStartElement "Member"
//        xml.WriteAttributeString("Kind", "Method")
//        xml.WriteAttributeString("Name", getName info)
//        xml.WriteAttributeString("Static",
//            if info.IsStatic then "True" else "False")
//        xml.WriteEndElement()
//    let reportProperty (info: PropertyInfo) =
//        xml.WriteStartElement "Member"
//        xml.WriteAttributeString("Kind", "Property")
//        xml.WriteAttributeString("Name", getName info)
//        xml.WriteAttributeString("Static",
//            if RH.IsStatic info then "True" else "False")
//        xml.WriteEndElement()
//    let reportField (info: FieldInfo) =
//        xml.WriteStartElement "Member"
//        xml.WriteAttributeString("Kind", "Field")
//        xml.WriteAttributeString("Name", getName info)
//        xml.WriteAttributeString("Static",
//            if RH.IsStatic info then "True" else "False")
//        xml.WriteEndElement()
//    let flags =
//        BindingFlags.Public
//        ||| BindingFlags.Instance
//        ||| BindingFlags.Static
//        ||| BindingFlags.DeclaredOnly
//    xml.WriteStartElement "Types"
//    for pT in metadata.ProxyResolver.ProxiedTypes do
//        let proxyTokens = Dictionary()
//        let isProxied x =
//            match metadata.ProxyResolver.GetMemberProxy x with
//            | None ->
//                false
//            | Some y ->
//                proxyTokens.[y.MetadataToken] <- ()
//                true
//        let isProxy (info: MemberInfo) =
//            proxyTokens.ContainsKey info.MetadataToken
//        match pT.TryLoad() with
//        | Some p ->
//            xml.WriteStartElement "Type"
//            xml.WriteAttributeString("FullName", p.FullName)
//            let (c0, c1) =
//                p.GetConstructors flags
//                |> Array.partition isProxied
//            let (m0, m1) =
//                p.GetMethods flags
//                |> Array.filter (fun x ->
//                    (x.Name.StartsWith "get_"
//                    || x.Name.StartsWith "set_")
//                    |> not)
//                |> Array.partition isProxied
//            let (p0, p1) =
//                p.GetProperties flags
//                |> Array.partition isProxied
//            let (f0, f1) =
//                p.GetFields flags
//                |> Array.partition isProxied
//            xml.WriteStartElement "SupportedMembers"
//            for x in c0 do
//                reportConstructor x
//            for x in m0 do
//                reportMethod x
//            for x in p0 do
//                reportProperty x
//            for x in f0 do
//                reportField x
//            xml.WriteEndElement()
//            xml.WriteStartElement "NotSupportedMembers"
//            for x in c1 do
//                reportConstructor x
//            for x in m1 do
//                reportMethod x
//            for x in p1 do
//                reportProperty x
//            for x in f1 do
//                reportField x
//            xml.WriteEndElement()
//
//            xml.WriteEndElement()
//        | None ->
//            Log.Error("Failed to load type: {0}", pT)
//    xml.WriteEndElement()
//
