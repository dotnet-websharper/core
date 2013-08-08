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

module IntelliFactory.WebSharper.Core.Metadata

open System
open System.Collections.Generic
open System.IO
open System.Reflection
open Mono.Cecil

type D<'T1,'T2> = Dictionary<'T1,'T2>
type Q<'T> = Queue<'T>

module P = IntelliFactory.JavaScript.Packager
module Re = IntelliFactory.WebSharper.Core.Reflection
module R = IntelliFactory.WebSharper.Core.Resources

type AssemblyMode =
    | CompiledAssembly
    | IgnoredAssembly

type Node =
    | AssemblyNode of Re.AssemblyName * AssemblyMode
    | ConstructorNode of Re.Constructor
    | MethodNode of Re.Method
    | ResourceNode of Re.TypeDefinition
    | TypeNode of Re.TypeDefinition

    override this.ToString() =
        match this with
        | AssemblyNode (x, mode) ->
            match mode with
            | CompiledAssembly -> string x
            | IgnoredAssembly -> "I:" + string x
        | ConstructorNode x -> string x
        | MethodNode x -> string x
        | ResourceNode x -> "R:" + string x
        | TypeNode x -> string x

type Resource =
    | AssemblyResource of Re.AssemblyName
    | UserResource of Re.TypeDefinition

    override this.ToString() =
        match this with
        | AssemblyResource x -> string x
        | UserResource x -> string x

[<Sealed>]
type MethodHandleSerializationException(e: exn) =
    inherit exn("Failed to parse a MethodHandle.", e)

type MethodHandle =
    {
        assembly : string
        code : int
    }

    member this.Pack() =
        string this.assembly + ":" + string this.code

    static member Unpack(s: string) =
        try
            let i = s.LastIndexOf ':'
            let code = int (s.Substring(i + 1))
            let assembly = s.Substring(0, i)
            { assembly = assembly; code = code }
        with e ->
            raise (MethodHandleSerializationException e)

type AssemblyInfo =
    {
        name : Re.AssemblyName
        records : D<Re.TypeDefinition,list<string*string>>
        remote : D<Re.Method,MethodHandle>
        requirements : D<Node,list<int>>
        resources : D<Resource,int>
        types : D<Re.TypeDefinition,P.Address>
    }

    member this.AddRemoteMethod(m: Re.Method) =
        match this.remote.TryGetValue m with
        | true, y -> y
        | _ ->
            let code = this.remote.Count
            let a = m.DeclaringType.AssemblyName.Name
            let y = { assembly = a; code = code }
            this.remote.[m] <- y
            y

    member this.AddRecord (tD: Re.TypeDefinition) (ts: list<string*string>) =
        this.records.[tD] <- ts

    member this.AddCompiledType (tD: Re.TypeDefinition) (a: P.Address) =
        this.types.[tD] <- a

    member this.AddRequirement (n: Node) (rs: list<Resource>) =
        let getId (r: Resource) =
            match this.resources.TryGetValue r with
            | true, k -> k
            | _ ->
                let k = this.resources.Count
                this.resources.[r] <- k
                k
        this.requirements.[n] <- List.map getId rs

    static member Create name =
        {
            name = name
            records = D()
            remote = D()
            requirements = D()
            resources = D()
            types = D()
        }

    member this.Requirements : seq<Node * list<Resource>> =
        let res = D()
        for KeyValue (k, v) in this.resources do
            res.[v] <- k
        let getR x = res.[x]
        this.requirements
        |> Seq.map (fun (KeyValue (k, v)) -> (k, List.map getR v))
        |> Seq.toArray :> _

    static member EmbeddedResourceName =
        "WebSharper.meta"

    override this.ToString() =
        this.name.Name

    member this.ToStream stream =
        AssemblyInfoEncoding.Encode stream this

    static member FromStream stream =
        AssemblyInfoEncoding.Decode stream

    static member LoadReflected(a: System.Reflection.Assembly) =
        if a.FullName.StartsWith "System" then None else
            let n = AssemblyInfo.EmbeddedResourceName
            if Array.exists ((=) n) (a.GetManifestResourceNames()) then
                use s = a.GetManifestResourceStream n
                try
                    Some (AssemblyInfo.FromStream s)
                with _ ->
                    failwithf "Failed to load metadata for: %s" a.FullName
            else
                None

    static member Load(path: string) : option<AssemblyInfo> =
        let aD = AssemblyDefinition.ReadAssembly(path)
        if aD.FullName.StartsWith "System" then None else
            aD.MainModule.Resources
            |> Seq.tryPick (fun r ->
                match r with
                | :? EmbeddedResource as r ->
                    if r.Name = AssemblyInfo.EmbeddedResourceName then
                        use s = r.GetResourceStream()
                        try
                            Some (AssemblyInfo.FromStream s)
                        with _ ->
                            failwithf "Failed to load metadata for: %s" aD.FullName
                    else None
                | _ -> None)

and AssemblyInfoEncoding() =

    static let enc =
        let eP = Binary.EncodingProvider.Create()
        eP.DeriveEncoding typeof<AssemblyInfo>

    static member Encode stream (info: AssemblyInfo) =
        enc.Encode stream info

    static member Decode stream =
        enc.Decode stream :?> AssemblyInfo

let trace =
    System.Diagnostics.TraceSource("WebSharper",
        System.Diagnostics.SourceLevels.All)

let add (k: 'K) (v: 'V) (d: D<'K,'V>) =
    if not (d.ContainsKey k) then d.[k] <- v else
        trace.TraceData(System.Diagnostics.TraceEventType.Warning, 0,
            "Duplicate runtime metadata table entry encountered.",
            k, d.[k], v)

type Type = Re.TypeDefinition

[<Sealed>]
type AssemblyResource(name: Re.AssemblyName) =
    interface R.IResource with
        member this.Render ctx html =
            let r = ctx.GetAssemblyRendering name
            r.Emit(html, R.Js)

let activate resource =
    match resource with
    | AssemblyResource name ->
        AssemblyResource name :> R.IResource
    | UserResource t ->
        try
            t.Load()
            |> System.Activator.CreateInstance
            |> unbox
        with e ->
            {
                new R.IResource with
                    member this.Render ctx writer =
                        writer.Write("<-- ")
                        writer.Write("Failed to load: {0}", t)
                        writer.WriteLine(" -->")
                        ()
            }

type Info =
    {
        getAddress : Type -> option<P.Address>
        getDependencies : seq<Node> -> list<R.IResource>
        getFieldName : Type -> string -> string
        getMethod : MethodHandle -> option<Re.Method>
    }

    static member Create(infos: seq<AssemblyInfo>) =
        let infos = Seq.toArray infos
        let records = D()
        let requirements = D()
        let types = D()
        let remote = D()
        for info in infos do
            for KeyValue (r, kv) in info.records do
                for (k, v) in kv do
                    add (r, k) v records
            for (k, rs) in info.Requirements do
                let rs =
                    rs
                    |> List.map (fun r -> (r, activate r))
                requirements.[k] <- rs
            for KeyValue (k, v) in info.types do
                add k v types
            for KeyValue (k, v) in info.remote do
                add v k remote
        let getFieldName t f =
            match records.TryGetValue((t, f)) with
            | true, f -> f
            | _ -> f
        let getAddress t =
            match types.TryGetValue t with
            | true, n -> Some n
            | _ -> None
        let getNodeDeps node =
            match requirements.TryGetValue node with
            | true, xs -> xs
            | _ -> []
        let getDeps (nodes: seq<Node>) =
            Seq.collect getNodeDeps nodes
            |> Seq.distinctBy fst
            |> Seq.map snd
            |> Seq.toList
        let getMethod m =
            match remote.TryGetValue m with
            | true, x -> Some x
            | _ -> None
        {
            getFieldName = getFieldName
            getAddress = getAddress
            getDependencies = getDeps
            getMethod = getMethod
        }

    member this.GetAddress ty =
        this.getAddress ty

    member this.GetFieldName t f =
        this.getFieldName t f

    member this.GetRemoteMethod m =
        this.getMethod m

    member this.GetDependencies nodes =
        this.getDependencies nodes

