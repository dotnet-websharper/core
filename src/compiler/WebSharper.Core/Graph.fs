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

namespace WebSharper.Core.DependencyGraph

open System.Collections.Generic

open WebSharper.Core

open WebSharper.Core.AST
open WebSharper.Core.Metadata

module R = Resources 

/// A resource class for including the compiled .js for an assembly in Sitelets
[<Sealed>]
type AssemblyResource(name) =
    interface R.IResource with
        member this.Render ctx writer =
            let fullAsmName =
                System.AppDomain.CurrentDomain.GetAssemblies()
                |> Seq.tryPick (fun a -> 
                    if a.FullName.StartsWith (name + ",") then Some (a.FullName) else None          
                )
            let r =
                match fullAsmName with
                | Some fullAsmName ->
                    let filename = name + if ctx.DebuggingEnabled then ".js" else ".min.js"
                    match R.Rendering.TryGetCdn(ctx, name, filename) with
                    | Some r -> r
                    | None -> ctx.GetAssemblyRendering name
                | None -> ctx.GetAssemblyRendering name
            r.Emit(writer R.Scripts, R.Js)

/// The compilation-time mutable representation of a code dependency graph
type Graph =
    {
        Nodes : ResizeArray<Node>
        Edges : ResizeArray<HashSet<int>>
        /// Nested dictionary lookup order: TypeNode -> AbstractMethodNode -> ImplementationNode 
        Overrides : IDictionary<int, IDictionary<int, int>>
        Lookup : IDictionary<Node, int>
        Resources : IDictionary<int, Resources.IResource>
        Requires : IDictionary<int, int[]> 
        NewNodes : ResizeArray<int>
    }

    /// Create a mutable graph from parsed immutable graph data
    static member FromData (data: GraphData) =
        let lookup = Dictionary()
        data.Nodes |> Seq.iteri (fun i n -> lookup.Add(n, i))
        let overrides = Dictionary()
        for typ, ms in data.Overrides do
            let o = Dictionary()
            for td, d in ms do o.Add(td, d)
            overrides.Add(typ, o :> IDictionary<_,_>)
        {
            Nodes = ResizeArray(data.Nodes)
            Edges = ResizeArray(data.Edges |> Array.map (fun e -> HashSet(e)))
            Overrides = overrides
            Lookup = lookup
            Resources = Dictionary()
            Requires = Dictionary()
            NewNodes = ResizeArray()
        }

    /// Create a mutable graph from a sequence of parsed immutable graph data
    static member FromData (data: seq<GraphData>) =
        let graphs = Array.ofSeq data
            
        let nodes = ResizeArray()
        let lookup = Dictionary()
        let edges = ResizeArray() 
        let overrides = Dictionary()
        for i = 0 to graphs.Length - 1 do
            let g = graphs.[i]
            let r = Dictionary() // redirects
            for j = 0 to g.Nodes.Length - 1 do
                let jn = g.Nodes.[j]
                let jr =
                    match lookup.TryFind jn with 
                    | Some jr -> jr
                    | _ ->
                        let jr = nodes.Count
                        nodes.Add jn 
                        edges.Add(HashSet())
                        lookup.Add(jn, jr)
                        jr
                r.Add(j, jr)
            
            for j = 0 to g.Edges.Length - 1 do
                let ejr = edges.[r.[j]]
                for d in g.Edges.[j] do
                    ejr.Add r.[d] |> ignore
            
            for typ, ors in g.Overrides do
                let o = Dictionary() :> IDictionary<_,_>
                overrides.Add(r.[typ], o)
                for a, n in ors do
                    o.Add(r.[a], r.[n])                 
        {
            Nodes = nodes
            Edges = edges
            Overrides = overrides
            Lookup = lookup
            Resources = Dictionary()
            Requires = Dictionary()
            NewNodes = ResizeArray()
        }

    member this.GetCurrentData() =
        let allNodes = SortedSet(this.NewNodes)
        // adding all assembly nodes cause them to be correctly ordered on unioning 
        this.Nodes |> Seq.iteri (fun i n ->
            match n with
            | AssemblyNode _ -> allNodes.Add i |> ignore
            | _ -> ()        
        )
        for n in this.NewNodes do
            for d in this.Edges.[n] do 
                allNodes.Add d |> ignore
            match this.Overrides.TryFind n with
            | Some td ->
                for KeyValue(a, m) in td do
                    allNodes.Add a |> ignore
                    allNodes.Add m |> ignore                   
            | _ -> ()
        let r = Dictionary() // redirects
        let nodes = ResizeArray()
        for i in allNodes do
            r.Add(i, nodes.Count)
            nodes.Add(this.Nodes.[i])            
        {
            GraphData.Nodes = nodes.ToArray()
            Edges =
                allNodes |> Seq.map (fun i ->
                    if this.NewNodes.Contains i then
                        this.Edges.[i] |> Seq.map (fun d -> r.[d]) |> Array.ofSeq  
                    else [||] 
                ) |> Array.ofSeq
            Overrides =
                this.NewNodes |> Seq.choose (fun tn ->
                    match this.Overrides.TryFind tn with
                    | Some td ->
                        Some (
                            r.[tn],
                            td |> Seq.map (fun (KeyValue (td, m)) -> r.[td], r.[m]) |> Array.ofSeq
                        )
                    | _ -> None
                ) |> Array.ofSeq
        }

    member this.GetData() =
        {
            GraphData.Nodes = this.Nodes.ToArray()
            Edges = this.Edges.ToArray() |> Array.map Array.ofSeq
            Overrides = 
                this.Overrides 
                |> Seq.map (fun (KeyValue (typ, ms)) -> 
                    typ,
                    ms |> Seq.map (fun (KeyValue(td, d)) -> td, d) |> Array.ofSeq
                ) 
                |> Array.ofSeq
        } 
    
    member this.GetDependencies (nodes : seq<Node>) =
        let allNodes = HashSet()
        let newNodes = HashSet()
        let allTypesWithOverrides = HashSet()
        let newTypesWithOverrides = HashSet()
        let allAbstractMembers = HashSet()
        let newAbstractMembers = HashSet()

//        let jqRes =
//            ResourceNode <|
//                Hashed { Assembly = "WebSharper.JQuery"; FullName = "WebSharper.JQuery.Resources.JQuery" }
//        this.Lookup.TryFind jqRes |> Option.iter (allNodes.Add >> ignore)

        let addTypeWithOverride i =
            if this.Overrides.ContainsKey i then newTypesWithOverrides.Add i |> ignore  
        
        let addNode n i =
            if allNodes.Add i then
                newNodes.Add i |> ignore
                match n with
                | AbstractMethodNode (td, m) -> this.Lookup.TryFind (AbstractMethodNode (td, m)) |> Option.iter (newAbstractMembers.Add >> ignore) 
                | TypeNode td -> this.Lookup.TryFind (TypeNode td) |> Option.iter addTypeWithOverride
                | _ -> ()
        
        nodes |> Seq.iter (fun n -> this.Lookup.TryFind n |> Option.iter (addNode n))

        while newNodes.Count > 0 do
            let currentNodes = Array.ofSeq newNodes
            newNodes.Clear()
            
            for n in currentNodes do
                this.Edges.[n] |> Seq.iter (fun i -> addNode this.Nodes.[i] i)
            
            if newTypesWithOverrides.Count > 0 || newAbstractMembers.Count > 0 then 
                let currentTypes = Set.ofSeq newTypesWithOverrides
                allTypesWithOverrides.UnionWith(newTypesWithOverrides)
                newTypesWithOverrides.Clear()
            
                let currentAbstractMembers = Array.ofSeq newAbstractMembers
                allAbstractMembers.UnionWith(newAbstractMembers)
                newAbstractMembers.Clear()
            
                for typ in allTypesWithOverrides do
                    let ors = this.Overrides.[typ] 
                    let currentOrAllAbstractMembers =
                        if currentTypes.Contains typ then 
                            allAbstractMembers :> _ seq
                        else currentAbstractMembers :> _ seq   
                    for mem in currentOrAllAbstractMembers do
                        ors.TryFind mem |> Option.iter (fun i -> addNode this.Nodes.[i] i)
        
        allNodes |> Seq.map (fun n -> this.Nodes.[n]) |> List.ofSeq

    member private this.GetRequires (node: int) =
        let allNodes = HashSet()
        let newNodes = HashSet()
        let allTypesWithOverrides = HashSet()
        let newTypesWithOverrides = HashSet()
        let allAbstractMembers = HashSet()
        let newAbstractMembers = HashSet()
        let resNodes = HashSet()

//        let jqRes =
//            ResourceNode <|
//                Hashed { Assembly = "WebSharper.JQuery"; FullName = "WebSharper.JQuery.Resources.JQuery" }
//        this.Lookup.TryFind jqRes |> Option.iter (allNodes.Add >> ignore)

        let addTypeWithOverride i =
            if this.Overrides.ContainsKey i then newTypesWithOverrides.Add i |> ignore  
        
        let addNode n i =
            if allNodes.Add i then
                match n with
                | ResourceNode _ 
                | AssemblyNode _ ->
                    resNodes.Add i |> ignore
                | _ ->
                newNodes.Add i |> ignore
                match n with
                | AbstractMethodNode (td, m) -> this.Lookup.TryFind (AbstractMethodNode (td, m)) |> Option.iter (newAbstractMembers.Add >> ignore) 
                | TypeNode td -> this.Lookup.TryFind (TypeNode td) |> Option.iter addTypeWithOverride
                | _ -> ()

        let addFirstNode n i =
            if allNodes.Add i then
                newNodes.Add i |> ignore
                match n with
                | AbstractMethodNode (td, m) -> this.Lookup.TryFind (AbstractMethodNode (td, m)) |> Option.iter (newAbstractMembers.Add >> ignore) 
                | TypeNode td -> this.Lookup.TryFind (TypeNode td) |> Option.iter addTypeWithOverride
                | _ -> ()
        
        addFirstNode this.Nodes.[node] node

        while newNodes.Count > 0 do
            let currentNodes = Array.ofSeq newNodes
            newNodes.Clear()
            
            for n in currentNodes do
                this.Edges.[n] |> Seq.iter (fun i -> addNode this.Nodes.[i] i)
            
            if newTypesWithOverrides.Count > 0 || newAbstractMembers.Count > 0 then 
                let currentTypes = Set.ofSeq newTypesWithOverrides
                allTypesWithOverrides.UnionWith(newTypesWithOverrides)
                newTypesWithOverrides.Clear()
            
                let currentAbstractMembers = Array.ofSeq newAbstractMembers
                allAbstractMembers.UnionWith(newAbstractMembers)
                newAbstractMembers.Clear()
            
                for typ in allTypesWithOverrides do
                    let ors = this.Overrides.[typ] 
                    let currentOrAllAbstractMembers =
                        if currentTypes.Contains typ then 
                            allAbstractMembers :> _ seq
                        else currentAbstractMembers :> _ seq   
                    for mem in currentOrAllAbstractMembers do
                        ors.TryFind mem |> Option.iter (fun i -> addNode this.Nodes.[i] i)
        
        resNodes :> _ seq

//    member private this.GetCachedRequires stack (node: int) =
//        match this.Requires.TryFind node with
//        | Some cached -> 
//            if obj.ReferenceEquals(cached, null) then
//                failwithf "Looped resource nodes: %s"
//                    (node :: stack |> List.map (fun n -> sprintf "%+A" this.Nodes.[n]) |> String.concat "<-") 
//            else cached
//        | _ ->
//            let directReqs = this.GetRequires(node)
//            this.Requires.[node] <- null 
//            let res =
//                directReqs
//                |> Seq.collect (this.GetCachedRequires (node :: stack))
//                |> Seq.append directReqs
//                |> Seq.distinct
//                |> Array.ofSeq
//            this.Requires.[node] <- res
//            res

    member private this.GetCachedRequires (node: int) =
        match this.Requires.TryFind node with
        | Some cached -> 
            if obj.ReferenceEquals(cached, null) then
                failwithf "Looped resource node: %+A" this.Nodes.[node]
            else cached
        | _ ->
            let directReqs = this.GetRequires(node)
            this.Requires.[node] <- null 
            let otherReqs = directReqs |> Seq.collect this.GetCachedRequires
            let res = Seq.append otherReqs directReqs |> Seq.distinct |> Array.ofSeq
            this.Requires.[node] <- res
            res

    member this.GetResources (nodes : seq<Node>) =
        let activate resource =
            match resource with
            | AssemblyNode (name, true) ->
                AssemblyResource name :> R.IResource
            | ResourceNode t ->
                try
                    Reflection.LoadTypeDefinition t
                    |> System.Activator.CreateInstance
                    |> unbox
                with e ->
                    {
                        new R.IResource with
                            member this.Render ctx writer =
                                let writer = writer R.Scripts
                                writer.Write("<-- ")
                                writer.Write("Failed to load: {0}; because of: {1}", t, e.Message)
                                writer.WriteLine(" -->")
                                ()
                    }
            | _ -> failwith "not a resource node"
        
        let res = 
            nodes |> Seq.collect (fun n ->
                match this.Lookup.TryFind n with
                | None -> Seq.empty
                | Some i ->
                    this.GetCachedRequires i |> Seq.ofArray
            )
            |> Seq.distinct |> Seq.sort
            |> Seq.choose (fun i ->
                let n = this.Nodes.[i]
                match n with
                | AssemblyNode (_, true)
                | ResourceNode _ ->
                    let i = this.Lookup.[n]
                    match this.Resources.TryFind i with
                    | Some _ as found -> found
                    | _ ->
                        let res = activate n
                        this.Resources.Add(i, res)
                        Some res
                | _ -> None
            ) 
            |> List.ofSeq

        Resources.Runtime.Instance :: res
   
     member this.GetResourcesOf (nodes : seq<Node>) =
        nodes |> Seq.filter (fun n ->
            match n with
            | AssemblyNode (_, true)
            | ResourceNode _ ->
                true
            | _ -> false
        )
        |> Seq.distinct
        |> this.GetResources

    member this.AddOrLookupNode n =
        match this.Lookup.TryFind n with
        | Some i -> i
        | _ ->
            let i = this.Nodes.Count
            this.Nodes.Add n
            this.Edges.Add(HashSet())
            this.Lookup.Add(n, i)
            this.NewNodes.Add i
            i  

    member this.GetNodeDeps n =
        this.Edges.[this.AddOrLookupNode n]    

    member this.AddEdge (n, d) = this.Edges.[n].Add(d) |> ignore
    member this.AddEdge (n, d) = this.Edges.[n].Add(this.AddOrLookupNode d) |> ignore
    member this.AddEdge (n, d) = this.Edges.[this.AddOrLookupNode n].Add(d) |> ignore
    member this.AddEdge (n, d) = this.Edges.[this.AddOrLookupNode n].Add(this.AddOrLookupNode d) |> ignore
    
    member this.AddOverride (typ, btyp, meth) =
        let typeNode = this.AddOrLookupNode (TypeNode typ)
        let abstractMethodNode = this.AddOrLookupNode (AbstractMethodNode (btyp, meth))
        let methodNode = this.AddOrLookupNode (MethodNode (typ, meth))
        let ovr =
            match this.Overrides.TryFind typeNode with
            | None ->
                let o = new Dictionary<_,_>() :> IDictionary<_,_>
                this.Overrides.Add(typeNode, o)
                o
            | Some o -> o
        ovr.Add(abstractMethodNode, methodNode)

    member this.AddImplementation (typ, intf, meth) =
        let typeNode = this.AddOrLookupNode (TypeNode typ)
        let abstractMethodNode = this.AddOrLookupNode (AbstractMethodNode (intf, meth))
        let implementationNode = this.AddOrLookupNode (ImplementationNode (typ, intf, meth))
        let ovr =
            match this.Overrides.TryFind typeNode with
            | None ->
                let o = new Dictionary<_,_>() :> IDictionary<_,_>
                this.Overrides.Add(typeNode, o)
                o
            | Some o -> o
        ovr.Add(abstractMethodNode, implementationNode)

    static member UnionOfMetadata (metas: seq<Info>) =
        { Info.UnionWithoutDependencies metas with
            Dependencies = Graph.FromData(metas |> Seq.map (fun m -> m.Dependencies)).GetData()    
        }

    static member Empty : Graph =
        {
            Nodes = ResizeArray()
            Edges = ResizeArray()
            Overrides = Dictionary()
            Lookup = Dictionary()
            Resources = Dictionary()
            Requires = Dictionary()
            NewNodes = ResizeArray()
        }

