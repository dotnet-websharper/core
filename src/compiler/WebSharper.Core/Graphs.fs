module WebSharper.Core.Graphs

open System.Collections.Generic

type INode =
    abstract Id: int with get, set

type Graph<'Ext> =
    {
        Lookup : IDictionary<int, list<INode>>
        Edges : IDictionary<int, list<int>>
        External : IDictionary<int, 'Ext>
    }

let findSCCs (graph: Graph<_>) =
    // true: in current path, false: already explored
    let visited = Dictionary<int, bool>()
    let searchPath = Stack<int * list<INode>>()
    
    let rec visit i =
//        if lookup.TryGetValue i then
//        | Some 
            match visited.TryGetValue i with
            | true, true ->
                let collapsing = [
                    while fst (searchPath.Peek()) <> i do 
                        yield! snd (searchPath.Pop())
                ]
                let toId = fst (searchPath.Peek())
                collapsing |> List.iter (fun n -> n.Id <- toId)
            | true, false -> ()
            | false, _ ->
                match graph.Lookup.TryGetValue i with
                | true, e ->
                    visited.Add(i, true)
                    searchPath.Push(i, e)
                    Dict.getFromMulti graph.Edges i |> List.iter visit
                    searchPath.Pop() |> ignore
                    visited.Add(i, false)
                | _ -> ()
                  
    graph.Edges.Keys |> Seq.iter visit

let union (graphs: Graph<'Ext>[]) (findExternal : 'Ext -> int * int) =
    let redirects = Array.init graphs.Length (fun _ -> Dictionary<int, int>())
            
    let mutable k = 0
    for i = 0 to graphs.Length - 1 do
        let g = graphs.[i]
        let r = redirects.[i]
        for j in g.Lookup.Keys do
            r.Add(j, k)             
            k <- k + 1

    for i = 0 to graphs.Length - 1 do
        let g = graphs.[i]
        let r = redirects.[i]        
        for KeyValue(j, ext) in g.External do
            let ni, nj = findExternal ext
            r.Add(j, redirects.[ni].[nj])    

    let lookup = Dictionary()
    let edges = Dictionary()

    for i = 0 to graphs.Length - 1 do
        let g = graphs.[i]
        let r = redirects.[i]
        for KeyValue(j, nodes) in g.Lookup do
            let rj = r.[j]
            for n in nodes do n.Id <- rj
            lookup.Add(rj, nodes)   
        for KeyValue(f, ts) in g.Edges do             
            try
                let rf = r.[f]
                let rts = ts |> List.map (fun t -> r.[t]) 
                edges.Add(rf, rts)
            with _ -> 
                // TODO: investigate
                ()
                 
    {
        Lookup = lookup
        Edges = edges
        External = dict []
    }

                