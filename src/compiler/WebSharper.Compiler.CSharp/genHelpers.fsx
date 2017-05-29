type Field =
    {
        Name : string
        Type : string
        IsList : bool
        Optional : bool
        Kinds : string list
    }

type Node =
    {
        Name : string
        Base : string
        Kinds : string list
        Fields : Field list
    }

#r "System.Xml.Linq"
open System.Xml.Linq

module Option =
//    let ofObj option =
//        if obj.ReferenceEquals(option, null) then None else Some option
    let def value option =
        match option with
        | Some v -> v
        | _ -> value

module Xml = 
    let name (n: string) = XName.Get n
    let elems (n: string) (x: XContainer) = x.Elements(name n)
    let attr (n: string) (x: XElement) = x.Attribute(name n).Value
    let hasTrueAttr (n: string) (x: XElement) =
        x.Attribute(name n) |> Option.ofObj |> Option.exists (fun a -> a.Value = "true")

let (|SyntaxList|_|) (s: string) =
    if s.StartsWith "SyntaxList<" then Some s.[11 .. s.Length - 2]
    elif s.StartsWith "SeparatedSyntaxList<" then Some s.[20 .. s.Length - 2]
    else None

let xml = XDocument.Load (__SOURCE_DIRECTORY__ + @"\Syntax.xml")

let nodes, abstractNodes =
    let abstractNodes =
        Seq.append 
            (xml.Root |> Xml.elems "AbstractNode") 
            (xml.Root |> Xml.elems "PredefinedNode") 
        |> Seq.map (fun n ->
            n |> Xml.attr "Name"
            ,
            ResizeArray<string>()
        )
        |> Map.ofSeq   

    xml.Root |> Xml.elems "AbstractNode"
    |> Seq.iter (fun n ->
        let baseTy = n |> Xml.attr "Base"
        abstractNodes.[baseTy].Add (n |> Xml.attr "Name") 
    )

    let getKinds n =
        n |> Xml.elems "Kind" |> Seq.map (Xml.attr "Name") |> List.ofSeq    
    let nodes =
        xml.Root |> Xml.elems "Node" |> Seq.map (fun n ->
            let name = n |> Xml.attr "Name"
            let baseTy = n |> Xml.attr "Base"
            let node =
                {
                    Node.Name = name
                    Base = baseTy
                    Kinds = n |> getKinds
                    Fields =
                        n |> Xml.elems "Field" |> Seq.map (fun f ->
                            let t, l =
                                match f |> Xml.attr "Type" with
                                | SyntaxList t -> t, true
                                | t -> t, false
                            {
                                Field.Name = f |> Xml.attr "Name"
                                Type = t
                                IsList = l
                                Optional = f |> Xml.hasTrueAttr "Optional"
                                Kinds = f |> getKinds
                            }     
                        )
                        |> List.ofSeq
                }
            abstractNodes.[baseTy].Add name
            name, node
        )
        |> Map.ofSeq
    nodes, abstractNodes |> Map.map (fun _ d -> List.ofSeq d)
  
let short (n: string) = n.Replace("Syntax", "")                                                
let data (n: string) = short n + "Data"

let keywords =
    Set ["type"; "default"; "else"; "finally"; "member"]

let fieldName (fn: string) =
    let lc = fn.[0 .. 0].ToLower() + fn.[1 ..]
    if keywords.Contains lc then lc + "_" else lc

let getStub (name: string) =

    let name = if name.StartsWith "Transform" then name.Replace("Transform", "") else name 
    let name = if name.EndsWith "Syntax" then name else name + "Syntax" 

    let stubCode = System.Text.StringBuilder()

    let inline rappendl x = stubCode.AppendLine ("    " + x) |> ignore
    let inline rprintfn x = Printf.kprintf rappendl x
    let inline remptyl() = stubCode.AppendLine() |> ignore 

    let retTy =
        if name.EndsWith "Expression" then "Expr"
        elif name.EndsWith "Statement" then "Statement"
        else "_"

    rprintfn "member this.Transform%s (x: %s) : %s =" (short name) (data name) retTy
    match nodes |> Map.tryFind name with
    | Some n ->
        for f in n.Fields do
            let ft = f.Type
            if ft <> "SyntaxToken" then
                let fn = f.Name
                let uncapitalize (s: string) = s
                if f.IsList then 
                    rprintfn "    let %s = x.%s |> Seq.map this.Transform%s |> List.ofSeq" (fieldName fn) fn (short ft)
                elif f.Optional then
                    rprintfn "    let %s = x.%s |> Option.map this.Transform%s" (fieldName fn) fn (short ft)
                else
                    rprintfn "    let %s = x.%s |> this.Transform%s" (fieldName fn) fn (short ft)
        if List.length n.Kinds > 1 then
            let unionName = short n.Name + "Kind"
            rprintfn "    match x.Kind with"
            for k in n.Kinds do
                rprintfn "    | %s.%s -> TODO x" unionName k
        rprintfn "    TODO x"     
        if name.EndsWith "Expression" then
            rprintfn "    |> withExprSourcePos env x.Node"
        elif name.EndsWith "Statement" then
            rprintfn "    |> withStatementSourcePos env x.Node"
    | None ->
        rprintfn "    match x with"
        let descendants = abstractNodes |> Map.find name
        let longest = descendants |> Seq.map (fun d -> (short d).Length) |> Seq.max
        for d in descendants do
            rprintfn "    | %s.%-*s x -> TODO x //this.Transform%s x" (data name) longest (short d) (short d)  

    System.Windows.Forms.Clipboard.SetText(string stubCode)

#if CODEGEN

let unionsHeader = """// Generated by genHelpers.fsx. Do not modify.

module WebSharper.Compiler.CSharp.RoslynHelpers

open Microsoft.CodeAnalysis
open Microsoft.CodeAnalysis.CSharp
open Microsoft.CodeAnalysis.CSharp.Syntax

let optionalToken (t: SyntaxToken) =
    match t.Kind() with
    | SyntaxKind.None -> None
    | _ -> Some t

"""

let generate() =

    let genCode = System.Text.StringBuilder(unionsHeader)

    let inline gappendl x = genCode.AppendLine x |> ignore
    let inline gprintfn x = Printf.kprintf gappendl x
    let inline gemptyl() = genCode.AppendLine() |> ignore 

    let recCode = System.Text.StringBuilder()

    let inline rappendl x = recCode.AppendLine x |> ignore
    let inline rprintfn x = Printf.kprintf rappendl x
    let inline remptyl() = recCode.AppendLine() |> ignore 

    let tokenTextNeeded (k: string) = k.Contains "Literal" || k.Contains "Identifier"

    let createTokenUnion name kinds =
        let longest = kinds |> Seq.map String.length |> Seq.max
        gprintfn "type [<RequireQualifiedAccess>] %s =" name
        for k in kinds do
            gprintfn "    | %-*s%s" longest k (if tokenTextNeeded k then " of string" else "")
        gprintfn "with"
        gprintfn "    static member FromToken(t: SyntaxToken) ="
        gprintfn "        match t.Kind() with"
        for k in kinds do
            gprintfn "        | SyntaxKind.%s -> %s%s" k k (if tokenTextNeeded k then " t.Text" else "")
        gprintfn "        | k -> failwithf \"Unexpected %s kind: %%O\" k" name
        gprintfn ""

    let createKindUnion name kinds =
        let longest = kinds |> Seq.map String.length |> Seq.max
        gprintfn "type [<RequireQualifiedAccess>] %s =" name
        for k in kinds do
            gprintfn "    | %-*s" longest k
        gprintfn "with"
        gprintfn "    static member FromKind(k: SyntaxKind) ="
        gprintfn "        match k with"
        for k in kinds do
            gprintfn "        | SyntaxKind.%s -> %s" k k
        gprintfn "        | _ -> failwithf \"Unexpected %s kind: %%O\" k" name
        gprintfn ""

    let mutable firstRec = true

    let typeOrAnd() =
        if firstRec then 
            firstRec <- false
            "type"
        else "and"

    let notVisited = 
        Set [
            "ExternAliasDirectiveSyntax"
            "UsingDirectiveSyntax"
            "AttributeListSyntax"   
            "TypeParameterConstraintClauseSyntax" 
        ]

    let visited = 
        System.Collections.Generic.HashSet ()

    let rec visitNode name =
        if notVisited.Contains name then false
        elif visited.Contains name then true else
        visited.Add name |> ignore
        match nodes |> Map.tryFind name with
        | Some n ->
            let kindMember =
                if List.length n.Kinds > 1 then
                    let unionName = short n.Name + "Kind"
                    createKindUnion unionName n.Kinds
                    Some <| sprintf "    member this.Kind = %s.FromKind(node.Kind())" unionName
                else None

            let fields = 
                n.Fields |> List.where (fun f -> visitNode f.Type)
        
            let members = 
                [
                    for f in fields do
                        let fn = f.Name
                        let ft = f.Type
                        if ft = "SyntaxToken" then
                            let fklen = f.Kinds |> List.length
                            if fklen > 1 then
                                let unionName = short name + fn
                                createTokenUnion unionName f.Kinds
                                if f.Optional then
                                    yield sprintf "    member this.%s = node.%s |> optionalToken |> Option.map %s.FromToken" fn fn unionName
                                else
                                    yield sprintf "    member this.%s = node.%s |> %s.FromToken" fn fn unionName
                            elif f.Kinds |> List.exists tokenTextNeeded then
                                yield sprintf  "    member this.%s = node.%s" fn fn
                        else
                            if f.IsList then 
                                yield sprintf "    member this.%s = node.%s |> Seq.map %s.FromNode" fn fn (data ft)
                            elif f.Optional then
                                yield sprintf "    member this.%s = node.%s |> Option.ofObj |> Option.map %s.FromNode" fn fn (data ft)
                            else
                                yield sprintf "    member this.%s = node.%s |> %s.FromNode" fn fn (data ft)
                ]

            rprintfn "%s %s(node: %s) =" (typeOrAnd()) (data name) name
            rprintfn "    member this.Node = node"
            kindMember |> Option.iter rappendl
            for m in members do
                rappendl m
            rprintfn "    static member FromNode(n: %s) = %s(n)" name (data name)
            remptyl()
            true

        | None ->
            let descendants = abstractNodes |> Map.find name |> List.where visitNode 
            if List.isEmpty descendants then false else
            rprintfn "%s [<RequireQualifiedAccess>] %s =" (typeOrAnd()) (data name)
            let longest = descendants |> Seq.map (fun d -> (short d).Length) |> Seq.max
            for d in descendants do
                rprintfn "    | %-*s of %s" longest (short d) (data d)
            rprintfn "with"
            rprintfn "    static member FromNode(n: %s) =" name
            rprintfn "        match n with"
            for d in descendants do
                rprintfn "        | :? %s as d -> %s (%s.FromNode(d))" d (short d) (data d)
            rprintfn "        | _ -> failwithf \"Unexpected descendant class of %s\"" name
            rprintfn "    member this.Node ="
            rprintfn "        match this with"
            for d in descendants do
                rprintfn "        | %s d -> d.Node :> %s" (short d) name
            remptyl()
            true

    visitNode "CompilationUnitSyntax" |> ignore

    genCode.Append(string recCode) |> ignore

    System.IO.File.WriteAllText(__SOURCE_DIRECTORY__ + @"\RoslynHelpers.fs", string genCode)

#endif