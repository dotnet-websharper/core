module WebSharper.Compiler.Common.AttributeReader

open WebSharper.Core
open WebSharper.Core.AST

[<RequireQualifiedAccess>]
type private Attribute =
    | Macro of TypeDefinition * option<obj>
    | Proxy of TypeDefinition
    | Inline of option<string>
    | Direct of string
    | Constant of Expression
    | Generated of TypeDefinition * option<obj>
    | Require of TypeDefinition * option<obj>
    | Name of string
    | Stub
    | OptionalField
    | JavaScript
    | Remote

type private A = Attribute

type TypeKind =
    | Proxy of TypeDefinition
    | Client
    | Server

type MemberKind = 
    | Inline of string
    | Direct of string
    | InlineJavaScript
    | JavaScript
    | Constant of Expression
    | Macro of TypeDefinition * option<obj> * option<MemberKind>
    | Generated of TypeDefinition * option<obj>
    | Remote
    | Stub
    | OptionalField
    
type Annotation<'T> =
    {
        Kind : 'T
        Name : option<string>
        Requires : list<TypeDefinition * option<obj>>
    } 

[<AbstractClass>]
type AttributeReader<'A>() =
    abstract GetAssemblyName : 'A -> string
    abstract GetName : 'A -> string
    abstract GetCtorArgs : 'A -> obj[]
    abstract GetTypeDef : obj -> TypeDefinition

    member private this.ReadTypeArg(attr: 'A) =
        let args = this.GetCtorArgs(attr)
        let def =
            match args.[0] with
            | :? string as s ->
                let ss = s.Split([|','|])
                Hashed {
                    FullName = ss.[0].Trim()
                    Assembly = ss.[1].Trim()
                }
            | t -> 
                try this.GetTypeDef t
                with _ -> failwith "Failed to parse type argument of attribute."
        let param =
            if args.Length = 2 then
                Some args.[1]
            else None
        def, param

    member private this.Read (attr: 'A) =
        let typeWithOptParam () =
            this.ReadTypeArg 
        match this.GetName(attr) with
        | "ProxyAttribute" ->
            Some (A.Proxy (this.ReadTypeArg attr |> fst))
        | "InlineAttribute" ->
            Some (A.Inline (Seq.tryHead (this.GetCtorArgs(attr)) |> Option.map unbox))
        | "DirectAttribute" ->
            Some (A.Direct (Seq.head (this.GetCtorArgs(attr)) |> unbox))
        | "ConstantAttribute" ->
            Some (A.Constant (Seq.head (this.GetCtorArgs(attr)) |> getConstrantValue))
        | "MacroAttribute" ->
            Some (A.Macro (this.ReadTypeArg attr))
        | "GeneratedAttribute" ->
            Some (A.Generated (this.ReadTypeArg attr))
        | "RemoteAttribute" ->
            Some A.Remote
        | "RequireAttribute" ->
            Some (A.Require (this.ReadTypeArg attr))
        | "StubAttribute" ->
            Some A.Stub
        | "NameAttribute" ->
            Some (A.Name (Seq.head (this.GetCtorArgs(attr)) |> unbox))
        | "JavaScriptAttribute" ->
            Some A.JavaScript
        | "OptionalFieldAttribute" ->
            Some A.OptionalField
        | _ -> None        

    member private this.GetAttrs (parent: option<TypeKind>, attrs: seq<'A>) =
        let attrArr = ResizeArray()
        let mutable name = None
        let reqs = ResizeArray()
        for a in attrs do
            if this.GetAssemblyName a = "CommonAST" then
                match this.Read a with
                | Some ar ->
                    match ar with 
                    | A.Name n -> name <- Some n
                    | A.Require (t, p) -> reqs.Add (t, p)
                    | _ -> attrArr.Add ar
                | None -> ()
        match parent with
        | Some (Proxy _)
        | Some Client -> if not (attrArr.Contains(A.JavaScript)) then attrArr.Add A.JavaScript 
        | Some Server -> if not (attrArr.Contains(A.Remote)) then attrArr.Add A.Remote 
        | _ -> ()
        attrArr.ToArray(), name, List.ofSeq reqs

    member this.GetTypeAnnot (parent: option<TypeKind>, attrs: seq<'A>) =
        let attrArr, name, reqs = this.GetAttrs (parent, attrs)
        let kind =
            match attrArr with
            | [||] -> None
            | [| A.JavaScript |] -> Some Client
            | [| A.Remote |] -> Some Server
            | [| A.Proxy p |]
            | [| A.Proxy p; A.JavaScript |] 
            | [| A.JavaScript; A.Proxy p |]-> Some (Proxy p)
            | _ -> failwith "Incompatible attributes" // TODO : warning only, location
        {
            Kind = kind
            Name = name
            Requires = reqs
        }

    member this.GetMemberAnnot (parent: option<TypeKind>, attrs: seq<'A>) =
        let attrArr, name, reqs = this.GetAttrs (parent, attrs)
        let rec getKind attrArr =
            match attrArr with
            | [||] -> None
            | _ ->
            match attrArr |> Array.tryPick (function A.Macro (m, p) -> Some (m, p) | _ -> None) with
            | Some (m, p) -> Some (Macro (m, p, getKind (attrArr |> Array.filter (function A.Macro _ -> false | _ -> true))))
            | _ ->
            let (|IgnoreJavaScript|_|) a =
                match a with
                | [| x |]
                | [| x; A.JavaScript |]
                | [| A.JavaScript; x |] -> Some x
                | _ -> None
            match attrArr with
            | [| A.JavaScript |] -> Some JavaScript 
            | [| A.Inline None; A.JavaScript |]
            | [| A.JavaScript; A.Inline None |] -> Some InlineJavaScript
            | _ ->
            let a =
                match attrArr with
                | [| a |]
                | [| a; A.JavaScript |]
                | [| A.JavaScript; a |] -> a
                | _ -> failwith "Incompatible attributes"
            match a with
            | A.Inline (Some i) -> Some (Inline i)
            | A.Direct s -> Some (Direct s)
            | A.Constant x -> Some (Constant x)
            | A.Generated (g, p) -> Some (Generated (g, p))
            | A.Stub -> Some Stub
            | A.OptionalField -> Some OptionalField
            | _ -> failwith "Incompatible attributes" // TODO : warning only, location
        {
            Kind = getKind attrArr
            Name = name
            Requires = reqs
        }
           
//type MetadataReader