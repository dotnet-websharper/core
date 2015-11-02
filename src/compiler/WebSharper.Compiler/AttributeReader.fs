module WebSharper.Compiler.AttributeReader

open WebSharper.Core
open WebSharper.Core.AST

module M = WebSharper.Core.Metadata

[<RequireQualifiedAccess>]
type private Attribute =
    | Macro of TypeDefinition * option<obj>
    | Proxy of TypeDefinition
    | Inline of option<string>
    | Direct of string
    | Constant of Expression
    | Generated of TypeDefinition * option<obj>
    | Require of TypeDefinition //* option<obj>
    | Name of string
    | Stub
    | OptionalField
    | JavaScript
    | Remote
    | RemotingProvider of TypeDefinition
    | NamedUnionCases of option<string>
    | DateTimeFormat of option<string> * string
    | Website of TypeDefinition

type private A = Attribute

type TypeAnnotation = 
    {
        ProxyOf : option<TypeDefinition>
        IsJavaScript : bool
        IsStub : bool
        OptionalFields : bool
        Name : option<string>
        Requires : list<TypeDefinition>
        NamedUnionCases : option<option<string>>
        DateTimeFormat : list<option<string> * string>
    }

    static member Empty =
        {
            ProxyOf = None
            IsJavaScript = false
            IsStub = false
            OptionalFields = false
            Name = None
            Requires = []
            NamedUnionCases = None
            DateTimeFormat = []
        }

type MemberKind = 
    | Inline of string
    | Direct of string
    | InlineJavaScript
    | JavaScript
    | Constant of Expression
    | Macro of TypeDefinition * option<obj> * option<MemberKind>
    | Generated of TypeDefinition * option<obj>
    | InlineGenerated of TypeDefinition * option<obj>
    | Remote
    | Stub
    | OptionalField

type MemberAnnotation =
    {
        Kind : option<MemberKind>
        Name : option<string>
        Requires : list<TypeDefinition>
    }

type AssemblyAnnotation =
    {
        SiteletDefinition : option<TypeDefinition>
        Requires : list<TypeDefinition>
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
//        let typeWithOptParam () =
//            this.ReadTypeArg 
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
            Some (A.Require (this.ReadTypeArg attr |> fst))
        | "StubAttribute" ->
            Some A.Stub
        | "NameAttribute" ->
            Some (A.Name (Seq.head (this.GetCtorArgs(attr)) |> unbox))
        | "JavaScriptAttribute" ->
            Some A.JavaScript
        | "OptionalFieldAttribute" ->
            Some A.OptionalField
        | "RemotingProviderAttribute" ->
            Some (A.RemotingProvider (this.ReadTypeArg attr |> fst))
        | "NamedUnionCasesAttribute" ->
            Some (A.NamedUnionCases (Seq.tryHead (this.GetCtorArgs(attr)) |> Option.map unbox))
        | "DateTimeFormatAttribute" ->
            match this.GetCtorArgs(attr) with
            | [| f |] -> Some (A.DateTimeFormat (None, unbox f))
            | [| a; f |] -> Some (A.DateTimeFormat (Some (unbox a), unbox f)) 
            | _ -> failwith "invalid constructor arguments for DateTimeFormatAttribute"
        | "WebsiteAttribute" ->
            Some (A.Website (this.ReadTypeArg attr |> fst))
        | _ -> None        

    member private this.GetAttrs (parent: TypeAnnotation, attrs: seq<'A>) =
        let attrArr = ResizeArray()
        let mutable name = None
        let reqs = ResizeArray()
        for a in attrs do
            if this.GetAssemblyName a = "WebSharper.Core" then
                match this.Read a with
                | Some ar ->
                    match ar with 
                    | A.Name n -> name <- Some n
                    | A.Require t -> reqs.Add t
                    | _ -> attrArr.Add ar
                | None -> ()
        if parent.IsJavaScript then
            if not (attrArr.Contains(A.JavaScript)) then attrArr.Add A.JavaScript
        if parent.IsStub then 
            if not (attrArr.Contains(A.Stub)) then attrArr.Add A.Stub
        if parent.OptionalFields then
            if not (attrArr.Contains(A.OptionalField)) then attrArr.Add A.OptionalField
        attrArr.ToArray(), name, List.ofSeq reqs

    member this.GetTypeAnnot (parent: TypeAnnotation, attrs: seq<'A>) =
        let attrArr, name, reqs = this.GetAttrs (parent, attrs)
//        let kind =
//            match attrArr with
//            | [||] -> None
//            | [| A.JavaScript |] -> Some Client
//            | [| A.Remote |] -> Some Server
//            | [| A.Stub |] -> Some StubType
//            | [| A.Proxy p |]
//            | [| A.Proxy p; A.JavaScript |] 
//            | [| A.JavaScript; A.Proxy p |] -> Some (Proxy p)
//            | [| A.OptionalField |]
//            | [| A.OptionalField; A.JavaScript |]
//            | [| A.JavaScript; A.OptionalField |] -> Some (OptionalFieldType)
//            // TODO
//            | [| A.NamedUnionCases _ |] -> None
//            // TODO
//            | [| A.DateTimeFormat _ |] -> None
//            | _ -> failwith "Incompatible attributes" // TODO : warning only, location
        let proxyOf = attrArr |> Array.tryPick (function A.Proxy p -> Some p | _ -> None) 
        {
            ProxyOf = proxyOf
            IsJavaScript = Option.isSome proxyOf || attrArr |> Array.exists (function A.JavaScript -> true | _ -> false)
            IsStub = attrArr |> Array.exists (function A.Stub -> true | _ -> false)
            OptionalFields = attrArr |> Array.exists (function A.OptionalField -> true | _ -> false)
            Name = name
            Requires = reqs
            NamedUnionCases = attrArr |> Array.tryPick (function A.NamedUnionCases uc -> Some uc | _ -> None)
            DateTimeFormat = attrArr |> Seq.choose (function A.DateTimeFormat (a,b) -> Some (a,b) | _ -> None) |> List.ofSeq
        }

    member this.GetMemberAnnot (parent: TypeAnnotation, attrs: seq<'A>) =
        let attrArr, name, reqs = this.GetAttrs (parent, attrs)
        let rec getKind attrArr =
            match attrArr with
            | [||] -> None
            | _ ->
            match attrArr |> Array.tryPick (function A.Macro (m, p) -> Some (m, p) | _ -> None) with
            | Some (m, p) -> Some (Macro (m, p, getKind (attrArr |> Array.filter (function A.Macro _ -> false | _ -> true))))
            | _ ->
//            let (|IgnoreJavaScript|_|) a =
//                match a with
//                | [| x |]
//                | [| x; A.JavaScript |]
//                | [| A.JavaScript; x |] -> Some x
//                | _ -> None
            match attrArr with
            | [| A.Remote |] -> Some Remote
            | [| A.JavaScript |] -> Some JavaScript 
            | [| A.Inline None |]
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
            // TODO
            | A.DateTimeFormat _ -> None
            | _ -> failwith "Incompatible attributes" // TODO : warning only, location
        {
            Kind = getKind attrArr
            Name = name
            Requires = reqs
        }
   
    member this.GetAssemblyAnnot (attrs: seq<'A>) =
        let reqs = ResizeArray()
        let mutable sitelet = None
        for a in attrs do
            let aAsmName = this.GetAssemblyName a
            if aAsmName = "WebSharper.Core" || aAsmName = "WebSharper.Sitelets" then
                match this.Read a with
                | Some ar ->
                    match ar with
                    | A.Require t -> reqs.Add t
                    | A.Website t -> sitelet <- Some t
                    | _ -> ()
                | None -> ()  
             
        {
            SiteletDefinition = sitelet
            Requires = reqs |> List.ofSeq
        }        
           
//type MetadataReader