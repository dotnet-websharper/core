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
    | SPAEntryPoint

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
        Macros : list<TypeDefinition * option<obj>>
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
            Macros = []
        }

type MemberKind = 
    | Inline of string
    | Direct of string
    | InlineJavaScript
    | JavaScript
    | Constant of Expression
    | NoFallback
    | Generated of TypeDefinition * option<obj>
    | Remote
    | Stub
    | OptionalField

type MemberAnnotation =
    {
        Kind : option<MemberKind>
        Macros : list<TypeDefinition * option<obj>> 
        Name : option<string>
        Requires : list<TypeDefinition>
        IsEntryPoint : bool
    }

    static member BasicJavaScript =
        {
            Kind = Some JavaScript
            Macros = []
            Name = None
            Requires = []
            IsEntryPoint = false
        }

    static member BasicInlineJavaScript =
        {
            Kind = Some InlineJavaScript
            Macros = []
            Name = None
            Requires = []
            IsEntryPoint = false
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
            Some (A.Constant (Seq.head (this.GetCtorArgs(attr)) |> getConstantValue))
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
        | "SPAEntryPointAttribute" ->
            Some A.SPAEntryPoint
        | _ -> None        

    member private this.GetAttrs (parent: TypeAnnotation, attrs: seq<'A>) =
        let attrArr = ResizeArray()
        let mutable name = None
        let reqs = ResizeArray()
        let macros = ResizeArray() 
        for a in attrs do
            if this.GetAssemblyName a = "WebSharper.Core" then
                match this.Read a with
                | Some ar ->
                    match ar with 
                    | A.Name n -> name <- Some n
                    | A.Require t -> reqs.Add t
                    | A.Macro (m, p) -> macros.Add (m, p)
                    | _ -> attrArr.Add ar
                | None -> ()
        if parent.IsJavaScript && macros.Count = 0 then
            if not (attrArr.Contains(A.JavaScript)) then attrArr.Add A.JavaScript
        if parent.IsStub then 
            if not (attrArr.Contains(A.Stub)) then attrArr.Add A.Stub
        if parent.OptionalFields then
            if not (attrArr.Contains(A.OptionalField)) then attrArr.Add A.OptionalField
        attrArr |> Seq.distinct |> Seq.toArray, macros.ToArray(), name, List.ofSeq reqs

    member this.GetTypeAnnot (parent: TypeAnnotation, attrs: seq<'A>) =
        let attrArr, macros, name, reqs = this.GetAttrs (parent, attrs)
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
            Macros = macros |> List.ofArray
        }

    member this.GetMemberAnnot (parent: TypeAnnotation, attrs: seq<'A>) =
        let attrArr, macros, name, reqs = this.GetAttrs (parent, attrs)
        let isEp, attrArr =
            if attrArr |> Array.exists ((=) A.SPAEntryPoint) then
                true, attrArr |> Array.filter ((<>) A.SPAEntryPoint)
            else
                false, attrArr
        let kind =
            match attrArr with
            | [||] -> if macros.Length = 0 then None else Some NoFallback
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
                | _ -> failwithf "Incompatible attributes: %+A" attrArr
            match a with
            | A.Inline (Some i) -> Some (Inline i)
            | A.Direct s -> Some (Direct s)
            | A.Constant x -> Some (Constant x)
            | A.Generated (g, p) -> Some (Generated (g, p))
            | A.Stub -> Some Stub
            | A.OptionalField -> Some OptionalField
            // TODO
            | A.DateTimeFormat _ -> None
            | _ -> failwithf "Incompatible attributes: %+A" attrArr // TODO : warning only, location
        {
            Kind = kind
            Macros = List.ofArray macros
            Name = name
            Requires = reqs
            IsEntryPoint = isEp
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
           
type ReflectionAttributeReader() =
    inherit AttributeReader<System.Reflection.CustomAttributeData>()
    override this.GetAssemblyName attr = attr.Constructor.DeclaringType.Assembly.FullName
    override this.GetName attr = attr.Constructor.DeclaringType.Name
    override this.GetCtorArgs attr = attr.ConstructorArguments |> Seq.map (fun a -> a.Value) |> Array.ofSeq
    override this.GetTypeDef o = Reflection.getTypeDefinition (o :?> System.Type) 

let attrReader = ReflectionAttributeReader()