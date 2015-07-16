// $begin{copyright}
//
// This file is part of WebSharper
//
// Copyright (c) 2008-2014 IntelliFactory
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

module WebSharper.Compiler.Reflector

module A = WebSharper.Core.Attributes
module C = WebSharper.Core.JavaScript.Core
module M = WebSharper.Core.Macros
module P = WebSharper.Core.JavaScript.Packager
module Q = WebSharper.Core.Quotations
module R = WebSharper.Core.Reflection

type Dictionary<'T1,'T2> = System.Collections.Generic.Dictionary<'T1,'T2>

[<Sealed>]
type AddressSlot() =
    let mutable addr : option<P.Address> = None

    member this.Address
        with get () =
            match addr with
            | None -> failwith "Address not set."
            | Some a -> a
        and set x =
            match addr with
            | None -> addr <- Some x
            | Some y ->
                if x = y then () else
                    failwithf "Address already set: %O <- %O" x y

[<Sealed>]
type ClassSlot() =
    let mutable bT : option<P.Address> = None

    member this.BaseType
        with get () = bT
        and set x = bT <- x

[<Sealed>]
type MemberSlot() =
    let mutable expr : option<P.Member> = None
    let addr = AddressSlot()

    member this.Member = expr
    member this.Address = addr

    member this.Field
        with set x =
            match expr with
            | None -> expr <- Some (P.Field x)
            | _ -> failwith "Already set."

    member this.Method
        with set x =
            match expr with
            | None -> expr <- Some (P.Method x)
            | _ -> failwith "Already set."

type Name =
    | RelativeName of string
    | AbsoluteName of P.Address

type Annotation =
    | Constant of Value
    | Curry of list<int>
    | Direct of string
    | Field of int
    | Inline of option<string>
    | JavaScript of Q.Expression
    | Macro of R.Type
    | Generated of R.Type
    | Name of Name
    | Proxy of R.TypeDefinition
    | Remote
    | RemotingProvider of R.TypeDefinition
    | Require of R.TypeDefinition
    | Stub
    | OptionalField

    override this.ToString() =
        match this with
        | Constant _ -> "Constant"
        | Curry _ -> "Curry"
        | Direct _ -> "Direct"
        | Field _ -> "Field"
        | Inline _ -> "Inline"
        | JavaScript _ -> "JavaScript"
        | Macro _ -> "Macro"
        | Generated _ -> "Generated"
        | Name _ -> "Name"
        | Proxy _ -> "Proxy"
        | Remote _ -> "Remote"
        | RemotingProvider _ -> "RemotingProvider"
        | Require _ -> "Require"
        | Stub -> "Stub"
        | OptionalField -> "OptionalField"

type Kind =
    | Class of ClassSlot
    | Enum
    | Exception
    | Interface
    | Module
    | Record of list<Member<PropertyDefinition>>
    | Union of list<UnionCase>

    override this.ToString() =
        match this with
        | Class _ -> "Class"
        | Enum -> "Enum"
        | Exception -> "Exception"
        | Interface -> "Interface"
        | Module -> "Module"
        | Record _ -> "Record"
        | Union _ -> "Union"

and Member<'T> =
    {
        Annotations : list<Annotation>
        Definition : 'T
        Location : Location
        MemberSlot : MemberSlot
    }

    member m.AddressSlot =
        m.MemberSlot.Address

    override this.ToString() =
        this.Definition.ToString()

and UnionCase =
    {
        Name : string
        Member : Member<MethodDefinition>
    }

    override this.ToString() =
        this.Name

type Method = Member<MethodDefinition>
type Case = Member<TypeDefinition>
type Field = Member<FieldDefinition>

type Property =
    {
        Member : Member<PropertyDefinition>
        Getter : option<Method>
        Setter : option<Method>
    }

    override this.ToString() =
        this.Member.Definition.ToString()

type Type =
    {
        AddressSlot : AddressSlot
        Annotations : list<Annotation>
        Definition : TypeDefinition
        Kind : Kind
        Location : Location
        Methods : list<Method>
        Nested : list<Type>
        Properties : list<Property>
        Fields : list<Field>
    }

    override this.ToString() =
        this.Definition.Name

type Assembly =
    {
        Name : R.AssemblyName
        Annotations : list<Annotation>
        Location : Location
        Types : list<Type>
    }

    override this.ToString() =
        this.Name.Name

type Definitions = Dictionary<Q.Definition,Q.Expression>

let parseTypeReference (warn: string -> unit) (t: TypeReference) =
    try Adapter.AdaptTypeDefinition t with :? R.InvalidType ->
        warn ("Invalid type reference: " + string t)
        R.TypeDefinition.FromType typeof<unit>

/// Identifies special F# type kinds by examining custom attributes.
let getTypeKind (t: TypeDefinition) =
    if t.Name.Contains("@") then None // hack for closure types
    elif t.IsInterface then Some Interface
    elif t.IsEnum then Some Enum else
        let cma = typeof<CompilationMappingAttribute>.FullName
        let (!) x = Some x
        let attr =
            t.CustomAttributes
            |> Seq.tryPick (fun a ->
                if a.AttributeType.FullName = cma then
                    match a.ConstructorArguments with
                    | IntArgument x :: _ ->
                        match enum x &&& SourceConstructFlags.KindMask with
                        | SourceConstructFlags.Closure -> ! None
                        | SourceConstructFlags.Exception -> ! ! Exception
                        | SourceConstructFlags.Module -> ! ! Module
                        | SourceConstructFlags.RecordType -> ! ! (Record [])
                        | SourceConstructFlags.SumType -> ! ! (Union [])
                        | o -> None
                    | _ -> None
                else None)
        defaultArg attr (Some (Class (ClassSlot())))

/// Reads reflected definition data from an F# assembly.
let getReflectedDefinitions (assembly: AssemblyDefinition) =
    let name = assembly.Name
    let resOpt =
        assembly.EmbeddedResources
        |> Seq.tryPick (fun (KeyValue (n, getStream)) -> 
            if n.ToUpper().StartsWith("REFLECTEDDEFINITIONS") then Some (n, getStream) else None)
    match resOpt with
    | Some (resName, getStream) ->
        let typeDefs =
            assembly.CustomAttributes |>
            Seq.tryPick (fun a -> 
                if a.AttributeType.FullName = "Microsoft.FSharp.Core.CompilationMappingAttribute" then 
                    match a.ConstructorArguments with
                    | [ StringArgument n; TypesArgument ts ] when n = resName -> 
                        Some (ts |> Array.map (parseTypeReference (failwithf "Error loading F# 4.0+ type reference table: %s")))
                    | _ -> None
                else None
            ) 
            |> function None -> [| |] | Some d -> d
        use stream = getStream ()
        let defs = Q.ReadStream name typeDefs stream
        let d = Dictionary()
        defs
        |> List.iter (fun (k, v) -> d.[k] <- v)
        d
    | None -> Dictionary()

/// Maps Mono.Cecil method representations to QR definitions.
let getMethodQuotationReaderDefinition (mD: MethodDefinition) =
    if mD.IsConstructor
    then Q.ConstructorDefinition (Adapter.AdaptConstructor mD)
    else Q.MethodDefinition (Adapter.AdaptMethod mD)

/// Maps Mono.Cecil property representations to QR definitions. Requires the TypeKind
/// of the declaring type so as not to compute it repeatedly.
let getPropertyQuotationReaderDefinition (k: Kind) (pD: PropertyDefinition) =
    match k with
    | Module -> Some (Q.PropertyDefinition (Adapter.AdaptProperty pD))
    | _ -> None

type AnnotationTarget =
    | AnnotatedProperty of Kind * PropertyDefinition
    | AnnotatedMethod of MethodDefinition
    | AnnotatedMember of list<CustomAttribute>

    member this.CustomAttributes =
        match this with
        | AnnotatedProperty (_, p) -> p.CustomAttributes
        | AnnotatedMethod m -> m.CustomAttributes
        | AnnotatedMember xs -> xs

    member this.RawQuotation =
        match this with
        | AnnotatedProperty (_, p) -> p.RawQuotation
        | AnnotatedMethod m -> m.RawQuotation
        | AnnotatedMember _ -> None

let getQuotationReaderDefinition (aT: AnnotationTarget) =
    match aT with
    | AnnotatedProperty (k, p) -> getPropertyQuotationReaderDefinition k p
    | AnnotatedMethod m -> Some (getMethodQuotationReaderDefinition m)
    | _ -> None

let (|UnionCaseMethod|_|) (m: MethodDefinition) =
    let fn = typeof<CompilationMappingAttribute>.FullName
    if m.IsStatic && not m.CustomAttributes.IsEmpty then
        m.CustomAttributes
        |> Seq.tryPick (fun a ->
            if a.AttributeType.FullName = fn then
                match a.ConstructorArguments with
                | [IntArgument uc; IntArgument x]
                    when enum uc = SourceConstructFlags.UnionCase ->
                        let name =
                            if m.Name.StartsWith "New" then m.Name.Substring 3
                            elif m.Name.StartsWith "get_" then m.Name.Substring 4
                            else m.Name
                        Some (name, x)
                | _ -> None
            else None)
    else None

let (|RecordField|_|) (p: PropertyDefinition) =
    let fn = typeof<CompilationMappingAttribute>.FullName
    if not p.CustomAttributes.IsEmpty then
        p.CustomAttributes
        |> Seq.tryPick (fun a ->
            if a.AttributeType.FullName = fn then
                match a.ConstructorArguments with
                | [IntArgument fld; IntArgument x]
                    when enum fld = SourceConstructFlags.Field ->
                        Some x
                | _ -> None
            else None)
    else None

[<Sealed>]
type Pool(logger: Logger) =
    let cache = Dictionary()

    static member Create logger =
        Pool logger

    member this.LoadMacro(x: R.Type) =
        match cache.TryGetValue x with
        | true, y -> y
        | _ ->
            let report reason =
                logger.Log {
                    Location = Locator.LocateReflectedType x
                    Priority = Error
                    Text = reason
                }
                let fail = { new M.IMacro with member this.Translate(_, _) = !~ C.Undefined }
                cache.[x] <- fail
                fail
            try
                let t = x.Load()
                let def = System.Activator.CreateInstance t
                let md = def :?> M.IMacro
                cache.[x] <- md
                md
            with
            | :? System.TypeLoadException as e ->
                report (sprintf "Failed to load macro definition: %A" e)
            | :? System.MissingMethodException as e ->
                report "No default constructor."
            | :? System.InvalidCastException as e ->
                report "No IMacro implementation."

    member this.LoadGenerator(x: R.Type) =
        let report reason =
            logger.Log {
                Location = Locator.LocateReflectedType x
                Priority = Error
                Text = reason
            }
            { new M.IGenerator with member this.Body = M.CoreBody (!~ C.Undefined) }
        try
            let t = x.Load()
            let def = System.Activator.CreateInstance t
            let md = def :?> M.IGenerator
            md
        with
        | :? System.TypeLoadException as e ->
            report (sprintf "Failed to load generator definition: %A" e)
        | :? System.MissingMethodException as e ->
            report "No default constructor."
        | :? System.InvalidCastException as e ->
            report "No IGenerator implementation."

let annotationsTable =
    let d = Dictionary<string,CustomAttribute->_>()
    let add (t: System.Type) value =
        if t.DeclaringType = null then
            d.[t.FullName] <- value
        else
            let ns = t.DeclaringType.FullName
            let key = System.String.Format("{0}/{1}", ns, t.Name)
            d.[key] <- value
    add typeof<A.ConstantAttribute> (fun attr _ ->
        match attr.ConstructorArguments with
        | [BoolArgument x] -> Some (Constant (Bool x))
        | [IntArgument x] -> Some (Constant (Int x))
        | [FloatArgument x] -> Some (Constant (Double x))
        | [StringArgument x] -> Some (Constant (String x))
        | _ -> None)
    add typeof<A.DirectAttribute> (fun attr _ ->
        match attr.ConstructorArguments with
        | [StringArgument x] -> Some (Direct x)
        | _ -> None)
    add typeof<A.InlineAttribute> (fun attr _ ->
        match attr.ConstructorArguments with
        | [StringArgument x] -> Some (Inline (Some x))
        | _ -> Some (Inline None))
    add typeof<A.MacroAttribute> (fun attr _ ->
        match attr.ConstructorArguments with
        | [TypeArgument x] -> Some (Macro (Adapter.AdaptType x))
        | _ -> None)
    add typeof<A.GeneratedAttribute> (fun attr _ ->
        match attr.ConstructorArguments with
        | [TypeArgument x] -> Some (Generated (Adapter.AdaptType x))
        | _ -> None)
    add typeof<A.NameAttribute> (fun attr warn ->
        match attr.ConstructorArguments with
        | [StringArgument x] ->
            if x.Contains "." then
                let a = Array.toList (x.Split '.')
                let n =
                    (P.Global a.Head, a.Tail)
                    ||> List.fold (fun a b -> P.Local (a, b))
                Some (Name (AbsoluteName n))
            else Some (Name (RelativeName x))
        | [StringsArgument x] ->
            match x with
            | x :: xs ->
                let n =
                    (P.Global x, xs)
                    ||> List.fold (fun a b -> P.Local (a, b))
                Some (Name (AbsoluteName n))
            | _ ->
                warn "Invalid empty name annotation."
                None
        | args -> None)
    add typeof<A.ProxyAttribute> (fun attr warn ->
        match attr.ConstructorArguments with
        | [TypeArgument x] ->
            let r = parseTypeReference warn x
            Some (Proxy r)
        | [StringArgument x] ->
            let r = R.TypeDefinition.Parse x
            Some (Proxy r)
        | _ -> None)
    add typeof<A.RemoteAttribute> (fun attr _ -> Some Remote)
    add typeof<A.RemotingProviderAttribute> (fun attr warn ->
        match attr.ConstructorArguments with
        | [TypeArgument x] ->
            Some (RemotingProvider (parseTypeReference warn x))
        | _ -> None)
    add typeof<A.RequireAttribute> (fun attr warn ->
        match attr.ConstructorArguments with
        | [TypeArgument x] ->
            Some (Require (parseTypeReference warn x))
        | _ -> None)
    add typeof<A.StubAttribute> (fun attr _ -> Some Stub)
    add typeof<A.OptionalFieldAttribute> (fun attr _ -> Some OptionalField)
    add typeof<CompilationMappingAttribute> (fun attr _ ->
        match attr.ConstructorArguments with
        | [IntArgument scf; IntArgument x] when enum scf = SourceConstructFlags.Field -> Some (Field x)
        | _ -> None)
    add typeof<CompilationArgumentCountsAttribute> (fun attr _ ->
        match attr.ConstructorArguments with
        | [IntsArgument xs] -> Some (Curry xs)
        | _ -> None)
    d

/// Reflects an assembly.
let Reflect (logger: Logger) (assembly: AssemblyDefinition) =
    let definitions =
        getReflectedDefinitions assembly
    let warn loc text =
        logger.Log { Text = text; Priority = Warning; Location = loc }
    /// Retrieves WebSharper annotations.  Requires the reflected definition
    /// table and the TypeKind of the declaring type, if any.
    let getAnnotations location (tgt: AnnotationTarget) =
        let rD =
            match tgt.RawQuotation with
            | None ->
                getQuotationReaderDefinition tgt
                |> Option.bind (fun x ->
                    match definitions.TryGetValue x with
                    | true, x -> Some (JavaScript x)
                    | _ -> None)
            | Some q -> Some (JavaScript q)
        Option.toList rD @
        (tgt.CustomAttributes
        |> List.choose (fun attr ->
            match annotationsTable.TryGetValue (attr.AttributeType.FullName.Replace('+', '/')) with
            | true, f -> f attr (warn location)
            | _ -> None))
    let reflectMethod inc loc (m: MethodDefinition) : option<Member<_>> =
        match getAnnotations loc (AnnotatedMethod m) with
        | [] | [Curry _] when not inc && not m.IsVirtual -> None
        | a ->
            Some {
                Annotations = a
                Definition = m
                Location = loc
                MemberSlot = MemberSlot()
            }
    let reflectProperty inc loc dT (m: PropertyDefinition) : option<Member<_>> =
        let isVirt (m: option<MethodDefinition>) =
            m.IsSome && m.Value.IsVirtual
        match getAnnotations loc (AnnotatedProperty (dT, m)) with
        | [] | [Curry _] when not inc && not (isVirt m.GetMethod || isVirt m.SetMethod) -> None
        | a ->
            Some {
                Annotations = a
                Definition = m
                Location = loc
                MemberSlot = MemberSlot()
            }
    let withMethodsAndProperties t =
        let k = t.Kind
        let inc =
            match t.Kind with
            | Interface | Exception -> true
            | _ ->
                t.Annotations
                |> Seq.exists (function
                    | Stub -> true
                    | _ -> false)
        { t with
            Methods =
                t.Definition.Methods
                |> Seq.choose (function
                    | UnionCaseMethod _ -> None
                    | m ->
                        if (m.IsSetter || m.IsGetter)
                           && not t.Definition.IsInterface
                        then None
                        else reflectMethod inc (Locator.LocateMethod m) m)
                |> Seq.toList
            Properties =
                t.Definition.Properties
                |> Seq.choose (function
                    | RecordField _ when t.Kind <> Exception ->
                        None
                    | p ->
                        let loc = Locator.LocateProperty p
                        let f = function
                            | None -> None
                            | Some m -> reflectMethod inc loc m
                        let self = reflectProperty inc loc k p
                        let getter = f p.GetMethod
                        let setter = f p.SetMethod
                        match self, getter, setter with
                        | None, None, None -> None
                        | _ ->
                            let self =
                                defaultArg self {
                                    Annotations = []
                                    Location = loc
                                    Definition = p
                                    MemberSlot = MemberSlot()
                                }
                            Some {
                                Member = self
                                Getter = getter
                                Setter = setter
                            })
                |> Seq.toList
        }
    let withFields (t: Type) = 
        let hasOptF a = 
            a |> Seq.exists (function OptionalField -> true | _ -> false)
        let optFields = hasOptF t.Annotations
        { t with
            Fields = 
                t.Definition.Fields
                |> Seq.map (fun f ->
                    let loc = Locator.LocateField f
                    let origAnnots = getAnnotations loc (AnnotatedMember f.CustomAttributes)
                    let annots = 
                        if optFields && not (hasOptF origAnnots) then
                            OptionalField :: origAnnots
                        else origAnnots 
                    {
                        Annotations = annots
                        Definition = f
                        Location = loc
                        MemberSlot = MemberSlot()
                    } 
                )
                |> Seq.toList
        }
    let yieldType (t: Type) =
        match t.Kind with
        | Kind.Record _ -> Some t
        | Kind.Union _ -> Some t
        | _ ->
            if t.Annotations.IsEmpty
               && t.Methods.IsEmpty
               && t.Nested.IsEmpty
               && t.Properties.IsEmpty
            then None
            else Some t
    let withCases loc (t: Type) =
        let cases =
            t.Definition.Methods
            |> Seq.choose (function
                | UnionCaseMethod (n, i) as m -> Some ((m, n), i)
                | _ -> None)
            |> Seq.sortBy snd
            |> Seq.map (fun ((m, n), _) ->
                {
                    Name = n
                    Member =
                        {
                            Annotations = getAnnotations loc (AnnotatedMethod m)
                            Definition = m
                            Location = loc
                            MemberSlot = MemberSlot()
                        }
                })
            |> Seq.toList
        { t with Kind = Union cases }
    let withRecordFields (t: Type) =
        let hasOptF a = 
            a |> Seq.exists (function OptionalField -> true | _ -> false)
        let optFields = hasOptF t.Annotations
        let fields =
            t.Definition.Properties
            |> Seq.choose (function
                | RecordField k as p ->
                    let loc = Locator.LocateProperty p
                    let origAnnots = getAnnotations loc (AnnotatedProperty (t.Kind, p))
                    let annots = 
                        if optFields && not (hasOptF origAnnots) then
                            OptionalField :: origAnnots
                        else origAnnots 
                    let m : Member<_> =
                        {
                            Annotations = annots
                            Definition = p
                            Location = loc
                            MemberSlot = MemberSlot()
                        }
                    Some (m, k)
                | _ -> None)
            |> Seq.sortBy snd
            |> Seq.map fst
            |> Seq.toList
        { t with Kind = Record fields }
    let rec withNested (t: Type) =
        let n =
            t.Definition.NestedTypes
            |> Seq.choose reflectType
            |> Seq.toList
        { t with Nested = n }
    and reflectType (t: TypeDefinition) =
        let loc = Locator.LocateType t
        getTypeKind t
        |> Option.bind (fun kind ->
            let notes = getAnnotations loc (AnnotatedMember t.CustomAttributes)
            let result =
                {
                    AddressSlot = AddressSlot()
                    Annotations = notes
                    Definition = t
                    Kind = kind
                    Location = loc
                    Methods = []
                    Nested = []
                    Properties = []
                    Fields = []
                }
            match kind with
            | Class _ ->
                result
                |> withMethodsAndProperties
                |> withFields
                |> withNested
                |> yieldType
            | Enum ->
                yieldType result
            | Exception ->
                result
                |> withMethodsAndProperties
                |> yieldType
            | Interface ->
                result
                |> withMethodsAndProperties
                |> yieldType
            | Module ->
                result
                |> withMethodsAndProperties
                |> withNested
                |> yieldType
            | Record _ ->
                result
                |> withMethodsAndProperties
                |> withRecordFields
                |> yieldType
            | Union _ ->
                result
                |> withMethodsAndProperties
                |> withCases result.Location
                |> yieldType)
    let loc = Locator.LocateAssembly assembly
    {
        Name = R.AssemblyName.Parse assembly.Name.FullName
        Annotations = getAnnotations loc (AnnotatedMember assembly.CustomAttributes)
        Location = loc
        Types =
            assembly.Types
            |> Seq.choose reflectType
            |> Seq.toList
    }
