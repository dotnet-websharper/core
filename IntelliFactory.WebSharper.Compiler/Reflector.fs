// $begin{copyright}
// 
// This file is part of WebSharper
// 
// Copyright (c) 2008-2011 IntelliFactory
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

module IntelliFactory.WebSharper.Compiler.Reflector

module A = IntelliFactory.WebSharper.Core.Attributes
module C = IntelliFactory.JavaScript.Core
module M = IntelliFactory.WebSharper.Core.Macros
module P = IntelliFactory.JavaScript.Packager
module Q = IntelliFactory.WebSharper.Core.Quotations
module R = IntelliFactory.WebSharper.Core.Reflection

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
    | Name of Name
    | Proxy of R.TypeDefinition
    | Remote
    | Require of R.TypeDefinition
    | Stub

    override this.ToString() =
        match this with
        | Constant _ -> "Constant"
        | Curry _ -> "Curry"
        | Direct _ -> "Direct"
        | Field _ -> "Field"
        | Inline _ -> "Inline"
        | JavaScript _ -> "JavaScript"
        | Macro _ -> "Macro"
        | Name _ -> "Name"
        | Proxy _ -> "Proxy"
        | Remote _ -> "Remote"
        | Require _ -> "Require"
        | Stub -> "Stub"

type Kind =
    | Class of ClassSlot
    | Enum
    | Exception
    | Interface
    | Module
    | Record of list<Member<Mono.Cecil.PropertyDefinition>>
    | Union of list<UnionCase>

and Member<'T> =
    {
        AddressSlot : AddressSlot
        Annotations : list<Annotation>
        Definition : 'T
        Location : Location
        MemberSlot : MemberSlot
    }

    override this.ToString() =
        this.Definition.ToString()

and UnionCase =
    {
        Name : string
        Member : Member<Mono.Cecil.MethodDefinition>
    }

    override this.ToString() =
        this.Name

type Method = Member<Mono.Cecil.MethodDefinition>
type Case = Member<Mono.Cecil.TypeDefinition>

type Property =
    {
        Member : Member<Mono.Cecil.PropertyDefinition>
        Getter : option<Method>
        Setter : option<Method>
    }

    override this.ToString() =
        this.Member.Definition.ToString()

type Type =
    {
        AddressSlot : AddressSlot
        Annotations : list<Annotation>
        Definition : Mono.Cecil.TypeDefinition
        Kind : Kind
        Location : Location
        Methods : list<Method>
        Nested : list<Type>
        Properties : list<Property>
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

let parseTypeReference (warn: string -> unit) (t: Mono.Cecil.TypeReference) =
    try Adapter.AdaptTypeDefinition t with :? R.InvalidTypeException ->
        warn ("Invalid type reference: " + string t)
        R.TypeDefinition.FromType typeof<unit>

/// Identifies special F# type kinds by examining custom attributes.
let getTypeKind (t: Mono.Cecil.TypeDefinition) =
    if t.IsInterface then Some Interface
    elif t.IsEnum then Some Enum else
        let cma = typeof<CompilationMappingAttribute>.FullName
        let (!) x = Some x
        let attr =
            t.CustomAttributes
            |> Seq.tryPick (fun a ->
                if a.AttributeType.FullName = cma then
                    match unbox a.ConstructorArguments.[0].Value
                          &&& SourceConstructFlags.KindMask with
                    | SourceConstructFlags.Closure -> ! None
                    | SourceConstructFlags.Exception -> ! ! Exception
                    | SourceConstructFlags.Module -> ! ! Module
                    | SourceConstructFlags.RecordType -> ! ! (Record [])
                    | SourceConstructFlags.SumType -> ! ! (Union [])
                    | o -> None
                else
                    None)
        defaultArg attr (Some (Class (ClassSlot())))

/// Reads reflected definition data from an F# assembly.
let getReflectedDefinitions (assembly: Mono.Cecil.AssemblyDefinition) =
    let name = System.Reflection.AssemblyName assembly.FullName
    let data =
        assembly.MainModule.Resources
        |> Seq.tryPick (fun x ->
            if x.Name.ToUpper().StartsWith("REFLECTEDDEFINITIONS") then
                match x.ResourceType with
                | Mono.Cecil.ResourceType.Embedded ->
                    let x = x :?> Mono.Cecil.EmbeddedResource
                    use stream = x.GetResourceStream()
                    let defs = Q.ReadStream name stream
                    let d = Dictionary()
                    defs
                    |> List.iter (fun (k, v) -> d.[k] <- v)
                    Some d
                | _ -> None
            else None)
    match data with
    | None -> Dictionary()
    | Some data -> data

/// Maps Mono.Cecil representations to QR definitions. Requires the TypeKind
/// of the declaring type so as not to compute it repeatedly.
let getQuotationReaderDefinition (k: Kind) (mR: Mono.Cecil.MemberReference) =
    match mR with
    | :? Mono.Cecil.MethodDefinition as mD ->
        if mD.IsConstructor
        then Q.ConstructorDefinition (Adapter.AdaptConstructor mD)
        else Q.MethodDefinition (Adapter.AdaptMethod mD)
        |> Some
    | :? Mono.Cecil.PropertyDefinition as pD ->
        match k with
        | Module -> Some (Q.PropertyDefinition (Adapter.AdaptProperty pD))
        | _ -> None
    | _ -> None

let (|UnionCaseMethod|_|) (m: Mono.Cecil.MethodDefinition) =
    let fn = typeof<CompilationMappingAttribute>.FullName
    if m.IsStatic && m.HasCustomAttributes then
        m.CustomAttributes
        |> Seq.tryPick (fun a ->
            let xs = a.ConstructorArguments
            if a.AttributeType.FullName = fn && xs.Count = 2 then
                match downcast xs.[0].Value, downcast xs.[1].Value with
                | SourceConstructFlags.UnionCase, (x : int) ->
                    let name =
                        if m.Name.StartsWith "New" then m.Name.Substring 3
                        elif m.Name.StartsWith "get_" then m.Name.Substring 4
                        else m.Name
                    Some (name, x)
                | _ -> None
            else None)
    else None

let (|RecordField|_|) (p: Mono.Cecil.PropertyDefinition) =
    let fn = typeof<CompilationMappingAttribute>.FullName
    if p.HasCustomAttributes then
        p.CustomAttributes
        |> Seq.tryPick (fun a ->
            let xs = a.ConstructorArguments
            if a.AttributeType.FullName = fn && xs.Count = 2 then
                match downcast xs.[0].Value, downcast xs.[1].Value with
                | SourceConstructFlags.Field, (x : int) -> Some x
                | _ -> None
            else None)
    else None

[<Sealed>]
type Pool(logger: Logger) =
    let cache = Dictionary()

    static member Create logger =
        Pool logger

    member this.Load(x: R.Type) =
        match cache.TryGetValue x with
        | true, y -> y
        | _ ->
            let fail : M.Macro =
                {
                    Body = None
                    Expand = fun _ _ -> !~ C.Undefined
                    Requirements = []
                }
            let report reason =
                logger.Log {
                    Location = Locator.LocateReflectedType x
                    Priority = Error
                    Text = reason
                }
                cache.[x] <- fail
                fail
            try
                let t = x.Load()
                let def = System.Activator.CreateInstance t
                let md = def :?> M.IMacroDefinition
                let y = md.Macro
                cache.[x] <- y
                y
            with
            | :? System.TypeLoadException as e ->
                report "Failed to load macro definition."
            | :? System.MissingMethodException as e ->
                report "No default constructor."
            | :? System.InvalidCastException as e ->
                report "No IMacroDefinition implementation."

let annotationsTable =
    let d = Dictionary<string,Mono.Cecil.CustomAttribute->_>()
    let add (t: System.Type) value =
        if t.DeclaringType = null then
            d.[t.FullName] <- value
        else
            let ns = t.DeclaringType.FullName
            let key = System.String.Format("{0}/{1}", ns, t.Name)
            d.[key] <- value
    add typeof<A.ConstantAttribute> (fun attr _ ->
        match attr.ConstructorArguments.[0].Value with
        | :? bool as x -> Some (Constant (Bool x))
        | :? int as x -> Some (Constant (Int x))
        | :? float as x -> Some (Constant (Double x))
        | :? string as x -> Some (Constant (String x))
        | _ -> None)
    add typeof<A.DirectAttribute> (fun attr _ ->
        match attr.ConstructorArguments.[0].Value with
        | :? string as x -> Some (Direct x)
        | _ -> None)
    add typeof<A.InlineAttribute> (fun attr _ ->
        match attr.ConstructorArguments.Count with
        | 1 ->
            match attr.ConstructorArguments.[0].Value with
            | :? string as x -> Some (Inline (Some x))
            | _ -> Some (Inline None)
        | _ -> Some (Inline None))
    add typeof<A.MacroAttribute> (fun attr _ ->
        match attr.ConstructorArguments.[0].Value with
        | (:? Mono.Cecil.TypeReference as x) ->
            Some (Macro (Adapter.AdaptType x))
        | _ -> None)
    add typeof<A.NameAttribute> (fun attr warn ->
        match attr.ConstructorArguments.[0].Value with
        | :? string as x ->
            if x.Contains "." then
                let a = Array.toList (x.Split '.')
                let n =
                    (P.Global a.Head, a.Tail)
                    ||> List.fold (fun a b -> P.Local (a, b))
                Some (Name (AbsoluteName n))
            else Some (Name (RelativeName x))
        | :? array<Mono.Cecil.CustomAttributeArgument> as x ->
            let x = x |> Array.map (fun x -> x.Value :?> string)
            match List.ofArray x with
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
        match attr.ConstructorArguments.[0].Value with
        | :? Mono.Cecil.TypeReference as x ->
            let r = parseTypeReference warn x
            Some (Proxy r)
        | :? string as x ->
            let r = R.TypeDefinition.Parse x
            Some (Proxy r)
        | _ -> None)
    add typeof<A.RemoteAttribute> (fun attr _ -> Some Remote)
    add typeof<A.RequireAttribute> (fun attr warn ->
        match attr.ConstructorArguments.[0].Value with
        | :? Mono.Cecil.TypeReference as x ->
            Some (Require (parseTypeReference warn x))
        | _ -> None)
    add typeof<A.StubAttribute> (fun attr _ -> Some Stub)
    add typeof<CompilationMappingAttribute> (fun attr _ ->
        let cA = attr.ConstructorArguments
        if cA.Count = 2 then
            match cA.[0].Value, cA.[1].Value with
            | scf, (:? int as x) ->
                if scf :?> int = int SourceConstructFlags.Field then
                    Some (Field x)
                else None
            | _ -> None
        else None)
    add typeof<CompilationArgumentCountsAttribute> (fun attr _ ->
        match attr.ConstructorArguments.[0].Value with
        | :? array<Mono.Cecil.CustomAttributeArgument> as x ->
            Curry [
                for e in x do
                    match e.Value with
                    | :? int as x -> yield x
                    | _ -> ()
            ]
            |> Some
        | _ -> None)
    d

/// Reflects an assembly.
let Reflect (logger: Logger) (assembly: Mono.Cecil.AssemblyDefinition) =
    let definitions =
        getReflectedDefinitions assembly
    let warn loc text =
        logger.Log { Text = text; Priority = Warning; Location = loc }
    /// Retrieves WebSharper annotations.  Requires the reflected definition
    /// table and the TypeKind of the declaring type, if any.
    let getAnnotations location tD
        (entity: Mono.Cecil.ICustomAttributeProvider) =
        let rD =
            match tD, entity with
            | Some tD, (:? Mono.Cecil.MemberReference as m) ->
                getQuotationReaderDefinition tD m
                |> Option.bind (fun x ->
                    match definitions.TryGetValue x with
                    | true, x -> Some (JavaScript x)
                    | _ -> None)
            | _ -> None
        Option.toList rD @
        (entity.CustomAttributes
        |> Seq.choose (fun attr ->
            match annotationsTable.TryGetValue attr.AttributeType.FullName with
            | true, f -> f attr (warn location)
            | _ -> None)
        |> Seq.toList)
    let reflectMethod inc loc dT (m: Mono.Cecil.MethodDefinition) : option<Member<_>> =
        match getAnnotations loc dT m with
        | [] | [Curry _] when not inc && not m.IsVirtual -> None
        | a ->
            Some {
                AddressSlot = AddressSlot()
                Annotations = a
                Definition = m
                Location = loc
                MemberSlot = MemberSlot()
            }
    let reflectProperty inc loc dT (m: Mono.Cecil.PropertyDefinition) : option<Member<_>> =
        let isVirt (m: Mono.Cecil.MethodDefinition) =
            m <> null && m.IsVirtual
        match getAnnotations loc dT m with
        | [] | [Curry _] when not inc && not (isVirt m.GetMethod || isVirt m.SetMethod) -> None
        | a ->
            Some {
                AddressSlot = AddressSlot()
                Annotations = a
                Definition = m
                Location = loc
                MemberSlot = MemberSlot()
            }
    let withMethodsAndProperties t =
        let k = Some t.Kind
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
                        else reflectMethod inc (Locator.LocateMethod m) k m)
                |> Seq.toList
            Properties =
                t.Definition.Properties
                |> Seq.choose (function
                    | RecordField _ when t.Kind <> Exception ->
                        None
                    | p ->
                        let loc = Locator.LocateProperty p
                        let f = function
                            | null -> None
                            | m -> reflectMethod inc loc k m
                        let self = reflectProperty inc loc k p
                        let getter = f p.GetMethod
                        let setter = f p.SetMethod
                        match self, getter, setter with
                        | None, None, None -> None
                        | _ ->
                            let self =
                                defaultArg self {
                                    AddressSlot = AddressSlot()
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
                            AddressSlot = AddressSlot()
                            Annotations = getAnnotations loc (Some t.Kind) m
                            Definition = m
                            Location = loc
                            MemberSlot = MemberSlot()
                        }
                })
            |> Seq.toList
        { t with Kind = Union cases }
    let withRecordFields (t: Type) =
        let fields =
            t.Definition.Properties
            |> Seq.choose (function
                | RecordField k as p ->
                    let loc = Locator.LocateProperty p
                    let m : Member<_> =
                        {
                            AddressSlot = AddressSlot()
                            Annotations = getAnnotations loc (Some t.Kind) p
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
    and reflectType (t: Mono.Cecil.TypeDefinition) =
        let loc = Locator.LocateType t
        getTypeKind t
        |> Option.bind (fun kind ->
            let notes = getAnnotations loc None t
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
                }
            match kind with
            | Class _ ->
                result
                |> withMethodsAndProperties
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
        Annotations = getAnnotations loc None assembly
        Location = loc
        Types =
            assembly.MainModule.Types
            |> Seq.choose reflectType
            |> Seq.toList
    }

