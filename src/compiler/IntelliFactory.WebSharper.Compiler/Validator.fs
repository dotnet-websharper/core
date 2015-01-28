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

module IntelliFactory.WebSharper.Compiler.Validator

module C = IntelliFactory.JavaScript.Core
module I = IntelliFactory.WebSharper.Compiler.Inlining
module M = IntelliFactory.WebSharper.Core.Macros
module Me = IntelliFactory.WebSharper.Core.Metadata
module R = IntelliFactory.WebSharper.Core.Reflection
module Re = IntelliFactory.WebSharper.Compiler.Reflector
module Res = IntelliFactory.WebSharper.Core.Resources
module P = IntelliFactory.JavaScript.Packager
module Q = IntelliFactory.WebSharper.Core.Quotations

type RecordField = string
type Requirement = R.TypeDefinition

type Status =
    | Compiled
    | Ignored

    override this.ToString() =
        match this with
        | Compiled -> "Compiled"
        | Ignored -> "Ignored"

type Name = P.Address

type ConstructorKind =
    | InlineConstructor of I.Inline
    | JavaScriptConstructor of Q.Expression
    | MacroConstructor of R.Type * M.Macro
    | StubConstructor of Name

type Constructor =
    {
        Currying : list<int>
        Kind : ConstructorKind
        Location : Location
        Name : Name
        Reference : R.Constructor
        Requirements : list<Requirement>
        Slot : Re.MemberSlot
    }

type RemotingKind =
    | RemoteAsync
    | RemoteSend
    | RemoteSync

type MethodKind =
    | InlineMethod of I.Inline
    | JavaScriptMethod of Q.Expression
    | MacroMethod of R.Type * M.Macro
    | RemoteMethod of RemotingKind * ref<option<Me.MethodHandle>>
    | StubMethod

type Method =
    {
        Currying : list<int>
        Definition : MethodDefinition
        Kind : MethodKind
        Location : Location
        Name : Name
        Reference : R.Method
        Requirements : list<Requirement>
        Scope : MemberScope
        Slot : Re.MemberSlot
    }

type PropertyKind =
    | BasicProperty of option<Method> * option<Method>
    | FieldProperty of int
    | InlineModuleProperty of I.Inline
    | InterfaceProperty
    | JavaScriptModuleProperty of Q.Expression
    | StubProperty

type RecordProperty =
    {
        JavaScriptName : string
        OriginalName : string
        PropertyType : TypeReference
    }

type Property =
    {
        Kind : PropertyKind
        Location : Location
        Name : Name
        PropertyType : TypeReference
        Reference : R.Property
        Scope : MemberScope
        Slot : Re.MemberSlot
    }

and TypeKind =
    | Class of ClassKind //Re.ClassSlot * option<R.Type> * list<Constructor> * list<Type> * list<string * string>
    | Exception
    | Interface
    | Module of list<Type>
    | Record of list<RecordProperty>
    | Resource
    | Union of list<UnionCase>

    member this.Nested =
        match this with
        | Class { Nested = n } | Module n -> n
        | _ -> []

and ClassKind =
    {
        Slot : Re.ClassSlot
        BaseClass : option<R.Type>
        Constructors : list<Constructor>
        Nested : list<Type>
        Fields : list<string>
        FieldRenames : list<string * string>   
    }

and Type =
    {
        Kind : TypeKind
        Location : Location
        Methods : list<Method>
        Name : Name
        Properties : list<Property>
        Proxy : option<R.TypeDefinition>
        Reference : R.TypeDefinition
        ReflectorType : Re.Type
        Requirements : list<Requirement>
        Status : Status
    }

    member this.Nested = this.Kind.Nested

and UnionCase =
    {
        Kind : UnionCaseKind
        Location : Location
        Reference : R.UnionCase
        Requirements : list<Requirement>
    }

and UnionCaseKind =
    | BasicUnionCase
    | ConstantUnionCase of Value

type Assembly =
    {
        Name : R.AssemblyName
        Location : Location
        Mode : Me.AssemblyMode
        RemotingProvider : option<R.TypeDefinition>
        Requirements : list<Requirement>
        Types : list<Type>
    }

type HashSet<'T> = System.Collections.Generic.HashSet<'T>

type Pool = I.Pool

let getCtorStatus this =
    match this with
    | InlineConstructor i when not i.IsTransformer -> Compiled
    | JavaScriptConstructor _ -> Compiled
    | MacroConstructor (_, m) ->
        match m.Body with
        | Some _ -> Compiled
        | _ -> Ignored
    | _ -> Ignored

let isCompiledConstructor ({Kind=ck}: Constructor) =
    match getCtorStatus ck with
    | Compiled -> true
    | Ignored -> false

let getMethodStatus this =
    match this with
    | InlineMethod i ->
        if i.IsTransformer then Ignored else Compiled
    | JavaScriptMethod _ -> Compiled
    | MacroMethod (_, m) ->
        match m.Body with
        | Some _ -> Compiled
        | _ -> Ignored
    | RemoteMethod _ -> Ignored
    | StubMethod -> Ignored

let isCompiledMethod ({Kind = mk}: Method) =
    match getMethodStatus mk with
    | Compiled -> true
    | _ -> false

let getPropStatus this =
    match this with
    | InlineModuleProperty i ->
        if i.IsTransformer then Ignored else Compiled
    | JavaScriptModuleProperty _ -> Compiled
    | BasicProperty (Some m, _) -> getMethodStatus m.Kind
    | BasicProperty (_, Some m) -> getMethodStatus m.Kind
    | _ -> Ignored

let isCompiledProperty ({Kind = pk}: Property) =
    match getPropStatus pk with
    | Compiled -> true
    | _ -> false

let isCompiledType (t: Type) =
    match t.Status with
    | Compiled -> true
    | _ -> false

let isStub (a: list<Re.Annotation>) =
    a |> List.exists (function Re.Stub -> true | _ -> false)

let ( % ) format arg = System.String.Format(format, [| arg |])

let getProxy annot =
    List.tryPick (function Re.Proxy cR -> Some cR | _ -> None) annot

let log priority (logger: Logger) loc text =
    logger.Log {
        Priority = priority
        Location = loc
        Text = text
    }

let error = log Error
let warn = log Warning

type Annotation =
    | Constant of Value
    | Direct of string
    | Field of int
    | Inline of string
    | InlineJavaScript of Q.Expression
    | Macro of R.Type
    | JavaScript of Q.Expression
    | Remote
    | Stub

let fullName (t: System.Type) =
    System.String.Format("{0}/{1}",
        t.DeclaringType.FullName,
        t.Name)

let iResourceFullName = fullName typeof<Res.IResource>
let baseResourceFullName = fullName typeof<Res.BaseResource>

let isIResource (t: TypeDefinition) =
    t.Interfaces
    |> Seq.exists (fun x -> x.FullName = iResourceFullName)

let rec isResource (t: TypeDefinition) =
    isIResource t ||
    match t.BaseType with
    | None -> false
    | Some bT ->
        bT.FullName = baseResourceFullName
        || isResource (bT.Resolve())

let isWebControl (t: TypeDefinition) =
    match t.BaseType with
    | Some bT when bT.FullName = "IntelliFactory.WebSharper.Web.Control" -> true
    | _ -> false

let verifyWebControl logger (verifier: Verifier.State) (t: Re.Type) =
    match verifier.VerifyWebControl t.Definition with
    | Verifier.Correct -> ()
    | Verifier.Incorrect msg ->
        error logger t.Location msg

let Validate (logger: Logger) (pool: I.Pool) (macros: Re.Pool) (fields: R.TypeDefinition -> list<string>)
    (assembly: Re.Assembly) : Assembly =

    let verifier = Verifier.Create(logger)
    let warn = warn logger
    let error = error logger

    let getRequirements loc scope annotations =
        let annot =
            annotations
            |> List.choose (function
                | Re.Require tR -> Some tR
                | _ -> None)
        match scope, annot with
        | Instance, _ :: _ ->
            warn loc "Require annotations are invalid \
                on instance members."
            []
        | _ -> annot

    let getMainAnnotation loc annotations =
        let parse x =
            match x with
            | Re.Constant x -> Some (Constant x)
            | Re.Direct x -> Some (Direct x)
            | Re.Field x -> Some (Field x)
            | Re.Inline (Some x) -> Some (Inline x)
            | Re.Inline None -> Some (Inline null)
            | Re.JavaScript x -> Some (JavaScript x)
            | Re.Macro t -> Some (Macro t)
            | Re.Remote -> Some Remote
            | Re.Stub -> Some Stub
            | _ -> None
        match List.choose parse annotations with
        | [] -> None
        | [a] -> Some a
        | [Inline null; JavaScript x]
        | [JavaScript x; Inline null] ->
            Some (InlineJavaScript x)
        | [a; JavaScript _]
        | [JavaScript _; a] ->
            Some a
        | a :: _ ->
            warn loc "Ignoring incompatible attributes."
            Some a

    let pCtor pStub (dT: Re.Type) (iP: Pool) (t: Re.Member<_>)
        : option<Constructor> =
        let self = t.Definition : MethodDefinition
        let reqs = getRequirements t.Location Static t.Annotations
        let pars = [for p in self.Parameters -> p.Name]
        let curr =
            let curr =
                t.Annotations
                |> List.tryPick (function
                    | Re.Curry x -> Some x
                    | _ -> None)
            match curr with
            | None ->
                let k = self.Parameters.Length
                if k > 1 then [k] else []
            | Some x -> x
        let annot =
            match getMainAnnotation t.Location t.Annotations with
            | Some (Direct pat) ->
                iP.CreateInline Static t.Location pars (I.Compiled pat)
                |> InlineConstructor
                |> Some
            | Some (Inline pat) ->
                iP.CreateInline Static t.Location pars (I.Inlined pat)
                |> InlineConstructor
                |> Some
            | Some (JavaScript js) ->
                Some (JavaScriptConstructor js)
            | Some (Macro d) ->
                let m = macros.Load d
                Some (MacroConstructor (d, m))
            | Some (InlineJavaScript js) ->
                warn t.Location
                    "Inline JavaScript constructors are not supported."
                None
            | Some Stub ->
                Some (StubConstructor dT.AddressSlot.Address)
            | Some (Constant _)
            | Some (Field _)
            | Some (Macro _)
            | Some (Remote _)
            | None ->
                if pStub
                then Some (StubConstructor dT.AddressSlot.Address)
                else None
        annot
        |> Option.map (fun kind ->
            {
                Currying = curr
                Name = t.AddressSlot.Address
                Kind = kind
                Location = t.Location
                Reference = Adapter.AdaptConstructor t.Definition
                Requirements = reqs
                Slot = t.MemberSlot
            })

    let pMethodCurrying (t: Re.Member<_>) =
        let self = t.Definition : MethodDefinition
        let curr =
            t.Annotations
            |> List.tryPick (function
                | Re.Curry x -> Some x
                | _ -> None)
        match curr with
        | None ->
            let k = self.Parameters.Length
            if self.IsStatic
                then if k > 1 then [k] else []
                else if k = 0 then [1; 1] else [1; k]
        | Some x ->
            if self.IsStatic
                then x
                else 1 :: x

    let pMethodKind pStub (iP: Pool) (t: Re.Member<_>) =
        let self = t.Definition : MethodDefinition
        let scope = if self.IsStatic then Static else Instance
        let pars = [for p in self.Parameters -> p.Name]
        let loc = t.Location
        let curr = pMethodCurrying t
        let annot =
            match getMainAnnotation t.Location t.Annotations with
            | Some (Direct pat) ->
                iP.CreateInline scope loc pars (I.Compiled pat)
                |> InlineMethod
                |> Some
            | Some (Inline pat) ->
                iP.CreateInline scope loc pars (I.Inlined pat)
                |> InlineMethod
                |> Some
            | Some (InlineJavaScript js) ->
                iP.CreateInline scope loc pars
                    (I.Quoted (Corrector.Method (curr, scope), js))
                |> InlineMethod
                |> Some
            | Some (JavaScript js) -> Some (JavaScriptMethod js)
            | Some (Macro d) ->
                let m = macros.Load d
                Some (MacroMethod (d, m))
            | Some Remote ->
                let (|Void|_|) (t: TypeReference) =
                    if  t.FullName = "Sysem.Void"
                        || t.FullName = "Microsoft.FSharp.Core.Unit"
                    then Some () else None
                let (|Async|_|) (t: TypeReference) =
                    match t.Shape with
                    | TypeShape.GenericInstanceType [x] when
                        t.Namespace = "Microsoft.FSharp.Control"
                        && t.Name = "FSharpAsync`1" ->
                        Some x
                    | _ -> None
                match t.Definition.ReturnType with
                | None | Some Void -> RemoteMethod (RemoteSend, ref None)
                | Some (Async _) -> RemoteMethod (RemoteAsync, ref None)
                | Some _ -> RemoteMethod (RemoteSync, ref None)
                |> Some
            | Some Stub -> Some StubMethod
            | Some (Constant _)
            | Some (Field _)
            | None ->
                if pStub || self.IsVirtual then Some StubMethod else None
        match annot with
        | Some (RemoteMethod _) ->
            match verifier.VerifyRemoteMethod t.Definition with
            | Verifier.Correct -> ()
            | Verifier.Incorrect msg -> warn t.Location msg
        | _ -> ()
        annot

    let fixStubName (n: Name) (name: string) : Name =
        match n with
        | P.Global x -> P.Global name
        | P.Local (a, _) -> P.Local (a, name)

    let pMethodFromKind (iP: Pool) (t: Re.Member<_>) kind =
        let self = t.Definition : MethodDefinition
        let scope = if self.IsStatic then Static else Instance
        let reqs = getRequirements t.Location scope t.Annotations
        let reference = Adapter.AdaptMethod self
        let loc = t.Location
        let curr = pMethodCurrying t
        kind
        |> Option.map (fun kind ->
            let a = t.Annotations
            {
                Currying = curr
                Definition = self
                Name =
                    match kind with
                    | MethodKind.StubMethod ->
                        let localName =
                            t.Annotations
                            |> List.tryPick (function
                                | Reflector.Annotation.Name name ->
                                    match name with
                                    | Reflector.RelativeName name -> Some name
                                    | Reflector.AbsoluteName addr -> Some addr.LocalName
                                | _ -> None)
                        let name = defaultArg localName t.Definition.Name
                        fixStubName t.AddressSlot.Address name
                    | _ -> t.AddressSlot.Address
                Kind = kind
                Location = loc
                Reference = reference
                Requirements = reqs
                Scope = scope
                Slot = t.MemberSlot
            })

    let pMethod pStub (iP: Pool) t =
        let kind = pMethodKind pStub iP t
        pMethodFromKind iP t kind

    let pPropKind pStub (iP: Pool) (p: Re.Property) =
        let prop = p.Member
        let self = prop.Definition
        let annot = getMainAnnotation prop.Location prop.Annotations
        let loc = p.Member.Location
        match annot with
        | Some (Constant _) ->
            warn loc "Constant attribute is not allowed on properties."
        | _ -> ()
        let isStub = match annot with Some Stub -> true | _ -> false
        let pM = pMethod false iP
        if isStub then
            match Option.bind pM p.Getter, Option.bind pM p.Setter with
            | None, None -> ()
            | _ ->
                warn prop.Location "Stub properties cannot have explicit \
                    getters and setters."
            Some StubProperty
        else
            let pM = pMethod pStub iP
            match annot with
            | Some (Field j) -> Some (FieldProperty j)
            | _ ->
                match Option.bind pM p.Getter, Option.bind pM p.Setter with
                | None, None -> None
                | g, s -> Some (BasicProperty (g, s))

    let pPropFromKind iP (p: Re.Property) kind : option<Property> =
        let prop = p.Member
        let self = prop.Definition
        let scope = if self.IsStatic then Static else Instance
        kind
        |> Option.map (fun kind ->
            {
                Name = p.Member.AddressSlot.Address
                Kind = kind
                Location = p.Member.Location
                PropertyType = p.Member.Definition.PropertyType
                Reference = Adapter.AdaptProperty prop.Definition
                Scope = scope
                Slot = p.Member.MemberSlot
            })

    let pProp pStub (iP: Pool) (p: Re.Property) : option<Property> =
        pPropFromKind iP p (pPropKind pStub iP p)

    let pFieldRename (f: Re.Member<FieldDefinition>) =
        let fn = f.Definition.Name
        let name = 
            f.Annotations |> Seq.tryPick (function
               | Re.Name (Re.RelativeName n) -> Some n 
               | _ -> None)
        match name with
        | Some n -> fn, Some n
        | _ -> fn, None

    let pField (iP: Pool) (p: Re.Property) : option<Property> =
        let prop = p.Member
        let self = p.Member.Definition : PropertyDefinition
        let reqs = getRequirements prop.Location Static prop.Annotations
        let loc = prop.Location
        let annot =
            match getMainAnnotation prop.Location prop.Annotations with
            | Some (Direct pat) ->
                let i = iP.CreateInline Static loc [] (I.Compiled pat)
                Some (InlineModuleProperty i)
            | Some (Inline pat) ->
                let i = iP.CreateInline Static loc [] (I.Inlined pat)
                Some (InlineModuleProperty i)
            | Some (InlineJavaScript js) ->
                let i =
                    iP.CreateInline Static loc []
                        (I.Quoted (Corrector.Field, js))
                Some (InlineModuleProperty i)
            | Some (JavaScript js) ->
                Some (JavaScriptModuleProperty js)
            | Some Stub ->
                Some StubProperty
            | Some (Constant _) ->
                warn loc "Constant attribute is not allowed on properties."
                None
            | Some (Macro _) ->
                warn loc "Macro attribute is not allowed on properties."
                None
            | _ ->
                None
        annot
        |> Option.map (fun kind ->
            {
                Name = p.Member.AddressSlot.Address
                Kind = kind
                Location = loc
                PropertyType = p.Member.Definition.PropertyType
                Reference = Adapter.AdaptProperty self
                Scope = Static
                Slot = p.Member.MemberSlot
            })

    let pCase loc (u: Re.UnionCase) : UnionCase =
        let self = u.Member
        let value =
            self.Annotations
            |> Seq.tryPick (function Re.Constant k -> Some k | _ -> None)
        let kind =
            match value with
            | Some k -> ConstantUnionCase k
            | None -> BasicUnionCase
        let reqs = getRequirements loc Static self.Annotations
        let name = u.Member.AddressSlot.Address.LocalName
        let r = Adapter.AdaptUnionCase self.Definition.DeclaringType name
        {
            Kind = kind
            Location = loc
            Reference = r
            Requirements = reqs
        }

    let rec pType (iP: Pool) (t: Re.Type) : option<Type> =
        let inline c f x = List.choose f x
        let loc = t.Location
        let rf = Adapter.AdaptTypeDefinition t.Definition
        let pStub = isStub t.Annotations
        match t.Kind with
        | Re.Class cSlot ->
            let d = t.Definition
            if isWebControl t.Definition then
                verifyWebControl logger verifier t
            if not d.IsAbstract && isResource d then
                Some {
                    Kind = Resource
                    Location = loc
                    Methods = []
                    Name = t.AddressSlot.Address
                    Properties = []
                    Proxy = None
                    Reference = rf
                    ReflectorType = t
                    Requirements = getRequirements loc Static t.Annotations
                    Status = Ignored
                }
            else
                let (ctors, mtods) =
                    t.Methods
                    |> List.partition (fun x -> x.Definition.IsConstructor)
                let cs = c (pCtor pStub t iP) ctors
                let ms = c (pMethod pStub iP) mtods
                let ns = c (pType iP) t.Nested
                let ps = c (pProp pStub iP) t.Properties
                let fs = t.Fields |> List.map pFieldRename 
                let bT =
                    match d.BaseType with
                    | None -> None
                    | Some bT -> Some (Adapter.AdaptType bT)
                Some {
                    Kind = 
                        Class { 
                            Slot = cSlot
                            BaseClass = bT
                            Constructors = cs
                            Nested = ns
                            Fields = fs |> List.map fst
                            FieldRenames = c (fun (on, rn) -> rn |> Option.map (fun n -> on, n)) fs
                        } 
                    Location = loc
                    Methods = ms
                    Name = t.AddressSlot.Address
                    Properties = ps
                    Proxy = getProxy t.Annotations
                    Reference = rf
                    ReflectorType = t
                    Requirements = getRequirements loc Static t.Annotations
                    Status =
                        let comp =
                            Seq.exists isCompiledConstructor cs
                            || Seq.exists isCompiledMethod ms
                            || Seq.exists isCompiledProperty ps
                            || Seq.exists isCompiledType ns
                        if comp then Compiled else Ignored
                }
        | Re.Enum ->
            match t.Annotations with
            | [] -> ()
            | _ ->
                warn t.Location "Annotations are ignored on \
                    enumeration types."
            None
        | Re.Exception ->
            match getProxy t.Annotations with
            | Some _ -> warn t.Location "Exception types cannot be proxies."
            | None -> ()
            match getRequirements t.Location Static t.Annotations with
            | [] -> ()
            | _ -> warn t.Location "Exception types cannot have requirements."
            let ms = c (pMethod pStub iP) t.Methods
            let ps = c (pProp pStub iP) t.Properties
            Some {
                Kind = Exception
                Location = loc
                Methods = ms
                Name = t.AddressSlot.Address
                Properties = ps
                Proxy = getProxy t.Annotations
                Reference = rf
                ReflectorType = t
                Requirements = getRequirements loc Static t.Annotations
                Status =
                    let comp =
                        Seq.exists isCompiledMethod ms
                        || Seq.exists isCompiledProperty ps
                    if comp then Compiled else Ignored
            }
        | Re.Interface ->
            let pMethod x =
                let k = pMethodKind pStub iP x
                match k with
                | None -> Some StubMethod
                | _ -> k
                |> pMethodFromKind iP x
            let pProp x =
                let k = pPropKind pStub iP x
                match k with
                | None -> Some InterfaceProperty
                | _ -> k
                |> pPropFromKind iP x
            let ms = c pMethod t.Methods
            let ps = c pProp t.Properties
            Some {
                Kind = Interface
                Location = loc
                Methods = ms
                Name = t.AddressSlot.Address
                Properties = ps
                Proxy = getProxy t.Annotations
                Reference = rf
                ReflectorType = t
                Requirements = getRequirements loc Static t.Annotations
                Status = Ignored
            }
        | Re.Module ->
            let ps = c (pField iP) t.Properties
            let ms = c (pMethod pStub iP) t.Methods
            let ns = c (pType iP) t.Nested
            Some {
                Kind = Module ns
                Location = loc
                Methods = ms
                Name = t.AddressSlot.Address
                Properties = ps
                Proxy = getProxy t.Annotations
                Reference = rf
                ReflectorType = t
                Requirements = getRequirements loc Static t.Annotations
                Status =
                    let comp =
                        Seq.exists isCompiledMethod ms
                        || Seq.exists isCompiledProperty ps
                        || Seq.exists isCompiledType ns
                    if comp then Compiled else Ignored
            }
        | Re.Record fs ->
            let fields =
                fs
                |> List.map (fun f ->
                    let oN = f.Definition.Name
                    let cN = f.AddressSlot.Address.LocalName
                    {
                        OriginalName = oN
                        JavaScriptName = cN
                        PropertyType = f.Definition.PropertyType
                    })
            let ms = c (pMethod pStub iP) t.Methods
            let ps = c (pProp pStub iP) t.Properties
            Some {
                Kind = Record fields
                Location = loc
                Methods = ms
                Name = t.AddressSlot.Address
                Properties = ps
                Proxy = getProxy t.Annotations
                Reference = rf
                ReflectorType = t
                Requirements = getRequirements loc Static t.Annotations
                Status =
                    let comp =
                        Seq.exists isCompiledMethod ms
                        || Seq.exists isCompiledProperty ps
                    if comp then Compiled else Ignored
            }
        | Re.Union cases ->
            let sR = ref Ignored
            let ms = c (pMethod pStub iP) t.Methods
            let ps = c (pProp pStub iP) t.Properties
            let cases = List.map (pCase t.Location) cases
            Some {
                Kind = Union cases
                Location = loc
                Methods = ms
                Name = t.AddressSlot.Address
                Properties = ps
                Proxy = getProxy t.Annotations
                Reference = rf
                ReflectorType = t
                Requirements = getRequirements loc Static t.Annotations
                Status =
                    let comp =
                        Seq.exists isCompiledMethod ms
                        || Seq.exists isCompiledProperty ps
                    if comp then Compiled else Ignored
            }

    let fieldRenames (types: list<Type>) =
        let localClasses = Dictionary()
        let rec addLocal (t: Type) =
            match t.Kind with
            | Class c -> 
                localClasses.Add(t.Reference, c)
                c.Nested |> List.iter addLocal
            | Module ns ->
                ns |> List.iter addLocal     
            | _ -> ()   
        types |> List.iter addLocal 
        let handled = Dictionary()
        let rec repl (bT: R.Type option) (fs : string list) (fr: (string * string) list) =
            match bT with
            | Some bt -> 
                let d = bt.DeclaringType 
                let baseFields =
                    match handled.TryGetValue d with
                    | true, (bfs, _) -> bfs
                    | _ ->
                    match localClasses.TryGetValue d with
                    | true, c ->
                        repl c.BaseClass c.Fields c.FieldRenames |> fst
                    | _ -> fields d
                    |> Set.ofList
                let newFields = HashSet()
                let newRenames = ResizeArray()
                let rec getSafeName n =
                    if baseFields.Contains n || newFields.Contains n then
                        getSafeName (n + "_")
                    else n
                for on, rn in fr do
                    let rn = getSafeName rn
                    newFields.Add rn |> ignore
                    if on <> rn then newRenames.Add (on, rn) 
                for on in fs do
                    let rn = getSafeName on
                    newFields.Add rn |> ignore
                    if on <> rn then newRenames.Add (on, rn) 
                List.ofSeq (Seq.append newFields baseFields), List.ofSeq newRenames
            | None -> fs, fr
        let rec updateReplacers (t: Type) =
            match t.Kind with 
            | Class c ->
                let fs, fr = repl c.BaseClass c.Fields c.FieldRenames
                { t with 
                    Kind = 
                        Class {
                            c with 
                                Nested = c.Nested |> List.map updateReplacers
                                Fields = fs
                                FieldRenames = fr
                        }
                }    
            | Module ns ->
                { t with Kind = Module (ns |> List.map updateReplacers) }
            | _ -> t
        types |> List.map updateReplacers   

    let reqs = getRequirements assembly.Location Static assembly.Annotations
    let types =
        assembly.Types
        |> List.choose (pType pool)
        |> fieldRenames
    let compiled =
        types
        |> List.exists isCompiledType
    let remotingProvider =
        assembly.Annotations
        |> List.tryPick (function
            | Reflector.Annotation.RemotingProvider cR -> Some cR
            | _ -> None)
    {
        Name = assembly.Name
        Location = assembly.Location
        Mode = if compiled then Me.CompiledAssembly else Me.IgnoredAssembly
        RemotingProvider = remotingProvider
        Requirements = reqs
        Types = types
    }

