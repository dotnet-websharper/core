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

[<AutoOpen>]
module internal IntelliFactory.WebSharper.Compiler.ReflectionLayer

open System.IO
module Q = IntelliFactory.WebSharper.Core.Quotations

[<AbstractClass>]
type Entity() =

    static let fail () =
        let msg = "Reflection entities do not support equality or printing"
        raise (new System.NotSupportedException(msg))

    override this.Equals(other: obj) = fail ()
    override this.GetHashCode() = fail ()
    override this.ToString() = fail ()

[<AbstractClass>]
type AssemblyDefinition() =
    inherit Entity()
    abstract CustomAttributes : list<CustomAttribute>
    abstract EmbeddedResources : Map<string,unit -> Stream>
    abstract Name : System.Reflection.AssemblyName
    abstract Types : seq<TypeDefinition>
    member this.FullName = this.Name.FullName

and [<AbstractClass>] CustomAttribute() =
    inherit Entity()
    abstract AttributeType : TypeReference
    abstract ConstructorArguments : list<CustomAttributeArgument>

and CustomAttributeArgument =
    | BoolArgument of bool
    | FloatArgument of float
    | IntArgument of int
    | IntsArgument of list<int>
    | StringArgument of string
    | StringsArgument of list<string>
    | TypeArgument of TypeReference

and [<AbstractClass>] FieldDefinition() =
    inherit Entity()
    abstract FieldType : TypeReference
    abstract IsNonSerialized : bool
    abstract IsStatic : bool

and [<AbstractClass>] MethodDefinition() =
    inherit MethodReference()

    abstract CustomAttributes : list<CustomAttribute>
    abstract IsConstructor : bool
    abstract IsGetter : bool
    abstract IsSetter : bool
    abstract IsStatic : bool
    abstract IsVirtual : bool
    abstract Overrides : list<MethodReference>
    abstract RawQuotation : option<Q.Expression>
    abstract SourceLocation : option<SourceLocation>

and [<AbstractClass>] MethodReference() =
    inherit Entity()
    abstract DeclaringType : TypeReference
    abstract GenericArity : int
    abstract Name : string
    abstract Parameters : list<ParameterDefinition>
    abstract ReturnType : option<TypeReference>

    member this.HasGenericParameters = this.GenericArity > 0

and [<AbstractClass>] ParameterDefinition() =
    inherit Entity()
    abstract Name : string
    abstract ParameterType : TypeReference

and [<AbstractClass>] PropertyDefinition() =
    inherit PropertyReference()

    abstract IsStatic : bool
    abstract CustomAttributes : list<CustomAttribute>
    abstract GetMethod : option<MethodDefinition>
    abstract SetMethod : option<MethodDefinition>
    abstract RawQuotation : option<Q.Expression>

and [<AbstractClass>] PropertyReference() =
    inherit Entity()
    abstract DeclaringType : TypeReference
    abstract Name : string
    abstract Parameters : list<ParameterDefinition>
    abstract PropertyType : TypeReference

and [<AbstractClass>] TypeDefinition() =
    inherit TypeReference()
    abstract BaseType : option<TypeReference>
    abstract CustomAttributes : list<CustomAttribute>
    abstract Fields : seq<FieldDefinition>
    abstract Interfaces : seq<TypeReference>
    abstract IsAbstract : bool
    abstract IsClass : bool
    abstract IsInterface : bool
    abstract IsEnum : bool
    abstract IsSerializable : bool
    abstract IsValueType : bool
    abstract Methods : seq<MethodDefinition>
    abstract NestedTypes : seq<TypeDefinition>
    abstract Properties : seq<PropertyDefinition>

and TypeGenericOwner =
    | OwnerMethod of MethodReference
    | OwnerType of TypeReference

and [<AbstractClass>] TypeReference() =
    inherit Entity()

    abstract Resolve : unit -> TypeDefinition
    abstract AssemblyName : string
    abstract DeclaringType : option<TypeReference>
    abstract FullName : string
    abstract GenericArity : int

    abstract IsPrimitive : bool
    abstract Name : string
    abstract Namespace : string
    abstract Shape : TypeShape

    member this.HasGenericParameters = this.GenericArity > 0

and TypeShape =
    | ArrayType of int * TypeReference
    | GenericInstanceType of list<TypeReference>
    | GenericParameter of TypeGenericOwner * int
    | OtherType

module Cecil =

    [<Sealed>]
    type Converter =
        static member TRef(x: Mono.Cecil.TypeReference) : TypeReference = TRef(x) :> _
        static member TDef(x: Mono.Cecil.TypeDefinition) : TypeDefinition = TDef(x) :> _
        static member MDef(x: Mono.Cecil.MethodDefinition) : MethodDefinition = MDef(x) :> _
        static member MRef(x: Mono.Cecil.MethodReference) : MethodReference = MRef(x) :> _
        static member PDef(x: Mono.Cecil.PropertyDefinition) : PropertyDefinition = PDef(x) :> _
        static member PRef(x: Mono.Cecil.PropertyReference) : PropertyReference = PRef(x) :> _
        static member FDef(x: Mono.Cecil.FieldDefinition) : FieldDefinition = FDef(x) :> _

        static member GetAssemblyName(tR: Mono.Cecil.TypeReference) =
            match tR.Scope.MetadataScopeType with
            | Mono.Cecil.MetadataScopeType.AssemblyNameReference ->
                let anr = tR.Scope :?> Mono.Cecil.AssemblyNameReference
                anr.FullName
            | _ -> tR.Module.Assembly.FullName

        static member GetShape(tR: Mono.Cecil.TypeReference) : TypeShape =
            if tR.IsArray then
                let tR = tR :?> Mono.Cecil.ArrayType
                ArrayType (tR.Rank, Converter.TRef(tR.ElementType))
            elif tR.IsGenericParameter then
                let tR = tR :?> Mono.Cecil.GenericParameter
                match tR.Owner with
                | :? Mono.Cecil.MethodReference as mR ->
                    GenericParameter (OwnerMethod (Converter.MRef(mR)), tR.Position)
                | :? Mono.Cecil.TypeReference as x ->
                    GenericParameter (OwnerType (Converter.TRef(x)), tR.Position)
                | _ ->
                    OtherType
            elif tR.IsGenericInstance then
                let tR = tR :?> Mono.Cecil.GenericInstanceType
                GenericInstanceType [for x in tR.GenericArguments -> Converter.TRef(x)]
            else
                OtherType

        static member ConvertArgument(x: Mono.Cecil.CustomAttributeArgument) : option<CustomAttributeArgument> =
            match x.Type.FullName with
            | "System.Boolean" -> Some (BoolArgument (x.Value :?> _))
            | "System.Double" -> Some (FloatArgument (x.Value :?> _))
            | "System.Int32" -> Some (IntArgument (x.Value :?> _))
            | "System.Int32[]" ->
                let arr = x.Value :?> Mono.Cecil.CustomAttributeArgument[]
                Some (IntsArgument [for x in arr -> x.Value :?> _])
            | "System.String" -> Some (StringArgument (x.Value :?> _))
            | "System.String[]" ->
                let arr = x.Value :?> Mono.Cecil.CustomAttributeArgument[]
                Some (StringsArgument [for x in arr -> x.Value :?> _])
            | _ ->
                match x.Value with
                | :? Mono.Cecil.TypeReference as tR ->
                    Some (TypeArgument (Converter.TRef(tR)))
                | :? int as i ->
                    Some (IntArgument i)
                | _ -> None

        static member GetCustomAttributes(x: Mono.Cecil.ICustomAttributeProvider) : list<CustomAttribute> =
            if x.HasCustomAttributes then
                [
                    for p in x.CustomAttributes do
                        yield {
                            new CustomAttribute() with
                                override this.AttributeType =
                                    Converter.TRef(p.AttributeType)
                                override this.ConstructorArguments =
                                    if p.HasConstructorArguments then
                                        List.ofSeq (Seq.choose Converter.ConvertArgument p.ConstructorArguments)
                                    else
                                        []
                        }
                ]
            else []

        static member GetReturnType(t: Mono.Cecil.TypeReference) =
            match t with
            | null -> None
            | t when t.FullName = "System.Void" -> None
            | t when t.FullName = typeof<unit>.FullName -> None
            | t -> Some (Converter.TRef t)

        static member GetParameters(has: bool, ps: seq<Mono.Cecil.ParameterDefinition>) =
            if has then
                [
                    for p in ps ->
                        {
                            new ParameterDefinition() with
                                override this.Name = p.Name
                                override this.ParameterType = Converter.TRef(p.ParameterType)
                        }
                ]
            else
                []

    and [<Sealed>] TRef(tR: Mono.Cecil.TypeReference) =
        inherit TypeReference()

        override this.AssemblyName = Converter.GetAssemblyName(tR)
        override this.Resolve() = Converter.TDef(tR.Resolve())
        override this.FullName = tR.FullName
        override this.IsPrimitive = tR.IsPrimitive
        override this.Name = tR.Name
        override this.Namespace = tR.Namespace
        override this.Shape = Converter.GetShape(tR)

        override this.DeclaringType =
            match tR.DeclaringType with
            | null -> None
            | dT -> Some (Converter.TRef(dT))

        override this.GenericArity =
            if tR.HasGenericParameters
                then tR.GenericParameters.Count
                else 0

    and [<Sealed>] TDef(tD: Mono.Cecil.TypeDefinition) =
        inherit TypeDefinition()

        override this.AssemblyName = Converter.GetAssemblyName(tD)
        override this.Resolve() = this :> _
        override this.FullName = tD.FullName
        override this.IsPrimitive = tD.IsPrimitive
        override this.Name = tD.Name
        override this.Namespace = tD.Namespace
        override this.Shape = Converter.GetShape(tD)

        override this.IsAbstract = tD.IsAbstract
        override this.IsClass = tD.IsClass
        override this.IsInterface = tD.IsInterface
        override this.IsEnum = tD.IsEnum
        override this.IsSerializable = tD.IsSerializable
        override this.IsValueType = tD.IsValueType

        override this.CustomAttributes = Converter.GetCustomAttributes(tD)

        override this.BaseType =
            match tD.BaseType with
            | null -> None
            | dT -> Some (Converter.TRef(dT))

        override this.DeclaringType =
            match tD.DeclaringType with
            | null -> None
            | dT -> Some (Converter.TRef(dT))

        override this.GenericArity =
            if tD.HasGenericParameters
                then tD.GenericParameters.Count
                else 0

        override this.Fields = Seq.map Converter.FDef tD.Fields
        override this.Interfaces = Seq.map Converter.TRef tD.NestedTypes
        override this.Methods = Seq.map Converter.MDef tD.Methods
        override this.NestedTypes = Seq.map Converter.TDef tD.NestedTypes
        override this.Properties = Seq.map Converter.PDef tD.Properties

    and [<Sealed>] MRef(mR: Mono.Cecil.MethodReference) =
        inherit MethodReference()

        override this.DeclaringType = Converter.TRef(mR.DeclaringType)

        override this.GenericArity =
            if mR.HasGenericParameters
                then mR.GenericParameters.Count
                else 0

        override this.Name = mR.Name
        override this.Parameters = Converter.GetParameters(mR.HasParameters, mR.Parameters)
        override this.ReturnType = Converter.GetReturnType(mR.ReturnType)

    and [<Sealed>] MDef(mR: Mono.Cecil.MethodDefinition) =
        inherit MethodDefinition()

        override this.DeclaringType = Converter.TRef(mR.DeclaringType)

        override this.GenericArity =
            if mR.HasGenericParameters
                then mR.GenericParameters.Count
                else 0

        override this.Name = mR.Name
        override this.Parameters = Converter.GetParameters(mR.HasParameters, mR.Parameters)
        override this.ReturnType = Converter.GetReturnType(mR.ReturnType)
        override this.CustomAttributes = Converter.GetCustomAttributes(mR)
        override this.IsConstructor = mR.IsConstructor
        override this.IsGetter = mR.IsGetter
        override this.IsSetter = mR.IsSetter
        override this.IsStatic = mR.IsStatic
        override this.IsVirtual = mR.IsVirtual
        override this.Overrides = [for x in mR.Overrides -> Converter.MRef(x)]
        override this.RawQuotation = None

        override this.SourceLocation =
            let def = mR
            if def.HasBody && def.Body.Instructions <> null then
                def.Body.Instructions
                |> Seq.tryPick (fun i ->
                    let sP = i.SequencePoint
                    if sP <> null then
                        Some {
                            File = sP.Document.Url
                            Line = sP.StartLine - 1
                            Column = sP.StartColumn
                        }
                    else
                        None)
            else
                None

    and [<Sealed>] PRef(x: Mono.Cecil.PropertyReference) =
        inherit PropertyReference()

        override this.DeclaringType = Converter.TRef(x.DeclaringType)
        override this.Name = x.Name
        override this.Parameters = Converter.GetParameters(x.Parameters.Count > 0, x.Parameters)
        override this.PropertyType = Converter.TRef(x.PropertyType)

    and [<Sealed>] PDef(x: Mono.Cecil.PropertyDefinition) =
        inherit PropertyDefinition()

        override this.DeclaringType = Converter.TRef(x.DeclaringType)
        override this.Name = x.Name
        override this.Parameters = Converter.GetParameters(x.Parameters.Count > 0, x.Parameters)
        override this.PropertyType = Converter.TRef(x.PropertyType)

        override this.IsStatic =
            match x.GetMethod, x.SetMethod with
            | null, null -> false
            | null, x | x, _ -> x.IsStatic

        override this.CustomAttributes =
            Converter.GetCustomAttributes(x)

        override this.GetMethod =
            match x.GetMethod with
            | null -> None
            | x -> Some (Converter.MDef(x))

        override this.SetMethod =
            match x.SetMethod with
            | null -> None
            | x -> Some (Converter.MDef(x))

        override this.RawQuotation = None

    and [<Sealed>] FDef(x: Mono.Cecil.FieldDefinition) =
        inherit FieldDefinition()
        override this.FieldType = Converter.TRef(x.FieldType)
        override this.IsNonSerialized = int (x.Attributes &&& Mono.Cecil.FieldAttributes.NotSerialized) > 0
        override this.IsStatic = x.IsStatic

    [<Sealed>]
    type ADef(a: Mono.Cecil.AssemblyDefinition) =
        inherit AssemblyDefinition()

        override this.CustomAttributes = Converter.GetCustomAttributes(a)
        override this.Name = System.Reflection.AssemblyName(a.FullName)
        override this.Types = Seq.map Converter.TDef a.MainModule.Types

        override this.EmbeddedResources =
            if a.MainModule.HasResources then
                Map.ofSeq [|
                    for r in a.MainModule.Resources do
                        if int (r.ResourceType &&& Mono.Cecil.ResourceType.Embedded) > 0 then
                            let r = r :?> Mono.Cecil.EmbeddedResource
                            yield (r.Name, fun () -> r.GetResourceStream())
                |]
            else Map.empty

    let AdaptAssembly (assembly: Mono.Cecil.AssemblyDefinition) : AssemblyDefinition =
        ADef(assembly) :> _

module QuotationUtils =
    open System.Collections.Generic
    open System.Reflection
    module RQ = Quotations.Patterns
    module CR = IntelliFactory.WebSharper.Core.Reflection

    let convertTypes (ts: System.Type[]) : list<CR.Type> =
        [
            for t in ts ->
                CR.Type.FromType(t)
        ]

    let getProperTypeGenerics (t: System.Type) =
        if t.IsGenericType || t.IsGenericTypeDefinition then
            t.GetGenericArguments()
            |> convertTypes
        else []

    let getProperMethodGenerics (m: MethodBase) =
        if m.IsGenericMethod|| m.IsGenericMethodDefinition then
            m.GetGenericArguments()
            |> convertTypes
        else []

    let rec getGenerics (t: System.Type) =
        match t.DeclaringType with
        | null -> getProperTypeGenerics t
        | dT -> getGenerics dT @ getProperTypeGenerics t

    let makeConcrete (dT: System.Type) (gs: list<CR.Type>) (entity: 'T) : Q.Concrete<'T> =
        {
            Entity = entity
            Generics = getGenerics dT @ gs
        }

    let makeSignature (xs: seq<ParameterInfo>) : CR.Signature =
        [for x in xs -> CR.Type.FromType x.ParameterType]

    let ConvertConstructor (c: ConstructorInfo) : Q.Concrete<CR.Constructor> =
        let tD = CR.TypeDefinition.FromType c.DeclaringType
        makeSignature (c.GetParameters())
        |> CR.Constructor.Create tD
        |> makeConcrete c.DeclaringType []

    let ConvertMethod (m: MethodInfo) : Q.Concrete<CR.Method> =
        CR.Method.Parse m
        |> makeConcrete m.DeclaringType (getProperMethodGenerics m)

    let ConvertField (f: FieldInfo) : Q.Concrete<CR.Field> =
        let tD = CR.TypeDefinition.FromType f.DeclaringType
        CR.Field.Create tD f.Name
        |> makeConcrete f.DeclaringType []

    let ConvertProperty (p: PropertyInfo) : Q.Concrete<CR.Property> =
        let tD = CR.TypeDefinition.FromType p.DeclaringType
        let ty = CR.Type.FromType p.PropertyType
        let pSig = makeSignature (p.GetIndexParameters())
        CR.Property.Create tD p.Name ty pSig
        |> makeConcrete p.DeclaringType []

    let ConvertUnionCase (uci: Reflection.UnionCaseInfo) : Q.Concrete<CR.UnionCase> =
        let tD = CR.TypeDefinition.FromType uci.DeclaringType
        CR.UnionCase.Create tD uci.Name
        |> makeConcrete uci.DeclaringType []

    let ParseLiteral : System.Type -> obj -> Q.Literal =
        let table : IDictionary<System.Type, obj -> Q.Literal> =
            dict [|
                typeof<bool>, fun x -> Q.Bool (x :?> _)
                typeof<byte>, fun x -> Q.Byte (x :?> _)
                typeof<char>, fun x -> Q.Char (x :?> _)
                typeof<double>, fun x -> Q.Double (x :?> _)
                typeof<int>, fun x -> Q.Int (x :?> _)
                typeof<int16>, fun x -> Q.Int16 (x :?> _)
                typeof<int64>, fun x -> Q.Int64 (x :?> _)
                typeof<sbyte>, fun x -> Q.SByte (x :?> _)
                typeof<single>, fun x -> Q.Single (x :?> _)
                typeof<string>, fun x -> Q.String (x :?> _)
                typeof<unit>, fun _ -> Q.Unit
                typeof<uint16>, fun x -> Q.UInt16 (x :?> _)
                typeof<uint32>, fun x -> Q.UInt32 (x :?> _)
                typeof<uint64>, fun x -> Q.UInt64 (x :?> _)
            |]
        fun t x ->
            match table.TryGetValue(t) with
            | true, f -> f x
            | _ -> Q.Literal.Unit

    let ConvertQuotation (q: Quotations.Expr) : Q.Expression =
        let ( !^ ) = CR.Type.FromType
        let d = Dictionary(HashIdentity.Reference)
        let ( !? ) (x: Quotations.Var) : Q.Id =
            match d.TryGetValue(x) with
            | true, v -> v
            | _ ->
                let ty = !^ x.Type
                let v =
                    if x.IsMutable
                        then Q.Id.CreateMutable x.Name ty
                        else Q.Id.Create x.Name ty
                d.Add(x, v)
                v
        let rec ( !! ) xs = List.map (!) xs
        and (!) x =
            match x with
            | RQ.AddressOf x -> Q.AddressOf !x
            | RQ.AddressSet (x, y) -> Q.AddressSet (!x, !y)
            | RQ.Application (f, x) -> Q.Application (!f, !x)
            | RQ.Call (None, m, xs) -> Q.Call (ConvertMethod m, !!xs)
            | RQ.Call (Some x, m, xs) -> Q.Call (ConvertMethod m, !!(x :: xs))
            | RQ.Coerce (q, t) -> Q.Coerce (!^t, !q)
            | RQ.DefaultValue t -> Q.DefaultValue !^t
            | RQ.FieldGet (None, fld) ->
                Q.FieldGetStatic (ConvertField fld)
            | RQ.FieldGet (Some t, fld) ->
                Q.FieldGetInstance (!t, ConvertField fld)
            | RQ.FieldSet (None, fld, v) ->
                Q.FieldSetStatic (ConvertField fld, !v)
            | RQ.ForIntegerRangeLoop (a, b, c, d) ->
                Q.ForIntegerRangeLoop (!?a, !b, !c, !d)
            | RQ.IfThenElse (a, b, c) -> Q.IfThenElse (!a, !b, !c)
            | RQ.Lambda (v, b) -> Q.Lambda (!?v, !b)
            | RQ.Let (a, b, c) -> Q.Let (!?a, !b, !c)
            | RQ.LetRecursive (bs, b) ->
                Q.LetRecursive ([for (k, v) in bs -> (!?k, !v)], !b)
            | RQ.NewArray (t, xs) -> Q.NewArray (!^t, !!xs)
            | RQ.NewDelegate (t, vs, e) -> Q.NewDelegate (!^t, !e)
            | RQ.NewObject (ctor, xs) ->
                Q.NewObject (ConvertConstructor ctor, !!xs)
            | RQ.NewRecord (t, xs) -> Q.NewRecord (!^t, !!xs)
            | RQ.NewTuple xs -> Q.NewTuple !!xs
            | RQ.NewUnionCase (x, xs) ->
                Q.NewUnionCase (ConvertUnionCase x, !!xs)
            | RQ.PropertyGet (None, prop, xs) ->
                Q.PropertyGet (ConvertProperty prop, !!xs)
            | RQ.PropertyGet (Some x, prop, xs) ->
                Q.PropertyGet (ConvertProperty prop, !!(x :: xs))
            | RQ.PropertySet (None, prop, xs, v) ->
                Q.PropertySet (ConvertProperty prop, !!(xs @ [v]))
            | RQ.PropertySet (Some x, prop, xs, v) ->
                Q.PropertySet (ConvertProperty prop, !!(x :: xs @ [v]))
            | RQ.Quote q -> Q.Quote !q
            | RQ.Sequential (a, b) -> Q.Sequential (!a, !b)
            | RQ.TryFinally (a, b) -> Q.TryFinally (!a, !b)
            | RQ.TryWith (a, b, c, d, e) -> Q.TryWith (!a, !?b, !c, !?d, !e)
            | RQ.TupleGet (a, b) -> Q.TupleGet (b, !a)
            | RQ.TypeTest (a, b) -> Q.TypeTest (!^b, !a)
            | RQ.UnionCaseTest (a, b) ->
                Q.UnionCaseTest (ConvertUnionCase b, !a)
            | RQ.Value (obj, ty) -> Q.Value (ParseLiteral ty obj)
            | RQ.Var x -> Q.Var !?x
            | RQ.VarSet (x, y) -> Q.VarSet (!?x, !y)
            | RQ.WhileLoop (a, b) -> Q.WhileLoop (!a, !b)
            | q -> failwithf "Unknown quotation: %O" q
        !q

module Reflection =
    open System.Reflection
    open System.Collections.Generic

    let memoize (f: 'T1 -> 'T2) : 'T1 -> 'T2 =
        let d = Dictionary()
        fun x ->
            match d.TryGetValue(x) with
            | true, y -> y
            | _ ->
                let y = f x
                d.Add(x, y)
                y

    type MethodKind =
        | GetterMethod
        | NormalMethod
        | SetterMethod

        static member Infer(name: string) =
            if name.StartsWith("get_") then GetterMethod
            elif name.StartsWith("set_") then SetterMethod
            else NormalMethod

        member this.IsGetter =
            match this with
            | GetterMethod -> true | _ -> false

        member this.IsSetter =
            match this with
            | SetterMethod -> true | _ -> false

    [<Sealed>]
    type Converter() as self =
        let convFild = memoize (fun (x: FieldInfo) -> RField(self, x) :> FieldDefinition)
        let convType = memoize (fun (t: System.Type) -> RType(self, t) :> TypeDefinition)
        let convMeth = memoize (fun (x: MethodBase) -> RMethod(self, x) :> MethodDefinition)
        let convProp = memoize (fun (x: PropertyInfo) -> RProperty(self, x) :> PropertyDefinition)

        let customArgMap =
            dict [|
                typeof<bool>, fun x -> BoolArgument (unbox x)
                typeof<float>, fun x -> FloatArgument (unbox x)
                typeof<int>, fun x -> IntArgument (unbox x)
                typeof<int[]>, fun x -> IntsArgument (unbox x)
                typeof<string>, fun x -> StringArgument (unbox x)
                typeof<string[]>, fun x -> StringsArgument (unbox x)
                typeof<System.Type>, fun (x: obj) -> TypeArgument (convType (unbox x) :> TypeReference)
            |]

        member this.ConvertAttributes(attrs: IList<CustomAttributeData>) : list<CustomAttribute> =
            [
                for x in attrs do
                    let aT = convType (x.Constructor.DeclaringType) :> TypeReference
                    let cs =
                        [
                            for arg in x.ConstructorArguments do
                                let t =
                                    if arg.ArgumentType.IsEnum then
                                        System.Enum.GetUnderlyingType(arg.ArgumentType)
                                    else
                                        arg.ArgumentType
                                match customArgMap.TryGetValue(t) with
                                | true, f -> yield f arg.Value
                                | _ -> ()
                        ]
                    yield {
                        new CustomAttribute() with
                            override this.AttributeType = aT
                            override this.ConstructorArguments = cs
                    }
            ]

        member this.ConvertField(f: FieldInfo) = convFild f
        member this.ConvertMethod(m: MethodBase) = convMeth m
        member this.ConvertProperty(p: PropertyInfo) = convProp p
        member this.ConvertType(t: System.Type) = convType t

    and [<Sealed>] RField(conv: Converter, p: FieldInfo) =
        inherit FieldDefinition()

        do if p = null then nullArg "p"

        let fT = lazy conv.ConvertType(p.FieldType)

        override this.FieldType = fT.Value :> _
        override this.IsNonSerialized = p.IsNotSerialized
        override this.IsStatic = p.IsStatic

    and [<Sealed>] RMethod(conv: Converter, b: MethodBase) =
        inherit MethodDefinition()

        do if b = null then nullArg "b"

        let quot =
            lazy
                try
                    match b with
                    | Quotations.DerivedPatterns.MethodWithReflectedDefinition d ->
                        Some (QuotationUtils.ConvertQuotation d)
                    | _ -> None
                with _ -> None

        let kind = MethodKind.Infer(b.Name)

        let attrs =
            lazy
                CustomAttributeData.GetCustomAttributes(b)
                |> conv.ConvertAttributes

        let declaringType =
            lazy conv.ConvertType(b.DeclaringType)

        let genericArity =
            if b.IsGenericMethod || b.IsGenericMethodDefinition
                then b.GetGenericArguments().Length
                else 0

        let ps =
            lazy [
                for p in b.GetParameters() ->
                    let name = p.Name
                    let ty = conv.ConvertType(p.ParameterType) :> TypeReference
                    {
                        new ParameterDefinition() with
                            member this.Name = name
                            member this.ParameterType = ty
                    }
            ]

        let returnType =
            lazy
            if b.IsConstructor then None else
                match b with
                | :? MethodInfo as m ->
                    match m.ReturnType with
                    | null -> None
                    | t when t = typeof<unit> -> None
                    | t when t = typeof<System.Void> -> None
                    | t -> Some (conv.ConvertType(t) :> TypeReference)
                | _ -> None

        let overrides =
            if b.IsConstructor then lazy [] else
                let m = b :?> MethodInfo
                lazy
                    let bD =
                        match m.GetBaseDefinition() with
                        | null -> None
                        | x when x = m -> None
                        | x -> Some (conv.ConvertMethod(x) :> MethodReference)
                    [
                        yield! Option.toList bD
                        for i in m.DeclaringType.GetInterfaces() do
                            let map = m.DeclaringType.GetInterfaceMap(i)
                            let index =
                                map.TargetMethods
                                |> Seq.tryFindIndex (fun x -> x = m)
                            match index with
                            | None -> ()
                            | Some i -> yield conv.ConvertMethod(map.InterfaceMethods.[i]) :> MethodReference
                    ]

        override this.DeclaringType = declaringType.Value :> _
        override this.GenericArity = genericArity
        override this.Name = b.Name
        override this.Parameters = ps.Value
        override this.ReturnType = returnType.Value

        override this.CustomAttributes = attrs.Value
        override this.IsConstructor = b.IsConstructor
        override this.IsGetter = kind.IsGetter
        override this.IsSetter = kind.IsSetter
        override this.IsStatic = b.IsStatic
        override this.IsVirtual = b.IsVirtual
        override this.Overrides = overrides.Value
        override this.RawQuotation = quot.Value
        override this.SourceLocation = None

    and [<Sealed>] RProperty(conv: Converter, p: PropertyInfo) =
        inherit PropertyDefinition()

        do if p = null then nullArg "p"

        let attrs =
            lazy
                CustomAttributeData.GetCustomAttributes(p)
                |> conv.ConvertAttributes

        let getMethod =
            lazy
            match p.GetGetMethod(true) with
            | null -> None
            | m -> Some (conv.ConvertMethod(m))

        let setMethod =
            lazy
            match p.GetSetMethod(true) with
            | null -> None
            | m -> Some (conv.ConvertMethod(m))

        let dType = lazy conv.ConvertType(p.DeclaringType)
        let pType = lazy conv.ConvertType(p.PropertyType)

        let isStatic =
            lazy
            match p.GetGetMethod(true) with
            | null ->
                match p.GetSetMethod(true) with
                | null -> false
                | m -> m.IsStatic
            | m -> m.IsStatic

        let quot =
            lazy
                try
                    match p with
                    | Quotations.DerivedPatterns.PropertyGetterWithReflectedDefinition d ->
                        Some (QuotationUtils.ConvertQuotation(d))
                    | _ -> None
                with _ -> None

        let ps =
            lazy [
                for x in p.GetIndexParameters() ->
                    let name = x.Name
                    let ty = conv.ConvertType(x.ParameterType) :> TypeReference
                    {
                        new ParameterDefinition() with
                            member this.Name = name
                            member this.ParameterType = ty
                    }
            ]

        override this.IsStatic = isStatic.Value
        override this.CustomAttributes = attrs.Value
        override this.GetMethod = getMethod.Value
        override this.SetMethod = setMethod.Value
        override this.RawQuotation = quot.Value
        override this.DeclaringType = dType.Value :> _
        override this.Name = p.Name
        override this.Parameters = ps.Value
        override this.PropertyType = pType.Value :> _

    and [<Sealed>] RType(conv: Converter, t: System.Type) =
        inherit TypeDefinition()

        do if t = null then nullArg "t"

        let attrs =
            lazy
                CustomAttributeData.GetCustomAttributes(t)
                |> conv.ConvertAttributes

        let genericArity =
            if t.IsGenericType || t.IsGenericTypeDefinition
                then t.GetGenericArguments().Length
                else 0

        let baseType =
            lazy
            match t.BaseType with
            | null -> None
            | bT -> Some (conv.ConvertType(bT) :> TypeReference)

        let declaringType =
            lazy
            match t.DeclaringType with
            | null -> None
            | dT -> Some (conv.ConvertType(dT) :> TypeReference)

        let shape =
            lazy
                if t.IsArray then
                    ArrayType (t.GetArrayRank(), conv.ConvertType(t.GetElementType()))
                elif t.IsGenericParameter then
                    let pos = t.GenericParameterPosition
                    let owner =
                        match t.DeclaringMethod with
                        | null -> OwnerType declaringType.Value.Value
                        | m -> OwnerMethod (conv.ConvertMethod(m))
                    GenericParameter (owner, pos)
                elif t.IsGenericType && not t.IsGenericTypeDefinition then
                    GenericInstanceType [
                        for x in t.GetGenericArguments() ->
                            conv.ConvertType(x) :> _
                    ]
                else
                    OtherType

        let flags =
            System.Reflection.BindingFlags.DeclaredOnly
            ||| System.Reflection.BindingFlags.Public
            ||| System.Reflection.BindingFlags.NonPublic
            ||| System.Reflection.BindingFlags.Static
            ||| System.Reflection.BindingFlags.Instance

        let fields =
            lazy [| for x in t.GetFields(flags) -> conv.ConvertField(x) |]

        let interfaces =
            lazy [| for x in t.GetInterfaces() -> conv.ConvertType(x) :> TypeReference |]

        let methods =
            lazy
                [|
                    for x in t.GetMethods(flags) do
                        yield conv.ConvertMethod(x)
                    for x in t.GetConstructors(flags) do
                        if not x.IsStatic then
                            yield conv.ConvertMethod(x)
                |]

        let nested =
            lazy [| for x in t.GetNestedTypes(flags) -> conv.ConvertType(x) |]

        let properties =
            lazy [| for x in t.GetProperties(flags) -> conv.ConvertProperty(x) |]

        override this.AssemblyName = t.Assembly.FullName
        override this.BaseType = baseType.Value
        override this.CustomAttributes = attrs.Value
        override this.DeclaringType = declaringType.Value
        override this.FullName = t.FullName
        override this.GenericArity = genericArity
        override this.Name = t.Name
        override this.Namespace = t.Namespace

        override this.IsAbstract = t.IsAbstract
        override this.IsClass = t.IsClass
        override this.IsEnum = t.IsEnum
        override this.IsInterface = t.IsInterface
        override this.IsPrimitive = t.IsPrimitive
        override this.IsSerializable = t.IsSerializable
        override this.IsValueType = t.IsValueType

        override this.Fields : seq<FieldDefinition> = fields.Value :> seq<_>
        override this.Interfaces : seq<TypeReference> = interfaces.Value :> seq<_>
        override this.Methods : seq<MethodDefinition> = methods.Value :> seq<_>
        override this.NestedTypes : seq<TypeDefinition> = nested.Value :> seq<_>
        override this.Properties : seq<PropertyDefinition> = properties.Value :> seq<_>

        override this.Resolve() = this :> _
        override this.Shape = shape.Value

    and [<Sealed>] RAssembly(conv: Converter, d: Assembly) =
        inherit AssemblyDefinition()

        let atts =
            lazy
                try CustomAttributeData.GetCustomAttributes(d)
                    |> conv.ConvertAttributes
                with _ -> []

        let name = lazy d.GetName()

        let embeddedResources =
            lazy
                let names = try d.GetManifestResourceNames() with _ -> Array.empty
                seq {
                    for n in names ->
                        (n, fun () -> d.GetManifestResourceStream(n))
                }
                |> Map.ofSeq

        let types =
            lazy
                try
                    d.GetTypes()
                    |> Array.map (fun x -> conv.ConvertType(x))
                with _ ->
                    Array.empty

        override this.CustomAttributes = atts.Value
        override this.EmbeddedResources = embeddedResources.Value
        override this.Name = name.Value
        override this.Types = types.Value :> seq<_>

    let AdaptAssembly (a: Assembly) : AssemblyDefinition =
        RAssembly(Converter(), a) :> _

module Dynamic =
    open IntelliFactory.WebSharper
    open IntelliFactory.WebSharper.Core.Attributes

    [<Name "WebSharper.EntryPoint">]
    module private Sample =

        type Marker = class end

        [<JavaScript>]
        let Example = ()

    let currentModule =
        let conv = Reflection.Converter()
        conv.ConvertType(typeof<Sample.Marker>.DeclaringType)

    [<Sealed>]
    type MockMethodDefinition(m: MethodDefinition, q) =
        inherit MethodDefinition()

        override this.CustomAttributes = m.CustomAttributes
        override this.IsConstructor = m.IsConstructor
        override this.IsGetter = m.IsGetter
        override this.IsSetter = m.IsSetter
        override this.IsStatic = m.IsStatic
        override this.IsVirtual = m.IsVirtual
        override this.Overrides = m.Overrides
        override this.RawQuotation = Some q
        override this.SourceLocation = m.SourceLocation

        override this.DeclaringType = m.DeclaringType
        override this.GenericArity = m.GenericArity
        override this.Name = m.Name
        override this.Parameters = m.Parameters
        override this.ReturnType = m.ReturnType

    [<Sealed>]
    type MockPropertyDefinition(p: PropertyDefinition, q) =
        inherit PropertyDefinition()

        let gm =
            match p.GetMethod with
            | Some m -> Some (MockMethodDefinition(m, q) :> MethodDefinition)
            | None -> None

        override this.IsStatic = p.IsStatic
        override this.CustomAttributes = p.CustomAttributes
        override this.GetMethod = gm
        override this.SetMethod = p.SetMethod
        override this.RawQuotation = Some q
        override this.DeclaringType = p.DeclaringType
        override this.Name = p.Name
        override this.Parameters = p.Parameters
        override this.PropertyType = p.PropertyType

    [<Sealed>]
    type MockTypeDefinition(q) =
        inherit TypeDefinition()

        let t = currentModule

        let props =
            [|
                for p in t.Properties do
                    if p.Name = "Example" then
                        yield MockPropertyDefinition(p, q) :> PropertyDefinition
                    else
                        yield p
            |]

        override this.BaseType = t.BaseType
        override this.CustomAttributes = t.CustomAttributes
        override this.Fields = t.Fields
        override this.Interfaces = t.Interfaces
        override this.IsAbstract = t.IsAbstract
        override this.IsClass = t.IsClass
        override this.IsInterface = t.IsInterface
        override this.IsEnum = t.IsEnum
        override this.IsSerializable = t.IsSerializable
        override this.IsValueType = t.IsValueType
        override this.Methods = t.Methods
        override this.NestedTypes = t.NestedTypes
        override this.Properties = props :> seq<_>
        override this.Resolve() = this :> _
        override this.AssemblyName = t.AssemblyName
        override this.DeclaringType = t.DeclaringType
        override this.FullName = t.FullName
        override this.GenericArity = t.GenericArity
        override this.IsPrimitive = t.IsPrimitive
        override this.Name = t.Name
        override this.Namespace = t.Namespace
        override this.Shape = t.Shape

    let FromQuotation (q: Quotations.Expr) : AssemblyDefinition =
        let q = QuotationUtils.ConvertQuotation(q)
        let t = MockTypeDefinition(q) :> TypeDefinition
        let n = System.Reflection.AssemblyName("WebSharper.EntryPoint")
        {
            new AssemblyDefinition() with
                override this.CustomAttributes = []
                override this.EmbeddedResources = Map.empty
                override this.Name = n
                override this.Types = Seq.singleton t
        }
