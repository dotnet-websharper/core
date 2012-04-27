// $begin{copyright}
//
// This file is part of WebSharper
//
// Copyright (c) 2008-2012 IntelliFactory
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

module IntelliFactory.WebSharper.Compiler.Verifier

type Type =
    | Array of Type
    | BuiltIn
    | DateTime
    | Enum
    | Product of list<Type>
    | SerializableClass of Class
    | Struct
    | TimeSpan
    | Union of list<list<Type>>
    | Unknown

    override this.ToString() =
        match this with
        | Array x -> "Array"
        | BuiltIn -> "BuiltIn"
        | DateTime -> "DateTime"
        | Enum -> "Enum"
        | Product ts -> "Product"
        | SerializableClass c -> "Class"
        | Struct -> "Struct"
        | TimeSpan -> "TimeSpan"
        | Union _ -> "Union"
        | Unknown -> "Unknown"

and Class =
    {
        Fields: list<Type>
        HasDefaultConstructor: bool
    }

let canEncode t =
    let rec canEncode t =
        let cE = canEncode
        match t with
        | Array t -> cE t
        | BuiltIn -> true
        | DateTime -> true
        | Enum -> true
        | Product ts -> List.forall cE ts
        | SerializableClass t -> List.forall cE t.Fields
        | Struct -> false
        | TimeSpan -> true
        | Union ts -> List.forall (List.forall cE) ts
        | Unknown -> false
    canEncode t

let canDecode t =
    let rec canDecode t =
        let cD = canDecode
        match t with
        | Array t -> cD t
        | BuiltIn -> true
        | DateTime -> true
        | Enum -> true
        | Product ts -> List.forall cD ts
        | SerializableClass t ->
            t.HasDefaultConstructor
            && List.forall cD t.Fields
        | Struct -> false
        | TimeSpan -> true
        | Union ts -> List.forall (List.forall cD) ts
        | Unknown -> false
    canDecode t

type CMA = CompilationMappingAttribute
type SCF = SourceConstructFlags

let (|RecordField|_|) (p: Mono.Cecil.PropertyDefinition) =
    if p.HasCustomAttributes then
        p.CustomAttributes
        |> Seq.tryPick (fun a ->
            let xs = a.ConstructorArguments
            let fN = a.AttributeType.FullName
            if fN = typeof<CMA>.FullName && xs.Count = 2 then
                match downcast xs.[0].Value, downcast xs.[1].Value with
                | SCF.Field, (x : int) -> Some x
                | _ -> None
            else None)
    else None

let (|UnionCaseMethod|_|) (m: Mono.Cecil.MethodDefinition) =
    if m.IsStatic && m.HasCustomAttributes then
        m.CustomAttributes
        |> Seq.tryPick (fun a ->
            let xs = a.ConstructorArguments
            let fN = a.AttributeType.FullName
            if fN = typeof<CMA>.FullName && xs.Count = 2 then
                match downcast xs.[0].Value, downcast xs.[1].Value with
                | SCF.UnionCase, (x : int) -> Some x
                | _ -> None
            else None)
    else None

let convertClassType convertType (t: Mono.Cecil.TypeDefinition) =
    let hasDC =
        t.Methods
        |> Seq.exists (fun x -> x.IsConstructor && x.Parameters.Count = 0)
    let rec getFields (t: Mono.Cecil.TypeDefinition) =
        let bF =
            if t.BaseType <> null
            then getFields (t.BaseType.Resolve())
            else Seq.empty
        if t.IsSerializable then
            t.Fields
            |> Seq.filter (fun f ->
                let nS = Mono.Cecil.FieldAttributes.NotSerialized
                not f.IsStatic
                && int (f.Attributes &&& nS) = 0)
        else Seq.empty
        |> Seq.append bF
    let fields =
        getFields t
        |> Seq.map (fun f -> convertType f.FieldType)
        |> Seq.toList
    SerializableClass {
        Fields = fields
        HasDefaultConstructor = hasDC
    }

let convertRecordType convertType (t: Mono.Cecil.TypeDefinition) =
    let fields =
        t.Properties
        |> Seq.choose (fun x ->
            match x with
            | RecordField n -> Some (n, x)
            | _ -> None)
        |> Seq.sortBy fst
        |> Seq.map (fun (_, x) -> convertType x.PropertyType)
        |> Seq.toList
    Product fields

let convertUnionType convertType (t: Mono.Cecil.TypeDefinition) =
    let cases =
        t.Methods
        |> Seq.choose (fun x ->
            match x with
            | UnionCaseMethod n -> Some (n, x)
            | _ -> None)
        |> Seq.sortBy fst
        |> Seq.map (fun (_, x) ->
            x.Parameters
            |> Seq.map (fun x -> convertType x.ParameterType)
            |> Seq.toList)
        |> Seq.toList
    Union cases

let convertSpecialType convertType (t: Mono.Cecil.TypeDefinition) =
    t.CustomAttributes
    |> Seq.tryPick (fun a ->
        if a.AttributeType.FullName = typeof<CMA>.FullName then
            let v = a.ConstructorArguments.[0].Value :?> SCF
            match v &&& SCF.KindMask with
            | SCF.RecordType -> Some (convertRecordType convertType t)
            | SCF.SumType -> Some (convertUnionType convertType t)
            | SCF.ObjectType -> None
            | _ -> Some Unknown
        else None)

let convertType convertType (t: Mono.Cecil.TypeReference) =
    if t.IsArray then
        let t = t :?> Mono.Cecil.ArrayType
        if t.Rank = 1
        then Array (convertType t.ElementType)
        else Unknown
    elif t.IsPrimitive || t.FullName = "System.String" then BuiltIn
    elif t.FullName = "System.DateTime" then DateTime
    elif t.FullName = "System.TimeSpan" then TimeSpan
    else
        let tD = t.Resolve()
        if t.IsGenericParameter then convertType t
        elif tD.IsEnum then Enum
        elif tD.IsInterface then Unknown
        elif tD.IsValueType then Struct
        elif tD.FullName.StartsWith "System.Tuple" && t.IsGenericInstance then
            let t = t :?> Mono.Cecil.GenericInstanceType
            t.GenericArguments
            |> Seq.map convertType
            |> Seq.toList
            |> Product
        else
            let conv : Mono.Cecil.TypeReference -> _ =
                if t.IsGenericInstance then
                    let t = t :?> Mono.Cecil.GenericInstanceType
                    let d = System.Collections.Generic.Dictionary()
                    Seq.iter2 (fun p a -> d.[p] <- convertType a)
                        tD.GenericParameters
                        t.GenericArguments
                    fun t ->
                        if t.IsGenericParameter
                        then d.[t :?> _]
                        else convertType t
                else
                    convertType
            match convertSpecialType conv tD with
            | Some r -> r
            | None ->
                if tD.IsClass && tD.IsSerializable
                then convertClassType conv tD
                else Unknown

[<Sealed>]
type Converter() =
    let cache = System.Collections.Generic.Dictionary()

    member this.Convert t =
        match cache.TryGetValue t with
        | true, r -> r
        | _ ->
            cache.[t] <- BuiltIn
            let res = convertType this.Convert t
            cache.[t] <- res
            res

let getRemoteContractError (conv: Converter) (m: Mono.Cecil.MethodDefinition) =
    if m.DeclaringType.HasGenericParameters then
        Some "Static remote methods must be defined on non-generic types."
    elif m.HasGenericParameters then
        Some "Remote methods must not be generic."
    else
        let rT =
            let rT = m.ReturnType
            if rT.IsGenericInstance then
                let t = rT :?> Mono.Cecil.GenericInstanceType
                if rT.Namespace = "Microsoft.FSharp.Control"
                    && rT.Name.StartsWith "FSharpAsync" then
                    t.GenericArguments.[0]
                else rT
            else rT
        if rT.FullName = "System.Void"
           || canEncode (conv.Convert rT)
        then
            m.Parameters
            |> Seq.fold (fun r p ->
                match r with
                | Some _ -> r
                | None ->
                    let pT = conv.Convert p.ParameterType
                    if canDecode pT then None else
                        let msg = "Cannot decode a parameter from JSON: "
                        Some (msg + p.Name))
                None
        else Some "Cannot encode the return type to JSON."

let getWebControlError (conv: Converter) (t: Mono.Cecil.TypeDefinition) =
    if not (canEncode (conv.Convert t)) then
        Some "Cannot encode the Web.Control type to JSON."
    else
        let body =
            t.Properties
            |> Seq.tryPick (fun x ->
                let fN = "IntelliFactory.WebSharper.Web.Control"
                if x.Name = "Body"
                    && x.GetMethod <> null
                    && x.GetMethod.IsVirtual
                then Some x
                else None)
        match body with
        | None -> Some "Web.Control types must override the Body property."
        | Some p ->
            let fN = typeof<ReflectedDefinitionAttribute>.FullName
            let ok =
                p.CustomAttributes
                |> Seq.exists (fun x -> x.AttributeType.FullName = fN)
            if ok then None else
                Some "JavaScript attribute is required on the Body property."

type Status =
    | Correct
    | Incorrect of string

[<Sealed>]
type State() =
    let conv = Converter()

    member this.VerifyRemoteMethod(m: Mono.Cecil.MethodDefinition) =
        match getRemoteContractError conv m with
        | None -> Correct
        | Some msg -> Incorrect msg

    member this.VerifyWebControl(t: Mono.Cecil.TypeDefinition) =
        match getWebControlError conv t with
        | None -> Correct
        | Some msg -> Incorrect msg

let Create () = State()
