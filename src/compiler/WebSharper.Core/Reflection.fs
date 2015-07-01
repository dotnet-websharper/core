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

module WebSharper.Core.Reflection

type FullName = string
type Namespace = string
type AssemblyQualifiedName = string
type Count = int
type Name = string
type Position = int
type Rank = int

let inline ( ++ ) (a: int) (b: int) =
    (a <<< 5) + a + b

[<NoComparison>]
[<CustomEquality>]
type AssemblyName =
    {
        full : string
    }

    static member Convert(n: System.Reflection.AssemblyName) =
        {full = n.FullName}

    override this.Equals other =
        match other with
        | :? AssemblyName as o -> this.Name = o.Name
        | _ -> false

    static member FromAssembly(a: System.Reflection.Assembly) =
        {full = a.FullName}

    member this.FullName =
        this.full

    override this.GetHashCode() =
        hash this.Name

    member this.Name =
        match this.full.IndexOf ',' with
        | -1 -> this.full
        | k -> this.full.Substring(0, k)

    static member Parse name =
        {full = name}

    override this.ToString() =
        this.full

let getGenerics (name: string) =
    match name.LastIndexOf '`' with
    | -1 -> 0
    | k -> int (name.Substring (k + 1))

type Hash = int

[<NoComparison>]
[<CustomEquality>]
type TypeDefinition =
    | NestedTD of TypeDefinition * Name * Hash
    | RootTD of AssemblyName * Namespace * Name * Hash

    member this.Encode(w: System.IO.BinaryWriter) =
        match this with
        | NestedTD (p, name, _) ->
            w.Write 0uy
            p.Encode w
            w.Write name
        | RootTD (aN, ns, name, _) ->
            w.Write 1uy
            w.Write aN.full
            w.Write ns
            w.Write name

    static member Decode(r: System.IO.BinaryReader) =
        match r.ReadByte() with
        | 0uy ->
            let p = TypeDefinition.Decode r
            let name = r.ReadString()
            TypeDefinition.CreateNested p name
        | _ ->
            let full = r.ReadString()
            let aN = {full = full}
            let ns = r.ReadString()
            let tn = r.ReadString()
            TypeDefinition.Create aN ns tn

    member this.GenericsCount =
        getGenerics this.Name

    override this.GetHashCode() =
        match this with
        | NestedTD (_, _, h)
        | RootTD (_, _, _, h) -> h

    override this.Equals other =
        match other with
        | :? TypeDefinition as o ->
            match this, o with
            | NestedTD (td1, n1, h1), NestedTD (td2, n2, h2) ->
                h1 = h1 && n1 = n2 && td1 = td2
            | RootTD (_, ns1, n1, h1), RootTD (_, ns2, n2, h2) ->
                h1 = h2 && ns1 = ns2 && n1 = n2
            | _ -> false
        | _ -> false

    member this.AssemblyName =
        match this with
        | NestedTD (tD, _, _) -> tD.AssemblyName
        | RootTD (n, _, _, _) -> n

    member this.DeclaringType =
        match this with
        | NestedTD (tD, _, _) -> Some tD
        | RootTD _ -> None

    member this.Namespace =
        match this with
        | NestedTD (tD, _, _) -> tD.Namespace
        | RootTD (_, null, _, _) -> None
        | RootTD (_, ns, _, _) -> Some ns

    member this.Name =
        match this with
        | NestedTD (_, n, _)
        | RootTD (_, _, n, _) -> n

    member this.FullName =
        match this with
        | NestedTD (dT, n, _) ->
            System.String.Format("{0}+{1}", dT.FullName, n)
        | RootTD (_, ns, n, _) ->
            match ns with
            | null -> n
            | ns -> System.String.Format("{0}.{1}", ns, n)

    member this.Address =
        let clean (s: string) =
            match s.LastIndexOf '`' with
            | -1 -> s
            | i -> s.[..i-1]
        let rec address = function
            | NestedTD (dT, n, _) -> clean n :: address dT
            | RootTD (_, ns, n, _) ->
                clean n :: List.rev (List.ofArray (ns.Split([|'.'|], System.StringSplitOptions.RemoveEmptyEntries)))
        List.rev (address this)

    member this.DeclaringAddress =
        match this with
        | NestedTD (dT, _, _) -> dT.Address
        | RootTD (_, ns, _, _) -> List.ofArray (ns.Split([|'.'|], System.StringSplitOptions.RemoveEmptyEntries))

    member this.AssemblyQualifiedName =
         System.String.Format("{0}, {1}", this.FullName, this.AssemblyName)

    override this.ToString() =
        this.FullName

    member this.Load() =
        System.Type.GetType(this.AssemblyQualifiedName, true)

    static member FromType(t: System.Type) =
        let t =
            if t.IsGenericType && not t.IsGenericTypeDefinition then
                t.GetGenericTypeDefinition()
            else
                t
        match t.DeclaringType with
        | null ->
            let aN = AssemblyName.FromAssembly t.Assembly
            TypeDefinition.Create aN t.Namespace t.Name
        | dT ->
            let p = TypeDefinition.FromType dT
            TypeDefinition.CreateNested p t.Name

    static member Create aN ns n =
        let ns = if ns = null then "" else ns
        let h = 1 ++ hash n ++ hash ns
        RootTD (aN, ns, n, h)

    static member CreateNested p n =
        let h = 2 ++ hash n ++ hash p
        NestedTD (p, n, h)

    static member Parse(aQN: string) =
        let comma = aQN.IndexOf ','
        let aN = aQN.Substring(comma + 1).TrimStart()
        let rec pN (name: string) =
            match name.LastIndexOf '+' with
            | -1 ->
                match name.LastIndexOf '.' with
                | -1 -> TypeDefinition.Create {full=aN} null name
                | k ->
                    TypeDefinition.Create {full=aN}
                        (name.Substring(0, k))
                        (name.Substring(k + 1))
            | k ->
                TypeDefinition.CreateNested
                    (pN (name.Substring(0, k)))
                    (name.Substring(k + 1))
        pN (aQN.Substring(0, comma))

exception InvalidType of message: string
    with override this.Message = this.message

type Type =
    | ArrayType of Type * Rank
    | ConcreteType of TypeDefinition * list<Type>
    | GenericType of Position

    member this.Encode(w: System.IO.BinaryWriter) =
        match this with
        | ArrayType (t, r) ->
            w.Write 0uy
            t.Encode w
            w.Write r
        | ConcreteType (tD, ts) ->
            w.Write 1uy
            tD.Encode w
            w.Write ts.Length
            for t in ts do
                t.Encode w
        | GenericType k ->
            w.Write 2uy
            w.Write k

    static member Decode(r: System.IO.BinaryReader) =
        match r.ReadByte() with
        | 0uy ->
            let t = Type.Decode r
            let r = r.ReadInt32()
            ArrayType (t, r)
        | 1uy ->
            let tD = TypeDefinition.Decode r
            let ts = List.init (r.ReadInt32()) (fun _ -> Type.Decode r)
            ConcreteType (tD, ts)
        | _ ->
            let k = r.ReadInt32()
            GenericType k

    member this.DeclaringType =
        match this with
        | ArrayType (t, _) -> t.DeclaringType
        | ConcreteType (t, _) -> t
        | GenericType _ -> raise (InvalidType "Cannot get type of a generic parameter")

    member this.Name =
        match this with
        | ArrayType (t, rank) ->
            System.String.Format(
                "{0}[{1}]",
                t.Name,
                match rank with
                | 1 -> ""
                | k -> "".PadLeft(k - 1, ',')
            )
        | ConcreteType (d, _) ->
            d.Name
        | GenericType p ->
            System.String.Format("T{0}", p + 1)

    member this.FullName =
        match this with
        | ArrayType (t, rank) ->
            System.String.Format(
                "{0}[{1}]",
                t.FullName,
                match rank with
                | 1 -> ""
                | k -> "".PadLeft(k - 1, ',')
            )
        | ConcreteType (d, []) ->
            d.FullName
        | ConcreteType (d, ts) ->
            let ts =
                ts
                |> Seq.map (fun t -> t.AssemblyQualifiedName)
                |> String.concat "],["
            System.String.Format("{0}[[{1}]]", d.FullName, ts)
        | GenericType p -> "System.Object"

    override this.ToString() =
        let fullName t =
            match t with
            | GenericType _ -> t.Name
            | _ -> t.FullName
        match this with
        | ArrayType (t, rank) ->
            System.String.Format(
                "{0}[{1}]",
                fullName t,
                match rank with
                | 1 -> ""
                | k -> "".PadLeft(k - 1, ',')
            )
        | ConcreteType (d, []) ->
            d.FullName
        | ConcreteType (d, ts) ->
            let ts =
                ts
                |> Seq.map string
                |> String.concat "],["
            System.String.Format("{0}[[{1}]]", d.FullName, ts)
        | GenericType p ->
            this.Name

    member private this.AssemblyName =
        match this with
        | ArrayType (t, _) -> t.AssemblyName
        | ConcreteType (t, _) -> string t.AssemblyName
        | GenericType _ -> "mscorlib, Version=4.0.0.0, Culture=neutral, PublicKeyToken=b77a5c561934e089"

    member this.AssemblyQualifiedName =
        System.String.Format(
            "{0}, {1}",
            this.FullName,
            this.AssemblyName
        )

    member private this.CheckNonGeneric() =
        match this with
        | ArrayType (t, _) -> t.CheckNonGeneric()
        | ConcreteType (_, ts) ->
            for t in ts do t.CheckNonGeneric()
        | GenericType _ ->
            raise (InvalidType "Cannot load generic type")    
    
    member this.Load(?allowGeneric) =
        if allowGeneric.IsNone || not allowGeneric.Value then
            this.CheckNonGeneric()
        System.Type.GetType(this.AssemblyQualifiedName, true)

    static member FromType(t: System.Type) =
        if t.IsArray then
            ArrayType (Type.FromType(t.GetElementType()), t.GetArrayRank())
        elif t.IsGenericParameter then
            if t.DeclaringMethod <> null then
                let dT = t.DeclaringType
                let k =
                    if dT.IsGenericType then 0 else
                        dT.GetGenericArguments().Length
                GenericType (k + t.GenericParameterPosition)
            else
                GenericType t.GenericParameterPosition
        else
            let ts =
                if t.IsGenericType then
                    t.GetGenericArguments()
                    |> Seq.map Type.FromType
                    |> Seq.toList
                else
                    []
            ConcreteType (TypeDefinition.FromType t, ts)

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Type =
    let Array (t, r) = ArrayType (t, r)
    let Concrete (t, ts) = ConcreteType (t, ts)
    let Generic p = GenericType p

    let (|Array|Concrete|Generic|) x =
        match x with
        | ArrayType (t, r) -> Array (t, r)
        | ConcreteType (t, ts) -> Concrete (t, ts)
        | GenericType p -> Generic p

type Generics = list<Type>
type Signature = list<Type>

let Flags =
    System.Reflection.BindingFlags.Instance
    ||| System.Reflection.BindingFlags.Static
    ||| System.Reflection.BindingFlags.Public
    ||| System.Reflection.BindingFlags.NonPublic

exception MethodBindingException

[<CustomEquality>]
[<NoComparison>]
type Method =
    | Method of TypeDefinition * Name * Count * Signature * Type * Hash
    | MethodReference of TypeDefinition * Name * Hash

    member this.Encode(w: System.IO.BinaryWriter) =
        match this with
        | Method (tD, n, c, si, rt, _) ->
            w.Write 0uy
            tD.Encode w
            w.Write n
            w.Write c
            w.Write si.Length
            for e in si do
                e.Encode w
            rt.Encode w
        | MethodReference (tD, n, _) ->
            w.Write 1uy
            tD.Encode w
            w.Write n

    static member Decode(r: System.IO.BinaryReader) =
        match r.ReadByte() with
        | 0uy ->
            let tD = TypeDefinition.Decode r
            let n = r.ReadString()
            let c = r.ReadInt32()
            let si = List.init (r.ReadInt32()) (fun _ -> Type.Decode r)
            let rT = Type.Decode r
            Method.Create tD n c si rT
        | _ ->
            let tD = TypeDefinition.Decode r
            let n = r.ReadString()
            Method.CreateReference tD n

    override this.GetHashCode() =
        match this with
        | MethodReference (_, _, h)
        | Method (_, _, _, _, _, h) -> h

    override this.Equals other =
        match other with
        | :? Method as o ->
            match this, o with
            | MethodReference _, _
            | _, MethodReference _ ->
                this.GetHashCode() = o.GetHashCode()
                && this.Name = o.Name
                && this.DeclaringType = o.DeclaringType
            | Method (d1, n1, c1, s1, t1, h1), Method (d2, n2, c2, s2, t2, h2) ->
                h1 = h2 && n1 = n2 && d1 = d2 && c1 = c2 && s1 = s2 && t1 = t2
        | _ -> false

    member this.DeclaringType =
        match this with
        | Method (d, _, _, _, _, _)
        | MethodReference (d, _, _) -> d

    member this.WithDeclaringType dt =
        match this with
        | Method (_, a, b, c, d, _) -> Method.Create dt a b c d
        | MethodReference (_, a, _) -> Method.CreateReference dt a

    member this.Name =
        match this with
        | Method (_, n, _, _, _, _)
        | MethodReference (_, n, _) -> n

    static member Create d n (c: Count) s r =
        Method (d, n, c, s, r, hash d ++ hash n)

    static member CreateReference d n =
        MethodReference (d, n, hash d ++ hash n)

    static member Parse(m: System.Reflection.MethodInfo) =
        let m = m.Module.ResolveMethod m.MetadataToken :?> System.Reflection.MethodInfo
        let c = if m.IsGenericMethod then m.GetGenericArguments().Length else 0
        let s = [for p in m.GetParameters() -> Type.FromType p.ParameterType]
        let r = Type.FromType m.ReturnParameter.ParameterType
        let d = TypeDefinition.FromType m.DeclaringType
        Method.Create d m.Name c s r

    member this.Load(generics: Generics option) =
        let typeGenerics =
            generics
            |> Option.map (fun g ->
                Seq.take this.DeclaringType.GenericsCount g
                |> Seq.toList)
        let methodGenerics =
            generics
            |> Option.map (fun g ->
                Seq.skip this.DeclaringType.GenericsCount g
                |> Seq.toList)
        let ty =
            if typeGenerics.IsSome then
                ConcreteType(this.DeclaringType, typeGenerics.Value).Load()
            else
                this.DeclaringType.Load()
        let m =
            try
                match ty.GetMethod(this.Name, Flags) with
                | null -> raise MethodBindingException
                | m -> m
            with :? System.Reflection.AmbiguousMatchException ->
                let m =
                    ty.GetMethods Flags
                    |> Seq.tryFind (fun m ->
                        m.Name = this.Name
                        && Method.Parse m = this)
                match m with
                | None -> raise MethodBindingException
                | Some m -> m
        if methodGenerics.IsSome then
            m.MakeGenericMethod [| for t in methodGenerics.Value -> t.Load() |]
        else
            m

    override this.ToString() =
        System.String.Format("{0}(..) [{1}]",
            this.Name, this.DeclaringType)

type Constructor =
    | Constructor of TypeDefinition * Signature

    member this.DeclaringType =
        match this with
        | Constructor (dT, s) -> dT

    member this.WithDeclaringType d =
        match this with
        | Constructor (_, s) -> Constructor (d, s)

    static member Create dT s = Constructor (dT, s)

    static member Parse(m: System.Reflection.ConstructorInfo) =
        let m = m.Module.ResolveMethod m.MetadataToken :?> System.Reflection.ConstructorInfo
        let s = [for p in m.GetParameters() -> Type.FromType p.ParameterType]
        let d = TypeDefinition.FromType m.DeclaringType
        Constructor.Create d s

    override this.ToString() =
        System.String.Format(".ctor(..) [{0}]", this.DeclaringType)

[<CustomEquality>]
[<NoComparison>]
type Property =
    | PropertyReference of TypeDefinition * Name
    | Property of TypeDefinition * Name * Type * Signature

    override this.GetHashCode() =
        match this with
        | PropertyReference (t, n)
        | Property (t, n, _, _) -> hash t ++ hash n

    override this.Equals other =
        match other with
        | :? Property as o ->
            this.Name = o.Name
            && this.DeclaringType = o.DeclaringType
            &&  match this, o with
                | PropertyReference _, _ | _, PropertyReference _ ->
                    true
                | Property (_, _, s1, t1),
                  Property (_, _, s2, t2) ->
                    s1 = s2 && t1 = t2
        | _ ->
            false

    member this.DeclaringType =
        match this with
        | PropertyReference (d, _)
        | Property (d, _, _, _) -> d

    member this.WithDeclaringType d =
        match this with
        | PropertyReference (_, a) -> PropertyReference (d, a)
        | Property (_, a, b, c) -> Property (d, a, b, c)

    member this.Name =
        match this with
        | PropertyReference (_, n) | Property (_, n, _, _) -> n

    static member Create d n t s = Property (d, n, t, s)
    static member CreateReference d n = PropertyReference (d, n)

    static member Parse(p: System.Reflection.PropertyInfo) =
        let s = [for p in p.GetGetMethod().GetParameters() -> Type.FromType p.ParameterType]
        let t = Type.FromType p.PropertyType
        let d = TypeDefinition.FromType p.DeclaringType
        Property.Create d p.Name t s

    override this.ToString() =
        System.String.Format("{0} [{1}]", this.Name, this.DeclaringType)

type Field =
    | Field of TypeDefinition * Name

    member this.DeclaringType = match this with Field (d, _) -> d
    member this.Name = match this with Field (_, n) -> n
    static member Create d n = Field (d, n)

    member this.WithDeclaringType d =
        match this with
        | Field (_, n) -> Field (d, n)

    override this.ToString() =
        System.String.Format("{0} [{1}]", this.Name, this.DeclaringType)

type UnionCase =
    | UnionCase of TypeDefinition * Name

    member this.DeclaringType = match this with UnionCase (d, _) -> d
    member this.Name = match this with UnionCase (_, n) -> n
    static member Create d n = UnionCase (d, n)

    member this.WithDeclaringType d =
        match this with
        | UnionCase (_, n) -> UnionCase (d, n)

    override this.ToString() =
        System.String.Format("{0} [{1}]", this.Name, this.DeclaringType)
