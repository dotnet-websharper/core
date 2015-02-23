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

module internal WebSharper.Compiler.Resolver

module P = WebSharper.Core.JavaScript.Packager
module R = WebSharper.Compiler.Reflector

type MethodMember   = R.Member<MethodDefinition>
type PropertyMember = R.Member<PropertyDefinition>

type Definition =
    {
        Address : R.AddressSlot
        Annotations : list<R.Annotation>
        Location : Location
        Name     : option<R.Name>
    }

type Env =
    {
        Logger : Logger
    }

    member this.Warn x =
        let k s =
            this.Logger.Log {
                Text = s
                Location = { ReadableLocation = ""; SourceLocation = None }
                Priority = Priority.Warning
            }
        Printf.ksprintf k x

type M<'T>       = Map<string,'T>
type ProtoMember = Definition * R.MemberSlot
type D           = Definition

type Mode =
    | CompileAlways
    | CompileIfNecessary

type Member =
    | Class of D * option<R.ClassSlot> * M<ProtoMember> * M<Member> * Mode
    | Member of D * R.MemberSlot
    | Module of D * M<Member>
    | Package of M<Member>

let fresh ok x =
    let fmt n =
        match n with
        | 0 -> x
        | n -> System.String.Format("{0}{1:x}", x, n)
    let rec loop i =
        let n = fmt i
        if ok n then n else loop (i + 1)
    loop 0

let gen x m =
    fresh (fun x -> not (Map.containsKey x m)) x

let rec merge a b =
    (a, b)
    ||> Map.fold (fun m k x ->
        match Map.tryFind k m with
        | None   -> Map.add k x m
        | Some y ->
            match x, y with
            | Package m1, Package m2 ->
                Map.add k (Package (merge m1 m2)) m
            | Package mo, Class (x, y, cl, st, z)
            | Class (x, y, cl, st, z), Package mo ->
                Map.add k (Class (x, y, cl, merge st mo, z)) m
            | Package pkg, Module (x, mo)
            | Module (x, mo), Package pkg ->
                Map.add k (Module (x, merge mo pkg)) m
            | _ ->
                let ok x = not (Map.containsKey x m || Map.containsKey x b)
                Map.add (fresh ok k) x m)

let annot annots =
    List.tryPick (function R.Name x -> Some x | _ -> None) annots

let isStub annots =
    List.exists (function R.Stub -> true | _ -> false) annots

let addr ctx name annot =
    let ( / ) a b =
        match a with
        | None   -> P.Global b
        | Some a -> P.Local (a, b)
    match annot with
    | None                       -> ctx / name
    | Some (R.RelativeName x)    -> ctx / x
    | Some (R.AbsoluteName addr) -> addr

let name name annot =
    match annot with
    | None                       -> name
    | Some (R.RelativeName x)    -> x
    | Some (R.AbsoluteName addr) -> addr.LocalName

let rec pack addr value =
    let ( => ) k v = Map.add k v Map.empty
    match addr with
    | P.Global name        -> name => value
    | P.Local (addr, name) -> pack addr (Package (name => value))

let isStatic (p: R.Property) =
    p.Member.Definition.IsStatic

let recStaticMethod ctx acc (m: MethodMember) =
    let def = m.Definition
    let a   = m.Annotations
    let x   =
        {
            Address  = m.AddressSlot
            Annotations = m.Annotations
            Name  = annot a
            Location = m.Location
        }
    let mem = Member (x, m.MemberSlot)
    if def.IsConstructor then
        pack (addr (Some ctx) "New" x.Name) mem :: acc
    elif def.IsStatic then
        pack (addr (Some ctx) def.Name x.Name) mem :: acc
    else
        acc

let recStaticProperty ctx acc (p: R.Property) =
    let def = p.Member.Definition
    if isStatic p then
        let a    = p.Member.Annotations
        let x    = { Address  = p.Member.AddressSlot
                     Name     = annot a
                     Location = p.Member.Location
                     Annotations = a }
        let addr = addr (Some ctx) def.Name x.Name
        let add (x: option<MethodMember>) acc =
            match x with
            | None   -> acc
            | Some x ->
                let p = if x.Definition.IsGetter then "get_" else "set_"
                let a =
                    match addr with
                    | P.Global n     -> P.Global (p + n)
                    | P.Local (a, n) -> P.Local (a, p + n)
                let y =
                    {
                        Address = x.AddressSlot
                        Annotations = x.Annotations
                        Name = None
                        Location = x.Location
                    }
                pack a (Member (y, x.MemberSlot)) :: acc
        pack addr (Member (x, p.Member.MemberSlot)) :: acc
        |> add p.Getter
        |> add p.Setter
    else
        acc

let recInstanceProperty (acc: Map<_,_>) (p: R.Property) =
    let def = p.Member.Definition
    if not (isStatic p) then
        let a = p.Member.Annotations
        let x =
            {
                Annotations = p.Member.Annotations
                Address = p.Member.AddressSlot
                Name = annot a
                Location = p.Member.Location
            }
        let n = name def.Name x.Name
        let add (x: option<MethodMember>) (acc: Map<_,_>) =
            match x with
            | None   -> acc
            | Some x ->
                let nm = annot x.Annotations
                let n =
                    if not x.Definition.Overrides.IsEmpty then
                        x.Definition.Overrides.[0].Name
                    else
                        name x.Definition.Name nm
                let y =
                    {
                        Address  = x.AddressSlot
                        Annotations  = x.Annotations
                        Location = x.Location
                        Name = nm
                    }
                Map.add (gen n acc) (y, x.MemberSlot) acc
        Map.add (gen n acc) (x, p.Member.MemberSlot) acc
        |> add p.Getter
        |> add p.Setter
    else
        acc

let recRecordField (acc: Map<_,_>) (p: PropertyMember) =
    let def = p.Definition
    let x =
        {
            Address = p.AddressSlot
            Annotations = p.Annotations
            Name = annot p.Annotations
            Location = p.Location
        }
    let n   = name def.Name x.Name
    Map.add (gen n acc) (x, p.MemberSlot) acc

let recUnionCase (acc: Map<_,_>) (p: R.UnionCase) =
    let def = p.Member.Definition
    let x =
        {
            Address  = p.Member.AddressSlot
            Annotations = p.Member.Annotations
            Name = annot p.Member.Annotations
            Location = p.Member.Location
        }
    let n   = name p.Name x.Name
    Map.add (gen n acc) (x, p.Member.MemberSlot) acc

let getNormalizedName (env: Env) (m: MethodReference) =
    if m.Name = "ToString"
        then "toString"
        else m.Name

let recInstanceMethod env (acc: Map<_,_>) (m: MethodMember) =
    let def = m.Definition
    if not def.IsConstructor && not def.IsStatic then
        let x =
            {
                Address = m.AddressSlot
                Annotations = m.Annotations
                Name = annot m.Annotations
                Location = m.Location
            }
        let n =
            x.Name
            |> name (if def.Overrides.Length = 1
                        then getNormalizedName env def.Overrides.[0]
                        else getNormalizedName env def)
        Map.add (gen n acc) (x, m.MemberSlot) acc
    else
        acc

let cleanTypeName (s: string) =
    match s.LastIndexOf('`') with
    | -1 -> s
    | k -> s.Substring(0, k)

let rec recType env ctx acc (t: R.Type) =
    let x =
        {
            Address = t.AddressSlot
            Annotations = t.Annotations
            Name = annot t.Annotations
            Location = t.Location
        }
    let ctx   =
        match ctx, t.Definition.Namespace with
        | None, null | None, "" -> None
        | None, ns ->
            Array.map P.Global (ns.Split '.')
            |> Array.reduce (fun x y -> P.Local (x, y.LocalName))
            |> Some
        | _ -> ctx
    let ctx = addr ctx (cleanTypeName t.Definition.Name) x.Name
    let acc = List.fold (recStaticProperty ctx) acc t.Properties
    let acc = List.fold (recStaticMethod ctx) acc t.Methods
    let acc = List.fold (recType env (Some ctx)) acc t.Nested
    let proto = Map.empty
    let proto = List.fold recInstanceProperty proto t.Properties
    let proto = List.fold (recInstanceMethod env) proto t.Methods
    let cs = match t.Kind with R.Class x -> Some x | _ -> None
    let c y z = pack ctx (Class (x, cs, y, Map.empty, z)) :: acc
    match t.Kind with
    | R.Enum ->
        acc
    | R.Module ->
        pack ctx (Module (x, Map.empty)) :: acc
    | R.Class _ | R.Interface ->
        c proto CompileIfNecessary
    | R.Exception ->
        c proto CompileAlways
    | R.Record fields ->
        c (List.fold recRecordField proto fields) CompileIfNecessary
    | R.Union cases ->
        c (List.fold recUnionCase proto cases) CompileIfNecessary

let reduceBalanced f z input =
    let rec reduce s t =
        if s + 1 >= t then Array.get input s
        else let m = (s + t) / 2
             f (reduce s m) (reduce m t)
    match Array.length input with
    | 0 -> z
    | _ -> reduce 0 (Array.length input)

let recAssembly env (assembly: R.Assembly) =
    List.fold (recType env None) [] assembly.Types
    |> List.toArray
    |> reduceBalanced merge Map.empty

let rec visit vD ctx m =
    let ( / ) a b = P.Local (a, b)
    let loop pkg = Map.iter (fun k v -> visit vD (ctx / k) v) pkg
    match m with
    | Class (d, slot, proto, st, z) ->
        vD ctx d
        Map.iter (fun k (v, _) -> vD (ctx / "prototype" / k) v) proto
        loop st
    | Member (d, slot) ->
        vD ctx d
    | Module (d, st) ->
        vD ctx d
        loop st
    | Package st ->
        loop st

let rec buildPackage (pkg: M<Member>) : P.Module =
    Map.toSeq pkg
    |> Seq.choose (fun (k, v) ->
        buildBinding v
        |> Option.map (fun v -> (k, v)))
    |> Map.ofSeq

and buildBinding (m: Member) : option<P.Binding> =
    match m with
    | Member.Member (d, s) -> Option.map P.Binding.Member s.Member
    | Member.Class (d, s, p, m, z) ->
        let proto =
            p
            |> Map.filter (fun _ (_, s) -> s.Member.IsSome)
            |> Map.map (fun k (_, s) -> s.Member.Value)
        let st = buildPackage m
        match z with
        | CompileIfNecessary when Map.isEmpty proto && Map.isEmpty st ->
            None
        | _ ->
            P.Binding.Class {
                Base      = s |> Option.bind (fun x -> x.BaseType)
                Prototype = proto
                Static    = st
            }
            |> Some
    | Member.Module (_, m) | Member.Package m ->
        if Map.isEmpty m then None else
            Some (P.Binding.Module (buildPackage m))

let Resolve (logger: Logger) (assembly: R.Assembly) =
    let env = { Logger = logger }
    let pkg = recAssembly env assembly
    let vD ctx (d: Definition) =
        if isStub d.Annotations then
            let addr =
                d.Annotations
                |> List.tryPick (function
                    | R.Annotation.Name (R.AbsoluteName addr) -> Some addr
                    | _ -> None)
            d.Address.Address <-
                match addr with
                | None -> ctx
                | Some addr -> addr
        else
            d.Address.Address <- ctx
            let fmt (a: obj) (b: obj) =
                System.String.Format("Name conflict: renamed {0} to {1}.", a, b)
            match d.Name with
            | None -> ()
            | Some (R.RelativeName name) ->
                if name <> ctx.LocalName then
                    logger.Log {
                        Text     = fmt name ctx.LocalName
                        Location = d.Location
                        Priority = Warning
                    }
            | Some (R.AbsoluteName abs) ->
                if abs <> ctx then
                    logger.Log {
                        Text     = fmt abs ctx
                        Location = d.Location
                        Priority = Warning
                    }
    Map.iter (fun k v -> visit vD (P.Global k) v) pkg
    lazy P.Simplify (buildPackage pkg)
