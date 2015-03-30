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

module WebSharper.Compiler.Metadata

module B = WebSharper.Core.Binary
module C = WebSharper.Core.JavaScript.Core
module L = WebSharper.Compiler.Locator
module M = WebSharper.Core.Metadata
module P = WebSharper.Core.JavaScript.Packager
module Q = WebSharper.Core.Quotations
module R = WebSharper.Core.Reflection
module Re = WebSharper.Core.Remoting
module V = WebSharper.Compiler.Validator

type Field = string
type Position = int
type Dictionary<'T1,'T2> = System.Collections.Generic.Dictionary<'T1,'T2>
type HashSet<'T> = System.Collections.Generic.HashSet<'T>

type ConstructorKind =
    | BasicConstructor of P.Address
    | InlineConstructor of Inlining.Inline
    | MacroConstructor of R.Type * ConstructorKind option
    | StubConstructor of P.Address

type DataTypeKind =
    | Class of P.Address * list<Field> * list<string * Field * bool>
    | Exception of P.Address
    | Interface of P.Address
    | Object of list<string * Field * bool>
    | Record of P.Address * list<string * Field * bool>

type MethodKind =
    | BasicInstanceMethod of Name
    | BasicStaticMethod of P.Address
    | InlineMethod of Inlining.Inline
    | MacroMethod of R.Type * MethodKind option
    | RemoteMethod of MemberScope * V.RemotingKind * M.MethodHandle

type PropertyKind =
    | BasicProperty of option<MethodKind> * option<MethodKind>
    | InstanceOptProperty of Name
    | StaticOptProperty of P.Address
    | FieldProperty of int
    | InstanceStubProperty of Name
    | InterfaceProperty of Name
    | StaticStubProperty of P.Address

type UnionCaseKind =
    | BasicUnionCase of Position
    | CompiledUnionCase of P.Address * Position
    | ConstantUnionCase of Value

type Code = string
type Param = string

exception InvalidMetadata

let inline Get (d: Dictionary<_,_>) key =
    match d.TryGetValue key with
    | true, value -> Some value
    | _ -> None

type T =
    {
        constructors : Dictionary<R.Constructor,ConstructorKind>
        datatypes : Dictionary<R.TypeDefinition,DataTypeKind>
        methods : Dictionary<R.Method,MethodKind>
        properties : Dictionary<R.Property,PropertyKind>
        proxies : Dictionary<R.TypeDefinition,R.TypeDefinition>
        unions : Dictionary<R.UnionCase,UnionCaseKind>
    }

    member this.Proxy(t: R.TypeDefinition) =
        match this.proxies.TryGetValue t with
        | true, t -> t
        | _ -> t

    member this.Constructor (c: R.Constructor) =
        c.WithDeclaringType (this.Proxy c.DeclaringType)
        |> Get this.constructors

    member this.DataType t =
        Get this.datatypes (this.Proxy t)

    member this.Method (m: R.Method) =
        m.WithDeclaringType (this.Proxy m.DeclaringType)
        |> Get this.methods

    member this.Property (p: R.Property) =
        p.WithDeclaringType (this.Proxy p.DeclaringType)
        |> Get this.properties

    member this.UnionCase (c: R.UnionCase) =
        c.WithDeclaringType (this.Proxy c.DeclaringType)
        |> Get this.unions

    member this.Field (f: R.Field) =
        this.Proxy f.DeclaringType
        |> Get this.datatypes
        |> function
        | Some (Class (_, _, renames)) ->
            let n = f.Name
            let rn = renames |> List.tryPick (fun (on, rn, opt) -> if on = n then Some (rn, opt) else None) 
            match rn with Some rn -> rn | _ -> n, false
        | _ -> f.Name, false

    static member Empty : T =
        {
            constructors = Dictionary()
            datatypes = Dictionary()
            methods = Dictionary()
            properties = Dictionary()
            proxies = Dictionary()
            unions = Dictionary()
        }

let Parse (logger: Logger) (assembly: Validator.Assembly) : T =

    let t =
        {
            constructors = Dictionary()
            datatypes = Dictionary()
            methods = Dictionary()
            properties = Dictionary()
            proxies = Dictionary()
            unions = Dictionary()
        }

    let Log p loc text =
        logger.Log {
            Location = loc
            Priority = p
            Text = text
        }

    let Warn = Log Warning

    let inline (|Compiled|Inlined|) (i: Inlining.Inline) =
        if i.IsTransformer then Inlined i else Compiled

    let ParseConstructor (c: V.Constructor) =
        let inline f x = t.constructors.[c.Reference] <- x
        let rec parse kind =
            match kind with
            | V.JavaScriptConstructor _
            | V.InlineConstructor Compiled ->
                BasicConstructor c.Name
            | V.InlineConstructor (Inlined i) ->
                InlineConstructor i
            | V.MacroConstructor (t, _, b) ->
                MacroConstructor (t, b |> Option.map parse)
            | V.StubConstructor n ->
                StubConstructor n
        parse c.Kind |> f

    let ConvertMethod (m: V.Method) =
        let rec convert kind =
            match kind with
            | V.InlineMethod Compiled
            | V.JavaScriptMethod _
            | V.StubMethod ->
                match m.Scope with
                | Static -> BasicStaticMethod m.Name
                | Instance -> BasicInstanceMethod m.Name.LocalName
            | V.RemoteMethod (kind, ref) ->
                match !ref with
                | Some x -> RemoteMethod (m.Scope, kind, x)
                | None -> failwith "Unexpected remote method problem."
            | V.InlineMethod (Inlined i) ->
                InlineMethod i
            | V.MacroMethod (t, _, b) ->
                MacroMethod (t, b |> Option.map convert)
        convert m.Kind

    let ParseMethod (m: V.Method) =
        t.methods.[m.Reference] <- ConvertMethod m

    let ParseProperty (p: V.Property) =
        t.properties.[p.Reference] <-
            match p.Kind with
            | V.BasicProperty (getter, setter) ->
                let m x = Option.map ConvertMethod x
                BasicProperty (m getter, m setter)
            | V.OptionalProperty ->
                match p.Scope with
                | Instance -> InstanceOptProperty p.Name.LocalName
                | Static -> StaticOptProperty p.Name
            | V.FieldProperty x ->
                FieldProperty x
            | V.InlineModuleProperty i ->
                let m =
                    if i.IsTransformer
                    then InlineMethod i
                    else BasicStaticMethod p.Name
                BasicProperty (Some m, None)
            | V.InterfaceProperty ->
                InterfaceProperty p.Name.LocalName
            | V.JavaScriptModuleProperty _ ->
                BasicProperty (Some (BasicStaticMethod p.Name), None)
            | V.StubProperty ->
                match p.Scope with
                | Instance -> InstanceStubProperty p.Name.LocalName
                | Static -> StaticStubProperty p.Name

    let rec ParseType (ty: V.Type) =
        let self = ty.Reference
        match ty.Proxy with
        | Some origin -> t.proxies.[origin] <- self
        | _ -> ()
        match ty.Kind with
        | V.Resource _ -> ()
        | V.Class c ->
            match ty.Status with
            | V.Compiled -> t.datatypes.[self] <- Class (ty.Name, c.Fields, c.FieldRenames)
            | V.Ignored  -> t.datatypes.[self] <- Interface ty.Name
            List.iter ParseConstructor c.Constructors
            List.iter ParseMethod ty.Methods
            List.iter ParseProperty ty.Properties
            List.iter ParseType c.Nested
        | V.Interface ->
            t.datatypes.[self] <- Interface ty.Name
            List.iter ParseMethod ty.Methods
            List.iter ParseProperty ty.Properties
        | V.Exception ->
            t.datatypes.[self] <- Exception ty.Name
            List.iter ParseMethod ty.Methods
            List.iter ParseProperty ty.Properties
        | V.Module nested ->
            List.iter ParseMethod ty.Methods
            List.iter ParseProperty ty.Properties
            List.iter ParseType nested
        | V.Record fields ->
            t.datatypes.[self] <-
                match ty.Status with
                | V.Compiled -> Record (ty.Name, [for f in fields -> (f.OriginalName, f.JavaScriptName, f.OptionalField)])
                | V.Ignored  -> Object [for f in fields -> (f.OriginalName, f.JavaScriptName, f.OptionalField)]
            List.iter ParseMethod ty.Methods
            List.iter ParseProperty ty.Properties
        | V.Union cases ->
            match ty.Status with
            | V.Compiled -> t.datatypes.[self] <- Class (ty.Name, [], [])
            | V.Ignored -> t.datatypes.[self] <- Interface ty.Name
            cases
            |> List.iteri (fun i c ->
                t.unions.[c.Reference] <-
                    match ty.Status, c.Kind with
                    | V.Compiled, V.BasicUnionCase ->
                        CompiledUnionCase (ty.Name, i)
                    | V.Ignored, V.BasicUnionCase ->
                        BasicUnionCase i
                    | _, V.ConstantUnionCase value ->
                        ConstantUnionCase value)
            List.iter ParseMethod ty.Methods
            List.iter ParseProperty ty.Properties

    List.iter ParseType assembly.Types
    t

let Union (logger: Logger) (ts: seq<T>) =
    let r =
        {
            constructors = Dictionary()
            datatypes = Dictionary()
            methods = Dictionary()
            properties = Dictionary()
            proxies = Dictionary()
            unions = Dictionary()
        }
    for t in ts do
        for KeyValue (k, v) in t.constructors do
            r.constructors.[k] <- v
        for KeyValue (k, v) in t.datatypes do
            r.datatypes.[k] <- v
        for KeyValue (k, v) in t.methods do
            r.methods.[k] <- v
        for KeyValue (k, v) in t.properties do
            r.properties.[k] <- v
        for KeyValue (k, v) in t.proxies do
            if r.proxies.ContainsKey k then
                logger.Log {
                    Priority = Warning
                    Text     = "Ignoring duplicate proxies."
                    Location =
                        {
                            ReadableLocation = string k
                            SourceLocation = None
                        }
                }
            r.proxies.[k] <- v
        for KeyValue (k, v) in t.unions do
            r.unions.[k] <- v
    r

let Fields (t: T) (td: R.TypeDefinition) =
    match t.Proxy td |> Get t.datatypes with
    | Some (Class (_, fs, _)) -> fs
    | _ -> []

let Encoding =
    let eP = B.EncodingProvider.Create()
    eP.DeriveEncoding typeof<T>

let Serialize (s: System.IO.Stream) (t: T) =
    Encoding.Encode s t

let Deserialize (s: System.IO.Stream) =
    Encoding.Decode s :?> T

let ReadFromCecilAssembly (a: Mono.Cecil.AssemblyDefinition) =
    let key = EMBEDDED_METADATA
    a.MainModule.Resources
    |> Seq.tryPick (function
        | :? Mono.Cecil.EmbeddedResource as r when r.Name = key ->
            try
                use s = r.GetResourceStream()
                Some (Deserialize s)
            with _ ->
                failwithf "Failed to deserialize metadata for: %s" a.FullName
            | _ -> None)

