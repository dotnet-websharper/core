// $begin{copyright}
//
// This file is part of WebSharper
//
// Copyright (c) 2008-2026 IntelliFactory
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

/// How WebSharper translates types and members, as the boosters' agent tools describe it: names, the
/// compiled forms in WebSharper metadata and their lookup. Shared by the F# booster (wsfscservice) and the
/// C# booster (wscscservice), it needs no compiler, only metadata.
module WebSharper.Compiler.AgentTranslation

open System
open System.Text
open System.Collections.Generic

module M = WebSharper.Core.Metadata
type TypeDefinition = WebSharper.Core.AST.TypeDefinition

// ---------------------------------------------------------------------------------------------
// Names

/// Strips generic arity and arguments, uses '.' for nesting: List`1, List<'T> and List<int> are List.
let normalizeName (name: string) =
    let sb = StringBuilder()
    let mutable depth = 0
    let mutable i = 0
    while i < name.Length do
        let c = name.[i]
        if c = '<' then depth <- depth + 1
        elif c = '>' then depth <- max 0 (depth - 1)
        elif depth = 0 then
            if c = '`' then
                // skip the arity digits
                while i + 1 < name.Length && Char.IsDigit name.[i + 1] do i <- i + 1
            elif c = '+' || c = '/' then sb.Append('.') |> ignore
            elif not (Char.IsWhiteSpace c) then sb.Append(c) |> ignore
        i <- i + 1
    sb.ToString().TrimEnd('.')

/// Splits "A.B.c" into ("A.B", "c"), member names can be operators containing dots: ( |>. )
let splitLast (name: string) =
    let i = name.LastIndexOf '.'
    if i <= 0 || i = name.Length - 1 then None else Some (name.Substring(0, i), name.Substring(i + 1))

/// Edit distance, for "did you mean" suggestions on typos (Server.ToggleTdo).
let editDistance (a: string) (b: string) =
    let a, b = a.ToLowerInvariant(), b.ToLowerInvariant()
    let d = Array2D.init (a.Length + 1) (b.Length + 1) (fun i j -> if i = 0 then j elif j = 0 then i else 0)
    for i in 1 .. a.Length do
        for j in 1 .. b.Length do
            let cost = if a.[i - 1] = b.[j - 1] then 0 else 1
            d.[i, j] <- min (min (d.[i - 1, j] + 1) (d.[i, j - 1] + 1)) (d.[i - 1, j - 1] + cost)
    d.[a.Length, b.Length]

// ---------------------------------------------------------------------------------------------
// Compiled forms

/// Short type text for WebSharper method keys: FSharpAsync<FSharpList<Todo>>, string[], (int -> unit)
let rec typeText (t: WebSharper.Core.AST.Type) =
    let short (fullName: string) =
        let n = normalizeName fullName
        match n.LastIndexOf '.' with
        | -1 -> n
        | i -> n.Substring(i + 1)
    match t with
    | WebSharper.Core.AST.ConcreteType c ->
        let n = short c.Entity.Value.FullName
        match c.Generics with
        | [] -> n
        | gs -> n + "<" + (gs |> List.map typeText |> String.concat ", ") + ">"
    | WebSharper.Core.AST.TypeParameter i -> sprintf "'T%d" i
    | WebSharper.Core.AST.ArrayType (e, 1) -> typeText e + "[]"
    | WebSharper.Core.AST.ArrayType (e, rank) -> typeText e + "[" + String(',', rank - 1) + "]"
    | WebSharper.Core.AST.TupleType (ts, isStruct) -> (if isStruct then "struct (" else "(") + (ts |> List.map typeText |> String.concat " * ") + ")"
    | WebSharper.Core.AST.FSharpFuncType (a, r) -> "(" + typeText a + " -> " + typeText r + ")"
    | WebSharper.Core.AST.ByRefType e -> "byref<" + typeText e + ">"
    | WebSharper.Core.AST.VoidType -> "unit"
    | WebSharper.Core.AST.StaticTypeParameter i -> sprintf "^T%d" i
    | WebSharper.Core.AST.LocalTypeParameter n -> "'" + n
    | other -> string other

let methodText (m: WebSharper.Core.AST.Method) =
    let m = m.Value
    sprintf "%s%s(%s): %s" m.MethodName
        (if m.Generics > 0 then "<" + String.concat ", " (List.init m.Generics (sprintf "'M%d")) + ">" else "")
        (m.Parameters |> List.map typeText |> String.concat ", ")
        (typeText m.ReturnType)

let constructorText (c: WebSharper.Core.AST.Constructor) =
    sprintf "new(%s)" (c.Value.CtorParameters |> List.map typeText |> String.concat ", ")

let describeCompiled (c: M.CompiledMember) =
    let rec d c =
        match c with
        | M.Instance (name, _, _) -> sprintf "instance method \"%s\"" name
        | M.Static (name, fromInstance, _) -> sprintf "static method \"%s\"%s" name (if fromInstance then " (instance member compiled as static)" else "")
        | M.Func (name, fromInstance) -> sprintf "function \"%s\"%s" name (if fromInstance then " (instance member compiled as function)" else "")
        | M.GlobalFunc (address, _) -> sprintf "global function %O" address
        | M.New (Some name) -> sprintf "constructor \"%s\"" name
        | M.New None -> "constructor"
        | M.Inline (true, _) -> "inlined (JavaScript compiled from the .NET code)"
        | M.Inline (false, _) -> "inline JavaScript"
        | M.Macro (macroType, _, fallback) ->
            sprintf "macro %s%s" macroType.Value.FullName (match fallback with Some f -> ", fallback: " + d f | None -> "")
        | M.Remote (_, path, _) -> sprintf "RPC \"%s\" (runs on the server)" path
    d c

let describeField (f: M.CompiledField) =
    match f with
    | M.InstanceField n -> sprintf "field \"%s\"" n
    | M.OptionalField n -> sprintf "optional field \"%s\"" n
    | M.StaticField n -> sprintf "static field \"%s\"" n
    | M.IndexedField i -> sprintf "indexed field [%d]" i
    | M.VarField v -> sprintf "variable %O" v

let describeCustomType (c: M.CustomTypeInfo) =
    match c with
    | M.NotCustomType -> None
    | M.FSharpRecordInfo fields -> Some (sprintf "F# record, JavaScript fields: %s" (fields |> List.map (fun f -> f.JSName) |> String.concat ", "))
    | M.FSharpUnionInfo u -> Some (sprintf "F# union, cases: %s" (u.Cases |> List.map (fun c -> c.Name) |> String.concat ", "))
    | M.FSharpUnionCaseInfo _ -> Some "F# union case"
    | M.FSharpAnonRecordInfo names -> Some (sprintf "F# anonymous record: %s" (String.concat ", " names))
    | M.DelegateInfo _ -> Some "delegate"
    | M.EnumInfo t -> Some (sprintf "enum of %s" t.Value.FullName)
    | M.StructInfo -> Some "struct"

// ---------------------------------------------------------------------------------------------
// Lookup

/// The translated classes and interfaces by normalized full name, with the source of their metadata.
type TranslationIndex =
    {
        Classes : Dictionary<string, ResizeArray<TypeDefinition * string * M.Info>>
        Interfaces : Dictionary<string, ResizeArray<TypeDefinition * string * M.Info>>
    }

/// The index of metadata sources: ("this project", its metadata), ("references", their merged metadata).
let buildTranslationIndex (sources: (string * M.Info) list) =
    let classes = Dictionary<string, ResizeArray<_>>(StringComparer.Ordinal)
    let interfaces = Dictionary<string, ResizeArray<_>>(StringComparer.Ordinal)
    let add (d: Dictionary<string, ResizeArray<_>>) (t: TypeDefinition) src info =
        let n = normalizeName t.Value.FullName
        match d.TryGetValue n with
        | true, l -> l.Add((t, src, info))
        | _ -> d.[n] <- ResizeArray [ (t, src, info) ]
    for (src, info) in sources do
        for kv in info.Classes do add classes kv.Key src info
        for kv in info.Interfaces do add interfaces kv.Key src info
    { Classes = classes; Interfaces = interfaces }

/// Whether a compiled method name matches one of the member names: as given, or its accessors and operators.
let memberNameMatches (names: string list) (methodName: string) =
    names |> List.exists (fun f ->
        methodName = f || methodName = "get_" + f || methodName = "set_" + f || methodName = "op_" + f
        || methodName = "add_" + f || methodName = "remove_" + f)

let private memberMatches (names: string list option) (methodName: string) =
    match names with
    | None -> true
    | Some names -> memberNameMatches names methodName

/// Whether the type has a translated member with one of the names (".ctor" for constructors).
let hasTranslatedMemberNamed (t: TypeDefinition) (info: M.Info) (names: string list) =
    let f = Some names
    match info.Classes.TryGetValue t with
    | true, (_, _, Some cls) ->
        (names |> List.contains ".ctor" && cls.Constructors.Count > 0)
        || cls.Methods.Keys |> Seq.exists (fun m -> memberMatches f m.Value.MethodName)
        || cls.Implementations.Keys |> Seq.exists (fun (_, m) -> memberMatches f m.Value.MethodName)
        || cls.Fields.Keys |> Seq.exists (fun n -> names |> List.contains n)
    | _ ->
    match info.Interfaces.TryGetValue t with
    | true, intf -> intf.Methods.Keys |> Seq.exists (fun m -> memberMatches f m.Value.MethodName)
    | _ -> false

/// Describes the translation of a type: its JavaScript address and the compiled form of its members, or of the
/// members having one of the names when memberFilter is (the member as given, its names). jsAttribute is how the
/// language writes the attribute of client-side code: [<JavaScript>] or [JavaScript].
let formatTranslationWith (jsAttribute: string) (t: TypeDefinition) (src: string) (info: M.Info) (memberFilter: (string * string list) option) =
    let names = memberFilter |> Option.map snd
    let sb = StringBuilder()
    let line (s: string) = sb.AppendLine(s) |> ignore
    let describe () =
        match info.Classes.TryGetValue t with
        | true, (address, custom, cls) ->
            line (sprintf "%s (%s), translated by WebSharper (metadata of %s)" t.Value.FullName t.Value.Assembly src)
            line (sprintf "  JavaScript address: %O" address)
            describeCustomType custom |> Option.iter (fun d -> line ("  " + d))
            match cls with
            | None -> line "  no class information: only the type is known, its members are translated where they are used (custom type)"
            | Some cls ->
                if cls.IsStub then line "  stub: declares an existing JavaScript class"
                cls.BaseClass |> Option.iter (fun b -> line (sprintf "  base class: %s" b.Entity.Value.FullName))
                if not cls.Macros.IsEmpty then
                    line (sprintf "  type macros: %s" (cls.Macros |> List.map (fun (m, _) -> m.Value.FullName) |> String.concat ", "))
                let ctors =
                    if names.IsNone || (names.Value |> List.contains ".ctor") then
                        cls.Constructors |> Seq.map (fun kv -> sprintf "    %s: %s" (constructorText kv.Key) (describeCompiled kv.Value.CompiledForm)) |> List.ofSeq
                    else []
                let methods =
                    cls.Methods
                    |> Seq.filter (fun kv -> memberMatches names kv.Key.Value.MethodName)
                    |> Seq.map (fun kv -> sprintf "    %s: %s" (methodText kv.Key) (describeCompiled kv.Value.CompiledForm))
                    |> Seq.sort |> List.ofSeq
                let impls =
                    cls.Implementations
                    |> Seq.filter (fun kv -> memberMatches names (snd kv.Key).Value.MethodName)
                    |> Seq.map (fun kv -> sprintf "    %s.%s: %s" (fst kv.Key).Value.FullName (methodText (snd kv.Key)) (describeCompiled kv.Value.CompiledForm))
                    |> Seq.sort |> List.ofSeq
                let fields =
                    cls.Fields
                    |> Seq.filter (fun kv -> names.IsNone || (names.Value |> List.contains kv.Key))
                    |> Seq.map (fun kv -> sprintf "    %s: %s" kv.Key (describeField kv.Value.CompiledForm))
                    |> List.ofSeq
                let section title (items: string list) =
                    if not items.IsEmpty then
                        line (sprintf "  %s (%d):" title items.Length)
                        for i in items do line i
                section "constructors" ctors
                section "methods" methods
                section "interface implementations" impls
                section "fields" fields
                match memberFilter with
                | Some (given, _) when ctors.IsEmpty && methods.IsEmpty && impls.IsEmpty && fields.IsEmpty ->
                    line (sprintf "  member %s has no WebSharper translation: it can't be used in client-side (%s) code" given jsAttribute)
                | _ -> ()
        | _ ->
        match info.Interfaces.TryGetValue t with
        | true, intf ->
            line (sprintf "%s (%s), interface translated by WebSharper (metadata of %s)" t.Value.FullName t.Value.Assembly src)
            line (sprintf "  JavaScript address: %O" intf.Address)
            let methods =
                intf.Methods
                |> Seq.filter (fun kv -> memberMatches names kv.Key.Value.MethodName)
                |> Seq.map (fun kv -> let (name, _, _) = kv.Value in sprintf "    %s: \"%s\"" (methodText kv.Key) name)
                |> List.ofSeq
            if not methods.IsEmpty then
                line (sprintf "  methods (%d):" methods.Length)
                for m in methods do line m
        | _ -> ()
    describe ()
    sb.ToString().TrimEnd()

/// The RPC methods of a project's metadata: "Type.Method(...): ...  ->  path", sorted.
let rpcMethods (info: M.Info) =
    [
        for kv in info.Classes do
            match kv.Value with
            | _, _, Some cls ->
                for m in cls.Methods do
                    match m.Value.CompiledForm with
                    | M.Remote (_, path, _) -> yield sprintf "%s.%s  ->  %s" kv.Key.Value.FullName (methodText m.Key) path
                    | _ -> ()
            | _ -> ()
    ]
    |> List.sort
