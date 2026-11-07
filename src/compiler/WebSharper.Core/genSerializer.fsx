// $begin{copyright}
//
// This file is part of WebSharper
//
// Copyright (c) 2008-2018 IntelliFactory
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

// Generates Serialization.g.fs: reflection-free binary readers and writers for
// WebSharper.Core.Metadata.Info and every type reachable from it.
//
// Usage, from the core repo root, after building WebSharper.Core:
//   dotnet fsi src/compiler/WebSharper.Core/genSerializer.fsx [path/to/WebSharper.Core.dll]
// If changed metadata types make the current generated file fail to compile, build
// WebSharper.Core with -p:WebSharperSkipGeneratedSerializer=true first, then regenerate.
// The `CheckGeneratedSerializer` build target regenerates and fails on any difference.

open System
open System.IO
open System.Reflection
open System.Collections.Generic
open Microsoft.FSharp.Reflection

let args = fsi.CommandLineArgs |> Array.tail
/// --check: do not write, exit with 1 if the generated file is out of date
let checkOnly = args |> Array.contains "--check"

let coreDll =
    match args |> Array.filter (fun a -> not (a.StartsWith "--")) with
    | [| p |] -> Path.GetFullPath p
    | _ -> Path.Combine(__SOURCE_DIRECTORY__, "../../../build/Release/netstandard2.0/WebSharper.Core.dll")

let outFile = Path.Combine(__SOURCE_DIRECTORY__, "Serialization.g.fs")

// load into a separate context, so a rebuilt dll is always picked up
let loadContext = System.Runtime.Loader.AssemblyLoadContext("genSerializer", false)
let coreDir = Path.GetDirectoryName coreDll
loadContext.add_Resolving(fun ctx name ->
    let p = Path.Combine(coreDir, name.Name + ".dll")
    if File.Exists p then ctx.LoadFromAssemblyPath p else null)
let core = loadContext.LoadFromAssemblyPath coreDll
let fsCore = typedefof<list<int>>.Assembly

let flags = BindingFlags.Public ||| BindingFlags.NonPublic

let infoType = core.GetType("WebSharper.Core.Metadata+Info", true)
let idType = core.GetType("WebSharper.Core.AST.Id", true)
let hashedDef = core.GetType("WebSharper.Core.Hashed`1", true)

/// Record fields written as blocks into another section (each with its own Id scope),
/// so a reader can skip them: the section, computed from the record value `x`,
/// and the value read when the section is skipped. For an option field, the block
/// is the value inside the option, so skipping keeps whether there is one.
let blockFields =
    dict [
        ("WebSharper.Core.Metadata+CompiledConstructorInfo", "Expression"),
            ("SectionHooks.memberSection x.CompiledForm", "T_Expression.Undefined")
        ("WebSharper.Core.Metadata+CompiledMethodInfo", "Expression"),
            ("SectionHooks.memberSection x.CompiledForm", "T_Expression.Undefined")
        ("WebSharper.Core.Metadata+CompiledImplementationInfo", "Expression"),
            ("SectionKind.Other", "T_Expression.Undefined")
        ("WebSharper.Core.Metadata+ClassInfo", "StaticConstructor"),
            ("SectionKind.Other", "T_Statement.Empty")
        ("WebSharper.Core.Metadata+Info", "Dependencies"),
            ("SectionKind.Graph", "T_GraphData.Empty")
    ]

let isGen (t: Type) (d: Type) = t.IsGenericType && t.GetGenericTypeDefinition() = d

let isList t = isGen t typedefof<list<int>>
let isOption t = isGen t typedefof<option<int>>
let isSet t = isGen t typedefof<Set<int>>
let isMap t = isGen t typedefof<Map<int, int>>
let isDict t = isGen t typedefof<IDictionary<int, int>>
let isHashed t = isGen t hashedDef
let arg (t: Type) i = t.GetGenericArguments().[i]

let primitives =
    dict [
        typeof<bool>, "Bool"
        typeof<byte>, "Byte"
        typeof<sbyte>, "SByte"
        typeof<int16>, "Int16"
        typeof<uint16>, "UInt16"
        typeof<int>, "Int"
        typeof<uint32>, "UInt"
        typeof<int64>, "Int64"
        typeof<uint64>, "UInt64"
        typeof<char>, "Char"
        typeof<double>, "Double"
        typeof<single>, "Single"
        typeof<decimal>, "Decimal"
        typeof<string>, "String"
    ]

let primNames =
    dict [
        typeof<bool>, "bool"
        typeof<byte>, "byte"
        typeof<sbyte>, "sbyte"
        typeof<int16>, "int16"
        typeof<uint16>, "uint16"
        typeof<int>, "int"
        typeof<uint32>, "uint32"
        typeof<int64>, "int64"
        typeof<uint64>, "uint64"
        typeof<char>, "char"
        typeof<double>, "double"
        typeof<single>, "single"
        typeof<decimal>, "decimal"
        typeof<string>, "string"
    ]

/// F# source name of a type definition, without generic arguments.
let defName (t: Type) =
    let t = if t.IsGenericType then t.GetGenericTypeDefinition() else t
    let n = t.FullName.Replace('+', '.')
    match n.IndexOf '`' with
    | -1 -> n
    | i -> n.Substring(0, i)

/// F# source syntax of a type.
let rec fsName (t: Type) : string =
    match primNames.TryGetValue t with
    | true, n -> n
    | _ ->
    if t.IsArray then fsName (t.GetElementType()) + "[]"
    elif FSharpType.IsTuple t then
        let es = FSharpType.GetTupleElements t |> Array.map fsName |> String.concat " * "
        if t.IsValueType then "struct (" + es + ")" else "(" + es + ")"
    elif isList t then "list<" + fsName (arg t 0) + ">"
    elif isOption t then "option<" + fsName (arg t 0) + ">"
    elif isSet t then "Set<" + fsName (arg t 0) + ">"
    elif isMap t then "Map<" + fsName (arg t 0) + ", " + fsName (arg t 1) + ">"
    elif isDict t then "System.Collections.Generic.IDictionary<" + fsName (arg t 0) + ", " + fsName (arg t 1) + ">"
    elif t.IsGenericType then
        defName t + "<" + (t.GetGenericArguments() |> Array.map fsName |> String.concat ", ") + ">"
    else defName t

/// Identifier-safe name for the generated function of a type.
let rec mangle (t: Type) : string =
    match primNames.TryGetValue t with
    | true, n -> n
    | _ ->
    if t.IsArray then mangle (t.GetElementType()) + "Array"
    elif FSharpType.IsTuple t then
        "Tuple_" + (FSharpType.GetTupleElements t |> Array.map mangle |> String.concat "_") + "_"
    elif t.IsGenericType then
        let d = defName t
        d.Substring(d.LastIndexOf '.' + 1) + "_" + (t.GetGenericArguments() |> Array.map mangle |> String.concat "_") + "_"
    else
        t.Name

// ---- type universe ----

type Shape =
    | Record of Type
    | Union of Type
    | HashedOf of Type
    | Custom of Type

let shapes = Dictionary<Type, Shape>()
let order = ResizeArray<Type>()
let hashedKinds = ResizeArray<Type>()

let rec visit (t: Type) =
    if primitives.ContainsKey t || t.IsEnum then ()
    elif t.IsArray then
        if t.GetArrayRank() <> 1 then failwithf "Only rank-1 arrays are supported: %s" t.FullName
        visit (t.GetElementType())
    elif FSharpType.IsTuple t then
        FSharpType.GetTupleElements t |> Array.iter visit
    elif isList t || isOption t || isSet t then visit (arg t 0)
    elif isMap t || isDict t then
        visit (arg t 0)
        visit (arg t 1)
    elif shapes.ContainsKey t then ()
    elif t = idType then
        shapes.[t] <- Custom t
        order.Add t
        visit (idType.GetProperty("VarType").PropertyType)
    elif isHashed t then
        shapes.[t] <- HashedOf t
        order.Add t
        hashedKinds.Add t
        visit (arg t 0)
    elif FSharpType.IsRecord(t, flags) then
        if FSharpType.GetRecordFields(t).Length = 0 then
            failwithf "Record with private representation needs custom serialization: %s" t.FullName
        shapes.[t] <- Record t
        order.Add t
        for f in FSharpType.GetRecordFields t do visit f.PropertyType
    elif FSharpType.IsUnion(t, flags) then
        if FSharpType.GetUnionCases(t).Length = 0 then
            failwithf "Union with private representation needs custom serialization: %s" t.FullName
        let cases = FSharpType.GetUnionCases t
        if cases.Length > 255 then failwithf "Too many union cases: %s" t.FullName
        shapes.[t] <- Union t
        order.Add t
        for c in cases do
            for f in c.GetFields() do visit f.PropertyType
    else
        failwithf "No serialization for type: %s" t.FullName

visit infoType

let hashedKind (t: Type) = hashedKinds.IndexOf t

// ---- code generation ----

let mutable var = 0
let fresh (p: string) =
    var <- var + 1
    p + string var

/// An expression writing value `v` of type `t` with writer `w`.
let rec writeExpr (t: Type) (v: string) : string =
    match primitives.TryGetValue t with
    | true, p -> sprintf "w.%s(%s)" p v
    | _ ->
    if t.IsEnum then
        let u = Enum.GetUnderlyingType t
        sprintf "w.%s(%s (%s))" primitives.[u] primNames.[u] v
    elif t.IsArray then
        let x = fresh "x"
        sprintf "writeArray w (fun w %s -> %s) %s" x (writeExpr (t.GetElementType()) x) v
    elif FSharpType.IsTuple t then
        let es = FSharpType.GetTupleElements t
        let xs = es |> Array.map (fun _ -> fresh "t")
        let pat = String.concat ", " xs
        let pat = if t.IsValueType then "struct (" + pat + ")" else "(" + pat + ")"
        sprintf "(let %s = %s in %s)" pat v (Array.map2 writeExpr es xs |> String.concat "; ")
    elif isList t then
        let x = fresh "x"
        sprintf "writeList w (fun w %s -> %s) %s" x (writeExpr (arg t 0) x) v
    elif isOption t then
        let x = fresh "x"
        sprintf "writeOption w (fun w %s -> %s) %s" x (writeExpr (arg t 0) x) v
    elif isSet t then
        let x = fresh "x"
        sprintf "writeSet w (fun w %s -> %s) %s" x (writeExpr (arg t 0) x) v
    elif isMap t || isDict t then
        let k = fresh "k"
        let x = fresh "x"
        sprintf "%s w (fun w %s -> %s) (fun w %s -> %s) %s"
            (if isMap t then "writeMap" else "writeDict")
            k (writeExpr (arg t 0) k) x (writeExpr (arg t 1) x) v
    else
        sprintf "w_%s w %s" (mangle t) v

/// An expression reading a value of type `t` with reader `r`.
let rec readExpr (t: Type) : string =
    match primitives.TryGetValue t with
    | true, p -> sprintf "r.%s()" p
    | _ ->
    if t.IsEnum then
        let u = Enum.GetUnderlyingType t
        sprintf "(LanguagePrimitives.EnumOfValue (r.%s()) : %s)" primitives.[u] (fsName t)
    elif t.IsArray then
        sprintf "readArray r (fun r -> %s)" (readExpr (t.GetElementType()))
    elif FSharpType.IsTuple t then
        let es = FSharpType.GetTupleElements t
        let xs = es |> Array.map (fun _ -> fresh "t")
        let lets = Array.map2 (fun x e -> sprintf "let %s = %s in " x (readExpr e)) xs es |> String.concat ""
        let tup = String.concat ", " xs
        sprintf "(%s%s)" lets (if t.IsValueType then "struct (" + tup + ")" else "(" + tup + ")")
    elif isList t then sprintf "readList r (fun r -> %s)" (readExpr (arg t 0))
    elif isOption t then sprintf "readOption r (fun r -> %s)" (readExpr (arg t 0))
    elif isSet t then sprintf "readSet r (fun r -> %s)" (readExpr (arg t 0))
    elif isMap t || isDict t then
        sprintf "%s r (fun r -> %s) (fun r -> %s)" (if isMap t then "readMap" else "readDict") (readExpr (arg t 0)) (readExpr (arg t 1))
    else
        sprintf "r_%s r" (mangle t)

let writeField (t: Type) (f: PropertyInfo) v =
    match blockFields.TryGetValue((t.FullName, f.Name)) with
    | true, (section, _) ->
        let ft = f.PropertyType
        let x = fresh "x"
        if isOption ft then
            let y = fresh "x"
            sprintf "writeOption w (fun w %s -> writeBlock w (%s) (fun w %s -> %s) %s) %s" x section y (writeExpr (arg ft 0) y) x v
        else
            sprintf "writeBlock w (%s) (fun w %s -> %s) %s" section x (writeExpr ft x) v
    | _ -> writeExpr f.PropertyType v

let readField (t: Type) (f: PropertyInfo) =
    match blockFields.TryGetValue((t.FullName, f.Name)) with
    | true, (_, skipped) ->
        let ft = f.PropertyType
        if isOption ft then
            sprintf "readOption r (fun r -> readBlock r (fun r -> %s) %s)" (readExpr (arg ft 0)) skipped
        else
            sprintf "readBlock r (fun r -> %s) %s" (readExpr ft) skipped
    | _ -> readExpr f.PropertyType

let sb = Text.StringBuilder()
let line (s: string) = sb.AppendLine(s) |> ignore

line "// <auto-generated>"
line "// Generated by genSerializer.fsx from the WebSharper.Core metadata types. Do not edit."
line "// Regenerate after changing any type reachable from WebSharper.Core.Metadata.Info."
line "// </auto-generated>"
line ""
line "module internal WebSharper.Core.SerializationGenerated"
line ""
line "#nowarn \"25\" // incomplete matches on enum-like values are not possible here"
line ""
line "open WebSharper.Core"
line "open WebSharper.Core.Serialization"
line "open WebSharper.Core.Serialization.Helpers"
line ""
line (sprintf "let [<Literal>] HashedKinds = %d" hashedKinds.Count)
line ""
line "let inline private failTag (t: string) (i: int) = raise (SerializationException (sprintf \"Invalid tag %d for %s\" i t))"
line ""
line "// aliases: qualified names can resolve to union cases, such as AST.TSType to Type.TSType"
for t in order do
    match shapes.[t] with
    | Record _ | Union _ -> line (sprintf "type private T_%s = %s" (mangle t) (fsName t))
    | _ -> ()
line ""

let mutable first = true
let defn (s: string) =
    // variable numbering restarts per function, so diffs stay local to changed types
    var <- 0
    if first then
        first <- false
        line ("let rec " + s)
    else
        line ("and " + s)

for t in order do
    let n = mangle t
    let tn = fsName t
    match shapes.[t] with
    | Custom _ ->
        // Id: private record, uses internal members added for serialization
        defn (sprintf "w_%s (w: MetaWriter) (x: %s) =" n tn)
        line "    let key = x.SerialNumber"
        line "    if w.BeginId key then"
        line "        w.String(Option.toObj x.Name)"
        line "        w.IdNumber(key)"
        line "        w.Byte((if x.IsMutable then 1uy else 0uy) ||| (if x.IsRest then 2uy else 0uy) ||| (if x.HasStrongName then 4uy else 0uy) ||| (if x.IsOptional then 8uy else 0uy))"
        line (sprintf "        %s" (writeExpr (idType.GetProperty("VarType").PropertyType) "x.VarType"))
        line "        w.EndId key"
        line ""
    | HashedOf _ ->
        let k = hashedKind t
        let vt = arg t 0
        defn (sprintf "w_%s (w: MetaWriter) (x: %s) =" n tn)
        line (sprintf "    if w.BeginHashed(%d, x) then" k)
        line (sprintf "        %s" (writeExpr vt "x.Value"))
        line (sprintf "        w.EndHashed(%d, x)" k)
        line ""
    | Record _ ->
        defn (sprintf "w_%s (w: MetaWriter) (x: %s) =" n tn)
        for f in FSharpType.GetRecordFields t do
            line ("    " + writeField t f ("x." + f.Name))
        line ""
    | Union _ ->
        defn (sprintf "w_%s (w: MetaWriter) (x: %s) =" n tn)
        line "    match x with"
        for c in FSharpType.GetUnionCases t do
            let fs = c.GetFields()
            let xs = fs |> Array.map (fun _ -> fresh "a")
            let pat = if fs.Length = 0 then "" else "(" + String.concat ", " xs + ")"
            line (sprintf "    | %s.%s%s ->" ("T_" + mangle t) c.Name pat)
            line (sprintf "        w.Tag %d" c.Tag)
            for f, x in Array.zip fs xs do
                line ("        " + writeExpr f.PropertyType x)
        line ""

for t in order do
    let n = mangle t
    let tn = fsName t
    match shapes.[t] with
    | Custom _ ->
        let vt = idType.GetProperty("VarType").PropertyType
        defn (sprintf "r_%s (r: MetaReader) : %s =" n tn)
        line "    let known = r.IdRef()"
        line "    if not (obj.ReferenceEquals(known, null)) then known else"
        line "        let name = r.String()"
        line "        let num = r.Int64()"
        line "        let fl = r.Byte()"
        line (sprintf "        let typ = %s" (readExpr vt))
        line "        let id = WebSharper.Core.AST.Id.Deserialize(Option.ofObj name, num, fl &&& 1uy <> 0uy, fl &&& 2uy <> 0uy, fl &&& 4uy <> 0uy, fl &&& 8uy <> 0uy, typ)"
        line "        r.AddId id"
        line "        id"
        line ""
    | HashedOf _ ->
        defn (sprintf "r_%s (r: MetaReader) : %s =" n tn)
        line (sprintf "    r.Hashed<%s>(%d)" (fsName (arg t 0)) (hashedKind t))
        line ""
    | Record _ ->
        defn (sprintf "r_%s (r: MetaReader) : %s =" n tn)
        let fs = FSharpType.GetRecordFields t
        let xs = fs |> Array.map (fun _ -> fresh "f")
        for f, x in Array.zip fs xs do
            line (sprintf "    let %s = %s" x (readField t f))
        let assigns =
            Array.zip fs xs
            |> Array.mapi (fun i (f, x) -> if i = 0 then sprintf "%s.%s = %s" ("T_" + mangle t) f.Name x else sprintf "%s = %s" f.Name x)
            |> String.concat "; "
        line (sprintf "    ({ %s } : %s)" assigns tn)
        line ""
    | Union _ ->
        defn (sprintf "r_%s (r: MetaReader) : %s =" n tn)
        line "    match r.Tag() with"
        for c in FSharpType.GetUnionCases t do
            let fs = c.GetFields()
            if fs.Length = 0 then
                line (sprintf "    | %d -> %s.%s" c.Tag ("T_" + mangle t) c.Name)
            else
                line (sprintf "    | %d ->" c.Tag)
                let xs = fs |> Array.map (fun _ -> fresh "a")
                for f, x in Array.zip fs xs do
                    line (sprintf "        let %s = %s" x (readExpr f.PropertyType))
                line (sprintf "        %s.%s(%s)" ("T_" + mangle t) c.Name (String.concat ", " xs))
        line (sprintf "    | i -> failTag \"%s\" i" tn)
        line ""

line "/// Reads the Hashed tables: entries in the order they were first written, each in its own Id scope."
line "let readTables (r: MetaReader) ="
line "    for _ in 1 .. r.Count() do"
line "        let kind = r.Tag()"
line "        r.BeginScope()"
line "        match kind with"
for t in hashedKinds do
    let vt = arg t 0
    line (sprintf "        | %d -> r.AddHashed<%s>(%d, %s)" (hashedKind t) (fsName vt) (hashedKind t) (readExpr vt))
line "        | i -> failTag \"Hashed table\" i"
line "        r.EndScope()"
line ""
line (sprintf "let writeInfo (w: MetaWriter) (x: %s) = w_%s w x" (fsName infoType) (mangle infoType))
line ""
line (sprintf "let readInfo (r: MetaReader) : %s = r_%s r" (fsName infoType) (mangle infoType))

let text = sb.ToString().Replace("\r\n", "\n")
let old = if File.Exists outFile then File.ReadAllText(outFile).Replace("\r\n", "\n") else ""
if old <> text && checkOnly then
    eprintfn "%s is out of date: run genSerializer.fsx and commit the result" outFile
    exit 1
elif old <> text then
    File.WriteAllText(outFile, text)
    printfn "Updated %s (%d types, %d hashed kinds)" outFile order.Count hashedKinds.Count
else
    printfn "Up to date: %s (%d types, %d hashed kinds)" outFile order.Count hashedKinds.Count
