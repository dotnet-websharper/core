// $begin{copyright}
//
// This file is part of WebSharper
//
// Copyright (c) 2008-2016 IntelliFactory
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

// Main translation module from .NET forms to JavaScript
module WebSharper.Compiler.TypeTranslator

open WebSharper.Core
open WebSharper.Core.AST
open WebSharper.Core.Metadata

type LookupTypeResult =
    | Class of Address * CustomTypeInfo * option<ClassInfo>
    | Interface of InterfaceInfo                
    | Unknown

let CustomTranslations: IDictionary<TypeDefinition, list<TSType> -> TSType> =
    let (|Last|_|) l =
        match List.rev l with
        | l :: r -> Some (List.rev r, l)
        | _ -> None

    let jsTyp t =
        TypeDefinition{
            Assembly = "WebSharper.JavaScript"
            FullName = t
        }
    let coreTyp t =
        TypeDefinition{
            Assembly = "WebSharper.Core"
            FullName = t
        }
    let fscoreTyp t =
        TypeDefinition{
            Assembly = "FSharp.Core"
            FullName = t
        }
    let corlibTyp t =
        TypeDefinition{
            Assembly = "netstandard"
            FullName = t
        }
    let inv() =
        invalidOp "unexpected type arguments for JS interop type"
    let nonOptional a = a |> List.map (fun a -> a, false)
    dict [
        yield jsTyp "WebSharper.JavaScript.Object`1", (List.head >> TSType.ObjectOf) 
        yield coreTyp "WebSharper.JavaScript.Optional`1", List.head
        yield coreTyp "WebSharper.JavaScript.Optional`1+Undefined", List.head
        yield coreTyp "WebSharper.JavaScript.Optional`1+Defined", List.head
        yield corlibTyp "System.Nullable`1", List.head
        yield Definitions.Dynamic, fun _ -> TSType.Any
        yield fscoreTyp "Microsoft.FSharp.Core.FSharpRef`1", TSType.Tuple  
        yield fscoreTyp "Microsoft.FSharp.Control.FSharpAsyncReplyChannel`1",
            function
            | [t] -> TSType.Function(None, [t, false], None, TSType.Void)
            | _ -> inv()
        for i = 1 to 7 do
            yield coreTyp ("WebSharper.JavaScript.Union`" + string i), TSType.Union
            for j = 1 to i do
                yield coreTyp (sprintf "WebSharper.JavaScript.Union`%d+Union%dOf%d" i j i), (List.item (j - 1))
        yield coreTyp "WebSharper.JavaScript.FuncWithArgs`2",
            function 
            | [ TSType.ArrayOf e; r ] -> TSType.Function(None, [], Some e, r)
            | [ TSType.Tuple a; r ] -> TSType.Lambda(a, r)
            | _ -> inv()
        yield coreTyp "WebSharper.JavaScript.FuncWithThis`2",
            function
            | [ t; TSType.Function(_, a, e, r) ] -> TSType.Function(Some t, a, e, r) 
            | _ -> inv()
        yield coreTyp "WebSharper.JavaScript.FuncWithOnlyThis`2", 
            function
            | [ t; r ] -> TSType.Function(Some t, [], None, r) 
            | _ -> inv()
        yield coreTyp "WebSharper.JavaScript.FuncWithArgsRest`2", 
            function
            | [ TSType.Tuple a; e; r ] -> TSType.Function(None, nonOptional a, Some e, r) 
            | _ -> inv()
        for i = 0 to 6 do
            yield coreTyp ("WebSharper.JavaScript.FuncWithRest`" + string (i + 2)), 
                function
                | Last (Last (a, e), r) -> TSType.Function(None, nonOptional a, Some e, r) 
                | _ -> inv()
            yield coreTyp ("WebSharper.JavaScript.ThisAction`" + string (i + 1)), 
                function
                | t :: a -> TSType.Function(Some t, nonOptional a, None, TSType.Void) 
                | _ -> inv()
            yield coreTyp ("WebSharper.JavaScript.ThisFunc`" + string (i + 2)), 
                function
                | t :: Last (a, r) -> TSType.Function(Some t, nonOptional a, None, r) 
                | _ -> inv()
            yield coreTyp ("WebSharper.JavaScript.ParamsAction`" + string (i + 1)), 
                function
                | Last (a, e) -> TSType.Function(None, nonOptional a, Some e, TSType.Void) 
                | _ -> inv()
            yield coreTyp ("WebSharper.JavaScript.ParamsFunc`" + string (i + 2)), 
                function
                | Last (Last (a, e), r) -> TSType.Function(None, nonOptional a, Some e, r) 
                | _ -> inv()
            yield coreTyp ("WebSharper.JavaScript.ThisParamsAction`" + string (i + 2)), 
                function
                | t :: Last (a, e) -> TSType.Function(Some t, nonOptional a, Some e, TSType.Void) 
                | _ -> inv()
            yield coreTyp ("WebSharper.JavaScript.ThisParamsFunc`" + string (i + 3)), 
                function
                | t :: Last (Last (a, e), r) -> TSType.Function(Some t, nonOptional a, Some e, r) 
                | _ -> inv()
    ]

let ByRefAddress = Address.TypeDefaultExport { Assembly = "WebSharper.StdLib"; Name = "WebSharper.ByRef`1" }
let InRefAddress = Address.TypeDefaultExport { Assembly = "WebSharper.StdLib"; Name = "WebSharper.InRef`1" }
let OutRefAddress = Address.TypeDefaultExport { Assembly = "WebSharper.StdLib"; Name = "WebSharper.OutRef`1" }

type TypeTranslator(lookupType: TypeDefinition -> LookupTypeResult, ?tsTypeOfAddress) =
    let defaultTsTypeOfAddress (a: Address) =
        let t = a.Address |> List.rev
        match a.Module with
        | StandardLibrary
        | JavaScriptFile _ -> TSType.Named t
        | JavaScriptModule _ 
        | DotNetType _ 
        | NpmPackage _ -> TSType.Importing a
        | ImportedModule _ -> failwith "Unexpected: ImportedModule in TypeTranslator"

    let tsTypeOfAddress = defaultArg tsTypeOfAddress defaultTsTypeOfAddress

    let mappedTypes = Dictionary()

    //let cleanName (s: string) = s.Replace('.', '_').Replace('+', '_').Replace('`', '_')

    member this.TSTypeOfDef(t: TypeDefinition) =
        match mappedTypes.TryGetValue t with
        | true, tt -> tt
        | _ ->
            let res =
                match lookupType t with
                | Class (a, (FSharpUnionInfo _), Some cls) ->
                    let className = (t.Value.FullName.Split([|'.';'+'|]) |> Array.last).Split('`') |> Array.head
                    if cls.HasWSPrototype then
                        tsTypeOfAddress { a with Address = [ className + "_T" ] } 
                    else
                        tsTypeOfAddress { a with Address = [ className ] } 
                | Class (a, (FSharpUnionCaseInfo _), Some cls) ->
                    let unionCaseTypeName = (t.Value.FullName.Split([|'.';|]) |> Array.last).Split('`') |> Array.head
                    let unionName = unionCaseTypeName.Split('+') |> Array.head
                    let unionCaseName = unionCaseTypeName.Split('+') |> Array.last
                    if cls.HasWSPrototype then
                        TSType.Intersection [ tsTypeOfAddress { a with Address = [ unionName + "_T" ] }; tsTypeOfAddress { a with Address = [ unionCaseName ] } ]
                    else
                        TSType.Intersection [ tsTypeOfAddress { a with Address = [ unionName ] }; tsTypeOfAddress { a with Address = [ unionCaseName ] } ] 
                | Class (a, _, Some c) ->
                    match c.Type with
                    | Some t -> t.ResolveAddress(tsTypeOfAddress)
                    | _ -> tsTypeOfAddress a
                | Class (_, DelegateInfo i, None) ->
                    TSType.LambdaWithOpt(i.DelegateArgs |> List.map (fun (t, d) -> this.TSTypeOf t, Option.isSome d), this.TSTypeOf i.ReturnType)
                | Class (_, EnumInfo t, None) ->
                    this.TSTypeOfDef t
                | Class (_, (FSharpRecordInfo r), None) ->
                    TSType.TypeLiteral (r |> List.map (fun f -> 
                        f.JSName, MemberKind.Simple, this.TSTypeOf f.RecordFieldType
                    ))
                | Class (a, (FSharpUnionInfo _ | FSharpUnionCaseInfo _), None) ->
                    let className = (t.Value.FullName.Split([|'.';'+'|]) |> Array.last).Split('`') |> Array.head
                    tsTypeOfAddress { a with Address = [ className ] } 
                | Interface i ->
                    tsTypeOfAddress i.Address
                | _ -> TSType.Any
            mappedTypes.Add(t, res)
            res

    member this.TSTypeOfConcrete (t: Concrete<TypeDefinition>) =
        let e = t.Entity
        let gen = t.Generics |> List.map this.TSTypeOf
        match CustomTranslations.TryGetValue e with
        | true, f -> 
            try f gen
            with _ ->
                failwithf "Error during translating type %O" (ConcreteType t)
        | _ ->
        let td = this.TSTypeOfDef e
        match gen with
        | [] -> td
        | _ -> 
            match td with
            | TSType.Importing _
            | TSType.Imported _
            | TSType.Named _ ->
                TSType.Generic(td, gen)
            | _ ->
                td.SubstituteGenerics (Array.ofList gen)

    member this.TSTypeOf (t: Type) =
        match t with 
        | ConcreteType t -> this.TSTypeOfConcrete t
        | ArrayType (t, a) -> 
            match a with
            | 1 -> TSType.ArrayOf (this.TSTypeOf t)
            | 2 -> TSType.ArrayOf (TSType.ArrayOf (this.TSTypeOf t))
            | _ -> failwith "only 1 and 2-dim arrays are supported"
        | TupleType (ts, _) -> TSType.Tuple (ts |> List.map (this.TSTypeOf))
        | FSharpFuncType (a, r) -> 
            let ta =
                match a with
                | VoidType -> []
                | _ -> [this.TSTypeOf a]
            TSType.Lambda(ta, this.TSTypeOf r)
        | ByRefType t -> 
            TSType.Generic (tsTypeOfAddress ByRefAddress, [this.TSTypeOf t])
        | VoidType -> TSType.Void
        | TypeParameter i 
        | StaticTypeParameter i -> 
            TSType.Param i
        | LocalTypeParameter _ -> TSType.Any
        | TSType t -> t.ResolveAddress(tsTypeOfAddress)
