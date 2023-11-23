#r "../../../build/Release/netstandard2.0/WebSharper.Core.JavaScript.dll"
#r "../../../build/Release/netstandard2.0/WebSharper.Core.dll"

#r "nuget: System.CodeDom, 4.4"

#load "Reflection.fsi" "Reflection.fs"
#load "Util.fs"
#load "Type.fs"
#load "CodeModel.fs"
#load "Pervasives.fs"
#load "Pattern.fs"
#load "TypeScriptImport.fs"

open System.IO
open System.Collections.Generic

open WebSharper.InterfaceGenerator.TypeScriptImport

let jsonFile = Path.Combine(__SOURCE_DIRECTORY__, "../WebSharper.TypeScriptParser/node_modules/typescript/lib/lib.d.ts.json")

let jsonString = File.ReadAllText(jsonFile)


let parsed = WebSharper.Json.Deserialize<TSFile[]>(jsonString)

//for file in parsed do
//    for st in file.Statements do
//        match st with
//        | TSVariableStatement decl -> printfn "vars %s" (decl |> Seq.map (fun d -> d.Name) |> String.concat ", ")
//        | TSFunction (Some name, pars, ret) -> printfn "function %s" name
//        | TSNew (Some name, pars, ret) -> printfn "new %s" name
//        | TSTypeAlias (name, typars, typ) -> printfn "alias %s" name
//        | TSClassDeclaration (name, typars, mems, ext, impl) -> printfn "class %s" name
//        | TSInterfaceDeclaration (name, typars, mems, ext) -> printfn "interface %s" name
//        | TSModuleDeclaration (name, mems) -> printfn "module %s" name

open WebSharper.InterfaceGenerator

let skippedDefs =
    HashSet [|
        "ThisParameterType"
        "OmitThisParameter"
        "CallableFunction"
        "NewableFunction"
        "ClassDecorator"
        "PropertyDecorator"
        "ElementTagNameMap"
        "MethodDecorator"
        "ParameterDecorator"
        "PromiseConstructorLike"
        "Awaited"
        "Partial"
        "Required"
        "Readonly"
        "Pick"
        "Record"
        "Exclude"
        "Extract"
        "Omit"
        "NonNullable"
        "Parameters"
        "ConstructorParameters"
        "ReturnType"
        "InstanceType"
        "ArrayBufferLike"
        "NodeFilter"
        "XPathNSResolver"
        "HeadersInit"
        "IDBValidKey"
    |]


let typeAliases =
    dict [
        "string", T<string>
        "boolean", T<bool>
        "any", T<obj>
        "unknown", T<obj>
        "object", T<obj>
        "void", T<unit>
        "undefined", T<unit>
        "number", T<int>
        "this", TSelf
    ] 
    |> Dictionary<string, Type.Type>

let typeNameRedirects = 
    dict [
        "symbol", "Symbol"
    ]
    |> Dictionary<string, string>

let classDefinitions = Dictionary<string, CodeModel.Class>()
let intfDefinitions = Dictionary<string, CodeModel.Interface>()

let getOrAdd key init (d: Dictionary<_,_>) =
    match d.TryGetValue key with
    | true, v -> v
    | _ ->
        let v = init key
        d.Add(key, v)
        v

let getOrAddClass name = classDefinitions |> getOrAdd name Class
let getOrAddIntf name = intfDefinitions |> getOrAdd name Interface

let lookupClass name = classDefinitions[name]
let lookupIntf name = intfDefinitions[name]

let rec lookupType name =
    match typeNameRedirects.TryGetValue(name) with
    | true, alias -> lookupType alias
    | _ -> 
        match typeAliases.TryGetValue(name) with
        | true, v -> v
        | _ -> 
            match classDefinitions.TryGetValue(name) with
            | true, v -> v.Type
            | _ -> 
                match intfDefinitions.TryGetValue(name) with
                | true, v -> v.Type
                | _ -> failwithf "Type not found %s" name

let glob = getOrAddClass "Global"

let typeAliasDefs = Dictionary<string, option<TSTypeParameter[]> * TSType>()

let iterStatements action =
    let rec iter mn st =
        match st with
        | TSModuleDeclaration (name, mems) ->
            for m in mems do
                iter (mn + name + ".") m
        | _ ->
            action mn st

    for file in parsed do
        for st in file.Statements do
            iter "" st
                
iterStatements <| fun moduleName st ->
    match st with
    | TSClassDeclaration (name, typars, mems, ext, impl) -> 
        let name = moduleName + name
        if not (skippedDefs.Contains name) then
            getOrAddClass name |> ignore
    | TSInterfaceDeclaration (name, typars, mems, ext) ->
        let name = moduleName + name
        if not (skippedDefs.Contains name) then
            getOrAddIntf name |> ignore
    | TSTypeAlias (name, typars, typ) ->
        let name = moduleName + name
        if not (skippedDefs.Contains name) then
            let valueUnionOpt =
                match typ with
                | TSUnionType typs ->
                    let values = 
                        typs |> Array.map (
                            function 
                            | TSLiteralType v -> Some v 
                            | TSSimpleType "undefined" -> Some "undefined"
                            | _ -> None
                        )
                    if values |> Array.forall Option.isSome then
                        Some (values |> Array.map Option.get)
                    else
                        None
                | TSLiteralType v -> Some [| v |]
                | _ -> None
            match valueUnionOpt with
            | Some valueUnion ->
                let values =
                    valueUnion 
                    |> Seq.choose (fun v ->
                        if v.StartsWith('"') && v.EndsWith('"') then
                            Some (v.Trim('"'))
                        else
                            None
                    )
                let cls = Pattern.EnumStrings name values
                classDefinitions.Add(name, cls)
            | None ->
                match typ with
                | TSSimpleType n ->
                    typeNameRedirects.Add(name, n)
                | _ ->
                    typeAliasDefs.Add(name, (typars, typ))
    | _ -> ()

let rec processType (typ: TSType) : Type.Type =
    match typ with
    | TSSimpleType name ->
        lookupType name
    | TSUnionType typs ->
        let nullCase, nonNullCase =
            typs |> Array.partition (
                function 
                | TSLiteralType "null"
                | TSLiteralType "undefined" -> true
                | _ -> false
            )
        let notOpt =
            nonNullCase |> Seq.map processType |> Seq.reduce ( + )    
        if Array.isEmpty nullCase then
            notOpt
        else
            Type.OptionType notOpt
    | TSArrayType typ ->
        !| (processType typ)
    | TSTypeReference (name, typars) ->
        match name with 
        | "Readonly" ->
            processType typars[0]
        | _ ->
            lookupType name   
    | TSIntersectionType [| typ; TSTypeReference ("ThisType", [| TSSimpleType "any" |]) |] ->
        processType typ  
    | TSTypeParamReference name ->
        T<obj>
    | TSFunctionType (pars, ret) ->
        (processParams pars) ^-> (processType ret) 
    | TSTypePredicate _ ->
        T<bool>
    | TSKeyOfType (TSSimpleType name) ->
        let names =
            match classDefinitions.TryGetValue(name) with
            | true, v ->
                Seq.append 
                    (v.Methods |> Seq.map (fun m -> m.Name))
                    (v.Properties |> Seq.map (fun p -> p.Name))
            | _ -> 
                match intfDefinitions.TryGetValue(name) with
                | true, v -> 
                    Seq.append 
                        (v.Methods |> Seq.map (fun m -> m.Name))
                        (v.Properties |> Seq.map (fun p -> p.Name))
                | _ -> failwithf "Type not found %s" name
        let cls = Pattern.EnumStrings name values
        classDefinitions.Add(name, cls)
    | _ ->
        failwithf "processType fail %A" typ

and processParam (param: TSParameter) : Type.Parameter =
    match param.Type with
    | TSUnionType [| typ; TSLiteralType "null" |]
    | TSUnionType [| TSLiteralType "null"; typ |] ->
        !? (processType typ)?(param.Name) 
    | typ ->
        (processType typ)?(param.Name) 

and processParams (pars: TSParameter[]) =
    {
        This = None
        Arguments = pars |> Seq.map processParam |> List.ofSeq
        Variable  = None
    } : Type.Parameters

let processMem mem =
    try
        match mem with
        | TSMethod (name, st, pars, typars, typ) ->
            let m = name => (processParams pars) ^-> (processType typ) 
            Some (m, st |> Option.defaultValue false)
        | _ -> None
    with e ->
        printfn "%s" e.Message
        failwithf "processMem fail %A" mem
    //| TSProperty (name, typ)
    //| TSNew (pars, typ)
    //| TSCall (pars, typars, typ)
    //| TSGet (name, typ)
    //| TSSet (name, typ)
    //| TSIndex (pars, typ)

let mutable typeAliasDefCount = typeAliasDefs.Count

while typeAliasDefCount > 0 do
    for KeyValue(name, (typars, typ)) in typeAliasDefs |> Array.ofSeq do
        try
            let tRes = processType typ
            typeAliases.Add(name, tRes)
            typeAliasDefs.Remove(name) |> ignore
        with _ ->
            ()
    if typeAliasDefCount = typeAliasDefs.Count then
        failwithf "Unresolved types: %s" (typeAliasDefs.Keys |> String.concat ", ")
    else
        typeAliasDefCount <- typeAliasDefs.Count 

iterStatements <| fun moduleName st ->
    match st with
    | TSClassDeclaration (name, typars, mems, ext, impl) -> 
        let name = moduleName + name
        if not (skippedDefs.Contains name) then
            try
                let cls = lookupClass name
                match ext with
                | Some (TSSimpleType b) ->
                    let bCls = lookupClass b
                    cls |=> Inherits bCls |> ignore
                | _ -> ()
                for mem in mems do
                    match processMem mem with
                    | Some (m, true) ->
                        cls |+> Static [ m ] |> ignore
                    | Some (m, false) ->
                        cls |+> Instance [ m ] |> ignore
                    | _ ->
                        ()
                    //| TSProperty (name, typ)
                    //| TSNew (pars, typ)
                    //| TSCall (pars, typars, typ)
                    //| TSGet (name, typ)
                    //| TSSet (name, typ)
                    //| TSIndex (pars, typ)
            with e ->
                printfn "%s" e.Message
                failwithf "class fail %s" name
    | TSInterfaceDeclaration (name, typars, mems, ext) ->
        let name = moduleName + name
        if not (skippedDefs.Contains name) then
            try
                let intf = lookupIntf name
                for mem in mems do
                    match processMem mem with
                    | Some (m, false) ->
                        intf |+> [ m ] |> ignore
                    | _ ->
                        ()
            with e ->
                printfn "%s" e.Message
                failwithf "interface fail %s" name
    | _ -> ()
 


