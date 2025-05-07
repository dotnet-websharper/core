#r "../../../build/Release/netstandard2.0/WebSharper.Core.JavaScript.dll"
#r "../../../build/Release/netstandard2.0/WebSharper.Core.dll"
#r "../../../build/Release/netstandard2.0/WebSharper.JavaScript.dll"

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

type JSObject<'T> = WebSharper.JavaScript.Object<'T>

let jsonFile = Path.Combine(__SOURCE_DIRECTORY__, "../WebSharper.TypeScriptParser/node_modules/typescript/lib/lib.es2022.full.d.ts.json")

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
        //"ThisParameterType"
        //"OmitThisParameter"
        //"CallableFunction"
        //"NewableFunction"
        //"ClassDecorator"
        //"PropertyDecorator"
        //"ElementTagNameMap"
        //"MethodDecorator"
        //"ParameterDecorator"
        //"PromiseConstructorLike"
        //"Awaited"
        //"Partial"
        //"Required"
        //"Readonly"
        //"Pick"
        //"Record"
        //"Exclude"
        //"Extract"
        //"Omit"
        //"NonNullable"
        //"Parameters"
        //"ConstructorParameters"
        //"ReturnType"
        //"InstanceType"
        //"ArrayBufferLike"
        //"NodeFilter"
        //"XPathNSResolver"
        //"HeadersInit"
        //"IDBValidKey"
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
        "bigint", "BigInt"
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
            //printfn $"Initialized class {name}"
            getOrAddClass name |> ignore
    | TSInterfaceDeclaration (name, typars, mems, ext) ->
        let name = moduleName + name
        if not (skippedDefs.Contains name) then
            //printfn $"Initialized interface {name}"
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
                //printfn $"Initialized type alias {name} as classDefinition"
                classDefinitions.Add(name, cls)
            | None ->
                match typ with
                | TSSimpleType n ->
                    //printfn $"Initialized type alias {name} as typeNameRedirect"
                    typeNameRedirects.Add(name, n)
                | _ ->
                    //printfn $"Initialized type alias {name} as typeAliasDef"
                    typeAliasDefs.Add(name, (typars, typ))
    | _ -> ()

let rec lookupType name =
    match typeNameRedirects.TryGetValue(name) with
    | true, alias -> lookupType alias
    | _ -> 
        match typeAliases.TryGetValue(name) with
        | true, v -> v
        | _ -> 
            match typeAliasDefs.TryGetValue(name) with
            | true, (typars, typ) ->
                //printfn $"typeAliasDef: {name} -> %A{typ}"
                let tRes = processType (Some name) typ
                typeAliases.Add(name, tRes)
                typeAliasDefs.Remove(name) |> ignore
                lookupType name
            | _ ->
                match classDefinitions.TryGetValue(name) with
                | true, v -> v.Type
                | _ -> 
                    match intfDefinitions.TryGetValue(name) with
                    | true, v -> v.Type
                    | _ -> failwithf "Type not found %s" name

and processType forAlias (typ: TSType) : Type.Type =
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
        let recCase, nonRecCase =
            match forAlias with 
            | Some alias ->
                nonNullCase |> Array.partition (
                    function 
                    //| TSTypeReference (rname, _) when rname = alias -> true
                    | TSArrayType (TSSimpleType (rname)) when rname = alias -> true
                    | _ -> false
                )
            | _ ->
                [||], nonNullCase 
        if Array.isEmpty nonRecCase then
            T<obj> // TODO
        else
            let notOpt =
                try
                    nonRecCase |> Seq.map (processType None) |> Seq.reduce ( + )   
                with
                | _ ->
                    printfn $"Error processing union type, nonRecCase=%A{nonRecCase}"
                    T<obj> // TODO
            let withNull =
                if Array.isEmpty nullCase then
                    notOpt
                else
                    Type.OptionType notOpt
            if Array.isEmpty recCase then
                withNull
            else
                Type.ItemOrArrayType withNull
    | TSArrayType typ ->
        !| (processType None typ)
    | TSTupleType typs ->
        typs |> Seq.map (processType None) |> Seq.reduce ( * )   
    | TSTypeReference (name, typars) ->
        match name with 
        | "Readonly" ->
            processType None typars[0]
        | "Record" ->
            //let k = processType typars[0]
            let t = processType None typars[1]
            T<JSObject<_>>[t]
        | _ ->
            lookupType name   
    | TSIntersectionType [| typ; TSTypeReference ("ThisType", [| TSSimpleType "any" |]) |] ->
        processType None typ  
    | TSTypeParamReference name ->
        T<obj>
    | TSFunctionType (pars, ret) ->
        (processParams pars) ^-> (processType None ret) 
    | TSNewType (pars, ret) ->
        (processParams pars) ^-> (processType None ret) // how to enforce ctor semantics?
    | TSTypePredicate _ ->
        T<bool>
    | TSTypeLiteral mems ->
        let clsName = $"Object{hash mems}"
        let cls =
            match classDefinitions.TryGetValue(clsName) with
            | true, cls -> cls
            | false, _ ->
                let cls = Class clsName
                for mem in mems do
                    match processMem mem with
                    | Some (m, true) ->
                        cls |+> Static [ m ] |> ignore
                    | Some (m, false) ->
                        cls |+> Instance [ m ] |> ignore
                    | _ ->
                        ()
                classDefinitions.Add(clsName, cls)
                cls
        cls.Type
    | TSKeyOfType (TSSimpleType name) ->
        let clsName = "KeyOf" + name
        let cls =
            match classDefinitions.TryGetValue(clsName) with
            | true, cls -> cls
            | false, _ ->
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
                let cls = Pattern.EnumStrings name names
                classDefinitions.Add(clsName, cls)
                cls
        cls.Type
    | TSIndexType (index, typ) ->
        T<string> // usually an index is a string   
    | TSLiteralType value ->
        T<obj> // TODO
    | TSConditionalType (check, extends, trueType, falseType) ->
        T<obj> // TODO
    | TSMappedType typ ->
        T<obj> // TODO
    | TSIntersectionType (typs) ->
        T<obj> // TODO
    | TSKeyOfType typ ->
        T<obj> // TODO
    | TSQueryType expr ->
        T<obj> // TODO

and processParam (param: TSParameter) : Type.Parameter =
    match param.Type with
    | TSUnionType [| typ; TSLiteralType "null" |]
    | TSUnionType [| TSLiteralType "null"; typ |] ->
        !? (processType None typ)?(param.Name) 
    | typ ->
        (processType None typ)?(param.Name) 

and processParams (pars: TSParameter[]) =
    {
        This = None
        Arguments = pars |> Seq.map processParam |> List.ofSeq
        Variable  = None
    } : Type.Parameters

and processMem mem =
    //try
        match mem with
        | TSMethod (name, st, pars, typars, typ) ->
            printfn "Processing member %s" name
            let m = name => (processParams pars) ^-> (processType None typ) 
            Some (m, st |> Option.defaultValue false)
        | _ -> None
    //with e ->
    //    printfn "%s" e.Message
    //    failwithf "processMem fail %A" mem
    //| TSProperty (name, typ)
    //| TSNew (pars, typ)
    //| TSCall (pars, typars, typ)
    //| TSGet (name, typ)
    //| TSSet (name, typ)
    //| TSIndex (pars, typ)

let mutable typeAliasDefCount = typeAliasDefs.Count

while typeAliasDefCount > 0 do
    let (KeyValue(name, (typars, typ))) = typeAliasDefs |> Seq.head
    try
        printfn "Processing typeAlias %s" name
        let tRes = processType (Some name) typ
        typeAliases.Add(name, tRes)
        typeAliasDefs.Remove(name) |> ignore
    with e ->
        printfn "%s" e.Message
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
                printfn "Processing class %s" name
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
            with e ->
                printfn "%s" e.Message
                failwithf "class fail %s" name
    | TSInterfaceDeclaration (name, typars, mems, ext) ->
        let name = moduleName + name
        if not (skippedDefs.Contains name) then
            //try
                printfn "Processing interface %s" name
                let intf = lookupIntf name
                for mem in mems do
                    match processMem mem with
                    | Some (m, false) ->
                        intf |+> [ m ] |> ignore
                    | _ ->
                        ()
            //with e ->
            //    printfn "%s" e.Message
            //    failwithf "interface fail %s" name
    //| TSVariableStatement
    | _ -> ()
 


