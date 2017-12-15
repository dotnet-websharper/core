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

namespace WebSharper.Sitelets

open System
open System.Collections.Generic
open System.Reflection

open WebSharper.Core
open WebSharper.Sitelets.RouterInferCommon
open WebSharper.Sitelets.ServerInferredOperators

module M = WebSharper.Core.Metadata
module P = FSharp.Quotations.Patterns

module internal ServerRouting =

    type ReflectionAttributeReader() =
        inherit AttributeReader<System.Reflection.CustomAttributeData>()
        override this.GetAssemblyName attr = attr.Constructor.DeclaringType.Assembly.FullName.Split(',').[0]
        override this.GetName attr = attr.Constructor.DeclaringType.Name
        override this.GetCtorArgOpt attr = attr.ConstructorArguments |> Seq.tryHead |> Option.map (fun a -> unbox<string> a.Value)
        override this.GetCtorParamArgs attr =
            match attr.ConstructorArguments |> Seq.tryHead with
            | Some a ->
                a.Value |> unbox<seq<CustomAttributeTypedArgument>> |> Seq.map (fun a -> unbox<string> a.Value) |> Array.ofSeq
            | _ -> [||]

    let attrReader = ReflectionAttributeReader() 

    type BF = System.Reflection.BindingFlags
    let flags = BF.Public ||| BF.NonPublic ||| BF.Static ||| BF.Instance

    let ReadEndPointString (e: string) =
        Route.FromUrl(e).Segments |> Array.ofList

    type EndPointSegment =
        | StringSegment of string
        | FieldSegment of string

    let GetEndPointHoles (parts: string[]) =
        parts
        |> Array.map (fun p -> 
            if p.StartsWith("{") && p.EndsWith("}") then  
                FieldSegment (p.Substring(1, p.Length - 2))
            else StringSegment p
        )

    let GetPathHoles (p: Route) =
        p.Segments |> Array.ofList |> GetEndPointHoles
        ,
        p.QueryArgs |> Map.map(fun _ s -> [| s |] |> GetEndPointHoles)

    let getTypeAnnot (t: Type) =
        attrReader.GetAnnotation(t.GetCustomAttributesData())

    let getUnionCaseAnnot (uc: Reflection.UnionCaseInfo) =
        attrReader.GetAnnotation(uc.GetCustomAttributesData(), uc.Name)

    let getPropertyAnnot (p: Reflection.PropertyInfo) =
        attrReader.GetAnnotation(p.GetCustomAttributesData())

    let getFieldAnnot (f: Reflection.FieldInfo) =
        attrReader.GetAnnotation(f.GetCustomAttributesData())

    let routerCache = System.Collections.Concurrent.ConcurrentDictionary<Type, InferredRouter>()
    let parsedClassEndpoints = Dictionary<Type, Annotation>()

    let getMethod expr =
        match expr with
        | P.Call(_, mi, _) -> mi.GetGenericMethodDefinition()
        | _ ->              
            eprintfn "Reflection error in Warp.Internals, not a Call: %A" expr
            Unchecked.defaultof<_>

    let jsonRouterM = getMethod <@ IJson<int> @>
    let getJsonRouter (t: Type) = 
        jsonRouterM.MakeGenericMethod(t).Invoke(null, [||]) :?> InferredRouter

    let recurringOn = HashSet()

    let rec getRouter t =
        if recurringOn.Add t then
            let res = routerCache.GetOrAdd(t, valueFactory = fun t -> createRouter t)
            recurringOn.Remove t |> ignore
            res
        else
            IDelayed (fun () -> routerCache.[t])

    and wildCardRouter (t: Type) : InferredRouter =
        if t = typeof<string> then 
            iWildcardString
        elif t.IsArray then
            let item = t.GetElementType()
            iWildcardArray item (getRouter item)
        elif t.IsGenericType && t.GetGenericTypeDefinition() = typedefof<list<_>> then
            let item = t.GetGenericArguments().[0]
            iWildcardList item (getRouter item)
        else failwithf "Invalid type for Wildcard field: %O" t

    and getDateTimeRouter name fmt (t: Type) =
        if t.FullName = "System.DateTime" then
            iDateTime (Some fmt)
        else failwithf "Expecting a DateTime field: %s" name
    
    and queryRouter (t: Type) name r =
        let gd = if t.IsGenericType then t.GetGenericTypeDefinition() else null
        if gd = typedefof<option<_>> then 
            let item = t.GetGenericArguments().[0]
            getRouter item |> IQueryOption item name
        elif gd = typedefof<Nullable<_>> then 
            let item = t.GetGenericArguments().[0]
            getRouter item |> IQueryNullable name
        else
            r() |> IQuery name
    
    and fieldRouter (t: Type) (annot: Annotation) name : InferredRouter =
        let r() = 
            match annot.DateTimeFormat with
            | Some (Choice1Of2 fmt) ->
                getDateTimeRouter name fmt t   
            | Some (Choice2Of2 m) when m.ContainsKey name ->
                getDateTimeRouter name m.[name] t    
            | _ ->
                getRouter t
        match annot.Query with
        | Some _ -> queryRouter t name r
        | _ -> 
        match annot.FormData with
        | Some _ -> queryRouter t name r |> IFormData
        | _ -> 
        match annot.Json with
        | Some _ -> getJsonRouter t
        | _ when annot.IsWildcard -> wildCardRouter t
        | _ -> r() 

    and createRouter (t: Type) : InferredRouter =
        if t.IsEnum then
            getRouter (System.Enum.GetUnderlyingType(t))
        elif t.IsArray then
            let item = t.GetElementType()
            IArray item (getRouter item)
        elif Reflection.FSharpType.IsTuple t then
            let items = Reflection.FSharpType.GetTupleElements t
            let itemReader = Reflection.FSharpValue.PreComputeTupleReader t
            let ctor = Reflection.FSharpValue.PreComputeTupleConstructor t
            ITuple itemReader ctor
                (items |> Array.map getRouter) 
        elif Reflection.FSharpType.IsRecord t then
            let fields =
                Reflection.FSharpType.GetRecordFields t
                |> Array.map (fun f -> 
                    fieldRouter f.PropertyType (getPropertyAnnot f) f.Name 
                )
            let fieldReader = Reflection.FSharpValue.PreComputeRecordReader(t, flags)
            let ctor = Reflection.FSharpValue.PreComputeRecordConstructor(t, flags)
            IRecord fieldReader ctor fields
        elif Reflection.FSharpType.IsUnion t then
            let isGen = t.IsGenericType
            if isGen && t.GetGenericTypeDefinition() = typedefof<list<_>> then
                let item = t.GetGenericArguments().[0]
                IList item (getRouter item)
            elif isGen && t.GetGenericTypeDefinition() = typedefof<ParseRequestResult<_>> then
                let item = t.GetGenericArguments().[0]
                IWithCustomErrors t (getRouter item)
            else
                let cases = Reflection.FSharpType.GetUnionCases(t, flags)
                let tagReader = Reflection.FSharpValue.PreComputeUnionTagReader(t, flags)
                let caseReaders = 
                    cases |> Array.map (fun c -> Reflection.FSharpValue.PreComputeUnionReader(c, flags))
                let caseCtors = 
                    cases |> Array.map (fun c -> Reflection.FSharpValue.PreComputeUnionConstructor(c, flags))
                IUnion 
                    tagReader 
                    caseReaders
                    caseCtors
                    (cases |> Array.map (fun c ->
                        let cAnnot = getUnionCaseAnnot c
                        let mutable queryFields = defaultArg cAnnot.Query Set.empty
                        let mutable jsonField = cAnnot.Json |> Option.bind id
                        let mutable formDataFields = defaultArg cAnnot.FormData Set.empty
                        let m, e = 
                            match cAnnot.EndPoint with
                            | Some (m, e) -> m, ReadEndPointString e
                            | _ -> None, [| c.Name |]
                        let f =
                            let fields = c.GetFields()
                            fields |> Array.mapi (fun i f -> 
                                let fTyp = f.PropertyType
                                let fName = f.Name
                                let r() = 
                                    match cAnnot.DateTimeFormat with
                                    | Some (Choice1Of2 fmt) ->
                                        getDateTimeRouter fName fmt t   
                                    | Some (Choice2Of2 m) when m.ContainsKey fName ->
                                        getDateTimeRouter fName m.[fName] t    
                                    | _ ->
                                        getRouter fTyp
                                if queryFields.Contains fName then 
                                    queryFields <- queryFields |> Set.remove fName
                                    queryRouter fTyp fName r
                                elif formDataFields.Contains fName then 
                                    formDataFields <- formDataFields |> Set.remove fName
                                    queryRouter fTyp fName r |> IFormData
                                elif Option.isSome jsonField && jsonField.Value = fName then
                                    jsonField <- None
                                    getJsonRouter fTyp
                                elif cAnnot.IsWildcard && i = fields.Length - 1 then
                                    wildCardRouter fTyp
                                else r()
                            )    
                        if queryFields.Count > 0 then 
                            failwithf "Query field not found: %s" (Seq.head queryFields)
                        match jsonField with
                        | Some j ->
                            failwithf "Json field not found: %s" j
                        | _ -> ()
                        if formDataFields.Count > 0 then 
                            failwithf "FormData field not found: %s" (Seq.head formDataFields)
                        // todo: more error reports
                        m, e, f
                    ))
                
        else
            match t.Namespace with
            | "System" ->
                match t.Name with
                | "Object" ->
                    IEmpty
                | "String" ->
                    iString 
                | "Char" ->
                    iChar 
                | "Guid" ->
                    iGuid 
                | "Boolean" ->
                    iBool 
                | "Int32" ->
                    iInt
                | "Double" ->
                    iDouble 
                | "DateTime" ->
                    iDateTime None
                | "Nullable`1" ->
                    let item = t.GetGenericArguments().[0]
                    INullable (getRouter item)
                | n ->
                    failwithf "System type not supported for inferred router: %s" n
            | _ ->
                let rec getClassAnnotation td : Annotation =
                    match parsedClassEndpoints.TryFind(td) with
                    | Some ep -> ep
                    | None ->
                        let b = 
                            let b = t.BaseType  
                            if b.FullName = "System.Object" then Annotation.Empty else getClassAnnotation b
                        let annot = getTypeAnnot t |> Annotation.Combine b
                        parsedClassEndpoints.Add(td, annot)
                        annot
                let annot = getClassAnnotation t 
                let endpoint = 
                    match annot.EndPoint with
                    | Some (_, e) -> e |> Route.FromUrl |> GetPathHoles |> fst
                    | None -> [||]
                let fields = ResizeArray()
                let partsAndFields =
                    endpoint |> Array.map (function
                        | StringSegment s -> Choice1Of2 s
                        | FieldSegment f ->  
                            let field = t.GetField(f)
                            fields.Add(field)
                            Choice2Of2 (fieldRouter field.FieldType (getFieldAnnot field) f)
                    )
                let fields = fields.ToArray()
                let readFields (o: obj) =
                    fields |> Array.map (fun f -> f.GetValue(o))
                let createObject values =
                    let o = System.Activator.CreateInstance(t)
                    (fields, values) ||> Array.iter2 (fun f v -> f.SetValue(o, v))
                    o
                let subClasses =
                    t.GetNestedTypes() |> Array.choose (fun nt ->
                        if nt.BaseType = t then Some (nt, getRouter nt) else None
                    )
                IClass readFields createObject partsAndFields subClasses
