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

namespace WebSharper.Sitelets

open WebSharper
open WebSharper.Core
open WebSharper.Core.AST
open System.Collections.Generic
open WebSharper.Sitelets.RouterInferCommon

module M = WebSharper.Core.Metadata
module P = FSharp.Quotations.Patterns
module S = ServerRouting

[<AutoOpen>]
module private ClientRoutingInternals =

    type MetadataAttributeReader() =
        inherit AttributeReader<TypeDefinition * M.ParameterObject[]>()
        override this.GetAssemblyName attr =
            (fst attr).Value.Assembly
        override this.GetName attr =
            let n = (fst attr).Value.FullName
            n.Substring(n.LastIndexOf('.') + 1)
        override this.GetCtorArgOpt attr = 
            (snd attr) |> Array.tryHead |> Option.map (M.ParameterObject.ToObj >> unbox<string>) 
        override this.GetCtorParamArgs attr = 
            match attr with
            | _, [| M.ParameterObject.String a |] -> [| a |]
            | _, [| M.ParameterObject.String a; M.ParameterObject.String b |] -> [| a; b |]
            | _, [| M.ParameterObject.Array a |] -> a |> Array.map (M.ParameterObject.ToObj >> unbox<string>)
            | _ -> [||]
        override this.GetCtorParamArgsOrPair attr =
            match attr with
            | _, [| M.ParameterObject.Array a |] -> 
                a |> Array.mapi (fun i x -> M.ParameterObject.ToObj x |> unbox<string>, i, true)
            | _, [| M.ParameterObject.String s |] -> [| s, 0, true |]
            | _, [| M.ParameterObject.String s; M.ParameterObject.Int32 o |] -> [| s, o, true |]
            | _, [| M.ParameterObject.String s; M.ParameterObject.Bool b |] -> [| s, 0, b |]
            | _, [| M.ParameterObject.String s; M.ParameterObject.Int32 o; M.ParameterObject.Bool b |] -> [| s, o, b |]
            | _ -> [||]

    let attrReader = MetadataAttributeReader()
    
    let cString s = !~ (Literal.String s)
    let cTrue = !~ (Literal.Bool true)
    let cFalse = !~ (Literal.Bool false)
    let cBool x = if x then cTrue else cFalse
    let inline cInt i = !~ (Int i)

    let routerOpsModule =
        match <@ RouterOperators.rString @> with
        | P.PropertyGet(_, pi, _) -> Reflection.ReadTypeDefinition pi.DeclaringType
        | e -> 
            failwithf "Reflection error in RouterInfer.Client, not a PropertyGet: %A" e
            Unchecked.defaultof<_>

    let getMethod expr =
        match expr with
        | P.Call(_, mi, _) -> Reflection.ReadMethod mi
        | P.PropertyGet(_, pi, _) -> Reflection.ReadMethod (pi.GetGetMethod())
        | _ ->              
            failwithf "Reflection error in RouterInfer.Client, not a Call or PropertyGet: %A" expr
    
    let rEmptyOp = getMethod <@ RouterOperators.JSEmpty() @>
    let rStringOp = getMethod <@ RouterOperators.rString @>
    let rCharOp = getMethod <@ RouterOperators.rChar @>
    let rGuidOp = getMethod <@ RouterOperators.rGuid @>
    let rBoolOp = getMethod <@ RouterOperators.rBool @>
    let rIntOp = getMethod <@ RouterOperators.rInt @>
    let rDoubleOp = getMethod <@ RouterOperators.rDouble @>
    let rDateTimeOp = getMethod <@ RouterOperators.rDateTime @>
    let rSByteOp = getMethod <@ RouterOperators.rSByte @>
    let rByteOp = getMethod <@ RouterOperators.rByte @>
    let rInt16Op = getMethod <@ RouterOperators.rInt16 @>
    let rUInt16Op = getMethod <@ RouterOperators.rUInt16 @>
    let rUInt32Op = getMethod <@ RouterOperators.rUInt @>
    let rInt64Op = getMethod <@ RouterOperators.rInt64 @>
    let rUInt64Op = getMethod <@ RouterOperators.rUInt64 @>
    let rSingleOp = getMethod <@ RouterOperators.rSingle @>
    let TupleOp = getMethod <@ RouterOperators.JSTuple [||] @>
    let ArrayOp = getMethod <@ RouterOperators.JSArray RouterOperators.rInt @>
    let ListOp = getMethod <@ RouterOperators.JSList RouterOperators.rInt @>
    let RecordOp = getMethod <@ RouterOperators.JSRecord null [||] @>
    let UnionOp = getMethod <@ RouterOperators.JSUnion null [||] @>
    let QueryOp = getMethod <@ RouterOperators.JSQuery null RouterOperators.rInt @>
    let QueryOptionOp = getMethod <@ RouterOperators.JSQueryOption null RouterOperators.rInt @>
    let QueryNullableOp = getMethod <@ RouterOperators.JSQueryNullable null RouterOperators.rInt @>
    let FormDataOp = getMethod <@ RouterOperators.JSFormData RouterOperators.rInt @>
    let JsonOp = getMethod <@ RouterOperators.JSJson<int> @>
    let NullableOp = getMethod <@ RouterOperators.JSNullable RouterOperators.rInt @>
    let ClassOp = getMethod <@ RouterOperators.JSClass (fun () -> null) [||] [||] [||] @>
    let BoxOp = getMethod <@ RouterOperators.JSBox Unchecked.defaultof<_> @>
    let DelayOp = getMethod <@ RouterOperators.JSDelayed Unchecked.defaultof<_> @>
    let rWildCardOp = getMethod <@ RouterOperators.rWildcard @>
    let rWildCardArrayOp = getMethod <@ RouterOperators.rWildcardArray RouterOperators.rInt @>
    let rWildCardListOp = getMethod <@ RouterOperators.rWildcardList RouterOperators.rInt @>
    let rCorsOp = getMethod <@ RouterOperators.rCors Unchecked.defaultof<_> @>

    let (|T|) (t: TypeDefinition) = t.Value.FullName
    let (|C|_|) (t: Type) =
        match t with 
        | ConcreteType { Entity = e; Generics = g} -> Some (e, g)
        | _ -> None

    let getAnnot attrsOpt =
        match attrsOpt with
        | Some attrs -> attrReader.GetAnnotation attrs
        | _ -> Annotation.Empty

    let getAnnotNamed name attrsOpt =
        match attrsOpt with
        | Some attrs -> attrReader.GetAnnotation(attrs, name)
        | _ -> Annotation.Empty

type RoutingMacro() =
    inherit Macro()
    
    let mutable allJSClassesInitialized = false
    let allJSClasses = Dictionary()
    let parsedClassEndpoints = Dictionary()

    let someOf x = Object [ "$", MemberKind.Simple, cInt 1; "$0", MemberKind.Simple, x ]
    let none = Value Null
    let optOf x =
        match x with
        | None -> none
        | Some v -> someOf (cString v)

    override this.TranslateCall(c) =
        match c.Method.Generics with
        | t :: _ -> 
            if t.IsParameter then MacroNeedsResolvedTypeArg t else // todo on inner generics too

            let comp = c.Compilation
            let top = comp.AssemblyName.Replace(".","$") + "_Router"
            let deps = HashSet()

            let recurringOn = Dictionary()

            let rec getRouter t =
                match recurringOn.TryGetValue t with
                | false, _ ->
                    let key = M.CompositeEntry [ M.StringEntry top; M.TypeEntry t ]
                    match comp.GetMetadataEntries key with 
                    | M.CompositeEntry [ M.TypeDefinitionEntry gtd; M.MethodEntry gm ] :: _ ->
                        Call(None, NonGeneric gtd, NonGeneric gm, [])
                    | _ ->
                        let genCall =
                            lazy 
                            let gtd, gm, _ = comp.NewGenerated("Router_" + t.DisplayName)
                            gtd, gm, Call(None, NonGeneric gtd, NonGeneric gm, [])
                        recurringOn.Add(t, genCall)
                        let isTrivial, res = createRouter t
                        recurringOn.Remove(t) |> ignore
                        if isTrivial then res else
                        let gtd, gm, call = genCall.Value
                        comp.AddGeneratedCode(gm, Lambda([], None, res))
                        comp.AddMetadataEntry(key, M.CompositeEntry [ M.TypeDefinitionEntry gtd; M.MethodEntry gm ])
                        call
                | true, genCall -> 
                    let _, _, call = genCall.Value
                    Call(None, NonGeneric routerOpsModule, Generic DelayOp [ t ], [ Lambda([], None, call) ])   
            
            and wildCardRouter (t: Type) =
                match t with
                | C (T "System.String", []) ->
                    Call(None, NonGeneric routerOpsModule, NonGeneric rWildCardOp, []) 
                | ArrayType (t, 1) ->
                    Call(None, NonGeneric routerOpsModule, Generic rWildCardArrayOp [ t ], [ getRouter t ])   
                | C (T "Microsoft.FSharp.Collections.FSharpList`1", [ t ]) ->
                    Call(None, NonGeneric routerOpsModule, Generic rWildCardListOp [ t ], [ getRouter t ])   
                | _ ->
                    failwithf "Invalid type for Wildcard field: %O" t

            and queryRouter (t: Type) name r =
                match t with
                | C (T "Microsoft.FSharp.Core.FSharpOption`1", [ t ]) ->
                    Call(None, NonGeneric routerOpsModule, Generic QueryOptionOp [ t ], [ cString name; getRouter t ])    
                | C (T "System.Nullable`1", [ t ]) ->
                    Call(None, NonGeneric routerOpsModule, Generic QueryNullableOp [ t ], [ cString name; getRouter t ])    
                | _ ->
                    Call(None, NonGeneric routerOpsModule, Generic QueryOp [t], [ cString name; r() ])

            and fieldRouter (t: Type) (annot: Annotation) name =
                let name =
                    match annot.EndPoints with
                    | { Path = n } :: _ -> n
                    | _ -> name
                let r() = getRouter t
                match annot.Query with
                | Some _ -> 
                    queryRouter t name r
                | _ ->
                match annot.FormData with
                | Some _ ->
                    Call(None, NonGeneric routerOpsModule, NonGeneric FormDataOp, [ queryRouter t name r ])
                | _ -> 
                match annot.Json with
                | Some _ ->
                    Call(None, NonGeneric routerOpsModule, Generic JsonOp [ t ], [])
                | _ when annot.IsWildcard ->
                    wildCardRouter t
                | _ -> r()

            and createRouter t =
                match t with
                | C (T "System.Object", []) ->
                    true, Call(None, NonGeneric routerOpsModule, NonGeneric rEmptyOp, []) 
                | C (T "System.String", []) ->
                    true, Call(None, NonGeneric routerOpsModule, NonGeneric rStringOp, []) 
                | C (T "System.Char", []) ->
                    true, Call(None, NonGeneric routerOpsModule, NonGeneric rCharOp, []) 
                | C (T "System.Guid", []) ->
                    true, Call(None, NonGeneric routerOpsModule, NonGeneric rGuidOp, []) 
                | C (T "System.Boolean", []) ->
                    true, Call(None, NonGeneric routerOpsModule, NonGeneric rBoolOp, []) 
                | C (T "System.Int32", []) ->
                    true, Call(None, NonGeneric routerOpsModule, NonGeneric rIntOp, []) 
                | C (T "System.Double", []) ->
                    true, Call(None, NonGeneric routerOpsModule, NonGeneric rDoubleOp, []) 
                | C (T "System.DateTime", []) ->
                    true, Call(None, NonGeneric routerOpsModule, NonGeneric rDateTimeOp, []) 
                | C (T "System.SByte", []) ->
                    true, Call(None, NonGeneric routerOpsModule, NonGeneric rSByteOp, []) 
                | C (T "System.Byte", []) ->
                    true, Call(None, NonGeneric routerOpsModule, NonGeneric rByteOp, []) 
                | C (T "System.Int16", []) ->
                    true, Call(None, NonGeneric routerOpsModule, NonGeneric rInt16Op, []) 
                | C (T "System.UInt16", []) ->
                    true, Call(None, NonGeneric routerOpsModule, NonGeneric rUInt16Op, []) 
                | C (T "System.UInt32", []) ->
                    true, Call(None, NonGeneric routerOpsModule, NonGeneric rUInt32Op, []) 
                | C (T "System.Int64", []) ->
                    true, Call(None, NonGeneric routerOpsModule, NonGeneric rInt64Op, []) 
                | C (T "System.UInt64", []) ->
                    true, Call(None, NonGeneric routerOpsModule, NonGeneric rUInt64Op, []) 
                | C (T "System.Single", []) ->
                    true, Call(None, NonGeneric routerOpsModule, NonGeneric rSingleOp, []) 
                | TupleType (ts, _) ->
                    let fields = NewArray (ts |> List.map getRouter)
                    true, Call(None, NonGeneric routerOpsModule, NonGeneric TupleOp, [ fields ])    
                | ArrayType (t, 1) ->
                    true, Call(None, NonGeneric routerOpsModule, Generic ArrayOp [ t ], [ getRouter t ])    
                | C (T "Microsoft.FSharp.Collections.FSharpList`1", [ t ]) ->
                    true, Call(None, NonGeneric routerOpsModule, Generic ListOp [ t ], [ getRouter t ])    
                | C (T "System.Nullable`1", [ t ]) ->
                    true, Call(None, NonGeneric routerOpsModule, Generic NullableOp [ t ], [ getRouter t ])    
                | C (T "WebSharper.Sitelets.Cors`1", [ t ]) ->
                    true, Call(None, NonGeneric routerOpsModule, Generic rCorsOp [ t ], [ getRouter t ]) 
                | C (e, g) ->
                    let getProto() =
                        match comp.GetClassInfo e with
                        | Some cls -> 
                            deps.Add (M.TypeNode e) |> ignore
                            if cls.HasWSPrototype then
                                GlobalAccess cls.Address
                            else
                                Undefined
                        | _ -> Undefined

                    let mutable isTrivial = false

                    let res =
                        match comp.GetCustomTypeInfo e with
                        | M.EnumInfo t ->
                            isTrivial <- true
                            getRouter (NonGenericType t)
                        | M.FSharpRecordInfo r ->
                            let fields =
                                NewArray (
                                    r |> List.map (fun f ->
                                        let fName = f.Name
                                        let fAnnot = comp.GetFieldAttributes(e, fName) |> getAnnotNamed fName
                                        let fTyp = f.RecordFieldType.SubstituteGenerics(Array.ofList g)
                                        NewArray [ cString f.JSName; cBool f.Optional; fieldRouter fTyp fAnnot fName ]
                                    )
                                )
                            Call(None, NonGeneric routerOpsModule, NonGeneric RecordOp, [ getProto(); fields ])    
                        | M.FSharpUnionInfo u ->                        
                            let cases =
                                NewArray (
                                    u.Cases |> List.map (fun c ->
                                        let annot = comp.GetMethodAttributes(e, M.UnionCaseConstructMethod e c) |> getAnnotNamed c.Name
                                        let endpoints =
                                            annot.EndPoints |> List.map (fun e -> 
                                                let paths =
                                                    match S.ReadEndPointString e.Path with
                                                    | [||] -> NewArray []
                                                    | s -> s |> Seq.map cString |> List.ofSeq |> NewArray
                                                NewArray [ optOf e.Method; paths ]
                                            ) |> NewArray
                                        let constant, routers = 
                                            match c.Kind with
                                            | M.ConstantFSharpUnionCase v ->
                                                someOf (Value v), NewArray []
                                            | M.SingletonFSharpUnionCase ->
                                                none, NewArray []
                                            | M.NormalFSharpUnionCase fields ->
                                                let queryFields = annot.Query |> Option.map HashSet
                                                let formDataFields = annot.FormData |> Option.map HashSet
                                                let jsonField = annot.Json |> Option.bind id
                                                let fRouters = 
                                                    fields |> List.map (fun f ->
                                                        let fTyp = f.UnionFieldType.SubstituteGenerics(Array.ofList g)
                                                        let r() = getRouter fTyp
                                                        if queryFields |> Option.exists (fun q -> q.Remove f.Name) then
                                                            queryRouter fTyp f.Name r
                                                        elif formDataFields |> Option.exists (fun q -> q.Remove f.Name) then
                                                            Call(None, NonGeneric routerOpsModule, NonGeneric FormDataOp, [ queryRouter fTyp f.Name r ])
                                                        elif jsonField |> Option.exists (fun j -> j = f.Name) then
                                                            Call(None, NonGeneric routerOpsModule, Generic JsonOp [ fTyp ], [])
                                                        elif annot.IsWildcard then wildCardRouter fTyp
                                                        else r()
                                                    )
                                                if queryFields |> Option.exists (fun q -> q.Count > 0) then
                                                    failwithf "Union case field specified by Query attribute not found: %s" (Seq.head queryFields.Value)
                                                none, NewArray fRouters
                                               
                                        NewArray [ constant; endpoints; routers ]
                                    )
                                )

                            Call(None, NonGeneric routerOpsModule, NonGeneric UnionOp, [ getProto(); cases ])    
                        | M.FSharpUnionCaseInfo _ ->
                            failwithf "Failed to create Router for type %O, F# union case types are not supported yet" t
                        | M.DelegateInfo _ ->
                            failwithf "Failed to create Router for type %O, delegate types are not supported" t
                        | M.FSharpAnonRecordInfo _ ->
                            failwith "Failed to create Router, anonymous F# record types are not supported"
                        | M.StructInfo
                        | M.NotCustomType ->
                            if not allJSClassesInitialized then
                                for td in comp.GetJavaScriptClasses() do
                                    match comp.GetClassInfo(td) with
                                    | Some cls ->
                                        allJSClasses.[td] <- cls
                                    | _ -> ()
                                allJSClassesInitialized <- true
                            match allJSClasses.TryFind e with
                            | Some cls -> 
                                deps.Add (M.TypeNode e) |> ignore
                                let rec getClassAnnotation td =
                                    match parsedClassEndpoints.TryFind(td) with
                                    | Some ep -> ep
                                    | None ->
                                        let b = 
                                            match cls.BaseClass with
                                            | None -> None
                                            | Some bc when bc.Entity = Definitions.Object -> None
                                            | Some bc -> Some ( getClassAnnotation bc)
                                        let thisAnnot = comp.GetTypeAttributes(e) |> getAnnot
                                        let annot = match b with Some b -> Annotation.Combine b thisAnnot | _ -> thisAnnot
                                        parsedClassEndpoints.Add(td, annot)
                                        annot
                                let annot = getClassAnnotation (NonGeneric e) 
                                let endpoints = 
                                    annot.EndPoints |> List.map (fun e ->
                                        e.Method, Route.FromUrl e.Path |> S.GetPathHoles |> fst
                                    ) 
                                let endpoints =
                                    if List.isEmpty endpoints then [None, [||]] else endpoints
                                let subClasses =
                                    let nestedIn = e.Value.FullName + "+"
                                    allJSClasses |> Seq.choose (fun (KeyValue(td, cls)) ->
                                        if td.Value.FullName.StartsWith nestedIn then
                                            match cls.BaseClass with
                                            | Some bc when bc.Entity = e -> Some td
                                            | _ -> None
                                        else None
                                    ) |> List.ofSeq
                                let choice i x = Object [ "$", MemberKind.Simple, cInt i; "$0", MemberKind.Simple, x ] 
                                let rec getAllFields td (cls: M.IClassInfo) = 
                                    let currentFields =
                                        cls.Fields |> Seq.map (fun (KeyValue(fn, f)) -> 
                                            let fAnnot = comp.GetFieldAttributes(td, fn) |> getAnnotNamed fn
                                            fn, (f, fAnnot)
                                        )
                                    match cls.BaseClass with
                                    | None -> currentFields
                                    | Some bc when bc.Entity = Definitions.Object -> currentFields
                                    | Some bc -> Seq.append currentFields (getAllFields bc.Entity allJSClasses.[bc.Entity])
                                let allFieldsArr = getAllFields e cls |> Array.ofSeq
                                let allFields = dict allFieldsArr
                                let allQueryFields =
                                    allFieldsArr |> Seq.choose (
                                        function
                                        | fn, (_, { Query = Some _ }) -> Some fn
                                        | _ -> None
                                    ) |> Set
                                let getFieldRouter fName = 
                                    match allFields.TryFind fName with
                                    | Some (f, fAnnot) ->
                                        let fTyp = f.Type.SubstituteGenerics(Array.ofList g)
                                        let res = fieldRouter fTyp fAnnot fName
                                        match f.CompiledForm with
                                        | M.InstanceField n ->
                                            NewArray [ cString n; cFalse; res ]
                                        | M.IndexedField i ->
                                            NewArray [ cInt i; cFalse; res ]
                                        | M.OptionalField n ->
                                            NewArray [ cString n; cTrue; res ] 
                                        | M.StaticField _
                                        | M.VarField _ ->
                                            failwith "Static field cannot be encoded to URL path"
                                    | _ ->
                                        failwithf "Could not find field %s" fName
                                let routedFieldNames = 
                                    endpoints |> Seq.collect (fun (_, ep) ->
                                        ep |> Seq.choose (fun s ->
                                            match s with
                                            | S.StringSegment _ -> None
                                            | S.FieldSegment fName -> Some fName 
                                        )
                                    ) |> Seq.distinct |> Array.ofSeq
                                let fieldIndexes =
                                    routedFieldNames |> Seq.mapi (fun i n -> n, cInt i) |> dict
                                let fieldRouters =
                                    routedFieldNames
                                    |> Seq.map getFieldRouter
                                    |> List.ofSeq |> NewArray
                                let partsAndFields =
                                    NewArray (
                                        endpoints |> List.map (fun (m, ep) ->
                                            let mutable queryFields = allQueryFields
                                            let explicitSegments =
                                                ep |> Seq.map (function
                                                    | S.StringSegment s -> cString s
                                                    | S.FieldSegment fName ->
                                                        queryFields <- queryFields.Remove fName
                                                        fieldIndexes.[fName]
                                                ) |> List.ofSeq
                                            NewArray [
                                                optOf m;
                                                NewArray (
                                                    explicitSegments @ (
                                                        queryFields |> Seq.map (fun f -> fieldIndexes.[f]) |> List.ofSeq    
                                                    )
                                                )
                                            ]
                                        )
                                    )
                                let subClassRouters =
                                    NewArray (
                                        subClasses |> List.map (fun sc -> getRouter (GenericType sc g))
                                    )
                                let ctor =
                                    Lambda([], None, Ctor(Generic e g, ConstructorInfo.Default(), []))
                                let unboxed = Call(None, NonGeneric routerOpsModule, NonGeneric ClassOp, [ ctor; fieldRouters; partsAndFields; subClassRouters ]) // todo use ctor instead of getProto   
                                Call(None, NonGeneric routerOpsModule, Generic BoxOp [ t ], [ unboxed ])
                            | _ -> failwithf "Failed to create Router for type %O, it does not have the JavaScript attribute" t
                        
                    recurringOn.Remove t |> ignore
                    isTrivial, res
                | _ -> failwithf "Failed to create Router for type %O, invalid shape" t
                
            let res = MacroOk <| getRouter t
            if deps.Count > 0 then
                WebSharper.Core.MacroDependencies (List.ofSeq deps, res)
            else res   

        | _ -> MacroError "Expecting a type argument for RoutingMacro"

[<AutoOpen>]
module InferRouter =

    module Router =
        /// Creates a router based on type shape and WebSharper attributes.
        [<Macro(typeof<RoutingMacro>)>]
        let Infer<'T when 'T: equality>() = 
            S.getRouter typeof<'T>
            |> ServerInferredOperators.Unbox<'T>

        /// Creates a router based on type shape and WebSharper attributes,
        /// that catches wrong method, query and request body errors.
        let InferWithCustomErrors<'T when 'T: equality>() =
            S.getRouter typeof<'T>
            |> ServerInferredOperators.IWithCustomErrors typeof<ParseRequestResult<'T>>
            |> ServerInferredOperators.Unbox<ParseRequestResult<'T>>

        /// Optimized version of Infer to use straight in a Sitelet
        let internal IInfer<'T when 'T: equality>() = 
            S.getRouter typeof<'T>
            |> ServerInferredOperators.IUnbox<'T>

        /// Optimized version of InferWithCustomErrors to use straight in a Sitelet
        let internal IInferWithCustomErrors<'T when 'T: equality>() =
            S.getRouter typeof<'T> 
            |> ServerInferredOperators.IWithCustomErrors typeof<ParseRequestResult<'T>>
            |> ServerInferredOperators.IUnbox<ParseRequestResult<'T>>
