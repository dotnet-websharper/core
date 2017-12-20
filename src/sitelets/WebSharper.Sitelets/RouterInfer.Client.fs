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
            | _, [| M.ParameterObject.Array a |] -> a |> Array.map (M.ParameterObject.ToObj >> unbox<string>)
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
            eprintfn "Reflection error in RouterInfer.Client, not a PropertyGet: %A" e
            Unchecked.defaultof<_>

    let getMethod expr =
        match expr with
        | P.Call(_, mi, _) -> Reflection.ReadMethod mi
        | P.PropertyGet(_, pi, _) -> Reflection.ReadMethod (pi.GetGetMethod())
        | _ ->              
            eprintfn "Reflection error in RouterInfer.Client, not a Call or PropertyGet: %A" expr
            Unchecked.defaultof<_>
    
    let rEmptyOp = getMethod <@ RouterOperators.JSEmpty() @>
    let rStringOp = getMethod <@ RouterOperators.rString @>
    let rCharOp = getMethod <@ RouterOperators.rChar @>
    let rGuidOp = getMethod <@ RouterOperators.rGuid @>
    let rBoolOp = getMethod <@ RouterOperators.rBool @>
    let rIntOp = getMethod <@ RouterOperators.rInt @>
    let rDoubleOp = getMethod <@ RouterOperators.rDouble @>
    let rDateTimeOp = getMethod <@ RouterOperators.rDateTime @>
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
    let ClassOp = getMethod <@ RouterOperators.JSClass (fun () -> null) [||] [||] @>
    let BoxOp = getMethod <@ RouterOperators.JSBox Unchecked.defaultof<_> @>
    let DelayOp = getMethod <@ RouterOperators.JSDelayed Unchecked.defaultof<_> @>
    let rWildCardOp = getMethod <@ RouterOperators.rWildcard @>
    let rWildCardArrayOp = getMethod <@ RouterOperators.rWildcardArray RouterOperators.rInt @>
    let rWildCardListOp = getMethod <@ RouterOperators.rWildcardList RouterOperators.rInt @>

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

    let someOf x = Object [ "$", cInt 1; "$0", x ]
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
                            let gtd, gm, _ = comp.NewGenerated([top; "r"])
                            gtd, gm, Call(None, NonGeneric gtd, NonGeneric gm, [])
                        recurringOn.Add(t, genCall)
                        let isTrivial, res = createRouter t
                        recurringOn.Remove(t) |> ignore
                        if isTrivial then res else
                        let gtd, gm, call = genCall.Value
                        comp.AddGeneratedCode(gm, Lambda([], res))
                        comp.AddMetadataEntry(key, M.CompositeEntry [ M.TypeDefinitionEntry gtd; M.MethodEntry gm ])
                        call
                | true, genCall -> 
                    let _, _, call = genCall.Value
                    Call(None, NonGeneric routerOpsModule, Generic DelayOp [ t ], [ Lambda([], call) ])   
            
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
                | TupleType (ts, _) ->
                    let fields = NewArray (ts |> List.map getRouter)
                    true, Call(None, NonGeneric routerOpsModule, NonGeneric TupleOp, [ fields ])    
                | ArrayType (t, 1) ->
                    true, Call(None, NonGeneric routerOpsModule, Generic ArrayOp [ t ], [ getRouter t ])    
                | C (T "Microsoft.FSharp.Collections.FSharpList`1", [ t ]) ->
                    true, Call(None, NonGeneric routerOpsModule, Generic ListOp [ t ], [ getRouter t ])    
                | C (T "System.Nullable`1", [ t ]) ->
                    true, Call(None, NonGeneric routerOpsModule, Generic NullableOp [ t ], [ getRouter t ])    
                | C (e, g) ->
                    let getProto() =
                        match comp.GetClassInfo e with
                        | Some cls -> 
                            deps.Add (M.TypeNode e) |> ignore
                            if cls.HasWSPrototype then
                                GlobalAccess cls.Address.Value
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
                                        let fAnnot = comp.GetFieldAttributes(e, fName) |> getAnnot
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
                                        let method, path, isEmpty = 
                                            match annot.EndPoint with 
                                            | Some (m, e) -> 
                                                match S.ReadEndPointString e with
                                                | [||] -> m, NewArray [], true
                                                | s -> m, s |> Seq.map cString |> List.ofSeq |> NewArray, false
                                            | _ -> None, NewArray [ cString c.Name ], false 
                                        let m = optOf method
                                        match c.Kind with
                                        | M.ConstantFSharpUnionCase v ->
                                            NewArray [ m; someOf (Value v); path; NewArray [] ]
                                        | M.SingletonFSharpUnionCase ->
                                            NewArray [ m; none; path; NewArray [] ]
                                        | M.NormalFSharpUnionCase fields ->
                                            if isEmpty && not (List.isEmpty fields) then
                                                failwithf "Union case %s.%s with root EndPoint cannot have any fields" e.Value.FullName c.Name
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
                                            NewArray [ m; none; path; NewArray fRouters ]
                                    )
                                )

                            Call(None, NonGeneric routerOpsModule, NonGeneric UnionOp, [ getProto(); cases ])    
                        | M.FSharpUnionCaseInfo _ ->
                            failwithf "Failed to create Router for type %O, F# union case types are not supported yet" t
                        | M.DelegateInfo _ ->
                            failwithf "Failed to create Router for type %O, delegate types are not supported" t
                        | M.StructInfo
                        | M.NotCustomType ->
                            if not allJSClassesInitialized then
                                for td in comp.GetJavaScriptClasses() do
                                    match comp.GetClassInfo(td) with
                                    | Some cls ->
                                        allJSClasses.Add(td, cls)
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
                                            | None -> Annotation.Empty
                                            | Some bc when bc = Definitions.Object -> Annotation.Empty
                                            | Some bc -> getClassAnnotation bc
                                        let annot = comp.GetTypeAttributes(e) |> getAnnot |> Annotation.Combine b
                                        parsedClassEndpoints.Add(td, annot)
                                        annot
                                let annot = getClassAnnotation e 
                                let endpoint = 
                                    match annot.EndPoint with
                                    | Some (_, e) -> e |> Route.FromUrl |> S.GetPathHoles |> fst
                                    | None -> [||]
                                let subClasses =
                                    let nestedIn = e.Value.FullName + "+"
                                    allJSClasses |> Seq.choose (fun (KeyValue(td, cls)) ->
                                        if td.Value.FullName.StartsWith nestedIn then
                                            match cls.BaseClass with
                                            | Some bc when bc = e -> Some td
                                            | _ -> None
                                        else None
                                    ) |> List.ofSeq
                                let choice i x = Object [ "$", cInt i; "$0", x ] 
                                let rec findField f (cls: M.IClassInfo) =
                                    match cls.Fields.TryFind f with
                                    | Some fi -> Some fi
                                    | _ ->
                                        match cls.BaseClass with
                                        | None -> None
                                        | Some bc when bc = Definitions.Object -> None
                                        | Some bc -> findField f allJSClasses.[bc]
                                let partsAndFields =
                                    NewArray (
                                        endpoint |> Seq.map (function
                                            | S.StringSegment s -> choice 0 (cString s)
                                            | S.FieldSegment fName ->
                                                match findField fName cls with
                                                | Some (f, _, fTyp) ->
                                                    let fAnnot = comp.GetFieldAttributes(e, fName) |> getAnnot
                                                    let fTyp = fTyp.SubstituteGenerics(Array.ofList g)
                                                    let res = fieldRouter fTyp fAnnot fName
                                                    match f with
                                                    | M.InstanceField n ->
                                                        choice 1 (NewArray [ cString n; cFalse; res ])
                                                    | M.IndexedField i ->
                                                        choice 1 (NewArray [ cInt i; cFalse; res ])
                                                    | M.OptionalField n ->
                                                        choice 1 (NewArray [ cString n; cTrue; res ]) 
                                                    | M.StaticField _ ->
                                                        failwith "Static field cannot be encoded to URL path"
                                                | _ ->
                                                    failwithf "Could not find field %s" fName
                                        )
                                        |> List.ofSeq
                                    )
                                let subClassRouters =
                                    NewArray (
                                        subClasses |> List.map (fun sc -> getRouter (GenericType sc g))
                                    )
                                let ctor =
                                    Lambda([], Ctor(Generic e g, ConstructorInfo.Default(), []))
                                let unboxed = Call(None, NonGeneric routerOpsModule, NonGeneric ClassOp, [ ctor; partsAndFields; subClassRouters ]) // todo use ctor instead of getProto   
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
            let t = typeof<'T>
            S.getRouter t 
            |> ServerInferredOperators.IWithCustomErrors t 
            |> ServerInferredOperators.Unbox<ParseRequestResult<'T>>

        /// Optimized version of Infer to use straight in a Sitelet
        let internal IInfer<'T when 'T: equality>() = 
            S.getRouter typeof<'T>
            |> ServerInferredOperators.IUnbox<'T>

        /// Optimized version of InferWithCustomErrors to use straight in a Sitelet
        let internal IInferWithCustomErrors<'T when 'T: equality>() =
            let t = typeof<'T>
            S.getRouter t 
            |> ServerInferredOperators.IWithCustomErrors t 
            |> ServerInferredOperators.Unbox<ParseRequestResult<'T>>