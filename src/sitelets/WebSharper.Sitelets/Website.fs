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
open System.Reflection
open System.Web

/// Define a Sitelets website. This interface must be
/// implemented by the type passed to WebsiteAttribute.
type IWebsite<'Action when 'Action : equality> =
    abstract Actions : list<'Action>
    abstract Sitelet : Sitelet<'Action>

/// The action type for SinglePageWebsite.
[<RequireQualifiedAccess>]
type SinglePageAction = | Index

/// Helper to create a Sitelets website consisting of a single page.
type SinglePageWebsite(page: Page, ?url: string) =

    let sitelet =
        Sitelet.Content (defaultArg url "/") SinglePageAction.Index
            <| Content.PageContent (fun _ -> page)

    interface IWebsite<SinglePageAction> with
        member this.Actions = [SinglePageAction.Index]
        member this.Sitelet = sitelet

type IHostedWebsite<'Action when 'Action : equality> =
    abstract Build : HttpApplication -> IWebsite<'Action>

module internal Specialization =
    open System

    /// Represents a generic algorithm.
    type IGeneric<'T1,'T2> =

        /// Runs the computation. Note the generic 'T3 argument.
        abstract member Run<'T3 when 'T3 : equality> : 'T1 -> 'T2

    /// Specializes the generic algorithm.
    let Specialize<'T1,'T2> (algorithm: IGeneric<'T1,'T2>) : Type -> 'T1 -> 'T2 =
        let aT = typeof<IGeneric<'T1,'T2>>
        let def = aT.GetMethod("Run").GetGenericMethodDefinition()
        let dT = typeof<Converter<'T1,'T2>>
        fun t ->
            let m = def.MakeGenericMethod [| t |]
            let f = Delegate.CreateDelegate(dT, algorithm, m)
            (f :?> Converter<'T1,'T2>).Invoke

module private Utils =
    module S = Specialization

    let GetSitelet : Type -> _ -> _ =
        S.Specialize {
            new S.IGeneric<obj * option<HttpApplication>,Sitelet<obj> * list<obj>> with
                member this.Run<'T when 'T : equality>((website, app)) =
                    let website =
                        match app, website with
                        | Some app, (:? IHostedWebsite<'T> as mk) -> mk.Build(app)
                        | _, (:? IWebsite<'T> as website) -> website
                        | _ -> failwith "Invalid type: IWebsite not implemented."
                    (Sitelet.Upcast website.Sitelet, List.map box website.Actions)
        }

/// Mark an assembly that contains a Sitelets website.
/// The type passed must implement IWebsite.
[<AttributeUsage(AttributeTargets.Assembly)>]
type WebsiteAttribute(ty: System.Type) =
    inherit Attribute()

    let innerType =
        ty.GetInterfaces()
        |> Seq.tryPick (fun iT ->
            if iT.IsGenericType
               && iT.GetGenericTypeDefinition() = typedefof<IWebsite<_>> then
                Some (iT.GetGenericArguments().[0])
            else
                None)

    let website =
        try
            Activator.CreateInstance(ty)
        with
            | :? MissingMethodException ->
                // TODO : Log, i.e. Log.At(ty).Warning("Hello..")
                failwith "Cannot create new instance"
            | :? InvalidCastException ->
                failwith "Type is not implementing IWebsite"

    member this.Run() =
        match innerType with
        | Some t -> Utils.GetSitelet t (website, None)
        | _ -> failwith "TODO"

    member this.Run(app: HttpApplication) =
        match innerType with
        | Some t -> Utils.GetSitelet t (website, Some app)
        | _ -> failwith "TODO"
