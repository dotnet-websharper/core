// $begin{copyright}
// 
// This file is part of WebSharper
// 
// Copyright (c) 2008-2011 IntelliFactory
// 
// GNU Affero General Public License Usage
// WebSharper is free software: you can redistribute it and/or modify it under
// the terms of the GNU Affero General Public License, version 3, as published
// by the Free Software Foundation.
//
// WebSharper is distributed in the hope that it will be useful, but WITHOUT
// ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
// FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License
// for more details at <http://www.gnu.org/licenses/>.
//
// If you are unsure which license is appropriate for your use, please contact
// IntelliFactory at http://intellifactory.com/contact.
//
// $end{copyright}

namespace IntelliFactory.WebSharper.Sitelets

open System
open System.Reflection

type IWebsite<'Action when 'Action : equality> =
    abstract member Sitelet : Sitelet<'Action>
    abstract member Actions : list<'Action>

module internal Specialization =
    open System

    /// Represents a generic algorithm.
    type IGeneric<'T1,'T2> =

        /// Runs the computation. Note the generic 'T3 argument.
        abstract member Run<'T3 when 'T3 : equality> :  'T1 -> 'T2

    /// Specializes the generic algorithm.
    let Specialize<'T1,'T2> (algorithm: IGeneric<'T1,'T2>) : Type -> _ -> _ =
        let aT  = typeof<IGeneric<'T1,'T2>>
        let def = aT.GetMethod("Run").GetGenericMethodDefinition()
        let dT  = typeof<Converter<'T1,'T2>>
        fun t ->
            let m = def.MakeGenericMethod [| t |]
            let f = Delegate.CreateDelegate(dT, algorithm, m)
            (f :?> Converter<'T1,'T2>).Invoke

module private Utils =
    module S = Specialization

    let GetSitelet : Type -> _ -> _ =
        S.Specialize {
            new S.IGeneric<obj,Sitelet<obj> * list<obj>> with
                member this.Run<'T when 'T : equality> website =
                    let website = website :?> IWebsite<'T>
                    Sitelet.Upcast website.Sitelet, (List.map box website.Actions)
        }

[<AttributeUsage(AttributeTargets.Assembly)>]
type WebsiteAttribute(ty: System.Type) =
    inherit System.Attribute()

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
            System.Activator.CreateInstance ty
        with
            | :? System.MissingMethodException ->
                // TODO : Log, i.e. Log.At(ty).Warning("Hello..")
                failwith "Cannot create new instance"
            | :? System.InvalidCastException ->
                failwith "Type is not implementing IWebsite"

    member this.Run () =
        match innerType with
        | Some t ->
            Utils.GetSitelet t website
        | _ ->
            failwith "TODO"
