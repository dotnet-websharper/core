// $begin{copyright}
//
// This file is part of WebSharper
//
// Copyright (c) 2008-2015 IntelliFactory
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

namespace WebSharper.Web

open WebSharper

module M = WebSharper.Core.Metadata
module R = WebSharper.Core.AST.Reflection
//module P = WebSharper.Core.JavaScript.Packager

open WebSharper.Core

/// A base class for defining custom ASP.NET controls. Inherit from this class,
/// override the Body property and use the new class as a Server ASP.NET
/// control in your application.
[<AbstractClass>]
type Control() =
    inherit System.Web.UI.Control()

    static let gen = System.Random()
    [<System.NonSerialized>]
    let mutable isR = true
    [<System.NonSerialized>]
    let mutable id = System.String.Format("ws{0:x}", gen.Next().ToString())

    override this.ID
        with get () = id
        and set x = id <- x; isR <- false

    override this.OnLoad _ =
        this.ID <-
            ScriptManager.Find(base.Page).Register
                (if isR then None else Some id) this

    interface INode with
        member this.IsAttribute = false
        member this.Write (meta, w) =
            w.Write("""<div id="{0}"></div>""", this.ID)

    [<JavaScript>]
    abstract member Body : IControlBody
    [<JavaScript>]
    default this.Body = Unchecked.defaultof<_>

    interface IControl with
        [<JavaScript>]
        member this.Body = this.Body
        member this.Id = this.ID

    interface IRequiresResources with
        member this.Requires =
            let t = this.GetType()
            let t = if t.IsGenericType then t.GetGenericTypeDefinition() else t
            let m = t.GetProperty("Body").GetGetMethod()
            [M.MethodNode (R.getTypeDefinition t, WebSharper.Core.Utilities.Hashed (R.getMethod m))] :> seq<_>

        member this.Encode(meta, json) =
            [this.ID, json.GetEncoder(this.GetType()).Encode this]

    override this.Render writer =
        writer.WriteLine("<div id='{0}'></div>", this.ID)

open WebSharper.JavaScript
open Microsoft.FSharp.Quotations
open Microsoft.FSharp.Quotations.Patterns

/// Implements a web control based on a quotation-wrapped top-level body.
/// Use the function ClientSide to create an InlineControl.
type InlineControl<'T when 'T :> IControlBody>(elt: Expr<'T>) =
    inherit Control()

    [<System.NonSerialized>]
    let elt = elt

    let getLocation() =
        let (|Val|_|) e : 't option =
            match e with
            | Quotations.Patterns.Value(:? 't as v,_) -> Some v
            | _ -> None
        let l =
            elt.CustomAttributes |> Seq.tryPick (function
                | NewTuple [ Val "DebugRange";
                             NewTuple [ Val (file: string)
                                        Val (startLine: int)
                                        Val (startCol: int)
                                        Val (endLine: int)
                                        Val (endCol: int) ] ] ->
                    Some (sprintf "%s: %i.%i-%i.%i" file startLine startCol endLine endCol)
                | _ -> None)
        defaultArg l "(no location)"

    static let ctrlReq = M.TypeNode (R.getTypeDefinition typeof<InlineControl<IControlBody>>)

    [<System.NonSerialized>]
    let bodyAndReqs =
        let declType, meth, args, fReqs =
            let elt =
                match elt :> Expr with
                | Coerce (e, _) -> e
                | e -> e
            match elt with
            | PropertyGet(None, p, args) ->
                //let rp = R.Property.Parse p
                let m = p.GetGetMethod()
                let dt = R.getTypeDefinition p.DeclaringType
                let meth = Hashed (R.getMethod m)
                dt, meth, args, [M.MethodNode (dt, meth)]
//                rp.DeclaringType, rp.Name, args, [M.TypeNode rp.DeclaringType]
            | Call(None, m, args) ->
//                let rm = R.Method.Parse m
                let dt = R.getTypeDefinition m.DeclaringType
                let meth = Hashed (R.getMethod m)
                dt, meth, args, [M.MethodNode (dt, meth)]
//                rm.DeclaringType, rm.Name, args, [M.MethodNode rm; M.TypeNode rm.DeclaringType]
            | e -> failwithf "Wrong format for InlineControl at %s: expected global value or function access, got: %A" (getLocation()) e
        let args, argReqs =
            args
            |> List.mapi (fun i -> function
                | Value (v, t) -> v, M.TypeNode (R.getTypeDefinition t)
                | _ -> failwithf "Wrong format for InlineControl at %s: argument #%i is not a literal or a local variable" (getLocation()) (i+1)
            )
            |> List.unzip
        let args = Array.ofList args
        let reqs = ctrlReq :: fReqs @ argReqs :> seq<_>
        args, (declType, meth, reqs)

    let args = fst bodyAndReqs
    let mutable funcName = [||]

    [<JavaScript>]
    override this.Body =
        let f = Array.fold (?) JS.Window funcName
        As<Function>(f).ApplyUnsafe(null, args) :?> _

    interface INode with
        member this.IsAttribute = false
        member this.Write (meta, w) =
            w.Write("""<div id="{0}"></div>""", this.ID)

    interface IRequiresResources with
        member this.Encode(meta, json) =
            if funcName.Length = 0 then
                let declType, meth, reqs = snd bodyAndReqs
                match meta.Classes.TryFind declType with
                | None -> failwithf "Error in InlineControl at %s: Couldn't find address for method" (getLocation())
                | Some cls ->
                    match cls.Methods.TryFind meth with
                    | Some (M.Static a, _) ->
                        funcName <- Array.ofList (List.rev a.Value)
                    | None -> failwithf "Error in InlineControl at %s: Couldn't find address for method" (getLocation())
            [this.ID, json.GetEncoder(this.GetType()).Encode this]

        member this.Requires =
            let _, _, reqs = snd bodyAndReqs
            reqs
                 
//                    let rec mk acc (a: P.Address) =
//                        let acc = a.LocalName :: acc
//                        match a.Parent with
//                        | None -> Array.ofList acc
//                        | Some p -> mk acc p
//                    funcName <- mk [name] a
//            [this.ID, json.GetEncoder(this.GetType()).Encode this]
//        member this.Requires =
//            let _, _, reqs = snd bodyAndReqs
//            reqs

//    interface IRequiresResources with
//        member this.Encode(meta, json) =
//            if funcName.Length = 0 then
//                match meta.Classes.TryFind declType with
//                | None -> failwithf "Error in InlineControl at %s: Couldn't find address for method" (getLocation())
//                | Some cls ->
//                    match cls.Methods.TryFind meth with
//                    | Some (M.Static a, _) ->
//                        funcName <- Array.ofList (List.rev a.Value)
//                | None -> failwithf "Error in InlineControl at %s: Couldn't find address for method" (getLocation())
//                | Some a ->
//                    let rec mk acc (a: P.Address) =
//                        let acc = a.LocalName :: acc
//                        match a.Parent with
//                        | None -> Array.ofList acc
//                        | Some p -> mk acc p
//                    funcName <- mk [name] a
//            reqs

namespace WebSharper

[<AutoOpen>]
module WebExtensions =

    open Microsoft.FSharp.Quotations

    /// Embed the given client-side control body in a server-side control.
    /// The client-side control body must be either a module-bound or static value,
    /// or a call to a module-bound function or static method, and all arguments
    /// must be either literals or references to local variables.
    let ClientSide (e: Expr<#IControlBody>) =
        new WebSharper.Web.InlineControl<_>(e)
