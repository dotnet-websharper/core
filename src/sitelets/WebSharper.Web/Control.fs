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

namespace WebSharper.Web

open WebSharper
open WebSharper.Core

module M = WebSharper.Core.Metadata
module R = WebSharper.Core.AST.Reflection
//module P = WebSharper.Core.JavaScript.Packager

/// A server-side control that adds a runtime dependency on a given resource.
type Require (t: System.Type) =
    inherit System.Web.UI.Control()

    let t = AST.Reflection.ReadTypeDefinition t
    let req = [M.ResourceNode t]

    interface INode with
        member this.Write(_, _) = ()
        member this.IsAttribute = false
        member this.AttributeValue = None
        member this.Name = None

    interface IRequiresResources with
        member this.Encode(_, _) = []
        member this.Requires = req :> _

    override this.OnLoad _ =
        this.ID <- ScriptManager.Find(base.Page).Register None this

    override this.Render _ = ()

/// A server-side control that adds a runtime dependency on a given resource.
type Require<'T when 'T :> Resources.IResource>() =
    inherit Require(typeof<'T>)

/// A base class for defining custom ASP.NET controls. Inherit from this class,
/// override the Body property and use the new class as a Server ASP.NET
/// control in your application.
[<AbstractClass>]
type Control() =
    inherit System.Web.UI.Control()

    static let gen = System.Random()
    [<System.NonSerialized>]
    let mutable id = System.String.Format("ws{0:x}", gen.Next().ToString())

    override this.ID
        with get () = id
        and set x = id <- x

    override this.OnLoad _ =
        this.ID <- ScriptManager.Find(base.Page).Register (Some id) this

    interface INode with
        member this.IsAttribute = false
        member this.Write (meta, w) =
            w.Write("""<div id="{0}"></div>""", this.ID)
        member this.AttributeValue = None
        member this.Name = None

    [<JavaScript>]
    abstract member Body : IControlBody

    interface IControl with
        [<JavaScript>]
        member this.Body = this.Body
        member this.Id = this.ID

    member this.GetBodyNode() =
        let t = this.GetType()
        let t = if t.IsGenericType then t.GetGenericTypeDefinition() else t
        let m = t.GetProperty("Body").GetGetMethod()
        M.MethodNode (R.ReadTypeDefinition t, R.ReadMethod m)

    interface IRequiresResources with
        member this.Requires =
            this.GetBodyNode() |> Seq.singleton

        member this.Encode(meta, json) =
            [this.ID, json.GetEncoder(this.GetType()).Encode this]

    override this.Render writer =
        writer.WriteLine("<div id='{0}'></div>", this.ID)

open WebSharper.JavaScript
open Microsoft.FSharp.Quotations
open Microsoft.FSharp.Quotations.Patterns

/// Implements a web control based on a quotation-wrapped top-level body.
/// Use the function ClientSide to create an InlineControl.

type private FSV = Reflection.FSharpValue

[<CompiledName "FSharpInlineControl">]
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

    static let ctrlReq = M.TypeNode (R.ReadTypeDefinition typeof<InlineControl<IControlBody>>)

    [<System.NonSerialized>]
    let bodyAndReqs =
        let declType, meth, args, fReqs, subs =
            let elt =
                match elt :> Expr with
                | Coerce (e, _) -> e
                | e -> e
            let rec get subs expr =
                match expr with
                | PropertyGet(None, p, args) ->
                    let m = p.GetGetMethod(true)
                    let dt = R.ReadTypeDefinition p.DeclaringType
                    let meth = R.ReadMethod m
                    dt, meth, args, [M.MethodNode (dt, meth)], subs
                | Call(None, m, args) ->
                    let dt = R.ReadTypeDefinition m.DeclaringType
                    let meth = R.ReadMethod m
                    dt, meth, args, [M.MethodNode (dt, meth)], subs
                | Let(var, value, body) ->
                    get (subs |> Map.add var value) body
                | e -> failwithf "Wrong format for InlineControl at %s: expected global value or function access, got: %A" (getLocation()) e
            get Map.empty elt
        let args, argReqs =
            args
            |> List.mapi (fun i value ->
                let rec get expr =
                    match expr with
                    | Value (v, t) ->
                        let v = match v with null -> WebSharper.Core.Json.Internal.MakeTypedNull t | _ -> v
                        v, M.TypeNode (R.ReadTypeDefinition t)
                    | TupleGet(v, i) ->
                        let v, n = get v
                        FSV.GetTupleField(v, i), n
                    | Var v when subs.ContainsKey v ->
                        get subs.[v]   
                    | _ -> failwithf "Wrong format for InlineControl at %s: argument #%i is not a literal or a local variable" (getLocation()) (i+1)
                get value
            )
            |> List.unzip
        let args = Array.ofList args
        let reqs = ctrlReq :: fReqs @ argReqs
        args, (declType, meth, reqs)

    let args = fst bodyAndReqs
    let mutable funcName = [||]

    [<JavaScript>]
    override this.Body =
        let f = Array.fold (?) JS.Window funcName
        As<Function>(f).ApplyUnsafe(null, args) :?> _

    interface IRequiresResources with
        member this.Encode(meta, json) =
            if funcName.Length = 0 then
                let declType, meth, reqs = snd bodyAndReqs
                let fail() =
                    failwithf "Error in InlineControl at %s: Couldn't find translation of method %s.%s. The method or type should have JavaScript attribute or a proxy, and the assembly needs to be compiled with WsFsc.exe" 
                        (getLocation()) declType.Value.FullName meth.Value.MethodName
                match meta.Classes.TryFind declType with
                | None -> fail()
                | Some cls ->
                    match cls.Methods.TryFind meth with
                    | Some (M.Static a, _, _) ->
                        funcName <- Array.ofList (List.rev a.Value)
                    | Some _ ->
                        failwithf "Error in InlineControl at %s: Method %s.%s must be static and not inlined"
                            (getLocation()) declType.Value.FullName meth.Value.MethodName
                    | None -> fail()
            [this.ID, json.GetEncoder(this.GetType()).Encode this]

        member this.Requires =
            let _, _, reqs = snd bodyAndReqs 
            this.GetBodyNode() :: reqs |> Seq.ofList

open System
open System.Reflection
open System.Linq.Expressions

// TODO: test in arguments: needs .NET 4.5
// open System.Runtime.CompilerServices
//[<CallerFilePath; Optional>] sourceFilePath 
//[<CallerLineNumber; Optional>] sourceLineNumber
[<CompiledName "InlineControl">]
type CSharpInlineControl(elt: System.Linq.Expressions.Expression<Func<IControlBody>>) =
    inherit Control()

    [<System.NonSerialized>]
    let elt = elt

    static let ctrlReq = M.TypeNode (R.ReadTypeDefinition typeof<InlineControl<IControlBody>>)

    [<System.NonSerialized>]
    let bodyAndReqs =
        let reduce (e: Expression) = if e.CanReduce then e.Reduce() else e
        let declType, meth, args, fReqs =
            match reduce elt.Body with
            | :? MemberExpression as e ->
                match e.Member with
                | :? PropertyInfo as p ->
                    let m = p.GetGetMethod(true)
                    let dt = R.ReadTypeDefinition p.DeclaringType
                    let meth = R.ReadMethod m
                    dt, meth, [], [M.MethodNode (dt, meth)]
                | _ -> failwith "member must be a property"
            | :? MethodCallExpression as e -> 
                let m = e.Method
                let dt = R.ReadTypeDefinition m.DeclaringType
                let meth = R.ReadMethod m
                dt, meth, e.Arguments |> List.ofSeq, [M.MethodNode (dt, meth)]
            | e -> failwithf "Wrong format for InlineControl: expected global value or function access, got: %A"  e
        let args, argReqs =
            args
            |> List.mapi (fun i a -> 
                let rec get needType (a: Expression) =
                    match reduce a with
                    | :? ConstantExpression as e ->
                        let v = match e.Value with null -> WebSharper.Core.Json.Internal.MakeTypedNull e.Type | _ -> e.Value
                        v, if needType then M.TypeNode (R.ReadTypeDefinition e.Type) else M.EntryPointNode
                    | :? MemberExpression as e ->
                        let o = 
                            match e.Expression with
                            | null -> null
                            | ee -> fst (get false ee)
                        match e.Member with
                        | :? FieldInfo as f ->
                            f.GetValue(o), if needType then M.TypeNode (R.ReadTypeDefinition f.FieldType) else M.EntryPointNode
                        | :? PropertyInfo as p ->
                            if p.GetIndexParameters().Length > 0 then
                                failwithf "Wrong format for InlineControl in argument #%i, indexed property not allowed" (i+1)
                            p.GetValue(o, null), if needType then M.TypeNode (R.ReadTypeDefinition p.PropertyType) else M.EntryPointNode
                        | m -> failwithf "Wrong format for InlineControl in argument #%i, member access not allowed: %s" (i+1) (m.GetType().Name)
                    | a -> failwithf "Wrong format for InlineControl in argument #%i, expression type: %s" (i+1) (a.GetType().Name)
                get true a
            )
            |> List.unzip
        let args = Array.ofList args
        let reqs = ctrlReq :: fReqs @ argReqs
        args, (declType, meth, reqs)

    let args = fst bodyAndReqs
    let mutable funcName = [||]

    [<JavaScript>]
    override this.Body =
        let f = Array.fold (?) JS.Window funcName
        As<Function>(f).ApplyUnsafe(null, args) :?> _

    interface IRequiresResources with
        member this.Encode(meta, json) =
            if funcName.Length = 0 then
                let declType, meth, reqs = snd bodyAndReqs
                let fail() =
                    failwithf "Error in InlineControl: Couldn't find translation of method %s.%s. The method or type should have JavaScript attribute or a proxy, and the project file needs to include Zafir.CSharp.targets" 
                        declType.Value.FullName meth.Value.MethodName
                match meta.Classes.TryFind declType with
                | None -> fail()
                | Some cls ->
                    match cls.Methods.TryFind meth with
                    | Some (M.Static a, _, _) ->
                        funcName <- Array.ofList (List.rev a.Value)
                    | Some _ -> 
                        failwithf "Error in InlineControl: Method %s.%s must be static and not inlined"
                            declType.Value.FullName meth.Value.MethodName
                    | None -> fail()
            [this.ID, json.GetEncoder(this.GetType()).Encode this]

        member this.Requires =
            let _, _, reqs = snd bodyAndReqs 
            this.GetBodyNode() :: reqs |> Seq.ofList

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
