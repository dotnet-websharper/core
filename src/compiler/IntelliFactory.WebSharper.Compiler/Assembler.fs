// $begin{copyright}
//
// This file is part of WebSharper
//
// Copyright (c) 2008-2013 IntelliFactory
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

module IntelliFactory.WebSharper.Compiler.Assembler

module C = IntelliFactory.JavaScript.Core
module I = IntelliFactory.WebSharper.Compiler.Inlining
module M = IntelliFactory.WebSharper.Compiler.Metadata
module Ma = IntelliFactory.WebSharper.Core.Macros
module P = IntelliFactory.JavaScript.Packager
module Q = IntelliFactory.WebSharper.Core.Quotations
module Re = IntelliFactory.WebSharper.Core.Remoting
module S = IntelliFactory.JavaScript.Syntax
module V = IntelliFactory.WebSharper.Compiler.Validator

let Assemble (logger: Logger) (iP: I.Pool) mP (meta: M.T)
    (assembly: V.Assembly) =
    let trans loc input =
        Translator.Translate logger iP mP meta loc input
    let visitCtor (c: V.Constructor) =
        match c.Kind with
        | V.InlineConstructor js ->
            match iP.Parse js with
            | I.Function f -> c.Slot.Method <- P.Syntax f
            | _ -> ()
        | V.JavaScriptConstructor q ->
            c.Slot.Method <-
                trans c.Location q
                |> Corrector.Correct (Corrector.Constructor c.Currying)
                |> C.Optimize
                |> P.Core
        | V.MacroConstructor (_, x) ->
            if x.Body.IsSome then
                c.Slot.Method <-
                    match x.Body.Value with
                    | Ma.CoreBody x -> P.Core (C.Optimize x)
                    | Ma.SyntaxBody x -> P.Syntax x
        | V.StubConstructor _ -> ()
    let visitMethod (m: V.Method) =
        match m.Kind with
        | V.InlineMethod js ->
            match iP.Parse js with
            | I.Function f -> m.Slot.Method <- P.Syntax f
            | _  -> ()
        | V.JavaScriptMethod q ->
            m.Slot.Method <-
                trans m.Location q
                |> Corrector.Correct (Corrector.Method (m.Currying, m.Scope))
                |> C.Optimize
                |> P.Core
        | V.MacroMethod (_, x) ->
            if x.Body.IsSome then
                m.Slot.Method <-
                    match x.Body.Value with
                    | Ma.CoreBody x -> P.Core (C.Optimize x)
                    | Ma.SyntaxBody x -> P.Syntax x
        | V.RemoteMethod _ -> ()
        | V.StubMethod -> ()
    let visitProp (p: V.Property) =
        match p.Kind with
        | V.InlineModuleProperty js ->
            match iP.Parse js with
            | I.Function f -> p.Slot.Field <- P.Syntax f
            | _ -> ()
        | V.JavaScriptModuleProperty q ->
            p.Slot.Field <-
                trans p.Location q
                |> Corrector.Correct Corrector.Field
                |> C.Optimize
                |> P.Core
        | V.StubProperty -> ()
        | V.InterfaceProperty -> ()
        | V.FieldProperty _ -> ()
        | V.BasicProperty (g, s) ->
            Option.iter visitMethod g
            Option.iter visitMethod s
    let rec visitType (t: V.Type) =
        match t.Kind with
        | V.Class (slot, bT, ctors, _) ->
            if bT.IsSome then
                match meta.DataType bT.Value.DeclaringType with
                | Some (M.Class addr) -> slot.BaseType <- Some addr
                | _ -> ()
            List.iter visitCtor ctors
        | _ -> ()
        List.iter visitMethod t.Methods
        List.iter visitProp t.Properties
        List.iter visitType t.Nested
    List.iter visitType assembly.Types
