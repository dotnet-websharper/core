// $begin{copyright}
//
// This file is part of WebSharper
//
// Copyright (c) 2008-2014 IntelliFactory
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

namespace IntelliFactory.WebSharper.Compiler

open System
open System.IO
open System.Reflection
open Nessos.FsPickler

module AppDomainUtility =

    let Pickle (x: 'T) : byte [] =
        let fsp = FsPickler()
        use stream = new MemoryStream()
        fsp.Serialize<'T>(stream, x)
        stream.ToArray()

    let Unpickle (x: byte[]) : 'T =
        let fsp = FsPickler()
        use stream = new MemoryStream(x)
        fsp.Deserialize<'T>(stream)

    type ITransform<'T1,'T2> =
        abstract Do : 'T1 -> 'T2

    type TypeMarker<'T> =
        | TypeMarker

    let MarkType<'T> : TypeMarker<'T> =
        TypeMarker

    type OpaqueJob =
        {
            Input : byte []
            T : Type
            T1 : Type
            T2 : Type
        }

    type ITransformer =
        abstract Do : byte[] -> byte[]

    [<Sealed>]
    type OpaqueTransformer<'T,'T1,'T2>() =
        interface ITransformer with
            member x.Do(input: byte[]) : byte[] =
                let tr = Activator.CreateInstance(typeof<'T>) :?> ITransform<'T1,'T2>
                input
                |> Unpickle
                |> tr.Do
                |> Pickle

    let OpaqueTransform jobBytes =
        let job = Unpickle jobBytes
        let ty = typedefof<OpaqueTransformer<_,_,_>>.MakeGenericType(job.T, job.T1, job.T2)
        let tr = Activator.CreateInstance(ty) :?> ITransformer
        tr.Do(job.Input)

    let SetupRedirects () =
        AppDomain.CurrentDomain.add_AssemblyResolve(fun h ev ->
            let name = AssemblyName(ev.Name)
            if name.Name = "FSharp.Core" then
                typedefof<list<_>>.Assembly
            else
                null)

    let CreateAppDomain () =
        let setup = AppDomainSetup()
        setup.ShadowCopyFiles <- string true
        setup.ApplicationBase <-
            Assembly.GetExecutingAssembly().Location
            |> Path.GetDirectoryName
        let id = "Slave" + Guid.NewGuid().ToString().GetHashCode().ToString("x")
        AppDomain.CreateDomain(id, null, setup)

    [<Literal>]
    let Input = "Input"

    [<Literal>]
    let Output = "Output"

    let ShieldedLogic () =
        SetupRedirects ()
        let jobBytes = AppDomain.CurrentDomain.GetData(Input) :?> byte []
        let output = OpaqueTransform jobBytes
        AppDomain.CurrentDomain.SetData(Output, output)

    let TransformWithAppDomain<'A,'B,'T when 'T :> ITransform<'A,'B>
                                         and 'T : (new : unit -> 'T)>
        (marker: TypeMarker<'T>) (input: 'A) : 'B =
            let dom = CreateAppDomain ()
            try
                let inst = new 'T() :> ITransform<'A,'B>
                let jobBytes =
                    Pickle {
                        Input = Pickle input
                        T = typeof<'T>
                        T1 = typeof<'A>
                        T2 = typeof<'B>
                    }
                dom.SetData(Input, jobBytes)
                dom.DoCallBack(fun () -> ShieldedLogic ())
                dom.GetData(Output) :?> byte []
                |> Unpickle
            finally
                AppDomain.Unload(dom)

