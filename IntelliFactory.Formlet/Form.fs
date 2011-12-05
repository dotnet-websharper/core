// This file is confidential and proprietary.
//
// Copyright (c) IntelliFactory, 2004-2011.
//
// All rights reserved.  Reproduction or use in whole or in part is
// prohibited without the written consent of the copyright holder.
//-----------------------------------------------------------------
// $end{copyright}

namespace IntelliFactory.Formlet.Base

open System

/// Represent a form
type Form<'Body,'State> =
    {
        Body    : IObservable<Tree.Edit<'Body>>
        Dispose : unit -> unit
        Notify  : obj -> unit
        State   : IObservable<Result<'State>>
    }
        interface IDisposable with
            [<ReflectedDefinition>]
            member this.Dispose() = this.Dispose()
