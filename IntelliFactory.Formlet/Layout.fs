// $begin{copyright}
//
// This file is part of WebSharper
//
// Copyright (c) 2008-2012 IntelliFactory
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

namespace IntelliFactory.Formlet.Base

open System
open IntelliFactory.Reactive

type Layout<'B> =
    {
        Apply : IObservable<Tree.Edit<'B>> ->
                option<'B * IDisposable>
    }

type Container<'Body> =
    {
        Body : 'Body
        SyncRoot : obj
        Insert : int -> 'Body -> unit
        Remove : seq<'Body> -> unit
    }

type internal D[<ReflectedDefinition>]() =
    interface IDisposable with
        [<ReflectedDefinition>]
        member this.Dispose () = ()

type Reactive =
    {
        Reactive : IReactive
    }

/// Defines layout managers and their associated operations.
/// TODO: make private (error with WS???)
type LayoutUtils [<ReflectedDefinition>]  (R: Reactive) =

//    [<ReflectedDefinition>]
//    static member New(r: IReactive) =
//        new LayoutUtils({Reactive = r})

    [<ReflectedDefinition>]
    member this.Default<'B>() : Layout<'B> =
        { Apply = fun _ -> None }

    [<ReflectedDefinition>]
    member this.Delay (f: unit -> Layout<'Body>) =
        { Apply = fun x -> f().Apply x }

    [<ReflectedDefinition>]
    member this.New (container: unit -> Container<'Body>) : Layout<'Body> =
        {
            Apply = fun event ->
                let panel = container ()
                let tree = ref Tree.Empty
                let disp =
                    event.Subscribe ( fun edit ->
                        // Get and delete sub tree to be removed
                        let deletedTree = Tree.ReplacedTree edit !tree
                        tree := Tree.Apply edit !tree
                        let (off, _) = Tree.Range edit !tree
                        panel.Remove deletedTree.Sequence
                        // Insert new nodes
                        edit
                        |> Seq.iteri (fun i e ->
                            panel.Insert (off + i) e
                        )
                    )
                Some (panel.Body, disp)
        }
