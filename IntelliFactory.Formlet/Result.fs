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

type Result<'T> =
    | Success of 'T
    | Failure of list<string>
    with
        [<ReflectedDefinition>]
        static member Join (res: Result<Result<'T>>) =
            match res with
            | Success s     -> s
            | Failure fs    -> Failure fs

        [<ReflectedDefinition>]
        static member Apply (f: Result<'T -> 'U>) (r: Result<'T>) : Result<'U> =
            match f, r with
            | Success f, Success v      ->
                Success (f v)
            | Success f, Failure fs     ->
                Failure fs
            | Failure fs1, Failure fs2  ->
                Failure (fs1 @ fs2)
            | Failure fs , _            ->
                Failure fs
        [<ReflectedDefinition>]
        static member OfOption (o: option<'T>) : Result<'T> =
            match o with
            | Some v    -> Success v
            | None      -> Failure []

        /// Maps the value.
        [<ReflectedDefinition>]
        static member Map (f: 'T -> 'U) (res: Result<'T>) : Result<'U> =
            match res with
            | Success v     -> Success (f v)
            | Failure fs    -> Failure fs


        /// Converts a sequence of results into a result with a
        /// a sequence of values.
        [<ReflectedDefinition>]
        static member Sequence (rs : seq<Result<'T>>) : Result<list<'T>> =
            let merge (rs : Result<List<'T>>) (r : Result<'T>) =
                match rs with
                | Success vs ->
                    match r with
                    | Success v -> Success (vs @ [v])
                    | Failure fs -> Failure fs
                | Failure fs1 ->
                    match r with
                    | Success v -> Failure fs1
                    | Failure fs2 -> Failure (fs1 @ fs2)
            Seq.fold merge  (Result.Success []) rs
