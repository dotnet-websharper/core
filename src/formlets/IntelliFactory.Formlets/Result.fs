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

namespace IntelliFactory.Formlets.Base

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
