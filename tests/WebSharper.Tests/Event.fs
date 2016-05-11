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

module WebSharper.Tests.Event

open WebSharper
open WebSharper.Testing

[<JavaScript>]
let Tests =
    TestCategory "Event" {

        Test "SimpleAdd" {
            do 
                let e = Event<int>()
                let res = ref 0
                e.Publish.Add(fun x -> res := x)
                e.Trigger 1
            equal 1 1
        }

        Test "Add" {
            let e = Event<int>()
            let res = ref 0
            e.Publish.Add(fun x -> res := x)
            e.Trigger 1
            equal !res 1
            e.Trigger 2
            equal !res 2
        }

        Test "AddHandler/RemoveHandler" {
            let e = Event<int>()
            let res = ref 0
            let handler = Handler(fun _ x -> res := x)
            e.Publish.AddHandler(handler)
            e.Trigger 1
            equal !res 1
            e.Publish.RemoveHandler(handler)
            e.Trigger 2
            equal !res 1
        }

        Test "Subscribe" {
            let e = Event<int>()
            let res = ref 0
            let sub = e.Publish.Subscribe(fun x -> res := x)
            e.Trigger 1
            equal !res 1
            sub.Dispose()
            e.Trigger 2
            equal !res 1
        }
    }
