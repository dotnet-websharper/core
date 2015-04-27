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

module WebSharper.Tests.Random

open WebSharper
open WebSharper.Testing

[<JavaScript>]
let Tests =
    Section "Random" {

        Test "Next" {
            let r = System.Random()
            ForR 100 (Random.Const ()) (fun () -> Do {
                let n = r.Next(10)
                True (0 <= n && n < 10)
            })
            ForR 100 (Random.Const ()) (fun () -> Do {
                let n = r.Next(-5, 5)
                True (-5 <= n && n < 5)
            }) 
        }

        Test "Guid" {
            let guids = List.init 100 (fun _ -> System.Guid.NewGuid())
            Equal (guids |> Seq.distinct |> Seq.length) 100
        }

    }