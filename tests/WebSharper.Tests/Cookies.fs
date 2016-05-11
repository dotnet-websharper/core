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

module WebSharper.Tests.Cookies

open WebSharper
open WebSharper.JavaScript
open WebSharper.Testing

[<JavaScript>]
let Tests =

    TestCategory "Cookies" {

        Test "Set and get" {
            let key = "somerandomkey"
            let value = "somerandomvalue"
            Cookies.Set(key, value)
            let got = Cookies.Get(key)
            Cookies.Expire(key)
            equal got (Some value)
        }

        Test "Set and expire" {
            let key = "somelessrandomkey"
            let value = "somelessrandomvalue"
            Cookies.Set(key, value)
            Cookies.Expire(key)
            let got = Cookies.Get(key)
            equal got None
        }

        Test "Set in the past" {
            let key = "sometotallynotrandomkey"
            let value = "sometotallynotrandomvalue"
            Cookies.Set(key, value, Cookies.Options(Expires = Date(2000, 10, 10)))
            let got = Cookies.Get(key)
            Cookies.Expire(key)
            equal got None
        }

        Test "Key and value encoding" {
            property (fun (key, value) -> Do {
                let key = "someactuallyrandomkey" + key
                Cookies.Set(key, value)
                let got = Cookies.Get(key)
                Cookies.Expire(key)
                equal got (Some value)
            })
        }

    }
