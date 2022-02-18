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

module WebSharper.Collections.Tests.Implementations

open WebSharper
open WebSharper.JavaScript
open WebSharper.Testing

module R = WebSharper.Testing.RandomValues
        
[<JavaScript>]
let Tests =
    let TestIEnumerable (x: System.Collections.IEnumerable) =
        let e = x.GetEnumerator()
        Do {
            // expecting items 'a', 'c'
            raises (e.Current)
            isTrue (e.MoveNext())
            equal (e.Current) 'a'
            isTrue (e.MoveNext())
            equal (e.Current) 'c'
            isFalse (e.MoveNext())
            raises (e.Current)
        }
        
    let TestIEnumerableGeneric (x: System.Collections.Generic.IEnumerable<char>) =
        let e = x.GetEnumerator()
        Do {
            // expecting items 'a', 'c'
            raises (e.Current)
            isTrue (e.MoveNext())
            equal (e.Current) 'a'
            isTrue (e.MoveNext())
            equal (e.Current) 'c'
            isFalse (e.MoveNext())
            raises (e.Current)
        }
    
    TestCategory "Collection interface implementations" {
        Test "Array as IEnumerable" {
            let arr = [|'a'; 'c'|]
            run (TestIEnumerable arr)
            run (TestIEnumerableGeneric arr)
        }
        
    }
