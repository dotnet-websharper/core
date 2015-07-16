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

module WebSharper.Tests.String

open WebSharper
open WebSharper.JavaScript
open WebSharper.Testing
    
[<JavaScript>]
type Hi() =
    override this.ToString() = "Hello"

[<JavaScript>]
let Tests =

    TestCategory "String" {

        let karina = "Karina"

        Test "Chars" {
            equal (karina.Chars 1) 'a'
        }

        Test "Contains" {
            isTrue (karina.Contains "ar")
            isFalse (karina.Contains "AR")
        }

        Test "CopyTo" {
            let c = Array.create 5 ' '
            karina.CopyTo(3, c, 1, 2)
            equal c [| ' '; 'i'; 'n'; ' '; ' ' |]
        }

        Test "EndsWith" {
            isTrue (karina.EndsWith "ina")
            isFalse (karina.EndsWith "INA")
        }

        Test "IndexOf" {
            equal (karina.IndexOf "rin"  ) 2
            equal (karina.IndexOf "RIN"  ) -1
            equal (karina.IndexOf 'a'    ) 1
            equal (karina.IndexOf('a', 2)) 5
            equal (karina.IndexOf("a", 2)) 5
        }

        Test "Length" {
            equal (karina.Length) 6
            equal ("".Length) 0
        }

        Test "PadLeft" {
            equal (karina.PadLeft 10      ) "    Karina"
            equal (karina.PadLeft(10, '.')) "....Karina"
            equal (karina.PadLeft 1       ) "Karina"
        }

        Test "PadRight" {
            equal (karina.PadRight 10) "Karina    "
            equal (karina.PadRight(10, '.')) "Karina...."
            equal (karina.PadRight 1       ) "Karina"
        }

        Test "Remove" {
            equal (karina.Remove 4) "Kari"
            equal (karina.Remove(2, 3)) "Kaa"
        }

        Test "Replace" {
            equal (karina.Replace("ar", "AR")) "KARina"
            equal (karina.Replace('a', 'A')) "KArinA"
            equal ("aaa".Replace("aa", "a")) "aa"
        }

        Test "Split" {
            let N = System.StringSplitOptions.None
            let R = System.StringSplitOptions.RemoveEmptyEntries
            equal ("a b c".Split(' ')) [| "a"; "b"; "c" |]
            equal ("a b,c".Split(' ', ',')) [| "a"; "b"; "c" |]
            equal ("a,;b;c".Split([| ','; ';' |], N)) [|"a"; ""; "b"; "c"|]
            equal ("a,;b;c".Split([| ','; ';' |], R)) [|"a"; "b"; "c"|]
            equal ("a; b, ; c".Split([| ", "; "; " |], N)) [| "a"; "b"; ""; "c" |]
            equal ("a; b, ; c".Split([| ", "; "; " |], R)) [| "a"; "b"; "c" |]
        }

        Test "StartsWith" {
            isTrue (karina.StartsWith "Kar")
            isFalse (karina.StartsWith "KAR")
        }

        Test "Substring" {
            equal (karina.Substring 3) "ina"
            equal (karina.Substring(3, 2)) "in"
        }

        Test "ToCharArray" {
            equal (karina.ToCharArray()) [|'K'; 'a'; 'r'; 'i'; 'n'; 'a'|]
            equal (karina.ToCharArray(1, 3)) [|'a'; 'r'; 'i'|]
        }

        Test "ToLower" {
            equal (karina.ToLower()) "karina"
        }

        Test "ToUpper" {
            equal (karina.ToUpper()) "KARINA"
        }

        Test "Trim" {
            equal ("   Karina  ".Trim()) karina
        }

        Test "String.collect" {
            equal (String.collect (fun x -> "`" + string x) "abc") "`a`b`c"
        }

        Test "String.concat" {
            equal (String.concat "," []) ""
            equal (String.concat "," ["a"; "b"; "c"]) "a,b,c"
        }

        Test "String.exists" {
            isTrue (String.exists System.Char.IsDigit "abc1")
            isFalse (String.exists System.Char.IsDigit "abc")
        }

        Test "String.forall" {
            isTrue (String.forall System.Char.IsDigit "123")
            isFalse (String.forall System.Char.IsDigit "12a")
        }

        Test "String.init" {
            equal (String.init 10 (fun i -> string (char (i + int 'a')))) "abcdefghij"
        }

        Test "String.iter" {
            let r = ref 0
            String.iter (fun x -> r := !r + int x) "abc"
            equal !r 294
        }

        Test "String.iteri" {
            let r = ref 0
            String.iteri (fun i x -> r := i + !r + int x) "abc"
            equal !r 297
        }

        Test "String.map" {
            equal (String.map (fun c -> char (int c + 1)) "abc") "bcd"
        }

        Test "String.mapi" {
            equal (String.mapi (fun i c -> char (int c + i)) "abc") "ace"
        }

        Test "String.replicate" {
            equal (String.replicate 3 "abc") "abcabcabc"
        }

        Test "GetStringSlice" {
            equal (karina.[.. 2]) "Kar"
            equal (karina.[4 ..]) "na" 
            equal (karina.[2 .. 4]) "rin" 
        }  
    
        Test "string" {
            equal (string "abc") "abc"
            equal (string 123) "123"
            equal (string (Hi())) "Hello"
        }               

        Test "Extensions" {
            equal ("abc".JS.CharAt(1)) "b"
            equal (String.FromCharCode(72, 69, 76, 76, 79)) "HELLO" 
        }

        #if FSHARP40

        Test "String.filter" {
            equal (String.filter (fun _ -> true) "") ""
            equal (String.filter ((=) 'a') "abba") "aa"
        }

        #endif

    }
