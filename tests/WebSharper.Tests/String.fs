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

    Section "String" {

        let karina = "Karina"

        Test "Chars" {
            Equal (karina.Chars 1) 'a'
        }

        Test "Contains" {
            True (karina.Contains "ar")
            False (karina.Contains "AR")
        }

        Test "CopyTo" {
            let c = Array.create 5 ' '
            karina.CopyTo(3, c, 1, 2)
            Equal c [| ' '; 'i'; 'n'; ' '; ' ' |]
        }

        Test "EndsWith" {
            True (karina.EndsWith "ina")
            False (karina.EndsWith "INA")
        }

        Test "IndexOf" {
            Equal (karina.IndexOf "rin"  ) 2
            Equal (karina.IndexOf "RIN"  ) -1
            Equal (karina.IndexOf 'a'    ) 1
            Equal (karina.IndexOf('a', 2)) 5
            Equal (karina.IndexOf("a", 2)) 5
        }

        Test "Length" {
            Equal (karina.Length) 6
            Equal ("".Length) 0
        }

        Test "PadLeft" {
            Equal (karina.PadLeft 10      ) "    Karina"
            Equal (karina.PadLeft(10, '.')) "....Karina"
        }

        Test "PadRight" {
            Equal (karina.PadRight 10) "Karina    "
            Equal (karina.PadRight(10, '.')) "Karina...."
        }

        Test "Remove" {
            Equal (karina.Remove 4) "Kari"
            Equal (karina.Remove(2, 3)) "Kaa"
        }

        Test "Replace" {
            Equal (karina.Replace("ar", "AR")) "KARina"
            Equal (karina.Replace('a', 'A')) "KArinA"
            Equal ("aaa".Replace("aa", "a")) "aa"
        }

        Test "Split" {
            let N = System.StringSplitOptions.None
            let R = System.StringSplitOptions.RemoveEmptyEntries
            Equal ("a b c".Split(' ')) [| "a"; "b"; "c" |]
            Equal ("a b,c".Split(' ', ',')) [| "a"; "b"; "c" |]
            Equal ("a,;b;c".Split([| ','; ';' |], N)) [|"a"; ""; "b"; "c"|]
            Equal ("a,;b;c".Split([| ','; ';' |], R)) [|"a"; "b"; "c"|]
            Equal ("a; b, ; c".Split([| ", "; "; " |], N)) [| "a"; "b"; ""; "c" |]
            Equal ("a; b, ; c".Split([| ", "; "; " |], R)) [| "a"; "b"; "c" |]
        }

        Test "StartsWith" {
            True (karina.StartsWith "Kar")
            False (karina.StartsWith "KAR")
        }

        Test "Substring" {
            Equal (karina.Substring 3) "ina"
            Equal (karina.Substring(3, 2)) "in"
        }

        Test "ToCharArray" {
            Equal (karina.ToCharArray()) [|'K'; 'a'; 'r'; 'i'; 'n'; 'a'|]
            Equal (karina.ToCharArray(1, 3)) [|'a'; 'r'; 'i'|]
        }

        Test "ToLower" {
            Equal (karina.ToLower()) "karina"
        }

        Test "ToUpper" {
            Equal (karina.ToUpper()) "KARINA"
        }

        Test "Trim" {
            Equal ("   Karina  ".Trim()) karina
        }

        Test "String.collect" {
            Equal (String.collect (fun x -> "`" + string x) "abc") "`a`b`c"
        }

        Test "String.concat" {
            Equal (String.concat "," []) ""
            Equal (String.concat "," ["a"; "b"; "c"]) "a,b,c"
        }

        Test "String.exists" {
            True (String.exists System.Char.IsDigit "abc1")
            False (String.exists System.Char.IsDigit "abc")
        }

        Test "String.forall" {
            True (String.forall System.Char.IsDigit "123")
            False (String.forall System.Char.IsDigit "12a")
        }

        Test "String.init" {
            Equal (String.init 10 (fun i -> string (char (i + int 'a')))) "abcdefghij"
        }

        Test "String.iter" {
            let r = ref 0
            String.iter (fun x -> r := !r + int x) "abc"
            Equal !r 294
        }

        Test "String.iteri" {
            let r = ref 0
            String.iteri (fun i x -> r := i + !r + int x) "abc"
            Equal !r 297
        }

        Test "String.map" {
            Equal (String.map (fun c -> char (int c + 1)) "abc") "bcd"
        }

        Test "String.mapi" {
            Equal (String.mapi (fun i c -> char (int c + i)) "abc") "ace"
        }

        Test "String.replicate" {
            Equal (String.replicate 3 "abc") "abcabcabc"
        }

        Test "GetStringSlice" {
            Equal (karina.[.. 2]) "Kar"
            Equal (karina.[4 ..]) "na" 
            Equal (karina.[2 .. 4]) "rin" 
        }  
    
        Test "string" {
            Equal (string "abc") "abc"
            Equal (string 123) "123"
            Equal (string (Hi())) "Hello"
        }               

        Test "Extensions" {
            Equal ("abc".JS.CharAt(1)) "b"
            Equal (String.FromCharCode(72, 69, 76, 76, 79)) "HELLO" 
        }

    }