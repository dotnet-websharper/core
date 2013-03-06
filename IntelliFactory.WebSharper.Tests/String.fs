// $begin{copyright}
//
// This file is part of WebSharper
//
// Copyright (c) 2008-2013 IntelliFactory
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

module IntelliFactory.WebSharper.Tests.String

open IntelliFactory.WebSharper
open IntelliFactory.WebSharper.Testing

[<JavaScript>]
let Tests =

    Section "String"

    let karina = "Karina"

    Test "Chars" {
        karina.Chars 1 =? 'a'
    }

    Test "Contains" {
        karina.Contains "ar" =? true
        karina.Contains "AR" =? false
    }

    Test "CopyTo" {
        let c = Array.create 5 ' '
        karina.CopyTo(3, c, 1, 2)
        c =? [| ' '; 'i'; 'n'; ' '; ' ' |]
    }

    Test "EndsWith" {
        karina.EndsWith "ina" =? true
        karina.EndsWith "INA" =? false
    }

    Test "IndexOf" {
        karina.IndexOf "rin"   =? 2
        karina.IndexOf "RIN"   =? -1
        karina.IndexOf 'a'     =? 1
        karina.IndexOf('a', 2) =? 5
        karina.IndexOf("a", 2) =? 5
    }

    Test "Length" {
        karina.Length =? 6
        "".Length =? 0
    }

    Test "PadLeft" {
        karina.PadLeft 10       =? "    Karina"
        karina.PadLeft(10, '.') =? "....Karina"
    }

    Test "PadRight" {
        karina.PadRight 10 =? "Karina    "
        karina.PadRight(10, '.') =? "Karina...."
    }

    Test "Remove" {
        karina.Remove 4 =? "Kari"
        karina.Remove(2, 3) =? "Kaa"
    }

    Test "Replace" {
        karina.Replace("ar", "AR") =? "KARina"
        karina.Replace('a', 'A') =? "KArinA"
    }

    Test "Split" {
        let N = System.StringSplitOptions.None
        let R = System.StringSplitOptions.RemoveEmptyEntries
        "a b c".Split(' ') =? [| "a"; "b"; "c" |]
        "a b,c".Split(' ', ',') =? [| "a"; "b"; "c" |]
        "a,;b;c".Split([| ','; ';' |], N) =? [|"a"; ""; "b"; "c"|]
        "a,;b;c".Split([| ','; ';' |], R) =? [|"a"; "b"; "c"|]
        "a; b, ; c".Split([| ", "; "; " |], N) =? [| "a"; "b"; ""; "c" |]
        "a; b, ; c".Split([| ", "; "; " |], R) =? [| "a"; "b"; "c" |]
    }

    Test "StartsWith" {
        karina.StartsWith "Kar" =? true
        karina.StartsWith "KAR" =? false
    }

    Test "Substring" {
        karina.Substring 3 =? "ina"
        karina.Substring(3, 2) =? "in"
    }

    Test "ToCharArray" {
        karina.ToCharArray() =? [|'K'; 'a'; 'r'; 'i'; 'n'; 'a'|]
        karina.ToCharArray(1, 3) =? [|'a'; 'r'; 'i'|]
    }

    Test "ToLower" {
        karina.ToLower() =? "karina"
    }

    Test "ToUpper" {
        karina.ToUpper() =? "KARINA"
    }

    Test "Trim" {
        "   Karina  ".Trim() =? karina
    }

    Test "String.collect" {
        String.collect (fun x -> "`" + string x) "abc" =? "`a`b`c"
    }

    Test "String.concat" {
        String.concat "," [] =? ""
        String.concat "," ["a"; "b"; "c"] =? "a,b,c"
    }

    Test "String.exists" {
        String.exists System.Char.IsDigit "abc1" =? true
        String.exists System.Char.IsDigit "abc" =? false
    }

    Test "String.forall" {
        String.forall System.Char.IsDigit "123" =? true
        String.forall System.Char.IsDigit "12a" =? false
    }

    Test "String.init" {
        String.init 10 (fun i -> string (char (i + int 'a'))) =? "abcdefghij"
    }

    Test "String.iter" {
        let r = ref 0
        String.iter (fun x -> r := !r + int x) "abc"
        !r =? 294
    }

    Test "String.iteri" {
        let r = ref 0
        String.iteri (fun i x -> r := i + !r + int x) "abc"
        !r =? 297
    }

    Test "String.map" {
        String.map (fun c -> char (int c + 1)) "abc" =? "bcd"
    }

    Test "String.mapi" {
        String.mapi (fun i c -> char (int c + i)) "abc" =? "ace"
    }

    Test "String.replicate" {
        String.replicate 3 "abc" =? "abcabcabc"
    }

