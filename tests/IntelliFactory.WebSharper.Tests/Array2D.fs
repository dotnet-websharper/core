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

module IntelliFactory.WebSharper.Tests.Array2D

open IntelliFactory.WebSharper
open IntelliFactory.WebSharper.Testing
open IntelliFactory.WebSharper.Testing.Assert
module R = IntelliFactory.WebSharper.Testing.Random

[<JavaScript>]
let Tests =
    Section "Array2D"

    Test "Initialize" {
        let n1 = 5
        let n2 = 10
        let value i1 i2 = i1 + i2
        let arr = Array2D.init n1 n2 value

        let test =
            for i in 0 .. (n1 - 1) do
                for j in 0 .. (n2 - 1) do
                    arr.[i, j] =? value i j
        test
    }

    Test "Create" {
        let n1 = 5
        let n2 = 10
        let arr = Array2D.create n1 n2 "hello"
        let test =
            for i in 0 .. n1 - 1 do
                for j in 0 .. n2 - 1 do
                    arr.[i, j] =? "hello"
        test
    }

    Test "Length1" {
        let n1 = 5
        let n2 = 10
        let arr = Array2D.init n1 n2 (fun (x: int) (y: int) -> 5)
        Array2D.length1 arr =? n1
    }

    Test "Length2" {
        let n1 = 5
        let n2 = 10
        let arr = Array2D.init n1 n2 (fun (x: int) (y: int) -> 5)
        Array2D.length2 arr =? n2
    }

    Test "get and set" {
        let n1 = 5
        let n2 = 10
        let arr = Array2D.create n1 n2 0
        arr.[0, 0] =? 0
        arr.[3, 3] =? 0
        arr.[3, 2] <- 100
        arr.[3, 2] =? 100

        Assert.Raises
            (fun () ->
                let x = arr.[-1, 3]
                x - 3 |> ignore )

        Assert.Raises
            (fun () ->
                let x = arr.[3, -1]
                x - 3 |> ignore )

        Assert.Raises
            (fun () ->
                let x = arr.[n1, 0]
                x - 3 |> ignore )

        Assert.Raises
            (fun () ->
                let x = arr.[0, n2]
                x - 3 |> ignore )
    }

    Test "Iterate" {
        let n1 = 5
        let n2 = 10
        let value r c = ref (r + c)
        let arr = Array2D.init n1 n2 value

        Array2D.iter (fun x -> x := !x + 1) arr
        let test =
            for i in 0 .. n1 - 1 do
                for j  in 0 .. n2 - 1 do
                    let cell = arr.[i, j]
                    !cell =? i + j + 1
        test
    }

    Test "IterateIndexed" {
        let n1 = 5
        let n2 = 10
        let arr = Array2D.init n1 n2 (fun r c -> r + c)
        Array2D.iteri (fun i j x -> arr.[i, j] <- x + 1) arr
        let test =
            for i in 0 .. n1 - 1 do
                for j in 0 .. n2 - 1 do
                    arr.[i, j] =? i + j + 1
        test
    }