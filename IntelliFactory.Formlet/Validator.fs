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

namespace IntelliFactory.Formlet.Base
open IntelliFactory.WebSharper

// Interface for basic validation functionality.
type IValidatorProvider =
    // abstract Validate<'T, 'F when 'F :> IFormlet<'B,'T>> : ('T -> bool) ->  string -> 'F -> 'F
    abstract Matches : string -> string -> bool

type Validator [<ReflectedDefinition>](VP: IValidatorProvider) =

    [<ReflectedDefinition>]
    member private this.Validate<'B, 'T, 'F when 'F :> IFormlet<'B,'T>> (f: 'T -> bool) (msg : string) (flet : 'F) : 'F =
        (flet :> IFormlet<'B,'T>).MapResult (fun res ->
            match res with
            | Success v ->
                if f v then
                    Success v
                else
                    Failure [msg]
            | Failure fs -> Failure fs)
        |> unbox

    /// Validator for preventing empty values.
    [<ReflectedDefinition>]
    member this.Is f m flet =
        this.Validate f m flet

    /// Validator for preventing empty values.
    [<ReflectedDefinition>]
    member this.IsNotEmpty<'B, 'F when 'F :> IFormlet<'B,string>> (msg : string) flet =
        this.Validate<'B, string,'F>  (fun s -> s <> "")  msg  flet

    /// Validates a formlet against a regex, with a given error message on failure.
    [<ReflectedDefinition>]
    member this.IsRegexMatch regex msg  flet =
        this.Validate (fun x -> VP.Matches regex x) msg flet

    /// TODO: Check for consistency!
    /// Only accept email addresses.
    [<ReflectedDefinition>]
    member this.IsEmail msg =
        let regex =
            "^[A-Za-z0-9!#$%&'*+/=?^_`{|}~-]+\
            (?:\.[A-Za-z0-9!#$%&'*+/=?^_`{|}~-]+)*\
            @(?:[A-Za-z0-9](?:[A-Za-z0-9-]*[A-Za-z0-9])?\.)+\
            [A-Za-z0-9](?:[A-Za-z0-9-]*[A-Za-z0-9])?$"
        this.IsRegexMatch regex msg

    /// Only accept integer input.
    [<ReflectedDefinition>]
    member this.IsInt msg =
        this.IsRegexMatch "^-?\d+$" msg

    /// Only accept float input.
    [<ReflectedDefinition>]
    member this.IsFloat  msg =
        let patt = "^\s*(\+|-)?((\d+(\.\d+)?)|(\.\d+))\s*$"
        this.IsRegexMatch patt msg

    /// Only accept "true" values.
    [<ReflectedDefinition>]
    member this.IsTrue (msg : string) flet  =
        this.Validate id msg flet

    /// Only accept values greater than the given value.
    [<ReflectedDefinition>]
    member this.IsGreaterThan min (msg: string) flet =
        this.Validate (fun i -> i > min)  msg  flet

    /// Only accept values less than than the given value.
    [<ReflectedDefinition>]
    member this.IsLessThan max (msg: string) flet =
        this.Validate (fun i -> i < max)  msg flet

    /// Only accept values equal to the given value.
    [<ReflectedDefinition>]
    member this.IsEqual value (msg: string) flet =
        this.Validate (fun i -> i = value) msg flet

    /// Only accept values not equal to the the given value.
    [<ReflectedDefinition>]
    member this.IsNotEqual value (msg: string) flet =
        this.Validate (fun i -> i <> value)  msg flet

