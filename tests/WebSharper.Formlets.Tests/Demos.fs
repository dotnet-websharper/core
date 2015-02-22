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

namespace WebSharper.Formlet.Test

open WebSharper
open WebSharper.Html
open WebSharper.Formlet

module Demo =        
    
    type Address =
        {
            Street : string
            City : string
            Country : string
        }

    [<JavaScript>]
    let AddressFormlet : Formlet<Address> =
        
        let inputF label errMsg =
            Controls.Input ""
            |> Validator.IsNotEmpty errMsg
            |> Enhance.WithValidationIcon
            |> Enhance.WithTextLabel label
        
        Formlet.Yield (fun st ct cnt -> 
            {Street = st; City = ct; Country = cnt})
        <*> inputF "Street" "Empty street not allowed"
        <*> inputF "City" "Empty city not allowed"
        <*> inputF "Country" "Empty country not allowed"
    
    type Contact =
        | Phone of string
        | Address of Address

    type ContactType = Phone | Address

    [<JavaScript>]
    let ContactFormlet () =
        let contactTypeF =
            [
                "Phone", ContactType.Phone 
                "Address" , ContactType.Address
            ]            
            |> Controls.Select 0
            |> Enhance.WithTextLabel "Contact Type"
        Formlet.Do {
            let! contactType = contactTypeF
            return!
                match contactType with
                | ContactType.Address ->
                    Formlet.Map Contact.Address AddressFormlet
                | ContactType.Phone ->
                    Controls.Input ""
                    |> Validator.IsNotEmpty "Enter a valid phone number"
                    |> Enhance.WithValidationIcon
                    |> Enhance.WithTextLabel "Phone"
                    |> Formlet.Map Contact.Phone  
            }  
    
    [<JavaScript>]
    let Main () =
        ContactFormlet ()

[<JavaScriptType>]
type FormletDemo () = 
    inherit Web.Control()

    [<JavaScript>]
    override this.Body = 
        Demo.Main ()
        |> Enhance.WithFormContainer
        |> Formlet.Run ignore
