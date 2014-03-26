namespace IntelliFactory.WebSharper.Formlet.Test

open IntelliFactory.WebSharper
open IntelliFactory.WebSharper.Html
open IntelliFactory.WebSharper.Formlet

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
