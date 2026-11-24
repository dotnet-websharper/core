module Program

[<EntryPoint>]
let main _ =
    for line in Conformance.Runner.Run () do stdout.WriteLine line
    0
