module WebSharper.Compiler.LoggerBase

[<AbstractClass>]
type LoggerBase() =
    abstract Error : string -> unit
    abstract Out : string -> unit
    member x.TimedOut() =
        let mutable time = None

        let start() = time <- Some System.DateTime.Now 
        let timed name =
            match time with
            | Some t ->
                let now = System.DateTime.Now
                sprintf "%s: %O" name (now - t)
                |> x.Out
                time <- Some now
            | _ -> ()
        start, timed
        
type ConsoleLogger() =
    inherit LoggerBase()
    override x.Error s =
        System.Console.Error.WriteLine(s)

    override x.Out s =
        System.Console.Out.WriteLine(s)


