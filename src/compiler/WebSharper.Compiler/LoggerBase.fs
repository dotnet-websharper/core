module WebSharper.Compiler.LoggerBase

[<AbstractClass>]
type LoggerBase() =
    let mutable time = System.DateTime.Now     
    
    abstract Error : string -> unit
    abstract Out : string -> unit
    member x.TimedStage name =
        let now = System.DateTime.Now
        sprintf "%s: %O" name (now - time)
        |> x.Out
        time <- now        

type ConsoleLogger() =
    inherit LoggerBase()
    override x.Error s =
        System.Console.Error.WriteLine(s)

    override x.Out s =
        System.Console.Out.WriteLine(s)