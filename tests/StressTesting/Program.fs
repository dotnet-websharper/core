open NBomber.Contracts
open NBomber.FSharp
open NBomber.Plugins.Http.FSharp
open NBomber.Plugins.Network.Ping
open NBomber.Time
open FSharp.Control.Tasks
open System.IO
open System.Text.Json
open System.Net.Http
open System.Net.Http.Json

let endpointUrl = "https://localhost:44336/"

type RecTest =
    {
        A : string
        B : int
        C : bool
    }

[<EntryPoint>]
let main argv =        

    let httpFactory = HttpClientFactory.create()

    let jsonObj = { A = "hello"; B = 123; C = false }
    let json = """{"A":"hello","B":123,"C":false}"""

    let jsonPost = 
        Step.create(
            "json_testing",
            clientFactory = httpFactory,
            execute = fun context ->
                Http.createRequest "POST" (endpointUrl + "perf-tests/json-input")
                //|> Http.withHeader "Accept" "text/html"
                //|> Http.withBody (JsonContent.Create(jsonObj))
                |> Http.withBody (new StringContent(json))
                |> Http.send context
    )

    Scenario.create "post_to_engine" [jsonPost]    
    |> Scenario.withWarmUpDuration(seconds 10)
    |> Scenario.withLoadSimulations [InjectPerSec(rate = 5, during = seconds 20)]
    |> NBomberRunner.registerScenario
    |> NBomberRunner.run
    |> ignore

    0