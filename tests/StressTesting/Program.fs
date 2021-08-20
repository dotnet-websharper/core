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

[<EntryPoint>]
let main argv =        

    let httpFactory = HttpClientFactory.create()

    let siteletTest = 
        Step.create(
            "sitelet_home",
            clientFactory = httpFactory, 
            timeout = seconds 10,
            execute = fun context ->
                Http.createRequest "GET" (endpointUrl + "sitelet-tests")
                |> Http.send context
    )

    let json = """{"A":"hello","B":123,"C":false}"""
    let jsonPost = 
        Step.create(
            "json_post",
            clientFactory = httpFactory, 
            timeout = seconds 10,
            execute = fun context ->
                Http.createRequest "POST" (endpointUrl + "perf-tests/json-input")
                |> Http.withBody (new StringContent(json))
                |> Http.send context
    )

    let jsonGet = 
        Step.create(
            "json_get",
            clientFactory = httpFactory, 
            timeout = seconds 10,
            execute = fun context ->
                Http.createRequest "GET" (endpointUrl + "sitelet-tests/api/person?id=1")
                |> Http.send context
    )

    Scenario.create "sitelets_testing" [
        siteletTest
        jsonPost
        jsonGet
    ]    
    |> Scenario.withWarmUpDuration(seconds 10)
    |> Scenario.withLoadSimulations [InjectPerSec(rate = 100, during = minutes 1)]
    |> NBomberRunner.registerScenario
    |> NBomberRunner.run
    |> ignore

    0