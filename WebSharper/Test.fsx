#I "bin/Debug"
#r "IntelliFactory.WebSharper.Core"
#r "IntelliFactory.WebSharper.Compiler"



//
//module FE = IntelliFactory.WebSharper.Compiler.FrontEnd
//
//let compiler = FE.Prepare FE.Options.Default (eprintfn "%O")
//
//compiler.Compile <@ "OKAY" @>
//
//
//
//#r "System.Data.Services.Client"
//#r "FSharp.Data.TypeProviders"
//#r "FSharp.PowerPack.Linq"
//
//open Microsoft.FSharp.Data.TypeProviders
//open System.Net
//open System.Linq
//
//module Basic =
//   
//    let Sales =
//        ["Jan", 1.1; "Feb", 1.2; "Mar", 1.4; "Apr", 1.3]
//   
//    let Expenditures =
//        ["Jan", 1.2; "Feb", 1.25; "Mar", 1.35; "Apr", 1.45]
//   
//    let Chart =
//        [
//            Charting.Cat.Bar Sales
//            |> Charting.WithLegend "Sales"
//            Charting.Cat.Line Expenditures
//            |> Charting.WithLegend "Expenditures"
//        ]
//        |> Charting.Compose
//        |> Charting.WithYBounds 0. 2.
// 
//module Northwind =
// 
//    type Northwind = ODataService<"http://services.odata.org/Northwind/Northwind.svc/">
//       
//    let Db = Northwind.GetDataContext()
//   
//    let Chart =
//        Db.Customers
//        |> Seq.groupBy (fun c -> c.City)
//        |> Seq.map (fun (city, custs) -> city, float (Seq.length custs))
//        |> Charting.Cat.Bar
//        |> Charting.WithYBounds 0. 4.
//   
//module Freebase =
// 
//    let MarketplaceCredentials = NetworkCredential("e81118b7-94ea-4cfa-93fa-2864aca76f03", "X8W53CRLMx+d+VzglSAIM3M/FhIl12Pn4OfNtmVIl8U=")
// 
//    type Crime = ODataService<"https://api.datamarket.azure.com/data.gov/Crimes">
// 
//    let GetCrime (cities: (string * string)[]) =
//        let db = Crime.GetDataContext(Credentials = MarketplaceCredentials)
//        cities
//        |> Array.map (fun (city, state) ->
//            (city, state),
//            query { for c in db.CityCrime do
//                    where (c.State = state)
//                    where (c.City = city)
//                    select (string c.Year, float c.AggravatedAssault) }
//            (*
//            db.CityCrime
//            |> Seq.filter (fun c -> c.State = state && c.City = city)
//            |> Seq.map (fun c -> string c.Year, float c.AggravatedAssault)
//            *)
//            (*
//            Microsoft.FSharp.Linq.Query.query <@ seq {
//                for c in db.CityCrime do
//                    if c.State = state && c.City = city then
//                        yield string c.Year, float c.AggravatedAssault } @>
//            *)
//            |> Array.ofSeq
//        )
// 
//    let DefaultCities =
//        [|
//            "Seattle", "Washington"
//            "Denver", "Colorado"
//        |]
// 
//    let Chart cities =
//        GetCrime cities
//        |> Array.map (fun ((city, state), data) ->
//            Charting.Cat.Bar data
//            |> Charting.WithLegend (city + ", " + state))
//        |> Charting.Compose
//        |> Charting.WithYBounds 0. 1.

