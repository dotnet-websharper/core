.paket/paket.exe restore
$err = $lastexitcode; if ($err -gt 0) { exit $err }
dotnet publish src/compiler/WebSharper.FSharp/WebSharper.FSharp.fsproj -f net461 -v n
$err = $lastexitcode; if ($err -gt 0) { exit $err }
dotnet publish src/compiler/WebSharper.FSharp/WebSharper.FSharp.fsproj -f netcoreapp2.0 -v n
$err = $lastexitcode; if ($err -gt 0) { exit $err }
# dotnet build src/stdlib/WebSharper.Main.Proxies/WebSharper.Main.Proxies.fsproj -v n
dotnet build src/stdlib/WebSharper.JavaScript/WebSharper.JavaScript.fsproj -v n
$err = $lastexitcode; if ($err -gt 0) { exit $err }
