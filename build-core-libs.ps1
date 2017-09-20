
dotnet build src/stdlib/WebSharper.Main.Proxies/WebSharper.Main.Proxies.fsproj -v n
$err = $lastexitcode; if ($err -gt 0) { exit $err }
dotnet build src/stdlib/WebSharper.Main/WebSharper.Main.fsproj -v n "/flp:Verbosity=normal;LogFile=out.txt"
$err = $lastexitcode; if ($err -gt 0) { exit $err }
