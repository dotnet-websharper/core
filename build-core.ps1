param(
    [ValidateSet("net4", "netcore", "")]
    [string]
    $target = ""
)

switch($target) {
    "net4" {
        $frameworks = @("net461")
        $arg = @("-f","net461")
    }
    "netcore" {
        $frameworks = @("netcoreapp2.0")
        $arg = @("-f","netcoreapp2.0")
    }
    "" {
        $frameworks = @("net461","netcoreapp2.0")
        $arg = @()
    }
}

.paket/paket.exe restore
$err = $lastexitcode; if ($err -gt 0) { exit $err }

foreach ($framework in $frameworks) {
    dotnet publish src/compiler/WebSharper.FSharp/WebSharper.FSharp.fsproj -f $framework -v n
    $err = $lastexitcode; if ($err -gt 0) { exit $err }
}

& ./build-core-libs.ps1
