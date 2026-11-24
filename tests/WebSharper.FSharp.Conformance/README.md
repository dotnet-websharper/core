# F# conformance (.NET vs JavaScript)

The F# counterpart of `../WebSharper.CSharp.Conformance`. Every case runs twice: on .NET (`Net/`, a plain console app)
and as JavaScript compiled by this repository's WebSharper F# compiler (`Js/`, run under Node). `check.mjs` compares the
two outputs and checks the result against `baseline.json`.

```sh
dotnet build tests/WebSharper.FSharp.Conformance/Net -c Release
dotnet build tests/WebSharper.FSharp.Conformance/Js -c Release
cd tests/WebSharper.FSharp.Conformance
node check.mjs            # compare with baseline.json
node check.mjs --update   # after a fix: rewrite baseline.json, commit it with the fix
```

Both sides run with `TZ=UTC`, so date cases don't depend on the machine's time zone.
`build.cmd RunFSharpConformanceRelease` runs the check (part of `CI-Release`; skipped with `SKIP_CORE_TESTING`).

## Results

Each case is `pass`, `mismatch` (compiles, but JavaScript gives a different result), `unsupported` (rejected by
WebSharper at compile time, fenced off, see below) or `known`: an accepted difference that follows from JavaScript
itself (no string or boxing identity, numeric types sharing one representation, no reflection, the JavaScript call
stack). `known.json` maps each such case to its reason, in the form "<difference>. <what to write instead>.": the
website shows the last sentence as the alternative.

Which differences are accepted follows the same rule as for C#: proxy a .NET or FSharp.Core API when it can follow
.NET semantics exactly or very closely; don't when that would blow up a clean, small JavaScript representation just for
coverage; and where only a partial implementation is possible, leave it out and point to the direct JavaScript binding.
Everything else that differs is a `mismatch` or `unsupported` case to fix.

`check.mjs` exits with an error when a case got worse than its baseline status, when a case is missing from
`baseline.json`, or when a `known.json` entry passes or is not a case.

## Writing cases

| Path | What |
|---|---|
| `Cases/H.fs` | helpers: `C "area.name" (fun () -> ...)` is one case; `J`/`Ji` join sequences, `B` prints a bool |
| `Cases/*.fs` | the cases, by area, shared by both projects (each project lists them in compile order) |
| `Cases/Runner.fs` | runs every case; an exception becomes `EXN <Kind>` (on JS followed by ` :: <message>`, which the comparison ignores); newlines in a result are written as `\n` |

- Keep each case on **one line** (the website's export reads them line by line), and keep results deterministic:
  compare only the sign of `compare`, and avoid hashes, `Random` values and platform-dependent conversions.
- A case WebSharper rejects at compile time is fenced, so the rest of the suite still builds (`Js/` defines `JS`):

  ```fsharp
  #if !JS
          C "name" (fun () -> ...)
  #else
          C "name" (fun () -> "WS-COMPILE-ERROR")
  #endif
  ```

  Remove the fence when the gap is fixed. A helper that only the fenced case uses is fenced the same way.
- A JavaScript runtime error (a `TypeError`, say) is not a .NET exception type, so the runner reports it as
  `Exception`; the message after ` :: ` shows what was thrown.
