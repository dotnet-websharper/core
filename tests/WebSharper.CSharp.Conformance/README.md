# C# conformance (.NET vs JavaScript)

Every case runs twice: on .NET (`Net/`, a plain console app) and as JavaScript compiled by this repository's
WebSharper C# compiler (`Js/`, run under Node). `check.mjs` compares the two outputs and checks the result against
`baseline.json`. Tracking issue: #1654.

```sh
dotnet build tests/WebSharper.CSharp.Conformance/Net -c Release
dotnet build tests/WebSharper.CSharp.Conformance/Js -c Release
cd tests/WebSharper.CSharp.Conformance
node check.mjs            # compare with baseline.json
node check.mjs --update   # after a fix: rewrite baseline.json, commit it with the fix
```

`build.cmd RunCSharpConformanceRelease` runs the check (part of `CI-Release`; skipped with `SKIP_CORE_TESTING`).

## Results

Each case is `pass`, `mismatch` (compiles, but JavaScript gives a different result) or `unsupported` (rejected by
WebSharper at compile time, fenced off, see below). `baseline.json` records the status of every case, with both outputs
for the ones that don't pass, so a PR diff of it shows exactly which behaviour changed.

`check.mjs` exits with an error when:

- a case got worse than its baseline status (**regression**), or
- a case is not in `baseline.json` (new case: run `--update`).

Fixed cases and changed mismatch output are reported but don't fail the check; update the baseline in the same commit.

## Writing cases

| Path | What |
|---|---|
| `Cases/*.cs` | the cases, by area, shared by both projects |
| `Cases/Runner.cs` | runs every case; an exception becomes `EXN <Kind>` (on JS followed by ` :: <message>`, which the comparison ignores) |

- A case is `C("area.name", () => "<string>")`. Keep results deterministic, and separate interpolation holes
  (`$"{a},{b}"`): adjacent holes hit #1643 and would mask other differences. `interp.adjacent.numbers` keeps that bug
  visible on its own.
- A case WebSharper rejects at compile time is fenced, so the rest of the suite still builds (`Js/` defines `JS`):

  ```csharp
  #if !JS
              C("name", () => ...),
  #else
              C("name", () => "WS-COMPILE-ERROR"),
  #endif
  ```

  Remove the fence when the gap is fixed. A WebSharper error without a source location (`WEBSHARPERTASK : …`) has to
  be bisected, for example by building one `Cases` file at a time.
- Exception kinds the JavaScript side can't test for yet (`DivideByZeroException`, `NullReferenceException`,
  `NotSupportedException`, `NotImplementedException`, see #1648) are fenced in `Runner.Kind` and report as `Exception`.
