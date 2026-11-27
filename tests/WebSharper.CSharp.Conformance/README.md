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

Both sides run with `TZ=UTC`, so date cases don't depend on the machine's time zone.

`check.mjs` also type-checks `dts-check.ts` with `strict` and without `skipLibCheck`, against the `.d.ts` files
WebSharper writes for the cases (needs `npm install` in the repository root for `typescript`).

`build.cmd RunCSharpConformanceRelease` runs the check (part of `CI-Release`; skipped with `SKIP_CORE_TESTING`).

## Results

Each case is `pass`, `mismatch` (compiles, but JavaScript gives a different result), `unsupported` (rejected by
WebSharper at compile time, fenced off, see below) or `known`: an accepted difference that follows from JavaScript
itself (no string or boxing identity, erased generics and enums, no reflection, unchecked array bounds). `known.json`
maps each such case to its reason; keep it short and only for differences that are not going to be fixed, and end it
with what to write instead (a C# alternative, or the direct `WebSharper.JavaScript` API).

Which differences are accepted follows one rule: proxy a .NET API when it can follow .NET semantics exactly or very
closely; don't proxy one when that would blow up a clean, small JavaScript representation just for coverage; and where
only a partial implementation is possible (e.g. `System.Type`, which cannot tell apart .NET types sharing a JavaScript
representation), prefer leaving it out and pointing to the direct JavaScript binding.

`baseline.json` records the status of every case, with both outputs
for the ones that don't pass, so a PR diff of it shows exactly which behaviour changed.

`check.mjs` exits with an error when:

- a case got worse than its baseline status (**regression**), or
- a case is not in `baseline.json` (new case: run `--update`), or
- an entry in `known.json` passes or is not a case (remove it).

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
- A JavaScript runtime error (a `TypeError`, say) is not a .NET exception type, so `Runner.Kind` reports it as
  `Exception`; the message after ` :: ` shows what was thrown.
