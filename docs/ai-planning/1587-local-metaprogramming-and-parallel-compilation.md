# Plan: `localMetaprogramming` opt-in + parallel compilation modes

Tracking issue: [#1587 — Make using macros/generators from current project opt-in](https://github.com/dotnet-websharper/core/issues/1587)
Depends on: [#1547 — remove Reflection reliance on the current project's compiled DLL](https://github.com/dotnet-websharper/core/issues/1547)

## Goal

Today the WebSharper compiler can use **macros and generators defined in the same project that is being compiled** ("local metaprogramming"). To run those, the compiler must reflection-load the project's freshly-emitted DLL during translation. That reflection dependency forces WS translation to run *after* the .NET assembly exists, which blocks parallelizing the WS pipeline with F# compilation.

This work:

1. Introduces a `localMetaprogramming` wsconfig setting (**default `false`, opt-in**) that gates use of current-project macros/generators.
2. When it is off and no reflection-into-current-DLL remains (see #1547), unlocks running WS translation in parallel with F# compilation, exposed through **two** parallel modes.

### Decisions locked in (from planning Q&A)

- **Default**: `localMetaprogramming` defaults to `false` (true opt-in per the issue title). This is a **breaking change**: a project that both *defines* and *uses* a macro/generator in the same assembly must add `"localMetaprogramming": true` to keep compiling. Macros/generators coming from **referenced** assemblies are unaffected and always work.
- **Scope**: includes the pipeline parallelization, not just the setting + gating.
- **Off behavior**: when `localMetaprogramming` is off and the compiler hits a macro/generator whose defining type lives in the current assembly, it emits a **hard error** directing the user to opt in — never a silent fallback that could produce wrong output.
- **F# only**: the parallel modes apply to the F# pipeline. C# already compiles with the real `csc` (WS never owns C# compilation), so there is nothing to parallelize there and C# is out of scope for these settings.
- **Two parallel-compilation settings** (new requirement), which are **independently combinable** (not mutually exclusive):
  - **Mode A — `parallelCompilation`** (wsconfig-level): `wsfsc` keeps owning the F# check via FCS and emits the IL itself; WS translation overlaps the IL emit.
  - **Mode B — `WebSharperExternalFSharpCompiler`** (MSBuild-level): the normal `fsc` produces the DLL as its own build step. This *must* be an MSBuild setting because it flips `SkipCompilerExecution` and target ordering — decisions made before `wsconfig` is ever read.
  - **Combined (A + B) — the tricky case, explicitly supported**: the WS task starts **before** `CoreCompile` and runs its FCS check + translation concurrently with the real `fsc.exe`; then, before writing WS resources into the assembly, it **waits for the DLL to exist** (i.e. for `fsc.exe` to finish). This is the maximal-overlap mode and needs careful join/synchronization.
- **Phase separation**: Phase 1 (the setting + gating) ships **independently** and ahead of Phase 2 (parallelization). Phase 2 depends on #1547.

## Background: how it works today

### Local macro/generator loading

- `WsConfig` (record of all settings) lives in [CommandTools.fs](../../src/compiler/WebSharper.Compiler/CommandTools.fs). `AddJson` parses `wsconfig.json` keys; `RecognizeWebSharperArg` parses CLI flags; `WsConfig.Empty` holds defaults.
- [Compilation.fs](../../src/compiler/WebSharper.Compiler/Compilation.fs) already has `member val UseLocalMacros = true`. In `GetMacroInstance` / `GetGeneratorInstance` it reflection-loads the type via `System.Type.GetType(macro.Value.AssemblyQualifiedName, true)`.
  - `UseLocalMacros = true` → a load failure is a **hard error**.
  - `UseLocalMacros = false` → a load failure is a **warning + fallback** (`Undefined` / `MacroOk Undefined`); used by the runtime "code service" `Compile` static methods where live types are not expected. See [Translator.fs](../../src/compiler/WebSharper.Compiler/Translator.fs) around lines 535 and 1134.
- `UseLocalMacros` is **not** the new switch: its two states are "error vs. warn on load failure", which is a *different, and in the off-case opposite*, concern from "must not use current-project macros". ~~Keep it; add a separate flag.~~ **Superseded during implementation**: the two were merged into a single `LocalMetaprogramming` flag (see "Resolved at planning review" below); `UseLocalMacros` is now an `[<Obsolete>]` alias property with the same backing value.
- A macro/generator's defining assembly is available **without reflection** as `macro.Value.Assembly` (metadata from the reader). The current assembly name is `Compilation.AssemblyName` (set in `ProjectReader.transformAssembly`, e.g. [FSharp ProjectReader.fs:1426](../../src/compiler/WebSharper.Compiler.FSharp/ProjectReader.fs)). So "is this a current-project macro?" is a cheap name comparison, checkable *before* any reflection load.

### Where the primary `Compilation` is created (config in scope)

- F#: [Main.fs:93](../../src/compiler/WebSharper.Compiler.FSharp/Main.fs) — `Compilation(refMeta, this.UseGraphs, SingleNoJSErrors = config.SingleNoJSErrors)`.
- C#: [Main.fs:90](../../src/compiler/WebSharper.Compiler.CSharp/Main.fs) — `Compilation(refMeta, SingleNoJSErrors = config.SingleNoJSErrors)`.
- Code-service paths use `UseLocalMacros = false` (FSharp Main.fs:118, CSharp Main.fs:114) and must be left as-is.

### MSBuild integration

[WebSharper.FSharp.targets](../../msbuild/WebSharper.FSharp.targets):

- `SkipCompilerExecution=True`, `ProvideCommandLineArgs=True`, `ProduceReferenceAssembly=False`.
- The real `fsc` (`CoreCompile`) is skipped; the `WebSharperCompile` target runs `AfterTargets="CoreCompile"` and invokes `WebSharperTask` with `@(FscCommandLineArgs)`.
- So **`wsfsc` currently does everything**: FCS `ParseAndCheckProject` (for the typed AST WS reads) *and* IL emission *and* WS translation. The IL emit and WS translation are today serialized inside `wsfsc`.

## Design

### Part 1 — `localMetaprogramming` setting + gating (foundational)

Add `LocalMetaprogramming: bool` to `WsConfig` (default `false`), thread it to a new `Compilation.LocalMetaprogramming` property (default `false`), and gate current-project macro/generator loading.

Gating logic, added at the top of both `GetMacroInstance` and `GetGeneratorInstance`, **before** the `System.Type.GetType` call:

```
if not this.LocalMetaprogramming && macro.Value.Assembly = this.AssemblyName then
    // current-project metaprogramming is opt-in and not enabled
    if this.UseLocalMacros then
        this.AddError(None, SourceError(
            sprintf "'%s' is a macro/generator defined in the current project. \
                     Set \"localMetaprogramming\": true in wsconfig.json to use it, \
                     or move it to a referenced assembly." macro.Value.FullName))
    None            // return without reflection-loading the current DLL
else
    <existing reflection-load path unchanged>
```

Notes:
- Gating on `this.UseLocalMacros` for the *error* keeps the code-service path (`UseLocalMacros = false`) at its existing warn-and-continue behavior; only real builds hard-error.
- Returning `None` **without** calling `System.Type.GetType` on a current-assembly type is what removes the reflection dependency on the current DLL — the precondition for Part 2.
- Referenced-assembly macros never hit this branch (`Assembly <> AssemblyName`), so they keep working regardless of the setting.

### Part 2 — Parallel compilation (F# only; two combinable modes)

All parallel behavior requires `localMetaprogramming = false` (otherwise WS translation genuinely needs the emitted DLL and cannot be moved earlier). It also requires #1547 (and an audit, below) to guarantee **no other** reflection-into-current-DLL remains during translation. C# is out of scope (already uses `csc`).

**Mode A — `parallelCompilation` (wsconfig-level).**
`wsfsc` keeps owning the F# check (FCS `ParseAndCheckProject`) and IL emission. Because WS translation now reads only the FCS typed AST (`checkProjectResults`) and never the emitted DLL, the IL-emit work and the WS `transformAssembly`/`CompileFull` work can run **concurrently** (kick off IL emission as a task, run WS translation, join before writing WS output into the assembly). Controlled entirely inside `wsfsc`, so it is a **wsconfig setting** (`bool`, default `false`).

**Mode B — `WebSharperExternalFSharpCompiler` (MSBuild-level).**
Let the normal `fsc` run as its own MSBuild step to produce the DLL (`SkipCompilerExecution=False`, restore `ProduceReferenceAssembly` as needed). Because this flips `SkipCompilerExecution` and reorders targets — decisions taken in the `.targets` before `wsconfig` is read — it is an **MSBuild property** (`bool`, default `False`), surfaced as a friendly `.fsproj` / `Directory.Build.props` knob, not a wsconfig key.

**Combined (A + B) — maximal overlap, explicitly supported.**
This is the trickiest case and must work. WS work is **launched before `CoreCompile`** so that WS translation (its own FCS check for the typed AST) proceeds **in parallel with the real `fsc.exe`** emitting the DLL. WS runs as far as it can independently, then blocks until the DLL exists before embedding WS resources into it.

*Why a single blocking pre-`CoreCompile` task cannot work (the constraint that shapes this).* Within one project build, MSBuild executes targets/tasks **sequentially on a single thread**: a task's `Execute()` must return before the next target runs. `WebSharperTask` today is a `ToolTask` whose `Execute()` blocks until `wsfsc` exits ([WebSharperTask.cs](../../src/compiler/WebSharper.MSBuild.FSharp/WebSharperTask.cs)). So if a pre-`CoreCompile` target ran a WS task that blocks waiting for the DLL, `CoreCompile` (fsc) could never start → the DLL never drops → **deadlock**. Therefore the concurrent WS work must live in a **separate OS process** launched non-blocking, and be **joined** by a later target.

*Answering "use a file watcher instead of the `wsfscservice` booster?"* — Yes, that works and is the recommended design, with one caveat: the watcher removes the need for the persistent booster **service** and its handoff protocol, but it does **not** remove the need for two targets (launch + join), because of the single-thread constraint above. Concretely:

1. **Launch target** (`BeforeTargets="CoreCompile"`): a lightweight task calls `Process.Start` on `wsfsc.exe` with a new flag (e.g. `--awaitdll`) and **returns immediately** (does not wait). The launched `wsfsc` starts its FCS check + WS translation right away, and internally uses a `FileSystemWatcher` (with an initial existence check + fallback poll) on `config.AssemblyFile` to await the DLL before its resource-embed step. The file watcher lives **inside the launched process**, not inside the MSBuild task — so MSBuild is free to proceed to `CoreCompile`.
2. `CoreCompile` runs the real `fsc` concurrently and drops the DLL.
3. **Join target** (`AfterTargets="CoreCompile"`, i.e. the existing `WebSharperCompile` slot repurposed): waits for the launched process to exit, surfaces its exit code / logs. This is a thin wait, not a second compile — so it is not really an *extra* target beyond today's `WebSharperCompile`; it changes role from "do everything" to "join".

So the booster service and the file watcher are **alternative synchronization mechanisms** for the same launch+join shape; the file-watcher option is simpler (no service lifecycle, no service protocol) and is the recommended path. The persistent `wsfscservice` booster stays orthogonal — it is a cold-start perf optimization, not required for parallelism.

Synchronization details to resolve during Phase 2:
- **Process handoff** from launch target to join target: pass the PID via an MSBuild output property, or write a `<dll>.wsrunning` marker the join target waits on; prefer waiting on process exit by PID.
- **DLL-complete detection** (avoid reading a partially-written DLL): `FileSystemWatcher` `Changed`/`Renamed` plus a "can open for exclusive read / size stable for N ms" check; fsc typically writes to a temp path and renames, so also watch for `Renamed`.
- **fsc-failure propagation**: if `fsc` fails after WS started, the launched WS process must be cancellable (marker file / kill by PID from the join target) and the build must fail cleanly; mirror the existing `.failed` rename logic in `HandleTaskExecutionErrors` / `clearOutput`.

## Implementation steps

### Phase 1 — setting + gating (ship independently)

Each step below is self-contained enough to hand to a separate implementer. Line numbers are approximate anchors — search for the quoted code.

**Step 1.1 — Add the setting to `WsConfig`** in [CommandTools.fs](../../src/compiler/WebSharper.Compiler/CommandTools.fs):
- In the `WsConfig` record (starts ~line 75), add `LocalMetaprogramming : bool` (place it near `ChangeTracking`/`RemotingProvider` for consistency).
- In `WsConfig.Empty` (~line 121), add `LocalMetaprogramming = false`.
- In `AddJson`'s `for k, v in settings do` match (~line 232), add a case. Note keys are lowercased before matching, so the pattern must be lowercase:
  ```
  | "localmetaprogramming" ->
      res <- { res with LocalMetaprogramming = getBool k v }
  ```
- In `RecognizeWebSharperArg` (~line 610), add a CLI flag using the existing `Flag` active pattern:
  ```
  | Flag "--localmetaprogramming" v -> Some { wsArgs with LocalMetaprogramming = v }
  ```
- Acceptance: `WsConfig.Empty.LocalMetaprogramming = false`; a `wsconfig.json` with `"localMetaprogramming": true` round-trips through `AddJson`.

**Step 1.2 — Add the `Compilation` property + gating** in [Compilation.fs](../../src/compiler/WebSharper.Compiler/Compilation.fs):
- Add `member val LocalMetaprogramming = false with get, set` next to `member val UseLocalMacros = true with get, set` (~line 133).
- In `GetMacroInstance` (~line 410), immediately inside the `| _ ->` branch that builds `res` (before the `maybe { ... System.Type.GetType ... }` block), short-circuit current-project macros:
  ```
  if not this.LocalMetaprogramming && macro.Value.Assembly = this.AssemblyName then
      if this.UseLocalMacros then
          this.AddError(None, SourceError(
              sprintf "Macro '%s' is defined in the current project. Set \"localMetaprogramming\": true in wsconfig.json to use it, or move it to a referenced assembly." macro.Value.FullName))
      macros.Add(macro, None)   // cache the negative result like the existing code does
      None
  else
      <existing maybe { ... } block, then macros.Add(macro, res); res>
  ```
  Mirror the existing caching: today the code does `macros.Add(macro, res); res`. Keep that shape — only add the early branch.
- Do the same in `GetGeneratorInstance` (~line 455) against `gen.Value.Assembly` / `generators`, wording "Generator '%s' is defined in the current project…".
- Confirm `TypeDefinition.Value` exposes `.Assembly` (used elsewhere, e.g. Compilation.fs:737 `Assembly = typ.Value.Assembly`) and `.FullName`.
- Acceptance: with `LocalMetaprogramming = false` and `UseLocalMacros = true`, a current-assembly macro yields the directed error and **no** `System.Type.GetType` call; with `LocalMetaprogramming = true` the existing reflection path runs unchanged; a referenced-assembly macro is never short-circuited.

**Step 1.3 — Wire config → Compilation** at the two primary construction sites (leave the `UseLocalMacros = false` code-service constructors untouched):
- F# [Main.fs:93](../../src/compiler/WebSharper.Compiler.FSharp/Main.fs): `Compilation(refMeta, this.UseGraphs, SingleNoJSErrors = config.SingleNoJSErrors, LocalMetaprogramming = config.LocalMetaprogramming)`.
- C# [Main.fs:90](../../src/compiler/WebSharper.Compiler.CSharp/Main.fs): add `LocalMetaprogramming = config.LocalMetaprogramming` the same way.
- `config` (a `WsConfig`) is already in scope at both sites.

**Step 1.4 — JSON schema**: locate the schema served at the `$schema` URL in [wsconfig.md](../../../docs/content/core/wsconfig.md) (`https://websharper.com/wsconfig.schema.json`; find its source file in the repo/site) and add a `"localMetaprogramming"` boolean property (default `false`) with a one-line description.

**Step 1.5 — Docs**: add a `"localMetaprogramming"` section to [wsconfig.md](../../../docs/content/core/wsconfig.md) in alphabetical order (between `"jsOutput"` and `"outputDir"`), stating: **Type** bool, default `false`; that macros/generators defined and used **within the same project** now require this to be `true`; that referenced-assembly metaprogramming is unaffected; and the migration note. Cross-reference from [Macros.md](../../docs/Macros.md) and [fsharp-code-generators.md](../../../docs/content/metaprogramming/fsharp-code-generators.md).

**Step 1.6 — Update existing same-assembly projects (sweep results)**:

Default-off breaks any project that **defines and applies** a macro/generator in the *same* assembly. A sweep of the `core` and `ui` working trees (macros referencing `M.*` = `WebSharper.Core.Macros`, i.e. the compiler assembly, are referenced-assembly usage and are **unaffected**) found these in-repo projects that must opt in (add `"localMetaprogramming": true`):

| Project | Defines | Applies in-assembly (evidence) |
|---|---|---|
| `tests/WebSharper.Tests` | `Hello{Quotation,AST,JS,String}Generator`, `NameOfMacro`, `AddMacro` | [Macro.fs:84-100](../../tests/WebSharper.Tests/Macro.fs) |
| `tests/WebSharper.Module.Tests` | `TestMacro`, `MacroOnType` | [StubInterfaces.fs:73,76](../../tests/WebSharper.Module.Tests/StubInterfaces.fs) |
| `tests/WebSharper.CSharp.Tests` | `AddMacro` (C#) | [Macro.cs:50](../../tests/WebSharper.CSharp.Tests/Macro.cs) |
| `src/stdlib/WebSharper.Testing` | `Internal.{TestProperty,Property,AutoGenerator,Sample}Macro` (+ a generator in `CSharp.fs`) | Random.fs:502,525,528; Pervasives.fs:1286,1302,1437 |
| `src/sitelets/WebSharper.Web` | `SerializeMacro` | ClientSideJson.fs:972 → TypedJsonProxy.fs:31-40 |
| `src/sitelets/WebSharper.Sitelets` | `RoutingMacro` | RouterInfer.Client.fs:134 → :498 |
| `ui/WebSharper.UI` | `Macros.*` ([Macros.fs](../../../ui/WebSharper.UI/Macros.fs)) | applied in Doc.fs, HTML.fs, Attr.Client.fs, Reactive.fs, ReactiveExtensions.fs, Doc.Client.fs |
| `ui/WebSharper.UI.CSharp` | `RouteMapBuilderMacro` | Routing.fs:112 → :53-59 (its `Macros.*` uses are from referenced `WebSharper.UI` — fine) |
| `ui/WebSharper.UI.Templating.Runtime` | `GetOrLoadTemplateMacro` | RuntimeClient.fs:112 → :195 |

Implications:
- **WebSharper's own core libraries are the primary local-metaprogramming users** — consistent with the issue's premise that *application* projects rarely need it. These libraries will simply opt in and forgo Phase 2 parallelization for their own build; the perf win targets user app projects.
- **Config mechanism**: several of these libraries build via the internal `build-script` (not necessarily the standard `.targets`) and may have **no `wsconfig.json`**. Decide per project between (a) adding a `wsconfig.json` with `"localMetaprogramming": true`, or (b) adding a new MSBuild property `WebSharperLocalMetaprogramming` threaded through [WebSharperTask.cs](../../src/compiler/WebSharper.MSBuild.FSharp/WebSharperTask.cs) → `--localmetaprogramming` (there is currently **no** generic wsconfig→MSBuild passthrough; only named settings are threaded). Recommend (b) so the flag is settable from `.fsproj`/`Directory.Build.props` build-wide; verify how `build-script` invokes `wsfsc` for these.
- Sweep is scoped to `core` + `ui`; **other workspaces not checked here** (e.g. `react`, `mvu`, `charting`, `feliz`, `fable`, `templates`) contain the same pattern and will each need the same treatment when built against the new default.

**Step 1.7 — Expose `localMetaprogramming` as an MSBuild property** (`WebSharperLocalMetaprogramming`). Applies to **both** F# and C# (the gating lives in `Compilation`, shared by both). Follow the existing `--dce` threading pattern so the property can override wsconfig in either direction:
- [WebSharper.FSharp.targets](../../msbuild/WebSharper.FSharp.targets) and [WebSharper.CSharp.targets](../../msbuild/WebSharper.CSharp.targets): add `WebSharperLocalMetaprogramming="$(WebSharperLocalMetaprogramming)"` to the `<WebSharperTask .../>` invocation.
- [WebSharper.MSBuild.FSharp/WebSharperTask.cs](../../src/compiler/WebSharper.MSBuild.FSharp/WebSharperTask.cs) and [WebSharper.MSBuild.CSharp/WebSharperTask.cs](../../src/compiler/WebSharper.MSBuild.CSharp/WebSharperTask.cs): add `public string WebSharperLocalMetaprogramming { get; set; } = "";` and in `WriteAtFileName`, mirroring the existing `--dce` line:
  ```csharp
  if (bool.TryParse(WebSharperLocalMetaprogramming, out bool lmp))
      w.WriteLine("--localmetaprogramming" + (lmp ? "+" : "-"));
  ```
- **Precedence note**: the task writes `--wsconfig:` **last** (F# task line ~112), so a value in `wsconfig.json` currently wins over the CLI flag. For the perf-testing/GHA use case (projects without the key in wsconfig) this is fine, but decide whether the MSBuild property should instead take precedence — if so, move `--wsconfig:` emission before the individual flags, or have `AddJson` skip keys already set from the command line.
- This is also the recommended way to opt-in the internal libraries from Step 1.6 (set `<WebSharperLocalMetaprogramming>true</WebSharperLocalMetaprogramming>` in a shared `Directory.Build.props`), avoiding per-project `wsconfig.json` files — **verify how `build-script` invokes `wsfsc`** for libraries built outside the standard `.targets` and thread the flag there too.

**Step 1.6b — New/negative tests**:
- Extend a `tests/` macro project to assert: compiles clean with `"localMetaprogramming": true`; **fails with the directed error** (assert on message text) when the setting is absent/false.
- Add/confirm a regression guard where the macro comes from a **referenced** assembly and compiles regardless of the setting.
- Run `tests/Scripts/FSharpTester.fsx` and `tests/Scripts/CSharpTester.fsx` (both already pass `UseLocalMacros = false`; the new `LocalMetaprogramming = false` default is inert there because gating's error is behind `UseLocalMacros`).

### Phase 2 — parallelization (after #1547 + audit)

**Step 2.0 — Audit remaining reflection-into-current-DLL** during translation, beyond macros/generators. Enumerate every path that touches the current assembly's compiled IL while translating:
- Anonymous records (#1547) — the known blocker; must land first.
- `System.Type.GetType` / `AssemblyResolver` loads of `config.AssemblyFile`. Trace `createAssemblyResolver … includeCurrent` in [FSharp Compile.fs:46](../../src/compiler/WebSharper.Compiler.FSharp/Compile.fs) (the `includeCurrent` path adds `config.AssemblyFile` to search paths) and where `includeCurrent=true` is passed.
- WIG (`LoadInterfaceGeneratorAssembly` in [CommandTools.fs:443](../../src/compiler/WebSharper.Compiler/CommandTools.fs)) — WIG projects load the current assembly by design; **exclude WIG project types from the parallel modes entirely**.
- Deliverable: a checklist confirming translation reads only FCS output (typed AST) when `localMetaprogramming=false`. Parallel modes must be gated on this holding.

**Step 2.1 — Mode A (wsconfig `parallelCompilation`)**:
- Add `ParallelCompilation : bool` to `WsConfig` following the exact same 4 edits as Step 1.1 (record field, `Empty`, `AddJson` key `"parallelcompilation"`, CLI `--parallelcompilation`), plus schema + docs.
- In the `wsfsc` F# path — `WebSharperFSharpCompiler.Compile` in [FSharp Main.fs:43](../../src/compiler/WebSharper.Compiler.FSharp/Main.fs) and the emit path in [FSharp Compile.fs](../../src/compiler/WebSharper.Compiler.FSharp/Compile.fs): when `config.ParallelCompilation && not config.LocalMetaprogramming`, run FCS **IL emission** (the step producing `config.AssemblyFile`) on a `Task` concurrently with `ProjectReader.transformAssembly` + `Translator.DotNetToJavaScript.CompileFull`; `Task.Wait` before the step that writes WS resources into the assembly (`FrontEnd.HandleExtraFiles` / `out.Save`).
- Guard: if `config.LocalMetaprogramming` is true, ignore `ParallelCompilation` and log a warning (correctness requires the DLL first).
- **MSBuild property** `WebSharperParallelCompilation` (F# only): add to [WebSharper.FSharp.targets](../../msbuild/WebSharper.FSharp.targets) `<WebSharperTask .../>` and [WebSharper.MSBuild.FSharp/WebSharperTask.cs](../../src/compiler/WebSharper.MSBuild.FSharp/WebSharperTask.cs) with the same `--dce`-style threading (`--parallelcompilation+/-`). Not added to the C# targets/task.
- Instrument with `logger.TimedStage`; measure via `--wstimings` / `TimingLog`.

**Step 2.2 — Mode B (MSBuild `WebSharperExternalFSharpCompiler`)**:
- [WebSharper.FSharp.targets](../../msbuild/WebSharper.FSharp.targets): add `<WebSharperExternalFSharpCompiler>` (default `False`). When `True`: do **not** set `SkipCompilerExecution=True` (let `CoreCompile`/real `fsc` produce the DLL), reconsider `ProduceReferenceAssembly`, and change `WebSharperTask` invocation so WS translation runs against the real-`fsc` output rather than doing the F# compile itself.
- [WebSharperTask.cs](../../src/compiler/WebSharper.MSBuild.FSharp/WebSharperTask.cs): add a task parameter (e.g. `ExternalFSharpCompiler`) and, when set, pass a corresponding CLI flag to `wsfsc` so it runs translation-only. `wsfsc` still needs a typed AST — it runs its own FCS check (accepted double front-end; see Risks) unless a shared-check optimization is added later.
- **F# only** — leave [WebSharper.CSharp.targets](../../msbuild/WebSharper.CSharp.targets) unchanged.

**Step 2.3 — Combined mode (A + B together)** — the hard case. Recommended design uses a **launched process + in-process file watcher** (not the persistent `wsfscservice` booster); see the "Combined" design section for why the launch+join two-target shape is unavoidable under MSBuild's single-threaded target execution.
- **`wsfsc` flag** `--awaitdll`: when set, `wsfsc` starts FCS check + WS translation immediately, then before the resource-embed step waits for `config.AssemblyFile` via `FileSystemWatcher` (initial `File.Exists` check + `Renamed`/`Changed` handlers + a "file openable for exclusive read and size-stable" confirmation + a bounded fallback poll and timeout).
- **Launch target** (`BeforeTargets="CoreCompile"`): a task (extend or add alongside `WebSharperTask`) that `Process.Start`s `wsfsc --awaitdll` **non-blocking** and returns immediately; publish the PID as an MSBuild output property and/or write a `<dll>.wsrunning` marker.
- **Join target** (repurpose the existing `WebSharperCompile` slot, `AfterTargets="CoreCompile"`): wait on the launched process by PID, surface exit code and captured output; on WS failure fail the build.
- **fsc-failure propagation**: if `CoreCompile` fails, the join target must terminate the launched WS process (kill by PID / delete a "proceed" marker it watches) and clean up, reusing the `.failed` rename logic in `HandleTaskExecutionErrors` ([WebSharperTask.cs:142](../../src/compiler/WebSharper.MSBuild.FSharp/WebSharperTask.cs)) and `clearOutput` ([FSharp Compile.fs:36](../../src/compiler/WebSharper.Compiler.FSharp/Compile.fs)).
- **Degradation**: with only Mode A set → in-process concurrency (Step 2.1), single blocking `WebSharperCompile` target as today. With only Mode B set → external fsc, WS runs after `CoreCompile` serially. With neither → unchanged. Add explicit tests for all four flag combinations.

**Step 2.4 — Benchmark** using `build-script-perftesting` to quantify Mode A, Mode B, and combined against baseline on representative projects before defaulting anything on. All new flags stay opt-in for the first release.

**Step 2.5 — GHA perf-testing input (extra MSBuild-properties input).** To make each mode easy to A/B in CI without editing project files:
- Add a `workflow_dispatch` **input** (e.g. `msbuildProperties`, free-text, default empty) to [.github/workflows/linux-build.yml](../../.github/workflows/linux-build.yml) (and any other perf/CI workflows), forwarding it to the build:
  ```yaml
  on:
    workflow_dispatch:
      inputs:
        msbuildProperties:
          description: "Extra MSBuild properties, e.g. WebSharperParallelCompilation=true;WebSharperExternalFSharpCompiler=true"
          required: false
          default: ""
  ...
      - name: Build WS Core
        run: ./build.sh CI-Release
        env:
          WS_MSBUILD_PROPERTIES: ${{ github.event.inputs.msbuildProperties }}
  ```
- Thread `WS_MSBUILD_PROPERTIES` through the build entry point: `build.sh`/`build.cmd` → `build.fsx`. In `build.fsx`, read the env var and pass the parsed `name=value` pairs as MSBuild properties on every `DotNet.build`/`publish` invocation (the FAKE `WSTargets`/`build` helper in the `build-script` workspace centralizes these calls — add a `MSBuildParams.Properties` passthrough there). Confirms the property reaches `dotnet build /p:...` for the solution builds.
- This gives one knob to flip `WebSharperLocalMetaprogramming`, `WebSharperParallelCompilation`, and `WebSharperExternalFSharpCompiler` for a whole CI run, feeding the Step 2.4 benchmark. Mirror the same input in `build-script-perftesting` if it has its own workflow.

### MSBuild property surface (summary)

| Setting | wsconfig key | CLI flag | MSBuild property | Targets | Scope |
|---|---|---|---|---|---|
| Local metaprogramming opt-in | `localMetaprogramming` | `--localmetaprogramming` | `WebSharperLocalMetaprogramming` | F# + C# | Phase 1 |
| In-process parallel (WS owns F#) | `parallelCompilation` | `--parallelcompilation` | `WebSharperParallelCompilation` | F# | Phase 2 |
| External `fsc` | — (MSBuild-only) | — | `WebSharperExternalFSharpCompiler` | F# | Phase 2 |

All three are also settable ad-hoc via `dotnet build /p:<Property>=true` and via the GHA `msbuildProperties` input.

## Risks / notes

- **Breaking change — including WebSharper's own libraries**: default-off breaks any project with same-assembly metaprogramming until it opts in. The Step 1.6 sweep shows this includes core/`ui` shipping libraries (`WebSharper.Testing`, `WebSharper.Web`, `WebSharper.Sitelets`, `WebSharper.UI`, …), so **Phase 1 cannot merge until those are opted in and the internal build is green**; other workspaces (react, mvu, …) need the same before they build against the new default. The directed error message is the user-facing mitigation; call it out in release notes and the upgrade guide.
- **`UseLocalMacros` vs `LocalMetaprogramming`**: originally planned as intentionally separate (their off-states have opposite error semantics). **Superseded**: per explicit follow-up direction, they were merged into one flag (`LocalMetaprogramming`); `UseLocalMacros` is now a deprecated alias with the same backing value. Consequence accepted: referenced-assembly macro/generator load failures (wrong type name, missing constructor, etc.) are now warn+fallback by default in real builds too (not just code-service), unless `localMetaprogramming: true` is set.
- **WIG projects** rely on loading the current assembly by design; keep them on the serial path and out of the parallel modes.
- **Mode B / combined double F# work**: when `fsc` compiles while `wsfsc` also runs its own FCS check, we pay two front-ends. Acceptable if wall-clock improves via parallelism (especially in the combined mode); measure. A later optimization could share a single check result.
- **Combined-mode races**: partial-DLL reads, job handoff across the start/join targets, and `fsc`-failure-after-WS-start are the main hazards; they are called out as explicit Phase 2 tasks.
- Parallel modes are **no-ops (with a warning) when `localMetaprogramming` is true**, since correctness then requires the emitted DLL.

## Resolved at planning review

- Setting names `parallelCompilation` (Mode A, wsconfig) and `WebSharperExternalFSharpCompiler` (Mode B, MSBuild) are accepted.
- **C# is out of scope** for parallel modes (always uses `csc`).
- **Phases are separated**: Phase 1 ships on its own (delivers the opt-in + the breaking change) ahead of #1547; Phase 2 follows.
- Mode A and Mode B are **combinable**, and the combined mode is a required, explicitly-planned case.
- **All new settings get an MSBuild property** (`WebSharperLocalMetaprogramming`, `WebSharperParallelCompilation`, plus the MSBuild-only `WebSharperExternalFSharpCompiler`) in addition to wsconfig/CLI, and the GHA workflow gains an `msbuildProperties` input so perf-testing can flip modes for a whole CI run without editing projects.
