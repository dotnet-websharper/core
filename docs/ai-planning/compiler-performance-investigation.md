# Compiler performance investigation — where the next boost can come from

Date: 2026-07-10. Branch: `perf-experiments` (includes #1587 Phase 2: `parallelCompilation`,
`WebSharperExternalFSharpCompiler`, combined `--awaitdll` mode).

Related documents:

- [1587-local-metaprogramming-and-parallel-compilation.md](1587-local-metaprogramming-and-parallel-compilation.md) —
  the parallel modes that are already implemented (this doc measures them).
- `build-script-perftesting` repo — `00-overview-and-plan.md` (idea list), `01-benchmark-harness.md`
  (CI harness design, not yet running: `data/results.jsonl` is empty), `03-sidecar-metadata.md`
  (the sidecar design this doc sizes with real numbers), `05-multiproject-watch.md`.

Everything below is grounded in fresh measurements taken on this machine (Windows 11,
`dotnet build` of `tests/WebSharper.Collections.Tests` and its dependency chain, Release
compiler from `build/Release`, per-stage numbers via `-p:WebSharperTimingLog=<file>` / #1590).
Single machine, 1–3 samples per configuration — treat numbers as directional, and re-verify on
the perf-testing harness before locking in defaults.

## 1. Measured baseline: where the time actually goes

### Full chain rebuild (`--no-incremental`, serial mode, standalone wsfsc)

Chain: WebSharper.Core → Core.JavaScript → InterfaceGenerator → WebSharper.JavaScript (WIG) →
StdLib.Proxies → StdLib → Testing → Collections.Tests. Wall clock **2:35**.

Stage totals across all WS-compiled projects:

| Stage | Total | Share of WS-compiler time |
|---|---:|---|
| `F# compilation` (FCS `checker.Compile` = parse+check+IL emit) | 43.1 s | ~36 % |
| `Checking project` (FCS `ParseAndCheckProject` for the typed AST) | 42.8 s | ~36 % |
| `Parsing with FCS` (typed-AST materialization in ProjectReader) | 14.7 s | ~12 % |
| `WebSharper translation` | 4.2 s | 3.5 % |
| All JS/TS writing + packaging + closures | ~3.8 s | 3 % |
| `Writing resources into assembly` (Cecil rewrite of the dll) | 1.7 s | 1.4 % |
| WIG-specific (IExtension load, assembly def, write) | ~6.4 s | 5 % |
| `Serializing metadata` | 0.5 s | 0.4 % |

**~84 % of cold compile time is FCS front-end work, and we pay it twice.** In the default
in-process mode, `wsfsc` runs the full fsc pipeline (`checker.Compile`) *and* its own
`ParseAndCheckProject` (needed for the typed AST that WS translation reads), and FCS shares
nothing between the two code paths (confirmed empirically: each costs the same alone or
together). Everything WebSharper adds on top of F# itself — translation, packaging, writing,
metadata — is only ~10 % of a cold build.

Per-project detail (single project rebuild, deps warm, serial standalone; wsfsc-internal total
25.3 s, wall 30.6 s):

| Stage (Collections.Tests) | ms |
|---|---:|
| `F# compilation` | 11 304 |
| `Checking project` | 7 340 |
| `Parsing with FCS` | 2 819 |
| `WebSharper translation` | 1 340 |
| `Writing per-class code files` | 777 |
| `Writing resources into assembly` | 1 024 |
| everything else | ~750 |
| **wsfsc total** | **25 332** |

Note also the constant ~5 s gap between wsfsc-internal total and wall clock: MSBuild
evaluation + task overhead + process startup. On warm builds this becomes a *large relative*
cost (see §3).

### Mode comparison (same single-project rebuild, touch `Main.fs` between runs)

| Configuration | Wall | wsfsc total | vs. serial |
|---|---:|---:|---|
| Serial, standalone (cold process each time) | 30.6 s | 25.3 s | — |
| **Combined mode** (`ExternalFSharpCompiler` + `ParallelCompilation`) | **23.8 s** | 18.0 s | **−22 %** |
| Serial + `--test:GraphBasedChecking --test:ParallelOptimization --test:ParallelIlxGen` | 28.8 s | 23.5 s | −6 % |
| Serial + `DOTNET_gcServer=1` | 27.7 s | 22.3 s | **−12 %** |
| Serial via **booster**, warm (3rd run) | 11.8 s | 6.9 s | **−61 %** |
| Booster warm + **Mode A** (`parallelCompilation`) | **9.7 s** | — | **−68 %** |

### Warm booster residual (3rd run, serial)

| Stage | ms | Comment |
|---|---:|---|
| `F# compilation` (`checker.Compile`) | 3 852 | **non-incremental by design** — full fsc pipeline every time |
| `Parsing with FCS` | 1 459 | typed-AST materialization, re-done every time |
| `Checking project` | 226 | FCS incremental cache **works** (was 7 340 cold) |
| WS translation + writing + resources | ~1 300 | |
| **wsfsc total** | **6 860** | wall 11.8 s → ~5 s MSBuild/pipe/task overhead |

## 2. Finding: the booster was silently disabled on this machine

All initial "booster" runs produced identical numbers to standalone. Cause:
**`WebSharperBuildService=False` is set machine-wide** in the environment, which flips
`WsConfig.Standalone` to true ([CommandTools.fs:162](../../src/compiler/WebSharper.Compiler/CommandTools.fs));
wsfsc then never contacts the service, and nothing in the build output says so.

This matters beyond this machine: the booster is the single biggest measured lever for the
inner loop (30.6 s → 11.8 s warm) and it can be off with zero visibility. Cheap fixes:

- Log one line when standalone is chosen because of the env var (the code already computes the
  `reason` string in [Program.fs:67-75](../../src/compiler/WebSharper.FSharp/Program.fs) — it is
  only used in the standalone branch's *log file*, not surfaced to the build).
- Consider a `WebSharperTimingLog`-style record of which path (service/standalone) ran, so perf
  reports are attributable.

## 3. Ranked opportunities

Ordered by measured-payoff ÷ effort. Items 1–3 are quick wins; 4–6 are the strategic ones.

### 3.1 Server GC for wsfsc / wsfscservice — trivial, measured −12 %

The compiler hosts run Workstation GC today (only `GCLatencyMode.Batch` is set in both
`Program.fs` entry points; no `<ServerGarbageCollection>` in
[WebSharper.FSharp.fsproj](../../src/compiler/WebSharper.FSharp/WebSharper.FSharp.fsproj) or
[WebSharper.FSharp.Service.fsproj](../../src/compiler/WebSharper.FSharp.Service/WebSharper.FSharp.Service.fsproj)).
`fsc` and Roslyn both ship with Server GC for exactly this allocation profile.
`DOTNET_gcServer=1` alone: 25.3 s → 22.3 s wsfsc-internal on the test project.

Action: add `<ServerGarbageCollection>true</ServerGarbageCollection>` (and
`<ConcurrentGarbageCollection>false</ConcurrentGarbageCollection>`, matching fsc) to
wsfsc, wsfscservice, and wsc (`WebSharper.CSharp`). On .NET 9+/10 hosts, also evaluate DATAS
(`System.GC.DynamicAdaptationMode`) — it is the default with Server GC there and can trade
throughput for memory; for a long-lived booster that may be the right default, for one-shot
wsfsc probably not. Measure both.

### 3.2 Make the booster + Mode A the blessed inner-loop config — measured 3×

30.6 s (today's effective default here) → 9.7 s (booster warm + `parallelCompilation`).
Nothing new to build; this is packaging/defaults/visibility work:

- Fix the silent-disable problem (§2).
- Default `parallelCompilation` on when its preconditions hold (`useLocalReflection=false`,
  non-WIG, non-bundleonly) — it is a pure overlap with no output change; the code already
  warns-and-ignores when preconditions fail.
- Document the recommended dev-loop setup in one place (booster on, Mode A on, what to expect).

### 3.3 Booster concurrency — solution-scale builds serialize today

[WebSharper.FSharp.Service/Program.fs](../../src/compiler/WebSharper.FSharp.Service/Program.fs)
processes compilations through a single `MailboxProcessor` — deliberately serialized. With
`msbuild -m`, N projects that would compile in parallel as standalone processes instead queue
one-by-one in the service. For multi-project solutions the booster can therefore *lose* to
standalone on cold builds, while winning on single-project warm edits.

Action: allow K concurrent compilations in the service (bounded — FCS caches plus
`keepAssemblyContents=true` make memory the real constraint; start with K=2–4 or
`min(procCount/4, 4)`), keep the per-project FCS checker shared. The timing-log records may
need a request id for attribution (already noted in perf-testing docs). Measure a full-solution
cold build booster-on vs. booster-off before and after.

### 3.4 Sidecar metadata files — now with real sizing data

Design exists in `build-script-perftesting/03-sidecar-metadata.md` (write
`MyProject.dll.websharper` / `.websharperruntime` next to the dll instead of embedding).
Measured facts to inform its cost/benefit:

- **WS payload share of dll size**: StdLib **92 %** (1.93 MB file, of which ~296 KB meta +
  942 KB js/ts/map + 526 KB embedded sources), Collections.Tests 65 %, Testing 57 %,
  WIG-produced WebSharper.JavaScript 10 %. Sidecars would shrink shipped dlls by 2–10×
  for WS libraries.
- **Direct compile-time win is modest**: `Writing resources into assembly` (the Cecil
  read-modify-write of the whole dll) is 0.7–1.0 s per project; `Serializing metadata`
  0.1–0.25 s. On a warm 6.9 s build that ~1 s is ~15 %, on cold builds ~4 %.
- **The structural wins are the real prize**:
  - The output dll stops being rewritten after fsc → **`ProduceReferenceAssembly` can stay on**
    in Mode B/combined, so downstream projects skip rebuilds on implementation-only edits —
    on multi-project solutions this dwarfs any per-project saving. Today the targets force
    `ProduceReferenceAssembly=False`.
  - Reference metadata can be read without Cecil-loading every referenced dll
    ([Loader.fs](../../src/compiler/WebSharper.Compiler/api/Loader.fs) re-reads and re-parses
    every reference **on every compile**; its cache is per-compile). Today this is hidden
    behind the FCS check via `Task.Run`, but it stays on the critical path of warm builds and
    of the service's memory.
  - A versioned standalone metadata file is the precondition for cross-build metadata caching
    (perf-testing 03 doc, "follow-on").
- Sequencing note: land it behind `Embedded | Sidecar | Both` as designed; the reader-side
  fallback keeps old packages working. All `EMBEDDED_METADATA` readers are enumerated by grep
  (FrontEnd, service memCache, sitelets runtime `IO.LoadMetadata`, DllBrowser, unpack).

### 3.5 Attack the remaining warm-loop floor: non-incremental `checker.Compile` + TAST materialization

After the booster makes `ParseAndCheckProject` ~free (226 ms), the warm floor is
`checker.Compile` 3.9 s + `Parsing with FCS` 1.5 s. Both are FCS-side. Options, cheapest first:

1. **Overlap it** — Mode A already hides most of it (measured §1); default it on (3.2).
2. **FCS flags**: `--test:GraphBasedChecking --test:ParallelOptimization --test:ParallelIlxGen`
   gave −6 % here (small project; graph checking scales with file count — retest on a
   50+-file project). Zero risk to adopt via documented `OtherFlags` opt-in; they flow through
   `CompilerArgs` to both FCS calls already.
3. **FCS TransparentCompiler**: `FSharpChecker.Create(useTransparentCompiler = true)` is the
   new FCS caching engine built for repeated whole-project checks. The booster is its ideal
   consumer. Experiment: flip it in the service's single `FSharpChecker.Create` and compare
   warm `Checking project` / memory. (FCS 43.12.x ships it; still flagged experimental —
   keep it a service-side opt-in setting.)
4. **Upstream ask**: an FCS API to emit the assembly *from existing check results*
   (`FSharpCheckProjectResults` → IL), eliminating the second front-end entirely. Today's
   `checker.Compile` re-parses and re-checks from scratch. This is the only path to a true
   single-front-end wsfsc in in-process mode; combined mode approximates it by paying the
   second front-end in a parallel process instead.

### 3.6 Shave the constant ~5 s per-project wall overhead

Wall minus wsfsc-internal is ~5 s in every configuration (30.6/25.3, 23.8/18.0, 11.8/6.9).
Components worth measuring separately (MSBuild `-bl` + `/clp:PerformanceSummary`):

- MSBuild evaluation + Fsc arg computation (`SkipCompilerExecution=true` still runs the full
  `CoreCompile` machinery).
- Combined mode's **double self-invocation** for arg harvesting
  (`FindReferenceAssembliesForReferences` + `CoreCompile` re-entry in
  [WebSharper.FSharp.Internal.targets](../../msbuild/WebSharper.FSharp.Internal.targets)) —
  candidate: cache harvested args keyed on project-file + references timestamps, the way
  `dotnet-ws` already caches MSBuild params, or have the join target export them for the next
  build.
- wsfsc process start + JIT (standalone only; the booster already removes it — and
  `PublishReadyToRun` is already enabled for the published tools, which is why further R2R
  experiments showed no upside: startup JIT is not where the time is).
- Booster pipe round-trip and per-line log forwarding (each output line is a JSON message +
  `Async.RunSynchronously`; batching would reduce syscalls on chatty builds).

### 3.7 WS-side micro-optimizations (bounded, do after the above)

Together ~2–3 s cold / ~1 s warm per project; only worth it once FCS work is overlapped:

- `Writing per-class code files` (0.5–1.5 s): the per-file JS/TS render loop in
  [FrontEnd.fs CreateResources](../../src/compiler/WebSharper.Compiler/FrontEnd.fs) is
  embarrassingly parallel (independent files, independent `CodeWriter`s) — `Parallel.ForEach`
  with ordered collection.
- TypeScript output is rendered even when only `.js` is consumed — `WebSharperTypeScriptOutput`
  defaults to `True` in the *internal* targets; check what the shipped targets default to and
  make the test/dev loop skip TS unless asked.
- `Info.UnionWithoutDependencies` of all reference metadata is recomputed per compile (service
  caches per-assembly decode, not the merged union). Cache the merged union in the service
  keyed on the ordered set of reference paths+timestamps. Small today (hidden behind the check)
  but it lands on the critical path once checking is ~free (TransparentCompiler world).
- The `Loader` per-compile Cecil parsing of references — superseded by sidecar (3.4), otherwise
  make the service's loader cache persistent with file-change invalidation like `memCache`.

## 4. Ruled out / parked

- **ReadyToRun**: already enabled (`PublishReadyToRun=true` for win-x64/linux-x64 published
  tools). No measurable upside in prior experiments because (a) one-shot wsfsc time is
  dominated by FCS compute, not JIT warmup of WS code, and (b) the booster removes process
  startup entirely. Keep it on for the tools; stop re-testing it.
- **NativeAOT for wsfsc**: not feasible — FCS itself, plus reflection-loading of
  macros/generators/WIG/source-generators, are dynamic-load workloads.
- **Faster metadata serializer**: `Serializing metadata` is 0.1–0.5 s; the custom
  reflection-derived binary encoder ([Binary.fs](../../src/compiler/WebSharper.Core/Binary.fs))
  is not a bottleneck. Only revisit if sidecar-era caching makes decode hot (service already
  caches decodes).
- **WIG projects**: excluded from all parallel modes by design (they execute the built
  assembly). Their costs are elsewhere (`Loading IExtension implementation` 3.4 s is
  assembly-load + JIT of the definition; booster-style persistence would help but WIG is
  explicitly not permitted in the service today). Park unless WIG-heavy solutions complain.

## 5. Suggested measurement follow-ups

1. Stand up the perf-testing harness (its `data/results.jsonl` is still empty) so the numbers
   above get multi-sample, multi-OS confirmation — especially before flipping any default.
2. A/B a **full solution** build with booster on/off and `-m` to quantify the MailboxProcessor
   serialization loss (3.3).
3. Re-run the graph-based-checking flags on a large-file-count project (StdLib.Proxies check,
   30 s, is the in-repo worst case).
4. `-bl` trace of one warm booster build to split the ~5 s wall overhead (3.6) into MSBuild
   vs. task vs. pipe.

## Appendix: how these numbers were produced

```powershell
# per-stage timing (writes JSONL; stage names from LoggerBase.TimedStage)
dotnet build tests/WebSharper.Collections.Tests/WebSharper.Collections.Tests.fsproj `
  -c Release -p:WebSharperTimingLog=<file>.jsonl `
  [-p:WebSharperStandalone=true] `
  [-p:WebSharperExternalFSharpCompiler=false] [-p:WebSharperParallelCompilation=false] `
  [-p:OtherFlags="--test:GraphBasedChecking --test:ParallelOptimization --test:ParallelIlxGen"]
# force rebuild between runs: touch a source file (or --no-incremental for the whole chain)
# booster runs: $env:WebSharperBuildService='true'  (this machine has it =False machine-wide!)
# server GC run: $env:DOTNET_gcServer='1'
# dll payload split: enumerate GetManifestResourceNames/stream lengths on built dlls
```
