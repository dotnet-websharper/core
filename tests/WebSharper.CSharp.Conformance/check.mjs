// $begin{copyright}
//
// This file is part of WebSharper
//
// Copyright (c) 2008-2026 IntelliFactory
//
// Licensed under the Apache License, Version 2.0 (the "License"); you
// may not use this file except in compliance with the License.  You may
// obtain a copy of the License at
//
//     http://www.apache.org/licenses/LICENSE-2.0
//
// Unless required by applicable law or agreed to in writing, software
// distributed under the License is distributed on an "AS IS" BASIS,
// WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or
// implied.  See the License for the specific language governing
// permissions and limitations under the License.
//
// $end{copyright}

// C# conformance check: runs the cases on .NET (Net/) and as WebSharper JavaScript (Js/, under Node),
// compares the two, and checks the result against baseline.json.
//
//   node check.mjs            compare; fail on regressions and on cases missing from the baseline
//   node check.mjs --update   rewrite baseline.json from this run (commit it with the fix)
//
// Build Net/ and Js/ first (Release), see README.md.

import { execFileSync } from "node:child_process";
import { readFileSync, writeFileSync, existsSync } from "node:fs";
import { dirname, join } from "node:path";
import { fileURLToPath } from "node:url";

const here = dirname(fileURLToPath(import.meta.url));
const update = process.argv.includes("--update");

function parse(text) {
  const res = new Map();
  for (const line of text.split(/\r?\n/)) {
    const i = line.indexOf(" = ");
    if (i > 0) res.set(line.slice(0, i), line.slice(i + 3));
  }
  return res;
}

const netDll = join(here, "Net", "bin", "Release", "WebSharper.CSharp.Conformance.Net.dll");
if (!existsSync(netDll)) throw new Error(`${netDll} not found: build Net/ in Release first`);
const net = parse(execFileSync("dotnet", [netDll], { encoding: "utf8" }));

const { Run } = await import("./Js/build/Scripts/WebSharper/Conformance/Conformance.Runner.js");
const js = parse(Run().join("\n"));

// JS failures carry " :: <message>" for diagnosis; it is not part of the comparison
const results = {};
for (const [name, expected] of net) {
  const actual = js.has(name) ? js.get(name) : "<missing>";
  const status =
    actual.split(" :: ")[0] === expected ? "pass" :
    actual.startsWith("WS-COMPILE-ERROR") ? "unsupported" : "mismatch";
  results[name] = status === "pass" ? { status } : { status, net: expected, js: actual };
}

const count = s => Object.values(results).filter(r => r.status === s).length;
console.log(`${net.size} cases: ${count("pass")} pass, ${count("unsupported")} unsupported (fenced), ${count("mismatch")} mismatch`);

// The TypeScript declarations of the cases: dts-check.ts must type-check with `strict`, without skipLibCheck
let dtsErrors = [];
{
  let ts = null;
  try { ts = (await import("typescript")).default; } catch { }
  if (!ts) {
    console.log("TypeScript declarations: not checked (run `npm install` in the repository root)");
  } else {
    const file = join(here, "dts-check.ts");
    const program = ts.createProgram([file], {
      strict: true, noEmit: true, skipLibCheck: false,
      module: ts.ModuleKind.NodeNext, moduleResolution: ts.ModuleResolutionKind.NodeNext, target: ts.ScriptTarget.ES2022,
    });
    dtsErrors = ts.getPreEmitDiagnostics(program).map(d => {
      const where = d.file ? `${d.file.fileName.replace(here + "/", "")}(${d.file.getLineAndCharacterOfPosition(d.start).line + 1})` : "";
      return `${where} TS${d.code}: ${ts.flattenDiagnosticMessageText(d.messageText, " ")}`;
    });
    console.log(`TypeScript declarations: ${dtsErrors.length === 0 ? "ok" : dtsErrors.length + " error(s)"}`);
  }
}

const baselinePath = join(here, "baseline.json");
if (update) {
  writeFileSync(baselinePath, JSON.stringify(results, null, 2) + "\n");
  console.log(`Wrote ${baselinePath}`);
  process.exit(0);
}

const baseline = existsSync(baselinePath) ? JSON.parse(readFileSync(baselinePath, "utf8")) : {};
const rank = { pass: 2, mismatch: 1, unsupported: 0 };
const regressions = [], unknown = [], improved = [], changed = [];
for (const [name, r] of Object.entries(results)) {
  const b = baseline[name];
  if (!b) unknown.push(name);
  else if (rank[r.status] < rank[b.status]) regressions.push(`${name}: ${b.status} -> ${r.status}\n    .NET: ${r.net}\n    JS:   ${r.js}`);
  else if (rank[r.status] > rank[b.status]) improved.push(`${name}: ${b.status} -> ${r.status}`);
  else if (r.status === "mismatch" && r.js.split(" :: ")[0] !== b.js.split(" :: ")[0]) changed.push(`${name}: JS was ${b.js}, now ${r.js}`);
}
const removed = Object.keys(baseline).filter(n => !(n in results));

const report = (title, xs) => { if (xs.length) console.log(`\n${title} (${xs.length}):\n  ` + xs.join("\n  ")); };
report("Improved — run `node check.mjs --update` and commit baseline.json", improved);
report("Mismatch output changed", changed);
report("Removed from the cases", removed);
report("Not in baseline.json — run `node check.mjs --update`", unknown);
report("REGRESSIONS", regressions);
report("TypeScript declaration errors", dtsErrors);

process.exit(regressions.length || unknown.length || dtsErrors.length ? 1 : 0);
