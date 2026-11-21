// Measures the JavaScript output of the test web project, to compare
// compiler changes that affect output size or shape.
//
// Usage (from the repo root, after building tests/Web):
//   node tests/Scripts/OutputSize.mjs [--save out.json] [--compare baseline.json] [--dir <folder>]
//
// Default folder: tests/Web/wwwroot/Scripts/WebSharper (the esbuild bundles).
// Uses esbuild from tests/Web/node_modules for minification.

import { readFileSync, readdirSync, writeFileSync, statSync } from 'fs'
import { join, resolve } from 'path'
import { gzipSync, brotliCompressSync } from 'zlib'
import { createRequire } from 'module'

const args = process.argv.slice(2)
const opt = (name) => {
  const i = args.indexOf(name)
  return i >= 0 ? args[i + 1] : undefined
}

const repoRoot = resolve(new URL('../..', import.meta.url).pathname.replace(/^\/([A-Za-z]:)/, '$1'))
const dir = resolve(opt('--dir') ?? join(repoRoot, 'tests/Web/wwwroot/Scripts/WebSharper'))
const require = createRequire(join(repoRoot, 'tests/Web/package.json'))
const esbuild = require('esbuild')

// Patterns that indicate output shapes the modernization work wants to reduce.
const patterns = {
  errorPlaceholder: /\$\$ERROR\$\$/g,
  compileErrorThrow: /WebSharper compile error/g,
  iife: /\)\s*\(\s*\)|\}\)\(\w+(?:_\d+)?\)/g,
  pureAnnotation: /\/\* @__PURE__ \*\//g,
  lazyClass: /Lazy\(\(_i\)/g,
  letNoInit: /^\s*let [\w$]+(?:, [\w$]+)*;$/gm,
  enumeratorDispose: /\.Dispose\(\)/g,
}

function measure(file) {
  const src = readFileSync(file, 'utf8')
  const min = esbuild.transformSync(src, { minify: true, loader: 'js', logLevel: 'silent' }).code
  const counts = {}
  for (const [k, re] of Object.entries(patterns)) counts[k] = (src.match(re) || []).length
  return {
    raw: Buffer.byteLength(src),
    lines: src.split('\n').length,
    min: Buffer.byteLength(min),
    minGzip: gzipSync(min, { level: 9 }).length,
    minBrotli: brotliCompressSync(Buffer.from(min)).length,
    counts,
  }
}

const results = {}
for (const f of readdirSync(dir).sort()) {
  const p = join(dir, f)
  if (f.endsWith('.js') && statSync(p).isFile()) results[f] = measure(p)
}

const save = opt('--save')
if (save) writeFileSync(save, JSON.stringify(results, null, 2))

const cmpFile = opt('--compare')
const base = cmpFile ? JSON.parse(readFileSync(cmpFile, 'utf8')) : null

const fmtDelta = (now, was) => {
  if (was === undefined) return String(now)
  const d = now - was
  if (d === 0) return String(now)
  const pct = was ? ` ${(100 * d / was).toFixed(2)}%` : ''
  return `${now} (${d > 0 ? '+' : ''}${d}${pct})`
}

const rows = []
for (const [f, r] of Object.entries(results)) {
  const b = base?.[f]
  rows.push({
    file: f,
    raw: fmtDelta(r.raw, b?.raw),
    min: fmtDelta(r.min, b?.min),
    minGzip: fmtDelta(r.minGzip, b?.minGzip),
    minBrotli: fmtDelta(r.minBrotli, b?.minBrotli),
    ...Object.fromEntries(Object.entries(r.counts).map(([k, v]) => [k, fmtDelta(v, b?.counts?.[k])])),
  })
}
console.table(rows)
