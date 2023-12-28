import { build } from 'esbuild'

var options =
{
  entryPoints: ['./wwwroot/Scripts/WebSharper/Web/root.js'],
  bundle: true,
  minify: true,
  format: 'iife',
  outfile: 'wwwroot/Scripts/WebSharper/bundle.js',
  globalName: 'wsbundle'
};

build(options);