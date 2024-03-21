import { readdirSync } from 'fs'
import { build } from 'esbuild'

const files = readdirSync('./wwwroot/Scripts/WebSharper/Web/');

files.forEach(file => {
  if (file.endsWith('.js')) {
    var options =
    {
      entryPoints: ['./wwwroot/Scripts/WebSharper/Web/' + file],
      bundle: true,
      minify: true,
      format: 'iife',
      outfile: 'wwwroot/Scripts/WebSharper/' + file,
      globalName: 'wsbundle'
    };

    build(options);
  }
});
