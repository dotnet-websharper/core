import { cpSync, readdirSync } from 'fs'
import { build } from 'esbuild'

cpSync('./build/', './wwwroot/', { recursive: true });

const root = './build/Scripts/WebSharper/'
const currentAsm = 'Web'

const files = readdirSync(root + currentAsm + '/');
const workers = readdirSync(root + 'workers/');

files.forEach(file => {
  if (file.endsWith('.js')) {
    var options =
    {
      entryPoints: [root + currentAsm + '/' + file],
      bundle: true,
      minify: false,
      format: 'iife',
      outfile: 'wwwroot/Scripts/WebSharper/' + file,
      globalName: 'wsbundle'
    };

    build(options);
  }
});

workers.forEach(file => {
  if (file.endsWith('.js')) {
    var options =
    {
      entryPoints: [root + 'workers/' + file],
      bundle: true,
      minify: false,
      format: 'iife',
      outfile: 'wwwroot/Scripts/WebSharper/workers/' + file,
      globalName: 'wsbundle'
    };

    build(options);
  }
});
