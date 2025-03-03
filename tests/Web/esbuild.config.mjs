import { cpSync, readdirSync } from 'fs'
import { build } from 'esbuild'

//cpSync('./build/Content/WebSharper/', './wwwroot/Content/WebSharper/', { recursive: true });
//cpSync('./build/Scripts/WebSharper/WebSharper.Html5.Tests/', './wwwroot/Scripts/WebSharper/WebSharper.Html5.Tests/', { recursive: true, filter: source => source.endsWith('WebSharper.Html5.Tests') || source.includes('worker') });
//cpSync('./build/Scripts/WebSharper/WebSharper.InterfaceGenerator.Tests/', './wwwroot/Scripts/WebSharper/WebSharper.InterfaceGenerator.Tests/', { recursive: true });
//cpSync('./build/Scripts/WebSharper/WebSharper.Testing.Resources.QUnit/', './wwwroot/Scripts/WebSharper/WebSharper.Testing.Resources.QUnit/', { recursive: true });
//cpSync('./build/Scripts/WebSharper/WebSharper.Tests.JQueryResource/', './wwwroot/Scripts/WebSharper/WebSharper.Tests.JQueryResource/', { recursive: true });
//cpSync('./build/Scripts/WebSharper/WebSharper.Tests.TwitterBootstrap/', './wwwroot/Scripts/WebSharper/WebSharper.Tests.TwitterBootstrap/', { recursive: true });

cpSync('./build/', './wwwroot/', { recursive: true });

const files = readdirSync('./build/Scripts/WebSharper/Web/');

files.forEach(file => {
  if (file.endsWith('.js')) {
    var options =
    {
      entryPoints: ['./build/Scripts/WebSharper/Web/' + file],
      bundle: true,
      minify: false,
      format: 'iife',
      outfile: 'wwwroot/Scripts/WebSharper/' + file,
      globalName: 'wsbundle'
    };

    build(options);
  }
});
