# Compiler settings file

WebSharper's compiler can be customized by a number of settings. These are read from a `wsconfig.json` located next to the project file.
A `WebSharperConfigFile` property in the `.csproj` / `.fsproj` file can override this default name or location where WebSharper looks for configs.

<a name="jsonConfiguration"></a>
## Json configuration

The `wsconfig.json` file must consist of a single JSON object. Keys and values are all case-insensitive, but there is a recommended provided by the schema. Boolean values can be `true` of `false` literals or strings that can be parsed to a `bool`. Here is an example `wsconfig.json`:

```json
{
  "$schema": "https://websharper.com/wsconfig.schema.json",
  "project": "web",
  "outputDir": "wwwroot",
  "release": {
    "outputDir": "build",
    "preBundle": true
  }
}
```

# Available settings

Below is a list of all settings ordered alphabetically.

<a name="analyzeClosures"></a>
## "analyzeClosures"

**Type**: bool or `"moveToTop"` (default is `false`)

There is an inconvenient source of memory leaks in most JavaScript engines which is
[described here](http://point.davidglasser.net/2013/06/27/surprising-javascript-memory-leak.html).

This setting can enable warnings on these kinds of captures, helping to eliminate memory leaks.

**Possible values:**
* `true` - Turns warnings on.
* `"moveToTop"` - Moves all non-capturing lambdas to top level automatically (experimental).
* `false` - Default setting, no JS closure analysis.

<a name="configurationName"></a>
## Configuration name (usually `"Debug"` or `"Release"`)

**Type**: JSON object

Allows overriding configuration values based on project configuration. For example, a "WebSharper 8 Client-Server Application" template project uses this to set [prebundle](#prebundle) for Release mode and use `esbuild` on the output, while Debug mode uses `vite`.

<a name="dce"></a>
## "dce"

**Type**: bool (default is `true` for Bundle/BundleOnly projects, `false` for library projects)

An `spa` or `bundleOnly` project uses dead code elimination to have a minimal size `.js` output. If you run into any errors with missing code, please [report as a bug](https://github.com/dotnet-websharper/core/issues). As a quick workaround you can set `"dce": false` to see if that resolves your problem.

The other use case for dead code elimination is producing npm-facing library code. For this, set `"dce": true` on a libray project and specify an [outputDir](#outputDir). You might also want to set `"javascriptExport": true` to make the whole current project exported into the final output, otherwise only classes and methods marked with the `JavascriptExport` attribute will be available.

To package the output for npm set `"outputDir": "build"` and then you can add a section to your project file like this:

```xml
  <Target Name="CleanBuildDir" BeforeTargets="CoreCompile">
    <RemoveDir Directories="build" />
  </Target>
  
  <Target Name="CopyPackageJsonAndPack" AfterTargets="WebSharperCompile">
    <Copy SourceFiles="assets/package.json" DestinationFolder="build" />
    <Exec Command="npm pack" WorkingDirectory="build" />
  </Target>
```

This cleans the build folder before a new build. After a successful WebSharper build, it copies over a `package.json` file to serve as your package declaration to your WebSharper project's output folder.

WebSharper will create an `index.js` to serve as the root of the npm package, so in your `assets/package.json` file, set `"main": "index.js"`. Also take note that all static methods on static classes will be exported as top level functions, make sure to give expressive names for your functions for npm library use that does not depend on F# module name for example to disambiguate them.

<a name="downloadResources"></a>
## "downloadResources"

**Type**: bool (default is `false`)

Set to `true` to have WebSharper download all 
remote `js`/`css` resources defined in the current project and all references. This is possible only for direct script resources, all npm imports you have to manage with running an `npm install` command.

When using this setting, you also add this to your `appsettings.json` so that WebSharper inserts a link to that downloaded file in your pages instead of a link to the online resource:

```json
  "websharper": {
    "UseDownloadedResources": true
  }
```

<a name="dts"></a>
## "dts"

**Type**: bool (default is `true`)

Turns on the generation or unpacking of TypeScript declaration files. This is available for all project types except `html`, and `web` with `prebundle` set to true.

<a name="javascript"></a>
## "javascript"

**Type**: bool or array of strings (default is `false`)

Setting this to `true` is equivalent to having a `JavaScript` attribute on the assembly level: it marks the entire assembly for JavaScript compilation.
You can still exclude types by using the `JavaScript(false)` attribute in your code.

Alternatively, you can pass an array of strings, containing file or type names. This is marking the given files or types for JavaScript compilation.

<a name="javascriptExport"></a>
## "javascriptExport"

**Type**: bool or array of strings (default is `false`)

Setting this to `true` is equivalent to having a `JavaScriptExport` attribute on the assembly level: it marks the entire assembly for JavaScript compilation and makes sure all the code in current assembly are exported as entry points.

Alternatively, you can pass an array of strings, containing file or type names. This is marking the given files or types for JavaScript compilation and export.

<a name="jsOutput"></a>
## "jsOutput"

**Type**: string (relative or absolute folder path)

Writes the generated `.js` code only, (usually one class per file) compiled output for current assembly to given location.

<a name="outputDir"></a>
## "outputDir"

Obligatory if [project](#project) is `web`; optional otherwise.

**Type**: string (relative or absolute folder path)

Specifies the path of the compilation output directory relative to the project file. Default folder is `./Content` for SPAs and `./bin/html` for HTML apps.

<a name="prebundle"></a>
## "prebundle"

**Type**: bool (default `false`)

Only for `web` projects, turns on production-ready mode: for all pages of a multi-page application a JavaScript file is created. This is readable format code, possibly importing npm packages, so it needs a proper JavaScript bundler before serving. It is recommended that this output goes to a `build` folder, and then bundling can be set up in project file like this: 

```xml
  <Target Name="ESBuildBundle" AfterTargets="WebSharperCompile">
    <Exec Command="npm install" />
    <Exec Command="node ./esbuild.config.mjs" />
  </Target>
```

where `esbuild.config.mjs` contains:

```javascript
import { existsSync, cpSync, readdirSync } from 'fs'
import { build } from 'esbuild'

if (existsSync('./build/Content/WebSharper/')) {
  cpSync('./build/Content/WebSharper/', './wwwroot/Content/WebSharper/', { recursive: true });
}

const files = readdirSync('./build/Scripts/WebSharper/$YOURPROJECTNAME$/');

files.forEach(file => {
  if (file.endsWith('.js')) {
    var options =
    {
      entryPoints: ['./build/Scripts/WebSharper/$YOURPROJECTNAME$/' + file],
      bundle: true,
      minify: true,
      format: 'iife',
      outfile: 'wwwroot/Scripts/WebSharper/' + file,
      globalName: 'wsbundle'
    };

    console.log("Bundling:", file);
    build(options);
  }
});
```

<a name="project"></a>
## "project"

**Type**: string (see below)

Specifies the WebSharper project type. The valid values and their corresponding project types
are listed below.

|Project type|"project" setting value|
|-|-|
|Library|`library`|
|Client-Server Application|`web`|
|Single Page Application|`spa`|
|Web Service|`microservice`|
|Single Page Application without .NET compilation|`bundleOnly`|
|HTML Application|`html`|
|JavaScript Binding|`binding`|
|Proxy Project|`proxy`|

The `library` project type is the default, it can be omitted. In this case the WebSharper compiler translates the found JavaScript scope, preparing the project to be used as a reference of other WebSharper projects.

The `web`, `spa`, and `microservice` project types work as web projects. The WebSharper compiler creates pre-optimized runtime metadata for them for fast startup of WebSharper Sitelets and Remoting services. A `web` project supports full Sitelet functionality. An `spa` outputs a single `.js` file to be linked from a static `html` file. A `microservice` is geared towards using server-side functionality only.

A `bundleOnly` project mimics `spa` for the `.js` output, but skips .NET compilation for F# and embedding resources for C# for faster turnaround of client-only use cases.

A `html` project outputs a multi-page website as static html with the necessary `.js` files linked also statically.

A `binding` project uses WebSharper Interface Generator, an concise F# DSL for defining the shapes of .NET types that would map to existing JavaScript code.

A `proxy` project takes code that is already translated as a .NET library without WebSharper and creates a library that won't contain the .NET types again but only the WebSharper translation information.

<a name="proxyTargetName"></a>
## "proxyTargetName"

**Type**: string

Only required for `proxy` projects. Specifies the assembly name of the original library current project will act as a proxy against. The usual use case is that you have a library that has no WebSharper references, and its proxy will be another project file that links in the very same source files, use `"project": "proxy"` and `"proxyTargetName": "originalAssemblyName"`. Then from a WebSharper project, you must reference the original library for the .NET types and the WebSharper proxy library for the JavaScript translation.

<a name="runtimeMetadata"></a>
## "runtimeMetadata"

**Type**: string (default is `"noexpressions"`)

This is a rarely needed setting, only relevant if the site itself want to host the WebSharper compiler at runtime, for example generating JavaScript snippets on the fly. Then a larger set of information of WebSharper metadata is necessary for the runtime than usual.

**Possible values:**
* `"inlines"` - keeps expressions for inlined values, needed for translating expressions that would use inlines.
* `"notinlines"` - keeps expressions for already translated functions.
* `"full"` - full WebSharper metadata.
* `"noexpressions"` - default setting, when no on-the-fly compilation is needed.

<a name="scriptBaseUrl"></a>
## "scriptBaseUrl"

**Type**: string (default `"Content"`)

Only needed if an `spa` or `bundleonly` project uses direct script references to non-module-based JavaScript. WebSharper will load these scripts via a `LoadScript` helper, which needs the root URL to where these extra scripts are located.

Current recommended approach is to use npm packages only for referencing external code and then this setting is unnecessary.

<a name="singleNoJSErrors"></a>
## "singleNoJSErrors"

**Type**: bool (default is `false`)

If the value is `true`, WebSharper errors for not finding a type or method in JavaScript scope will show up only once per type/method, where it's first encountered.

<a name="standalone"></a>
## "standalone"

**Type**: bool (default is `false` except if `WebSharperBuildService` environment variable is set to `false`)

If the value is `false`, the WebSharper F# compiler will not use a backend process for faster compilation speeds. If set to `true`, it will use backend process even if `WebSharperBuildService` environment variable is `false`.

<a name="stubInterfaces"></a>
## "stubInterfaces"

**Type**: bool (default is `false`)

If the value is `true`, WebSharper treats all interfaces in current project as if marked by the `Stub` attribute. This has the effect that interfaces act as easy interop tools with JavaScript, all method names are kept as is. By default, WebSharper creates longer unique names, so that .NET semantics can be used for interfaces, where a class can implement methods of the same name and signature from multiple interfaces.

<a name="useJavaScriptSymbol"></a>
## "useJavaScriptSymbol"

**Type**: bool (default is `true` for `proxy` projects, `false` otherwise)

If the value is `true`, for the JavaScript compilation a `JAVASCRIPT` conditional compilation symbol is added.

<a name="warnOnly"></a>
## "warnOnly"

**Type**: bool (default is `false`)

If the value is `true`, all WebSharper compiler errors will be treated only as warnings. This can help finding and debugging non-translateable code, those expressions will show up as `$$ERROR$$` in output code.




