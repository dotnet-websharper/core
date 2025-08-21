# WebSharper 8 upgrade guide

The biggest change coming with WebSharper 8 is that it outputs module-based JavaScript code. This output needs additional processing for both production and debugging purposes, this document covers how to:

* configure `esbuild` to bundle the WebSharper 8 output,
* update HTML files for SPA projects,
* make the code bundles smaller by separating them by page,
* optionally add a debug mode using `vite`,
* adapt to changes of C# templating.

First, update all your WebSharper NuGet packages to 8.0 versions.

## Production bundling

This is applicable for Web, SPA, and Html (offline) projects.

1. Redirect your WebSharper output to a temporary directory by changing the `"outputDir"` setting in `wsconfig.json` to for example `"build"`. For older projects, you might have your output folder configured in project settings as `WebProjectOutputDir`, `WebSharperBundleOutputDir`, or `WebSharperHtmlDirectory`. For client-server projects, also add `"preBundle": true` to `wsconfig.json`.

2. Add a `package.json` file with contents and replace "YourProjectName":
  ```json
  {
    "name": "YourProjectName",
    "version": "1.0.0",
    "devDependencies": {
      "esbuild": "^0.25.1"
    }
  }
  ```

If you use any WebSharper bindings, Femto is an automated tool to install their npm dependencies. To run it, execute:
  ```
  dotnet tool install femto --global
  femto --resolve
  ```

3. Add an `esbuild.config.mjs` with contents and replace "YourProjectName":
  ```javascript
  import { existsSync, cpSync, readdirSync } from 'fs'
  import { build } from 'esbuild'
  
  if (existsSync('./build/Content/WebSharper/')) {
    cpSync('./build/Content/WebSharper/', './wwwroot/Content/WebSharper/', { recursive: true });
  }
  
  const files = readdirSync('./build/Scripts/WebSharper/YourProjectName/');
  
  files.forEach(file => {
    if (file.endsWith('.js')) {
      var options =
      {
        entryPoints: ['./build/Scripts/WebSharper/YourProjectName/' + file],
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

This copies over the WebSharper-handled Content files if any, and uses esbuild to bundle up the JavaScript output.

4. Add this to your project to execute the mjs script on build:
```xml
  <Target Name="ESBuildBundle" AfterTargets="WebSharperCompile">
    <Exec Command="npm install" />
    <Exec Command="node ./esbuild.config.mjs" />
  </Target>
```

At this point, when running your website or generating html, you should see an `all.js` file referenced that contains all the code in bundled and minified form. This is not yet ideal, on a multi-page site, it's best to minimize the required code per page. A new functionality allows for this.

## SPA update

Single-page application projects use a static HTML file, in which the WebSharper generated JavaScript code is linked. Do the following updates to your HTML file:
* change `<link rel="stylesheet" type="text/css" href="Content/ProjectName.css" />` to `<link rel="stylesheet" type="text/css" href="Scripts/ProjectName.css" />`
* change `<script type="text/javascript" src="Content/ProjectName.head.js"></script>` to `<script type="text/javascript" src="Scripts/ProjectName.head.js"></script>`
* change `<script type="text/javascript" src="Content/ProjectName.min.js"></script>` to <script type="module" src="Scripts/ProjectName.min.js"></script>

To summarize: the generated files are now moved to the `Scripts` folder instead of `Content` for better uniformity, and the main JavaScript file must be loaded as a module.

## Optimize per-page bundles

In your Sitelet definition, where you return `Content.Page` responses, you can add a new `Bundle` argument that will set the name of the bundle that is created for that page. The WebSharper compiler looks inside the `Body` argument as provided in the source code for any `Web.Control` initializations (including the `client`) helper and will include all necessary client-side code in the bundle. The `Bundle` argument must be a file name without an extension that WebSharper will create for your project.

At runtime, the necessary imports are checked against the known bundle name, and if the pre-compiled bundle for the page is not sufficient (for example your code includes some client-side content from a non-annotated helper function), `all.js` will be linked instead as a fallback. So to optimize your website, it's best to avoid this.

These are some considerations:
* If multiple pages use exactly or even just roughly the client-side code, using the same bundle name will cover everything needed for all, optimizing total code sizes.
* If you use server-side helper functions that create client-side content, and you want to include it in a bundle, use the `Content.BundleScope` helper. Or if it is used in multiple bundles, use `Content.BundleScopes`.

Example:

```fsharp
// This helper uses a client-side function to construct some DOM.
// As it is not within the Content.Page initialization,
// we are using Content.BundleScopes to add them to the code bundles.
let Shared() =
    Content.BundleScopes [| "home"; "about" |] (
        div [] [ client (Client.Shared()) ]
    )

// The pages are marked separately with bundle names,
// Content.Page registers the client-side contents to go to that bundle.
let HomePage ctx =
    Content.Page(
        Templating.Main ctx EndPoint.Home "Home" [
            h1 [] [text "Say Hi to the server!"]
            div [] [client (Client.Main())]
            Shared()
        ], 
        Bundle = "home"
    )

let AboutPage ctx =
    Content.Page(
        Templating.Main ctx EndPoint.About "About" [
            h1 [] [text "About"]
            div [] [client (Client.About())]
            Shared()
        ], 
        Bundle = "about"
    )
```

## Debug mode

For debugging one class per file readable code, you can make `esbuild` to run in Release Mode only.

1. Change `wsconfig.json`:
  ```json
    "outputDir": "wwwroot",
    "release": {
      "outputDir": "build",
      "preBundle": true
    }
  ```
and add a conditional to the project target:
```xml
  <Target Name="ESBuildBundle" AfterTargets="WebSharperCompile" Condition=" '$(Configuration)' == 'Release' ">
```

2. With above changes in Debug configuration, WebSharper will create one class per file readable `.js` files as output and there is no prebundling. 
If you are not using any npm packages, your site could be fully functional with your browser interpreting module-based JavaScript.
However, some tool like `vite` is needed to handle any npm packages if present.
There is a built-in helper, add
```fsharp
#if DEBUG        
        .UseWebSharperScriptRedirect(startVite = true)
#endif
```
in your ASP.Net Core startup, before `.UseStaticFiles()`. 
This will start `vite` in a separate process if not running yet when your website starts.

3. Set a `"DebugScriptRedirectUrl": "http://localhost:1234"` within the `"websharper"` section of your `appsettings.json`.
Change the 1234 port to something not colliding with other local ports generated for your solution to avoid conflicts.

## C# templating

The WebSharper.UI C# templating now uses a source code generator. Some project file changes are required to make it work.
1. Add these to a `PropertyGroup` section of your project file:
```xml
<EmitCompilerGeneratedFiles>true</EmitCompilerGeneratedFiles>
<CompilerGeneratedFilesOutputPath>Generated</CompilerGeneratedFilesOutputPath>
```
This will make the compiler write out generated code to the disk. You may want to also add the `**/Generated/` folders or the `.g.cs` pattern to your `.gitignore` file.
2. Change your template html files to have `AdditionalFiles` item type instead of `Content` or `None`.
3. Now, during a compilation the generated files are appearing twice, we must exclude the files from the C# compilation.
You can do this by adding this to your project file:
```xml
  <ItemGroup>
    <!-- Exclude the earlier output of source generators from the C# compilation -->
    <Compile Remove="$(CompilerGeneratedFilesOutputPath)/**/*.cs" />
  </ItemGroup>
```