# TypeScript definition output

WebSharper includes an experimental feature of producing
[TypeScript](http://www.typescriptlang.org/) definition files (version 1.4)
corresponding to the JavaScript files produced by WebSharper.

## Usage

Set thise properties in your project file to unpack:

    <WebSharperProject>Export</WebSharperProject>
    <WebSharperTypeScriptDeclaration>True</WebSharperTypeScriptDeclaration>

For `WebSharperProject`, values `Web`, `Site` or `Website` are also enabling unpacking. 
Also, you can set the `<WebProjectOutputDir>` property to a directory if you
want to unpack to some directory other than the project folder.

## Limitations

* WIG projects are currently producing only empty interfaces.
We are planning on adding custom .d.ts and mapping its declarations to
the types in the .NET assembly produced by WIG
* Dependencies between WebSharper output `.js` files are not exposed
for outside use.
We are planning switching to exporting
[AMD](https://github.com/amdjs/amdjs-api/blob/master/AMD.md) modules.