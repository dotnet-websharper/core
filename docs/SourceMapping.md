# Source maps

## Embedding source maps in WebSharper libraries

You can enable including source maps and the required source files
in a WebSharper assembly, by adding the

    <WebSharperSourceMap>True</WebSharperSourceMap>

property to your project file.

It is also recommended to set

    <OtherFlags>--quotations-debug</OtherFlags>

to have source position information inside reflected definitions available 
for WebSharper. Otherwise only the starting lines of functions can be mapped.

If you build with `WebSharper.exe` directly, add `-sm` to the command line.

WebSharper itself is built with source map embedding, so you can debug into
WebSharper library code.

## Outputting source maps for WebSharper web projects

If you have a separate web project hosting your WebSharper application,
add the same `WebSharperSourceMap` property to the project file to unpack
the source map files.

## Usage

### Internet Explorer

You need to have Internet Explorer 11 on Windows 8.1 Update 1 or newer
for source map support.

In the Debugger tab of F12 tools, the last icon in the toolbar says
"Load the sources mapped to this generated file".
Use this to jump between generated and original source code.
You can set breakpoints in the original code.

![IE source mapping](images/ExplorerSourceMap.PNG)

### Google Chrome

To enable source maps, check the "Enable JavaScript source maps" 
setting in Developer Tools Settings (cog icon in F12 panel).

In the Sources tab of Developer Tools, open a generated `.js` file in 
the `Scripts/WebSharper` folder to make Chrome load its source mappings.
A folder named `FSharpSources` will appear with the original `.fs` files used
for WebSharper translation.
You can use this to set breakpoints in original code.

![Chrome source mapping](images/ChromeSourceMap.PNG)

### Mozilla Firefox

Check the "Show Original Sources" item in the "Debugger Options"
menu (cog icon in F12 panel).

You can access the list of original sources, however breakpoints
don't work correctly as of version 37.0.1.

### Limitations

* Single-Page Application projects are currently not supported.

* Can not have multiple source files with the same file name in a single project.

* Inlines are not mapped.
