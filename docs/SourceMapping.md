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

## Outputting source maps for WebSharper web projects

To unpack the source maps in your web project, add the same `WebSharperSourceMap`
property to the project file, and

    <staticContent>
      <mimeMap fileExtension=".fs" mimeType="text/plain" />
    </staticContent>

inside the web.config file's `<system.webServer>` element to allow serving
 `.fs` files.

## Usage

* Single-Page Application projects are currently not supported.

* In Google Chrome, you need to check the "Enable JavaScript source maps" 
setting in Developer Tools Settings.

* For Internet Explorer, you need to have Windows 8.1 Update 1.
