## Main Application Templates

The three main WebSharper application types are:

 1. **Client-Server Application** - a full client-server application implemented as a WebSharper [sitelet](Sitelets.md).

    The sitelet uses a dynamic HTML template and placeholders that are instantiated by the sitelet. See the [HTML templates](Templates.md) page for more information on dynamic templating.

    These applications exist in two flavors: a **Client-Server Web Application** runs as an ASP.NET module, while a **Self-Hosted Client-Server Web Application** runs as a self-contained executable using an OWIN self-host container.

 2. **HTML Application** - a multi-page HTML/JavaScript application.

    Like the Client-Server Application, this application also uses [sitelets](Sitelets.md) and dynamic HTML [templates](Templates.md) to define web pages. However it creates a set of static HTML and JavaScript files that can be deployed in any HTML container.

    WebSharper mobile web applications can also be created from this template.

 3. **Single-Page Application** - a single-page HTML/JavaScript application with an HTML page and an F# source file that plugs content into it.

    The easiest way to get started with WebSharper: write a few lines of F#, add a placeholder for it in the HTML, and you are ready to go.

    See the "Your first app in 2 minutes" tab for an example.

The following helper projects are also available:

 1. **Library** - a simple WebSharper library. Annotated code is compiled to JavaScript, and can then be used from any application project.
 
 2. **Extension** - defines the interface to an existing JavaScript library using [a convenient declarative F# syntax](WIG.md).

## Capabilities

This table summarizes the capabilities of the available application/helper project templates:

<table class="price-table">
    <tbody>
        <tr class="header">
            <td style="border:none;">Template</td>
            <th class="first">All-F#</th>
            <th>Is Sitelet?</th>
            <th>Client</th>
            <th>Server</th>
            <th class="last">Remote</th>
        </tr>
        <tr class="header">
            <th colspan="6">Applications</th>
        </tr>
        <tr>
            <td>Client-Server App<br/><span style="color:#888;font-size:smaller">ASP.NET-based or self-hosted via OWIN</span></td>
            <td><img src="https://raw.githubusercontent.com/intellifactory/websharper/master/docs/images/ok.png" alt="X"/></td>
            <td><img src="https://raw.githubusercontent.com/intellifactory/websharper/master/docs/images/ok.png" alt="X"/></td>
            <td><img src="https://raw.githubusercontent.com/intellifactory/websharper/master/docs/images/ok.png" alt="X"/></td>
            <td><img src="https://raw.githubusercontent.com/intellifactory/websharper/master/docs/images/ok.png" alt="X"/></td>
            <td><img src="https://raw.githubusercontent.com/intellifactory/websharper/master/docs/images/ok.png" alt="X"/></td>
        </tr>
        <tr>
            <td>HTML App</td>
            <td><img src="https://raw.githubusercontent.com/intellifactory/websharper/master/docs/images/ok.png" alt="X"/></td>
            <td><img src="https://raw.githubusercontent.com/intellifactory/websharper/master/docs/images/ok.png" alt="X"/></td>
            <td><img src="https://raw.githubusercontent.com/intellifactory/websharper/master/docs/images/ok.png" alt="X"/></td>
            <td></td>
            <td><img src="https://raw.githubusercontent.com/intellifactory/websharper/master/docs/images/ok.png" alt="X"/></td>
        </tr>
        <tr>
            <td>Single-Page App</td>
            <td><img src="https://raw.githubusercontent.com/intellifactory/websharper/master/docs/images/ok.png" alt="X"/></td>
            <td></td>
            <td><img src="https://raw.githubusercontent.com/intellifactory/websharper/master/docs/images/ok.png" alt="X"/></td>
            <td></td>
            <td><img src="https://raw.githubusercontent.com/intellifactory/websharper/master/docs/images/ok.png" alt="X"/></td>
        </tr>
        <tr class="header">
            <th colspan="6">Helpers</th>
        </tr>
        <tr>
            <td>Library</td>
            <td><img src="https://raw.githubusercontent.com/intellifactory/websharper/master/docs/images/ok.png" alt="X"/></td>
            <td></td>
            <td><img src="https://raw.githubusercontent.com/intellifactory/websharper/master/docs/images/ok.png" alt="X"/></td>
            <td><img src="https://raw.githubusercontent.com/intellifactory/websharper/master/docs/images/ok.png" alt="X"/></td>
            <td><img src="https://raw.githubusercontent.com/intellifactory/websharper/master/docs/images/ok.png" alt="X"/></td>
        </tr>
        <tr>
            <td>Extension</td>
            <td><img src="https://raw.githubusercontent.com/intellifactory/websharper/master/docs/images/ok.png" alt="X"/></td>
            <td></td>
            <td><img src="https://raw.githubusercontent.com/intellifactory/websharper/master/docs/images/ok.png" alt="X"/></td>
            <td></td>
            <td><img src="https://raw.githubusercontent.com/intellifactory/websharper/master/docs/images/ok.png" alt="X"/></td>
        </tr>
        <tr>
            <td>ASP.NET Container</td>
            <td></td>
            <td></td>
            <td></td>
            <td><img src="https://raw.githubusercontent.com/intellifactory/websharper/master/docs/images/ok.png" alt="X"/></td>
            <td></td>
        </tr>
    </tbody>
</table>

  * **Client** - Contains code compiled to JavaScript, executing on the client-side.

  * **Server** - Contains code you deploy for your application, executing on your server-side.

  * **Remote** - Contains code you from someone else's server, typically through a web service.

## MSBuild / XBuild Project File Configuration

WebSharper project files must include the file `WebSharper.targets`, which defines the necessary compilation tasks. The type of project is driven by the property `WebSharperProject`. Here are the possible values:

* `Site`: for Client-Server Applications.
    * Compiles JavaScript-annotated code;
    * Extracts resources in the folders `Content/WebSharper` and `Scripts/WebSharper`.
* `Html`: for HTML Applications.
    * Compiles JavaScript-annotated code;
    * Generates HTML files in `$(WebSharperHtmlDirectory)` and extracts resources in `$(WebSharperHtmlDirectory)/Content/WebSharper` and `$(WebSharperHtmlDirectory)/Scripts/WebSharper`.
* `Bundle`: for Single-Page Applications.
    * Compiles JavaScript-annotated code;
    * Extracts and concatenates resources into the folder `Content`.
* `Library`: for Libraries.
    * Compiles JavaScript-annotated code.
* `InterfaceGenerator`: For Extensions.
    * Compiles the classes defined using WIG into an assembly.