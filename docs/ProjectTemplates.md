## Main Application Templates
The three main WebSharper application types are:

 1. <b>Client-Server Application</b> - a full client-server application implemented as a WebSharper sitelet.
    <p>The sitelet uses a dynamic HTML template and placeholders (<code>&lt;div data-replace="placeholder" /&gt;</code>,
        <code>&lt;div data-hole="placeholder" /&gt;</code> for HTML content, and <code>${placeholder}</code> for string
        content) that are instantiated by the sitelet.
    </p>
    <p>See the <a href="/docs/html-templates">HTML templates</a> page for more information on dynamic templating.</p>
    These applications exist in two flavors: a <b>Client-Server Web Application</b> runs as an ASP.NET module, while a
    <b>Self-Hosted Client-Server Web Application</b> runs as a self-contained executable using an OWIN self-host container.

 2. <b>HTML Application</b> - a multi-page HTML/JavaScript application based on a sitelet.
    <p>Similar to a Sitelet Website, this application also uses dynamic HTML templates and creates a set of HTML
        and JavaScript files that can be deployed in any HTML container.
    </p>
    <p>WebSharper mobile web applications can also be created from this template.</p>

 3. <b>Single-Page Application</b> - a single-page HTML/JavaScript application with an HTML page and an F# source file that plugs content into it.
    <p>The easiest way to get started with WebSharper: write a few lines of F#, add a placeholder for it in the HTML, and you are ready to go.</p>
    <p>See the "Your first app in 2 minutes" tab for an example.</p>

## Inner workings
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
            <td><img src="images/ok.png" alt="X"/></td>
            <td><img src="images/ok.png" alt="X"/></td>
            <td><img src="images/ok.png" alt="X"/></td>
            <td><img src="images/ok.png" alt="X"/></td>
            <td><img src="images/ok.png" alt="X"/></td>
        </tr>
        <tr>
            <td>HTML App</td>
            <td><img src="images/ok.png" alt="X"/></td>
            <td><img src="images/ok.png" alt="X"/></td>
            <td><img src="images/ok.png" alt="X"/></td>
            <td></td>
            <td><img src="images/ok.png" alt="X"/></td>
        </tr>
        <tr>
            <td>Single-Page App</td>
            <td><img src="images/ok.png" alt="X"/></td>
            <td></td>
            <td><img src="images/ok.png" alt="X"/></td>
            <td></td>
            <td><img src="images/ok.png" alt="X"/></td>
        </tr>
        <tr class="header">
            <th colspan="6">Helpers</th>
        </tr>
        <tr>
            <td>Library</td>
            <td><img src="images/ok.png" alt="X"/></td>
            <td></td>
            <td><img src="images/ok.png" alt="X"/></td>
            <td><img src="images/ok.png" alt="X"/></td>
            <td><img src="images/ok.png" alt="X"/></td>
        </tr>
        <tr>
            <td>Extension</td>
            <td><img src="images/ok.png" alt="X"/></td>
            <td></td>
            <td><img src="images/ok.png" alt="X"/></td>
            <td></td>
            <td><img src="images/ok.png" alt="X"/></td>
        </tr>
        <tr>
            <td>ASP.NET Container</td>
            <td></td>
            <td></td>
            <td></td>
            <td><img src="images/ok.png" alt="X"/></td>
            <td></td>
        </tr>
    </tbody>
</table>

### Application Tiers

  * **Client** - Contains code compiled to JavaScript, executing on the client-side.

  * **Server** - Contains code you deploy for your application, executing on your server-side.

  * **Remote** - Contains code you from someone else's server, typically through a web service.

