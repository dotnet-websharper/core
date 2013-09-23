TODO: update instructions for WebSharper 2.5

# Mobile Applications

WebSharper includes support for building mobile applications for the
Android and Windows Phone 7/7.1 platforms.  The strategy is to
pre-generate HTML, JavaScript and CSS files, and then package them as
a native application for the target platform, exposing some of the
native API to the JavaScript runtime.  In addition, the server-side
code can be deployed to a public server, and the mobile devices are
then able to communicate with it using the WebSharper Remoting/RPC
mechanism.

## Android Applications

To build Android applications with WebSharper you will need:

* JDK (Such as Sun Java SE Development Kit 1.7 update 3)
* Android SDK (Recommended versions: 2.2, 3.2)

You will also need either of the following:

* Eclipse and Android Development Tools (ADT) plugin
* Apache Ant

To create a new application, open Visual Studio and create a new
project using the "Android Application" template.  This creates a
variant of the standard WebSharper HTML site.  It also generates a new
Android application under the `android` folder.

### Building with Eclipse/ADT

The recommended way to build and debug the Android application is by
using Eclipse with the Android Development Tools - [ADT plugin][adt].
After installing the plugin, open Eclipse, import the project from the
file system, pointing it to the `android` sub-folder in your
WebSharper solution.

[adt]: http://developer.android.com/sdk/eclipse-adt.html
[ant]: "http://ant.apache.org

### Building with Ant

You can also build the application from the command-line or on a
continuous build server.  Install [Apache Ant][ant] and set
`ANT_HOME`, `JAVA_HOME` and `ANDROID_SDK` environment variables to the
installation paths of Ant, JDK, and Android SDK respectively.  Your
MSBuild-based builds will now invoke Ant to produce debug and release
Android packages automatically in the `android\bin` folder.

Note that release packages should be signed before distribution.  To
enable signing with command-line builds, generate a keystore with the
`keytool.exe` tool that comes with the JDK and configure
`android\ant.properties`.

## Windows Phone Applications

Windows Phone application development requires:

* Windows Vista or Windows 7
* Windows Phone 7.1 SDK
* Microsoft Visual Studio 2010 Service Pack 1

Create a new project using the "Windows Phone" template and build it.
The default build of the Mobile project starts a Windows Phone
Emulator and connects the debugger to it, enabling you to see log
messages from WebSharper in the Visual Studio Output console (Debug).

Consider tweaking the mobile project template to customize your
application metadata and enable signing.
