# Mobile Applications

WebSharper lets you leverage the JavaScript runtime to run your code
on mobile devices. There are three broad approaches to mobile
development with WebSharper:

* HTML5 Applications
* [WebSharper.Mobile][wsm]
* Adobe PhoneGap / Apache Cordova

## HTML5 Applications

The simplest way to get started with mobile is to create HTML5
applications that would run on a mobile device via a browser.

Pros:

* It is easy to get started
* Your app works on every platform, mobile or otherwise
* Your app has access to some of the APIs via HTML5

Cons:

* Your app is not considered a "native" app - less convenient to start
* It cannot be distributed through a Store
* Your app has no access to native APIs that have not yet made it into
  the HTML5 standard

### Mobile app frameworks

Fully typed extensions are available for two popular JavaScript frameworks for mobile development: [JQuery Mobile][jqm] and [Sencha Touch][stouch]. They provide customizable and themeable UI elements to use in your HTML5 app designed for mobile touchscreens.

## WebSharper.Mobile

WebSharper.Mobile peforms two functions for your JavaScript/HTML5 app:

* Provides a native wrapper - a thin native app that launches the
  browser on the mobile device and yields control to your
  JavaScript/HTML5 code
  
* Makes more APIs available to your app

Consider using WebSharper.Mobile if your application requires the
extra APIs, or needs to have a "native" wraper for Store distribution
or user convenience.

Pros:

* Builds native apps
* Provides More APIs than HTML5

Cons:

* Only available for Windows Phone and Android platforms

Since the 2.5 version of WebSharper, WebSharper.Mobile is available as
a separate project.

Links:

* [WebSharper.Mobile][wsm] homepage with downloads and more information
* [WebSharper.Mobile manual][wsm-manual]

## Adobe PhoneGap / Apache Cordova

We have plans to support building PhoneGap/Cordova applications with
WebSharper. Similarly to WebSharper.Mobile, PhoneGap provides native
wrappers and uniform API for mobile HTML5/JavaScript applications. In
addition, it has the following advantages:

* Works on every major mobile platform
* Support from Adobe
* Comes with useful tooling such as PhoneGap Build

[wsm]: http://github.com/intellifactory/websharper.mobile
[wsm-manual]: http://github.com/intellifactory/websharper.mobile/blob/master/docs/WebSharperMobile.md
[jqm]: http://jquerymobile.com/
[stouch]: http://www.sencha.com/products/touch/