# WebSharper 2.5.114.49

Released: June 4, 2014.

We are working to support building on Mono and integrating with
MonoDevelop, Xamarin Studio and CloudSharper on Linux and Mac OS
X.  This release contains a revision of the build system and
templates to make heavier use of MSBuild tasks in place of
XML-defined MSBuild targets, working around bugs in XBuild
implementation of MSBuild shipping with Mono.

For more information on using WebSharper with MonoDevelop and
Xamarin Studio, please see
[monodevelop.websharper](https://github.com/intellifactory/monodevelop.websharper).

## Improvements:

* Support for 2-dimensional arrays and F# 3.1 slicing operators

* Upgraded to jQuery 1.11.0

* InterfaceGenerator: pattern for config objects with obsolete
  option amembers and Obsolete attribute helper

## Bug fixes:

* [Issue 179](http://bitbucket.org/IntelliFactory/websharper/issue/179)
* [Issue 226](http://bitbucket.org/IntelliFactory/websharper/issue/226) 
* [Issue 231](http://bitbucket.org/IntelliFactory/websharper/issue/231)
* [Issue 234](http://bitbucket.org/IntelliFactory/websharper/issue/234)
* [Issue 236](http://bitbucket.org/IntelliFactory/websharper/issue/236)
* [Issue 239](http://bitbucket.org/IntelliFactory/websharper/issue/239)
* [Issue 241](http://bitbucket.org/IntelliFactory/websharper/issue/241)
* [Issue 246](http://bitbucket.org/IntelliFactory/websharper/issue/246)
* [Issue 247](http://bitbucket.org/IntelliFactory/websharper/issue/247)
* [Issue 249](http://bitbucket.org/IntelliFactory/websharper/issue/249)
* [Issue 252](http://bitbucket.org/IntelliFactory/websharper/issue/252)


# WebSharper 2.5.98.29

Released: February 21, 2014.

This release brings asynchronous sitelets, brings back support for
targeting .NET 4.0 - 4.5.1, and offers several bug fixes.

* By popular demand, we have downgraded the WebSharper stack to work
  well on .NET 4.0, while also working with .NET 4.5 and 4.5.1.  Both
  F# 3.0 and F# 3.1 are now supported.

* Thanks to @Oenotria, WebSharper sitelets now support fully
  asynchronous execution, hooking into asynchronous API as an IIS
  handler.

## Bug fixes:

* [Issue 168](http://bitbucket.org/IntelliFactory/websharper/issue/168)
* [Issue 207](http://bitbucket.org/IntelliFactory/websharper/issue/207)
* [Issue 208](http://bitbucket.org/IntelliFactory/websharper/issue/208)
* [Issue 216](http://bitbucket.org/IntelliFactory/websharper/issue/216)
* [Issue 217](http://bitbucket.org/IntelliFactory/websharper/issue/217)
* [Issue 218](http://bitbucket.org/IntelliFactory/websharper/issue/218)
* [Issue 219](http://bitbucket.org/IntelliFactory/websharper/issue/219)

# WebSharper 2.5.93

Released: February 4, 2014.

This release brings a new and improved optimizer for the Core language
that is used as an intermediate form during compilation to JavaScript.
The optimizer is able to eliminate more JavaScript expressions,
generates more readable code, and improves JavaScript performance of
numeric code (such as code using `for` loops heavily) by 30-50%.

The release also fixes a number of bugs related to code generation and
extension templates.

## Bug fixes:

* [Issue #209](http://bitbucket.org/IntelliFactory/websharper/issue/209) 
* [Issue #210](http://bitbucket.org/IntelliFactory/websharper/issue/210)
* [Issue #212](http://bitbucket.org/IntelliFactory/websharper/issue/212)
* [Issue #213](http://bitbucket.org/IntelliFactory/websharper/issue/213)

# WebSharper 2.5

See [Release Notes for WebSharper 2.5](ReleaseNotes-2.5.md)
