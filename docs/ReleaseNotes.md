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
