# WebSharper 2.5.93.22

This release brings a new and improved optimizer for the Core language
that is used as an intermediate form during compilation to JavaScript.
The optimizer is able to eliminate more JavaScript expressions,
generates more readable code, and improves JavaScript performance of
numeric code (such as code using `for` loops heavily) by 30-50%.

The release also fixes a number of bugs related to code generation and
extension templates.

## Bug fixes:

* Issue #209 
* Issue #210
* Issue #211
* Issue #213

# WebSharper 2.5

See [ReleaseNotes for WebSharper 2.5](ReleaseNotes-2.5.md)
