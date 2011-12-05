namespace IntelliFactory.WebSharper.JQueryExtension

module Main =
    open IntelliFactory.WebSharper.InterfaceGenerator
    do Compiler.Compile stdout JQuery.Assembly
