namespace WebSharperExtension

module Main =
    open IntelliFactory.WebSharper.InterfaceGenerator

    do Compiler.Compile stdout Module.Assembly
