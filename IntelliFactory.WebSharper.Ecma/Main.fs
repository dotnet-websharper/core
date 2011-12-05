namespace IntelliFactory.WebSharper.EcmaExtension

module Main =
    open IntelliFactory.WebSharper.InterfaceGenerator
    do Compiler.Compile stdout Ecma.Assembly
