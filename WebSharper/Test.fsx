#I "bin/Debug"
#r "IntelliFactory.WebSharper.Core"
#r "IntelliFactory.WebSharper.Compiler"


module FE = IntelliFactory.WebSharper.Compiler.FrontEnd

let compiler = FE.Prepare FE.Options.Default (eprintfn "%O")

compiler.Compile <@ "OKAY" @>
