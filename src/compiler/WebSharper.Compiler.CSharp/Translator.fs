module WebSharper.Compiler.CSharp.Translator

//open Microsoft.FSharp.Compiler.SourceCodeServices
open Microsoft.CodeAnalysis.CSharp
open System.Collections.Generic

open WebSharper.Core.AST
open WebSharper.Core.Metadata

open Microsoft.CodeAnalysis
open Microsoft.CodeAnalysis.CSharp
open Microsoft.CodeAnalysis.CSharp.Syntax

module A = WebSharper.Compiler.Common.AttributeReader

type FSharpAttributeReader() =
    inherit A.AttributeReader<AttributeData, INamedTypeSymbol>()
    override this.GetAssemblyName attr = attr.AttributeClass.ContainingAssembly.Name
    override this.GetName attr = attr.AttributeClass.Name
    override this.GetCtorArgs attr = attr.ConstructorArguments |> Seq.map (fun a -> a.Value) |> Array.ofSeq          
    override this.GetTypeDef o = ToCSharpAST.getNamedTypeDefinition (o :?> INamedTypeSymbol)

let attrReader = FSharpAttributeReader()

