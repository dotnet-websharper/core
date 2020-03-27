// $begin{copyright}
//
// This file is part of WebSharper
//
// Copyright (c) 2008-2018 IntelliFactory
//
// Licensed under the Apache License, Version 2.0 (the "License"); you
// may not use this file except in compliance with the License.  You may
// obtain a copy of the License at
//
//     http://www.apache.org/licenses/LICENSE-2.0
//
// Unless required by applicable law or agreed to in writing, software
// distributed under the License is distributed on an "AS IS" BASIS,
// WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or
// implied.  See the License for the specific language governing
// permissions and limitations under the License.
//
// $end{copyright}

// Reads F# quotations as WebSharper.Core.AST 
namespace WebSharper.Compiler

open FSharp.Quotations

open WebSharper.Core
open WebSharper.Core.AST

module M = WebSharper.Core.Metadata

type QuotationCompiler (?meta : M.Info) =
    let meta = defaultArg meta M.Info.Empty
    let comp = Compilation(meta)

    member this.Compilation = comp

    member this.CompileReflectedDefinitions() =
            


    member this.Compile (expr: Expr) = 
        let e = QuotationReader.readExpression comp expr
        Translator.
        ()