// $begin{copyright}
//
// This file is part of WebSharper
//
// Copyright (c) 2008-2016 IntelliFactory
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

namespace WebSharper.InterfaceGenerator

open System
open System.CodeDom
open System.IO
open Microsoft.CSharp

module internal Util =

    /// Quotes a string, returning a string literal.
    let Quote (text: string) =
        use writer = new StringWriter()
        use provider = new CSharpCodeProvider()
        let expr = new CodePrimitiveExpression(text)
        provider.GenerateCodeFromExpression(expr, writer, null)
        writer.ToString()

[<assembly: System.Runtime.CompilerServices.InternalsVisibleToAttribute 
    "WebSharper.Compiler, PublicKey=\
    0024000004800000940000000602000000240000525341310004000001000100b9c3a2acca0ecc\
    b7f45a6050dfd600ee0c17a677ef9976af57d490e286482049c825b71b9285fa2c6c711eaff63f\
    9ccfbdd18e88ad7a5b30b45bd7efe8cd871a2c45bdca3ae30ef057db8e12490e7f6e2067a78eba\
    aaba11b7f96ff862afaed79ef20b2e902375b3c755429cff93a1ef76a6f70fd849aed9e2c6fb2a\
    e9bfaff0">]
[<assembly: System.Runtime.CompilerServices.InternalsVisibleToAttribute
    "WebSharper.Compiler, PublicKey=\
    0024000004800000940000000602000000240000525341310004000011000000538656ad14eec4\
    dfae83f35e50b2b3fb19a37c98898ba3d41cbc82a9b47723771158c65dbedb0ca7e68165612e90\
    28285f9eb045d5e32991ef932fe3341e7fd0d8afa8e926c7ca35fb68cb349a6e45b240d0314588\
    0fe391390887bc7d284ef5338912cb791c1590b233c2fd50de2e565616c77a4c494ab79997d287\
    f6a31f86">]
()
