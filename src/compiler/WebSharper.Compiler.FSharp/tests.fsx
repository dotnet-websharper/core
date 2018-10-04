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
#I __SOURCE_DIRECTORY__
#r @"System.dll"
#r @"System.Configuration.dll"
#r @"System.Core.dll"
#r @"System.Numerics.dll"
#r @"System.Web.dll"
#r @"System.Xml.dll"
#r @"System.Xml.Linq.dll"
#r @"..\..\..\packages\FSharp.Compiler.Service\lib\net45\FSharp.Compiler.Service.dll"
// DEBUG
#r @"..\..\..\build\Debug\WebSharper.Core.JavaScript.dll"
#r @"..\..\..\build\Debug\WebSharper.Core.dll"
#r @"..\..\..\build\Debug\WebSharper.Compiler.dll"
#r @"..\..\..\build\Debug\WebSharper.Compiler.FSharp.dll"
// RELEASE
//#r @"..\..\..\build\Release\WebSharper.Core.JavaScript.dll"
//#r @"..\..\..\build\Release\WebSharper.Core.dll"
//#r @"..\..\..\build\Release\WebSharper.Compiler.dll"
//#r @"..\..\..\build\Release\WebSharper.Compiler.FSharp.dll"

// translation
let comp = WebSharper.Compiler.FSharp.Main.translateProject None @"D:\repo\websharper.csharp\src\stdlib\WebSharper.Main\WebSharper.Main.fsproj"
WebSharper.Compiler.Packager.packageAssembly comp
|> WebSharper.Compiler.Packager.exprToString WebSharper.Core.JavaScript.Readable

let prevMeta = None
let path =  @"D:\repo\websharper.csharp\src\stdlib\WebSharper.Main\WebSharper.Main.fsproj"

// compilation encoding
module M = WebSharper.Core.Metadata
module B = WebSharper.Core.Binary
try
    let eP = B.EncodingProvider.Create()
    eP.DeriveEncoding typeof<M.Compilation>
with B.NoEncodingException t ->
    failwithf "Failed to create binary encoder for type %s" t.FullName

