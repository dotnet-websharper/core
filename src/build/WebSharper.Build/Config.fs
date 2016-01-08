// $begin{copyright}
//
// This file is part of WebSharper
//
// Copyright (c) 2008-2015 IntelliFactory
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

namespace WebSharper.Build

open System

module Config =
    let PackageId = "Zafir"
    let FSharpPackageId = "Zafir.FSharp"
    let CSharpPackageId = "Zafir.CSharp"
    let CompilerPackageId = "Zafir.Compiler"
    let TestingPackageId = "Zafir.Testing"
    let PlainVersion = "4.0"
    let PackageVersion = PlainVersion
    let NumericVersion = Version(PlainVersion + ".0.0")
    let VersionSuffix = Some "alpha"
    let Company = "IntelliFactory"
    let Description = ".NET-to-JavaScript compiler and web application framework"
    let FSharpDescription = "F#-to-JavaScript compiler"
    let CSharpDescription = "C#-to-JavaScript compiler"
    let TestingDescription = "Testing framework for F#-to-JavaScript compiled libraries"
    let CompilerDescription = "Compiler libraries for C#/F#-to-JavaScript translation"
    let LicenseUrl = "http://websharper.com/licensing"
    let Tags = ["Web"; "JavaScript"; "F#"]
    let Website = "http://bitbucket.org/IntelliFactory/websharper"



