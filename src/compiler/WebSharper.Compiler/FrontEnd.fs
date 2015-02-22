// $begin{copyright}
//
// This file is part of WebSharper
//
// Copyright (c) 2008-2014 IntelliFactory
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

namespace WebSharper.Compiler

module M = WebSharper.Core.Metadata
module R = WebSharper.Compiler.ReflectionLayer
module TSE = WebSharper.TypeScriptExporter

module FrontEnd =
    type Assembly = WebSharper.Compiler.Assembly
    type Bundle = WebSharper.Compiler.Bundle
    type CompiledAssembly = WebSharper.Compiler.CompiledAssembly
    type Content = WebSharper.Compiler.Content
    type EmbeddedFile = WebSharper.Compiler.EmbeddedFile
    type Loader = WebSharper.Compiler.Loader
    type ResourceContent = WebSharper.Compiler.ResourceContent
    type ResourceContext = WebSharper.Compiler.ResourceContext
    type Symbols = WebSharper.Compiler.Symbols

    type Options =
        {
            ErrorLimit : int
            KeyPair : option<StrongNameKeyPair>
            References : list<Assembly>
            IncludeSourceMap : bool
        }

        static member Default =
            {
                ErrorLimit = 20
                KeyPair = None
                References = []
                IncludeSourceMap = false
            }

    [<Sealed>]
    type Compiler(errorLimit: int, log: Message -> unit, ctx: Context) =

        member this.Compile(quotation: Quotations.Expr, context: System.Reflection.Assembly, ?name) : option<CompiledAssembly> =
            this.CompileAssembly(R.Dynamic.FromQuotation quotation context (defaultArg name "Example"), false)

        member this.Compile(quotation: Quotations.Expr, ?name) : option<CompiledAssembly> =
            this.Compile(quotation, System.Reflection.Assembly.GetCallingAssembly(), ?name = name)

        member this.CompileAssembly(assembly: R.AssemblyDefinition, sourceMap: bool) : option<CompiledAssembly> =
            let succ = ref true
            let err (m: Message) =
                match m.Priority with
                | Priority.Warning -> ()
                | _ -> succ := false
                log m
            let logger = Logger.Create err errorLimit
            let meta = Metadata.Union logger ctx.MetadataRecords
            let pool = Inlining.Pool.Create logger
            let macros = Reflector.Pool.Create logger
            try
                let ra = Reflector.Reflect logger assembly
                let pkg = Resolver.Resolve logger ra
                let va = Validator.Validate logger pool macros (Metadata.Fields meta) ra
                let rm = Analyzer.Analyze ctx.AssemblyInfos va
                let local = Metadata.Parse logger va
                let joined = Metadata.Union logger [meta; local]
                Assembler.Assemble logger pool macros joined va
                if !succ then
                    let mInfo = M.Info.Create (rm :: ctx.AssemblyInfos)
                    let pkg = pkg.Value
                    let tsDecls = TSE.ExportDeclarations joined va
                    Some (CompiledAssembly.Create(ctx, assembly, local, rm, mInfo, pkg, tsDecls, sourceMap))
                else None
            with ErrorLimitExceeded -> None

        member this.CompileAndModify(assembly: Assembly, ?sourceMap: bool) : bool =
            let sourceMap = defaultArg sourceMap false
            let asm = R.Cecil.AdaptAssembly assembly.Raw
            match this.CompileAssembly(asm, sourceMap) with
            | None -> false
            | Some a -> a.WriteToCecilAssembly(assembly.Raw); true

    let Prepare (options: Options) (log: Message -> unit) : Compiler =
        let ctx = Context.Get(options.References)
        Compiler(options.ErrorLimit, log, ctx)

    let Compile (options: Options) (log: Message -> unit) : Assembly -> bool =
        let c = Prepare options log
        fun aF -> c.CompileAndModify(aF, options.IncludeSourceMap)
