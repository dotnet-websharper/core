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

module WebSharper.Compiler.FSharp.ErrorPrinting

open FSharp.Compiler.Diagnostics
open WebSharper.Core
open WebSharper.Compiler
open WebSharper.Compiler.ErrorPrinting

type WarnSettings =
    {
        NoWarn : int Set
        WarnLevel : int
        WarnAsError : int Set
        AllWarnAsError : bool
        DontWarnAsError : int Set
        VSStyleErrors : bool
    }    

    static member Default =
        {
            // see https://github.com/fsharp/FSharp.Compiler.Service/blob/533e728f08f4f9f8527b58877d377f9d6eed09ce/src/fsharp/CompileOps.fs#L403
            NoWarn = Set [ 1182; 3218; 3180; 3186 ] 
            WarnLevel = 3 
            WarnAsError = Set []
            AllWarnAsError = false
            DontWarnAsError = Set []
            VSStyleErrors = false
        }   
        
    member this.CheckNoWarn(c) =
        this.NoWarn.Contains(c)

    member this.CheckWarnAsError(c) =
        this.WarnAsError.Contains(c)
        || (this.AllWarnAsError && this.DontWarnAsError.Contains(c))

let PrintGlobalError (logger: LoggerBase) err =
    sprintf "output error FS9001: %s" (NormalizeErrorString err)
    |> logger.Error

let PrintGlobalWarning (warnSettings: WarnSettings) (logger: LoggerBase) err =
    if warnSettings.CheckNoWarn(9002) then
        ()
    elif warnSettings.CheckWarnAsError(9002) then
        PrintGlobalError logger err
    else
        sprintf "output warning FS9002: %s" (NormalizeErrorString err)
        |> logger.Error

// see https://github.com/fsharp/FSharp.Compiler.Service/blob/533e728f08f4f9f8527b58877d377f9d6eed09ce/src/fsharp/CompileOps.fs#L380
let private Level5Warnings =
    System.Collections.Generic.HashSet [
        21   // RecursiveUseCheckedAtRuntime
        22   // LetRecEvaluatedOutOfOrder
        52   // DefensiveCopyWarning
        45   // FullAbstraction
        1178 // 1178,tcNoComparisonNeeded1,"The struct, record or union type '%s' is not structurally comparable because the type parameter %s does not satisfy the 'comparison' constraint. Consider adding the 'NoComparison' attribute to the type '%s' to clarify that the type is not comparable"
             // 1178,tcNoComparisonNeeded2,"The struct, record or union type '%s' is not structurally comparable because the type '%s' does not satisfy the 'comparison' constraint. Consider adding the 'NoComparison' attribute to the type '%s' to clarify that the type is not comparable"
             // 1178,tcNoEqualityNeeded1,"The struct, record or union type '%s' does not support structural equality because the type parameter %s does not satisfy the 'equality' constraint. Consider adding the 'NoEquality' attribute to the type '%s' to clarify that the type does not support structural equality"
             // 1178,tcNoEqualityNeeded2,"The struct, record or union type '%s' does not support structural equality because the type '%s' does not satisfy the 'equality' constraint. Consider adding the 'NoEquality' attribute to the type '%s' to clarify that the type does not support structural equality"
    ]

let PrintFSharpErrors (settings: WarnSettings) (logger: LoggerBase) (errs: FSharpDiagnostic[]) =
    for err in errs do
        let isError, isPrinted =
            if err.Severity = FSharpDiagnosticSeverity.Error then
                true, true
            else
                let n = err.ErrorNumber
                if settings.WarnAsError.Contains n then
                    true, true
                elif settings.NoWarn.Contains n || (settings.WarnLevel < 5 && Level5Warnings.Contains n) then
                    false, false
                elif settings.AllWarnAsError && not (settings.DontWarnAsError.Contains n) then
                    true, true
                else
                    false, true
                    
        if isPrinted then 
            let pos =
                let fn = err.FileName
                if fn <> "unknown" && fn <> "startup" && fn <> "commandLineArgs" then
                    let file = fn.Replace("/","\\")
                    sprintf "%s(%d,%d,%d,%d): " file err.StartLine (err.StartColumn + 1) err.EndLine (err.EndColumn + 1)
                else ""
            let info =
                sprintf "%s%s FS%04d: " (if settings.VSStyleErrors then err.Subcategory + " " else "") (if isError then "error" else "warning") err.ErrorNumber
            sprintf "%s%s%s" pos info (NormalizeErrorString err.Message)
            |> logger.Error

// We need to print full rooted path for VS to correctly link the error to the file
open System.IO
let fullpath cwd (nm: string) = 
    let p = if Path.IsPathRooted(nm) then nm else Path.Combine(cwd,nm)
    try Path.GetFullPath(p) with _ -> p

let PrintWebSharperErrors warnOnly (projFile: string) (warnSettings: WarnSettings) (logger: LoggerBase) (comp: Compilation) =
    let projDir = Path.GetDirectoryName projFile
    let printWebSharperError (pos: AST.SourcePos option) isError msg =
        if (not isError || warnOnly) && warnSettings.CheckNoWarn(9002) then
            ()     
        else
            let subcategory =
                if warnSettings.VSStyleErrors then "output " else ""        
            let severity =
                if (isError && not warnOnly) || (not isError && warnSettings.CheckWarnAsError(9002)) then 
                    "error FS9001" 
                else 
                    "warning FS9002"
            match pos with
            | Some pos ->
                sprintf "%s(%d,%d,%d,%d): %s%s: %s" 
                    (fullpath projDir pos.FileName) (fst pos.Start) (snd pos.Start) (fst pos.End) (snd pos.End)
                    subcategory severity (NormalizeErrorString msg)
                |> logger.Error
            | _ ->
                sprintf "%s%s: %s" subcategory severity (NormalizeErrorString msg)
                |> logger.Error
    
    for pos, err in comp.Errors do
        printWebSharperError pos true (string err)
    for pos, err in comp.Warnings do
        printWebSharperError pos false (string err)
