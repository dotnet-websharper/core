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

module WebSharper.Compiler.Closures

open System.Collections.Generic

open WebSharper.Core
open WebSharper.Core.AST

module M = WebSharper.Core.Metadata

type CollectVariables() =
    inherit StatementVisitor()

    let vars = ResizeArray()

    override this.VisitFuncDeclaration(f, _, _, _) =
        vars.Add f

    override this.VisitVarDeclaration(v, _) =
        vars.Add v

    member private this.Vars = vars :> _ seq

    static member ScopeVars(s: Statement) =
        let c = CollectVariables() 
        c.VisitStatement(s)
        c.Vars

type Scope =
    {
        Vars : HashSet<Id>
        Captured : HashSet<Id>
        CaptureSets : ResizeArray<Set<Id> * option<SourcePos>>
    }

type ExamineClosures (logger: LoggerBase, comp: Compilation, moveNonCapturingFunctionsToTop) =
    inherit TransformerWithSourcePos(comp)

    let mutable outerScope = true
    let mutable scopeChain = []
    let topScopeVars = HashSet()
    let movedToTop = ResizeArray()

    do sprintf "analyzing closures"
    |> logger.Out

    member this.EnterScope(args, body) =
        scopeChain <- 
            {
                Vars = HashSet (Seq.append args (CollectVariables.ScopeVars(body)))
                Captured = HashSet()
                CaptureSets = ResizeArray()
            } :: scopeChain

    member this.Warning(pos, msg) =
        match pos with 
        | Some pos ->
            let oneCharPos =                 
                { pos with End = pos.Start }
            comp.AddWarning(Some oneCharPos, SourceWarning msg)
        | _ -> ()
    
    member this.Warning(msg) =
        this.Warning(this.CurrentSourcePos, msg)

    member this.ExitScope(?f) =
        let s = List.head scopeChain
        scopeChain <- List.tail scopeChain
        let res =
            match scopeChain with
            | c :: _ ->
                if s.Captured.Count = 0 then
                    c.CaptureSets.Add(Set.empty, this.CurrentSourcePos)
                    // This function captures no variables. Move to top scope if enabled.
                    moveNonCapturingFunctionsToTop
                else
                    for cv in s.Captured do
                        if not (c.Vars.Contains cv) then
                            c.Captured.Add cv |> ignore
                    //if s.Captured |> Seq.forall (c.Vars.Contains >> not) then
                    //    this.Warning("This function captures no variables from parent scope. Consider moving it to higher scope.")
                    c.CaptureSets.Add(Set s.Captured, this.CurrentSourcePos)
                    false
            | _ -> false
        let retained = 
            s.CaptureSets |> Seq.map fst |> Set.unionMany
            |>  match f with
                | Some f -> Set.remove f
                | _ -> id
        for cs, pos in s.CaptureSets do
            let diff = retained - cs
            if not (Set.isEmpty diff) then
                let names =
                    diff |> Seq.map (fun i -> defaultArg i.Name "_") |> String.concat ", "
                this.Warning(pos, "This function do not use all retained variables through capture: " + names)    
        res

    override this.TransformFuncDeclaration(f, args, body, gen) =
        // top scope is not a named function
        this.EnterScope(args, body)
        let res = base.TransformFuncDeclaration(f, args, body, gen)
        if this.ExitScope(f) then
            movedToTop.Add(res)  
            Empty  
        else res

    override this.TransformFunction(args, arr, ret, body) =
        if outerScope then
            outerScope <- false
            CollectVariables.ScopeVars(body) |> Seq.iter (topScopeVars.Add >> ignore)
            let trBody = this.TransformStatement body
            outerScope <- true
            Function(args, arr, ret, CombineStatements (trBody :: List.ofSeq movedToTop))
        else
            this.EnterScope(args, body)
            let trBody = this.TransformStatement body
            if this.ExitScope() then
                let f = Id.New("f", mut = false)
                movedToTop.Add(FuncDeclaration(f, args, body, []))
                Var f
            else Function(args, arr, ret, trBody)

    override this.TransformId(i) =
        match scopeChain with
        | s :: _ ->
            if not (s.Vars.Contains i) && not (topScopeVars.Contains i) then
                s.Captured.Add i |> ignore   
        | _ -> ()
        i
