namespace IntelliFactory.WebSharper.MSBuild

open System
open System.IO
open Microsoft.Build.Framework
open Microsoft.Build.Utilities
open IntelliFactory.Core
open IntelliFactory.WebSharper.Compiler
module FE = FrontEnd

/// Commands handled by the WebSharper task (below).
type Command =
    | Compile
    | ComputeReferences

    /// Parses from string.
    static member Parse(command: string) =
        match command with
        | "Compile" -> Compile
        | "ComputeReferences" -> ComputeReferences
        | _ -> invalidArg "command" ("Invalid command name: " + command)

/// WebSharper project types handled by the task (below).
type ProjectType =
    | Library

    /// Parses from string.
    static member Parse(ty: string) =
        match ty with
        | "Library" -> Library
        | _ -> invalidArg "type" ("Invalid project type: " + ty)

[<AutoOpen>]
module private WebSharperTaskModule =

    [<Sealed>]
    type private Marker = class end

    let private BaseDir =
        typeof<Marker>.Assembly.Location
        |> Path.GetDirectoryName

    let GetReferences (ty: ProjectType) =
        [
            "IntelliFactory.Core"
            "IntelliFactory.Formlet"
            "IntelliFactory.Html"
            // "IntelliFactory.JavaScript"
            "IntelliFactory.Reactive"
            "IntelliFactory.WebSharper.Collections"
            // "IntelliFactory.WebSharper.Compiler"
            "IntelliFactory.WebSharper.Control"
            "IntelliFactory.WebSharper.Core"
            "IntelliFactory.WebSharper.Dom"
            "IntelliFactory.WebSharper.Ecma"
            "IntelliFactory.WebSharper.Formlet"
            "IntelliFactory.WebSharper.Html"
            "IntelliFactory.WebSharper.Html5"
            "IntelliFactory.WebSharper.InterfaceGenerator"
            "IntelliFactory.WebSharper.JQuery"
            "IntelliFactory.WebSharper.Sitelets"
            "IntelliFactory.WebSharper.Testing"
            "IntelliFactory.WebSharper.Web"
            "IntelliFactory.WebSharper"
            "IntelliFactory.Xml"
        ]

    let DoComputeReferences projTy : ITaskItem [] =
        let assemblies = GetReferences projTy
        let priv =
            match projTy with
            | ProjectType.Library -> false
            // | _ -> true
        [|
            for asm in assemblies do
                let hintPath = Path.Combine(BaseDir, asm + ".dll")
                if File.Exists(hintPath) then
                    let it = TaskItem(asm)
                    it.SetMetadata("HintPath", hintPath)
                    it.SetMetadata("Private", string priv)
                    yield it :> _
        |]

    let SendMessage (log: TaskLoggingHelper) (msg: Message) =
        match msg.Priority with
        | Priority.Critical
        | Priority.Error ->
            match msg.Location.SourceLocation with
            | Some loc ->
                log.LogError("WebSharper", "WebSharper", "WebSharper",
                    loc.File, loc.Line, loc.Column, loc.Line, loc.Column, msg.Text)
            | None ->
                log.LogError(string msg)
        | Priority.Warning ->
            match msg.Location.SourceLocation with
            | Some loc ->
                log.LogWarning("WebSharper", "WebSharper", "WebSharper",
                    loc.File, loc.Line, loc.Column, loc.Line, loc.Column,
                    msg.Text)
            | None ->
                log.LogWarning(string msg)

    let DoCompile (log: TaskLoggingHelper) (input: ITaskItem[]) =
        match List.ofArray input with
        | raw :: refs ->
            let rawInfo = FileInfo(raw.ItemSpec)
            let temp = raw.ItemSpec + ".tmp"
            let tempInfo = FileInfo(temp)
            if not tempInfo.Exists || tempInfo.LastWriteTimeUtc < rawInfo.LastWriteTimeUtc then
                let aR = AssemblyResolution.AssemblyResolver.Create()
                let refPaths =
                    Set [ for i in input -> Path.GetFullPath(i.ItemSpec) ]
                let aR = aR.SearchPaths(refPaths)
                aR.Wrap <| fun () ->
                    let loader = FE.Loader.Create aR (fun msg -> log.LogWarning(msg))
                    let refs = [ for r in refs -> loader.LoadFile(r.ItemSpec) ]
                    let opts = { FE.Options.Default with References = refs }
                    let compiler = FE.Prepare opts (SendMessage log)
                    let fileName = raw.ItemSpec
                    let assem = loader.LoadFile fileName
                    if not (compiler.CompileAndModify assem) then
                        failwith "Could not compile the assembly with WebSharper"
                    assem.Write None fileName
                File.WriteAllText(tempInfo.FullName, "")
        | _ ->
            failwith "Need 1+ items for Compile command"

/// Implements MSBuild logic used in WebSharper.targets
[<Sealed>]
type WebSharperTask() =
    inherit Task()

    override this.Execute() =
        try
            match Command.Parse this.Command with
            | Compile ->
                DoCompile this.Log this.ItemInput
                true
            | ComputeReferences ->
                this.ItemOutput <- DoComputeReferences (ProjectType.Parse this.ProjectType)
                true
        with e ->
            this.Log.LogErrorFromException(e)
            false

    /// Used to specify which "method" to call
    [<Required>]
    member val Command = "" with get, set

    /// Item input for item commands.
    member val ItemInput : ITaskItem [] = Array.empty with get, set

    /// Item output for item commands.
    [<Output>]
    member val ItemOutput : ITaskItem [] = Array.empty with get, set

    /// Specifies which project type is being built.
    member val ProjectType = "Library" with get, set
