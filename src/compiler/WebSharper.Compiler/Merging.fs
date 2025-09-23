module WebSharper.Compiler.Merging

open System
open System.IO
open WebSharper.Compiler

// netstandard2.0 does not have Path.GetRelativePath, implementing it via Uri
let getRelativePath (fromPath: string) (toPath: string) =
    let appendDirectorySeparatorChar (path: string) =
        if not (path.EndsWith("/") || path.EndsWith("\\")) then
            path + "/"
        else
            path
    let fromUri = Uri(appendDirectorySeparatorChar fromPath)
    let toUri = Uri(toPath)
    let relativeUri = fromUri.MakeRelativeUri(toUri)
    Uri.UnescapeDataString(relativeUri.ToString())

let getAllFilesRelative (dir: string) =
    Directory.GetFiles(dir, "*.*", SearchOption.AllDirectories)
    |> Array.map (fun f -> getRelativePath dir f)

let deleteDir (logger: LoggerBase) (dir: string) =
    if Directory.Exists(dir) then
        logger.Out $"Deleting directory {dir}"
        Directory.Delete(dir, true)

let copyAll (logger: LoggerBase) (src: string) (dst: string) =
    if Directory.Exists(src) then
        logger.Out $"Copying directory {src} -> {dst}"
        for file in Directory.GetFiles(src, "*.*", SearchOption.AllDirectories) do
            let rel = getRelativePath src file
            let target = Path.Combine(dst, rel)
            let targetDir = Path.GetDirectoryName(target)
            if not (Directory.Exists(targetDir)) then
                Directory.CreateDirectory(targetDir) |> ignore
            File.Copy(file, target, true)

type UnrollableIOActions(logger: LoggerBase) =
    let actions = ResizeArray()
    member this.Add(action: unit -> unit, undo: unit -> unit, currentUndoNeeded: bool, msg: string) =
        actions.Add((action, undo, currentUndoNeeded, msg))
    member this.Run() =
        for i, (action, _, currentUndoNeeded, msg) in Seq.indexed actions do
            try
                action()
            with e ->
                logger.Error $"WarpMerge error WM9011: {msg}: {e.Message}"
                for (_, undo, _, _) in Seq.take (i + if currentUndoNeeded then 1 else 0) actions |> Seq.rev do
                    undo()
                reraise()

let moveAllSafe (logger: LoggerBase) (src: string) (dst: string) =
    let dstBackup = dst + "_Backup"
    let dstExists = Directory.Exists(dst)

    let act = UnrollableIOActions(logger)
    let copyAll = copyAll logger
    let deleteDir = deleteDir logger

    act.Add((fun () -> if dstExists then copyAll dst dstBackup), (fun () -> deleteDir dstBackup), true, $"Failed to backup {dst}")

    act.Add((fun () -> deleteDir dst), (fun () -> copyAll dstBackup dst), false, $"Failed to delete {dst}")

    act.Add((fun () -> copyAll src dst), (fun () -> deleteDir dst; if dstExists then copyAll dstBackup dst), true, $"Failed to copy {src} to {dst}")

    act.Run()

    deleteDir dstBackup

let switchAllSafe2 (logger: LoggerBase) (src1dst2: string) (dst1: string) (src2: string) =
    let dst1Backup = dst1 + "_Backup"
    let dst2Backup = src1dst2 + "_Backup"
    let dst1Exists = Directory.Exists(dst1)
    let dst2Exists = Directory.Exists(src1dst2)
       
    let act = UnrollableIOActions(logger)
    let copyAll = copyAll logger
    let deleteDir = deleteDir logger

    act.Add((fun () -> if dst1Exists then copyAll dst1 dst1Backup), (fun () -> deleteDir dst1Backup), true, $"Failed to backup {dst1}")

    act.Add((fun () -> if dst2Exists then copyAll src1dst2 dst2Backup), (fun () -> deleteDir dst2Backup), true, $"Failed to backup {src1dst2}")

    act.Add((fun () -> deleteDir dst1), (fun () -> copyAll dst1Backup dst1), false, $"Failed to delete {dst1}")
    
    act.Add((fun () -> copyAll src1dst2 dst1), (fun () -> deleteDir dst1; if dst1Exists then copyAll dst1Backup dst1), true, $"Failed to copy {src1dst2} to {dst1}")

    act.Add((fun () -> copyAll src2 src1dst2), (fun () -> deleteDir src1dst2; if dst2Exists then copyAll dst2Backup src1dst2), true, $"Failed to copy {src2} to {src1dst2}")

    act.Run()

    deleteDir src2
    deleteDir dst1Backup
    deleteDir dst2Backup

[<Literal>]
let missingMarker = "<<<<<<< MISSING >>>>>>>"

let missingMarkerBinary = System.Text.Encoding.UTF8.GetBytes(missingMarker)

let binaryMergeConflictMarker = System.Text.Encoding.UTF8.GetBytes("<<<<<<< BINARY MERGE CONFLICT >>>>>>>")

let readOpt dir file =
    let path = Path.Combine(dir, file)
    if File.Exists(path) then
        File.ReadAllText(path)
    else
        missingMarker

let writeOpt dir file content =
    let path = Path.Combine(dir, file)
    if content = missingMarker then
        if File.Exists(path) then
            File.Delete(path)
    else
        let targetDir = Path.GetDirectoryName(path)
        if not (Directory.Exists(targetDir)) then
            Directory.CreateDirectory(targetDir) |> ignore
        File.WriteAllText(path, content)

let readBinaryOpt dir file =
    let path = Path.Combine(dir, file)
    if File.Exists(path) then
        File.ReadAllBytes(path)
    else
        missingMarkerBinary

let writeBinaryOpt dir file content =
    let path = Path.Combine(dir, file)
    if content = missingMarkerBinary then
        if File.Exists(path) then
            File.Delete(path)
    else
        let targetDir = Path.GetDirectoryName(path)
        if not (Directory.Exists(targetDir)) then
            Directory.CreateDirectory(targetDir) |> ignore
        File.WriteAllBytes(path, content)

let finishMerge (logger: LoggerBase) baselineDir outputDir modifiedDir conflictDir =
    
    if Directory.Exists(conflictDir) then
        let conflictFiles = getAllFilesRelative conflictDir
        let mutable allSuccessful = true
        for file in conflictFiles do
            let conflictText = readOpt conflictDir file
            if conflictText.Contains("<<<<<<<") then
                allSuccessful <- false
                logger.Error $"{Path.Combine(conflictDir, file)}: WarpMerge error WM9012: Unresolved merge conflict"
        if allSuccessful then
            switchAllSafe2 logger outputDir baselineDir conflictDir
            deleteDir logger modifiedDir
            0
        else
            1
    else 
        0

let prepareForBuild (logger: LoggerBase) baselineDir outputDir modifiedDir conflictDir =
    if Directory.Exists(baselineDir) then 
        moveAllSafe logger outputDir modifiedDir

let textFileExts =
    System.Collections.Generic.HashSet([|
        ".fs"
        ".fsx"
        ".cs"
        ".js"
        ".jsx"
        ".ts"
        ".tsx"
        ".css"
        ".scss"
        ".htm"
        ".html"
        ".json"
        ".xml"
    |])

let doMerge (logger: LoggerBase) baselineDir outputDir modifiedDir conflictDir =
    
    if not (Directory.Exists(baselineDir)) then
        copyAll logger outputDir baselineDir
        0
    else

    deleteDir logger conflictDir

    let differ = DiffPlex.ThreeWayDiffer.Instance
    let chunker = DiffPlex.Chunkers.LineEndingsPreservingChunker.Instance
   
    let baselineFiles = getAllFilesRelative baselineDir
    let modifiedFiles = getAllFilesRelative modifiedDir
    let outputFiles = getAllFilesRelative outputDir
    let allFiles = Seq.concat [ baselineFiles; modifiedFiles; outputFiles ] |> Seq.distinct |> Seq.toArray

    let mutable allSuccessful = true

    for file in allFiles do
        
        if textFileExts.Contains(Path.GetExtension(file)) then
            let baselineText = readOpt baselineDir file
            let modifiedText = readOpt modifiedDir file
            let outputText = readOpt outputDir file
            let merge = differ.CreateMerge(baselineText, modifiedText, outputText, false, false, chunker) 
            let mergedText = String.Concat(merge.MergedPieces)
            writeOpt conflictDir file mergedText
            if not merge.IsSuccessful then
                allSuccessful <- false
                logger.Error $"{Path.Combine(conflictDir, file)}: WarpMerge error WM9011: Merge conflict"
        else
            let baselineBytes = readBinaryOpt baselineDir file
            let modifiedBytes = readBinaryOpt modifiedDir file
            let outputBytes = readBinaryOpt outputDir file
            let mergedBytes, isSuccessful =
                if baselineBytes = modifiedBytes then
                    outputBytes, true
                elif baselineBytes = outputBytes then
                    modifiedBytes, true
                else
                    binaryMergeConflictMarker, false
            writeBinaryOpt conflictDir file mergedBytes
            if not isSuccessful then
                allSuccessful <- false
                logger.Error $"{Path.Combine(conflictDir, file)}: WarpMerge error WM9011: Merge conflict"

    if allSuccessful then
        switchAllSafe2 logger outputDir baselineDir conflictDir
        deleteDir logger modifiedDir
        0
    else
        1
