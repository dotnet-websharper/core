open System
open System.IO
open System.Text


/// Replaces the copyright notice in a given file.
let replaceCopyright file (newCopyright: string []) =
    let lines = File.ReadLines file
    let text =
        use output = new StringWriter()
        output.NewLine <- Environment.NewLine
        let mutable skip = false
        for line in lines do
            if line.Contains "$begin{copyright}" then
                skip <- true
                for c in newCopyright do
                    output.WriteLine(c)
            if not skip then
                output.WriteLine line
            if line.Contains "$end{copyright}" then
                skip <- false
        output.ToString()
    File.WriteAllText(file, text)

/// Updates copyright notices in all F# files in a given folder.
let updateLicense license dir =
    let copyright =
        File.ReadAllLines(license)
        |> Array.map (fun line -> line.TrimEnd())
    let findFiles pattern dir =
        Directory.GetFiles(dir, pattern, SearchOption.AllDirectories)
    let ( ++ ) a b = Seq.append a b
    let files =
        findFiles "*.fs" dir
        ++ findFiles "*.fsi" dir
    for f in files do
        stdout.WriteLine("Replacing: {0}", f)
        replaceCopyright f copyright

/// Update all copyright notices in all F# files in WebSharper.
let updateAllLicenses () =
    let s = __SOURCE_DIRECTORY__
    let ( ++ ) a b = Path.Combine(a, b)
    updateLicense (s ++ "LICENSE.txt") (s ++ "..")
