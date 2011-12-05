/// Replaces the copyright notice in a given file.
let replaceCopyright file (newCopyright: string) =
    let lines = System.IO.File.ReadLines file
    let output = new System.IO.StringWriter()
    let mutable skip = false
    for line in lines do
        if line.Contains "$begin{copyright}" then
            skip <- true
            output.WriteLine newCopyright
        if not skip then
            output.WriteLine line
        if line.Contains "$end{copyright}" then
            skip <- false
    System.IO.File.WriteAllText(file, output.ToString())

/// Updates copyright notices in all F# files in a given folder.
let updateLicense license dir =
    let copyright = System.IO.File.ReadAllText(license).Trim()
    let findFiles pattern dir =
        System.IO.Directory.GetFiles(dir, pattern, 
            System.IO.SearchOption.AllDirectories)
    let ( ++ ) a b = Seq.append a b
    let files =
        findFiles "*.fs" dir
        ++ findFiles "*.fsi" dir
    for f in files do
        stdout.WriteLine("Replacing: {0}", f)
        replaceCopyright f copyright

