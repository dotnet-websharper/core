param($installPath, $toolsPath, $package, $project)

$project.Object.AnalyzerReferences.Add((Join-Path $toolsPath "WebSharper.CSharp.Analyzer.dll"))
