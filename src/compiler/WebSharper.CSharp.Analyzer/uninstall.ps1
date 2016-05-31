param($installPath, $toolsPath, $package, $project)

try { $project.Object.AnalyzerReferences.Remove((Join-Path $toolsPath "net45/FSharp.Core.dll")) } catch { }
try { $project.Object.AnalyzerReferences.Remove((Join-Path $toolsPath "net45/Mono.Cecil.dll")) } catch { }
try { $project.Object.AnalyzerReferences.Remove((Join-Path $toolsPath "net45/Mono.Cecil.Mdb.dll")) } catch { }
try { $project.Object.AnalyzerReferences.Remove((Join-Path $toolsPath "net45/Mono.Cecil.Pdb.dll")) } catch { }
try { $project.Object.AnalyzerReferences.Remove((Join-Path $toolsPath "net45/WebSharper.Core.JavaScript.dll")) } catch { }
try { $project.Object.AnalyzerReferences.Remove((Join-Path $toolsPath "net45/WebSharper.Core.dll")) } catch { }
try { $project.Object.AnalyzerReferences.Remove((Join-Path $toolsPath "net45/WebSharper.InterfaceGenerator.dll")) } catch { }
try { $project.Object.AnalyzerReferences.Remove((Join-Path $toolsPath "net45/WebSharper.Compiler.dll")) } catch { }
try { $project.Object.AnalyzerReferences.Remove((Join-Path $toolsPath "net45/WebSharper.Compiler.CSharp.dll")) } catch { }
try { $project.Object.AnalyzerReferences.Remove((Join-Path $toolsPath "net45/WebSharper.CSharp.Analyzer.dll")) } catch { }
