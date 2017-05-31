param($installPath, $toolsPath, $package, $project)

try { $project.Object.AnalyzerReferences.Remove((Join-Path $toolsPath "FSharp.Core.dll")) } catch { }
try { $project.Object.AnalyzerReferences.Remove((Join-Path $toolsPath "System.ValueTuple.dll")) } catch { }
try { $project.Object.AnalyzerReferences.Remove((Join-Path $toolsPath "Mono.Cecil.dll")) } catch { }
try { $project.Object.AnalyzerReferences.Remove((Join-Path $toolsPath "Mono.Cecil.Mdb.dll")) } catch { }
try { $project.Object.AnalyzerReferences.Remove((Join-Path $toolsPath "Mono.Cecil.Pdb.dll")) } catch { }
try { $project.Object.AnalyzerReferences.Remove((Join-Path $toolsPath "WebSharper.Core.JavaScript.dll")) } catch { }
try { $project.Object.AnalyzerReferences.Remove((Join-Path $toolsPath "WebSharper.Core.dll")) } catch { }
try { $project.Object.AnalyzerReferences.Remove((Join-Path $toolsPath "WebSharper.InterfaceGenerator.dll")) } catch { }
try { $project.Object.AnalyzerReferences.Remove((Join-Path $toolsPath "WebSharper.Compiler.dll")) } catch { }
try { $project.Object.AnalyzerReferences.Remove((Join-Path $toolsPath "WebSharper.Compiler.CSharp.dll")) } catch { }
try { $project.Object.AnalyzerReferences.Remove((Join-Path $toolsPath "WebSharper.CSharp.Analyzer.dll")) } catch { }
