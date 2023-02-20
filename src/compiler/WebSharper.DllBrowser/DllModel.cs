using Microsoft.FSharp.Core;
using System;
using System.CodeDom;
using System.Collections.Generic;
using System.Linq;
using System.Reflection;
using System.Text;
using System.Threading.Tasks;
using System.Windows;
using WebSharper.Compiler;
using WebSharper.Core;
using WebSharper.Core.AST;
using static WebSharper.Core.Metadata;

namespace WebSharper.DllBrowser
{
    public abstract class TreeNodeModel
    {
        public abstract string Name { get; }
        public abstract List<TreeNodeModel> Contents { get; }
        public abstract string Details { get; }
    }

    public abstract class TreeGroupNodeModel : TreeNodeModel
    {
        public override List<TreeNodeModel> Contents { get; } = new();
        public override string Details => "";
    }

    public abstract class TreeLeafNodeModel : TreeNodeModel
    {
        private static List<TreeNodeModel> emptyContents = new();
        public override List<TreeNodeModel> Contents => emptyContents;
    }

    public class DllModel : TreeGroupNodeModel
    {
        public Compiler.Assembly Assembly { get; init; }
        public override List<TreeNodeModel> Contents { get; } = new();
        public DllModel(Compiler.Assembly assembly, Metadata.Info? meta)
        {
            Assembly = assembly;
            if (meta != null)
            {
                Contents.Add(new MetadataModel(meta));
                Contents.Add(new GraphModel(meta.Dependencies));
            }
            Contents.Add(new ResourcesModel(assembly));
        }

        public override string Name => Assembly.Name;
    }

    public class MetadataModel : TreeGroupNodeModel
    {
        public Metadata.Info Metadata { get; init; }
        public override string Name => "Metadata";
        public MetadataModel(Metadata.Info metadata) 
        {
            Metadata = metadata;
            foreach (var x in metadata.Classes.OrderBy(x => x.Key.Value.FullName))
            {
                Contents.Add(new ClassModel(x.Key.Value, x.Value));
            }
            foreach (var x in metadata.Interfaces.OrderBy(x => x.Key.Value.FullName))
            {
                Contents.Add(new InterfaceModel(x.Key.Value, x.Value));
            }
        }
    }

    public class GraphModel : TreeGroupNodeModel
    {
        public Metadata.GraphData Dependencies { get; init; }
        public override string Name => "Graph";
        public GraphModel(Metadata.GraphData dependencies)
        {
            Dependencies = dependencies;
            var i = 0;
            var nodesWithDeps = new List<NodeModel>();

            foreach (var x in dependencies.Nodes)
            {
                if (dependencies.Edges[i].Any())
                {
                    nodesWithDeps.Add(new NodeModel(x.ToString(), i, dependencies));
                }
                i++;
            }

            Contents.AddRange(nodesWithDeps.OrderBy(n => n.Name));
        }
    }

    public class ClassModel : TreeLeafNodeModel
    {
        public TypeDefinitionInfo Type { get; init; }
        public Tuple<Address, Metadata.CustomTypeInfo, FSharpOption<Metadata.ClassInfo>> ClassInfo { get; init; }
        public override string Name => "Class: " + Type.FullName;
        public override string Details
        {
            get
            {
                var sb = new StringBuilder();
                
                sb.Append("Address: ").AppendLine(ClassInfo.Item1.ToString().Replace("\n", ""));
                sb.AppendLine();

                if (!ClassInfo.Item2.IsNotCustomType)
                {
                    sb.AppendLine(ClassInfo.Item2.ToString());
                    sb.AppendLine();
                }

                var cls = ClassInfo.Item3?.Value;

                if (cls != null)
                {
                    foreach (var m in cls.Methods)
                    {
                        sb.AppendLine(m.Key.Value.ToString());
                        sb.Append("  CompiledForm: ").AppendLine(m.Value.CompiledForm.ToString().Replace("\n", ""));
                        sb.Append("  Optimizations: ").AppendLine(m.Value.Optimizations.ToString().Replace("\n", ""));
                        sb.Append("  Expression: ").AppendLine(Debug.PrintExpression(m.Value.Expression).Replace("\n", ""));
                        sb.AppendLine();
                    }
                }

                return sb.ToString();
            }
        }
        public ClassModel(TypeDefinitionInfo type, Tuple<Address, Metadata.CustomTypeInfo, FSharpOption<Metadata.ClassInfo>> classInfo)
        {
            Type = type;
            ClassInfo = classInfo;
        }
    }

    public class InterfaceModel : TreeLeafNodeModel
    {
        public TypeDefinitionInfo Type { get; init; }
        public Metadata.InterfaceInfo InterfaceInfo { get; init; }
        public override string Name => "Interface: " + Type.FullName;
        public override string Details
        {
            get
            {
                var sb = new StringBuilder();
                foreach (var m in InterfaceInfo.Methods)
                {
                    sb.AppendLine(m.Key.Value.ToString());
                    sb.Append("  CompiledName: ").AppendLine(m.Value.Item1);
                    sb.Append("  MemberKind: ").AppendLine(m.Value.Item2.ToString());
                    //sb.Append("  Generics: ").AppendLine(m.Value.Item3.ToString());
                    sb.AppendLine();
                }
                return sb.ToString();
            }
        }
        public InterfaceModel(TypeDefinitionInfo type, Metadata.InterfaceInfo interfaceInfo)
        {
            Type = type;
            InterfaceInfo = interfaceInfo;
        }
    }

    public class ResourcesModel : TreeGroupNodeModel
    {
        public Compiler.Assembly Assembly { get; init; }
        public override string Name => "Resources";
        public ResourcesModel(Compiler.Assembly assembly)
        {
            Assembly = assembly;
            
            var f = assembly.ReadableJavaScript;
            if (f != null)
            {
                Contents.Add(new SpecialFileModel("WebSharper.js", f.Value));
            }
            foreach (var x in assembly.GetScripts(Core.JavaScript.Output.JavaScript))
            {
                Contents.Add(new EmbeddedFileModel(x));
            }
            foreach (var x in assembly.GetScripts(Core.JavaScript.Output.TypeScriptDeclaration))
            {
                Contents.Add(new EmbeddedFileModel(x));
            }
            foreach (var x in assembly.GetScripts(Core.JavaScript.Output.TypeScript))
            {
                Contents.Add(new EmbeddedFileModel(x));
            }
            foreach (var x in assembly.GetContents())
            {
                Contents.Add(new EmbeddedFileModel(x));
            }
        }
    }

    public class SpecialFileModel : TreeLeafNodeModel
    {
        private string _name;
        public override string Name => _name;
        private string _details;
        public override string Details => _details;
        public SpecialFileModel(string name, string details)
        {
            _name = name;
            _details = details;
        }
    }

    public class EmbeddedFileModel : TreeLeafNodeModel
    {
        public Compiler.EmbeddedFile EmbeddedFile { get; init; }
        public override string Name => EmbeddedFile.FileName;
        public override string Details => EmbeddedFile.Content;
        public EmbeddedFileModel(EmbeddedFile embeddedFile)
        {
            EmbeddedFile = embeddedFile;
        }
    }

    public class NodeModel : TreeLeafNodeModel
    {
        public Metadata.GraphData Dependencies { get; init; }
        private string _name;
        private int _index;
        public override string Name => _name;
        public override string Details
        {
            get
            {
                var sb = new StringBuilder();
                sb.AppendLine("Depends on:");
                foreach (int d in Dependencies.Edges[_index])
                {
                    sb.Append("  ").AppendLine(Dependencies.Nodes[d].ToString());
                }
                return sb.ToString();
            }
        }

        public NodeModel(string name, int index, Metadata.GraphData dependencies)
        {
            _name = name;
            _index = index;
            Dependencies = dependencies;
        }
    }

}
