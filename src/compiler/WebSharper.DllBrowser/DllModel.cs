using Microsoft.FSharp.Collections;
using Microsoft.FSharp.Core;
//using Mono.Cecil.Cil;
using System;
using System.CodeDom;
using System.Collections.Generic;
//using System.Diagnostics;
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
        public abstract string GetDetails();
        private string? _details = null;
        public string Details
        {
            get
            {
                if (_details == null)
                {
                    _details = GetDetails();
                }
                return _details;
            }
        }
    }

    public abstract class TreeGroupNodeModel : TreeNodeModel
    {
        public override List<TreeNodeModel> Contents { get; } = new();
        public override string GetDetails() => "";
    }

    public abstract class TreeLeafNodeModel : TreeNodeModel
    {
        private readonly static List<TreeNodeModel> _emptyContents = new();
        public override List<TreeNodeModel> Contents => _emptyContents;
    }

    public class LoadingDllModel : TreeLeafNodeModel
    {
        private readonly string _name;
        public LoadingDllModel(string name)
        {
            _name = name;
        }

        public override string Name => "Loading: " + _name;
        public override string GetDetails() => "";
    }
    public class DllModel : TreeGroupNodeModel
    {
        public Compiler.Assembly Assembly { get; init; }
        public override List<TreeNodeModel> Contents { get; } = new();
        public DllModel(Compiler.Assembly assembly, Info? meta)
        {
            Assembly = assembly;
            if (meta != null)
            {
                Contents.Add(new MetadataModel(meta));
                Contents.Add(new GraphModel(meta.Dependencies));
                Contents.Add(new MacroEntriesModel(meta));
                Contents.Add(new QuotationsModel(meta));
            }
            Contents.Add(new ResourcesModel(assembly));
        }

        public override string Name => Assembly.Name;
    }

    public class MetadataModel : TreeGroupNodeModel
    {
        public Info Metadata { get; init; }
        public override string Name => "Metadata";
        public MetadataModel(Info metadata)
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
        public override string Name => "Graph";
        public GraphModel(GraphData dependencies)
        {
            var graph = Core.DependencyGraph.Graph.FromData(dependencies);
            var i = 0;
            var nodesWithDeps = new List<NodeModel>();

            foreach (var x in dependencies.Nodes)
            {
                var e = dependencies.Edges[i];
                graph.Overrides.TryGetValue(i, out var o);
                if (e.Length > 0 || o != null)
                {
                    nodesWithDeps.Add(new NodeModel(x, dependencies.Nodes, e, o));
                }
                i++;
            }

            Contents.AddRange(nodesWithDeps.OrderBy(n => n.Name));
        }
    }

    public class ClassModel : TreeLeafNodeModel
    {
        public TypeDefinitionInfo Type { get; init; }
        public Tuple<Address, CustomTypeInfo, FSharpOption<ClassInfo>> ClassInfo { get; init; }
        public override string Name => "Class: " + Type.FullName;
        public override string GetDetails()
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
                if (cls.BaseClass != null)
                {
                    sb.Append("BaseClass: ").AppendLine(cls.BaseClass.Value.ToString().Replace("\n", ""));
                    sb.AppendLine();
                }
                foreach (var m in cls.Constructors)
                {
                    sb.AppendLine(".ctor(" + m.Key.Value.ToString() + ")");
                    sb.Append("  CompiledForm: ").AppendLine(m.Value.CompiledForm.ToString().Replace("\n", ""));
                    sb.Append("  Optimizations: ").AppendLine(m.Value.Optimizations.ToString().Replace("\n", ""));
                    sb.Append("  Expression: ").AppendLine(Debug.PrintExpression(m.Value.Expression).Replace("\n", ""));
                    sb.AppendLine();
                }
                if (cls.StaticConstructor != null)
                {
                    sb.AppendLine(".cctor()");
                    sb.Append("  Statement: ").AppendLine(Debug.PrintStatement(cls.StaticConstructor.Value).Replace("\n", ""));
                    sb.AppendLine();
                }
                foreach (var m in cls.Methods)
                {
                    sb.AppendLine(m.Key.Value.ToString());
                    sb.Append("  CompiledForm: ").AppendLine(m.Value.CompiledForm.ToString().Replace("\n", ""));
                    sb.Append("  Optimizations: ").AppendLine(m.Value.Optimizations.ToString().Replace("\n", ""));
                    sb.Append("  Expression: ").AppendLine(Debug.PrintExpression(m.Value.Expression).Replace("\n", ""));
                    sb.AppendLine();
                }
                foreach (var f in cls.Fields)
                {
                    sb.AppendLine(f.Key.ToString());
                    sb.Append("  CompiledForm: ").AppendLine(f.Value.CompiledForm.ToString().Replace("\n", ""));
                }
                foreach (var m in cls.Implementations)
                {
                    sb.AppendLine(m.Key.Item1.Value.ToString() + ": " + m.Key.Item2.Value.ToString());
                    sb.Append("  CompiledForm: ").AppendLine(m.Value.CompiledForm.ToString().Replace("\n", ""));
                    sb.Append("  Expression: ").AppendLine(Debug.PrintExpression(m.Value.Expression).Replace("\n", ""));
                    sb.AppendLine();
                }

            }

            return sb.ToString();
        }
        public ClassModel(TypeDefinitionInfo type, Tuple<Address, CustomTypeInfo, FSharpOption<ClassInfo>> classInfo)
        {
            Type = type;
            ClassInfo = classInfo;
        }
    }

    public class InterfaceModel : TreeLeafNodeModel
    {
        public TypeDefinitionInfo Type { get; init; }
        public InterfaceInfo InterfaceInfo { get; init; }
        public override string Name => "Interface: " + Type.FullName;
        public override string GetDetails()
        {
            var sb = new StringBuilder();
            sb.Append("Address: ").AppendLine(InterfaceInfo.Address.ToString().Replace("\n", ""));
            if (!InterfaceInfo.Extends.IsEmpty)
            {
                sb.Append("Extends: ").AppendLine(string.Join(", ", InterfaceInfo.Extends.Select(i => i.ToString())));
            }
            if (!InterfaceInfo.Generics.IsEmpty)
            {
                sb.Append("Generics: ").AppendLine(string.Join(", ", InterfaceInfo.Generics.Select(i => i.ToString())));
            }
            sb.AppendLine();
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
        public InterfaceModel(TypeDefinitionInfo type, InterfaceInfo interfaceInfo)
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
        private readonly string _name;
        private readonly string _details;
        public override string Name => _name;
        public override string GetDetails() => _details;
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
        public override string GetDetails() => EmbeddedFile.Content;
        public EmbeddedFileModel(EmbeddedFile embeddedFile)
        {
            EmbeddedFile = embeddedFile;
        }
    }

    public class NodeModel : TreeLeafNodeModel
    {
        private readonly Node _node;
        private readonly Node[] _nodes;
        private readonly int[] _edges;
        private readonly IDictionary<int, int>? _overrides;
        public override string Name => _node.ToString();
        public override string GetDetails()
        {
            var sb = new StringBuilder();
            if (_edges.Length > 0)
            {
                sb.AppendLine("Depends on:");
                foreach (int d in _edges)
                {
                    sb.Append("  ").AppendLine(_nodes[d].ToString());
                }
            }
            if (_overrides != null)
            {
                sb.AppendLine("Implementations:");
                foreach (var kv in _overrides)
                {
                    if (
                        _node is Node.TypeNode typeNode
                        && _nodes[kv.Key] is Node.AbstractMethodNode abstractNode
                        && _nodes[kv.Value] is Node.ImplementationNode implNode
                        && implNode.typ.Equals(typeNode.Item)
                        && implNode.baseTyp.Equals(abstractNode.Item1)
                        && implNode.Item3.Equals(abstractNode.Item2)
                    )
                    {
                        sb.Append("  ").AppendLine(abstractNode.ToString());
                    }
                    else
                    {
                        sb.Append("  ").AppendLine(_nodes[kv.Key].ToString())
                            .Append("    => ").AppendLine(_nodes[kv.Value].ToString());
                    }
                }
            }
            return sb.ToString();
        }

        public NodeModel(Node node, Node[] nodes, int[] edges, IDictionary<int, int>? overrides)
        {
            _node = node;
            _nodes = nodes;
            _edges = edges;
            _overrides = overrides;
        }
    }

    public class MacroEntriesModel : TreeGroupNodeModel
    {
        public Info Metadata { get; init; }
        public override string Name => "MacroEntries";
        public MacroEntriesModel(Info metadata)
        {
            Metadata = metadata;
            foreach (var x in metadata.MacroEntries)
            {
                Contents.Add(new MacroEntryModel(x.Key, x.Value));
            }
        }
    }
    public class MacroEntryModel : TreeLeafNodeModel
    {
        public MetadataEntry Key { get; init; }
        public IEnumerable<MetadataEntry> Values { get; init; }
        public override string Name => Key.ToString();
        public override string GetDetails()
        {
            var sb = new StringBuilder();
            foreach (var v in Values)
            {
                sb.AppendLine(v.ToString());
            }
            return sb.ToString();
        }
        public MacroEntryModel(MetadataEntry key, IEnumerable<MetadataEntry> values)
        {
            Key = key;
            Values = values;
        }
    }
    public class QuotationsModel : TreeGroupNodeModel
    {
        public Info Metadata { get; init; }
        public override string Name => "Quotations";
        public QuotationsModel(Info metadata)
        {
            Metadata = metadata;
            foreach (var x in metadata.Quotations)
            {
                Contents.Add(new QuotationModel(x.Key, x.Value));
            }
            Contents.Add(new QuotatedMethodsModel(metadata));
        }
    }
    public class QuotationModel : TreeLeafNodeModel
    {
        private SourcePos Key;
        private QuotationInfo Value;
        public override string Name => Key.ToString();
        public override string GetDetails()
        {
            return $"{Value.TypeDefinition.Value.FullName}.{Value.Method.Value.MethodName}({string.Join(", ", Value.Arguments)}) in bundles: {string.Join(", ", Value.PreBundles)}";
        }
        public QuotationModel(SourcePos key, QuotationInfo value)
        {
            Key = key;
            Value = value;
        }
    }
    public class QuotatedMethodsModel : TreeLeafNodeModel
    {
        public Info Metadata { get; init; }
        public override string Name => "Quoted methods";
        public override string GetDetails()
        {
            var sb = new StringBuilder();
            foreach (var qm in Metadata.QuotedMethods)
            {
                sb.AppendLine($"{qm.Key.Item1.Value.FullName}.{qm.Key.Item2.Value.MethodName} in bundles: {string.Join(", ", qm.Value)}");
            }
            return sb.ToString();
        }
        public QuotatedMethodsModel(Info metadata)
        {
            Metadata = metadata;
        }
    }
}
