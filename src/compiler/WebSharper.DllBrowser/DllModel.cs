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
            var classes = new Dictionary<Hashed<TypeDefinitionInfo>, (ClassInfo?, CustomTypeInfo?)>();
            foreach (var x in metadata.Classes)
            {
                classes.Add(x.Key, (x.Value, null));
            }
            foreach (var x in metadata.CustomTypes)
            {
                if (classes.TryGetValue(x.Key, out var ct))
                {
                    classes[x.Key] = (ct.Item1, x.Value);
                }
                else
                {
                    classes.Add(x.Key, (null, x.Value));
                }
            }
            foreach (var x in classes.OrderBy(x => x.Key.Value.FullName))
            {
                Contents.Add(new ClassModel(x.Key.Value, x.Value.Item1, x.Value.Item2));
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
        public ClassInfo? ClassInfo { get; init; }
        public CustomTypeInfo? CustomTypeInfo { get; init; }
        public override string Name => "Class: " + Type.FullName;
        public override string GetDetails()
        {
            var sb = new StringBuilder();

            if (CustomTypeInfo != null)
            {
                sb.AppendLine(CustomTypeInfo.ToString());
            }

            var cls = ClassInfo;
            if (cls != null)
            {
                if (cls.Address != null)
                {
                    sb.Append("Address: ").AppendLine(string.Join(".", cls.Address.Value.Value.Reverse()));
                    sb.AppendLine();
                }
                if (cls.BaseClass != null)
                {
                    sb.Append("BaseClass: ").AppendLine(cls.BaseClass.Value.ToString().Replace("\n", ""));
                    sb.AppendLine();
                }
                foreach (var m in cls.Constructors)
                {
                    sb.AppendLine(".ctor(" + m.Key.Value.ToString() + ")");
                    sb.Append("  CompiledForm: ").AppendLine(m.Value.Item1.ToString().Replace("\n", ""));
                    sb.Append("  Optimizations: ").AppendLine(m.Value.Item2.ToString().Replace("\n", ""));
                    sb.Append("  Expression: ").AppendLine(Debug.PrintExpression(m.Value.Item3).Replace("\n", ""));
                    sb.AppendLine();
                }
                if (cls.StaticConstructor != null)
                {
                    sb.AppendLine(".cctor()");
                    sb.Append("  Statement: ").AppendLine(Debug.PrintExpression(cls.StaticConstructor.Value.Item2).Replace("\n", ""));
                    sb.AppendLine();
                }
                foreach (var m in cls.Methods)
                {
                    sb.AppendLine(m.Key.Value.ToString());
                    sb.Append("  CompiledForm: ").AppendLine(m.Value.Item1.ToString().Replace("\n", ""));
                    sb.Append("  Optimizations: ").AppendLine(m.Value.Item2.ToString().Replace("\n", ""));
                    sb.Append("  Expression: ").AppendLine(Debug.PrintExpression(m.Value.Item3).Replace("\n", ""));
                    sb.AppendLine();
                }
                foreach (var m in cls.Implementations)
                {
                    sb.AppendLine(m.Key.Item1.Value.ToString() + ": " + m.Key.Item2.Value.ToString());
                    sb.Append("  CompiledForm: ").AppendLine(m.Value.Item1.ToString().Replace("\n", ""));
                    sb.Append("  Expression: ").AppendLine(Debug.PrintExpression(m.Value.Item2).Replace("\n", ""));
                    sb.AppendLine();
                }

            }

            return sb.ToString();
        }
        public ClassModel(TypeDefinitionInfo type, ClassInfo? classInfo, CustomTypeInfo? customTypeInfo)
        {
            Type = type;
            ClassInfo = classInfo;
            CustomTypeInfo = customTypeInfo;
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
            foreach (var m in InterfaceInfo.Methods)
            {
                sb.AppendLine(m.Key.Value.ToString());
                sb.Append("  CompiledName: ").AppendLine(m.Value);
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
            foreach (var x in assembly.GetScripts())
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
        private readonly  string _name;
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
                        && implNode.Item1.Equals(typeNode.Item)
                        && implNode.Item2.Equals(abstractNode.Item1)
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

        public NodeModel(Node node, Node[] nodes, int[] edges, IDictionary<int,int>? overrides)
        {
            _node = node;
            _nodes = nodes;
            _edges = edges;
            _overrides = overrides;
        }
    }

}
