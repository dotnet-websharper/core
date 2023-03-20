using System;
using System.Collections.Generic;
using System.Collections.ObjectModel;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using System.Windows;
using System.Windows.Controls;
using System.Windows.Data;
using System.Windows.Documents;
using System.Windows.Input;
using System.Windows.Media;
using System.Windows.Media.Imaging;
using System.Windows.Navigation;
using System.Windows.Shapes;
using WebSharper.Compiler;
using WebSharper.Core;

namespace WebSharper.DllBrowser
{
    /// <summary>
    /// Interaction logic for MainWindow.xaml
    /// </summary>
    public partial class MainWindow : Window
    {
        public ObservableCollection<TreeNodeModel> DllModels { get; } = new();
        public string TitleText => $"WebSharper {Metadata.IO.CurrentVersion} Dll Browser";

        public MainWindow()
        {
            InitializeComponent();
        }

        private async void LoadFile(string path)
        {
            var loading = new LoadingDllModel(System.IO.Path.GetFileName(path));
            DllModels.Add(loading);
            try
            {
                var loaded = await Task.Run(() =>
                {
                    var asmRes = FrontEnd.ReadFullFromFile(path);
                    return new DllModel(asmRes.Item1, asmRes.Item2?.Value);
                });
                lock (DllModels)
                {
                    var index = DllModels.IndexOf(loading);
                    DllModels[index] = loaded;
                }
            }
            catch (Exception ex)
            {
                MessageBox.Show(ex.Message, "Error reading assembly", MessageBoxButton.OK, MessageBoxImage.Error);
                DllModels.Remove(loading);
            }
        }

        private void TreeView_Drop(object sender, DragEventArgs e)
        {
            if (e.Data.GetDataPresent(DataFormats.FileDrop))
            {
                var files = (string[])e.Data.GetData(DataFormats.FileDrop);
                foreach (var file in files)
                {
                    LoadFile(file);
                }
            }
        }

    }
}