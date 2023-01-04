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

namespace WebSharper.DllBrowser
{
    /// <summary>
    /// Interaction logic for MainWindow.xaml
    /// </summary>
    public partial class MainWindow : Window
    {
        public ObservableCollection<DllModel> DllModels { get; } = new();

        public MainWindow()
        {
            InitializeComponent();
        }

        private void TreeView_Drop(object sender, DragEventArgs e)
        {
            if (e.Data.GetDataPresent(DataFormats.FileDrop))
            {
                var files = (string[])e.Data.GetData(DataFormats.FileDrop);
                foreach (var file in files)
                {
                    try
                    {
                        var loaded = FrontEnd.ReadFullFromFile(file);
                        DllModels.Add(new DllModel(loaded.Item1, loaded.Item2?.Value));
                    }
                    catch (Exception ex)
                    {
                        MessageBox.Show(ex.Message, "Error reading assembly", MessageBoxButton.OK, MessageBoxImage.Error);
                    }
                }
            }
        }

    }
}
