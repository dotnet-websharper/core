namespace web
{
    public class Global : System.Web.HttpApplication
    {
        protected void Application_Start(object sender, System.EventArgs e)
        {
            IntelliFactory.WebSharper.Web.Tests.Server.Initialize();
        }
    }
}
