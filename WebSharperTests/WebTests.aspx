<%@ Page Language="C#" %>

<html>
<head runat="server">
    <title>WebSharper Web TestSuite</title>
    <WS:ScriptManager runat="server" />
</head>
<body>
    <h1>
        WebSharper Web TestSuite
    </h1>
    <Tests:HelloWorld runat="server" />
    <Tests:RemotingTests runat="server" />
</body>
</html>
