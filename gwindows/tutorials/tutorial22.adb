with GWindows.Windows.Main; use GWindows.Windows.Main;
with GWindows.ActiveX; use GWindows.ActiveX;
with GWindows.GStrings;
with GWindows.Base;
with GWindows.Application;

with GNATCOM.Initialize;

--  The 57 files ie*.ad* are created through the command
--  bindcom C:\windows\system32\shdocvw.dll IE
--  See gnatcom/tools for bindcom
--
with IE;
with IE.IWebBrowser_Interface;

procedure Tutorial22 is
   Main_Window : Main_Window_Type;
   IE_Control  : ActiveX_Type;
   IE_COM      : IE.IWebBrowser_Interface.IWebBrowser_Type;
begin
   GNATCOM.Initialize.Initialize_COM;

   Create (Main_Window, "My Web Browser");

   Create (IE_Control, Main_Window,
           IE.CLSID_WebBrowser,
           1, 1, 1, 1);
   Dock (IE_Control, GWindows.Base.Fill);

   Visible (Main_Window);

   IE.IWebBrowser_Interface.Query (IE_COM, Interfac (IE_Control));
   IE.IWebBrowser_Interface.Navigate (IE_COM,
                                      GWindows.GStrings.To_BSTR_From_GString
                                        ("http://www.adapower.com"));

   GWindows.Application.Message_Loop;
end Tutorial22;
