with GWindows.Windows.Main; use GWindows.Windows.Main;
with GWindows.ActiveX; use GWindows.ActiveX;
with GWindows.GStrings;
with GWindows.Base;
with GWindows.Application;

with GNATCOM.Initialize;
with GNATCOM.Create.COM_Interface;
with GNATCOM.Events;

with IE;
with IE.IWebBrowser_Interface;
with IE.DWebBrowserEvents2_Events;

with Tutorial23_Handle_Events;

procedure Tutorial23 is
   Main_Window : Main_Window_Type;
   IE_Control  : Activex_Type;
   IE_COM      : IE.IWebBrowser_Interface.IWebBrowser_Type;
   
   
   Handle_Event_Object : aliased Tutorial23_Handle_Events.Handle_Event_Type;
   -- Create event object
   
   Handle_Event : GNATCOM.Create.COM_Interface.Pointer_To_COM_Interface_Type :=
     IE.DWebBrowserEvents2_Events.Create
     (Handle_Event_Object'Unchecked_Access);
   --  Create COM interface wrapper around event object
   
   Handle_Events_Connection_Point : GNATCOM.Events.IConnectionPoint_Type;
   --  Interface to connection point with in COM object 
   --  Used to connect and disconnect event object
   --  Disconnect is performed automaticly when Handle_Events is finalized

begin
   GNATCOM.Initialize.Initialize_COM;

   Create (Main_Window, "My Web Browser");

   Create (IE_Control, Main_Window,
           IE.CLSID_WebBrowser,
           1, 1, 1, 1);
   Dock (IE_Control, GWindows.Base.Fill);

   Visible (Main_Window);

   IE.IWebBrowser_Interface.Query (IE_COM, Interface (IE_Control));
   
   IE.DWebBrowserEvents2_Events.Set_Events
     (Handle_Events_Connection_Point,
      For_Object      => IE_COM,
      Event_Interface => Handle_Event);
   
   
   IE.IWebBrowser_Interface.Navigate (IE_COM,
                                      GWindows.GStrings.To_BSTR_From_GString
                                        ("http://www.adapower.com"));

   GWindows.Application.Message_Loop;
end Tutorial23;
