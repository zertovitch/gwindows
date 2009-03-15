with Ada.Exceptions;
with Interfaces.C;
with GNAT.OS_Lib;

with GNATCOM.Initialize; use GNATCOM.Initialize;
with GNATCOM.VARIANT; use GNATCOM.VARIANT;
with GNATCOM.Types; use GNATCOM.Types;
with GNATCOM.Dispinterface; use GNATCOM.Dispinterface;
with GNATCOM.Utility;
with GNATCOM.Events;
with GNATCOM.Events.Event_Object;
with GNATCOM.Create.COM_Interface;
use GNATCOM.Create;
use GNATCOM;

with IE.IWebBrowser2_Object; use IE.IWebBrowser2_Object;
with MSHTML.IHTMLDocument2_Object; use MSHTML.IHTMLDocument2_Object;
with MSHTML.IHTMLElement_Object; use MSHTML.IHTMLElement_Object;
with MSHTML.IHTMLBodyElement_Object; use MSHTML.IHTMLBodyElement_Object;
with StructuredGraphicControl.ISGrfxCtl_Object;
use StructuredGraphicControl.ISGrfxCtl_Object;

package body Spin_Browse is

   Explorer   : IWebBrowser2_Type;
   Document   : IHTMLDocument2_Type;
   Element    : IHTMLElement_Type;
   HTML_Body  : IHTMLBodyElement_Type;
   Elements   : Dispinterface_Type;
   Spinner    : ISGrfxCtl_Type;
   --  HTML objects

   Browser_Event_Object : aliased Spin_Browse.Browser_Event_Type;
   Browser_Event        : COM_Interface.Pointer_To_COM_Interface_Type :=
     IE.DWebBrowserEvents2_Events.Create
     (Browser_Event_Object'Unchecked_Access);
   Browser_Events       : GNATCOM.Events.IConnectionPoint_Type;
   --  Events

   task Spin_Task is
      entry Start;
      entry Stop;
   end Spin_Task;

   --------
   -- Go --
   --------

   procedure Go is
   begin
      Initialize_COM_Multi_Threaded;

      IE.IWebBrowser2_Object.Create
        (Explorer, "InternetExplorer.Application");

      Put_AddressBar (Explorer, VARIANT_FALSE);
      Put_MenuBar (Explorer, VARIANT_FALSE);
      Put_ToolBar (Explorer, VARIANT_FALSE);
      Put_StatusBar (Explorer, VARIANT_FALSE);

      Put_Width (Explorer, To_VARIANT (480));
      Put_Height (Explorer, To_VARIANT (480));

      IE.DWebBrowserEvents2_Events.Set_Events (Browser_Events,
                                               For_Object      => Explorer,
                                               Event_Interface =>
                                                 Browser_Event);

      Navigate (Explorer, To_VARIANT ("about:blank"));

      GNATCOM.Utility.Message_Loop;

   exception
      when E : others =>
         GNATCOM.Utility.Message_Box ("Spin",
                                      Ada.Exceptions.Exception_Name (E) &
                                      " : " &
                                      Ada.Exceptions.Exception_Message (E));
         GNAT.OS_Lib.OS_Exit (1);
   end Go;


   ---------------
   -- Spin_Task --
   ---------------

   task body Spin_Task is
      use type Interfaces.C.double;
   begin
      accept Start;
      Initialize_COM_Multi_Threaded;
      loop
         select
            accept Stop;
            Uninitialize_COM;
            exit;
         or
            delay 0.05;
            Rotate (Spinner,
                    To_VARIANT (-1.0),
                    To_VARIANT (1.0),
                    To_VARIANT (2.0));
         end select;
      end loop;
   end Spin_Task;

   ----------------------
   -- DocumentComplete --
   ----------------------

   procedure DocumentComplete
     (This  : Browser_Event_Type;
      pDisp : GNATCOM.Types.VARIANT;
      URL   : GNATCOM.Types.VARIANT)
   is
   begin
      Attach (Document, Get_Document (Explorer));

      Put_bgColor (Document, To_VARIANT ("Teal"));

      Attach (Element,
              Get_Body (Document));

      Query (HTML_Body, Element);

      Put_Text (HTML_Body, To_VARIANT ("Yellow"));

      Put_InnerHTML
        (Element, To_VARIANT
         ("<OBJECT ID=""Spinner"" Width=400 Height=400 " &
          "CLASSID=""CLSID:369303C2-D7AC-11D0-89D5-00A0C90833E6"" " &
          "STYLE=""border:solid 2 yellow""> " &
          "<PARAM NAME=""CoordinateSystem"" VALUE=0> " &
          "<PARAM NAME=""HighQuality"" VALUE=-1> " &
          "<PARAM NAME=""Line0001"" VALUE=""SetLineStyle(1, 2)""> " &
          "<PARAM NAME=""Line0002""" &
          " VALUE=""SetLineColor(0, 0, 0)""> " &
          "<PARAM NAME=""Line0003""" &
          " VALUE=""SetFillColor(0, 0, 255)""> " &
          "<PARAM NAME=""Line0004"" VALUE=""SetFillStyle(1)""> " &
          "<PARAM NAME=""Line0005""" &
          " VALUE=""SetFont('Arial', 72, 600, 0, 0, 0)""> " &
          "<PARAM NAME=""Line0006""" &
          " VALUE=""Text('GNATCOM', -160, 20, 0)""> " &
          "</OBJECT>"));

      Attach (Elements, Get_All (Element));

      Attach (Spinner, Invoke (Elements, "item",
                               (To_VARIANT (0),
                                To_VARIANT ("Spinner"))));

      Put_title (Document, To_VARIANT ("Spinner"));

      Put_Visible (Explorer, VARIANT_TRUE);

      Spin_Task.Start;
   end DocumentComplete;

   ------------
   -- OnQuit --
   ------------

   procedure OnQuit
     (This : Browser_Event_Type)
   is
   begin
      Spin_Task.Stop;
      GNATCOM.Events.Unadvise (Browser_Events);
      GNAT.OS_Lib.OS_Exit (0);
   end OnQuit;

end Spin_Browse;


