with Ada.Exceptions;

with Interfaces.C;

with GNATCOM.Initialize; use GNATCOM.Initialize;
with GNATCOM.VARIANT; use GNATCOM.VARIANT;
with GNATCOM.Types; use GNATCOM.Types;
with GNATCOM.GUID;
with GNATCOM.Dispinterface; use GNATCOM.Dispinterface;
with GNATCOM.Utility;
with GNATCOM.Events;
with GNATCOM.Events.Event_Object;
with GNATCOM.Create.COM_Interface;
use GNATCOM.Create;
use GNATCOM;

with GNAT.OS_Lib;

procedure Spin is
   pragma Linker_Options ("-mwindows");
   pragma Linker_Options ("../spin.coff");

   Explorer   : Dispinterface_Type;
   Document   : Dispinterface_Type;
   HTML_Body  : Dispinterface_Type;
   Elements   : Dispinterface_Type;
   Spinner    : Dispinterface_Type;
   --  HTML objects

   task Spin_Task is
      entry Start;
      entry Stop;
   end Spin_Task;

   task body Spin_Task is
   begin
      accept Start;
      Initialize_COM_Multi_Threaded;
      loop
         select
            accept Stop;
            Uninitialize_COM;
            exit;
         or
            delay 0.5;
            Invoke (Spinner, "Rotate", (To_VARIANT (2),
                                        To_VARIANT (1),
                                        To_VARIANT (-1)));
         end select;
      end loop;
   end Spin_Task;

   ID_DWebBrowserEvents2_OnQuit            : constant := 253;
   ID_DWebBrowserEvents2_DocumentComplete  : constant := 259;
   --  Event IDs

   procedure Browser_Invoke
     (dispidMember : in Interfaces.C.long;
      wFlags       : in Interfaces.C.unsigned_short;
      pdispparams  : in GNATCOM.Types.Pointer_To_DISPPARAMS;
      Event_Object : in GNATCOM.Events.Event_Object.Event_Pointer);
   --  Handles Browser Events

   IID_DWebBrowserEvents2 : GNATCOM.Types.GUID :=
     GNATCOM.GUID.To_GUID ("{34A715A0-6587-11D0-924A-0020AFC7AC4D}");
   --  IID of Browser events

   Browser_Event  : COM_Interface.Pointer_To_COM_Interface_Type :=
     GNATCOM.Events.Event_Object.Create (Browser_Invoke'Unrestricted_Access,
                                         IID_DWebBrowserEvents2);
   Browser_Events : GNATCOM.Events.IConnectionPoint_Type;


   --------------------
   -- Browser_Invoke --
   --------------------

   procedure Browser_Invoke
     (dispidMember : in Interfaces.C.long;
      wFlags       : in Interfaces.C.unsigned_short;
      pdispparams  : in GNATCOM.Types.Pointer_To_DISPPARAMS;
      Event_Object : in GNATCOM.Events.Event_Object.Event_Pointer)
   is
      use type Interfaces.C.long;
   begin
      case dispidMember is
         when ID_DWebBrowserEvents2_DocumentComplete =>
            Attach (Document, Get (Explorer, "document"));

            Put (Document, "bgcolor", To_VARIANT ("Teal"));

            Attach (HTML_Body, Get (Document, "body"));

            Put (HTML_Body, "Text", To_VARIANT ("Yellow"));

            Put (HTML_Body, "InnerHTML", To_VARIANT
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
                  " VALUE=""Text('Ada Rules!', -160, 20, 0)""> " &
                  "</OBJECT>"));

            Attach (Elements, Get (HTML_Body, "all"));

            Attach (Spinner, Invoke (Elements, "item",
                                     (To_VARIANT (0),
                                      To_VARIANT ("Spinner"))));

            Put (Document, "title", To_VARIANT ("Spinner"));

            Put (Explorer, "visible", VARIANT_TRUE);

            Spin_Task.Start;

         when ID_DWebBrowserEvents2_OnQuit =>
            Spin_Task.Stop;
            GNATCOM.Events.Unadvise (Browser_Events);
            GNAT.OS_Lib.OS_Exit (0);
         when others =>
            null;
      end case;
   end Browser_Invoke;

begin

   Initialize_COM_Multi_Threaded;

   Dispinterface.Create
     (Explorer, "InternetExplorer.Application");

   Put (Explorer, "AddressBar", VARIANT_FALSE);
   Put (Explorer, "MenuBar", VARIANT_FALSE);
   Put (Explorer, "ToolBar", VARIANT_FALSE);
   Put (Explorer, "StatusBar", VARIANT_FALSE);

   Put (Explorer, "width", To_VARIANT (480));
   Put (Explorer, "height", To_VARIANT (480));

   GNATCOM.Events.Set_Events (Browser_Events,
                              For_Object => Explorer,
                              Event_IID  => IID_DWebBrowserEvents2,
                              Event_Interface => Browser_Event);

   Invoke (Explorer, "navigate",
           (1 => To_VARIANT ("about:blank")));

   GNATCOM.Utility.Message_Loop;

exception
   when E : others =>
      GNATCOM.Utility.Message_Box ("Spin",
                                   Ada.Exceptions.Exception_Name (E) & " : " &
                                   Ada.Exceptions.Exception_Message (E));
end Spin;

