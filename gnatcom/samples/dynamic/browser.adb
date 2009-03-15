with Ada.Strings.Unbounded;
with Interfaces.C;

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

with Pipe_Commands;

package body Browser is

   procedure On_Click (Text : String);

   procedure Button_Invoke
     (dispidMember : in Interfaces.C.long;
      wFlags       : in Interfaces.C.unsigned_short;
      pdispparams  : in GNATCOM.Types.Pointer_To_DISPPARAMS;
      Event_Object : in GNATCOM.Events.Event_Object.Event_Pointer);
   --  Handles Button Events

   procedure Browser_Invoke
     (dispidMember : in Interfaces.C.long;
      wFlags       : in Interfaces.C.unsigned_short;
      pdispparams  : in GNATCOM.Types.Pointer_To_DISPPARAMS;
      Event_Object : in GNATCOM.Events.Event_Object.Event_Pointer);
   --  Handles Browser Events

   Explorer   : Dispinterface_Type;
   Document   : Dispinterface_Type;
   HTML_Body  : Dispinterface_Type;
   Elements   : Dispinterface_Type;
   OK_Button  : Dispinterface_Type;
   --  HTML objects

   ID_HTMLButtonElementEvents_onclick      : constant := -600;
   ID_DWebBrowserEvents2_OnQuit            : constant := 253;
   ID_DWebBrowserEvents2_DocumentComplete  : constant := 259;
   --  Event IDs

   IID_HTMLButtonElementEvents : GNATCOM.Types.GUID :=
     GNATCOM.GUID.To_GUID ("{3050F2B3-98B5-11CF-BB82-00AA00BDCE0B}");
   --  IID of Events for buttons

   Button_Event  : COM_Interface.Pointer_To_COM_Interface_Type :=
     GNATCOM.Events.Event_Object.Create (Button_Invoke'Access,
                                         IID_HTMLButtonElementEvents);
   Button_Events : GNATCOM.Events.IConnectionPoint_Type;

   IID_DWebBrowserEvents2 : GNATCOM.Types.GUID :=
     GNATCOM.GUID.To_GUID ("{34A715A0-6587-11D0-924A-0020AFC7AC4D}");
   --  IID of Browser events

   Browser_Event  : COM_Interface.Pointer_To_COM_Interface_Type :=
     GNATCOM.Events.Event_Object.Create (Browser_Invoke'Access,
                                         IID_DWebBrowserEvents2);
   Browser_Events : GNATCOM.Events.IConnectionPoint_Type;

   procedure ChangeTitle
     (hwnd   : Integer;
      uMsg   : Interfaces.C.unsigned := 12;
      wParam : Interfaces.C.unsigned := 0;
      Title  : Interfaces.C.char_array);
   pragma Import (StdCall, ChangeTitle, "SendMessageA");

   procedure SetIcon
     (hwnd   : Integer;
      uMsg   : Interfaces.C.unsigned := 128;
      Size   : Interfaces.C.unsigned := 0;
      Icon   : Interfaces.C.long);
   pragma Import (StdCall, SetIcon, "SendMessageA");

   function LoadIcon
     (hInst  : Interfaces.C.long;
      idIcon : Interfaces.C.long)
     return Interfaces.C.long;
   pragma Import (StdCall, LoadIcon, "LoadIconA");

   function Retrieve_hInstance return Interfaces.C.long;
   pragma Import (C, Retrieve_hInstance, "rts_get_hInstance");

   function GetModuleFileName
     (hInst        : Interfaces.C.long;
      lpszFileName : Interfaces.C.char_array;
      cbFileName   : Interfaces.C.int)
     return Interfaces.C.int;
   pragma Import (StdCall, GetModuleFileName, "GetModuleFileNameA");

   --------
   -- GO --
   --------

   procedure Go is
      use type Interfaces.C.int;

      MAX_PATH : constant := 1024;
      Exe_Path : aliased Interfaces.C.char_array (1 .. MAX_PATH)
        := (others => Interfaces.C.nul);
   begin
      if GetModuleFileName (Retrieve_hInstance, Exe_Path, MAX_PATH) < 0 then
         raise FILE_NAME_ERROR;
      end if;

      Dispinterface.Create
        (Explorer, "InternetExplorer.Application");

      Put (Explorer, "AddressBar", VARIANT_FALSE);
      Put (Explorer, "MenuBar", VARIANT_FALSE);
      Put (Explorer, "ToolBar", VARIANT_FALSE);
      Put (Explorer, "StatusBar", VARIANT_FALSE);

      Put (Explorer, "width", To_VARIANT (510));
      Put (Explorer, "height", To_VARIANT (250));

      GNATCOM.Events.Set_Events (Browser_Events,
                                 For_Object => Explorer,
                                 Event_IID  => IID_DWebBrowserEvents2,
                                 Event_Interface => Browser_Event);

      Invoke (Explorer, "navigate",
              (1 => To_VARIANT ("res://" &
                                Interfaces.C.To_Ada (Exe_Path) &
                                "/nt_gnatfind.html")));

      GNATCOM.Utility.Message_Loop;

   end Go;

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

            Attach (HTML_Body, Get (Document, "body"));

            Attach (Elements, Get (HTML_Body, "all"));

            Attach (OK_Button, Invoke (Elements, "item", (To_VARIANT (0),
                                                          To_VARIANT ("ok"))));

            declare
               Title : Interfaces.C.char_array :=
                 Interfaces.C.To_C ("GNATFIND");
            begin
               ChangeTitle (HWND  => To_Ada (Get (Explorer, "HWND")),
                            Title => Title);
            end;


            SetIcon (HWND => To_Ada (Get (Explorer, "HWND")),
                     Icon => LoadIcon (Retrieve_hInstance, 101));

            Put (Explorer, "visible", VARIANT_TRUE);

            GNATCOM.Events.Set_Events
              (Button_Events,
               For_Object => OK_Button,
               Event_IID  => IID_HTMLButtonElementEvents,
               Event_Interface => Button_Event);

         when ID_DWebBrowserEvents2_OnQuit =>
            GNATCOM.Events.Unadvise (Browser_Events);
            GNATCOM.Utility.Post_Quit;
         when others =>
            null;
      end case;
   end Browser_Invoke;

   -------------------
   -- Button_Invoke --
   -------------------

   procedure Button_Invoke
     (dispidMember : in     Interfaces.C.long;
      wFlags       : in     Interfaces.C.unsigned_short;
      pdispparams  : in GNATCOM.Types.Pointer_To_DISPPARAMS;
      Event_Object : in GNATCOM.Events.Event_Object.Event_Pointer)
   is
      use type Interfaces.C.long;
      Text_Field : Dispinterface_Type;
   begin
      Attach (Text_Field, Invoke (Elements, "item",
                                  (To_VARIANT (0),
                                   To_VARIANT ("sstring"))));

      if dispidMember = ID_HTMLButtonElementEvents_onclick then
         On_Click (To_Ada (Get (Text_Field, "Value")));
      end if;
   end Button_Invoke;

   --------------
   -- On_Click --
   --------------

   procedure On_Click (Text : String) is
      use Ada.Strings.Unbounded;

      FileStream : Pipe_Commands.stream;
      Result     : Unbounded_String;
      Window     : Dispinterface_Type;
      Screen     : Dispinterface_Type;
   begin
      GNATCOM.Events.Unadvise (Button_Events);

      FileStream := Pipe_Commands.execute ("gnatfind " & Text,
                                           Pipe_Commands.read_file);

      loop
         begin
            Append (Result, Pipe_Commands.read_next (FileStream));
            Append (Result, "<BR>");
         exception
            when Pipe_Commands.End_Of_File =>
               exit;
         end;
      end loop;

      Attach (Window, Get (Document, "parentWindow"));
      Attach (Screen, Get (Window, "screen"));

      Put (Explorer, "top", To_VARIANT (0));
      Put (Explorer, "left", To_VARIANT (0));
      Put (Explorer, "width", Get (Screen, "availWidth"));
      Put (Explorer, "height", Get (Screen, "availHeight"));

      Put (HTML_Body, "InnerHTML", To_VARIANT
           ("<IMG SRC=""act.gif""> " &
            "<p><H3>gnatfind results for " & Text & ":</H3><code>" &
            To_String (Result) &
            "</code><p>----"));


      GNATCOM.Utility.Post_Quit;
   end On_Click;

end Browser;



