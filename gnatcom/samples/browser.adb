with Ada.Strings.Unbounded;
with Interfaces.C;
with GNAT.IO; use GNAT.IO;

with GNATCOM.VARIANT; use GNATCOM.VARIANT;
with GNATCOM.BSTR; use GNATCOM.BSTR;
with GNATCOM.Types; use GNATCOM.Types;
with GNATCOM.GUID;
with GNATCOM.Dispinterface; use GNATCOM.Dispinterface;
with GNATCOM.Utility;
with GNATCOM.Events;
with GNATCOM.Events.Event_Object;
with GNATCOM.Create.COM_Interface;
use GNATCOM.Create;
use GNATCOM;

with IE.IWebBrowser2_Interface; use IE.IWebBrowser2_Interface;

with MSHTML.IHTMLDocument2_Interface; use MSHTML.IHTMLDocument2_Interface;
with MSHTML.IHTMLElement_Interface; use MSHTML.IHTMLElement_Interface;
with MSHTML.IHTMLBodyElement_Interface; use MSHTML.IHTMLBodyElement_Interface;
with MSHTML.IHTMLWindow2_Interface; use MSHTML.IHTMLWindow2_Interface;
with MSHTML.IHTMLScreen_Interface; use MSHTML.IHTMLScreen_Interface;
with MSHTML.IHTMLInputButtonElement_Interface;
use MSHTML.IHTMLInputButtonElement_Interface;

with Pipe_Commands;

package body Browser is

   Explorer   : IWebBrowser2_Type;
   Document   : IHTMLDocument2_Type;
   HTML_Body  : IHTMLBodyElement_Type;
   Element    : IHTMLElement_Type;
   Elements   : Dispinterface_Type;
   OK_Button  : IHTMLInputButtonElement_Type;
   --  HTML objects

   Button_Event_Object : aliased Button_Event_Type;
   Button_Event        : COM_Interface.Pointer_To_COM_Interface_Type :=
     MSHTML.HTMLButtonElementEvents_Events.Create
     (Button_Event_Object'Unchecked_Access);
   Button_Events : GNATCOM.Events.IConnectionPoint_Type;

   Browser_Event_Object : aliased Browser_Event_Type;
   Browser_Event        : COM_Interface.Pointer_To_COM_Interface_Type :=
     IE.DWebBrowserEvents2_Events.Create
     (Browser_Event_Object'Unchecked_Access);
   Browser_Events       : GNATCOM.Events.IConnectionPoint_Type;
   --  Events

   function Retrieve_hInstance return Interfaces.C.long;
   pragma Import (C, Retrieve_hInstance, "rts_get_hInstance");

   --------
   -- GO --
   --------

   procedure Go is
      use type Interfaces.C.int;

      function GetModuleFileName
        (hInst        : Interfaces.C.long;
         lpszFileName : Interfaces.C.char_array;
         cbFileName   : Interfaces.C.int)
        return Interfaces.C.int;
      pragma Import (StdCall, GetModuleFileName, "GetModuleFileNameA");

      MAX_PATH : constant := 1024;
      Exe_Path : aliased Interfaces.C.char_array (1 .. MAX_PATH)
        := (others => Interfaces.C.nul);
   begin
      if GetModuleFileName (Retrieve_hInstance, Exe_Path, MAX_PATH) < 0 then
         raise FILE_NAME_ERROR;
      end if;

      IE.IWebBrowser2_Interface.Create
        (Explorer, "InternetExplorer.Application");

      Put_AddressBar (Explorer, VARIANT_BOOL_FALSE);
      Put_MenuBar (Explorer, VARIANT_BOOL_FALSE);
      Put_ToolBar (Explorer, VARIANT_BOOL_FALSE);
      Put_StatusBar (Explorer, VARIANT_BOOL_FALSE);

      Put_Width (Explorer, 510);
      Put_Height (Explorer, 250);

      IE.DWebBrowserEvents2_Events.Set_Events
        (Browser_Events,
         For_Object      => Explorer,
         Event_Interface => Browser_Event);

      Navigate (Explorer, To_BSTR ("res://" &
                                   Interfaces.C.To_Ada (Exe_Path) &
                                   "/nt_gnatfind.html"));

      GNATCOM.Utility.Message_Loop;

   end Go;

   ----------------------
   -- DocumentComplete --
   ----------------------

   procedure DocumentComplete
     (This  : Browser_Event_Type;
      pDisp : GNATCOM.Types.VARIANT;
      URL   : GNATCOM.Types.VARIANT)
   is
      procedure ChangeTitle
        (hwnd   : Interfaces.C.long;
         uMsg   : Interfaces.C.unsigned := 12;
         wParam : Interfaces.C.unsigned := 0;
         Title  : Interfaces.C.char_array);
      pragma Import (StdCall, ChangeTitle, "SendMessageA");

      function LoadIcon
        (hInst  : Interfaces.C.long;
         idIcon : Interfaces.C.long)
        return Interfaces.C.long;
      pragma Import (StdCall, LoadIcon, "LoadIconA");

      procedure SetIcon
        (hwnd   : Interfaces.C.long;
         uMsg   : Interfaces.C.unsigned := 128;
         Size   : Interfaces.C.unsigned := 0;
         Icon   : Interfaces.C.long);
      pragma Import (StdCall, SetIcon, "SendMessageA");

   begin
      Attach (Document, Get_Document (Explorer));

      Attach (Element,
              Get_Body (Document));

      Query (HTML_Body, Element);

      Attach (Elements, Get_All (Element));

      Attach (OK_Button, Invoke (Elements, "item",
                                 (To_VARIANT (0),
                                        To_VARIANT ("ok"))));

      declare
         Title : Interfaces.C.char_array :=
           Interfaces.C.To_C ("GNATFIND");
      begin
         ChangeTitle (HWND  => Get_HWND (Explorer),
                      Title => Title);
      end;

      SetIcon (HWND => Get_HWND (Explorer),
               Icon => LoadIcon (Retrieve_hInstance, 101));

      Put_Visible (Explorer, VARIANT_BOOL_TRUE);

      MSHTML.HTMLButtonElementEvents_Events.Set_Events
        (Button_Events,
         For_Object => OK_Button,
         Event_Interface => Button_Event);
   end DocumentComplete;

   ------------
   -- OnQuit --
   ------------

   procedure OnQuit
     (This : Browser_Event_Type)
   is
   begin
      GNATCOM.Events.Unadvise (Browser_Events);
      GNATCOM.Utility.Post_Quit;
   end OnQuit;

   -------------
   -- OnClick --
   -------------

   procedure OnClick
     (This : Button_Event_Type)
   is
      use Ada.Strings.Unbounded;

      Text_Field : Dispinterface_Type;
      FileStream : Pipe_Commands.stream;
      Result     : Unbounded_String;
      Window     : IHTMLWindow2_Type;
      Screen     : IHTMLScreen_Type;
   begin
      Attach (Text_Field, Invoke (Elements, "item",
                                  (To_VARIANT (0),
                                   To_VARIANT ("sstring"))));

      GNATCOM.Events.Unadvise (Button_Events);

      FileStream := Pipe_Commands.execute ("gnatfind " &
                                           To_Ada (Get (Text_Field, "Value")),
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

      Attach (Window, Get_parentWindow (Document));
      Attach (Screen, Get_Screen (Window));

      Put_Top (Explorer, 0);
      Put_Left (Explorer, 0);
      Put_Width (Explorer, Get_availWidth (Screen));
      Put_Height (Explorer, Get_availHeight (Screen));

      Put_InnerHTML (Element, To_BSTR
                     ("<IMG SRC=""act.gif""> " &
                      "<p><H3>gnatfind results for " &
                      To_Ada (Get (Text_Field, "Value")) &
                      ":</H3><code>" &
                      To_String (Result) &
                      "</code><p>----"));

      GNATCOM.Utility.Post_Quit;
   end OnClick;

end Browser;



