with IE.DWebBrowserEvents2_Events;
with MSHTML.HTMLButtonElementEvents_Events;
with GNATCOM.Types;

package Browser is

   procedure Go;

   type Browser_Event_Type is
     new IE.DWebBrowserEvents2_Events.DWebBrowserEvents2_Event
     with null record;

   procedure DocumentComplete
     (This  : Browser_Event_Type;
      pDisp : GNATCOM.Types.VARIANT;
      URL   : GNATCOM.Types.VARIANT);

   procedure OnQuit
     (This : Browser_Event_Type);

   type Button_Event_Type is
     new MSHTML.HTMLButtonElementEvents_Events.HTMLButtonElementEvents_Event
     with null record;

   procedure onclick
     (This : Button_Event_Type);

   FILE_NAME_ERROR : exception;

end Browser;
