with IE.DWebBrowserEvents2_Events;
with GNATCOM.Types;

package Spin_Browse is

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

end Spin_Browse;
