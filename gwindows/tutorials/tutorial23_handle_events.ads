with IE.DWebBrowserEvents2_Events;
with GNATCOM.Types;

package Tutorial23_Handle_Events is

   type Handle_Event_Type is
     new IE.DWebBrowserEvents2_Events.DWebBrowserEvents2_Event
     with null record;

   procedure DocumentComplete
     (This  : Handle_Event_Type;
      pDisp : GNATCOM.Types.VARIANT;
      URL   : GNATCOM.Types.VARIANT);
   --  Override from IE.DWebBrowserEvents2_Events.DWebBrowserEvents2_Event

end Tutorial23_Handle_Events;
